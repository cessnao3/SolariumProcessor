use crate::messages::{ThreadToUi, UiToThread};
use gtk::glib::Sender;
use sproc::common::MemoryWord;
use std::sync::mpsc::{Receiver, RecvError, TryRecvError};

use sproc::{
    common::SolariumError, cpu::SolariumProcessor, devices::InterruptClockDevice,
    devices::SerialInputOutputDevice, memory::MemorySegment, memory::ReadOnlySegment,
    memory::ReadWriteSegment,
};
use std::cell::RefCell;
use std::rc::Rc;

struct ThreadState {
    running: bool,
    multiplier: f64,
    run_thread: bool,
    cpu: SolariumProcessor,
    serial_io_dev: Rc<RefCell<SerialInputOutputDevice>>,
    last_code: Vec<MemoryWord>,
}

impl ThreadState {
    const DEVICE_START_IND: usize = 0xA000;

    fn new() -> Self {
        let mut s = Self {
            run_thread: true,
            running: false,
            multiplier: 1.0,
            cpu: SolariumProcessor::new(),
            serial_io_dev: Rc::new(RefCell::new(SerialInputOutputDevice::new(usize::MAX))),
            last_code: Vec::new(),
        };

        s.reset().unwrap();
        s
    }

    fn reset(&mut self) -> Result<(), SolariumError> {
        const INIT_RO_LEN: usize = SolariumProcessor::INIT_DATA_SIZE;

        self.cpu = SolariumProcessor::new();
        self.serial_io_dev.borrow_mut().reset();

        let reset_vec_data: Vec<MemoryWord> = (0..INIT_RO_LEN)
            .map(|i| {
                if i < self.last_code.len() {
                    self.last_code[i]
                } else {
                    MemoryWord::default()
                }
            })
            .collect();
        assert!(reset_vec_data.len() == INIT_RO_LEN);

        self.cpu.memory_add_segment(
            0,
            Rc::new(RefCell::new(ReadOnlySegment::new(reset_vec_data))),
        )?;

        self.cpu.memory_add_segment(
            INIT_RO_LEN,
            Rc::new(RefCell::new(ReadWriteSegment::new(
                Self::DEVICE_START_IND - INIT_RO_LEN,
            ))),
        )?;
        self.cpu
            .memory_add_segment(Self::DEVICE_START_IND, self.serial_io_dev.clone())?;

        self.cpu.device_add(self.serial_io_dev.clone())?;

        self.cpu
            .device_add(Rc::new(RefCell::new(InterruptClockDevice::new(1000, 0))))?;

        self.cpu.hard_reset()?;

        Ok(())
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        fn inner_handler(
            state: &mut ThreadState,
            msg: UiToThread,
        ) -> Result<Option<ThreadToUi>, SolariumError> {
            match msg {
                UiToThread::CpuStep => state.cpu.step()?,
                UiToThread::CpuStart => state.running = true,
                UiToThread::CpuStop => state.running = false,
                UiToThread::Exit => state.run_thread = false,
                UiToThread::CpuReset => state.cpu.hard_reset()?,
                UiToThread::CpuIrq(irq) => {
                    if !state.cpu.hardware_interrupt(irq as usize)? {
                        return Ok(Some(ThreadToUi::LogMessage(format!(
                            "irq {irq} not triggered"
                        ))));
                    }
                }
                UiToThread::SetMultiplier(m) => {
                    state.multiplier = m;
                    println!("Setting to {m}");
                }
                UiToThread::SetCode(data) => {
                    state.last_code = data;
                    state.reset()?;
                }
                UiToThread::SerialInput(s) => {
                    for c in s.chars() {
                        match sproc::text::character_to_word(c) {
                            Ok(word) => {
                                if !state.serial_io_dev.borrow_mut().push_input(word) {
                                    return Ok(Some(ThreadToUi::LogMessage("device serial input buffer full".to_string())));
                                }
                            }
                            Err(e) => {
                                return Ok(Some(ThreadToUi::LogMessage(e.to_string())))
                            }
                        }
                    }
                }
                UiToThread::RequestMemory(base, size) => {
                    let mut resp_vec = Vec::new();
                    for i in 0..size {
                        resp_vec.push(state.cpu.memory_inspect(base + i)?);
                    }
                    return Ok(Some(ThreadToUi::ResponseMemory(base, resp_vec)));
                }
            }

            Ok(None)
        }

        match inner_handler(self, msg) {
            Ok(resp) => resp,
            Err(e) => Some(ThreadToUi::LogMessage(format!("error: {e}"))),
        }
    }
}

pub fn cpu_thread(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) {
    let mut state = ThreadState::new();

    const THREAD_LOOP_MS: u64 = 50;
    //const THREAD_LOOP_HZ: u64 = 1000 / THREAD_LOOP_MS;

    'mainloop: while state.run_thread {
        if state.running {
            for _ in 0..1000 {
                let resp = match rx.try_recv() {
                    Ok(msg) => state.handle_msg(msg),
                    Err(TryRecvError::Disconnected) => break 'mainloop,
                    Err(TryRecvError::Empty) => break,
                };

                if let Some(r) = &resp {
                    tx.send(r.clone())
                        .expect("Unable to send response to main thread!");
                }
            }
        } else {
            let resp = match rx.recv() {
                Ok(msg) => state.handle_msg(msg),
                Err(RecvError) => break 'mainloop,
            };

            if let Some(r) = &resp {
                tx.send(r.clone())
                    .expect("Unable to send response to main thread!");
            }
        }

        // Check for serial output
        let mut char_vec = Vec::new();
        while let Some(w) = state.serial_io_dev.borrow_mut().pop_output() {
            let c = match sproc::text::word_to_character(w) {
                Ok(v) => v,
                Err(e) => {
                    tx.send(ThreadToUi::LogMessage(format!("{e}"))).unwrap();
                    '?'
                }
            };

            char_vec.push(c);
        }

        if !char_vec.is_empty() {
            tx.send(ThreadToUi::SerialOutput(char_vec.into_iter().collect::<String>())).unwrap();
        }

        // Step if required
        if state.running {
            let step_repeat_count = state.multiplier as i64;

            for _ in 0..step_repeat_count {
                if let Err(e) = state.cpu.step() {
                    state.running = false;
                    tx.send(ThreadToUi::LogMessage(format!("{e}"))).unwrap();
                    break;
                }
            }
        }

        // Send Registers
        tx.send(ThreadToUi::RegisterState(state.cpu.get_register_state())).unwrap();

        // Final sleep
        if state.running {
            std::thread::sleep(std::time::Duration::from_millis(THREAD_LOOP_MS));
        }
    }

    let _ = tx.send(ThreadToUi::ThreadExit);
}
