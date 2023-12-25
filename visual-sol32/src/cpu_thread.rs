use crate::messages::{ThreadToUi, UiToThread};
use sol32::cpu::{Processor, ProcessorError};
use sol32::device::{InterruptClockDevice, SerialInputOutputDevice};
use sol32::memory::{MemorySegment, ReadOnlySegment, ReadWriteSegment};
use std::sync::mpsc::{Receiver, RecvError, Sender, TryRecvError};

use std::cell::RefCell;
use std::rc::Rc;

struct ThreadState {
    running: bool,
    multiplier: f64,
    run_thread: bool,
    memory_request: (u32, u32),
    cpu: Processor,
    serial_io_dev: Rc<RefCell<SerialInputOutputDevice>>,
    last_code: Vec<u8>,
}

impl ThreadState {
    const DEVICE_START_IND: u32 = 0xA000;

    fn new() -> Result<Self, ProcessorError> {
        let mut s = Self {
            run_thread: true,
            running: false,
            multiplier: 1.0,
            cpu: Processor::new(),
            serial_io_dev: Rc::new(RefCell::new(SerialInputOutputDevice::new(2048))),
            last_code: Vec::new(),
            memory_request: (0, 0),
        };

        s.reset()?;
        Ok(s)
    }

    fn reset(&mut self) -> Result<(), ProcessorError> {
        const INIT_RO_LEN: u32 = Processor::TOP_VEC_SEG_ADDR;

        self.cpu = Processor::new();
        self.serial_io_dev.borrow_mut().reset();

        let reset_vec_data: Vec<u8> = (0..INIT_RO_LEN)
            .map(|i| {
                let is = i as usize;
                if is < self.last_code.len() {
                    self.last_code[is]
                } else {
                    0
                }
            })
            .collect();
        assert!(reset_vec_data.len() == INIT_RO_LEN as usize);

        self.cpu.memory_add_segment(
            0,
            Rc::new(RefCell::new(ReadOnlySegment::new(reset_vec_data))),
        )?;

        self.cpu.memory_add_segment(
            INIT_RO_LEN,
            Rc::new(RefCell::new(ReadWriteSegment::new(
                (Self::DEVICE_START_IND - INIT_RO_LEN) as usize,
            ))),
        )?;
        self.cpu
            .memory_add_segment(Self::DEVICE_START_IND, self.serial_io_dev.clone())?;

        self.cpu.device_add(self.serial_io_dev.clone())?;

        self.cpu
            .device_add(Rc::new(RefCell::new(InterruptClockDevice::new(1000, 0))))?;

        self.cpu.reset(sol32::cpu::ResetType::Hard)?;

        for (i, val) in self.last_code.iter().enumerate() {
            if i < INIT_RO_LEN as usize {
                continue;
            }

            self.cpu.memory_set(i as u32, *val)?;
        }

        Ok(())
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        fn inner_handler(
            state: &mut ThreadState,
            msg: UiToThread,
        ) -> Result<Option<ThreadToUi>, ProcessorError> {
            match msg {
                UiToThread::CpuStep => state.cpu.step()?,
                UiToThread::CpuStart => state.running = true,
                UiToThread::CpuStop => state.running = false,
                UiToThread::Exit => state.run_thread = false,
                UiToThread::CpuReset => {
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::CpuIrq(irq) => {
                    if !state.cpu.hardware_interrupt(irq as u32)? {
                        return Ok(Some(ThreadToUi::LogMessage(format!(
                            "irq {irq} not triggered"
                        ))));
                    }
                }
                UiToThread::SetMultiplier(m) => {
                    state.multiplier = m;
                }
                UiToThread::SetCode(data) => {
                    state.running = false;
                    state.last_code = data;
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::SerialInput(s) => {
                    for c in s.chars().chain(['\n'; 1]) {
                        match sol32::text::character_to_word(c) {
                            Ok(word) => {
                                if !state.serial_io_dev.borrow_mut().push_input(word) {
                                    return Ok(Some(ThreadToUi::LogMessage(
                                        "device serial input buffer full".to_string(),
                                    )));
                                }
                            }
                            Err(e) => return Ok(Some(ThreadToUi::LogMessage(e.to_string()))),
                        }
                    }
                }
                UiToThread::RequestMemory(base, size) => state.memory_request = (base, size),
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
    let mut state = ThreadState::new().unwrap();

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
            let c = match sol32::text::word_to_character(w) {
                Ok(v) => v,
                Err(e) => {
                    tx.send(ThreadToUi::LogMessage(format!("{e}"))).unwrap();
                    '?'
                }
            };

            char_vec.push(c);
        }

        if !char_vec.is_empty() {
            tx.send(ThreadToUi::SerialOutput(
                char_vec.into_iter().collect::<String>(),
            ))
            .unwrap();
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
        tx.send(ThreadToUi::RegisterState(state.cpu.get_register_state()))
            .unwrap();

        let pc = state
            .cpu
            .get_register_state()
            .get(sol32::cpu::Register::ProgramCounter)
            .unwrap_or(0);
        let mem = state.cpu.memory_inspect_u32(pc).unwrap_or(0);

        tx.send(ThreadToUi::ProgramCounterValue(pc, mem)).unwrap();

        // Send memory if needed
        let (base, size) = state.memory_request;
        let mut resp_memory = Vec::new();
        for i in 0..size {
            resp_memory.push(state.cpu.memory_inspect(base + i).unwrap_or_default());
        }
        tx.send(ThreadToUi::ResponseMemory(base, resp_memory))
            .unwrap();

        // Final sleep
        if state.running {
            std::thread::sleep(std::time::Duration::from_millis(THREAD_LOOP_MS));
        }
    }

    let _ = tx.send(ThreadToUi::ThreadExit);
}
