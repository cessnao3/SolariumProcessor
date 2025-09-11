use crate::messages::{ThreadToUi, UiToThread};
use jib::cpu::{Processor, ProcessorError};
use jib::device::{InterruptClockDevice, SerialInputOutputDevice};
use jib::memory::{MemorySegment, ReadOnlySegment, ReadWriteSegment};
use jib_asm::InstructionList;
use std::io::Write;
use std::sync::mpsc::{Receiver, RecvError, Sender, TryRecvError};

use std::cell::RefCell;
use std::rc::Rc;

struct CircularBuffer<T, const S: usize> {
    history: [Option<T>; S],
    index: usize,
    last: Option<T>,
}

impl<T: Clone + PartialEq + Eq, const S: usize> CircularBuffer<T, S> {
    pub fn reset(&mut self) {
        self.history.fill(None);
        self.index = 0;
        self.last = None;
    }

    pub fn push(&mut self, val: T) {
        let new_last = Some(val);
        if new_last != self.last {
            self.history[self.index] = new_last.clone();
            self.index = (self.index + 1) % self.history.len();
            self.last = new_last;
        }
    }

    pub fn list(&self) -> Vec<T> {
        let mut vals = Vec::new();

        for i in 0..self.history.len() {
            let iv = (self.index + i) % self.history.len();

            if let Some(t) = self.history[iv].as_ref() {
                vals.push(t.clone());
            }
        }

        vals
    }
}

impl<T: Clone + PartialEq + Eq, const S: usize> Default for CircularBuffer<T, S> {
    fn default() -> Self {
        const {
            assert!(S > 0);
        }

        Self {
            history: [const { None }; S],
            index: 0,
            last: None,
        }
    }
}

struct ThreadState {
    running: bool,
    multiplier: f64,
    run_thread: bool,
    memory_request: (u32, u32),
    cpu: Processor,
    serial_io_dev: Rc<RefCell<SerialInputOutputDevice>>,
    last_code: Vec<u8>,
    inst_history: CircularBuffer<String, 10>,
    inst_map: InstructionList,
    breakpoint: Option<u32>,
    history_file: std::fs::File,
    step_count: u128,
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
            inst_history: Default::default(),
            inst_map: InstructionList::default(),
            breakpoint: None,
            history_file: std::fs::File::create("history.csv").expect("unable to open file"),
            step_count: 0,
        };

        s.reset()?;
        Ok(s)
    }

    fn step_cpu(&mut self, enable_breakpoints: bool) -> Result<(), ThreadToUi> {
        let mut inst_details = "??".to_string();

        let pc = self.cpu.get_current_pc().unwrap_or(0);
        if let Ok(inst) = self.cpu.get_current_inst() {
            if let Some(disp_val) = self.inst_map.get_display_inst(inst) {
                inst_details = disp_val;
            }
        }

        inst_details = format!("0x{pc:08x} = {inst_details}");
        self.inst_history.push(inst_details.clone());

        if let Some(brk) = self.breakpoint {
            if brk == pc && enable_breakpoints {
                self.running = false;

                let msg = format!(
                    "Breaking at 0x{brk:08x}\n{}",
                    self.inst_history
                        .list()
                        .into_iter()
                        .map(|s| format!("    {s}"))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
                return Err(ThreadToUi::LogMessage(msg));
            }
        }

        let debug_stop = if let Ok(op) = self.cpu.get_current_op() {
            op == Processor::OP_DEBUG_BREAK || op == Processor::OP_HALT
        } else {
            false
        };

        if let Err(e) = self.cpu.step() {
            let msg = format!(
                "{}\n{}",
                e,
                self.inst_history
                    .list()
                    .into_iter()
                    .map(|s| format!("    {s}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            );

            Err(ThreadToUi::LogMessage(msg))
        } else {
            if debug_stop {
                self.running = false;
            }

            self.step_count += 1;
            write!(self.history_file, "{}", self.step_count).unwrap();

            for r in self.cpu.get_register_state().get_state() {
                write!(self.history_file, ",{r:#010x}").unwrap();
            }

            writeln!(self.history_file, ",{inst_details}").unwrap();

            Ok(())
        }
    }

    fn reset(&mut self) -> Result<(), ProcessorError> {
        const INIT_RO_LEN: u32 = Processor::TOP_VEC_SEG_ADDR;

        self.cpu = Processor::new();
        self.serial_io_dev.borrow_mut().reset();

        self.inst_history.reset();
        self.step_count = 0;

        write!(self.history_file, "#StepCount").unwrap();

        for (i, _) in self.cpu.get_register_state().get_state().iter().enumerate() {
            let extra_details = match i {
                0 => "PC",
                1 => "Stat",
                2 => "SP",
                3 => "OVF",
                4 => "Ret",
                5 => "ArgBase",
                6 => "FunctionLocal",
                7 => "FunctionTemp",
                8 => "Spare",
                _ => "",
            };

            write!(self.history_file, ",{i:02}").unwrap();

            if extra_details.len() > 0 {
                write!(self.history_file, " ({extra_details})").unwrap();
            }
        }

        writeln!(self.history_file, ",Details").unwrap();

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

        let dev_interrupt = Rc::new(RefCell::new(InterruptClockDevice::new(0)));

        self.cpu.device_add(dev_interrupt.clone())?;
        self.cpu.memory_add_segment(
            Self::DEVICE_START_IND + self.serial_io_dev.borrow().len(),
            dev_interrupt,
        )?;

        self.cpu.reset(jib::cpu::ResetType::Hard)?;

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
                UiToThread::SetBreakpoint(brk) => {
                    state.breakpoint = if brk == 0 { None } else { Some(brk) };
                    let msg = if brk == 0 {
                        "Disabling Breakpoint".into()
                    } else {
                        format!("Setting Breakpoint to 0x{brk:08x}")
                    };
                    return Ok(Some(ThreadToUi::LogMessage(msg)));
                }
                UiToThread::CpuStep => {
                    if let Err(e) = state.step_cpu(false) {
                        return Ok(Some(e));
                    }
                }
                UiToThread::CpuStart => state.running = true,
                UiToThread::CpuStop => state.running = false,
                UiToThread::Exit => state.run_thread = false,
                UiToThread::CpuReset => {
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::CpuIrq(irq) => {
                    if !state.cpu.trigger_hardware_interrupt(irq as u32)? {
                        return Ok(Some(ThreadToUi::LogMessage(format!(
                            "irq {irq} not triggered"
                        ))));
                    }
                }
                UiToThread::SetMultiplier(m) => {
                    state.multiplier = m;
                }
                UiToThread::SetCode(data) => {
                    let code = if data.start_address != 0 {
                        let mut asm_vals = vec![0; data.start_address as usize];
                        asm_vals.extend(data.bytes);
                        asm_vals
                    } else {
                        data.bytes
                    };

                    state.running = false;
                    state.last_code = code;
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::SerialInput(s) => {
                    for c in s.chars().chain(['\n'; 1]) {
                        match jib::text::character_to_byte(c) {
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
            let c = match jib::text::byte_to_character(w) {
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
                if let Err(msg) = state.step_cpu(true) {
                    state.running = false;
                    tx.send(msg).unwrap();
                    break;
                }
            }
        }

        // Send Registers
        tx.send(ThreadToUi::RegisterState(Box::new(
            state.cpu.get_register_state(),
        )))
        .unwrap();

        let pc = state
            .cpu
            .get_register_state()
            .get(jib::cpu::Register::ProgramCounter)
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
