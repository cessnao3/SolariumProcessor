use std::rc::Rc;
use std::cell::RefCell;

use super::messages::GuiMessage;

use sproc::cpu::SolariumProcessor;
use sproc::devices::SerialInputOutputDevice;
use sproc::memory::{MEM_MAX_SIZE, MemorySegment, ReadOnlySegment, ReadWriteSegment};
use sproc::common::MemoryWord;


pub type RegisterArray = [u16; SolariumProcessor::NUM_REGISTERS];

pub struct ProcessorStatusStruct
{
    cpu: SolariumProcessor,
    regs: RegisterArray,
    regs_updated: bool,
    step_error: bool,
    last_assembly: Vec::<MemoryWord>,
    serial_io_dev: Rc<RefCell<SerialInputOutputDevice>>,
    pub msg_queue: Vec<GuiMessage>
}

impl ProcessorStatusStruct
{
    const DEVICE_START_IND: usize = 0xA000;

    pub fn new() -> ProcessorStatusStruct
    {
        let mut stat = ProcessorStatusStruct
        {
            cpu: SolariumProcessor::new(),
            regs: [0u16; SolariumProcessor::NUM_REGISTERS],
            regs_updated: false,
            step_error: false,
            last_assembly: Vec::new(),
            serial_io_dev: Rc::new(RefCell::new(SerialInputOutputDevice::new(Self::DEVICE_START_IND, usize::MAX))),
            msg_queue: Vec::new()
        };

        stat.reset();

        return stat;
    }

    pub fn reset(&mut self)
    {
        self.cpu.memory_map.clear();
        self.serial_io_dev.borrow_mut().reset();
        self.step_error = false;

        const INIT_RO_LEN: usize = SolariumProcessor::INIT_DATA_SIZE;

        let reset_vec_data: Vec<MemoryWord> = (0..INIT_RO_LEN)
            .map(|i| if i < self.last_assembly.len() { self.last_assembly[i] } else { MemoryWord::new(0) })
            .collect();
        assert!(reset_vec_data.len() == INIT_RO_LEN);

        assert!(Self::DEVICE_START_IND < MEM_MAX_SIZE);

        match self.cpu.memory_map.add_segment(Rc::new(RefCell::new(ReadOnlySegment::new(0, reset_vec_data))))
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        };
        match self.cpu.memory_map.add_segment(Rc::new(RefCell::new(ReadWriteSegment::new(INIT_RO_LEN, Self::DEVICE_START_IND - INIT_RO_LEN))))
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        };
        match self.cpu.memory_map.add_segment(self.serial_io_dev.clone())
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        };

        self.cpu.hard_reset();

        for (i, val) in self.last_assembly.iter().enumerate()
        {
            if i < INIT_RO_LEN
            {
                continue;
            }

            match self.cpu.memory_map.set(i, *val)
            {
                Ok(()) => (),
                Err(e) => panic!("error on memory set: {0:}", e.to_string())
            };
        }

        self.update_regs();
    }

    pub fn soft_reset(&mut self)
    {
        self.cpu.soft_reset();
        self.update_regs();
        self.step_error = false;
    }

    pub fn hardware_interrupt(&mut self, hw_irq_num: usize)
    {
        match self.cpu.hardware_interrupt(hw_irq_num)
        {
            Ok(interrupt_ran) =>
            {
                if !interrupt_ran
                {
                    self.msg_queue.push(GuiMessage::LogMessage("hardware interrupt did not run".to_string()));
                }
                else
                {
                    self.update_regs();
                }
            },
            Err(e) =>
            {
                self.msg_queue.push(GuiMessage::LogMessage(e.to_string()));
                self.step_error = true;
            }
        }
    }

    pub fn step(&mut self)
    {
        if !self.step_error
        {
            match self.cpu.step()
            {
                Ok(()) => (),
                Err(e) =>
                {
                    self.msg_queue.push(GuiMessage::LogMessage(e.to_string()));
                    self.step_error = true;
                }
            }
            self.update_regs();
        }
    }

    pub fn update_regs(&mut self)
    {
        for i in 0..SolariumProcessor::NUM_REGISTERS
        {
            self.regs[i] = self.cpu.get_register_value(i).get();
        }
        self.regs_updated = true;
    }

    pub fn has_step_error(&self) -> bool
    {
        return self.step_error;
    }

    pub fn load_data(&mut self, data: Vec::<MemoryWord>)
    {
        self.last_assembly = data;

        self.msg_queue.push(GuiMessage::LogMessage(format!(
            "loaded assembly code with length {0:}",
            self.last_assembly.len())));

        self.reset();
    }

    pub fn update_msg_queue(&mut self)
    {
        if self.regs_updated
        {
            self.msg_queue.push(GuiMessage::UpdateRegisters(self.regs));
            self.regs_updated = false;
        }
    }

    pub fn add_log_to_queue(&mut self, msg: String)
    {
        self.msg_queue.push(GuiMessage::LogMessage(msg));
    }

    pub fn send_memory_to_queue(&mut self)
    {
        let mem_vec: Vec<MemoryWord> = (0..MEM_MAX_SIZE).map(|i| {
            return match self.cpu.memory_map.get_debug(i)
            {
                Ok(v) => v,
                Err(_) => MemoryWord::new(0)
            };
        }).collect();

        self.msg_queue.push(GuiMessage::UpdateMemory(mem_vec));
    }

    pub fn pop_serial_output(&self) -> Option<MemoryWord>
    {
        return self.serial_io_dev.borrow_mut().pop_output();
    }

    pub fn push_serial_input_char(&mut self, c: char)
    {
        match sproc::text::character_to_word(c)
        {
            Ok(word) =>
            {
                if !self.serial_io_dev.borrow_mut().push_input(word)
                {
                    self.msg_queue.push(GuiMessage::LogMessage("device serial input buffer full".to_string()))
                }
            },
            Err(e) => self.msg_queue.push(GuiMessage::LogMessage(e.to_string()))
        };
    }
}
