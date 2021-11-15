use std::rc::Rc;
use std::cell::RefCell;

use super::messages::GuiMessage;

use libsproc::cpu::SolariumProcessor;
use libsproc::devices::SerialInputOutputDevice;
use libsproc::memory::{ReadWriteSegment, ReadOnlySegment, MEM_MAX_SIZE};
use libsproc::common::MemoryWord;


pub type RegisterArray = [u16; SolariumProcessor::NUM_REGISTERS];

pub struct ProcessorStatusStruct
{
    cpu: SolariumProcessor,
    regs: RegisterArray,
    regs_updated: bool,
    step_error: bool,
    last_assembly: Vec::<MemoryWord>,
    pub msg_queue: Vec<GuiMessage>
}

impl ProcessorStatusStruct
{
    pub fn new() -> ProcessorStatusStruct
    {
        let mut stat = ProcessorStatusStruct
        {
            cpu: SolariumProcessor::new(),
            regs: [0u16; SolariumProcessor::NUM_REGISTERS],
            regs_updated: false,
            step_error: false,
            last_assembly: Vec::new(),
            msg_queue: Vec::new()
        };

        stat.reset();

        return stat;
    }

    pub fn reset(&mut self)
    {
        self.cpu.memory_map.clear();

        const INIT_RO_LEN: usize = SolariumProcessor::INIT_DATA_SIZE;

        let reset_vec_data: Vec<MemoryWord> = (0..INIT_RO_LEN)
            .map(|i| if i < self.last_assembly.len() { self.last_assembly[i] } else { MemoryWord::new(0) })
            .collect();
        assert!(reset_vec_data.len() == INIT_RO_LEN);

        const DEVICE_START_IND: usize = 0xA000;
        assert!(DEVICE_START_IND < MEM_MAX_SIZE);

        match self.cpu.memory_map.add_segment(Rc::new(RefCell::new(ReadOnlySegment::new(0, reset_vec_data))))
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        };
        match self.cpu.memory_map.add_segment(Rc::new(RefCell::new(ReadWriteSegment::new(INIT_RO_LEN, DEVICE_START_IND - INIT_RO_LEN))))
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        };
        match self.cpu.memory_map.add_segment(Rc::new(RefCell::new(SerialInputOutputDevice::new(DEVICE_START_IND))))
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
                    println!("Error: {0:}", e.to_string());
                    eprintln!("Error: {0:}", e.to_string());
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

    pub fn send_memory_to_queue(&mut self)
    {
        let mem_vec: Vec<MemoryWord> = (0..MEM_MAX_SIZE).map(|i| {
            return match self.cpu.memory_map.get(i)
            {
                Ok(v) => v,
                Err(_) => MemoryWord::new(0)
            };
        }).collect();

        self.msg_queue.push(GuiMessage::UpdateMemory(mem_vec));
    }
}
