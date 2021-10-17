use super::messages::GuiMessage;

use libscpu::cpu::SolariumCPU;
use libscpu::memory::{MemoryWord, ReadWriteSegment};

pub type RegisterArray = [u16; SolariumCPU::NUM_REGISTERS];

pub struct ProcessorStatusStruct
{
    cpu: SolariumCPU,
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
            cpu: SolariumCPU::new(),
            regs: [0u16; SolariumCPU::NUM_REGISTERS],
            regs_updated: false,
            step_error: false,
            last_assembly: Vec::new(),
            msg_queue: Vec::new()
        };

        stat.cpu.memory_map.add_segment(Box::new(ReadWriteSegment::new(
            0,
            (2usize).pow(16))));

        stat.reset();

        return stat;
    }

    pub fn reset(&mut self)
    {
        self.cpu.reset();
        self.update_regs();

        for (i, val) in self.last_assembly.iter().enumerate()
        {
            self.cpu.memory_map.set(i, *val);
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
                    self.msg_queue.push(GuiMessage::LogMessage(e));
                    self.step_error = true;
                }
            }
            self.update_regs();
        }
    }

    pub fn update_regs(&mut self)
    {
        for i in 0..SolariumCPU::NUM_REGISTERS
        {
            self.regs[i] = self.cpu.get_register_value(i);
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
        self.reset();

        self.msg_queue.push(GuiMessage::LogMessage(format!(
            "loaded assembly code with length {0:}",
            self.last_assembly.len())));
    }

    pub fn update_msg_queue(&mut self)
    {
        if self.regs_updated
        {
            self.msg_queue.push(GuiMessage::UpdateRegisters(self.regs));
            self.regs_updated = false;
        }
    }
}
