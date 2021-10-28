use super::messages::GuiMessage;

use libsproc::cpu::SolariumProcessor;
use libsproc::memory::{ReadWriteSegment, ReadOnlySegment, MAX_SEGMENT_INDEX};
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

        const INIT_RO_LEN: usize = 2;

        let reset_vec_data: Vec<MemoryWord> = (0..INIT_RO_LEN)
            .map(|i| if i < self.last_assembly.len() { self.last_assembly[i] } else { MemoryWord::new(0) })
            .collect();
        assert!(reset_vec_data.len() == INIT_RO_LEN);

        match self.cpu.memory_map.add_segment(Box::new(ReadOnlySegment::new(0, reset_vec_data)))
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        };
        match self.cpu.memory_map.add_segment(Box::new(ReadWriteSegment::new(2, MAX_SEGMENT_INDEX - 2)))
        {
            Ok(()) => (),
            Err(e) => panic!("{0:}", e)
        }

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

        //self.cpu.soft_reset();
        self.update_regs();
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
}
