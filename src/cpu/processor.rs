use crate::memory::MemoryWord;
use crate::memory::memory_map::MemoryMap;

use crate::cpu::registers::{Register, RegisterManager};

const VECTOR_RESET: MemoryWord = 0x400;
const VECTOR_IRQ: MemoryWord = 0x401;

pub struct SolariumCPU
{
    memory_map: MemoryMap,
    registers: RegisterManager
}

impl SolariumCPU
{
    pub fn new() -> SolariumCPU
    {
        let mut cpu = SolariumCPU
        {
            memory_map: MemoryMap::new(),
            registers: RegisterManager::new()
        };

        cpu.reset();

        return cpu;
    }

    pub fn reset(&mut self)
    {
        self.memory_map.reset();
        self.registers.reset();
        self.registers.set(Register::ProgramCounter, VECTOR_RESET);
    }

    pub fn step(&mut self)
    {
        let current = self.memory_map.get(self.registers.get(Register::ProgramCounter));
    }

    fn reset_vector_loc(&self) -> MemoryWord
    {
        return self.memory_map.get(0);
    }
}
