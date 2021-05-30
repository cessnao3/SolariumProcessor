use crate::memory::*;
use crate::memory::memory_map::MemoryMap;

const NUM_REGISTERS: usize = 16;

pub struct SolariumCPU
{
    memory_map: MemoryMap,
    registers: [MemoryWord; NUM_REGISTERS],
}

impl SolariumCPU
{
    pub fn new() -> SolariumCPU
    {
        let mut cpu = SolariumCPU
        {
            memory_map: MemoryMap::new(),
            registers: [0; NUM_REGISTERS],
        };

        cpu.reset();

        return cpu;
    }

    pub fn reset(&mut self)
    {
        self.memory_map.reset();
        self.registers = [0; NUM_REGISTERS];
    }

    fn reset_vector_loc(&self) -> MemoryWord
    {
        return self.memory_map.get(0);
    }


}
