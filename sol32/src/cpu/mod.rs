use crate::memory::MemoryMap;

pub struct Processor {
    memory: MemoryMap,
    registers: [u32; 32],
}

impl Processor {
    pub fn hard_reset(&mut self) {
        self.memory.reset();
    }

    pub fn soft_reset(&mut self) {

    }

    pub fn step(&self) {

    }
}
