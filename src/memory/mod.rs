pub mod memory_segment;
pub mod read_write_memory;
pub mod memory_map;

pub type MemoryWord = u16;
pub type MemoryIndex = u32;
pub type MemorySize = u32;

pub trait MemorySegment
{
    fn get(&self, ind: MemoryIndex) -> MemoryWord;

    fn set(&mut self, ind: MemoryIndex, data: MemoryWord) -> bool;

    fn start_address(&self) -> MemoryIndex;

    fn address_len(&self) -> MemoryIndex;

    fn reset(&mut self);

    fn within(&self, ind: MemoryIndex) -> bool;
}

