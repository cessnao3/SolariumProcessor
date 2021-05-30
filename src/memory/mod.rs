pub mod memory_map;
pub mod read_write_memory;
pub mod read_only_memory;

pub type MemoryWord = u32;

pub trait MemorySegment
{
    fn get(&self, ind: MemoryWord) -> MemoryWord;

    fn set(&mut self, ind: MemoryWord, data: MemoryWord) -> bool;

    fn reset(&mut self);

    fn start_address(&self) -> MemoryWord;

    fn address_len(&self) -> MemoryWord;

    fn within(&self, ind: MemoryWord) -> bool;
}

