pub type MemoryWord = u16;
pub type MemoryIndex = u32;
pub type MemorySize = u32;

pub trait MemorySegment
{
    fn get(&self, ind: MemoryIndex) -> MemoryWord;

    fn set(&mut self, ind: MemoryIndex, data: MemoryWord) -> bool;

    fn start_address(&self) -> MemoryIndex;

    fn address_len(&self) -> MemoryIndex;

    fn within(&self, ind: MemoryIndex) -> bool;
}
