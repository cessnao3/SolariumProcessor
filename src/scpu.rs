use crate::memory::*;

pub struct SCPU
{
    memory_map: Vec<Box<dyn MemorySegment>>
}