use crate::memory::*;

pub struct MemoryMap
{
    memory_map: Vec<Box<dyn MemorySegment>>
}

impl MemoryMap
{
    pub fn new() -> MemoryMap
    {
        return MemoryMap
        {
            memory_map: Vec::new()
        };
    }

    pub fn get(&self, ind: MemoryIndex) -> MemoryWord
    {
        return match self.segment_for_index(ind)
        {
            Some(seg) => seg.get(ind),
            None => 0
        };
    }

    pub fn set(&mut self, ind: MemoryIndex, data: MemoryWord) -> bool
    {
        return match self.segment_for_index_mut(ind)
        {
            Some(seg) => seg.set(ind, data),
            None => false
        };
    }

    fn segment_for_index(&self, ind: MemoryIndex) -> Option<&Box<dyn MemorySegment>>
    {
        for seg in self.memory_map.iter()
        {
            if seg.within(ind)
            {
                return Some(seg);
            }
        }

        return None;
    }

    fn segment_for_index_mut(&mut self, ind: MemoryIndex) -> Option<&mut Box<dyn MemorySegment>>
    {
        for seg in self.memory_map.iter_mut()
        {
            if seg.within(ind)
            {
                return Some(seg);
            }
        }

        return None;
    }

    pub fn reset(&mut self)
    {
        for seg in self.memory_map.iter_mut()
        {
            seg.reset();
        }
    }

    pub fn clear(&mut self)
    {
        self.memory_map.clear();
    }
}
