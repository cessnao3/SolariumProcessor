use super::*;

/// Defines the overarching memory mapping structure
pub struct MemoryMap
{
    memory_map: Vec<Box<dyn MemorySegment>>
}

impl MemoryMap
{
    /// Generates a new, empty, memory mapping
    pub fn new() -> MemoryMap
    {
        return MemoryMap
        {
            memory_map: Vec::new()
        };
    }

    /// Adds a new memory segment to the memory map
    /// Returns true if the segment is able to be added
    pub fn add_segment(&mut self, segment: Box<dyn MemorySegment>) -> bool
    {
        // Extract the start and ending indices
        let start_ind = segment.start_address();
        let end_ind = start_ind + segment.address_len();

        // Ensure that the segment is valid
        if start_ind >= end_ind
        {
            return false;
        }

        // Check that the new segment will fit within the provided other segments
        for seg in self.memory_map.iter()
        {
            let seg_start = seg.start_address();
            let seg_end = seg_start + seg.address_len();

            let all_below = seg_start < start_ind && seg_end < start_ind;
            let all_above = seg_start >= end_ind && seg_end >= end_ind;

            if !all_below || !all_above
            {
                return false;
            }
        }

        // Add the segment if all else passes
        self.memory_map.push(segment);
        return true;
    }

    /// Gets the value in memory for a particular location
    pub fn get(&self, ind: MemoryWord) -> MemoryWord
    {
        return match self.segment_for_index(ind)
        {
            Some(seg) => seg.get(ind),
            None => 0
        };
    }

    /// Sets the value in memory for a particular memory location
    /// Returns true if the value was able to be set; otherwise returns false
    pub fn set(&mut self, ind: MemoryWord, data: MemoryWord) -> bool
    {
        return match self.segment_for_index_mut(ind)
        {
            Some(seg) => seg.set(ind, data),
            None => false
        };
    }

    /// Provides the memory segment that contains the given memory location
    fn segment_for_index(&self, ind: MemoryWord) -> Option<&Box<dyn MemorySegment>>
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

    /// Provides the mutable memory segment that contains the given memory location
    fn segment_for_index_mut(&mut self, ind: MemoryWord) -> Option<&mut Box<dyn MemorySegment>>
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

    /// resets all contained memory segments
    pub fn reset(&mut self)
    {
        for seg in self.memory_map.iter_mut()
        {
            seg.reset();
        }
    }
}
