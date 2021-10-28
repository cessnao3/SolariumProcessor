use crate::common::{MemoryWord, SolariumError};

use super::MemorySegment;

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

    /// Adds a new memory segment to the memory map, returning an error
    /// if it could not be added
    pub fn add_segment(&mut self, segment: Box<dyn MemorySegment>) -> Result<(), String>
    {
        // Extract the start and ending indices
        let start_ind = segment.start_address();
        let end_ind = start_ind + segment.address_len();

        // Ensure that the segment is valid
        if start_ind >= end_ind
        {
            return Err(format!("segment starting index {0:} is >= ending index {1:}", start_ind, end_ind));
        }

        // Check that the new segment will fit within the provided other segments
        for seg in self.memory_map.iter()
        {
            let seg_start = seg.start_address();
            let seg_end = seg_start + seg.address_len();

            let all_below = seg_start < start_ind && seg_end <= start_ind;
            let all_above = seg_start >= end_ind && seg_end > end_ind;

            if !(all_above || all_below)
            {
                return Err(format!("segment from [{0:}, {1:}) intersects with other segments", start_ind, end_ind));
            }
        }

        // Add the segment if all else passes
        self.memory_map.push(segment);
        return Ok(());
    }

    /// Gets the value in memory for a particular location
    pub fn get(&self, ind: usize) -> Result<MemoryWord, SolariumError>
    {
        return match self.segment_for_index(ind)
        {
            Some(seg) => seg.get(ind),
            None => Err(SolariumError::InvalidMemoryAccess(ind))
        };
    }

    /// Sets the value in memory for a particular memory location
    /// Returns true if the value was able to be set; otherwise returns false
    pub fn set(&mut self, ind: usize, data: MemoryWord) -> Result<(), SolariumError>
    {
        return match self.segment_for_index_mut(ind)
        {
            Some(seg) => seg.set(ind, data),
            None => Err(SolariumError::InvalidMemoryAccess(ind))
        };
    }

    /// Provides the memory segment that contains the given memory location
    fn segment_for_index(&self, ind: usize) -> Option<&Box<dyn MemorySegment>>
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
    fn segment_for_index_mut(&mut self, ind: usize) -> Option<&mut Box<dyn MemorySegment>>
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

    /// clears all data from the memory map
    pub fn clear(&mut self)
    {
        self.memory_map.clear();
    }
}
