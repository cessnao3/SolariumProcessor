use std::cell::RefCell;
use std::rc::Rc;

use crate::common::{MemoryWord, SolariumError};

use super::MemorySegment;

/// Defines the overarching memory mapping structure
pub struct MemoryMap
{
    memory_map: Vec<Rc<RefCell<dyn MemorySegment>>>
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
    pub fn add_segment(&mut self, segment: Rc<RefCell<dyn MemorySegment>>) -> Result<(), String>
    {
        // Extract the start and ending indices
        let start_ind = segment.borrow().start_address();
        let end_ind = start_ind + segment.borrow().address_len();

        // Ensure that the segment is valid
        if start_ind >= end_ind
        {
            return Err(format!("segment starting index {0:} is >= ending index {1:}", start_ind, end_ind));
        }

        // Check that the new segment will fit within the provided other segments
        for seg in self.memory_map.iter()
        {
            let seg_start = seg.borrow().start_address();
            let seg_end = seg_start + seg.borrow().address_len();

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
            Some(seg) => seg.borrow().get(ind),
            None => Err(SolariumError::InvalidMemoryAccess(ind))
        };
    }

    /// Sets the value in memory for a particular memory location
    /// Returns true if the value was able to be set; otherwise returns false
    pub fn set(&mut self, ind: usize, data: MemoryWord) -> Result<(), SolariumError>
    {
        return match self.segment_for_index(ind)
        {
            Some(seg) => seg.borrow_mut().set(ind, data),
            None => Err(SolariumError::InvalidMemoryAccess(ind))
        };
    }

    /// Provides the memory segment that contains the given memory location
    fn segment_for_index(&self, ind: usize) -> Option<&Rc<RefCell<dyn MemorySegment>>>
    {
        for seg in self.memory_map.iter()
        {
            if seg.borrow().within(ind)
            {
                return Some(seg);
            }
        }

        return None;
    }

    /// resets all contained memory segments
    pub fn reset(&mut self)
    {
        for seg in self.memory_map.iter()
        {
            seg.borrow_mut().reset();
        }
    }

    /// clears all data from the memory map
    pub fn clear(&mut self)
    {
        self.memory_map.clear();
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use super::super::{ReadOnlySegment, ReadWriteSegment, MEM_MAX_SIZE};

    /// A simple initialization test
    #[test]
    fn init_memory_map()
    {
        // Initialize the map
        let map = MemoryMap::new();

        // Ensure that all memory values are invalid
        for i in 0..MEM_MAX_SIZE
        {
            assert!(map.get(i).is_err());
        }
    }

    /// A single read-write test
    #[test]
    fn single_read_write()
    {
        // Define the memory size values
        let base = 100;
        let size = 512;

        // Initialize the map
        let mut map = MemoryMap::new();
        let add_result = map.add_segment(Rc::new(RefCell::new(ReadWriteSegment::new(
            base,
            size))));

        assert!(add_result.is_ok());

        let set_val = 314;

        // Iterate through and check if values are within the expected results
        for i in 0..MEM_MAX_SIZE
        {
            let segment_val = map.segment_for_index(i);

            let get_result = map.get(i);

            assert_eq!(get_result.is_ok(), segment_val.is_some());

            let within_expected = i >= base && i < base + size;
            if within_expected
            {
                assert!(segment_val.is_some());
                assert!(get_result.unwrap().get() == 0);
            }
            else
            {
                assert!(segment_val.is_none());
            }

            let set_result = map.set(i, MemoryWord::new(set_val));

            if within_expected
            {
                assert!(set_result.is_ok());
            }
            else
            {
                assert!(set_result.is_err());
            }
        }

        // Iterate through and check if the set worked as expected
        for i in 0..MEM_MAX_SIZE
        {
            // Check the map value directly
            let map_val = map.get(i);
            if i >= base && i < base + size
            {
                assert!(map_val.is_ok());
                assert!(map_val.unwrap().get() == set_val);
            }
            else
            {
                assert!(map_val.is_err());
            }

            // Check the immutable segment value
            {
                let index_segment = map.segment_for_index(i);

                if index_segment.is_some()
                {
                    let seg_val = index_segment.unwrap().borrow().get(i);

                    assert!(seg_val.is_ok());
                    assert_eq!(seg_val.unwrap().get(), set_val);
                }
            }

            // Check the mutable segment value
            {
                let index_segment = map.segment_for_index(i);

                match index_segment
                {
                    Some(seg) =>
                    {
                        let seg_val = seg.borrow().get(i);

                        assert!(seg_val.is_ok());
                        assert_eq!(seg_val.unwrap().get(), set_val);
                    },
                    None => ()
                }
            }
        }
    }

    /// A single read-only test
    #[test]
    fn single_read_only()
    {
        // Define the memory size values
        let base = 100;
        let size = 512;

        // Define the base update function
        let calc_func = |i: usize| (i as u16) * 2;

        // Initialize the map
        let mut map = MemoryMap::new();
        let add_result = map.add_segment(Rc::new(RefCell::new(ReadOnlySegment::new(
            base,
            (0..size).map(|i| MemoryWord::new(calc_func(i))).collect()))));

        assert!(add_result.is_ok());

        let set_val = 314;

        // Iterate through and check if values are within the expected results
        for i in 0..MEM_MAX_SIZE
        {
            let segment_val = map.segment_for_index(i);
            let get_result = map.get(i);

            assert_eq!(get_result.is_ok(), segment_val.is_some());

            let within_expected = i >= base && i < base + size;
            if within_expected
            {
                assert!(segment_val.is_some());
                assert!(get_result.unwrap().get() == calc_func(i - base));
            }
            else
            {
                assert!(segment_val.is_none());
            }

            let set_result = map.set(i, MemoryWord::new(set_val));

            assert!(set_result.is_err());
        }

        // Iterate through and check if the set worked as expected
        for i in 0..MEM_MAX_SIZE
        {
            // Check the map value directly
            let map_val = map.get(i);
            if i >= base && i < base + size
            {
                assert!(map_val.is_ok());
                assert!(map_val.unwrap().get() == calc_func(i - base));
            }
            else
            {
                assert!(map_val.is_err());
            }

            // Check the immutable segment value
            {
                let index_segment = map.segment_for_index(i);

                match index_segment
                {
                    Ok(seg) =>
                    {
                        let seg_val = seg.borrow().get(i);

                        assert!(seg_val.is_ok());
                        assert_eq!(seg_val.unwrap().get(), calc_func(i - base));
                    },
                    None => ()
                }
            }

            // Check the mutable segment value
            {
                let index_segment = map.segment_for_index(i);

                match index_segment
                {
                    Some(seg) =>
                    {
                        let seg_val = seg.borrow().get(i);

                        assert!(seg_val.is_ok());
                        assert_eq!(seg_val.unwrap().get(), calc_func(i - base));
                    },
                    None => ()
                }
            }
        }
    }
}
