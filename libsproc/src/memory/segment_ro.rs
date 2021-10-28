use crate::common::{MemoryWord, SolariumError};

use super::{MemorySegment, MAX_SEGMENT_INDEX};

/// Provides a read-write memory segment type
pub struct ReadOnlySegment
{
    base_address: usize,
    top_address:usize,
    data: Vec<MemoryWord>,
}

impl ReadOnlySegment
{
    /// Defines a new memory segment with empty data, zero, in each memory location
    pub fn new(
        base_address: usize,
        data: Vec<MemoryWord>) -> ReadOnlySegment
    {
        // Define the top address and ensure that the memory address is valid
        let top_address = base_address + data.len();

        assert!(top_address >= base_address);
        assert!(top_address <= MAX_SEGMENT_INDEX);

        // Create the memory segment
        return ReadOnlySegment
        {
            base_address,
            top_address,
            data
        };
    }
}

impl MemorySegment for ReadOnlySegment
{
    /// Provides the word at the requested memory location
    fn get(&self, ind: usize) -> Result<MemoryWord, SolariumError>
    {
        return if self.within(ind)
        {
            Ok(self.data[ind - self.base_address])
        }
        else
        {
            Err(SolariumError::InvalidMemoryAccess(ind))
        };
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, ind: usize, _: MemoryWord) -> Result<(), SolariumError>
    {
        return Err(SolariumError::InvalidMemoryWrite(ind));
    }

    /// Resets the memory segment
    fn reset(&mut self)
    {
        // Do Nothing
    }

    /// Provides the starting address of the memory segment
    fn start_address(&self) -> usize
    {
        return self.base_address;
    }

    /// Provides the length of the memory segment
    fn address_len(&self) -> usize
    {
        return self.data.len();
    }

    /// Determines if the given memory index is within the memory segment
    fn within(&self, ind: usize) -> bool
    {
        return ind >= self.base_address && ind < self.top_address;
    }
}

/*
#[cfg(test)]
mod tests {
    // Pull in the super instance
    use super::*;
    use super::super::MAX_SEGMENT_INDEX;

    #[test]
    /// Test the initialization of the memory segment
    fn test_init()
    {
        // Define the base and the size
        let base = 16;
        let size = 1024;

        // Create the segment
        let mem = ReadWriteSegment::new(base, size);

        // Ensure that the expected values match
        assert_eq!(mem.start_address(), base);
        assert_eq!(mem.address_len(), size);

        // Iterate over memory items to check that the correct values are set
        for i in 0..MAX_SEGMENT_SIZE
        {
            let is_within = i >= base && i < base + size;
            assert_eq!(mem.within(i), is_within);

            match mem.get(i)
            {
                Ok(_) => assert!(is_within),
                Err(_) => assert!(!is_within)
            };
        }
    }

    #[test]
    #[should_panic]
    /// Test initialization with an invalid base and range values
    fn test_init_invalid_range()
    {
        let base = MAX_SEGMENT_SIZE - 100;
        let size = 1024;
        ReadWriteSegment::new(base, size);
    }

    /// Provide a default memory segment for testing
    fn get_default_test_segment() -> ReadWriteSegment
    {
        let base = 256;
        let size = 1024;

        return ReadWriteSegment::new(
            base,
            size);
    }

    #[test]
    /// Test setting a memory location above the top address
    fn test_panic_set_above()
    {
        let mut mem = get_default_test_segment();
        let result = mem.set(
            mem.top_address,
            MemoryWord::new(32));
        assert!(result.is_err());
    }

    #[test]
    /// Test getting a memory location above the top address
    fn test_panic_get_above()
    {
        let mem = get_default_test_segment();
        let result = mem.get(mem.top_address);
        assert!(result.is_err());
    }

    #[test]
    /// Test setting a memory location below the base address
    fn test_panic_set_below()
    {
        let mut mem = get_default_test_segment();
        let result = mem.set(
            mem.base_address - 1,
            MemoryWord::new(32));
        assert!(result.is_err());
    }

    #[test]
    /// Test getting a memory location below the base address
    fn test_panic_get_below()
    {
        let mem = get_default_test_segment();
        let result = mem.get(mem.base_address - 1);
        assert!(result.is_err());
    }

    #[test]
    /// Test the initial base offset value
    fn test_offset_base()
    {
        let base = 256;
        let size = 1024;

        let mut mem = ReadWriteSegment::new(base, size);

        for i in base..(base + size)
        {
            let success = mem.set(i, MemoryWord::new((i - base + 1) as u16));
            assert_eq!(success.is_ok(), true);
        }

        for i in 0..2048
        {
            let should_be_within = i >= base && i < (base + size);
            assert_eq!(mem.within(i), should_be_within);
        }
    }
}
*/
