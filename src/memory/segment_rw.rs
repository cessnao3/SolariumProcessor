use super::*;

/// Provides a read-write memory segment type
pub struct ReadWriteSegment
{
    base_address: MemoryWord,
    top_address: MemoryWord,
    data: Vec<MemoryWord>,
}

impl ReadWriteSegment
{
    /// Defines a new memory segmetn with empty data, zero, in each memory location
    pub fn new(
        base_address: MemoryWord,
        size: MemoryWord) -> ReadWriteSegment
    {
        // Define the top address and ensure that the memory address is valid
        let top_address = (base_address + size) as MemoryWord;

        assert!(top_address >= base_address);

        // Define the initial data array
        let data: Vec::<MemoryWord> = (0..size).map(|_| 0 as MemoryWord).collect();

        // Create the memory segment
        return ReadWriteSegment
        {
            base_address,
            top_address,
            data
        };
    }
}

impl MemorySegment for ReadWriteSegment
{
    /// Provides the word at the requested memory location
    fn get(&self, ind: MemoryWord) -> MemoryWord
    {
        return if self.within(ind)
        {
            self.data[(ind - self.base_address) as usize]
        }
        else
        {
            panic!();
        };
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, ind: MemoryWord, data: MemoryWord) -> bool
    {
        if self.within(ind)
        {
            self.data[(ind - self.base_address) as usize] = data;
            return true;
        }
        else
        {
            panic!();
        }
    }

    /// Resets the memory segment
    fn reset(&mut self)
    {
        // Reset all data values to 0 if not read only
        for val in self.data.iter_mut()
        {
            *val = 0;
        }
    }

    /// Provides the starting address of the memory segment
    fn start_address(&self) -> MemoryWord
    {
        return self.base_address;
    }

    /// Provides the length of the memory segment
    fn address_len(&self) -> MemoryWord
    {
        return self.data.len() as MemoryWord;
    }

    /// Determines if the given memory index is within the memory segment
    fn within(&self, ind: MemoryWord) -> bool
    {
        return ind >= self.base_address && ind < self.top_address;
    }
}

#[cfg(test)]
mod tests {
    // Pull in the super instance
    use super::*;

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
        for i in 0..(u16::MAX as u32)
        {
            let is_within = i >= base && i < base + size;
            assert_eq!(mem.within(i), is_within);

            if is_within
            {
                assert_eq!(mem.get(i), 0);
            }
        }
    }

    #[test]
    #[should_panic]
    /// Test initialization with an invalid base and range values
    fn test_init_invalid_range()
    {
        let base = MemoryWord::MAX - 100;
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
    #[should_panic]
    /// Test setting a memory location above the top address
    fn test_panic_set_above()
    {
        let mut mem = get_default_test_segment();
        mem.set(
            mem.top_address,
            32);
    }

    #[test]
    #[should_panic]
    /// Test getting a memory location above the top address
    fn test_panic_get_above()
    {
        let mem = get_default_test_segment();
        mem.get(mem.top_address);
    }

    #[test]
    #[should_panic]
    /// Test setting a memory location below the base address
    fn test_panic_set_below()
    {
        let mut mem = get_default_test_segment();
        mem.set(
            mem.base_address - 1,
            32);
    }

    #[test]
    #[should_panic]
    /// Test getting a memory location below the base address
    fn test_panic_get_below()
    {
        let mem = get_default_test_segment();
        mem.get(mem.base_address - 1);
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
            let success = mem.set(i, i - base + 1);
            assert_eq!(success, true);
        }

        for i in 0..2048
        {
            let should_be_within = i >= base && i < (base + size);
            assert_eq!(mem.within(i), should_be_within);
        }
    }
}
