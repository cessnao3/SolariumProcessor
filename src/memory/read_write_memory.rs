use crate::memory::*;

pub struct ReadWriteMemory
{
    base_address: MemoryWord,
    top_address: MemoryWord,
    data: Vec<MemoryWord>
}

impl ReadWriteMemory
{
    pub fn new(base_address: MemoryWord, size: MemoryWord) -> ReadWriteMemory
    {
        let top_address = (base_address + size) as MemoryWord;

        assert!(top_address >= base_address);

        let data: Vec::<MemoryWord> = (0..size).map(|_| 0 as MemoryWord).collect();

        return ReadWriteMemory
        {
            base_address,
            top_address,
            data
        };
    }
}

impl MemorySegment for ReadWriteMemory
{
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

    fn reset(&mut self)
    {
        // Reset all data values to 0
        for val in self.data.iter_mut()
        {
            *val = 0;
        }
    }

    fn start_address(&self) -> MemoryWord
    {
        return self.base_address;
    }

    fn address_len(&self) -> MemoryWord
    {
        return self.data.len() as MemoryWord;
    }

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
    fn test_init()
    {
        let base = 0;
        let size = 1024;

        let mem = ReadWriteMemory::new(base, size);

        assert_eq!(mem.start_address(), base);
        assert_eq!(mem.address_len(), size);

        for i in base..(base + size as u32)
        {
            assert_eq!(mem.within(i), true);
        }
        assert_eq!(mem.within(1024), false);
    }

    #[test]
    fn test_offset_base()
    {
        let base = 256;
        let size = 1024;

        let mem = ReadWriteMemory::new(base, size);

        for i in 0..2048
        {
            let should_be_within = i >= base && i < (base + size);
            assert_eq!(mem.within(i), should_be_within);
        }
    }
}
