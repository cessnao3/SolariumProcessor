use crate::memory::*;

pub struct ReadOnlyMemory
{
    base_address: MemoryWord,
    top_address: MemoryWord,
    data: Vec<MemoryWord>
}

impl ReadOnlyMemory
{
    pub fn new(base_address: MemoryWord, size: MemoryWord, data: &[MemoryWord]) -> ReadOnlyMemory
    {
        let top_address = (base_address + size) as MemoryWord;

        assert!(top_address >= base_address);
        assert!(data.len() <= size as usize);

        let data: Vec::<MemoryWord> = (0..size as usize)
            .map(|i| if i < data.len() { data[i] } else { 0 } as MemoryWord)
            .collect();

        return ReadOnlyMemory
        {
            base_address,
            top_address,
            data
        };
    }
}

impl MemorySegment for ReadOnlyMemory
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

    fn set(&mut self, ind: MemoryWord, _data: MemoryWord) -> bool
    {
        if self.within(ind)
        {
            return false;
        }
        else
        {
            panic!();
        }
    }

    fn reset(&mut self)
    {
        // Do Nothing
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
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_init()
    {
        let base = 0 as MemoryWord;
        let size = 1024 as MemoryWord;

        let data_in: Vec<MemoryWord> = (0..20).map(|v| v as MemoryWord).collect();

        let mem = ReadOnlyMemory::new(base, size, &data_in);

        assert_eq!(mem.start_address(), base);
        assert_eq!(mem.address_len(), size);

        for i in base..(base + size as u32)
        {
            assert_eq!(mem.within(i), true);
        }
        assert_eq!(mem.within(1024), false);

        for i in base as usize .. size as usize
        {
            let mem_val = mem.get(i as MemoryWord);

            if i < data_in.len()
            {
                assert_eq!(data_in[i], mem_val);
            }
            else
            {
                assert_eq!(0, mem_val);
            }
        }
    }

    #[test]
    fn test_offset_base()
    {
        let base = 256;
        let size = 1024;

        let data_in: Vec<MemoryWord> = (0..size as usize)
            .map(|v| v as MemoryWord)
            .collect();


        let mem = ReadOnlyMemory::new(base, size, &data_in);

        for i in 0..2048
        {
            let should_be_within = i >= base && i < (base + size);
            assert_eq!(mem.within(i), should_be_within);
        }
    }
}
