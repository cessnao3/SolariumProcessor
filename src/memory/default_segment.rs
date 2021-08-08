use super::*;

pub struct DefaultMemorySegment<const READ_ONLY: bool>
{
    base_address: MemoryWord,
    top_address: MemoryWord,
    data: Vec<MemoryWord>,
}

impl<const READ_ONLY: bool> DefaultMemorySegment::<READ_ONLY>
{
    pub fn new(
        base_address: MemoryWord,
        size: MemoryWord) -> DefaultMemorySegment::<READ_ONLY>
    {
        let top_address = (base_address + size) as MemoryWord;

        assert!(top_address >= base_address);

        let data: Vec::<MemoryWord> = (0..size).map(|_| 0 as MemoryWord).collect();

        return DefaultMemorySegment::<READ_ONLY>
        {
            base_address,
            top_address,
            data
        };
    }

    pub fn new_with_data(
        base_address: MemoryWord,
        size: MemoryWord,
        data: &[MemoryWord]) -> DefaultMemorySegment::<READ_ONLY>
    {
        let mut mem_val = DefaultMemorySegment::<READ_ONLY>::new(
            base_address,
            size);

        assert!(data.len() <= size as usize);

        mem_val.data = (0..size as usize)
            .map(|i| if i < data.len() { data[i] } else { 0 })
            .collect();

        return mem_val
    }
}

impl<const WRITEABLE: bool> MemorySegment for DefaultMemorySegment::<WRITEABLE>
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
            if WRITEABLE
            {
                self.data[(ind - self.base_address) as usize] = data;
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            panic!();
        }
    }

    fn reset(&mut self)
    {
        // Reset all data values to 0 if not read only
        if WRITEABLE
        {
            for val in self.data.iter_mut()
            {
                *val = 0;
            }
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

        let mem = DefaultMemorySegment::<false>::new(base, size);

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

        let mem = DefaultMemorySegment::<false>::new(base, size);

        for i in 0..2048
        {
            let should_be_within = i >= base && i < (base + size);
            assert_eq!(mem.within(i), should_be_within);
        }
    }
}
