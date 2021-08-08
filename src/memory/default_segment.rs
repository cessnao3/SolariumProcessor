use super::*;

pub struct DefaultMemorySegment<const WRITEABLE: bool>
{
    base_address: MemoryWord,
    top_address: MemoryWord,
    data: Vec<MemoryWord>,
}

impl<const WRITEABLE: bool> DefaultMemorySegment::<WRITEABLE>
{
    pub fn new(
        base_address: MemoryWord,
        size: MemoryWord) -> DefaultMemorySegment::<WRITEABLE>
    {
        let top_address = (base_address + size) as MemoryWord;

        assert!(top_address >= base_address);

        let data: Vec::<MemoryWord> = (0..size).map(|_| 0 as MemoryWord).collect();

        return DefaultMemorySegment::<WRITEABLE>
        {
            base_address,
            top_address,
            data
        };
    }

    pub fn new_with_data(
        base_address: MemoryWord,
        size: MemoryWord,
        data: &[MemoryWord]) -> DefaultMemorySegment::<WRITEABLE>
    {
        let mut mem_val = DefaultMemorySegment::<WRITEABLE>::new(
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
        let base = 16;
        let size = 1024;

        let mem = DefaultMemorySegment::<false>::new(base, size);

        assert_eq!(mem.start_address(), base);
        assert_eq!(mem.address_len(), size);

        for i in 0..base
        {
            assert_eq!(mem.within(i), false);
        }

        for i in base..(base + size as u32)
        {
            assert_eq!(mem.within(i), true);
            assert_eq!(mem.get(i), 0);
        }
        assert_eq!(mem.within(base + size), false);
    }

    #[test]
    #[should_panic]
    fn test_init_invalid_values()
    {
        let base = MemoryWord::MAX - 100;
        let size = 1024;
        DefaultMemorySegment::<false>::new(base, size);
    }

    #[test]
    fn test_init_data_ro()
    {
        let base = 0;
        let size = 1024;
        let data_size = 512;

        let expected_data: Vec<MemoryWord> = (0..data_size)
            .map(|v| v + 1)
            .collect();

        let mut mem = DefaultMemorySegment::<false>::new_with_data(
            base,
            size,
            &expected_data);

        for i in 0..size
        {
            assert_eq!(mem.within(base + i), true);
            if i < data_size
            {
                assert_eq!(mem.get(i), expected_data[i as usize]);
            }
            else
            {
                assert_eq!(mem.get(i), 0);
            }
        }

        mem.reset();

        for i in 0..size
        {
            assert_eq!(mem.within(base + i), true);
            if i < data_size
            {
                assert_eq!(mem.get(i), expected_data[i as usize]);
            }
            else
            {
                assert_eq!(mem.get(i), 0);
            }
        }
    }

    #[test]
    fn test_init_data_rw()
    {
        let base = 0;
        let size = 1024;
        let data_size = 512;

        let expected_data: Vec<MemoryWord> = (0..data_size)
            .map(|v| v + 1)
            .collect();

        let mut mem = DefaultMemorySegment::<true>::new_with_data(
            base,
            size,
            &expected_data);

        for i in 0..size
        {
            assert_eq!(mem.within(base + i), true);
            let val = mem.get(base + i);
            if i < data_size
            {
                assert_eq!(val, expected_data[i as usize]);
            }
            else
            {
                assert_eq!(val, 0);
            }
        }

        mem.reset();

        for i in 0..size
        {
            assert_eq!(mem.within(base + i), true);
            assert_eq!(mem.get(base + i), 0);
        }
    }

    #[test]
    #[should_panic]
    fn test_init_data_overflow()
    {
        let base = 0;
        let size = 1024;

        let expected_data: Vec<MemoryWord> = (0..size*2)
            .map(|v| v)
            .collect();

        let mut mem = DefaultMemorySegment::<false>::new_with_data(
            base,
            size,
            &expected_data);
        mem.reset();
    }

    #[test]
    #[should_panic]
    fn test_panic_set_above_ro()
    {
        let base = 0;
        let size = 1024;

        let mut mem = DefaultMemorySegment::<false>::new(base, size);

        mem.set(size, 32);
    }

    #[test]
    #[should_panic]
    fn test_panic_get_above_ro()
    {
        let base = 0;
        let size = 1024;

        let mem = DefaultMemorySegment::<false>::new(base, size);
        mem.get(size);
    }

    #[test]
    #[should_panic]
    fn test_panic_set_below_ro()
    {
        let base = 1024;
        let size = 1024;

        let mut mem = DefaultMemorySegment::<false>::new(base, size);

        mem.set(base - 1, 32);
    }

    #[test]
    #[should_panic]
    fn test_panic_get_below_ro()
    {
        let base = 1024;
        let size = 1024;

        let mem = DefaultMemorySegment::<false>::new(base, size);
        mem.get(base - 1);
    }

    #[test]
    #[should_panic]
    fn test_panic_set_above_rw()
    {
        let base = 0;
        let size = 1024;

        let mut mem = DefaultMemorySegment::<true>::new(base, size);

        mem.set(size, 32);
    }

    #[test]
    #[should_panic]
    fn test_panic_get_above_rw()
    {
        let base = 0;
        let size = 1024;

        let mem = DefaultMemorySegment::<true>::new(base, size);
        mem.get(size);
    }

    #[test]
    #[should_panic]
    fn test_panic_set_below_rw()
    {
        let base = 1024;
        let size = 1024;

        let mut mem = DefaultMemorySegment::<true>::new(base, size);

        mem.set(base - 1, 32);
    }

    #[test]
    #[should_panic]
    fn test_panic_get_below_rw()
    {
        let base = 1024;
        let size = 1024;

        let mem = DefaultMemorySegment::<true>::new(base, size);
        mem.get(base - 1);
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
