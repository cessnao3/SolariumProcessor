use crate::memory::*;

pub struct ReadWriteMemory
{
    base: MemoryIndex,
    top: MemoryIndex,
    data: Vec<MemoryWord>
}

impl ReadWriteMemory
{
    pub fn new(base_addr: MemoryIndex, size: MemorySize) -> ReadWriteMemory
    {
        let top_addr = (base_addr + size) as MemoryIndex;
        if top_addr < base_addr
        {
            panic!();
        }

        let data_vec: Vec::<MemoryWord> = (0..size).map(|_| 0 as MemoryWord).collect();

        return ReadWriteMemory
        {
            base: base_addr,
            top: top_addr,
            data: data_vec
        };
    }
}

impl MemorySegment for ReadWriteMemory
{
    fn get(&self, ind: MemoryIndex) -> MemoryWord
    {
        return if self.within(ind)
        {
            self.data[(ind - self.base) as usize]
        }
        else
        {
            panic!();
        };
    }

    fn set(&mut self, ind: MemoryIndex, data: MemoryWord) -> bool
    {
        if self.within(ind)
        {
            self.data[(ind - self.base) as usize] = data;
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

    fn start_address(&self) -> MemoryIndex
    {
        return self.base;
    }

    fn address_len(&self) -> MemoryIndex
    {
        return self.data.len() as MemoryIndex;
    }

    fn within(&self, ind: MemoryIndex) -> bool
    {
        return ind >= self.base && ind < self.top;
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
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
