type MemoryWord = u16;
type MemoryIndex = u32;

trait MemorySegment
{
    fn get(&self, ind: MemoryIndex) -> MemoryWord;

    fn set(&mut self, ind: MemoryIndex, data: MemoryWord) -> bool;

    fn start_address(&self) -> MemoryIndex;

    fn address_len(&self) -> MemoryIndex;

    fn within(&self, ind: MemoryIndex) -> bool;
}

struct ReadWriteMemory
{
    base: MemoryIndex,
    top: MemoryIndex,
    data: Vec<MemoryWord>
}

impl ReadWriteMemory
{
    fn new(base_addr: MemoryIndex, size: usize) -> ReadWriteMemory
    {
        let top_addr = (base_addr as usize + size) as MemoryIndex;
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
            0
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
            return false;
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

fn main() {
    println!("Hello, world!");
}

