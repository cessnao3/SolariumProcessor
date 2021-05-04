type data_t = u16;
type ind_t = u32;

trait MemorySegment
{
    fn get(&self, ind: ind_t) -> data_t;

    fn set(&mut self, ind: ind_t, data: data_t) -> bool;

    fn start_address(&self) -> ind_t;

    fn address_len(&self) -> usize;

    fn within(&self, ind: ind_t) -> bool;
}

struct ReadWriteMemory
{
    base: ind_t,
    data: Vec<data_t>
}

impl ReadWriteMemory
{
    fn new(base_addr: ind_t, size: ind_t) -> ReadWriteMemory
    {
        if ((base_addr as usize) + (size as usize)) as ind_t > ind_t::MAX
        {
            panic!();
        }

        let mut data_vec = Vec::<data_t>::new();
        for _ in 0..size
        {
            data_vec.push(0);
        }

        return ReadWriteMemory
        {
            base: base_addr,
            data: data_vec
        };
    }
}


impl MemorySegment for ReadWriteMemory
{
    fn get(&self, ind: ind_t) -> data_t
    {
        if (ind as usize) < self.data.len()
        {
            return self.data[ind as usize];
        }
        else
        {
            return 0;
        }
    }

    fn set(&mut self, ind: ind_t, data: data_t) -> bool
    {
        if (ind as usize) < self.data.len()
        {
            self.data[ind as usize] = data;
            return true;
        }
        else
        {
            return false;
        }
    }

    fn start_address(&self) -> ind_t
    {
        return self.base;
    }

    fn address_len(&self) -> usize
    {
        return self.data.len();
    }

    fn within(&self, ind: ind_t) -> bool
    {
        return ind >= self.base && ind <= (self.base as usize + self.data.len()) as ind_t;
    }
}

fn main() {
    println!("Hello, world!");
}

