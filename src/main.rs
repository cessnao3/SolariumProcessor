mod memory;
mod scpu;

use memory::read_write_memory::ReadWriteMemory;

fn main()
{
    let seg1 = ReadWriteMemory::new(0, 1024);
}

