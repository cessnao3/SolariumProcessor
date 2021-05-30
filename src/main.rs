mod memory;
mod scpu;

use memory::read_write_memory::ReadWriteMemory;

fn main()
{
    println!("Initializing CPU");
    let scpu = scpu::SolariumCPU::new();
}

