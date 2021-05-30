mod memory;
mod cpu;

use memory::read_write_memory::ReadWriteMemory;

fn main()
{
    println!("Initializing CPU");
    let scpu = cpu::processor::SolariumCPU::new();
}

