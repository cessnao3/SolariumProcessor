mod cpu;
mod memory;
mod assembler;

//use memory::read_write_memory::ReadWriteMemory;

fn main()
{
    println!("Initializing CPU");
    let mut cpu = cpu::processor::SolariumCPU::new();
    cpu.step();
}
