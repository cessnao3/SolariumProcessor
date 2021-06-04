mod cpu;
mod memory;
mod assembler;

//use memory::read_write_memory::ReadWriteMemory;

fn main()
{
    println!("Assembly Test");
    let line_test = vec!{
        "copy r5 $6",
        "jmp 16",
        "jg r4 r5 r8", // TODO - Allow address-based registers here?
    };

    match assembler::assemble(line_test)
    {
        Ok(_) => println!("Assembled Successfully"),
        Err(e) => println!(" Error assembling: {0:}", e)
    };

    println!("Initializing CPU");
    let mut cpu = cpu::processor::SolariumCPU::new();
    cpu.step();
}
