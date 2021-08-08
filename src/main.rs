mod cpu;
mod memory;
mod assembler;

use memory::default_segment::DefaultMemorySegment;

use crate::memory::MemorySegment;

fn main()
{
    // Setup a memory test
    let mut mem_val = DefaultMemorySegment::<true>::new(0, 1024);
    mem_val.set(5, 32);

    let box_val: Box<dyn memory::MemorySegment> = Box::new(mem_val);

    println!("Memory Test: {0}", box_val.get(5));

    println!("Assembly Test");
    let line_test = vec!{
        "copy r5 $6",
        "jmp 63",
        "jmp -64",
        "jg r4 r5 r8", // TODO - Allow address-based registers here?
    };

    match assembler::assemble(line_test)
    {
        Ok(_) => println!("Assembled Successfully"),
        Err(e) => println!(" Error assembling: {0:}", e)
    };

    println!("Initializing CPU");
    let mut cpu = cpu::processor::SolariumCPU::new();
    for i in 0..100
    {
        println!("Step {0:}", i + 1);
        cpu.step();
    }
}
