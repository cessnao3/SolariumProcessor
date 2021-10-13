use crate::memory::{MemoryMap, MemoryWord, MemoryWordSigned};

use super::registers::{Register, RegisterManager};

/// Defines the reset vector location
const VECTOR_RESET: MemoryWord = 0x0;

/// Defines the IRQ reset vector location
//const VECTOR_IRQ_SW: MemoryWord = 0x1;
//const VECTOR_HW_HJW: MemoryWord = 0x2;

/// Creates the Solarium CPU parameters
pub struct SolariumCPU
{
    memory_map: MemoryMap,
    registers: RegisterManager,
    allow_interrupts: bool
}

impl SolariumCPU
{
    /// Creates a new CPU parameter
    pub fn new() -> SolariumCPU
    {
        // Create the CPU
        let mut cpu = SolariumCPU
        {
            memory_map: MemoryMap::new(),
            registers: RegisterManager::new(),
            allow_interrupts: true
        };

        // Initiate the reset
        cpu.reset();

        // Return the CPU
        return cpu;
    }

    /// Resets the CPU to a known state as a hard-reset
    pub fn reset(&mut self)
    {
        self.soft_reset();
        self.memory_map.reset();
        self.allow_interrupts = true;
    }

    pub fn soft_reset(&mut self)
    {
        self.registers.reset();
        self.registers.set(
            Register::ProgramCounter,
            VECTOR_RESET);
    }

    fn get_pc_offset(&self, reg: Register) -> MemoryWord
    {
        let pc = self.registers.get(Register::ProgramCounter);
        return (pc as i32 + self.registers.get(reg) as i32) as MemoryWord;
    }

    fn increment_pc(&mut self, pc_incr: MemoryWordSigned)
    {
        let pc = self.registers.get(Register::ProgramCounter);
        let new_pc = (pc as i32 + pc_incr as i32) as MemoryWord;
        self.registers.set(
            Register::ProgramCounter,
            new_pc);
    }

    /// Step the CPU
    pub fn step(&mut self) -> Result<(), String>
    {
        // Define the current memory word
        let pc = self.registers.get(Register::ProgramCounter);
        let inst = self.memory_map.get(pc);

        // Define the PC increment
        let mut pc_incr = 1 as MemoryWordSigned;

        // Increment the PC
        self.registers.set(Register::ProgramCounter, pc);

        // Extract the different argument types
        let opcode = ((inst & 0xF000) >> 12) as u8;
        let arg0 = ((inst & 0x0F00) >> 8) as u8;
        let arg1 = ((inst & 0x00F0) >> 4) as u8;
        let arg2 = ((inst & 0x000F) >> 0) as u8;

        assert!(opcode & 0xF == opcode);
        assert!(arg0 & 0xF == arg0);
        assert!(arg1 & 0xF == arg1);
        assert!(arg2 & 0xF == arg2);

        // Define a function to combine two arguments into an item
        fn get_immediate_value(
            arg_high: u8,
            arg_low: u8) -> u8
        {
            assert!(arg_low & 0xF == arg_low);
            assert!(arg_high & 0xF == arg_high);
            return (arg_high << 4) | arg_low;
        }

        // Switch based on opcode
        if opcode == 0x0
        {
            // Determine the number of arguments for the given opcode
            if arg0 != 0
            {
                assert!(opcode == 0);

                let reg_a = Register::GP(arg2 as usize);
                let reg_b = Register::GP(arg1 as usize);

                match arg0
                {
                    1 => // jmpri
                    {
                        let immediate_val = ((arg1 as u8) << 4) | arg0 as u8;
                        pc_incr = (immediate_val as i8) as MemoryWordSigned;
                    },
                    2 => // ld
                    {
                        let reg_val = self.registers.get(reg_b);
                        let mem_val = self.memory_map.get(reg_val);
                        self.registers.set(
                            reg_a,
                            mem_val);
                    },
                    3 => // sav
                    {
                        self.memory_map.set(
                            self.registers.get(reg_a),
                            self.registers.get(reg_b));
                    },
                    4 => // ldr
                    {
                        self.memory_map.set(
                            self.registers.get(reg_a),
                            self.get_pc_offset(reg_b));
                    },
                    5 => // savr
                    {
                        self.memory_map.set(
                            self.get_pc_offset(reg_a),
                            self.registers.get(reg_b));
                    },
                    6..=9 => // jz, jzr, jgz, jgzr
                    {
                        let cmp = self.registers.get(reg_b) as MemoryWordSigned;

                        let should_jump = ((arg0 == 6 || arg0 == 7) && cmp == 0) || ((arg0 == 8 || arg0 == 9) && cmp > 0);
                        let jump_relative = arg0 == 7 || arg0 == 9;

                        if should_jump
                        {
                            if jump_relative
                            {
                                pc_incr = self.registers.get(reg_a) as MemoryWordSigned;
                            }
                            else
                            {
                                self.registers.set(
                                    Register::ProgramCounter,
                                    self.registers.get(reg_a));
                            }
                        }
                    },
                    v => // ERROR
                    {
                        return Err(format!("invalid two-argument opcode {0:}", v));
                    }
                }
            }
            else if arg1 != 0
            {
                assert!(opcode == 0);
                assert!(arg0 == 0);

                let dest_register = Register::GP(arg2 as usize);

                match arg1
                {
                    1 => // jmp
                    {
                        self.registers.set(
                            Register::ProgramCounter,
                            self.registers.get(dest_register));
                        pc_incr = 0;
                    },
                    2 => // jmpr
                    {
                        pc_incr = self.registers.get(dest_register) as MemoryWordSigned;
                    },
                    3 => // push
                    {
                        let sp = self.registers.get(Register::StackPointer);
                        self.memory_map.set(
                            sp,
                            self.registers.get(dest_register));
                        self.registers.set(
                            Register::StackPointer,
                            sp + 1);
                    },
                    4 => // popr
                    {
                        let sp = self.registers.get(Register::StackPointer) - 1;

                        self.registers.set(
                            dest_register,
                            self.memory_map.get(sp));

                        self.registers.set(
                            Register::StackPointer,
                            sp);
                    },
                    v => // ERROR
                    {
                        return Err(format!("invalid one-argument opcode {0:}", v))
                    }
                };
            }
            else
            {
                assert!(opcode == 0);
                assert!(arg0 == 0);
                assert!(arg1 == 0);

                match arg2
                {
                    0 => // noop
                    {
                        ()
                    },
                    1 => // inton
                    {
                        self.allow_interrupts = true;
                    },
                    2 => // intoff
                    {
                        self.allow_interrupts = false;
                    },
                    3 => // intcall
                    {
                        return Err("interrupt calling not yet supported".to_string());
                    }
                    4 => // reset
                    {
                        self.reset();
                        pc_incr = 0;
                    },
                    5 => // pop
                    {
                        let sp = self.registers.get(Register::StackPointer);
                        self.registers.set(
                            Register::StackPointer,
                            sp - 1);
                    },
                    v => // ERROR
                    {
                        return Err(format!("invalid no-argument opcode {0:}", v))
                    }
                };
            }
        }
        else if opcode == 1 // ldi
        {
            let immediate = get_immediate_value(
                arg0,
                arg1) as MemoryWord;

            self.registers.set(
                Register::GP(arg2 as usize),
                immediate);
        }
        else if opcode <= 11 // arithmetic
        {
            let val_a = self.registers.get(Register::GP(arg1 as usize));
            let val_b = self.registers.get(Register::GP(arg0 as usize));

            let result: MemoryWord;

            match opcode
            {
                2 => // add
                {
                    result = val_a + val_b;
                },
                3 => //sub
                {
                    result = val_a - val_b;
                },
                4 => // mul
                {
                    result = (val_a as MemoryWordSigned * val_b as MemoryWordSigned) as MemoryWord;
                },
                5 => // div
                {
                    if val_b == 0
                    {
                        return Err("divide-by-zero detected".to_string());
                    }
                    result = (val_a as MemoryWordSigned / val_b as MemoryWordSigned) as MemoryWord;
                },
                6 => // mod
                {
                    if val_b == 0
                    {
                        return Err("mod-by-zero detected".to_string());
                    }
                    result = (val_a as MemoryWordSigned % val_b as MemoryWordSigned) as MemoryWord;
                },
                7 => // band
                {
                    result = val_a & val_b;
                }
                8 => // bor
                {
                    result = val_a | val_b;
                }
                9 => //bxor
                {
                    result = val_a ^ val_b;
                }
                10 => // bsftl
                {
                    result = val_a << val_b;
                },
                11 => // bsftr
                {
                    result = val_a >> val_b;
                },
                v => // ERROR
                {
                    return Err(format!("invalid three-argument opcode {0:}", v))
                }
            }

            let reg_dest = Register::GP(arg2 as usize);
            self.registers.set(
                reg_dest,
                result);
        }

        // Increment the program counter
        self.increment_pc(pc_incr);

        // Return success
        return Ok(());
    }
}
