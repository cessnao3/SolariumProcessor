use crate::common::{MemoryWord, SolariumError, InstructionGroup};
use crate::memory::MemoryMap;

use super::registers::{Register, RegisterManager};


/// Defines the reset vector location
const VECTOR_HARD_RESET: usize = 0x0;
const VECTOR_SOFT_RESET: usize = 0x1;

/// Defines the IRQ reset vector location
const VECTOR_IRQ_SW_OFFSET: usize = 0x2;
const VECTOR_IRQ_SW_SIZE: usize = 16;
const VECTOR_HW_HJW_OFFSET: usize = 0x2 + VECTOR_IRQ_SW_SIZE;
const VECTOR_HW_HJW_SIZE: usize = 16;

// Define the stack pointer offset and allowed size
const STACK_POINTER_OFFSET: usize = 0x400;
const STACK_POINTER_MAX_SIZE: usize = 0xC00;


/// Creates the Solarium CPU parameters
pub struct SolariumProcessor
{
    pub memory_map: MemoryMap,
    pub registers: RegisterManager,
    allow_interrupts: bool
}

impl SolariumProcessor
{
    /// Provide the number of registers
    pub const NUM_REGISTERS: usize = Register::NUM_REGISTERS;

    /// Define the public overall initial data size
    pub const INIT_DATA_SIZE: usize = VECTOR_HW_HJW_OFFSET + VECTOR_HW_HJW_SIZE;

    /// Creates a new CPU parameter
    pub fn new() -> SolariumProcessor
    {
        // Create the CPU
        let mut cpu = SolariumProcessor
        {
            memory_map: MemoryMap::new(),
            registers: RegisterManager::new(),
            allow_interrupts: true
        };

        // Initiate the reset
        cpu.hard_reset();

        // Return the CPU
        return cpu;
    }

    /// Provides common functionality between soft and hard reset vectors, while providing
    /// the reset vector as an input to distinguish between the two. Additionally,
    /// the interrupts will be set to allowed
    fn inner_reset(&mut self, reset_vector: usize)
    {
        let reset_loc = match self.memory_map.get(reset_vector)
        {
            Ok(v) => v,
            Err(_) => MemoryWord::new(0)
        };

        self.registers.reset();
        self.registers.set(
            Register::ProgramCounter,
            reset_loc);

        self.allow_interrupts = true;
    }

    /// Resets the CPU and memory to a known state as a hard-reset
    pub fn hard_reset(&mut self)
    {
        self.inner_reset(VECTOR_HARD_RESET);
        self.memory_map.reset();
    }

    /// Sofr-resets only the registers, but leaves the memory values intact
    pub fn soft_reset(&mut self)
    {
        self.inner_reset(VECTOR_SOFT_RESET);
    }

    /// Obtains the current value of a register
    pub fn get_register_value(&self, index: usize) -> MemoryWord
    {
        return self.registers.get(Register::from_index(index));
    }

    /// Obtains the current program counter offset
    fn get_pc_offset(&self, reg: Register) -> MemoryWord
    {
        let pc = self.registers.get(Register::ProgramCounter).get();
        return MemoryWord::new((pc as i32 + self.registers.get(reg).get() as i32) as u16);
    }

    /// Increments the program counter by the specified amount
    fn increment_pc(&mut self, pc_incr: i32)
    {
        let pc = self.registers.get(Register::ProgramCounter).get();
        let new_pc = (pc as i32 + pc_incr) as u16;
        self.registers.set(
            Register::ProgramCounter,
            MemoryWord::new(new_pc));
    }

    /// Obtains the current value of the stack pointer offset from the initial stack location
    fn get_sp_offset(&self) -> usize
    {
        return self.registers.get(Register::StackPointer).get() as usize;
    }

    /// Pushes a value onto the stack
    fn push_sp(&mut self, value: MemoryWord) -> Result<(), SolariumError>
    {
        let current_sp = self.get_sp_offset();
        let new_sp = current_sp + 1;

        if new_sp > STACK_POINTER_MAX_SIZE
        {
            return Err(SolariumError::StackOverflow);
        }
        else
        {
            self.registers.set(
                Register::StackPointer,
                MemoryWord::new(new_sp as u16));

            return self.memory_map.set(
                self.get_sp_address_for_offset(current_sp),
                value);
        }
    }

    /// Provide the resulting stack pointer address for the given offset
    fn get_sp_address_for_offset(&self, offset: usize) -> usize
    {
        return STACK_POINTER_OFFSET + offset;
    }

    /// Pops a value off of the stack and returns the result
    fn pop_sp(&mut self) -> Result<MemoryWord, SolariumError>
    {
        // Attempt to get the current location
        let ret_val = match self.peek_sp()
        {
            Ok(v) => v,
            Err(e) => return Err(e)
        };

        // Subtract one from the stack pointer
        self.registers.set(
            Register::StackPointer,
            MemoryWord::new(self.get_sp_offset() as u16 - 1));

        // Return the result
        return Ok(ret_val);
    }

    /// Peeks at the value currently on the top of the stack
    fn peek_sp(&self) -> Result<MemoryWord, SolariumError>
    {
        if self.get_sp_offset() == 0
        {
            return Err(SolariumError::StackUnderflow);
        }
        else
        {
            return self.memory_map.get(self.get_sp_address_for_offset(self.get_sp_offset() - 1));
        }
    }

    pub fn hardware_interrupt(&mut self, hw_irq_num: usize) -> Result<bool, SolariumError>
    {
        // Ensure that the hardware IRQ number is valid
        if hw_irq_num >= VECTOR_HW_HJW_SIZE
        {
            return Err(SolariumError::InvalidHardwareInterrupt(hw_irq_num));
        }

        // Call the interrupt and return the result
        return self.call_interrupt(VECTOR_HW_HJW_OFFSET + hw_irq_num);
    }

    /// Calls the interrupt at the provided interrupt vector if interrupts are enabled
    fn call_interrupt(&mut self, interrupt_vector: usize) -> Result<bool, SolariumError>
    {
        if self.allow_interrupts
        {
            // Push all register values to the stack
            match self.push_all_registers()
            {
                Ok(()) => (),
                Err(e) => return Err(e)
            };

            // Obtain the desired value from the program counter
            let new_pc = match self.memory_map.get(interrupt_vector)
            {
                Ok(v) => v,
                Err(e) => return Err(e)
            };

            // Update the program counter to the value in the interrupt vector
            self.registers.set(
                Register::ProgramCounter,
                new_pc);

            // Return true if the interrupt was called
            return Ok(true);
        }
        else
        {
            // Return false if no interrupt was called
            return Ok(false);
        }
    }

    /// Pushes all register values onto the stack
    fn push_all_registers(&mut self) -> Result<(), SolariumError>
    {
        // Push all the existing register values
        for i in 0..Self::NUM_REGISTERS
        {
            match self.push_sp(self.registers.get(Register::GP(i)))
            {
                Ok(()) => (),
                Err(e) => return Err(e)
            };
        }

        return Ok(());
    }

    /// Pops all register values from the stack back into the register values
    fn pop_all_registers(&mut self) -> Result<(), SolariumError>
    {
        for i in 0..Self::NUM_REGISTERS
        {
            let mem_val = match self.pop_sp()
            {
                Ok(v) => v,
                Err(e) => return Err(e)
            };

            self.registers.set(
                Register::GP(Self::NUM_REGISTERS - 1 - i),
                mem_val);
        }

        return Ok(());
    }

    /// Step the CPU
    pub fn step(&mut self) -> Result<(), SolariumError>
    {
        // Define the current memory word
        let pc = self.registers.get(Register::ProgramCounter);
        let inst_word = match self.memory_map.get(pc.get() as usize)
        {
            Ok(v) => v,
            Err(e) => return Err(e)
        };

        // Define the PC increment
        let mut pc_incr = 1i32;

        // Increment the PC
        self.registers.set(Register::ProgramCounter, pc);

        // Extract the different argument types
        let inst = InstructionGroup::new(inst_word);

        // Define a function to combine two arguments into an item
        fn get_immediate_value_signed(
            arg_high: u8,
            arg_low: u8) -> MemoryWord
        {
            assert!(arg_low & 0xF == arg_low);
            assert!(arg_high & 0xF == arg_high);
            return MemoryWord::new(((((arg_high << 4) | arg_low) as i8) as i16) as u16);
        }

        fn get_immediate_value_unsigned(
            arg_high: u8,
            arg_low: u8) -> MemoryWord
        {
            assert!(arg_low & 0xF == arg_low);
            assert!(arg_high & 0xF == arg_high);
            return MemoryWord::new(((arg_high << 4) | arg_low) as u16);
        }

        // Switch based on opcode
        match inst
        {
            InstructionGroup { opcode: 0, arg0: 0, arg1: 0, arg2: opcode } =>
            {
                match opcode
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
                    3 => // reset
                    {
                        self.soft_reset();
                        pc_incr = 0;
                    },
                    4 => // pop
                    {
                        match self.pop_sp()
                        {
                            Ok(_) => (),
                            Err(e) => return Err(e)
                        };
                    },
                    5 => // ret
                    {
                        // Save the return register
                        let ret_register = self.registers.get(Register::ReturnValue);

                        // Pop all register values
                        match self.pop_all_registers()
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };

                        // Copy the new return register value
                        self.registers.set(
                            Register::ReturnValue,
                            ret_register);

                        // Set no PC increment so that we start at the first instruction value
                        pc_incr = 0;
                    },
                    6 => // retint
                    {
                        // Pop all register values
                        match self.pop_all_registers()
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };

                        // Set no PC increment to ensure that we don't loose track of the current value
                        pc_incr = 0;
                    },
                    _ => // ERROR
                    {
                        return Err(SolariumError::InvalidInstruction(inst_word));
                    }
                };
            },
            InstructionGroup { opcode: 0, arg0: 0, arg1: opcode, arg2: arg0 } =>
            {
                let reg_a = Register::from_index(arg0 as usize);

                match opcode
                {
                    1 => // jmp
                    {
                        self.registers.set(
                            Register::ProgramCounter,
                            self.registers.get(reg_a));
                        pc_incr = 0;
                    },
                    2 => // jmpr
                    {
                        pc_incr = self.registers.get(reg_a).get_signed() as i32;
                    },
                    3 => // push
                    {
                        match self.push_sp(self.registers.get(reg_a))
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };
                    },
                    4 => // popr
                    {
                        match self.pop_sp()
                        {
                            Ok(val) =>
                            {
                                self.registers.set(
                                    reg_a,
                                    val);
                            },
                            Err(e) => return Err(e)
                        };
                    },
                    5 => // call
                    {
                        // Increment the PC so that the new value is pushed onto the stack
                        self.increment_pc(1);

                        // Push all registers to the stack
                        match self.push_all_registers()
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };

                        // Move to the new location
                        let new_loc = self.registers.get(reg_a);
                        self.registers.set(
                            Register::ProgramCounter,
                            new_loc);

                        // Ensure that we run the first instruction at the new location
                        pc_incr = 0;
                    },
                    6 | 7 => // int, intr
                    {
                        // Determine the interrupt vector value
                        let int_offset = if opcode == 6
                        {
                            reg_a.to_index()
                        }
                        else if opcode == 7
                        {
                            self.registers.get(reg_a).get() as usize
                        }
                        else
                        {
                            panic!();
                        };

                        // Return error if the interrupt offset is invalid
                        if int_offset >= VECTOR_IRQ_SW_SIZE
                        {
                            return Err(SolariumError::InvalidSoftwareInterrupt(int_offset))
                        }

                        // Increment the program counter
                        self.increment_pc(1);

                        // Otherwise, trigger interrupt
                        match self.call_interrupt(int_offset + VECTOR_IRQ_SW_OFFSET)
                        {
                            Ok(_) => (),
                            Err(e) => return Err(e)
                        };

                        // Disable the PC increment
                        pc_incr = 0;
                    },
                    8 | 9 | 10 => // tz, tgz, tlz
                    {
                        let reg_val = self.registers.get(reg_a).get_signed();

                        let test_passed = match opcode
                        {
                            8 => // tz
                            {
                                reg_val == 0
                            },
                            9 => // tgz
                            {
                                reg_val > 0
                            },
                            10 => //tlz
                            {
                                reg_val < 0
                            },
                            _ =>
                            {
                                panic!();
                            }
                        };

                        if test_passed
                        {
                            pc_incr = 1;
                        }
                        else
                        {
                            pc_incr = 2;
                        }
                    },
                    _ => // ERROR
                    {
                        return Err(SolariumError::InvalidInstruction(inst_word));
                    }
                };
            },
            InstructionGroup { opcode: 0, arg0: opcode, arg1: arg0, arg2: arg1 } =>
            {
                let reg_a = Register::from_index(arg1 as usize);
                let reg_b = Register::from_index(arg0 as usize);

                match opcode
                {
                    1 => // jmpri
                    {
                        pc_incr = get_immediate_value_signed(
                            arg0,
                            arg1).get_signed() as i32;
                    },
                    2 => // ld
                    {
                        let reg_val = self.registers.get(reg_b);
                        let mem_val = match self.memory_map.get(reg_val.get() as usize)
                        {
                            Ok(v) => v,
                            Err(e) => return Err(e)
                        };

                        self.registers.set(
                            reg_a,
                            mem_val);
                    },
                    3 => // sav
                    {
                        match self.memory_map.set(
                            self.registers.get(reg_a).get() as usize,
                            self.registers.get(reg_b))
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };
                    },
                    4 => // ldr
                    {
                        match self.memory_map.set(
                            self.registers.get(reg_a).get() as usize,
                            self.get_pc_offset(reg_b))
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };
                    },
                    5 => // savr
                    {
                        match self.memory_map.set(
                            self.get_pc_offset(reg_a).get() as usize,
                            self.registers.get(reg_b))
                        {
                            Ok(()) => (),
                            Err(e) => return Err(e)
                        };
                    },
                    6 => // copy
                    {
                        self.registers.set(
                            reg_a,
                            self.registers.get(reg_b));
                    }
                    7..=10 => // tg, tge, tl, tle
                    {
                        let val_a = self.registers.get(reg_a).get_signed();
                        let val_b = self.registers.get(reg_b).get_signed();

                        let test_passed = match opcode
                        {
                            7 => // tg
                            {
                                val_a > val_b
                            },
                            8 => //tge
                            {
                                val_a >= val_b
                            },
                            9 => // tl
                            {
                                val_a < val_b
                            },
                            10 => // tle
                            {
                                val_a <= val_b
                            },
                            _ =>
                            {
                                panic!();
                            }
                        };

                        if test_passed
                        {
                            pc_incr = 1;
                        }
                        else
                        {
                            pc_incr = 2;
                        }
                    },
                    _ => // ERROR
                    {
                        return Err(SolariumError::InvalidInstruction(inst_word));
                    }
                }
            },
            InstructionGroup { opcode, arg0, arg1, arg2 } =>
            {
                match opcode
                {
                    1 | 2 => // ldi, ldui
                    {
                        let immediate = match opcode
                        {
                            1 =>  get_immediate_value_signed(arg0, arg1) as MemoryWord,
                            2 => get_immediate_value_unsigned(arg0, arg1),
                            _ => return Err(SolariumError::InvalidInstruction(inst_word))
                        };

                        self.registers.set(
                            Register::from_index(arg2 as usize),
                            immediate);
                    },
                    3 => // ldir
                    {
                        let immediate = get_immediate_value_signed(arg0, arg1);

                        let load_loc = pc.get() as i32 + immediate.get_signed() as i32;

                        assert!(load_loc >= 0);

                        let mem_val = match self.memory_map.get(load_loc as usize)
                        {
                            Ok(v) => v,
                            Err(e) => return Err(e)
                        };

                        self.registers.set(
                            Register::from_index(arg2 as usize),
                            mem_val);
                    },
                    opcode if opcode <= 13 =>
                    {
                        let val_a = self.registers.get(Register::from_index(arg1 as usize));
                        let val_b = self.registers.get(Register::from_index(arg0 as usize));

                        let result: u16;

                        match opcode
                        {
                            4 => // add
                            {
                                result = val_a.get() + val_b.get();
                            },
                            5 => //sub
                            {
                                result = val_a.get() - val_b.get();
                            },
                            6 => // mul
                            {
                                result = (val_a.get_signed() * val_b.get_signed()) as u16;
                            },
                            7 => // div
                            {
                                if val_b.get() == 0
                                {
                                    return Err(SolariumError::DivideByZero);
                                }
                                result = (val_a.get_signed() / val_b.get_signed()) as u16;
                            },
                            8 => // mod
                            {
                                if val_b.get() == 0
                                {
                                    return Err(SolariumError::ModByZero);
                                }
                                result = (val_a.get_signed() % val_b.get_signed()) as u16;
                            },
                            9 => // band
                            {
                                result = val_a.get() & val_b.get();
                            }
                            10 => // bor
                            {
                                result = val_a.get() | val_b.get();
                            }
                            11 => //bxor
                            {
                                result = val_a.get() ^ val_b.get();
                            }
                            12 => // bsftl
                            {
                                let shift_count = val_b.get();
                                if shift_count >= 16
                                {
                                    return Err(SolariumError::ShiftError(shift_count as usize));
                                }
                                result = val_a.get() << shift_count;
                            },
                            13 => // bsftr
                            {
                                let shift_count = val_b.get();
                                if shift_count >= 16
                                {
                                    return Err(SolariumError::ShiftError(shift_count as usize));
                                }
                                result = val_a.get() >> shift_count;
                            },
                            _ => // ERROR
                            {
                                panic!();
                            }
                        }

                        let reg_dest = Register::from_index(arg2 as usize);
                        self.registers.set(
                            reg_dest,
                            MemoryWord::new(result));
                    },
                    _ =>
                    {
                        return Err(SolariumError::InvalidInstruction(inst_word))
                    }
                }
            }
        };

        // Increment the program counter
        self.increment_pc(pc_incr);

        // Return success
        return Ok(());
    }
}
