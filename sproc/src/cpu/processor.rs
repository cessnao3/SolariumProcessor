use crate::common::{MemoryWord, SolariumError, InstructionData};
use crate::memory::{MemoryMap, self};

use super::registers::{Register, RegisterManager, StatusFlag};


/// Defines the reset vector location
const VECTOR_HARD_RESET: usize = 0x0;
const VECTOR_SOFT_RESET: usize = 0x1;

/// Defines the IRQ reset vector location
const VECTOR_IRQ_SW_OFFSET: usize = 0x2;
const VECTOR_IRQ_SW_SIZE: usize = 16;
const VECTOR_HW_HJW_OFFSET: usize = 0x2 + VECTOR_IRQ_SW_SIZE;
const VECTOR_HW_HJW_SIZE: usize = 16;


/// Creates the Solarium CPU parameters
pub struct SolariumProcessor
{
    pub memory_map: MemoryMap,
    pub registers: RegisterManager
}

impl SolariumProcessor
{
    /// Provide the number of registers
    pub const NUM_REGISTERS: usize = Register::NUM_REGISTERS;

    /// Define the public overall initial data size
    pub const INIT_DATA_SIZE: usize = VECTOR_HW_HJW_OFFSET + VECTOR_HW_HJW_SIZE;

    // Define the stack pointer offset and allowed size
    pub const STACK_POINTER_OFFSET: usize = 0x400;
    pub const STACK_POINTER_MAX_SIZE: usize = 0xC00;

    /// Creates a new CPU parameter
    pub fn new() -> SolariumProcessor
    {
        // Create the CPU
        let mut cpu = SolariumProcessor
        {
            memory_map: MemoryMap::new(),
            registers: RegisterManager::new()
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

        if new_sp > SolariumProcessor::STACK_POINTER_MAX_SIZE
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
        return SolariumProcessor::STACK_POINTER_OFFSET + offset;
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
        // Return false if interrupts are not allowed
        if !self.registers.get_flag(StatusFlag::InterruptEnable)
        {
            return Ok(false);
        }

        // Obtain the desired value from the program counter
        let new_pc = match self.memory_map.get(interrupt_vector)
        {
            Ok(v) => v,
            Err(e) => return Err(e)
        };

        // Return false if the vector value is 0 (Disabled)
        if new_pc.get() == 0
        {
            return Ok(false);
        }

        // Push all register values to the stack
        match self.push_all_registers()
        {
            Ok(()) => (),
            Err(e) => return Err(e)
        };

        // Update the program counter to the value in the interrupt vector
        self.registers.set(
            Register::ProgramCounter,
            new_pc);

        // Return true if the interrupt was called
        return Ok(true);
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
        let inst = InstructionData::new(inst_word);

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
            InstructionData { opcode: 0, arg0: 0, arg1: 0, arg2: opcode } =>
            {
                match opcode
                {
                    0 => // noop
                    {
                        ()
                    },
                    1 => // inton
                    {
                        self.registers.set_flag(StatusFlag::InterruptEnable);
                    },
                    2 => // intoff
                    {
                        self.registers.clear_flag(StatusFlag::InterruptEnable);
                    },
                    3 => // reset
                    {
                        self.soft_reset();
                        pc_incr = 0;
                    },
                    4 => // ari
                    {
                        self.registers.set_flag(StatusFlag::SignedArithmetic);
                    },
                    5 => // aru
                    {
                        self.registers.clear_flag(StatusFlag::SignedArithmetic);
                    },
                    6 => // pop
                    {
                        match self.pop_sp()
                        {
                            Ok(_) => (),
                            Err(e) => return Err(e)
                        };
                    },
                    7 => // ret
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
                    8 => // retint
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
            InstructionData { opcode: 0, arg0: 0, arg1: opcode, arg2: arg0 } =>
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
                    8 | 9 => // tz, tnz
                    {
                        let reg_val = self.registers.get(reg_a).get_signed();

                        let test_passed = match opcode
                        {
                            8 => // tz
                            {
                                reg_val == 0
                            },
                            9 => // tnz
                            {
                                reg_val != 0
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
                    10 => // bool
                    {
                        let reg_val = self.registers.get(reg_a).get();
                        let new_val = if reg_val == 0
                        {
                            0
                        }
                        else
                        {
                            1
                        };
                        self.registers.set(
                            reg_a,
                            MemoryWord::new(new_val));
                    },
                    11 => // not
                    {
                        let reg_val = self.registers.get(reg_a).get();
                        let new_val = if reg_val == 0
                        {
                            1
                        }
                        else
                        {
                            0
                        };
                        self.registers.set(
                            reg_a,
                            MemoryWord::new(new_val));
                    },
                    12 => //ldn
                    {
                        // Load the memory location at the PC + 1 value
                        let pc_val = self.registers.get(Register::ProgramCounter).get();
                        let mem_val = match self.memory_map.get((pc_val + 1) as usize)
                        {
                            Ok(v) => v,
                            Err(e) => return Err(e)
                        };

                        // Save the resulting memory location in the register
                        self.registers.set(
                            reg_a,
                            mem_val);

                        // Save the PC Increment
                        pc_incr = 2;
                    },
                    _ => // ERROR
                    {
                        return Err(SolariumError::InvalidInstruction(inst_word));
                    }
                };
            },
            InstructionData { opcode: 0, arg0: opcode, arg1: arg0, arg2: arg1 } =>
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
                    7..=11 => // tg, tge, tl, tle, teq
                    {
                        let fun_tg: fn(MemoryWord, MemoryWord) -> bool;
                        let fun_tge: fn(MemoryWord, MemoryWord) -> bool;
                        let fun_tl: fn(MemoryWord, MemoryWord) -> bool;
                        let fun_tle: fn(MemoryWord, MemoryWord) -> bool;
                        let fun_teq: fn(MemoryWord, MemoryWord) -> bool = |a, b| a.get() == b.get();

                        if self.registers.get_flag(StatusFlag::SignedArithmetic)
                        {
                            fun_tg = |a, b| a.get_signed() > b.get_signed();
                            fun_tge = |a, b| a.get_signed() >= b.get_signed();
                            fun_tl = |a, b| a.get_signed() < b.get_signed();
                            fun_tle = |a, b| a.get_signed() <= b.get_signed();
                        }
                        else
                        {
                            fun_tg = |a, b| a.get() > b.get();
                            fun_tge = |a, b| a.get() >= b.get();
                            fun_tl = |a, b| a.get() < b.get();
                            fun_tle = |a, b| a.get() <= b.get();
                        }

                        let test_function = match opcode
                        {
                            7 => // tg
                            {
                                fun_tg
                            },
                            8 => //tge
                            {
                                fun_tge
                            },
                            9 => // tl
                            {
                                fun_tl
                            },
                            10 => // tle
                            {
                                fun_tle
                            },
                            11 => // teq
                            {
                                fun_teq
                            },
                            _ =>
                            {
                                panic!();
                            }
                        };

                        let word_a = self.registers.get(reg_a);
                        let word_b = self.registers.get(reg_b);

                        if test_function(word_a, word_b)
                        {
                            pc_incr = 1;
                        }
                        else
                        {
                            pc_incr = 2;
                        }
                    },
                    12 => //bnot
                    {
                        self.registers.set(
                            reg_a,
                            MemoryWord::new(!self.registers.get(reg_b).get()));
                    },
                    13 => // arg
                    {
                        // Define the backtrack values
                        let sp_subtract = 16 + 1 + arg0 as usize;
                        let sp_val = self.get_sp_offset();

                        if sp_subtract > sp_val
                        {
                            return Err(SolariumError::StackUnderflow);
                        }

                        // Load the value at the stack pointer location into the current argument
                        let mem_index = SolariumProcessor::STACK_POINTER_OFFSET + sp_val - sp_subtract;
                        let mem_val = match self.memory_map.get(mem_index)
                        {
                            Ok(v) => v,
                            Err(e) => return Err(e)
                        };

                        self.registers.set(
                            reg_a,
                            mem_val);
                    }
                    _ => // ERROR
                    {
                        return Err(SolariumError::InvalidInstruction(inst_word));
                    }
                }
            },
            InstructionData { opcode, arg0, arg1, arg2 } =>
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
                    3 => // ldri
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
                        type ArithFun = fn(MemoryWord, MemoryWord) -> Result<MemoryWord, SolariumError>;

                        let fun_add: ArithFun;
                        let fun_sub: ArithFun;
                        let fun_mul: ArithFun;
                        let fun_div: ArithFun;
                        let fun_rem: ArithFun;
                        let fun_band: ArithFun = |a, b| Ok(MemoryWord::new(a.get() & b.get()));
                        let fun_bor: ArithFun = |a, b| Ok(MemoryWord::new(a.get() | b.get()));
                        let fun_bxor: ArithFun = |a, b| Ok(MemoryWord::new(a.get() ^ b.get()));
                        let fun_bsftl: ArithFun = |a, b| {
                            let shift_count = b.get();
                            if shift_count >= memory::BITS_PER_WORD as u16
                            {
                                return Err(SolariumError::ShiftError(shift_count as usize));
                            }
                            return Ok(MemoryWord::new(a.get() << shift_count));
                        };
                        let fun_bsftr: ArithFun = |a, b| {
                            let shift_count = b.get();
                            if shift_count >= memory::BITS_PER_WORD as u16
                            {
                                return Err(SolariumError::ShiftError(shift_count as usize));
                            }
                            return Ok(MemoryWord::new(a.get() >> shift_count));
                        };

                        if self.registers.get_flag(StatusFlag::SignedArithmetic)
                        {
                            fun_add = |a, b| Ok(MemoryWord::new(a.get_signed().wrapping_add(b.get_signed()) as u16));
                            fun_sub = |a, b| Ok(MemoryWord::new(a.get_signed().wrapping_sub(b.get_signed()) as u16));
                            fun_mul = |a, b| Ok(MemoryWord::new(a.get_signed().wrapping_mul(b.get_signed()) as u16));
                            fun_div = |a, b| {
                                if b.get() == 0
                                {
                                    return Err(SolariumError::DivideByZero);
                                }
                                else
                                {
                                    return Ok(MemoryWord::new(a.get_signed().wrapping_div(b.get_signed()) as u16));
                                }
                            };
                            fun_rem = |a, b| {
                                if b.get() == 0
                                {
                                    return Err(SolariumError::ModByZero);
                                }
                                else
                                {
                                    return Ok(MemoryWord::new(a.get_signed().wrapping_rem(b.get_signed()) as u16));
                                }
                            };
                        }
                        else
                        {
                            fun_add = |a, b| Ok(MemoryWord::new(a.get().wrapping_add(b.get())));
                            fun_sub = |a, b| Ok(MemoryWord::new(a.get().wrapping_sub(b.get())));
                            fun_mul = |a, b| Ok(MemoryWord::new(a.get().wrapping_mul(b.get())));
                            fun_div = |a, b| {
                                if b.get() == 0
                                {
                                    return Err(SolariumError::DivideByZero);
                                }
                                else
                                {
                                    return Ok(MemoryWord::new(a.get().wrapping_div(b.get())));
                                }
                            };
                            fun_rem = |a, b| {
                                if b.get() == 0
                                {
                                    return Err(SolariumError::ModByZero);
                                }
                                else
                                {
                                    return Ok(MemoryWord::new(a.get().wrapping_rem(b.get())));
                                }
                            };
                        }

                        let arith_func = match opcode
                        {
                            4 => // add
                            {
                                fun_add
                            },
                            5 => //sub
                            {
                                fun_sub
                            },
                            6 => // mul
                            {
                                fun_mul
                            },
                            7 => // div
                            {
                                fun_div
                            },
                            8 => // rem
                            {
                                fun_rem
                            },
                            9 => // band
                            {
                                fun_band
                            }
                            10 => // bor
                            {
                                fun_bor
                            }
                            11 => //bxor
                            {
                                fun_bxor
                            }
                            12 => // bsftl
                            {
                                fun_bsftl
                            },
                            13 => // bsftr
                            {
                                fun_bsftr
                            },
                            _ => // ERROR
                            {
                                panic!();
                            }
                        };

                        let val_a = self.registers.get(Register::from_index(arg1 as usize));
                        let val_b = self.registers.get(Register::from_index(arg0 as usize));


                        let result = match arith_func(val_a, val_b)
                        {
                            Ok(v) => v,
                            Err(e) => return Err(e)
                        };

                        let reg_dest = Register::from_index(arg2 as usize);
                        self.registers.set(
                            reg_dest,
                            result);
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
