use crate::memory::{MemoryWord, MemoryWordSigned};
use crate::memory::memory_map::MemoryMap;

use crate::cpu::registers::{Register, RegisterManager};

/// Defines the reset vector location
const VECTOR_RESET: MemoryWord = 0x400;

/// Defines the IRQ reset vector location
//const VECTOR_IRQ: MemoryWord = 0x401;

/// Creates the Solarium CPU parameters
pub struct SolariumCPU
{
    memory_map: MemoryMap,
    registers: RegisterManager
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
            registers: RegisterManager::new()
        };

        // Initiate the reset
        cpu.reset();

        // Return the CPU
        return cpu;
    }

    /// Resets the CPU to a known state as a hard-reset
    pub fn reset(&mut self)
    {
        self.memory_map.reset();
        self.registers.reset();
        self.registers.set(&Register::ProgramCounter, VECTOR_RESET);
    }

    /// Step the CPU
    pub fn step(&mut self)
    {
        // Define the current memory word
        let pc = self.registers.get(&Register::ProgramCounter);
        let inst = self.memory_map.get(pc);

        // Increment the PC
        self.registers.set(&Register::ProgramCounter, pc);

        // Extract the different argument types
        let opcode = (inst & 0xF) as u8;
        let arg0 = ((inst & 0xF0) >> 8) as u8;
        let arg1 = ((inst & 0xF00) >> 16) as u8;
        let arg2 = ((inst & 0xF000) >> 32) as u8;

        // Match opcode parameters
        if opcode == 0x0
        {
            // NOOP
        }
        else if opcode == 0x30
        {
            // Determine the source and destination
            let src_is_reg = 0x1 & arg0 > 0;
            let dst_is_reg = 0x2 & arg0 > 0;

            // Process Results
            let src_reg_val = self.registers.get(&Register::GP(arg1 as usize & 0xF));
            let dst_reg = Register::GP((arg1 as usize & 0xF0) >> 8);

            // Determine the source parameter
            let source_val;
            if src_is_reg
            {
                source_val = src_reg_val;
            }
            else
            {
                source_val = self.memory_map.get(src_reg_val);
            }

            // Save to the destination parameter
            if dst_is_reg
            {
                self.registers.set(&dst_reg, source_val);
            }
            else
            {
                self.memory_map.set(self.registers.get(&dst_reg), source_val);
            }
        }
        else if opcode >= 0x40 && opcode < 0x50
        {
            // Define a function to obtain an operand value
            fn get_operand(cpu: &SolariumCPU, arg: u8, is_immediate: bool) -> MemoryWordSigned
            {
                return if is_immediate
                {
                    (arg as i8) as MemoryWordSigned
                }
                else
                {
                    let reg_val = cpu.registers.get(&Register::GP(arg as usize));
                    cpu.memory_map.get(reg_val) as MemoryWordSigned
                };
            }

            // Extract flag values
            let op_c_is_address = arg0 & 0x1 > 0;
            let op_a_is_immediate = arg0 & 0x2 > 0;
            let op_b_is_immediate = arg0 & 0x4 > 0;

            // Determine the register 3 value
            let reg_c = Register::GP(((arg0 & 0xF0) >> 8) as usize);

            // Determine the values of A and B operands
            let op_a = get_operand(self, arg1, op_a_is_immediate);
            let op_b = get_operand(self, arg2, op_b_is_immediate);

            // Determine the resulting values
            let result = match opcode & 0xF
            {
                0 => op_a + op_b,
                1 => op_a - op_b,
                2 => op_a * op_b,
                3 => op_a / op_b,
                4 => op_a % op_b,
                _ => panic!("unknown opcode provided")
            } as MemoryWord;

            // Store the resulting value
            if op_c_is_address
            {
                let address = self.registers.get(&reg_c);
                self.memory_map.set(address, result);
            }
            else
            {
                self.registers.set(&reg_c, result);
            }
        }
        else if opcode >= 0x20 && opcode < 0x30
        {
            // Extract arguments
            let is_immediate_relative = arg0 & 0x1 > 0;
            let reg_1_val = self.registers.get(&Register::GP((arg1 & 0xF) as usize)) as MemoryWordSigned;
            let reg_2_val = self.registers.get(&Register::GP(((arg2 & 0xF0) >> 8) as usize)) as MemoryWordSigned;

            // Determine the new program counter
            let new_pc;
            if is_immediate_relative
            {
                new_pc = (pc as MemoryWordSigned + (arg2 as i8) as MemoryWordSigned) as MemoryWord;
            }
            else
            {
                new_pc = self.registers.get(&Register::GP(arg2 as usize));
            }

            // Determine the jump type
            let jmp_type = opcode - 0x20;

            // Iterate based on jump types to determine if we will jump
            let will_jump = match jmp_type
            {
                0 => true,
                1 => reg_1_val != reg_2_val,
                2 => reg_1_val == reg_2_val,
                3 => reg_1_val < 0,
                4 => reg_1_val > 0,
                5 => reg_1_val >= reg_2_val,
                6 => reg_1_val > reg_2_val,
                7 => reg_1_val < reg_2_val,
                8 => reg_1_val <= reg_2_val,
                _ => panic!("unknown jump command provided")
            };

            // Perform the jump if needed
            if will_jump
            {
                self.registers.set(&Register::ProgramCounter, new_pc);
            }
        }
        else
        {
            panic!("unknown instruction provided");
        }
    }
}
