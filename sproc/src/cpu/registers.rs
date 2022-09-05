use crate::common::MemoryWord;

#[derive(Clone, Copy)]
/// Defines the enumeration for available registers
pub enum Register
{
    ProgramCounter,
    StackPointer,
    StackBase,
    StatusFlags,
    Return,
    ArgumentBase,
    GP(usize)
}

impl Register
{
    /// Defines the number of registers available in the processor
    pub const NUM_REGISTERS: usize = 16;

    /// Provides a General-Purpose register from a given index
    pub fn from_index(ind: usize) -> Register
    {
        if ind < Self::NUM_REGISTERS
        {
            return Register::GP(ind);
        }
        else
        {
            panic!("unable to create a register from index {0:}", ind);
        }
    }

    /// Converts the enumeration types to an index for a registry array
    pub fn to_index(&self) -> usize
    {
        // Determine the index
        let ind: usize = match self
        {
            Register::ProgramCounter => 0,
            Register::StatusFlags => 1,
            Register::StackPointer => 2,
            Register::StackBase => 3,
            Register::Return => 4,
            Register::ArgumentBase => 5,
            Register::GP(ind) => *ind
        };

        // Check for the index values
        if ind < Self::NUM_REGISTERS
        {
            return ind;
        }
        else
        {
            panic!("Register index is greater than available processor registers");
        }
    }
}

/// Define the types of status flags allowed
#[derive(Clone, Copy, Debug)]
pub enum StatusFlag
{
    InterruptEnable,
    SignedArithmetic
}

impl StatusFlag
{
    /// Provides the mask required to set the flag with a bitwise-or
    pub fn get_mask(&self) -> MemoryWord
    {
        return MemoryWord::new(match self
        {
            StatusFlag::InterruptEnable => (1 << 0),
            StatusFlag::SignedArithmetic => (1 << 1)
        });
    }
}

impl ToString for StatusFlag
{
    fn to_string(&self) -> String
    {
        return match self
        {
            StatusFlag::InterruptEnable => "I".to_string(),
            StatusFlag::SignedArithmetic => "U".to_string()
        };
    }
}

/// Defines a register manager to maintain register values
pub struct RegisterManager
{
    registers: [MemoryWord; Register::NUM_REGISTERS]
}

impl RegisterManager
{
    /// Creates a new register manager
    pub fn new() -> RegisterManager
    {
        return RegisterManager
        {
            registers: [MemoryWord::new(0); Register::NUM_REGISTERS]
        };
    }

    /// Resets all registers to a known, zero, state
    pub fn reset(&mut self)
    {
        for v in self.registers.iter_mut()
        {
            v.set(0);
        }
    }

    /// Gets the selected register value
    pub fn get(&self, register: Register) -> MemoryWord
    {
        return self.registers[register.to_index()];
    }

    /// Sets the selecteed register value
    pub fn set(&mut self, register: Register, value: MemoryWord) -> bool
    {
        let reg_ind = register.to_index();
        self.registers[reg_ind] = value;
        return true;
    }

    /// Sets the given flag for the processor status flags
    pub fn set_flag(&mut self, flag: StatusFlag)
    {
        let last_val = self.get(Register::StatusFlags);
        let new_val = MemoryWord::new(last_val.get() | flag.get_mask().get());
        self.set(
            Register::StatusFlags,
            new_val);
    }

    /// Clears the given flag to the processor status
    pub fn clear_flag(&mut self, flag: StatusFlag)
    {
        let last_val = self.get(Register::StatusFlags);
        let new_val = MemoryWord::new(last_val.get() & !flag.get_mask().get());
        self.set(
            Register::StatusFlags,
            new_val);
    }

    /// Gets the value of the given processor status flags
    pub fn get_flag(&self, flag: StatusFlag) -> bool
    {
        return (self.get(Register::StatusFlags).get() & flag.get_mask().get()) != 0;
    }
}

#[cfg(test)]
mod tests
{
    use super::*;

    /// Provide the register vector for available registers
    fn get_registers() -> Vec<Register>
    {
        // Define the list of named registers
        let mut registers: Vec<Register> = vec!{
            Register::ProgramCounter,
            Register::StackPointer,
        };

        // Add the register indices
        for i in registers.len()..Register::NUM_REGISTERS
        {
            registers.push(Register::GP(i));
        }

        // Ensure the resulting value is correct
        assert!(registers.len() == Register::NUM_REGISTERS);

        // Return the register vector
        return registers;
    }

    #[test]
    /// Tests the general get/set register values
    fn test_set_get_register()
    {
        // Create a register manager to test register values
        let mut register_manager = RegisterManager::new();

        // Iterate over available registers
        for register in get_registers().iter()
        {
            // Determine values ot iterate over
            for v in 0..1000
            {
                // Set the register manager, and then ensure that the output result matches
                register_manager.set(
                    *register,
                    MemoryWord::new(v));
                assert_eq!(register_manager.get(*register), MemoryWord::new(v));
            }
        }
    }

    #[test]
    /// Test that each register is stored in its own storage location
    fn test_individual_values()
    {
        // Define a register manager
        let mut register_manager = RegisterManager::new();

        // Add a value to each register
        for i in 0..Register::NUM_REGISTERS
        {
            register_manager.set(Register::GP(i), MemoryWord::new(i as u16));
        }

        // Ensure that we can get the resulting value out
        for i in 0..Register::NUM_REGISTERS
        {
            let val = register_manager.get(Register::GP(i));
            assert_eq!(val, MemoryWord::new(i as u16));
        }
    }
}
