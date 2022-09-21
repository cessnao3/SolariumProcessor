use crate::common::{MemoryWord, SolariumError};

#[derive(Clone, Copy)]
/// Defines the enumeration for available registers
pub enum Register
{
    ProgramCounter,
    StackPointer,
    StatusFlags,
    Return,
    ArgumentBase,
    GP(usize)
}

impl Register
{
    /// Converts the enumeration types to an index for a registry array
    pub fn to_index(&self) -> usize
    {
        // Determine the index
        let ind: usize = match self
        {
            Register::ProgramCounter => 0,
            Register::StatusFlags => 1,
            Register::StackPointer => 2,
            Register::Return => 4,
            Register::ArgumentBase => 5,
            Register::GP(ind) => *ind
        };

        return ind;
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
    registers: [MemoryWord; RegisterManager::NUM_REGISTERS]
}

impl RegisterManager
{
    /// Defines the number of registers available in the processor
    pub const NUM_REGISTERS: usize = 16;


    /// Creates a new register manager
    pub fn new() -> RegisterManager
    {
        return RegisterManager
        {
            registers: [MemoryWord::new(0); RegisterManager::NUM_REGISTERS]
        };
    }

    /// Provides the current state of the register values
    pub fn get_state(&self) -> [MemoryWord; RegisterManager::NUM_REGISTERS]
    {
        return self.registers;
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
    pub fn get(&self, register: Register) -> Result<MemoryWord, SolariumError>
    {
        let ind = register.to_index();

        if ind < RegisterManager::NUM_REGISTERS
        {
            return Ok(self.registers[register.to_index()]);
        }
        else
        {
            return Err(SolariumError::RegisterIndexError(ind));
        }
    }

    /// Sets the selecteed register value
    pub fn set(&mut self, register: Register, value: MemoryWord) -> Result<(), SolariumError>
    {
        let reg_ind = register.to_index();
        if reg_ind < Self::NUM_REGISTERS
        {
            self.registers[reg_ind] = value;
            return Ok(());
        }
        else
        {
            return Err(SolariumError::RegisterIndexError(reg_ind));
        }
    }

    /// Sets the given flag for the processor status flags
    pub fn set_flag(&mut self, flag: StatusFlag) -> Result<(), SolariumError>
    {
        let last_val = self.get(Register::StatusFlags)?;
        let new_val = MemoryWord::new(last_val.get() | flag.get_mask().get());
        return self.set(
            Register::StatusFlags,
            new_val);
    }

    /// Clears the given flag to the processor status
    pub fn clear_flag(&mut self, flag: StatusFlag) -> Result<(), SolariumError>
    {
        let last_val = self.get(Register::StatusFlags)?;
        let new_val = MemoryWord::new(last_val.get() & !flag.get_mask().get());
        return self.set(
            Register::StatusFlags,
            new_val);
    }

    /// Gets the value of the given processor status flags
    pub fn get_flag(&self, flag: StatusFlag) -> Result<bool, SolariumError>
    {
        return Ok((self.get(Register::StatusFlags)?.get() & flag.get_mask().get()) != 0);
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
        for i in registers.len()..RegisterManager::NUM_REGISTERS
        {
            registers.push(Register::GP(i));
        }

        // Ensure the resulting value is correct
        assert!(registers.len() == RegisterManager::NUM_REGISTERS);

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
                let set_res = register_manager.set(
                    *register,
                    MemoryWord::new(v));

                assert!(set_res.is_ok());

                let get_res = register_manager.get(*register);
                assert!(get_res.is_ok());
                assert_eq!(get_res.unwrap(), MemoryWord::new(v));
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
        for i in 0..RegisterManager::NUM_REGISTERS
        {
            assert!(register_manager.set(Register::GP(i), MemoryWord::new(i as u16)).is_ok());
        }

        // Ensure that we can get the resulting value out
        for i in 0..RegisterManager::NUM_REGISTERS
        {
            let val = register_manager.get(Register::GP(i));
            assert!(val.is_ok());
            assert_eq!(val.unwrap(), MemoryWord::new(i as u16));
        }
    }
}
