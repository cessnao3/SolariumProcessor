use crate::memory::MemoryWord;

/// Defines the number of registers available in the processor
const NUM_REGISTERS: usize = 16;

#[derive(Clone)]
/// Defines the enumeration for available registers
pub enum Register
{
    ProgramCounter,
    StackPointer,
    StatusFlag,
    Zero,
    GP(usize)
}

impl Register
{
    /// Converts the enumeration types to an index for a registry array
    fn to_index(&self) -> usize
    {
        // Determine the index
        let ind: usize = match self
        {
            Register::ProgramCounter => 0,
            Register::StackPointer => 1,
            Register::StatusFlag => 2,
            Register::Zero => 3,
            Register::GP(ind) => *ind
        };

        // Check for the index values
        if ind < NUM_REGISTERS
        {
            return ind;
        }
        else
        {
            panic!("Register index is greater than available processor registers");
        }
    }
}

/// Defines a register manager to maintain register values
pub struct RegisterManager
{
    registers: [MemoryWord; NUM_REGISTERS]
}

impl RegisterManager
{
    /// Creates a new register manager
    pub fn new() -> RegisterManager
    {
        return RegisterManager
        {
            registers: [0; NUM_REGISTERS]
        };
    }

    /// Resets all registers to a known, zero, state
    pub fn reset(&mut self)
    {
        for v in self.registers.iter_mut()
        {
            *v = 0;
        }
    }

    /// Gets the selected register value
    pub fn get(&self, register: &Register) -> MemoryWord
    {
        return self.registers[register.to_index()];
    }

    /// Sets the selecteed register value
    pub fn set(&mut self, register: &Register, value: MemoryWord)
    {
        let reg_ind = register.to_index();
        self.registers[reg_ind] = if reg_ind == Register::Zero.to_index() { 0 } else { value };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Provide the register vector for available registers
    fn get_registers() -> Vec<Register>
    {
        // Define the list of named registers
        let mut registers: Vec<Register> = vec!{
            Register::Zero,
            Register::ProgramCounter,
            Register::StackPointer,
            Register::StatusFlag
        };

        // Add the register indices
        for i in 0..NUM_REGISTERS
        {
            registers.push(Register::GP(i));
        }

        // Return the register vector
        return registers;
    }

    #[test]
    /// Tests the general get/set register values
    fn test_set_get_register()
    {
        // Create a register manager to test register values
        let mut register_manager = RegisterManager::new();

        // Determine the register index
        let zero_ind = Register::Zero.to_index();

        // Iterate over available registers
        for register in get_registers().iter()
        {
            // Determine values ot iterate over
            for v in 0..1000
            {
                // Define the expected value, ensuring that the zero register is always 0
                let expected_value: MemoryWord = if register.to_index() == zero_ind
                {
                    0
                }
                else
                {
                    v
                };

                // Set the register manager, and then ensure that the output result matches
                register_manager.set(&register, v);
                assert_eq!(register_manager.get(&register), expected_value);
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
        for i in 0..NUM_REGISTERS
        {
            register_manager.set(&Register::GP(i), i as MemoryWord);
        }

        // Ensure that we can get the resulting value out
        for i in 0..NUM_REGISTERS
        {
            let val = register_manager.get(&Register::GP(i));

            let expected = if i == Register::Zero.to_index() { 0 } else { i as MemoryWord };

            assert_eq!(val, expected);
        }
    }
}
