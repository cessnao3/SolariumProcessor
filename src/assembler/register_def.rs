use std::str::FromStr;
use crate::cpu::registers::Register;

/// Defines the register input definition
pub struct RegisterDefinition
{
    pub index: u8,
    pub is_immediate: bool
}

impl RegisterDefinition
{
    pub fn new(index: u8, is_immediate: bool) -> Result<RegisterDefinition, String>
    {
        return if (index as usize) < Register::NUM_REGISTERS
        {
            Ok(RegisterDefinition
            {
                index,
                is_immediate
            })
        }
        else
        {
            Err(format!("register index {0:} is greater than allowed", index))
        };
    }
}

impl FromStr for RegisterDefinition
{
    /// Defines the standard error type
    type Err = String;

    /// Constructs a register definition from an input word
    fn from_str(word: &str) -> Result<RegisterDefinition, Self::Err>
    {
        // Check that the words aren't empty
        if word.len() == 0
        {
            return Err("unknown word provided".to_string());
        }

        // Return the first character
        let first_char = word.chars().next().unwrap();
        let rest = &word[1..];

        let is_immediate: bool;
        if first_char == 'r'
        {
            is_immediate = true;
        }
        else if first_char == '$'
        {
            is_immediate = false;
        }
        else
        {
            return Err(format!("unknown register string type {0:} for {1:}", first_char, rest));
        }

        // Convert the remainder of the register to an index
        let reg_num: u8;
        match rest.parse::<u8>()
        {
            Ok(v) => reg_num = v,
            Err(_) => return Err(format!("unable to parse register index for {0:}", word))
        };

        // Return result
        return RegisterDefinition::new(reg_num, is_immediate);
    }
}
