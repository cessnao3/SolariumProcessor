use super::registers::Register;

use std::str::FromStr;

/// Provides the location of the register address provided to the assembler
pub enum Location
{
    Register(usize),
    AddressOf(usize),
    Value(i8)
}

impl FromStr for Location
{
    /// Defines the standard error type for the Location enum
    type Err = String;

    /// Provides a function to convert from a string into a location
    fn from_str(s: &str) -> Result<Location, Self::Err>
    {
        // Ensure not empty
        let trimmed = s.trim().to_ascii_lowercase();

        if trimmed.len() == 0
        {
            return Err("cannot get a Location from an empty string".to_string());
        }

        // Determine if the first character is is a keyword
        let first = trimmed.chars().next().unwrap();
        let rest = &trimmed[1..];

        const CHAR_DIRECT: char = 'r';
        const CHAR_ADDRESS_OF: char = '$';

        let val_str = if first == CHAR_DIRECT || first == CHAR_ADDRESS_OF
        {
            rest
        }
        else
        {
            &trimmed
        };

        // Parse the value string
        let value = match val_str.parse::<i8>()
        {
            Ok(v) => v,
            _ => return Err(format!("unable to parse {0:} as integer", val_str))
        };

        // Error-check
        let valid = if first == CHAR_DIRECT || first == CHAR_ADDRESS_OF
        {
            value >= 0 && (value as usize) < Register::NUM_REGISTERS
        }
        else
        {
            let mask_val = value as u8 & Self::MASK_FLAG;
            mask_val == Self::MASK_FLAG || mask_val == 0
        };

        if !valid
        {
            return Err(format!("location argument {0:} is not valid", s));
        }

        // Return the resulting value
        return if first == CHAR_DIRECT
        {
            if value >= 0
            {
                Ok(Location::Register(value as usize))
            }
            else
            {
                Err(format!("register index {0:} < 0", value))
            }
        }
        else if first == CHAR_ADDRESS_OF
        {
            if value >= 0
            {
                Ok(Location::AddressOf(value as usize))
            }
            else
            {
                Err(format!("register address index {0:} < 0", value))
            }
        }
        else
        {
            Ok(Location::Value(value))
        };
    }
}

impl Location
{
    /// The flag to be used for a direct register location type
    const FLAG_DIRECT: u8 = 0;

    /// The flag to be used for a address-of location type
    const FLAG_ADDRESS_OF: u8 = 1;

    /// The flag to be used for an immediate value
    const FLAG_VALUE: u8 = 3;

    /// The mask to be used for the flag
    const MASK_FLAG: u8 = 0xC0;

    /// The mask to be used for the parameter value
    const MASK_PARAM: u8 = 0x3F;

    /// The offset to use to offset the flag parameters
    const FLAG_OFFSET: usize = 6;

    /// Converts an argument to a location enumeration
    pub fn from_arg(arg: u8) -> Result<Location, String>
    {
        // Determine the flag value
        let flag = (arg & Self::MASK_FLAG) >> Self::FLAG_OFFSET;
        let param = arg & Self::MASK_PARAM;

        // Determine if the flag value is a valid register
        fn get_register_index(parameter: u8) -> Result<usize, String>
        {
            return if (parameter as usize) < Register::NUM_REGISTERS
            {
                Ok(parameter as usize)
            }
            else
            {
                Err(format!("register {0:} is out of range", parameter))
            }
        }

        // Switch based on the provided flag and value
        return if flag == Self::FLAG_DIRECT
        {
            match get_register_index(param)
            {
                Ok(v) => Ok(Self::Register(v)),
                Err(e) => Err(e)
            }
        }
        else if flag == Self::FLAG_ADDRESS_OF
        {
            match get_register_index(param)
            {
                Ok(v) => Ok(Self::AddressOf(v)),
                Err(e) => Err(e)
            }
        }
        else if flag == Self::FLAG_VALUE
        {
            Ok(Self::Value(param as i8))
        }
        else
        {
            Err(format!("undefined location flag value {0:} provided", flag))
        };
    }

    /// Converts the location enumeration to an argument parameter
    pub fn to_arg(&self) -> Result<u8, String>
    {
        // Function to parse a register size to an argument
        fn register_to_arg_param(index: usize) -> Result<u8, String>
        {
            return if index < Register::NUM_REGISTERS
            {
                Ok(index as u8)
            }
            else
            {
                Err(format!("register index {0:} is out of range", index))
            }
        }

        // Return the location value
        return match self
        {
            Self::Register(v) =>
                {
                    match register_to_arg_param(*v)
                    {
                        Ok(m) => Ok(m | (Self::FLAG_DIRECT << Self::FLAG_OFFSET)),
                        Err(e) => Err(e)
                    }
                },
            Self::AddressOf(v) =>
                {
                    match register_to_arg_param(*v)
                    {
                        Ok(m) => Ok(m | (Self::FLAG_DIRECT << Self::FLAG_OFFSET)),
                        Err(e) => Err(e)
                    }
                },
            Self::Value(v) =>
                {
                    let vu = *v as u8;
                    let v_mask = vu & Self::MASK_FLAG;
                    if v_mask == Self::MASK_FLAG || v_mask == 0
                    {
                        Ok(vu | Self::FLAG_VALUE)
                    }
                    else
                    {
                        Err(format!("cannot convert {0:} to immediate parameter", v))
                    }
                }
        };
    }
}
