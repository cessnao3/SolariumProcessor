use std::{num::ParseIntError, str::FromStr};

use super::asm_regex::{ARG_HEX_REGEX, ARG_NUMBER_REGEX, ARG_LABEL_REGEX, ARG_REGISTER_REGEX};

pub enum ParseArgumentErrorEnum
{
    Integer(ParseIntError),
    Register(String)
}

pub struct ParseArgumentError
{
    val: ParseArgumentErrorEnum
}

impl ToString for ParseArgumentError
{
    fn to_string(&self) -> String
    {
        return match &self.val
        {
            ParseArgumentErrorEnum::Integer(v) =>
            {
                v.to_string()
            },
            ParseArgumentErrorEnum::Register(v) =>
            {
                format!("unable to convert {0:} to register index", v)
            }
        }
    }
}

#[derive(Clone)]
pub enum Argument
{
    UnsignedNumber(u32),
    SignedNumber(i32),
    Label(String),
    Text(String)
}

impl Argument
{
    pub fn to_u16(&self) -> Result<u16, String>
    {
        return match &self
        {
            Argument::UnsignedNumber(v) =>
            {
                let uval = *v as u16;
                if uval as u32 == *v
                {
                    Ok(uval)
                }
                else
                {
                    Err(format!("unable to convert {0:} to u16", self.to_string()))
                }
            },
            Argument::SignedNumber(v) =>
            {
                let ival = *v as i16;
                if ival as i32 == *v
                {
                    Ok(ival as u16)
                }
                else
                {
                    Err(format!("uanble to convert {0:} to u16", self.to_string()))
                }
            },
            _ => Err(format!("unable to convert {0:} to u16", self.to_string()))
        }
    }

    pub fn to_u8(&self) -> Result<u8, String>
    {
        return match &self
        {
            Argument::UnsignedNumber(v) =>
            {
                let uval = *v as u8;
                if uval as u32 == *v
                {
                    Ok(uval)
                }
                else
                {
                    Err(format!("unable to convert {0:} to u8", self.to_string()))
                }
            },
            Argument::SignedNumber(v) =>
            {
                let ival = *v as i8;
                if ival as i32 == *v
                {
                    Ok(ival as u8)
                }
                else
                {
                    Err(format!("uanble to convert {0:} to u8", self.to_string()))
                }
            },
            _ => Err(format!("unable to convert {0:} to u8", self.to_string()))
        }
    }

    pub fn to_register_val(&self) -> Result<u8, String>
    {
        let reg_val = match self.to_u8()
        {
            Ok(v) => v,
            Err(e) => return Err(e)
        };

        if reg_val & 0xF == reg_val
        {
            return Ok(reg_val);
        }
        else
        {
            return Err(format!("register value {0:} exceeds number of registers", reg_val));
        }
    }
}

impl ToString for Argument
{
    fn to_string(&self) -> String
    {
        match &self
        {
            Argument::Label(s) => format!("Label({0:})", s),
            Argument::UnsignedNumber(v) => format!("Unsigned({0:})", v),
            Argument::SignedNumber(v) => format!("Signed({0:})", v),
            Argument::Text(s) => format!("Text(\"{0:}\")", s)
        }
    }
}

impl FromStr for Argument
{
    type Err = ParseArgumentError;

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        if ARG_NUMBER_REGEX.is_match(s)
        {
            if s.chars().next().unwrap() == '-'
            {
                return match s.parse::<i32>()
                {
                    Ok(v) => Ok(Argument::SignedNumber(v)),
                    Err(e) => Err(ParseArgumentError
                    {
                        val: ParseArgumentErrorEnum::Integer(e)
                    })
                };
            }
            else
            {
                return match s.parse::<u32>()
                {
                    Ok(v) => Ok(Argument::UnsignedNumber(v)),
                    Err(e) => Err(ParseArgumentError
                    {
                        val: ParseArgumentErrorEnum::Integer(e)
                    })
                };
            }
        }
        else if ARG_HEX_REGEX.is_match(s)
        {
            return match u32::from_str_radix(
                &s[2..],
                16)
            {
                Ok(v) => Ok(Argument::UnsignedNumber(v)),
                Err(e) => Err(ParseArgumentError
                {
                    val: ParseArgumentErrorEnum::Integer(e)
                })
            };
        }
        else if ARG_LABEL_REGEX.is_match(s)
        {
            return Ok(Argument::Label(s.to_string()));
        }
        else if ARG_REGISTER_REGEX.is_match(s)
        {
            let reg_str = &s[1..];

            let reg_ind = if reg_str == "pc"
            {
                0
            }
            else if reg_str == "sp"
            {
                1
            }
            else if reg_str == "spb"
            {
                2
            }
            else if reg_str == "stat"
            {
                3
            }
            else
            {
                return Err(ParseArgumentError
                {
                    val: ParseArgumentErrorEnum::Register(reg_str.to_string())
                });
            };

            return Ok(Argument::SignedNumber(reg_ind));
        }
        else
        {
            panic!();
        }
    }
}
