use std::str::FromStr;

use regex::Regex;

use lazy_static::lazy_static;

pub fn read_register_value(reg: &str) -> Result<u8, String>
{
    let reg_val = match reg.parse::<u8>()
    {
        Ok(v) => v,
        Err(_) => return Err(format!("unable to convert {0:} to register value", reg))
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

pub struct ImmedateByteValues
{
    val: u8
}

impl ImmedateByteValues
{
    pub fn new(val: u8) -> ImmedateByteValues
    {
        return ImmedateByteValues
        {
            val
        };
    }

    pub fn get_low_nybble(&self) -> u8
    {
        return self.val & 0xF;
    }

    pub fn get_high_nybble(&self) -> u8
    {
        return (self.val & 0xF0) >> 4;
    }
}

impl FromStr for ImmedateByteValues
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        lazy_static! {
            static ref ARGS_IMMEDIATE_HEX_REGEX: Regex = Regex::new(r"^0x(?P<digit>[0-9a-fA-F][0-9a-fA-F]?)$").unwrap();
            static ref ARGS_IMMEDIATE_NUM_NEG_REGEX: Regex = Regex::new(r"^\-[\d]+$").unwrap();
            static ref ARGS_IMMEDIATE_NUM_POS_REGEX: Regex = Regex::new(r"^+?[\d]+$").unwrap();
        }

        let immediate: u8;

        if ARGS_IMMEDIATE_HEX_REGEX.is_match(s)
        {
            immediate = match u8::from_str_radix(
                &s[2..],
                16)
            {
                Ok(x) => x,
                Err(_) => return Err(format!("unable to convert {0:} to immediate hex value", s))
            };
        }
        else if ARGS_IMMEDIATE_NUM_NEG_REGEX.is_match(s)
        {
            immediate = match s.parse::<i8>()
            {
                Ok(x) => x as u8,
                Err(_) => return Err(format!("unable to convert {0:} to immediate signed value", s))
            }
        }
        else if ARGS_IMMEDIATE_NUM_POS_REGEX.is_match(s)
        {
            immediate = match s.parse::<u8>()
            {
                Ok(x) => x,
                Err(_) => return Err(format!("unable to convert {0:} to immediate unsigned value", s))
            }
        }
        else
        {
            return Err(format!("immediate value '{0:}' does not match expected format", s));
        }

        return Ok(ImmedateByteValues::new(immediate));
    }
}
