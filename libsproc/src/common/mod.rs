use std::{num::ParseIntError, str::FromStr};

/// Provides the data type to use for a word in memory
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct MemoryWord
{
    value: u16
}

impl MemoryWord
{
    /// Constructs a new memory value
    pub fn new(val: u16) -> MemoryWord
    {
        return Self
        {
            value: val
        };
    }

    /// Gets the current value
    pub fn get(&self) -> u16
    {
        return self.value;
    }

    /// Gets the current value as a signed integer
    pub fn get_signed(&self) -> i16
    {
        return self.value as i16;
    }

    /// Sets a new value
    pub fn set(&mut self, value: u16)
    {
        self.value = value;
    }
}

impl ToString for MemoryWord
{
    fn to_string(&self) -> String
    {
        return self.value.to_string();
    }
}

impl FromStr for MemoryWord
{
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        return match s.parse::<u16>()
        {
            Ok(v) => Ok(MemoryWord::new(v)),
            Err(e) => Err(e)
        };
    }
}

/// Provides a CPU error code
#[derive(Clone, Copy, Debug)]
pub enum SolariumError
{
    None,
    InvalidMemoryAccess(usize),
    InvalidMemoryWrite(usize),
    InvalidSoftwareInterrupt(usize),
    StackOverflow,
    InvalidInstruction(MemoryWord),
    DivideByZero,
    ModByZero
}

impl ToString for SolariumError
{
    fn to_string(&self) -> String
    {
        return match self
        {
            SolariumError::None => "none".to_string(),
            SolariumError::DivideByZero => "divide-by-zero".to_string(),
            SolariumError::InvalidInstruction(inst) => format!("instruction \"{0:}\" not yet supported", inst.to_string()),
            SolariumError::InvalidMemoryAccess(loc) => format!("invalid memory access at location \"{0:}\"", loc),
            SolariumError::InvalidMemoryWrite(loc) => format!("invalid memory write at location \"{0:}\"", loc),
            SolariumError::InvalidSoftwareInterrupt(intnum) => format!("invalid sw interrupt {0:} provided", intnum),
            SolariumError::ModByZero => "mod-by-zero".to_string(),
            SolariumError::StackOverflow => "stack overflow".to_string()
        }
    }
}
