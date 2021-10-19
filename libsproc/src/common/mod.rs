/// Provides the data type to use for a word in memory
pub type MemoryWord = u16;

/// Provides the corresponding signed type for the type used for [MemoryWord]
pub type MemoryWordSigned = i16;

/// Provides a CPU error code
#[derive(Clone, Copy)]
pub enum SolariumError
{
    None,
    InvalidMemoryAccess(usize),
    StackOverflow,
    InvalidInstruction(MemoryWord),
    InterruptsNotSupported,
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
            SolariumError::InterruptsNotSupported => "interrupts not yet supported".to_string(),
            SolariumError::InvalidInstruction(inst) => format!("instruction \"{0:}\" not yet supported", inst),
            SolariumError::InvalidMemoryAccess(loc) => format!("invalid memory access at location \"{0:}\"", loc),
            SolariumError::ModByZero => "mod-by-zero".to_string(),
            SolariumError::StackOverflow => "stack overflow".to_string()
        }
    }
}
