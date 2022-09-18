use std::num::ParseIntError;
use std::str::FromStr;

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

    /// Constructs a new memory value
    pub fn new_signed(val: i16) -> MemoryWord
    {
        return Self
        {
            value: val as u16
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
    InvalidHardwareInterrupt(usize),
    StackUnderflow,
    InvalidInstruction(MemoryWord),
    DivideByZero,
    ModByZero,
    ShiftError(usize),
    CharacterToWord(char),
    WordToCharacter(MemoryWord),
    DeviceError(usize, SolariumDeviceError),
    RegisterIndexError(usize),
    StartEndIndexMismatch(usize, usize),
    SegmentOverlap(usize, usize),
    StopRequested
}

impl ToString for SolariumError
{
    fn to_string(&self) -> String
    {
        return match self
        {
            SolariumError::None => "none".to_string(),
            SolariumError::DivideByZero => "divide-by-zero".to_string(),
            SolariumError::InvalidInstruction(inst) => format!("invalid instruction \"{0:}\"", inst.to_string()),
            SolariumError::InvalidMemoryAccess(loc) => format!("invalid memory access at location \"{0:}\"", loc),
            SolariumError::InvalidMemoryWrite(loc) => format!("invalid memory write at location \"{0:}\"", loc),
            SolariumError::InvalidSoftwareInterrupt(intnum) => format!("invalid sw interrupt {0:} provided", intnum),
            SolariumError::InvalidHardwareInterrupt(intnum) => format!("invalid hw interrupt {0:} provided", intnum),
            SolariumError::ModByZero => "mod-by-zero".to_string(),
            SolariumError::ShiftError(shift_count) => format!("invalid attempt to shift by {0:}", shift_count),
            SolariumError::StackUnderflow => "stack underflow".to_string(),
            SolariumError::CharacterToWord(c) => format!("unable to convert {0:02X} to word", *c as u8),
            SolariumError::WordToCharacter(word) => format!("unable to convert {0:04X} to character", word.get()),
            SolariumError::DeviceError(base_addr, err) => format!("device {0:} error: {1:}", base_addr, err.to_string()),
            SolariumError::RegisterIndexError(ind) => format!("register {0:} exceeds the register size", ind),
            SolariumError::StartEndIndexMismatch(start_index, end_index) => format!("segment starting index {0:} is >= ending index {1:}", start_index, end_index),
            SolariumError::SegmentOverlap(start_index, end_index) => format!("segment from [{0:}, {1:}) overlaps with other segments", start_index, end_index),
            SolariumError::StopRequested => format!("stop requested")
        }
    }
}

/// Provides a device error code
#[derive(Clone, Copy, Debug)]
pub enum SolariumDeviceError
{
    BufferFull
}

impl ToString for SolariumDeviceError
{
    fn to_string(&self) -> String
    {
        return match self
        {
            SolariumDeviceError::BufferFull => "device buffer is full".to_string()
        };
    }
}

/// Defines the core instruction group value
pub struct InstructionData
{
    pub opcode: u8,
    pub arg0: u8,
    pub arg1: u8,
    pub arg2: u8
}

impl InstructionData
{
    /// Constructs the instruction group from a given instruction value
    pub fn new(instruction: MemoryWord) -> InstructionData
    {
        // Extracts the instruction value
        let inst_val = instruction.get();

        // Extract the different argument types
        let opcode = ((inst_val & 0xF000) >> 12) as u8;
        let arg0 = ((inst_val & 0x0F00) >> 8) as u8;
        let arg1 = ((inst_val & 0x00F0) >> 4) as u8;
        let arg2 = ((inst_val & 0x000F) >> 0) as u8;

        assert!(opcode & 0xF == opcode);
        assert!(arg0 & 0xF == arg0);
        assert!(arg1 & 0xF == arg1);
        assert!(arg2 & 0xF == arg2);

        return Self
        {
            opcode,
            arg0,
            arg1,
            arg2
        };
    }

    /// Combines the instruction components into their word values
    pub fn combine(&self) -> u16
    {
        assert!(self.opcode & 0xF == self.opcode);
        assert!(self.arg0 & 0xF == self.arg0);
        assert!(self.arg1 & 0xF == self.arg1);
        assert!(self.arg2 & 0xF == self.arg2);

        return
            ((self.opcode as u16) << 12) |
            ((self.arg0 as u16) << 8) |
            ((self.arg1 as u16) << 4) |
            (self.arg2 as u16);
    }
}
