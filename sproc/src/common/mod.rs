use std::num::ParseIntError;
use std::fmt;
use std::str::FromStr;

/// Provides the data type to use for a word in memory
#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct MemoryWord {
    value: u16,
}

impl MemoryWord {
    /// Gets the current value
    pub fn get(&self) -> u16 {
        self.value
    }

    /// Gets the current value as a signed integer
    pub fn get_signed(&self) -> i16 {
        self.value as i16
    }

    /// Sets a new value
    pub fn set(&mut self, value: u16) {
        self.value = value;
    }
}

impl fmt::Display for MemoryWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl FromStr for MemoryWord {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<u16>() {
            Ok(v) => Ok(MemoryWord::from(v)),
            Err(e) => Err(e),
        }
    }
}

impl From<u16> for MemoryWord {
    fn from(value: u16) -> Self {
        Self { value }
    }
}

impl From<i16> for MemoryWord {
    fn from(value: i16) -> Self {
        Self { value: value as u16 }
    }
}

/// Provides a CPU error code
#[derive(Clone, Copy, Debug)]
pub enum SolariumError {
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
    DeviceError(usize, SolariumDeviceError),
    RegisterIndexError(usize),
    StartEndIndexMismatch(usize, usize),
    SegmentOverlap(usize, usize),
    StopRequested,
}

impl fmt::Display for SolariumError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SolariumError::None => write!(f, "none"),
            SolariumError::DivideByZero => write!(f, "divide-by-zero"),
            SolariumError::InvalidInstruction(inst) => write!(f, "invalid instruction \"{inst}\""),
            SolariumError::InvalidMemoryAccess(loc) => write!(f, "invalid memory access at location \"{loc}\""),
            SolariumError::InvalidMemoryWrite(loc) => write!(f, "invalid memory write at location \"{loc}\""),
            SolariumError::InvalidSoftwareInterrupt(intnum) => write!(f, "invalid sw interrupt {intnum} provided"),
            SolariumError::InvalidHardwareInterrupt(intnum) => write!(f, "invalid hw interrupt {0:} provided", intnum),
            SolariumError::ModByZero => write!(f, "mod-by-zero"),
            SolariumError::ShiftError(shift_count) => write!(f, "invalid attempt to shift by {shift_count}"),
            SolariumError::StackUnderflow => write!(f, "stack underflow"),
            SolariumError::DeviceError(base_addr, err) => write!(f, "device {base_addr} error: {err}"),
            SolariumError::RegisterIndexError(ind) => write!(f, "register {ind} exceeds the register size"),
            SolariumError::StartEndIndexMismatch(start_index, end_index) => write!(f, "segment starting index {start_index} is >= ending index {end_index}"),
            SolariumError::SegmentOverlap(start_index, end_index) => write!(f, "segment from [{start_index}, {end_index}) overlaps with other segments",),
            SolariumError::StopRequested => write!(f, "stop requested"),
        }
    }
}

/// Provides a device error code
#[derive(Clone, Copy, Debug)]
pub enum SolariumDeviceError {
    BufferFull,
}

impl fmt::Display for SolariumDeviceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SolariumDeviceError::BufferFull => write!(f, "device buffer is full"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum InstructionError {
    NybbleOversize(InstructionData)
}

impl std::fmt::Display for InstructionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NybbleOversize(v) => write!(f, "Instruction Nybble Oversize for {v}"),
        }
    }
}

/// Defines the core instruction group value
#[derive(Copy, Clone, Debug)]
pub struct InstructionData {
    pub opcode: u8,
    pub arg0: u8,
    pub arg1: u8,
    pub arg2: u8,
}

impl InstructionData {
    pub fn new_arg0(opcode: u8) -> Result<Self, InstructionError> {
        Self::new(0, 0, 0, opcode)
    }

    pub fn new_arg1(opcode: u8, arg0: u8) -> Result<Self, InstructionError> {
        Self::new(0, 0, opcode, arg0)
    }

    pub fn new_arg2(opcode: u8, arg0: u8, arg1: u8) -> Result<Self, InstructionError> {
        Self::new(0, opcode, arg0, arg1)
    }

    pub fn new(opcode: u8, arg0: u8, arg1: u8, arg2: u8) -> Result<Self, InstructionError> {
        let v = Self { opcode, arg0, arg1, arg2 };

        if !v.check_nybbles() {
            Err(InstructionError::NybbleOversize(v))
        } else {
            Ok(v)
        }
    }

    /// Checks for internal consistency for all nybbles in the data struct
    fn check_nybbles(&self) -> bool {
        fn is_nybble(v: u8) -> bool {
            v & 0xF == v
        }

        is_nybble(self.opcode) &&
            is_nybble(self.arg0) &&
            is_nybble(self.arg1) &&
            is_nybble(self.arg2)
    }
}

impl std::fmt::Display for InstructionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instruction({}, {}, {}, {})", self.opcode, self.arg0, self.arg1, self.arg2)
    }
}

/// Constructs instruction data from a given memory word
impl From<MemoryWord> for InstructionData {
    fn from(value: MemoryWord) -> Self {
        // Extracts the instruction value
        let inst_val = value.get();

        // Extract the different argument types
        let opcode = ((inst_val & 0xF000) >> 12) as u8;
        let arg0 = ((inst_val & 0x0F00) >> 8) as u8;
        let arg1 = ((inst_val & 0x00F0) >> 4) as u8;
        let arg2 = (inst_val & 0x000F) as u8;

        assert!(opcode & 0xF == opcode);
        assert!(arg0 & 0xF == arg0);
        assert!(arg1 & 0xF == arg1);
        assert!(arg2 & 0xF == arg2);

        Self {
            opcode,
            arg0,
            arg1,
            arg2,
        }
    }
}

/// Combines the instruction components into their word values
impl TryFrom<InstructionData> for MemoryWord {
    type Error = InstructionError;

    fn try_from(value: InstructionData) -> Result<Self, Self::Error> {
        if !value.check_nybbles() {
            return Err(InstructionError::NybbleOversize(value));
        }

        let mw =
            ((value.opcode as u16) << 12)
            | ((value.arg0 as u16) << 8)
            | ((value.arg1 as u16) << 4)
            | (value.arg2 as u16);

        Ok(Self::from(mw))
    }
}
