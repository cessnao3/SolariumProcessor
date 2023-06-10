use std::num::ParseIntError;
use std::str::FromStr;

/// Provides the data type to use for a word in memory
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct MemoryWord {
    value: u16,
}

impl MemoryWord {
    /// Constructs a new memory value
    pub fn new(val: u16) -> MemoryWord {
        Self { value: val }
    }

    /// Constructs a new memory value
    pub fn new_signed(val: i16) -> MemoryWord {
        Self { value: val as u16 }
    }

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

impl ToString for MemoryWord {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

impl FromStr for MemoryWord {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<u16>() {
            Ok(v) => Ok(MemoryWord::new(v)),
            Err(e) => Err(e),
        }
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
    CharacterToWord(char),
    WordToCharacter(MemoryWord),
    DeviceError(usize, SolariumDeviceError),
    RegisterIndexError(usize),
    StartEndIndexMismatch(usize, usize),
    SegmentOverlap(usize, usize),
    StopRequested,
}

impl ToString for SolariumError {
    fn to_string(&self) -> String {
        match self {
            SolariumError::None => "none".to_string(),
            SolariumError::DivideByZero => "divide-by-zero".to_string(),
            SolariumError::InvalidInstruction(inst) => {
                format!("invalid instruction \"{0:}\"", inst.to_string())
            }
            SolariumError::InvalidMemoryAccess(loc) => {
                format!("invalid memory access at location \"{0:}\"", loc)
            }
            SolariumError::InvalidMemoryWrite(loc) => {
                format!("invalid memory write at location \"{0:}\"", loc)
            }
            SolariumError::InvalidSoftwareInterrupt(intnum) => {
                format!("invalid sw interrupt {0:} provided", intnum)
            }
            SolariumError::InvalidHardwareInterrupt(intnum) => {
                format!("invalid hw interrupt {0:} provided", intnum)
            }
            SolariumError::ModByZero => "mod-by-zero".to_string(),
            SolariumError::ShiftError(shift_count) => {
                format!("invalid attempt to shift by {0:}", shift_count)
            }
            SolariumError::StackUnderflow => "stack underflow".to_string(),
            SolariumError::CharacterToWord(c) => {
                format!("unable to convert {0:02X} to word", *c as u8)
            }
            SolariumError::WordToCharacter(word) => {
                format!("unable to convert {0:04X} to character", word.get())
            }
            SolariumError::DeviceError(base_addr, err) => {
                format!("device {0:} error: {1:}", base_addr, err.to_string())
            }
            SolariumError::RegisterIndexError(ind) => {
                format!("register {0:} exceeds the register size", ind)
            }
            SolariumError::StartEndIndexMismatch(start_index, end_index) => format!(
                "segment starting index {0:} is >= ending index {1:}",
                start_index, end_index
            ),
            SolariumError::SegmentOverlap(start_index, end_index) => format!(
                "segment from [{0:}, {1:}) overlaps with other segments",
                start_index, end_index
            ),
            SolariumError::StopRequested => "stop requested".to_string(),
        }
    }
}

/// Provides a device error code
#[derive(Clone, Copy, Debug)]
pub enum SolariumDeviceError {
    BufferFull,
}

impl ToString for SolariumDeviceError {
    fn to_string(&self) -> String {
        match self {
            SolariumDeviceError::BufferFull => "device buffer is full".to_string(),
        }
    }
}

#[derive(Copy, Clone)]
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
#[derive(Copy, Clone)]
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

        Ok(Self::new(mw))
    }
}
