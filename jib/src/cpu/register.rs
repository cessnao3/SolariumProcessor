use core::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    ProgramCounter,
    Status,
    StackPointer,
    Overflow,
    Return,
    ArgumentBase,
    GeneralPurpose(usize),
}

impl Register {
    pub const NUM_REGISTERS: usize = 32;

    pub const IDX_PROGRAM_COUNTER: usize = 0;
    pub const IDX_STATUS: usize = 1;
    pub const IDX_STACK_POINTER: usize = 2;
    pub const IDX_OVERFLOW: usize = 3;
    pub const IDX_RETURN: usize = 4;
    pub const IDX_ARGUMENT_BASE: usize = 5;
    pub const IDX_FIRST_GP: usize = 6;

    pub const fn first_gp_register() -> Self {
        Self::GeneralPurpose(Self::IDX_FIRST_GP)
    }

    pub const fn last_register() -> Self {
        Self::GeneralPurpose(Self::NUM_REGISTERS - 1)
    }

    pub const fn get_index(&self) -> usize {
        match self {
            Self::ProgramCounter => Self::IDX_PROGRAM_COUNTER,
            Self::Status => Self::IDX_STATUS,
            Self::StackPointer => Self::IDX_STACK_POINTER,
            Self::Overflow => Self::IDX_OVERFLOW,
            Self::Return => Self::IDX_RETURN,
            Self::ArgumentBase => Self::IDX_ARGUMENT_BASE,
            Self::GeneralPurpose(num) => *num,
        }
    }

    pub const fn as_special(&self) -> Option<Self> {
        match self.get_index() {
            Self::IDX_PROGRAM_COUNTER => Some(Self::ProgramCounter),
            Self::IDX_STATUS => Some(Self::Status),
            Self::IDX_STACK_POINTER => Some(Self::StackPointer),
            Self::IDX_OVERFLOW => Some(Self::Overflow),
            Self::IDX_RETURN => Some(Self::Return),
            Self::IDX_ARGUMENT_BASE => Some(Self::ArgumentBase),
            _ => None,
        }
    }

    pub const fn get_special_name(&self) -> &str {
        match self.as_special() {
            Some(Self::ProgramCounter) => "pc",
            Some(Self::Status) => "stat",
            Some(Self::StackPointer) => "sp",
            Some(Self::Overflow) => "ovf",
            Some(Self::Return) => "ret",
            Some(Self::ArgumentBase) => "arg",
            _ => "",
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(spr) = self.as_special() {
            write!(f, "{} ({})", spr.get_special_name(), self.get_index())
        } else {
            write!(f, "{}", self.get_index())
        }
    }
}

impl TryFrom<usize> for Register {
    type Error = RegisterError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(match value {
            Self::IDX_PROGRAM_COUNTER => Self::ProgramCounter,
            Self::IDX_STATUS => Self::Status,
            Self::IDX_STACK_POINTER => Self::StackPointer,
            Self::IDX_OVERFLOW => Self::Overflow,
            Self::IDX_RETURN => Self::Return,
            Self::IDX_ARGUMENT_BASE => Self::ArgumentBase,
            x if (Self::IDX_FIRST_GP..Self::NUM_REGISTERS).contains(&x) => Self::GeneralPurpose(x),
            x => return Err(Self::Error::UnknownRegister(x)),
        })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterError {
    UnknownRegister(usize),
}

impl fmt::Display for RegisterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownRegister(r) => write!(f, "Unknown Register {r}"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterFlag {
    InterruptEnable,
    Carry,
}

impl RegisterFlag {
    pub fn get_bit(&self) -> i32 {
        match self {
            Self::InterruptEnable => 0,
            Self::Carry => 1,
        }
    }

    pub fn get_mask(&self) -> u32 {
        1 << self.get_bit()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RegisterManager {
    pub registers: [u32; Self::REGISTER_COUNT],
}

impl RegisterManager {
    pub const REGISTER_COUNT: usize = Register::NUM_REGISTERS;

    pub fn get(&self, reg: Register) -> Result<u32, RegisterError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            Ok(self.registers[ind])
        } else {
            Err(RegisterError::UnknownRegister(ind))
        }
    }

    pub fn set(&mut self, reg: Register, val: u32) -> Result<(), RegisterError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            self.registers[ind] = val;
            Ok(())
        } else {
            Err(RegisterError::UnknownRegister(ind))
        }
    }

    pub fn reset(&mut self) {
        self.registers.fill(0);
    }

    pub fn get_state(&self) -> [u32; Self::REGISTER_COUNT] {
        self.registers
    }

    pub fn set_state(&mut self, values: [u32; Self::REGISTER_COUNT]) {
        for (i, v) in values.into_iter().enumerate() {
            self.registers[i] = v;
        }
    }

    pub fn get_flag(&self, flag: RegisterFlag) -> Result<bool, RegisterError> {
        let val = self.get(Register::Status)?;
        Ok((val & flag.get_mask()) != 0)
    }

    pub fn set_flag(&mut self, flag: RegisterFlag, value: bool) -> Result<(), RegisterError> {
        let status = self.get(Register::Status)?;
        let new_status = if value {
            status | flag.get_mask()
        } else {
            status & !flag.get_mask()
        };
        self.set(Register::Status, new_status)?;
        Ok(())
    }
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self {
            registers: [0; Self::REGISTER_COUNT],
        }
    }
}
