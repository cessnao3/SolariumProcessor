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
    pub fn get_index(&self) -> usize {
        match self {
            Self::ProgramCounter => 0,
            Self::Status => 1,
            Self::StackPointer => 2,
            Self::Overflow => 3,
            Self::Return => 4,
            Self::ArgumentBase => 5,
            Self::GeneralPurpose(num) => *num,
        }
    }

    pub fn get_special(&self) -> Option<(&str, Self)> {
        match self.get_index() {
            0 => Some(("pc", Self::ProgramCounter)),
            1 => Some(("stat", Self::Status)),
            2 => Some(("sp", Self::StackPointer)),
            3 => Some(("ovf", Self::Overflow)),
            4 => Some(("ret", Self::Return)),
            5 => Some(("arg",Self::ArgumentBase)),
            _ => None,
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((name, _)) = self.get_special() {
            write!(f, "{} ({})", name, self.get_index())
        } else {
            write!(f, "{}", self.get_index())
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterError {
    UnknownRegister(usize),
}

impl fmt::Display for RegisterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownRegister(r) => write!(f, "Unknown Register {r}")
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterFlag {
    InterruptEnable,
}

impl RegisterFlag {
    pub fn get_bit(&self) -> i32 {
        match self {
            Self::InterruptEnable => 0,
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
    pub const REGISTER_COUNT: usize = 32;

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
