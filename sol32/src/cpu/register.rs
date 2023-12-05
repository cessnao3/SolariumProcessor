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
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterError {
    UnknownRegister(usize),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RegisterManager {
    registers: [u32; Self::REGISTER_COUNT],
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
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self {
            registers: [0; Self::REGISTER_COUNT],
        }
    }
}
