use crate::memory::{MemoryMap, MemoryError};

pub enum ProcessorError {
    Memory(MemoryError),
    UnsupportedInterrupt(usize),
    UnknownRegister,
}

impl From<MemoryError> for ProcessorError {
    fn from(value: MemoryError) -> Self {
        Self::Memory(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterMap {
    ProgramCounter,
    Status,
    StackPointer,
    Overflow,
    Return,
    ArgumentBase,
    GeneralPurpose(usize),
}

impl RegisterMap {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResetType {
    Hard,
    Soft,
}

pub struct Processor {
    memory: MemoryMap,
    registers: [u32; 32],
}

impl Processor {
    pub const HARD_RESET_VECTOR: usize = 0;
    pub const SOFT_RESET_VECTOR: usize = 1;

    pub fn reset(&mut self, reset_type: ResetType) -> Result<(), ProcessorError> {
        if ResetType::Hard == reset_type {
            self.memory.reset();
        }

        let reset_vec = match reset_type {
            ResetType::Hard => Self::HARD_RESET_VECTOR,
            ResetType::Soft => Self::SOFT_RESET_VECTOR,
        };

        self.registers.fill(0);
        self.registers[RegisterMap::ProgramCounter.get_index()] = self.memory.get_u32(self.get_reset_vector(reset_vec)?)?;
        Ok(())
    }

    pub fn step(&self) {

    }

    pub fn get_reset_vector(&self, index: usize) -> Result<u32, ProcessorError> {
        if index < 64 {
            Ok(index as u32 * 4)
        } else {
            Err(ProcessorError::UnsupportedInterrupt(index))
        }
    }
}
