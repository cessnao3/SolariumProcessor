use sproc::common::InstructionError;

use crate::assembly::argument::ArgumentError;

pub enum AssemblerError {
    ArgumentError(ArgumentError),
    ArgumentCount{ expected: usize, actual: usize },
    InstructionError(InstructionError),
}

impl From<ArgumentError> for AssemblerError {
    fn from(value: ArgumentError) -> Self {
        AssemblerError::ArgumentError(value)
    }
}

impl From<InstructionError> for AssemblerError {
    fn from(value: InstructionError) -> Self {
        AssemblerError::InstructionError(value)
    }
}

impl std::fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArgumentError(arge) => write!(f, "{arge}"),
            Self::ArgumentCount { expected, actual } => write!(f, "argument count expected {expected}, found {actual}"),
            Self::InstructionError(inste) => write!(f, "{inste}")
        }
    }
}
