use crate::assembly::argument::ArgumentError;

pub enum AssemblerError {
    ArgumentError(ArgumentError),
    ArgumentCount{ expected: usize, actual: usize }
}

impl std::fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArgumentError(arge) => write!(f, "{arge}"),
            Self::ArgumentCount { expected, actual } => write!(f, "argument count expected {expected}, found {actual}")
        }
    }
}
