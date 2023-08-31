#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BuiltinTypes {
    U16,
    I16,
    Void
}

impl BuiltinTypes {
    pub fn word_count(&self) -> usize {
        1
    }
}

impl std::fmt::Display for BuiltinTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U16 => write!(f, "u16"),
            Self::I16 => write!(f, "i16"),
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SpType {
    Primitive{ name: String, base: BuiltinTypes },
    Array{ base: Box<SpType>, size: usize },
    Struct{ name: String, fields: Vec<(String, Box<SpType>)> },
    Pointer{ base: Box<SpType> },
    Constant{ base: Box<SpType> },
    Function{ args: Vec<Box<SpType>> },
}

impl SpType {
    pub fn is_valid_name(s: &str) -> bool {
        // Ensure that the first character is alphabetic and that the only characters are ascii-alphanumeric/_/-
        if !s.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_') {
            false
        } else if let Some(c) = s.chars().next() {
               c.is_ascii_alphabetic()
        } else {
            false
        }
    }

    pub fn word_count(&self) -> usize {
        match self {
            Self::Primitive { base, .. } => base.word_count(),
            Self::Array { base, size } => base.word_count() * size,
            Self::Struct { fields, .. } => fields.iter().map(|(_, t)| t.word_count()).sum(),
            Self::Pointer { .. } => BuiltinTypes::U16.word_count(),
            Self::Function { .. } => BuiltinTypes::U16.word_count(),
            Self::Constant { base } => base.word_count(),
        }
    }
}

impl std::fmt::Display for SpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive{ name: n, .. } => write!(f, "{n}"),
            Self::Array{ base, size } => write!(f, "[{size}]{}", base.to_string()),
            Self::Struct{ name, .. } => write!(f, "{name}"),
            Self::Pointer{ base, .. } => write!(f, "*{base}"),
            Self::Constant{ base, .. } => write!(f, "${base}"),
            Self::Function { args } => {
                write!(f, "fn(")?;
                for (i, t) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                }
                write!(f, ")")
            },
        }
    }
}
