#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BuiltinTypes {
    U16,
    I16,
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
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SpType {
    OpaqueType{ name: String },
    Primitive{ base: BuiltinTypes },
    Alias{ name: String, base: Box<SpType> },
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

    pub fn word_count(&self) -> Result<usize, String> {
        match self {
            Self::OpaqueType { .. } => Err("opaque type has no size".to_string()),
            Self::Primitive { base, .. } => Ok(base.word_count()),
            Self::Array { base, size } => Ok(base.word_count()? * size),
            Self::Struct { fields, .. } => fields.iter().map(|(_, t)| t.word_count()).sum(),
            Self::Pointer { .. } => Ok(BuiltinTypes::U16.word_count()),
            Self::Function { .. } => Ok(BuiltinTypes::U16.word_count()),
            Self::Constant { base } => base.word_count(),
            Self::Alias { base, .. } => base.word_count(),
        }
    }

    pub fn base_primitive(&self) -> Option<SpType> {
        match self {
            Self::Pointer { .. } => Some(SpType::Primitive { base: BuiltinTypes::U16 }),
            Self::Array { .. } => Some(SpType::Primitive { base: BuiltinTypes::U16 }),
            Self::Primitive { .. } => Some(self.clone()),
            Self::Alias { base, .. } => base.base_primitive(),
            Self::Constant { base } => base.base_primitive(),
            _ => None,
        }
    }
}

impl std::fmt::Display for SpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpaqueType { name } => write!(f, "{name}"),
            Self::Primitive{ base } => write!(f, "{base}"),
            Self::Array{ base, size } => write!(f, "[{size}]{base}"),
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
            Self::Alias { name, .. } => write!(f, "{name}"),
        }
    }
}
