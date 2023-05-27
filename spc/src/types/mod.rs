#[derive(Clone, Copy)]
pub enum BuiltinTypes {
    U16,
    I16
}

impl ToString for BuiltinTypes {
    fn to_string(&self) -> String {
        match self {
            Self::U16 => "u16",
            Self::I16 => "i16",
        }.to_string()
    }
}

#[derive(Clone)]
pub enum SpType {
    Primitive{ name: String, base: BuiltinTypes },
    Array{ base: Box<SpType>, size: usize },
    Struct{ name: String, fields: Vec<(String, Box<SpType>)> },
    Pointer{ base: Box<SpType> },
    Constant{ base: Box<SpType> },
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
}

impl ToString for SpType {
    fn to_string(&self) -> String {
        match self {
            Self::Primitive{ name: n, .. } => n.to_string(),
            Self::Array{ base, size } => format!("[{size}]{}", base.to_string()),
            Self::Struct{ name, .. } => name.to_string(),
            Self::Pointer{ base, .. } => format!("*{}", base.to_string()),
            Self::Constant{ base, .. } => format!("${}", base.to_string()),
        }
    }
}
