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

pub enum SpType {
    Primitive{ base: BuiltinTypes },
    Array{ base: Box<SpType>, size: usize },
    Struct{ name: String, fields: Vec<(String, Box<SpType>)> },
    Pointer{ base: Box<SpType> }
}

impl ToString for SpType {
    fn to_string(&self) -> String {
        match self {
            Self::Primitive{ base: t } => t.to_string(),
            Self::Array{ base, .. } => format!("{}[{}]", base.to_string(), s),
            Self::Struct{ name, .. } => name.to_string(),
            Self::Pointer{ base, .. } => format!("{}*", base.to_string())
        }
    }
}
