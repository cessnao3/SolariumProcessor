#[derive(Clone, PartialEq, Eq)]
pub struct TypeInfo {
    pub name: String,
    pub size: usize,
}

impl TypeInfo {
    pub fn new(name: &str, size: usize) -> Self {
        Self { name: name.to_string(), size }
    }

    pub fn get_default_types() -> Vec<Self> {
        return vec![
            TypeInfo::new("word", 1),
            TypeInfo::new("uword", 1),
            TypeInfo::new("dec", 1)
        ]
    }
}
