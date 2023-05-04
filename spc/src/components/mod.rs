pub struct CompilerState {
    pub signed_airthmetic: bool,
    pub functions: Vec<Box<dyn Function>>,
    pub types: Vec<SpType>,
    pub scopes: Vec<Scope>,
}

pub struct Scope {
    pub variables: Vec<Box<dyn Variable>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: Vec::new()
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilerState {
    pub fn new() -> Self {
        Self {
            signed_airthmetic: false,
            functions: Vec::new(),
            scopes: vec![Scope::new()],
            types: vec![
                SpType::new("i16", 1),
                SpType::new("u16", 1),
                SpType::new("void", 1),
            ]
        }
    }
}

impl Default for CompilerState {
    fn default() -> Self {
        Self::new()
    }
}

pub trait CodeComponent {
    fn generate_code(&self, state: &mut CompilerState);
}

pub struct SpType {
    name: String,
    size: usize
}

impl SpType {
    pub fn new(name: &str, size: usize) -> Self {
        Self {
            name: name.to_string(),
            size
        }
    }
}

pub trait Variable {
    fn get_address(&self) -> u16;
    fn get_type(&self) -> SpType;
}

pub trait Function {
    fn get_address(&self) -> u16;
    fn get_input_parameters(&self) -> Vec<(String, SpType)>;
}
