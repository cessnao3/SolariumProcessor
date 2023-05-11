use once_cell::sync::OnceCell;
use super::types::SpType;

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

pub struct Literal {
    words: Vec<u16>,
    var_type: Box<SpType>
}

impl Constant {

}

impl Into<Constant> for u16 {
    fn into(self) -> Constant {

    }
}

impl Into<Constant> for i16 {
}

impl Into<Constant> for &str {
    fn into(self) -> Constant {
    }
}

pub trait Expression {
    fn get_type(&self) -> Box<SpType>;
}

pub trait Addressable {
    fn get_address(&self) -> u16;
}

pub trait Variable: Addressable + Expression {

}

pub trait Function: Addressable {
    fn get_input_parameters(&self) -> Vec<(String, SpType)>;
}
