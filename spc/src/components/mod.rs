use super::types::{SpType, BuiltinTypes};

pub struct CompilerState {
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
            functions: Vec::new(),
            scopes: vec![Scope::new()],
            types: vec![
                SpType::Primitive{ name: "void".to_string(), base: BuiltinTypes::U16 },
                SpType::Primitive{ name: "u16".to_string(), base: BuiltinTypes::U16 },
                SpType::Primitive{ name: "i16".to_string(), base: BuiltinTypes::I16 },
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

impl Literal {

}

impl From<Literal> for u16 {
    fn from(value: Literal) -> Self {
        return value.words[0];
    }
}

impl From<Literal> for i16 {
    fn from(value: Literal) -> Self {
        return value.words[0] as i16;
    }
}

impl From<Literal> for String {
    fn from(value: Literal) -> Self {
        return value.words.iter()
            .map(|v| (*v as u8) as char)
            .collect::<String>();
    }
}

pub trait Statement {
}

pub trait BaseStatement {
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

pub struct AsmFunction {
    params: Vec<(String, SpType)>,
    lines: Vec<String>,
}

impl AsmFunction {
    pub fn new(params: &[(String, SpType)], lines: &[String]) -> Self {
        Self {
            params: params.to_vec(),
            lines: lines.to_vec(),
        }
    }
    
    pub fn get_assembly(&self) -> Vec<(sda::LineInformation, sda::ParsedValue)> {
        let res = sda::parse_lines(&self.lines.iter().map(|v| v.as_ref()).collect::<Vec<_>>());
        res.unwrap()
    }
}
