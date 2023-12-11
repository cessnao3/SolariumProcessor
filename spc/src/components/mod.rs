use sol32asm::{
    instruction::{Add, Ld, Ldn},
    AssemblerCommand,
};
use sol32::cpu::Register;
use std::collections::HashMap;

use super::types::{SpType, SpTypeDict};

pub struct CompilerState {
    pub globals: HashMap<String, GlobalVariable>,
    //pub functions: HashMap<String, Box<dyn Function>>,
    pub types: SpTypeDict,
    pub scopes: Vec<Scope>,
}

pub struct Scope {
    pub variables: HashMap<String, Box<dyn Variable>>,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            statements: Vec::new(),
        }
    }

    pub fn add_statement(&mut self, s: Box<dyn Statement>) {
        self.statements.push(s);
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
            globals: HashMap::new(),
            //functions: HashMap::new(),
            scopes: vec![Scope::new()],
            types: SpTypeDict::new(),
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
    var_type: Box<SpType>,
}

impl Literal {}

impl From<Literal> for u16 {
    fn from(value: Literal) -> Self {
        value.words[0]
    }
}

impl From<Literal> for i16 {
    fn from(value: Literal) -> Self {
        value.words[0] as i16
    }
}

impl From<Literal> for String {
    fn from(value: Literal) -> Self {
        return value
            .words
            .iter()
            .map(|v| (*v as u8) as char)
            .collect::<String>();
    }
}

pub trait BaseStatement {}

pub trait Statement {}

pub struct DefinitionStatement {
    var_type: SpType,
    var_name: String,
    init_expr: Option<Box<dyn Expression>>,
}

impl DefinitionStatement {
    pub fn new(name: &str, t: SpType) -> Self {
        Self {
            var_type: t,
            var_name: name.into(),
            init_expr: None,
        }
    }

    pub fn set_init(&mut self, expr: Box<dyn Expression>) {
        self.init_expr = Some(expr);
    }
}

impl Statement for DefinitionStatement {}

impl BaseStatement for DefinitionStatement {}

pub trait Expression {
    fn get_type(&self) -> SpType;

    fn save_value_to(&self, reg: Register, spare: Register) -> Vec<sda::ParsedValue>;
}

pub struct BinaryExpression {
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>,
}

pub struct UnaryExpression {
    expr: Box<dyn Expression>,
}

pub struct AsExpression {
    expr: Box<dyn Expression>,
    new_type: Box<SpType>,
}

impl Expression for AsExpression {
    fn get_type(&self) -> SpType {
        return self.new_type.as_ref().clone();
    }

    fn save_value_to(&self, reg: Register, spare: Register) -> Vec<sda::ParsedValue> {
        // TODO - Update? Or is this okay (e.g., a struct saving pointers, to be copied later?)
        return self.expr.save_value_to(reg, spare);
    }
}

pub trait Addressable {
    fn get_address(&self, reg: Register) -> Vec<sda::ParsedValue>;
}

pub trait Variable: Addressable + Expression {}

pub struct LocalVariable {
    var_type: SpType,
    base_offset: i16,
}

impl Expression for LocalVariable {
    fn get_type(&self) -> SpType {
        self.var_type.clone()
    }

    fn save_value_to(&self, reg: Register, spare: Register) -> Vec<sda::ParsedValue> {
        let mut res = self.get_address(reg);
        res.push(sda::ParsedValue::InstructionValue(Box::new(Ld::new(
            reg, reg,
        ))));
        res
    }
}

impl Addressable for LocalVariable {
    fn get_address(&self, reg: Register) -> Vec<sda::ParsedValue> {
        let mut a = Vec::new();
        a.push(sda::ParsedValue::InstructionValue(Box::new(Ldn::new(reg))));
        a.push(sda::ParsedValue::Command(AssemblerCommand::Load(
            MemoryWord::from(self.base_offset),
        )));
        a.push(sda::ParsedValue::InstructionValue(Box::new(Add::new(
            reg,
            reg,
            Register::ArgumentBase,
        ))));
        a
    }
}

impl Variable for LocalVariable {}

pub struct GlobalVariable {
    var_type: SpType,
    var_label: String,
}

impl Expression for GlobalVariable {
    fn get_type(&self) -> SpType {
        self.var_type.clone()
    }

    fn save_value_to(&self, reg: Register, spare: Register) -> Vec<sda::ParsedValue> {
        let mut res = self.get_address(reg);
        res.push(sda::ParsedValue::InstructionValue(Box::new(Ld::new(
            reg, reg,
        ))));
        res
    }
}

impl Addressable for GlobalVariable {
    fn get_address(&self, reg: Register) -> Vec<sda::ParsedValue> {
        let mut a = Vec::new();
        a.push(sda::ParsedValue::InstructionValue(Box::new(Ldn::new(reg))));
        a.push(sda::ParsedValue::Command(AssemblerCommand::LoadLoc(
            self.var_label.clone(),
        )));
        a
    }
}

impl Variable for GlobalVariable {}

pub trait Function: Addressable {
    fn get_input_parameters(&self) -> Vec<(String, SpType)>;
}

pub struct SpcFunction {
    params: Vec<(String, SpType)>,
    return_type: SpType,
    statements: Vec<Box<dyn Statement>>,
}

impl BaseStatement for SpcFunction {}

pub struct AsmFunction {
    params: Vec<(String, SpType)>,
    return_type: SpType,
    lines: Vec<String>,
}

impl AsmFunction {
    pub fn new(params: &[(String, SpType)], return_type: SpType, lines: &[String]) -> Self {
        Self {
            params: params.to_vec(),
            return_type,
            lines: lines.to_vec(),
        }
    }

    pub fn get_assembly(&self) -> Vec<(sol32asm::LocationInfo, sda::ParsedValue)> {
        // TODO - Mangle label names?
        let res = sda::parse_lines(&self.lines.iter().map(|v| v.as_ref()).collect::<Vec<_>>());
        res.unwrap()
    }
}

impl BaseStatement for AsmFunction {}
