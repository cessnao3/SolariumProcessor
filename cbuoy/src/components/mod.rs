pub mod expression;
pub mod statement;
pub mod variable;

use core::fmt;
use expression::ExpressionError;
use jib::cpu::Register;
use jib_asm::{AsmToken, AsmTokenLoc, AssemblerErrorLoc, LocationInfo, TokenList};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    parser::{ParseError, ParserState},
    tokenizer::Token,
    types::TypeError,
};

use self::{
    expression::Expression,
    variable::{GlobalVariable, LocalVariable, Variable},
};

use super::types::{Type, TypeDict};

pub struct AsmGenState {
    pub label_num: u64,
    pub current_register_count: usize,
}

impl AsmGenState {
    pub fn new() -> Self {
        Self {
            label_num: 0,
            current_register_count: Register::first_gp_register().get_index(),
        }
    }

    pub fn reg_a(&self) -> Register {
        Register::try_from(self.current_register_count).unwrap()
    }

    pub fn reg_b(&self) -> Register {
        Register::try_from(self.current_register_count + 1).unwrap()
    }

    pub fn temporary_register(&self) -> Register {
        Register::last_register()
    }
}

enum ParserScopeType {
    Root(HashMap<String, GlobalVariable>),
    Child(Rc<RefCell<ParserScope>>),
}

#[derive(Debug, Clone)]
pub enum ParserScopeError {
    DuplicateName(String),
    UnknownVariable(String),
    TypeError(TypeError),
}

impl From<TypeError> for ParserScopeError {
    fn from(value: TypeError) -> Self {
        Self::TypeError(value)
    }
}

impl fmt::Display for ParserScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateName(n) => write!(
                f,
                "varible name \"{n}\" already exists in the current scope"
            ),
            Self::UnknownVariable(n) => write!(f, "unknown variable name \"{n}\" in scope"),
            Self::TypeError(t) => write!(f, "{t}"),
        }
    }
}

pub struct ParserScope {
    variables: HashMap<String, LocalVariable>,
    scope_type: ParserScopeType,
    base_offset: i32,
}

impl ParserScope {
    pub fn new(parent: Rc<RefCell<Self>>) -> Self {
        Self {
            variables: HashMap::new(),
            scope_type: ParserScopeType::Child(parent),
            base_offset: 0,
        }
    }

    pub fn has_variable(&self, name: &str) -> bool {
        let exists_local = self.variables.contains_key(name);
        let exists_global = match &self.scope_type {
            ParserScopeType::Root(vars) => vars.contains_key(name),
            _ => false,
        };

        exists_global || exists_local
    }

    pub fn add_variable(
        &mut self,
        tok: Token,
        name: &str,
        t: Type,
    ) -> Result<Box<dyn Variable>, ParserScopeError> {
        if self.has_variable(name) {
            return Err(ParserScopeError::DuplicateName(name.into()));
        }

        match &mut self.scope_type {
            ParserScopeType::Root(global_vars) => {
                let var = GlobalVariable::new(tok, name, t);
                global_vars.insert(name.into(), var.clone());
                Ok(Box::new(var))
            }
            _ => {
                let base_offset = self.base_offset;
                self.base_offset += t.byte_count()? as i32;

                let var = LocalVariable::new(tok, t, base_offset);
                self.variables.insert(name.into(), var.clone());
                Ok(Box::new(var))
            }
        }
    }

    pub fn get_variable(&self, s: &str) -> Result<Box<dyn Variable>, ParserScopeError> {
        let gen_err = || Err(ParserScopeError::UnknownVariable(s.into()));

        if let Some(var) = self.variables.get(s) {
            Ok(Box::new(var.clone()))
        } else if let ParserScopeType::Root(globals) = &self.scope_type {
            globals
                .get(s)
                .map_or(gen_err(), |v| Ok(Box::new(v.clone()) as Box<dyn Variable>))
        } else if let ParserScopeType::Child(parent) = &self.scope_type {
            parent.borrow().get_variable(s)
        } else {
            gen_err()
        }
    }

    pub fn get_variable_expr(&self, s: &str) -> Result<Box<dyn Expression>, ParserScopeError> {
        let gen_err = || Err(ParserScopeError::UnknownVariable(s.into()));

        if let Some(var) = self.variables.get(s) {
            Ok(Box::new(var.clone()))
        } else if let ParserScopeType::Root(globals) = &self.scope_type {
            globals.get(s).map_or(
                gen_err(),
                |v| Ok(Box::new(v.clone()) as Box<dyn Expression>),
            )
        } else if let ParserScopeType::Child(parent) = &self.scope_type {
            parent.borrow().get_variable_expr(s)
        } else {
            gen_err()
        }
    }
}

impl Default for ParserScope {
    fn default() -> Self {
        Self {
            variables: HashMap::new(),
            scope_type: ParserScopeType::Root(HashMap::new()),
            base_offset: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ErrorToken {
    tok: Token,
    msg: Option<String>,
}

impl ErrorToken {
    pub fn test<T, E: ToString>(tok: &Token, value: Result<T, E>) -> Result<T, Self> {
        match value {
            Ok(v) => Ok(v),
            Err(e) => Err(Self::new(tok.clone(), &e.to_string())),
        }
    }

    pub fn new<T: ToString>(tok: Token, msg: T) -> Self {
        Self {
            tok,
            msg: Some(msg.to_string()),
        }
    }

    pub fn get_msg(&self) -> Option<&String> {
        self.msg.as_ref()
    }

    pub fn get_msg_text(&self) -> String {
        self.msg.clone().unwrap_or(String::new())
    }

    pub fn get_token(&self) -> &Token {
        &self.tok
    }
}

impl Display for ErrorToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = write!(f, "Error @ {}", self.tok)?;
        if let Some(msg) = &self.msg {
            write!(f, " - {msg}")
        } else {
            Ok(res)
        }
    }
}

impl From<Token> for ErrorToken {
    fn from(value: Token) -> Self {
        Self {
            tok: value,
            msg: None,
        }
    }
}

impl From<AssemblerErrorLoc> for ErrorToken {
    fn from(value: AssemblerErrorLoc) -> Self {
        Self::new(value.loc.into(), &value.err.to_string())
    }
}

pub trait CodeComponent {
    fn generate_init_code(&self, _state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken> {
        return Ok(Vec::new());
    }

    fn generate_code(&self, state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken>;
}

pub trait BaseStatement: CodeComponent {}

pub trait Statement {
    fn stack_size(&self) -> usize;
}

pub trait Function: BaseStatement {
    fn get_input_parameters(&self) -> Vec<(String, Type)>;

    fn get_return_type(&self) -> Option<Type>;
}

pub struct FunctionDefinition {
    name: String,
    parameters: Vec<(String, Type)>,
    return_type: Option<Type>,
    statements: Vec<Box<dyn Statement>>,
}

impl FunctionDefinition {
    pub fn new(
        name: &str,
        parameters: Vec<(String, Type)>,
        return_type: Option<Type>,
        statements: Vec<Box<dyn Statement>>,
    ) -> Self {
        Self {
            name: name.to_string(),
            parameters,
            return_type,
            statements,
        }
    }

    fn assembler_label(&self) -> String {
        format!("func_def_{}", self.name)
    }
}

impl BaseStatement for FunctionDefinition {}

impl CodeComponent for FunctionDefinition {
    fn generate_code(&self, state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken> {
        let tokens = vec![AsmToken::CreateLabel(self.assembler_label())];

        // TODO - Create spots for the input parameters?

        for s in self.statements.iter() {
            todo!("generate code for statements");
        }

        Ok(tokens)
    }
}

impl Function for FunctionDefinition {
    fn get_input_parameters(&self) -> Vec<(String, Type)> {
        self.parameters.clone()
    }

    fn get_return_type(&self) -> Option<Type> {
        self.return_type.clone()
    }
}

pub struct FunctionPtr {
    parameters: Vec<(String, Type)>,
    return_type: Option<Type>,
    addr: u32, // TODO - Update Type Here
}

impl FunctionPtr {
    pub fn new(parameters: Vec<(String, Type)>, return_type: Option<Type>, addr: u32) -> Self {
        Self {
            parameters,
            return_type,
            addr,
        }
    }
}

impl BaseStatement for FunctionPtr {}

impl CodeComponent for FunctionPtr {
    fn generate_code(&self, state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken> {
        todo!("generate code to jump to the stored function pointer?");
    }
}

impl Function for FunctionPtr {
    fn get_input_parameters(&self) -> Vec<(String, Type)> {
        self.parameters.clone()
    }

    fn get_return_type(&self) -> Option<Type> {
        self.return_type.clone()
    }
}

pub struct AsmFunction {
    params: Vec<(String, Type)>,
    return_type: Type,
    lines: Vec<String>,
}

impl AsmFunction {
    pub fn new(params: &[(String, Type)], return_type: Type, lines: &[String]) -> Self {
        Self {
            params: params.to_vec(),
            return_type,
            lines: lines.to_vec(),
        }
    }

    pub fn get_assembly(&self, state: &mut TokenList) -> Result<(), AssemblerErrorLoc> {
        // TODO - Mangle label names?
        for l in self.lines.iter() {
            let loc = LocationInfo {
                line: 0,
                full_line: Some(l.into()),
                base_loc: None,
            };

            if let Err(err) = state.parse_line(l, loc) {
                return Err(AssemblerErrorLoc {
                    err,
                    loc: LocationInfo {
                        base_loc: None,
                        full_line: Some(l.into()),
                        line: 0,
                    },
                });
            }
        }

        Ok(())
    }
}

impl BaseStatement for AsmFunction {}

impl CodeComponent for AsmFunction {
    fn generate_code(&self, state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken> {
        panic!("not implemented");
    }
}
