pub mod expression;
pub mod statement;
pub mod variable;

use jasm::{AssemblerErrorLoc, LocationInfo, TokenList};
use jib::cpu::Register;
use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{parser::ParseError, types::TypeError};

use self::{
    expression::Expression,
    variable::{GlobalVariable, LocalVariable, Variable},
};

use super::types::{Type, TypeDict};

pub struct AsmGenstate {
    pub label_num: u64,
    pub current_register_count: usize,
}

impl AsmGenstate {
    pub fn new() -> Self {
        Self {
            label_num: 0,
            current_register_count: Register::first_gp_register().get_index(),
        }
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
            Self::DuplicateName(n) => write!(f, "varible name \"{n}\" already exists in the current scope"),
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

    pub fn add_variable(&mut self, name: &str, t: Type) -> Result<Box<dyn Variable>, ParserScopeError> {
        if self.has_variable(name) {
            return Err(ParserScopeError::DuplicateName(name.into()));
        }

        match &mut self.scope_type {
            ParserScopeType::Root(global_vars) =>
            {
                let var = GlobalVariable::new(name, t);
                global_vars.insert(name.into(), var.clone());
                Ok(Box::new(var))
            }
            _ =>
            {
                let base_offset = self.base_offset;
                self.base_offset += t.byte_count()? as i32;

                let var = LocalVariable::new(t, base_offset);
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
            globals.get(s).map_or(gen_err(), |v| Ok(Box::new(v.clone()) as Box<dyn Variable>))
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
            globals.get(s).map_or(gen_err(), |v| Ok(Box::new(v.clone()) as Box<dyn Expression>))
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

pub trait CodeComponent {
    fn generate_code(&self, state: &mut TokenList);
}

pub trait BaseStatement {}

pub trait Statement {
    fn stack_size(&self) -> usize;
}

pub trait Function: BaseStatement {
    fn get_input_parameters(&self) -> Vec<(String, Type)>;

    fn get_return_type(&self) -> Option<Type>;
}

pub struct FunctionDefinition {
    parameters: Vec<(String, Type)>,
    return_type: Option<Type>,
    statements: Vec<Box<dyn Statement>>,
}

impl FunctionDefinition {
    pub fn new(
        parameters: Vec<(String, Type)>,
        return_type: Option<Type>,
        statements: Vec<Box<dyn Statement>>,
    ) -> Self {
        Self {
            parameters,
            return_type,
            statements,
        }
    }
}

impl BaseStatement for FunctionDefinition {}

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
