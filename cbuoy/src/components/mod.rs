pub mod expression;
pub mod variable;

use jasm::{AssemblerErrorLoc, LocationInfo, TokenList};
use jib::cpu::Register;
use std::collections::HashMap;

use self::{expression::Expression, variable::{GlobalVariable, LocalVariable, Variable}};

use super::types::{Type, TypeDict};

pub struct CompilerState {
    pub globals: HashMap<String, GlobalVariable>,
    //pub functions: HashMap<String, Box<dyn Function>>,
    pub types: TypeDict,
    pub scopes: Vec<Scope>,
}

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

pub struct Scope {
    pub variables: HashMap<String, LocalVariable>,
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
            types: TypeDict::new(),
        }
    }

    pub fn get_variable(&self, s: &str) -> Option<Box<dyn Variable>> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables.get(s) {
                return Some(Box::new(var.clone()));
            }
        }

        if let Some(var) = self.globals.get(s) {
            return Some(Box::new(var.clone()));
        } else {
            return None;
        }
    }

    pub fn get_variable_expr(&self, s: &str) -> Option<Box<dyn Expression>> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables.get(s) {
                return Some(Box::new(var.clone()));
            }
        }

        if let Some(var) = self.globals.get(s) {
            return Some(Box::new(var.clone()));
        } else {
            return None;
        }
    }
}

impl Default for CompilerState {
    fn default() -> Self {
        Self::new()
    }
}

pub trait CodeComponent {
    fn generate_code(&self, state: &mut TokenList);
}

pub trait BaseStatement {}

pub trait Statement {}

pub struct DefinitionStatement {
    var_type: Type,
    var_name: String,
    init_expr: Option<Box<dyn expression::Expression>>,
}

impl DefinitionStatement {
    pub fn new(name: &str, t: Type) -> Self {
        Self {
            var_type: t,
            var_name: name.into(),
            init_expr: None,
        }
    }

    pub fn set_init(&mut self, expr: Box<dyn expression::Expression>) {
        self.init_expr = Some(expr);
    }
}

impl Statement for DefinitionStatement {}

impl BaseStatement for DefinitionStatement {}

pub trait Function: Expression {
    fn get_input_parameters(&self) -> Vec<(String, Type)>;
}

pub struct SpcFunction {
    params: Vec<(String, Type)>,
    return_type: Type,
    statements: Vec<Box<dyn Statement>>,
}

impl BaseStatement for SpcFunction {}

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
