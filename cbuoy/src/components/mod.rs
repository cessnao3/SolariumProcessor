mod addressable;
mod expression;
mod variable;

use jasm::{AssemblerErrorLoc, LocationInfo, TokenList};
use std::collections::HashMap;

use self::{expression::Expression, variable::{GlobalVariable, Variable}, addressable::Addressable};

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
    fn generate_code(&self, state: &mut TokenList);
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
