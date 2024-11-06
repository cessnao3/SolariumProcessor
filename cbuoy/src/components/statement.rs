use crate::types::Type;

use super::{expression::Expression, variable::Variable, BaseStatement, Statement};

pub struct DefinitionStatement {
    pub name: String,
    pub var_type: Type,
    pub init_expr: Option<Box<dyn Expression>>,
}

impl DefinitionStatement {
    pub fn new(name: &str, var_type: Type) -> Self {
        Self {
            name: name.into(),
            var_type,
            init_expr: None,
        }
    }

    pub fn set_init(&mut self, expr: Box<dyn Expression>) {
        self.init_expr = Some(expr);
    }
}

impl Statement for DefinitionStatement {
    fn stack_size(&self) -> usize {
        0
    }
}

impl BaseStatement for DefinitionStatement {}

pub struct ExpressionStatement {
    pub expr: Box<dyn Expression>,
}

impl ExpressionStatement {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        Self { expr }
    }
}

impl Statement for ExpressionStatement {
    fn stack_size(&self) -> usize {
        0
    }
}

pub struct IfStatement {
    pub conditional: Box<dyn Expression>,
    pub statements: Vec<Box<dyn Statement>>,
    pub else_clause: Option<Box<dyn Statement>>,
}

impl Statement for IfStatement {
    fn stack_size(&self) -> usize {
        self.statements
            .iter()
            .map(|s| s.stack_size())
            .fold(0, |a, b| a + b)
    }
}

pub struct VariableInitStatement {
    var: Box<dyn Variable>,
    init_expr: Option<Box<dyn Expression>>,
}

impl VariableInitStatement {
    pub fn new(var: Box<dyn Variable>, init_expr: Option<Box<dyn Expression>>) -> Self {
        Self { var, init_expr }
    }
}

impl Statement for VariableInitStatement {
    fn stack_size(&self) -> usize {
        self.var
            .get_type()
            .map_or(0, |t| t.byte_count().unwrap_or(0))
    }
}
