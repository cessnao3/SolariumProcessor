use jib_asm::{AsmToken, AsmTokenLoc, LocationInfo};

use crate::types::Type;

use super::{
    expression::Expression, variable::Variable, AsmGenState, BaseStatement, CodeComponent,
    ErrorToken, Statement,
};

pub struct GlobalDefinitionStatement {
    pub name: String,
    pub var_type: Type,
    pub init_expr: Option<Box<dyn Expression>>,
}

impl GlobalDefinitionStatement {
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

    fn assmebler_label(&self) -> String {
        format!("static_var_{}", self.name)
    }
}

impl Statement for GlobalDefinitionStatement {
    fn stack_size(&self) -> usize {
        self.var_type.byte_count().unwrap()
    }
}

impl BaseStatement for GlobalDefinitionStatement {}

impl CodeComponent for GlobalDefinitionStatement {
    fn generate_init_code(&self, state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken> {
        if let Some(e) = &self.init_expr {
            Ok(ErrorToken::test(
                &e.get_token(),
                e.load_to(state.reg_a(), state.reg_b(), state),
            )?)
        } else {
            Ok(Vec::new())
        }
    }

    fn generate_code(&self, state: &mut AsmGenState) -> Result<Vec<AsmToken>, ErrorToken> {
        let mut v = vec![jib_asm::AsmToken::CreateLabel(self.assmebler_label())];

        for _ in 0..self.stack_size() {
            v.push(jib_asm::AsmToken::Literal1(0));
        }

        Ok(v)
    }
}

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

pub struct ReturnStatement {
    pub expr: Box<dyn Expression>,
}

impl Statement for ReturnStatement {
    fn stack_size(&self) -> usize {
        0
    }
}
