use jib_asm::AsmToken;

use crate::{tokenizer::Token, types::Type};

use super::{
    expression::Expression, variable::Variable, AsmGenState, BaseStatement, CodeComponent,
    CodeLocation, ErrorToken, Statement,
};

pub struct GlobalDefinitionStatement {
    pub tok: Token,
    pub name: String,
    pub var_type: Type,
    pub init_expr: Option<Box<dyn Expression>>,
}

impl GlobalDefinitionStatement {
    pub fn new(tok: Token, name: &str, var_type: Type) -> Self {
        Self {
            tok,
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
    fn stack_size(&self) -> Result<usize, ErrorToken> {
        ErrorToken::test(self.get_token(), self.var_type.byte_count())
    }
}

impl CodeLocation for GlobalDefinitionStatement {
    fn get_token(&self) -> &Token {
        &self.tok
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

        for _ in 0..self.stack_size()? {
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
    fn stack_size(&self) -> Result<usize, ErrorToken> {
        Ok(0)
    }
}

impl CodeLocation for ExpressionStatement {
    fn get_token(&self) -> &Token {
        self.expr.get_token()
    }
}

pub struct IfStatement {
    pub tok: Token,
    pub conditional: Box<dyn Expression>,
    pub statements: Vec<Box<dyn Statement>>,
    pub else_clause: Option<Box<dyn Statement>>,
}

impl Statement for IfStatement {
    fn stack_size(&self) -> Result<usize, ErrorToken> {
        type SizeError = Result<usize, ErrorToken>;

        fn sum_size(a: SizeError, b: SizeError) -> SizeError {
            Ok(a? + b?)
        }

        self.statements
            .iter()
            .map(|s| s.stack_size())
            .fold(Ok(0), sum_size)
    }
}

impl CodeLocation for IfStatement {
    fn get_token(&self) -> &Token {
        &self.tok
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
    fn stack_size(&self) -> Result<usize, ErrorToken> {
        let t = ErrorToken::test(self.get_token(), self.var.get_type())?;
        ErrorToken::test(self.get_token(), t.byte_count())
    }
}

impl CodeLocation for VariableInitStatement {
    fn get_token(&self) -> &Token {
        self.var.get_token()
    }
}

pub struct ReturnStatement {
    pub tok: Token,
    pub expr: Box<dyn Expression>,
}

impl Statement for ReturnStatement {
    fn stack_size(&self) -> Result<usize, ErrorToken> {
        Ok(0)
    }
}

impl CodeLocation for ReturnStatement {
    fn get_token(&self) -> &Token {
        &self.tok
    }
}
