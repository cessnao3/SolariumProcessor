use crate::tokenizer::Token;

mod common;
mod variable;
mod function;

mod expression;
mod statement;

mod statement_if;
mod statement_while;

use common::{EmitAssembly, ScopeManager};

struct ProgramTree
{
    statements: Vec<Box<dyn EmitAssembly>>
}

pub fn lexer(tokens: Vec<Token>) -> Result<Vec<Box<dyn EmitAssembly>>, String>
{
    let mut scopes = ScopeManager::new();

    return Err("lexer not implemented".to_string());
}
