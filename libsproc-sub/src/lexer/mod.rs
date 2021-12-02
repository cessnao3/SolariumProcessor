use crate::tokenizer::Token;

mod common;
mod variable;
mod function;

mod expression;
mod statement;

mod statement_if;

mod token_iter;

use self::common::ScopeManager;
use self::common::REG_FRAME_SP_BASE;
use self::statement::read_base_statement;
use self::token_iter::TokenIter;



pub fn lexer(tokens: Vec<Token>) -> Result<Vec<String>, String>
{
    let mut token_iter = TokenIter::new(tokens.clone());

    let mut scopes = ScopeManager::new();

    let mut assembly: Vec<String> = Vec::new();
    assembly.push(".load 0x4000".to_string());
    assembly.push(".load 0x4000".to_string());

    assembly.push(".oper 0x4000".to_string());

    assembly.push("; Load and call the main function".to_string());
    assembly.push("jmpri 3".to_string());
    assembly.push(format!(".load {0:}", libsproc::cpu::SolariumProcessor::STACK_POINTER_OFFSET));
    assembly.push(".loadloc main_entry_point".to_string());
    assembly.push(format!("ldri {0:}, -2", REG_FRAME_SP_BASE));
    assembly.push("ldri 5, -2".to_string());
    assembly.push("call 5".to_string());

    assembly.push("; Infinite Loop Ending Program".to_string());
    assembly.push("jmpri 0".to_string());
    // Load the main function location and call!

    // Define the scope results
    assembly.extend(scopes.add_scope());

    loop
    {
        match read_base_statement(&mut token_iter, &mut scopes)
        {
            Ok(Some(v)) => assembly.extend(v),
            Ok(None) => break,
            Err(e) => return Err(e)
        };
    }

    assembly.extend(scopes.pop_scope());

    return Ok(assembly);
}
