use crate::tokenizer::Token;

mod common;
mod variable;
mod function;

mod expression;
mod statement;

mod token_iter;

mod program;

use self::common::ScopeManager;
use self::common::REG_FRAME_SP_BASE;
use self::statement::read_base_statement;
use self::token_iter::TokenIter;
use self::program::ProgramSection;


pub fn lexer(tokens: Vec<Token>) -> Result<Vec<String>, String>
{
    let mut token_iter = TokenIter::new(tokens.clone());

    let mut scopes = ScopeManager::new();

    let mut program = ProgramSection::new_static();
    program.extend(vec![
        "; Load and call the main function".to_string(),
        "jmpri 3".to_string(),
        format!(".load {0:}", libsproc::cpu::SolariumProcessor::STACK_POINTER_OFFSET),
        ".loadloc main_entry_point".to_string(),
        format!("ldri {0:}, -2", REG_FRAME_SP_BASE),
        "ldri 5, -2".to_string(),
        "call 5".to_string(),
        "; Infinite Loop Ending Program".to_string(),
        "jmpri 0".to_string()]);

    // Define the scope results
    program.extend(scopes.add_scope());

    loop
    {
        match read_base_statement(&mut token_iter, &mut scopes)
        {
            Ok(Some(v)) => program.append(v),
            Ok(None) => break,
            Err(e) => return Err(e)
        };
    }

    program.extend(scopes.pop_scope());

    let mut resulting_instructions = Vec::new();
    resulting_instructions.extend(vec![
        ".load 0x2000".to_string(),
        ".load 0x2000".to_string(),
        "; Program Start".to_string(),
        ".oper 0x2000".to_string()
    ]);

    resulting_instructions.extend(program.get_static_assembly().iter().map(|v| v.clone()).collect::<Vec<_>>());
    resulting_instructions.extend(program.get_primary_assembly().iter().map(|v| v.clone()).collect::<Vec<_>>());

    return Ok(resulting_instructions);
}
