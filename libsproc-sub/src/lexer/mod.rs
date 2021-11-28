use crate::{lexer::variable::Variable, tokenizer::{Token, Keyword, StringLiteral, Symbol}};

mod common;
mod variable;
mod function;

mod expression;
mod statement;

mod statement_if;
mod statement_while;

use common::{EmitAssembly, ScopeManager};

use std::rc::Rc;

use self::{common::NamedMemoryValue, variable::StaticVariable};

enum VariableType
{
    Stack,
    Static
}

fn read_expression(iter: &mut core::slice::Iter<Token>, scopes: &mut ScopeManager, register: usize) -> Result<Vec<String>, String>
{
    return Err(format!("not implemented"));
}

fn read_variable_def(iter: &mut core::slice::Iter<Token>, scopes: &mut ScopeManager, variable_type: VariableType) -> Result<Vec<String>, String>
{
    // Define the variable name
    let variable_name: String;
    let variable_value: Rc<dyn NamedMemoryValue>;
    let variable_size: usize;

    // Define the assembly to emit
    let mut assembly = Vec::new();

    // Check the variable type
    if let Some(Token::Keyword(Keyword::Auto)) = iter.next()
    {
        // Defines the variable size, type
        variable_size = 1;
    }
    else
    {
        return match iter.last()
        {
            Some(t) => Err(format!("no valid variable type provided, found {0:}", t.to_string())),
            None => Err(format!("unexpected end of token stream"))
        };
    }

    // Check the variable name
    if let Some(Token::Name(name)) = iter.next()
    {
        variable_name = name.clone();

        match variable_type
        {
            VariableType::Static =>
            {
                let variable_label = format!("static_variable_{0:}_{1:}", variable_name, scopes.generate_index());

                assembly.push(format!(":{0:}", variable_label));
                for _ in 0..variable_size
                {
                    assembly.push(".load 0".to_string());
                }

                variable_value = Rc::new(StaticVariable::new(
                    &variable_name,
                    &variable_label));
            }
            VariableType::Stack =>
            {
                let variable_offset = scopes.get_stack_offset();
                scopes.add_stack_offset(variable_size);

                variable_value = Rc::new(Variable::new(
                    &variable_name,
                    variable_offset as i32));

                for _ in 0..variable_size
                {
                    assembly.push("push 16".to_string());
                }
            }
        };

        match scopes.add_variable(&variable_name, variable_value.clone())
        {
            Ok(()) => (),
            Err(e) => return Err(e)
        };
    }
    else
    {
        return Err(format!("expected name for variable definition"));
    }

    // Read the assignment operator if present for initial value
    let next_val = iter.next();
    if let Some(Token::Symbol(Symbol::Assignment)) = next_val
    {
        // Read the expression to assign the variable to
        match read_expression(iter, scopes, 6)
        {
            Ok(asm) =>
            {
                assembly.extend(asm);
                assembly.extend(variable_value.set_value_from_register(6));
            },
            Err(e) => return Err(e)
        };

        // Check for final semicolon
        if let Some(Token::Symbol(Symbol::Semicolon)) = iter.next()
        {
            // Skip the closing assignment operator
            return Ok(assembly);
        }
        else
        {
            return Err(match iter.last()
            {
                Some(t) => format!("expected closing semicolon, found {0:}", t.to_string()),
                None => format!("unexpected end of token stream")
            });
        }
    }
    else if let Some(Token::Symbol(Symbol::Semicolon)) = next_val
    {
        // Return as-is
        return Ok(assembly);
    }
    else
    {
        return Err(match next_val
        {
            Some(t) => format!("unexpected symbol {0:} found, expected assignment or semicolon", t.to_string()),
            None => format!("unexpected end of token stream")
        });
    }
}

fn read_base_statement(iter: &mut core::slice::Iter<Token>, scopes: &mut ScopeManager) -> Result<Option<Vec<String>>, String>
{
    if let Some(tok) = iter.next()
    {
        let mut assembly: Vec<String> = Vec::new();

        match tok
        {
            Token::Keyword(Keyword::Static) =>
            {
                match read_variable_def(iter, scopes, VariableType::Static)
                {
                    Ok(v) => assembly.extend(v),
                    Err(e) => return Err(e)
                };
            },
            Token::Keyword(Keyword::Func) =>
            {

            },
            _ => return Err(format!("unable to find base expression for token {0:}", tok.to_string()))
        }

        return Ok(Some(assembly));
    }
    else
    {
        return Ok(None);
    }
}

pub fn lexer(tokens: Vec<Token>) -> Result<Vec<String>, String>
{
    let mut test = tokens.iter();

    let mut scopes = ScopeManager::new();

    let mut assembly: Vec<String> = Vec::new();
    assembly.push(".load 0x4000".to_string());
    assembly.push(".load 0x4000".to_string());

    assembly.push(".oper 0x4000".to_string());

    assembly.push("jmpri 2".to_string());
    // Load the main function location and call!

    // Define the scope results
    assembly.extend(scopes.add_scope());

    loop
    {
        match read_base_statement(&mut test, &mut scopes)
        {
            Ok(Some(v)) => assembly.extend(v),
            Ok(None) => break,
            Err(e) => return Err(e)
        };
    }

    assembly.extend(scopes.pop_scope());

    return Ok(assembly);
}
