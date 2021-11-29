use crate::{lexer::{common::{REG_DEFAULT_SPARE, REG_DEFAULT_TEST_RESULT}, variable::Variable}, tokenizer::{Token, Keyword, Symbol}};

mod common;
mod variable;
mod function;

mod expression;
mod statement;

mod statement_if;
mod statement_while;

mod token_iter;

use common::ScopeManager;

use std::rc::Rc;

use self::{common::NamedMemoryValue, variable::StaticVariable};

use self::token_iter::TokenIter;

use self::expression::read_expression;

enum VariableType
{
    Stack,
    Static
}

fn read_statement(iter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<Vec<String>, String>
{
    // Define the assembly list
    let mut assembly = Vec::new();

    // Check for a return statement
    let first_token;
    if let Some(tok) = iter.peek()
    {
        first_token = tok;
    }
    else
    {
        return Err(format!("unexpected end of stream found"));
    }

    match first_token
    {
        Token::Symbol(Symbol::OpenBrace) =>
        {
            match read_statement_brackets(iter, scopes)
            {
                Ok(v) => assembly.extend(v),
                Err(e) => return Err(e)
            };
        },
        Token::Keyword(Keyword::Auto) =>
        {
            match read_variable_def(iter, scopes, VariableType::Stack)
            {
                Ok(v) => assembly.extend(v),
                Err(e) => return Err(e)
            };
        },
        Token::Keyword(Keyword::Return) =>
        {
            // Read the init return keyword
            iter.next();

            // Check for an expression
            if let Some(Token::Symbol(Symbol::Semicolon)) = iter.peek()
            {
                // Ignore semicolon for now
            }
            else
            {
                match read_expression(iter, scopes, REG_DEFAULT_TEST_RESULT, REG_DEFAULT_SPARE)
                {
                    Ok(v) => assembly.extend(v),
                    Err(e) => return Err(e)
                };

                assembly.push(format!("copy $ret, {0:}", REG_DEFAULT_TEST_RESULT))
            }

            // Check for the ending semicolon
            if let Some(Token::Symbol(Symbol::Semicolon)) = iter.peek()
            {
                iter.next();
                let end_label = scopes.get_function_end_label().unwrap();

                assembly.extend(scopes.assembly_to_pop_for_return());

                assembly.push("jmpri 2".to_string());
                assembly.push(format!(".loadloc {0:}", end_label));
                assembly.push(format!("ldir {0:}, -1", REG_DEFAULT_TEST_RESULT));
                assembly.push(format!("jmp {0:}", REG_DEFAULT_TEST_RESULT));
            }
            else
            {
                return Err("return statement must end in a semicolon".to_string());
            }
        },
        _ =>
        {
            match read_expression(iter, scopes, 6, 7)
            {
                Ok(v) => assembly.extend(v),
                Err(e) => return Err(e)
            };

            if let Some(Token::Symbol(Symbol::Semicolon)) = iter.next()
            {
                // Do Nothing
            }
            else
            {
                return Err(format!("expected semicolon at the end of the expression"));
            }
        }
    }


    // Return the resulting values
    return Ok(assembly);
}

fn read_statement_brackets(iter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<Vec<String>, String>
{
    // Define the assembly results
    let mut assembly = Vec::new();

    // Add a new scope
    assembly.extend(scopes.add_scope());

    // Read the statement list
    if let Some(Token::Symbol(Symbol::OpenBrace)) = iter.next()
    {
        loop
        {
            if let Some(Token::Symbol(Symbol::CloseBrace)) = iter.peek()
            {
                break;
            }

            match read_statement(iter, scopes)
            {
                Ok(v) => assembly.extend(v),
                Err(e) => return Err(e)
            };
        }
    }

    // Clear the ending scope
    assembly.extend(scopes.pop_scope());

    // Define the function ending by clearing the closing brace
    iter.next();

    // Return the successful assembly
    return Ok(assembly);
}

fn read_variable_def(iter: &mut TokenIter, scopes: &mut ScopeManager, variable_type: VariableType) -> Result<Vec<String>, String>
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

                assembly.push("jmpri 2".to_string());
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
        match read_expression(iter, scopes, 6, 7)
        {
            Ok(asm) =>
            {
                assembly.extend(asm);
                assembly.extend(variable_value.set_value_from_register(6, 7));
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

fn read_base_statement(iter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<Option<Vec<String>>, String>
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
                // Extract the function name
                let function_name;
                if let Some(Token::Name(name)) = iter.next()
                {
                    function_name = name;
                }
                else
                {
                    return Err(format!("function definition expects a function name"));
                }

                // Read the list of variables
                if let Some(Token::Symbol(Symbol::OpenParen)) = iter.next()
                {
                    // Clear open paren
                }
                else
                {
                    return Err(format!("function definition expects ( for the function list"));
                }

                // Define the variable name list
                let mut varnames = Vec::new();

                // Loop through the results
                loop
                {
                    if let Some(Token::Name(varname)) = iter.peek()
                    {
                        iter.next();
                        varnames.push(varname);

                        if let Some(Token::Symbol(Symbol::Comma)) = iter.peek()
                        {
                            iter.next();
                        }
                    }
                    else if let Some(Token::Symbol(Symbol::CloseParen)) = iter.peek()
                    {
                        iter.next();
                        break;
                    }
                    else if let Some(tok) = iter.peek()
                    {
                        return Err(format!("unexpected token {0:} found", tok.to_string()));
                    }
                    else
                    {
                        return Err(format!("unexpected end of stream found"));
                    }
                }

                // Check for the open brace
                if let Some(Token::Symbol(Symbol::OpenBrace)) = iter.peek()
                {
                    // Do Nothing
                }
                else
                {
                    return Err(format!("no new scope provided"));
                }

                // Determine the function label
                let function_label = format!("function_{0:}_{1:}", function_name, scopes.generate_index());
                let function_label_end = format!("{0:}_end", function_label);

                // Add the init values
                assembly.push(format!("; {0:}({1:})", function_name, varnames.join(", ")));
                assembly.push(format!(":{0:}_start", function_label));

                // Define the new scope list and add offset values
                assembly.extend(scopes.add_function_scope(&function_label_end));

                for (i, name) in varnames.iter().enumerate()
                {
                    match scopes.add_variable(name, Rc::new(Variable::new(name, -(i as i32) - 16)))
                    {
                        Ok(()) => (),
                        Err(e) => return Err(e)
                    };
                }

                match read_statement_brackets(iter, scopes)
                {
                    Ok(v) => assembly.extend(v),
                    Err(e) => return Err(e)
                };

                // Clear the scope
                assembly.extend(scopes.pop_scope());

                // Provide the return call
                assembly.push(format!(":{0:}", function_label_end));
                assembly.push("ret".to_string());
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
    let mut token_iter = TokenIter::new(tokens.clone());

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
