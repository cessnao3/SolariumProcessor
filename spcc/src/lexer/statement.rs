use super::common::{ScopeManager, NamedMemoryValue, REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_TEST_JUMP_B, REG_DEFAULT_SPARE, REG_DEFAULT_TEST_RESULT};
use super::function::FunctionDefinition;
use super::program::ProgramSection;
use super::token_iter::TokenIter;

use super::variable::{StaticVariable, Variable};

use crate::tokenizer::{Token, Symbol, Keyword};

use super::expression::read_base_expression;

use std::rc::Rc;

enum VariableType
{
    Stack,
    Static
}


fn read_statement(iter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<ProgramSection, String>
{
    // Define the assembly list
    let mut program = ProgramSection::new();

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
                Ok(v) => program.append(v),
                Err(e) => return Err(e)
            };
        },
        Token::Keyword(Keyword::Auto) =>
        {
            match read_variable_def(iter, scopes, VariableType::Stack)
            {
                Ok(v) => program.append(v),
                Err(e) => return Err(e)
            };
        },
        Token::Keyword(Keyword::While) =>
        {
            // Read the first token
            iter.next();

            // Read the list of variables
            if let Some(Token::Symbol(Symbol::OpenParen)) = iter.next()
            {
                // Clear open paren
            }
            else
            {
                return Err(format!("while loop definition expects ( for the expression start"));
            }

            // Define the while label
            let while_label = format!("while_loop_{0:}", scopes.generate_index());

            program.push(format!(":{0:}_start", while_label));

            // Read the statement value
            match read_base_expression(iter, scopes, REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_SPARE)
            {
                Ok(v) => program.extend(v),
                Err(e) => return Err(e)
            };

            // Read the close paren
            if let Some(Token::Symbol(Symbol::CloseParen)) = iter.next()
            {
                // Clear open paren
            }
            else
            {
                return Err(format!("while loop definition expects ) for expression end"));
            }

            // Determine where to jump
            program.extend(vec![
                format!("jmpri 2"),
                format!(".loadloc {0:}_end", while_label),
                format!("ldri {0:}, -1", REG_DEFAULT_SPARE),
                format!("tz {0:}", REG_DEFAULT_TEST_JUMP_A),
                format!("jmp {0:}", REG_DEFAULT_SPARE)
            ]);

            // Loop back to the start
            program.push("; while loop content start".to_string());

            // Read the resulting values
            match read_statement_brackets(iter, scopes)
            {
                Ok(v) => program.append(v),
                Err(e) => return Err(e)
            };

            // Loop back to the start
            program.extend(vec![
                format!("jmpri 2"),
                format!(".loadloc {0:}_start", while_label),
                format!("ldri {0:}, -1", REG_DEFAULT_SPARE),
                format!("jmp {0:}", REG_DEFAULT_SPARE)
            ]);

            // Define the ending value
            program.push(format!(":{0:}_end", while_label));

        },
        Token::Keyword(Keyword::If) =>
        {
            // Read the if keyword
            iter.next();

            // Determine the input expression
            // Read the list of variables
            if let Some(Token::Symbol(Symbol::OpenParen)) = iter.next()
            {
                // Clear open paren
            }
            else
            {
                return Err(format!("if loop definition expects ( for the expression start"));
            }

            // Define the if label
            let if_label = format!("if_statement_{0:}", scopes.generate_index());

            // Read the statement value
            match read_base_expression(iter, scopes, REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_SPARE)
            {
                Ok(v) => program.extend(v),
                Err(e) => return Err(e)
            };

            // Read the close paren
            if let Some(Token::Symbol(Symbol::CloseParen)) = iter.next()
            {
                // Clear open paren
            }
            else
            {
                return Err(format!("while loop definition expects ) for expression end"));
            }

            // Define the resulting jumps
            program.extend(vec![
                format!("jmpri 3"),
                format!(".loadloc {0:}_true", if_label),
                format!(".loadloc {0:}_false", if_label),
                format!("ldri {0:}, -2", REG_DEFAULT_TEST_JUMP_B),
                format!("ldri {0:}, -2", REG_DEFAULT_SPARE),
                format!("tnz {0:}", REG_DEFAULT_TEST_JUMP_A),
                format!("jmp {0:}", REG_DEFAULT_TEST_JUMP_B),
                format!("jmp {0:}", REG_DEFAULT_SPARE)
            ]);

            // Read the "true" block
            program.push(format!(":{0:}_true", if_label));

            match read_statement(iter, scopes)
            {
                Ok(v) => program.append(v),
                Err(e) => return Err(e)
            };

            if let Some(Token::Keyword(Keyword::Else)) = iter.peek()
            {
                // Clear the next
                iter.next();

                // Read the "false" block if it exists
                program.extend(vec![
                    format!("jmpri 2"),
                    format!(".loadloc {0:}_end", if_label),
                    format!("ldri {0:}, -1", REG_DEFAULT_SPARE),
                    format!("jmp {0:}", REG_DEFAULT_SPARE),
                    format!(":{0:}_false", if_label)
                ]);

                // Read the next statement
                match read_statement(iter, scopes)
                {
                    Ok(v) => program.append(v),
                    Err(e) => return Err(e)
                };
            }
            else
            {
                // Read the "false" block if it exists
                program.push(format!(":{0:}_false", if_label));
            }

            // Mark the end of the if statement
            program.push(format!(":{0:}_end", if_label));
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
                match read_base_expression(iter, scopes, REG_DEFAULT_TEST_RESULT, REG_DEFAULT_SPARE)
                {
                    Ok(v) => program.extend(v),
                    Err(e) => return Err(e)
                };

                program.push(format!("copy $ret, {0:}", REG_DEFAULT_TEST_RESULT))
            }

            // Check for the ending semicolon
            if let Some(Token::Symbol(Symbol::Semicolon)) = iter.peek()
            {
                iter.next();
                let end_label = scopes.get_function_end_label().unwrap();

                program.extend(scopes.assembly_to_pop_for_return());

                program.extend(vec![
                    "jmpri 2".to_string(),
                    format!(".loadloc {0:}", end_label),
                    format!("ldri {0:}, -1", REG_DEFAULT_TEST_RESULT),
                    format!("jmp {0:}", REG_DEFAULT_TEST_RESULT)
                ]);
            }
            else
            {
                return Err("return statement must end in a semicolon".to_string());
            }
        },
        _ =>
        {
            match read_base_expression(iter, scopes, REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_TEST_JUMP_B)
            {
                Ok(v) => program.extend(v),
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
    return Ok(program);
}

fn read_variable_def(iter: &mut TokenIter, scopes: &mut ScopeManager, variable_type: VariableType) -> Result<ProgramSection, String>
{
    // Define the variable name
    let variable_name: String;
    let variable_value: Rc<dyn NamedMemoryValue>;
    let variable_size: usize;

    // Define the assembly to emit
    let mut program = ProgramSection::new_static();

    // Check the variable type
    if let Some(Token::Keyword(Keyword::Auto)) = iter.next()
    {
        // Currently auto is the only type allowed
    }
    else
    {
        return match iter.last()
        {
            Some(t) => Err(format!("no valid variable type provided, found {0:}", t.to_string())),
            None => Err(format!("unexpected end of token stream"))
        };
    }

    // Check for a variable size definition
    if let Some(Token::Symbol(Symbol::OpenBracket)) = iter.peek()
    {
        // Clear the bracket
        iter.next();

        // Check for a word literal to create the size
        match iter.next()
        {
            Some(Token::WordLiteral(val)) => variable_size = val as usize,
            Some(tok) => return Err(format!("size must be provided as a word literal, found {0:}", tok.to_string())),
            None => return Err("unexpected end of token stream".to_string())
        };

        // Check for a closing brace
        match iter.next()
        {
            Some(Token::Symbol(Symbol::CloseBracket)) => (),
            _ => return Err("expected close brace after variable size definition".to_string())
        };
    }
    else
    {
        variable_size = 1;
    }

    // Check the variable name
    if let Some(Token::VariableName(name)) = iter.next()
    {
        variable_name = name.clone();

        match variable_type
        {
            VariableType::Static =>
            {
                let variable_label = format!("static_variable_{0:}_{1:}", variable_name, scopes.generate_index());

                program.push_static("jmpri 2".to_string());
                program.push_static(format!(".loadloc {0:}_end", variable_label));
                program.push_static(format!("ldri {0:}, -1", REG_DEFAULT_TEST_RESULT));
                program.push_static(format!("jmp {0:}", REG_DEFAULT_TEST_RESULT));
                program.push_static(format!(":{0:}", variable_label));
                for _ in 0..variable_size
                {
                    program.push_static(".load 0".to_string());
                }
                program.push_static(format!(":{0:}_end", variable_label));

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
                    program.push("push 15".to_string());
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
        match read_base_expression(iter, scopes, REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_TEST_JUMP_B)
        {
            Ok(asm) =>
            {
                let mut vals = Vec::new();
                vals.extend(asm);
                vals.extend(variable_value.set_value_from_register(
                    REG_DEFAULT_TEST_JUMP_A,
                    REG_DEFAULT_TEST_JUMP_B));

                match variable_type
                {
                    VariableType::Stack => program.extend(vals),
                    VariableType::Static => program.extend_static(vals)
                };
            },
            Err(e) => return Err(e)
        };

        // Check for final semicolon
        if let Some(Token::Symbol(Symbol::Semicolon)) = iter.next()
        {
            // Skip the closing assignment operator
            return Ok(program);
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
        return Ok(program);
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

pub fn read_base_statement(iter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<Option<ProgramSection>, String>
{
    if let Some(tok) = iter.next()
    {
        let mut program = ProgramSection::new_static();

        match tok
        {
            Token::Keyword(Keyword::Static) =>
            {
                match read_variable_def(iter, scopes, VariableType::Static)
                {
                    Ok(v) => program.append(v),
                    Err(e) => return Err(e)
                };
            },
            Token::Keyword(Keyword::Func) =>
            {
                // Extract the function name
                let function_name;
                if let Some(Token::FunctionName(name)) = iter.next()
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
                    if let Some(Token::VariableName(varname)) = iter.peek()
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
                program.push(format!("; {0:}({1:})", function_name, varnames.join(", ")));
                program.push(format!(":{0:}_start", function_label));
                if function_name == "main"
                {
                    program.push(format!(":main_entry_point"));
                }

                // Define the new scope list and add offset values
                program.extend(scopes.add_function_scope(&function_label_end));

                for (i, name) in varnames.iter().enumerate()
                {
                    match scopes.add_variable(name, Rc::new(Variable::new(name, (i as i32) - 16 - varnames.len() as i32)))
                    {
                        Ok(()) => (),
                        Err(e) => return Err(e)
                    };
                }

                let func = Rc::new(FunctionDefinition::new(
                    &function_name,
                    varnames.len(),
                    &format!("{0:}_start", function_label)));

                match scopes.add_function(&function_name, func)
                {
                    Ok(()) => (),
                    Err(e) => return Err(e)
                };

                match read_statement_brackets(iter, scopes)
                {
                    Ok(v) => program.append(v),
                    Err(e) => return Err(e)
                };

                // Clear the scope
                program.extend(scopes.pop_scope());

                // Provide the return call
                program.push(format!(":{0:}", function_label_end));
                program.push("ret".to_string());
            },
            _ => return Err(format!("unable to find base expression for token {0:}", tok.to_string()))
        }

        return Ok(Some(program));
    }
    else
    {
        return Ok(None);
    }
}

fn read_statement_brackets(iter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<ProgramSection, String>
{
    // Define the assembly results
    let mut program = ProgramSection::new();

    // Add a new scope
    program.extend(scopes.add_scope());

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
                Ok(v) => program.append(v),
                Err(e) => return Err(e)
            };
        }
    }

    // Clear the ending scope
    program.extend(scopes.pop_scope());

    // Define the function ending by clearing the closing brace
    iter.next();

    // Return the successful assembly
    return Ok(program);
}
