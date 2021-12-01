use super::common::*;

use crate::tokenizer::{Token, Symbol};
use super::token_iter::TokenIter;

pub fn read_base_expression(iter: &mut TokenIter, scopes: &mut ScopeManager, register: usize, register_spare: usize) -> Result<Vec<String>, String>
{
    if let Some(init_token) = iter.peek()
    {
        // Provide the assembly values
        let mut assembly = Vec::new();

        // Check for an initial variable name (for assignment, etc)
        if let Token::VariableName(name) = init_token
        {
            // Read the first token and the variable name
            iter.next();

            let var = match scopes.get_variable(&name)
            {
                Ok(v) => v,
                Err(e) => return Err(e)
            };

            if let Some(Token::Symbol(Symbol::Assignment)) = iter.peek()
            {
                // Clear the assignment operator
                iter.next();

                // Get the results of the following expression
                match read_base_expression(iter, scopes, REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_TEST_JUMP_B)
                {
                    Ok(v) => assembly.extend(v),
                    Err(e) => return Err(e)
                };

                // Assign the variable result
                assembly.extend(var.set_value_from_register(REG_DEFAULT_TEST_JUMP_A, REG_DEFAULT_TEST_JUMP_B));

                // Return the current assembly to prevent additional binary expressions from causing problems
                return Ok(assembly);
            }
            else
            {
                assembly.extend(var.load_value_to_register(register, register_spare));
            }
        }
        else
        {
            match read_expression(iter, scopes, register, register_spare)
            {
                Ok(v) => assembly.extend(v),
                Err(e) => return Err(e)
            };
        }

        // TODO - Check for binary expression here?
        let mut post_load_instruction = Vec::new();

        match iter.peek()
        {
            Some(Token::Symbol(symb)) => match symb
            {
                Symbol::AddressAssignment =>
                {
                    post_load_instruction = vec![
                        format!("sav {0:}, {1:}", register, register_spare),
                        format!("copy {0:}, {1:}", register, register_spare)
                    ];
                },
                Symbol::Plus |
                Symbol::Minus |
                Symbol::Star |
                Symbol::Divide |
                Symbol::BitwiseAnd |
                Symbol::BitwiseOr |
                Symbol::BooleanAnd |
                Symbol::BooleanOr =>
                {
                    let arith_inst = match symb
                    {
                        Symbol::Plus => "add",
                        Symbol::Minus => "sub",
                        Symbol::Star => "mul",
                        Symbol::Divide => "div",
                        Symbol::BitwiseAnd => "band",
                        Symbol::BitwiseOr => "bor",
                        Symbol::BooleanAnd => "band",
                        Symbol::BooleanOr => "bor",
                        _ => panic!()
                    };

                    post_load_instruction.push(format!("{0:} {1:}, {1:}, {2:}", arith_inst, register, register_spare));

                    match symb
                    {
                        Symbol::BooleanAnd |
                        Symbol::BooleanOr =>
                        {
                            post_load_instruction.push(format!("bool {0:}", register))
                        }
                        _ => ()
                    }
                },
                Symbol::Greater |
                Symbol::Less |
                Symbol::GreaterEqual |
                Symbol::LessEqual |
                Symbol::Equal |
                Symbol::NotEqual =>
                {
                    let test_inst = match symb
                    {
                        Symbol::Greater => "tg",
                        Symbol::GreaterEqual => "tge",
                        Symbol::Less => "tl",
                        Symbol::LessEqual => "tle",
                        Symbol::Equal |
                        Symbol::NotEqual => "teq",
                        _ => panic!()
                    };

                    post_load_instruction.push(format!("ldi {0:}, 0", register));
                    post_load_instruction.push(format!("{0:} {1:}, {2:}", test_inst, register, register_spare));
                    post_load_instruction.push(format!("ldi {0:}, 1", register));

                    let post_inst_boolean = match symb
                    {
                        Symbol::NotEqual => "bnot",
                        _ => "bool"
                    };

                    post_load_instruction.push(format!("{0:} {1:}", post_inst_boolean, register));
                },
                _ => ()
            },
            _ => ()
        };

        if post_load_instruction.len() > 0
        {
            // Consume the next value
            iter.next();

            // Add the current value to the stack
            assembly.push(format!("push {0:}", register));

            // Read the right-hand of the expression
            match read_expression(iter, scopes, register, register_spare)
            {
                Ok(v) => assembly.extend(v),
                Err(e) => return Err(e)
            };

            // Move values into the correct locations
            assembly.push(format!("copy {0:}, {1:}", register_spare, register));
            assembly.push(format!("popr {0:}", register));

            // Add the resulting instruction values
            assembly.extend(post_load_instruction);
        }

        // Return the assembly result
        return Ok(assembly);
    }
    else
    {
        return Err(format!("unexpected end of token stream"));
    }
}

fn read_expression(iter: &mut TokenIter, scopes: &mut ScopeManager, register: usize, register_spare: usize) -> Result<Vec<String>, String>
{
    let mut assembly = Vec::new();

    if let Some(init_token) = iter.next()
    {
        match init_token
        {
            Token::WordLiteral(val) =>
            {
                assembly.extend(vec![
                    "jmpri 2".to_string(),
                    format!(".load {0:}", val),
                    format!("ldri {0:}, -1", register)
                ]);
            },
            Token::VariableName(name) =>
            {
                match scopes.get_variable(&name)
                {
                    Ok(var) => assembly.extend(var.load_value_to_register(register, register_spare)),
                    Err(e) => return Err(e)
                };
            },
            Token::FunctionName(name) =>
            {
                // Check for the open paren
                match iter.next()
                {
                    Some(Token::Symbol(Symbol::OpenParen)) => (),
                    Some(tok) => return Err(format!("expected (, but found {0:}, after function call", tok.to_string())),
                    _ => return Err("unexpected end of stream found".to_string())
                }

                // Check for the output function
                let func = match scopes.get_function(&name)
                {
                    Ok(f) => f,
                    Err(e) => return Err(e)
                };

                // Loop through to read each of the input arguments
                let mut num_args = 0usize;

                loop
                {
                    // Break on close paren
                    if let Some(Token::Symbol(Symbol::CloseParen)) = iter.peek()
                    {
                        iter.next();
                        break;
                    }

                    // Check for comma if needed
                    if num_args > 0
                    {
                        // Read the comma
                        match iter.next()
                        {
                            Some(Token::Symbol(Symbol::Comma)) => (),
                            Some(tok) => return Err(format!("comma must follow expression in function {0:} call, found {1:}", name, tok.to_string())),
                            None => return Err("unexpected end of stream found".to_string())
                        };
                    }

                    // Read the expression
                    match read_base_expression(iter, scopes, register, register_spare)
                    {
                        Ok(v) => assembly.extend(v),
                        Err(e) => return Err(e)
                    };

                    // Push the result to the stack
                    assembly.push(format!("push {0:}", register));

                    // Increment the argument count
                    num_args += 1;
                }

                // Check the argument count
                if num_args != func.num_args()
                {
                    return Err(format!("function {0:} expected {1:} arguments, found {2:}", name, func.num_args(), num_args));
                }

                // Load the function call value
                assembly.extend(func.load_function_address(register));
                assembly.push(format!("call {0:}", register));

                // Pop the resulting stack values
                for _ in 0..func.num_args()
                {
                    assembly.push("pop".to_string());
                }

                // Copy the return address into the expected register
                assembly.push(format!("copy {0:}, $ret", register));
            },
            Token::Symbol(Symbol::OpenParen) =>
            {
                match read_base_expression(iter, scopes, register, register_spare)
                {
                    Ok(v) => assembly.extend(v),
                    Err(e) => return Err(e)
                };

                let next_token = iter.next();

                if let Some(Token::Symbol(Symbol::CloseParen)) = next_token
                {
                    // Do nothing
                }
                else if let Some(tok) = next_token
                {
                    return Err(format!("expected closing paren - found {0:}", tok.to_string()));
                }
                else
                {
                    return Err("unexpected end of stream".to_string());
                }
            },
            Token::Symbol(Symbol::BitwiseAnd) =>
            {
                if let Some(Token::VariableName(varname)) = iter.next()
                {
                    match scopes.get_variable(&varname)
                    {
                        Ok(var) => assembly.extend(var.load_address_to_register(register)),
                        Err(e) => return Err(e)
                    };
                }
                else
                {
                    return Err(format!("the next symbol for the address-of must be a variable name"));
                }
            }
            Token::Symbol(Symbol::Plus) |
            Token::Symbol(Symbol::Minus) |
            Token::Symbol(Symbol::Star) |
            Token::Symbol(Symbol::BooleanNot) |
            Token::Symbol(Symbol::BitwiseNot) =>
            {
                let symb;
                if let Token::Symbol(s) = init_token
                {
                    symb = s;
                }
                else
                {
                    panic!();
                }

                // Determine instructions that must be run on the resulting data values
                let post_load_vec = match symb
                {
                    Symbol::Plus =>
                    {
                        Vec::new()
                    },
                    Symbol::Minus =>
                    {
                        vec![
                            format!("ldi {0:}, -1", register_spare),
                            format!("mul {0:}, {0:}, {1:}", register, register_spare)
                        ]
                    },
                    Symbol::Star =>
                    {
                        vec![
                            format!("ld {0:}, {0:}", register)
                        ]
                    },
                    Symbol::BooleanNot =>
                    {
                        vec![
                            format!("not {0:}", register)
                        ]
                    },
                    Symbol::BitwiseNot =>
                    {
                        vec![
                            format!("bnot {0:}, {0:}", register)
                        ]
                    },
                    _ =>
                    {
                        return Err(format!("unexpected use of symbol {0:} in expression", symb.to_string()));
                    }
                };

                // Provide the resulting read instruction
                match read_expression(iter, scopes, register, register_spare)
                {
                    Ok(vals) =>
                    {
                        assembly.extend(vals);
                        assembly.extend(post_load_vec);
                    },
                    Err(e) => return Err(e)
                };
            }
            _ => return Err(format!("unexpected token {0:} found in expression", init_token.to_string()))
        };
    }
    else
    {
        return Err(format!("unexpected end of token stream"));
    }

    return Ok(assembly);
}
