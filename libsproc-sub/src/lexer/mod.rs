use crate::{lexer::variable::Variable, tokenizer::{Token, Keyword, Symbol}};

mod common;
mod variable;
mod function;

mod expression;
mod statement;

mod statement_if;
mod statement_while;

use common::ScopeManager;

use std::rc::Rc;

use self::{common::NamedMemoryValue, variable::StaticVariable};

struct TokenIter
{
    list: Vec<Token>,
    current: usize,
    started: bool
}

impl TokenIter
{
    pub fn new(tokens: Vec<Token>) -> TokenIter
    {
        return Self
        {
            list: tokens,
            current: 0,
            started: false
        };
    }

    pub fn next(&mut self) -> Option<&Token>
    {
        if !self.started
        {
            self.started = true;
        }
        else if self.current < self.list.len()
        {
            self.current += 1;
        }

        return self.get_index_val(self.current);
    }

    pub fn last(&self) -> Option<&Token>
    {
        if !self.started
        {
            return None;
        }
        else
        {
            return self.get_index_val(self.current);
        }
    }

    pub fn peek(&self) -> Option<&Token>
    {
        if !self.started
        {
            return self.get_index_val(self.current);
        }
        else
        {
            return self.get_index_val(self.current + 1);
        }
    }

    fn get_index_val(&self, ind: usize) -> Option<&Token>
    {
        if ind < self.list.len()
        {
            return Some(&self.list[ind]);
        }
        else
        {
            return None;
        }
    }
}

enum VariableType
{
    Stack,
    Static
}

fn read_expression(iter: &mut TokenIter, scopes: &mut ScopeManager, register: usize, register_spare: usize) -> Result<Vec<String>, String>
{
    let mut assembly = Vec::new();

    if let Some(t) = iter.next()
    {
        match t
        {
            Token::Name(name) =>
            {
                if let Some(Token::Symbol(Symbol::OpenParen)) = iter.peek()
                {
                    panic!("function calls do not yet work...");
                }
                else
                {
                    match scopes.get_variable(name)
                    {
                        Ok(var) => assembly.extend(var.load_value_to_register(register, register_spare)),
                        Err(e) => return Err(e)
                    };
                }
            },
            Token::Symbol(Symbol::OpenParen) =>
            {
                match read_expression(iter, scopes, register, register_spare)
                {
                    Ok(v) => assembly.extend(v),
                    Err(e) => return Err(e)
                };

                let post_load_instruction;

                match iter.peek()
                {
                    Some(v) => match v
                    {
                        Token::Symbol(Symbol::BitwiseAnd) =>
                        {
                            post_load_instruction = vec![
                                format!("band {0:}, {0:}, {1:}", register, register_spare)
                            ];
                        }
                        Token::Symbol(Symbol::BitwiseOr) =>
                        {
                            post_load_instruction = vec![
                                format!("bor {0:}, {0:}, {1:}", register, register_spare)
                            ];
                        }
                        Token::Symbol(Symbol::BooleanAnd) =>
                        {
                            post_load_instruction = vec![
                                format!("band {0:}, {0:}, {1:}", register, register_spare),
                                format!("bool {0:}", register)
                            ];
                        }
                        Token::Symbol(Symbol::BooleanOr) =>
                        {
                            post_load_instruction = vec![
                                format!("bor {0:}, {0:}, {1:}", register, register_spare),
                                format!("bool {0:}", register)
                            ];
                        }
                        Token::Symbol(Symbol::Equal) =>
                        {
                            post_load_instruction = vec![
                                format!("teq {0:}, {1:}"),

                            ];
                        }
                        Token::Symbol(Symbol::NotEqual) =>
                        {

                        }
                        Token::Symbol(Symbol::Greater) =>
                        {

                        }
                        Token::Symbol(Symbol::GreaterEqual) =>
                        {

                        }
                        Token::Symbol(Symbol::Less) =>
                        {

                        }
                        Token::Symbol(Symbol::LessEqual) =>
                        {

                        }
                        Token::Symbol(Symbol::Plus) =>
                        {

                        }
                        Token::Symbol(Symbol::Minus) =>
                        {

                        }
                        Token::Symbol(Symbol::Star) =>
                        {

                        }
                        Token::Symbol(Symbol::Divide) =>
                        {

                        }
                        Token::Symbol(Symbol::Modulus) =>
                        {

                        }
                        _ => ()
                    }
                    _ => ()
                };

                if let Some(Token::Symbol(Symbol::CloseParen)) = iter.peek()
                {
                    // Add the

                    // Clear out the close paren
                    iter.next();
                }
                else
                {
                    return Err(match iter.peek()
                    {
                        Some(t) => format!("expected closing parenthesis, found {0:}", t.to_string()),
                        None => format!("unexpected end of token stream")
                    });
                }
            }
            Token::Symbol(symb) =>
            {
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
                            format!("ldi {0:}, -1", register_spare_a),
                            format!("mul {0:}, {0:}, {1:}", register, register_spare_a)
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
                    Symbol::BitwiseAnd =>
                    {
                        panic!("address-of not yet implemented!");
                    }
                    _ =>
                    {
                        return Err(format!("unexpected use of symbol {0:} in expression", symb.to_string()));
                    }
                };

                // Provide the resulting read instruction
                match read_expression(iter, scopes, register, register_spare_a, register_spare_b)
                {
                    Ok(vals) =>
                    {
                        assembly.extend(vals);
                        assembly.extend(post_load_vec);
                    },
                    Err(e) => return Err(e)
                };
            },
            _ => return Err(format!("unexpexcted token {0:} found in expression", t.to_string()))
        };
    }
    else
    {
        return Err(format!("unexpected end of token stream"));
    }
    return Err(format!("not implemented"));
}

fn read_statement(inter: &mut TokenIter, scopes: &mut ScopeManager) -> Result<Vec<String>, String>
{
    return Err(format!("not implemented"));
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
        match read_expression(iter, scopes, 6, 7, 8)
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
