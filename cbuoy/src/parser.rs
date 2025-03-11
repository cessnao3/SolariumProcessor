use std::rc::Rc;

use jib::cpu::DataType;
use jib_asm::AsmTokenLoc;

use crate::{
    compiler::{CompilingState, ScopeManager},
    expressions::{Expression, parse_expression},
    functions::parse_fn_statement,
    tokenizer::{Token, TokenError, TokenIter, tokenize},
    typing::Type,
};

pub fn parse(s: &str) -> Result<Vec<AsmTokenLoc>, TokenError> {
    let mut state = CompilingState::default();
    let tokens = tokenize(s)?;
    let mut token_iter = TokenIter::from(&tokens);

    while let Some(next) = token_iter.peek().map(|v| v.get_value().to_string()) {
        if next == "global" {
            parse_global_statement(&mut token_iter, &mut state)?;
        } else if next == "fn" {
            parse_fn_statement(&mut token_iter, &mut state)?;
        } else {
            return Err(token_iter
                .next()?
                .clone()
                .into_err("unknown token provided"));
        }
    }

    state.get_assembler()
}

struct VariableDefinition {
    token: Token,
    dtype: Type,
    init_expr: Option<Rc<dyn Expression>>,
}

pub fn parse_generic_var(
    tokens: &mut TokenIter,
    state: &CompilingState,
) -> Result<VariableDefinition, TokenError> {
    let name_token = tokens.next()?;

    tokens.expect(":")?;

    let mut type_tokens = Vec::new();
    while let Some(t) = tokens.next_if(|s| s != ";" && s != "=") {
        type_tokens.push(t.to_owned());
    }

    let init_expr = if tokens.expect_peek("=") {
        tokens.next()?;
        let mut expr_tokens = Vec::new();
        while let Some(t) = tokens.next_if(|s| s != ";") {
            expr_tokens.push(t);
        }
        Some(parse_expression(&mut TokenIter::from(&expr_tokens), state)?)
    } else {
        None
    };

    tokens.expect(";")?;

    let dtype = match DataType::try_from(type_tokens.first().unwrap().to_owned()) {
        Ok(t) => Type::Primitive(t),
        Err(e) => {
            return Err(TokenError {
                token: Some(type_tokens.first().unwrap().to_owned()),
                msg: e.to_string(),
            });
        }
    };

    Ok(VariableDefinition {
        token: name_token,
        dtype,
        init_expr,
    })
}

fn parse_global_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<(), TokenError> {
    tokens.expect("global")?;
    let def = parse_generic_var(tokens, state)?;
    state.add_global_var(def.token, def.dtype, def.init_expr)
}

fn parse_def_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
    scope_manager: &mut ScopeManager,
) -> Result<(), TokenError> {
    tokens.expect("def")?;
    let def = parse_generic_var(tokens, state)?;
    scope_manager.add_var(def.token, def.dtype, def.init_expr)
}

#[cfg(test)]
mod tests {
    use crate::{
        literals::{Literal, LiteralValue},
        tokenizer::{Token, TokenLocation},
    };

    #[test]
    fn test_literal_parsing() {
        let tokens = [
            ("0u32", Some(LiteralValue::U32(0))),
            ("23834i8", None),
            ("1i8", Some(LiteralValue::I8(1))),
            ("23834i16", Some(LiteralValue::I16(23834))),
            ("394i23", None),
            ("38905u32", Some(LiteralValue::U32(38905))),
            ("389u35", None),
            ("3.234i8", None),
            ("3.2f32", Some(LiteralValue::F32(3.2))),
            (".3f32", Some(LiteralValue::F32(0.3))),
            ("32.34f32", Some(LiteralValue::F32(32.34))),
            ("32.34", Some(LiteralValue::F32(32.34))),
            ("394", Some(LiteralValue::I32(394))),
        ];

        for (s, expected) in tokens {
            let t = Token::new(s, TokenLocation { line: 0, column: 0 });
            let lit: Result<Literal, _> = t.try_into();

            println!("{:?} <=> {:?}", lit, expected);
            assert_eq!(lit.is_ok(), expected.is_some());
            if let Ok(v) = lit {
                if let Some(ev) = expected {
                    assert_eq!(v.get_value().get_dtype(), ev.get_dtype());
                    assert_eq!(v.get_value().as_u32(), ev.as_u32());
                } else {
                    panic!("unexpected value mismatch");
                }
            }
        }
    }
}
