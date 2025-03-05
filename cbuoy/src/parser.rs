use jib_asm::{AsmToken, AsmTokenLoc, LocationInfo};

use crate::{
    tokenizer::{Token, TokenError, TokenIter, TokenLocation, get_identifier, tokenize},
    typing::{PrimitiveType, Type},
};
use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

pub fn parse(s: &str) -> Result<Vec<AsmTokenLoc>, TokenError> {
    let mut state = CompilingState::default();
    let tokens = tokenize(s);
    let mut token_iter = TokenIter::from(&tokens);

    while let Some(next) = token_iter.peek().map(|v| v.get_value().to_string()) {
        if next == "global" {
            parse_global_statement(&mut token_iter, &mut state)?;
        } else {
            return Err(token_iter
                .next()?
                .clone()
                .into_err("unknown token provided".to_owned()));
        }
    }

    fn blank_token_loc(tok: AsmToken) -> AsmTokenLoc {
        AsmTokenLoc {
            tok,
            loc: LocationInfo::default(),
        }
    }

    let init_asm = vec![
        AsmToken::ChangeAddress(0x2000),
        AsmToken::Comment("Initialization".into()),
    ];

    let mut asm: Vec<_> = init_asm
        .into_iter()
        .map(|tok| blank_token_loc(tok))
        .collect();

    asm.extend_from_slice(&state.asm_init);
    asm.push(blank_token_loc(AsmToken::Comment("Static".into())));
    asm.extend_from_slice(&state.asm_static);
    asm.push(blank_token_loc(AsmToken::Comment("Functions".into())));
    asm.extend_from_slice(&state.asm_func);

    Ok(asm)
}

fn parse_global_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<(), TokenError> {
    tokens.expect("global")?;
    let name_token = tokens.next()?;

    tokens.expect(":")?;

    let mut type_tokens = Vec::new();
    while let Some(t) = tokens.next_if(|s| s != ";") {
        type_tokens.push(t.to_owned());
    }

    tokens.expect(";")?;

    let dtype = match PrimitiveType::try_from(type_tokens.first().unwrap().to_owned()) {
        Ok(t) => Type::Primitive(t),
        Err(e) => {
            return Err(TokenError {
                token: Some(type_tokens.first().unwrap().to_owned()),
                msg: e.to_string(),
            });
        }
    };

    state.add_global_var(name_token, dtype)
}

#[derive(Debug, Clone)]
struct GlobalVariable {
    name: Token,
    dtype: Type,
    id: usize,
}

impl GlobalVariable {
    pub fn access_label(&self) -> String {
        format!("global_variable_{}_{}", self.id, self.get_name())
    }

    pub fn get_name(&self) -> &str {
        self.name.get_value()
    }
}

#[derive(Debug, Default)]
struct CompilingState {
    asm_static: Vec<jib_asm::AsmTokenLoc>,
    asm_init: Vec<jib_asm::AsmTokenLoc>,
    asm_func: Vec<jib_asm::AsmTokenLoc>,
    global_variables: HashMap<Rc<str>, Rc<GlobalVariable>>,
    current_id: usize,
}

impl CompilingState {
    pub fn get_next_id(&mut self) -> usize {
        let current = self.current_id;
        self.current_id += 1;
        current
    }

    pub fn add_global_var(&mut self, name: Token, dtype: Type) -> Result<(), TokenError> {
        let id = self.get_next_id();
        let name_str: Rc<str> = get_identifier(&name)?.into();

        let var = match self.global_variables.entry(name_str.clone()) {
            Entry::Occupied(_) => {
                return Err(name.into_err(format!(
                    "global variable already exists with name \"{name_str}\""
                )));
            }
            Entry::Vacant(e) => e.insert(Rc::new(GlobalVariable {
                name: name.clone(),
                dtype,
                id,
            })),
        };

        self.asm_static
            .push(name.to_asm(AsmToken::CreateLabel(var.access_label())));

        let mut needed_size = var.dtype.byte_size();
        while needed_size > 0 {
            let tok = if needed_size >= 4 {
                needed_size -= 4;
                AsmToken::Literal4(0)
            } else if needed_size >= 2 {
                needed_size -= 2;
                AsmToken::Literal2(0)
            } else {
                needed_size -= 1;
                AsmToken::Literal1(0)
            };

            self.asm_static.push(name.to_asm(tok));
        }

        // TODO - Check if the expression can be converted to the raw byte values and loaded directly
        // TODO - Add support for initializing expressions

        Ok(())
    }
}

struct VariableStatement {
    name: String,
    data_type: Type,
    loc: TokenLocation,
}

#[cfg(test)]
mod tests {}
