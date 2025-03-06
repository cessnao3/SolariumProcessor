use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use jib_asm::{AsmToken, AsmTokenLoc, LocationInfo};

use crate::{
    TokenError,
    expressions::Expression,
    tokenizer::{Token, get_identifier},
    typing::Type,
    variables::{GlobalVariable, Variable},
};

#[derive(Debug)]
pub struct CompilingState {
    init_loc: u32,
    asm_static: Vec<jib_asm::AsmTokenLoc>,
    asm_init: Vec<jib_asm::AsmTokenLoc>,
    asm_func: Vec<jib_asm::AsmTokenLoc>,
    global_variables: HashMap<Rc<str>, Rc<dyn Variable>>,
    current_id: usize,
}

impl Default for CompilingState {
    fn default() -> Self {
        Self {
            init_loc: 0x2000,
            asm_init: vec![Self::blank_token_loc(AsmToken::Comment(
                "Initialization".into(),
            ))],
            asm_func: vec![Self::blank_token_loc(AsmToken::Comment("Functions".into()))],
            asm_static: vec![Self::blank_token_loc(AsmToken::Comment("Static".into()))],
            global_variables: HashMap::new(),
            current_id: 0,
        }
    }
}

impl CompilingState {
    fn blank_token_loc(tok: AsmToken) -> AsmTokenLoc {
        AsmTokenLoc {
            tok,
            loc: LocationInfo::default(),
        }
    }

    pub fn get_next_id(&mut self) -> usize {
        let current = self.current_id;
        self.current_id += 1;
        current
    }

    pub fn get_assembler(&self) -> Vec<AsmTokenLoc> {
        let mut asm = vec![Self::blank_token_loc(AsmToken::ChangeAddress(
            self.init_loc,
        ))];
        asm.extend_from_slice(&self.asm_init);
        asm.extend_from_slice(&self.asm_static);
        asm.extend_from_slice(&self.asm_func);

        asm
    }

    pub fn add_global_var(
        &mut self,
        name: Token,
        dtype: Type,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<(), TokenError> {
        let id = self.get_next_id();
        let name_str: Rc<str> = get_identifier(&name)?.into();

        let var = Rc::new(GlobalVariable::new(name.clone(), id, dtype));

        self.asm_static
            .push(name.to_asm(AsmToken::CreateLabel(var.access_label().into())));

        let init_literal = if let Some(expr) = &init_expr {
            if let Some(simplified) = expr.simplify() {
                Some(simplified.get_value().as_asm_literal())
            } else {
                None
            }
        } else {
            None
        };

        let literal_value = init_expr
            .clone()
            .map_or(None, |x| x.simplify())
            .map(|x| x.get_value().as_u32());

        let literal_value = literal_value.unwrap_or(0);

        if let Some(a) = init_literal.clone() {
            self.asm_static.push(name.to_asm(a));
        } else {
            let mut needed_size = var.get_type().byte_size();
            while needed_size > 0 {
                let tok = if needed_size >= 4 {
                    needed_size -= 4;
                    AsmToken::Literal4(literal_value)
                } else if needed_size >= 2 {
                    needed_size -= 2;
                    AsmToken::Literal2(literal_value as u16)
                } else {
                    needed_size -= 1;
                    AsmToken::Literal1(literal_value as u8)
                };

                self.asm_static.push(name.to_asm(tok));
            }
        }

        if init_expr.is_some() && init_literal.is_none() {
            todo!();
        }

        match self.global_variables.entry(name_str.clone()) {
            Entry::Occupied(_) => {
                return Err(name.into_err(format!(
                    "global variable already exists with name \"{name_str}\""
                )));
            }
            Entry::Vacant(e) => e.insert(var),
        };

        // TODO - Check if the expression can be converted to the raw byte values and loaded directly
        // TODO - Add support for initializing expressions

        Ok(())
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Variable>, TokenError> {
        todo!()
    }
}
