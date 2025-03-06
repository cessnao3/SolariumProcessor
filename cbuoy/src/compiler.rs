use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc, LocationInfo, OpConv, OpSav};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef},
    tokenizer::{Token, get_identifier},
    typing::Type,
    variables::GlobalVariable,
};

#[derive(Debug)]
pub struct CompilingState {
    init_loc: u32,
    asm_static: Vec<jib_asm::AsmTokenLoc>,
    asm_init: Vec<jib_asm::AsmTokenLoc>,
    asm_func: Vec<jib_asm::AsmTokenLoc>,
    global_variables: HashMap<String, Rc<dyn Expression>>,
    current_id: usize,
}

impl Default for CompilingState {
    fn default() -> Self {
        fn init_vec(name: &str) -> Vec<AsmTokenLoc> {
            vec![
                CompilingState::blank_token_loc(AsmToken::Comment(name.into())),
                CompilingState::blank_token_loc(AsmToken::AlignInstruction),
            ]
        }

        Self {
            init_loc: 0x2000,
            asm_init: init_vec("Initialization"),
            asm_func: init_vec("Functions"),
            asm_static: init_vec("Static"),
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

        let var = Rc::new(GlobalVariable::new(name.clone(), id, dtype)?);

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
            let mut needed_size = var.get_type()?.byte_size();
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

        if init_literal.is_none() {
            if let Some(init_expr) = init_expr {
                if let Some(init_p) = init_expr.get_type()?.primitive_type() {
                    if let Some(p) = var.get_type()?.primitive_type() {
                        // TODO - Replace with a better expression (operation expression value?)
                        let reg_state_var = RegisterDef::default();
                        let reg_state_init = match reg_state_var.increment_register() {
                            Some(x) => x,
                            None => {
                                return Err(name
                                    .clone()
                                    .into_err("unable to get valid register definition"));
                            }
                        };

                        self.asm_init
                            .extend_from_slice(&var.load_address_to_register(reg_state_var)?);
                        self.asm_init
                            .extend_from_slice(&init_expr.load_value_to_register(reg_state_init)?);

                        let reg_init = reg_state_init.reg;
                        let reg_var = reg_state_var.reg;

                        if p != init_p {
                            self.asm_init
                                .push(name.to_asm(AsmToken::OperationLiteral(Box::new(
                                    OpConv::new(
                                        ArgumentType::new(reg_init.into(), p),
                                        ArgumentType::new(reg_init.into(), init_p),
                                    ),
                                ))));
                        }

                        self.asm_init
                            .push(name.to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                                ArgumentType::new(reg_var, p),
                                reg_init.into(),
                            )))));
                    } else {
                        return Err(name
                            .clone()
                            .into_err("type does not have a valid primitive type"));
                    }
                } else {
                    return Err(name
                        .clone()
                        .into_err("init expression does not result in a valid primitive type"));
                }
            }
        }

        match self.global_variables.entry(name_str.to_string()) {
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

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        self.global_variables.get(&get_identifier(name)?).map_or(
            Err(name
                .clone()
                .into_err("no variable with matching name found")),
            |x| Ok(x.clone()),
        )
    }
}
