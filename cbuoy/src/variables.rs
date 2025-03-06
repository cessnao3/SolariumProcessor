use std::{fmt::Debug, rc::Rc};

use jib_asm::{AsmToken, AsmTokenLoc, argument::ArgumentType};

use crate::{TokenError, tokenizer::Token, typing::Type};

pub trait Variable: Debug {
    fn get_token(&self) -> &Token;
    fn load_to_register(
        &self,
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;

    fn load_address_to_register(
        &self,
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;

    fn get_name(&self) -> &str {
        self.get_token().get_value()
    }

    fn get_type(&self) -> &Type;
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    name: Token,
    dtype: Type,
    id: usize,
    label: Rc<str>,
}

impl GlobalVariable {
    pub fn new(token: Token, id: usize, dtype: Type) -> Self {
        let label = format!("global_variable_{}_{}", id, token).into();
        Self {
            name: token,
            dtype,
            id,
            label,
        }
    }

    pub fn access_label(&self) -> &str {
        self.label.as_ref()
    }

    pub fn get_name(&self) -> &str {
        self.name.get_value()
    }

    fn to_token_loc<T: IntoIterator<Item = AsmToken>>(
        &self,
        it: T,
    ) -> impl Iterator<Item = AsmTokenLoc> {
        it.into_iter().map(|x| self.name.to_asm(x))
    }
}

impl Variable for GlobalVariable {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn get_type(&self) -> &Type {
        &self.dtype
    }

    fn load_to_register(
        &self,
        reg: jib::cpu::Register,
        _spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        if let Type::Primitive(p) = self.dtype {
            let vals = [
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLd::new(
                    ArgumentType::new(reg, p.into()),
                    reg.into(),
                ))),
            ];
            Ok(self.to_token_loc(vals.into_iter()).collect())
        } else {
            Err(self.name.clone().into_err(format!(
                "unable to load value for {} to register",
                self.dtype
            )))
        }
    }

    fn load_address_to_register(
        &self,
        reg: jib::cpu::Register,
        _spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .to_token_loc([
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
            ])
            .collect())
    }
}

#[derive(Debug, Clone)]
struct LocalVariable {}
