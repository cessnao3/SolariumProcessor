use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef},
    tokenizer::{Token, get_identifier},
    typing::Type,
};

pub trait Variable: Debug + Expression {
    fn get_name(&self) -> &str {
        self.get_token().get_value()
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    name: Token,
    dtype: Type,
    label: Rc<str>,
}

impl GlobalVariable {
    pub fn new(token: Token, id: usize, dtype: Type) -> Result<Self, TokenError> {
        let label = format!("global_variable_{}_{}", id, get_identifier(&token)?).into();
        Ok(Self {
            name: token,
            dtype,
            label,
        })
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

impl Display for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

impl Variable for GlobalVariable {
    fn get_name(&self) -> &str {
        self.name.get_value()
    }
}

impl Expression for GlobalVariable {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.dtype.clone())
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        if let Type::Primitive(p) = self.dtype {
            let vals = [
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg.reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLd::new(
                    ArgumentType::new(reg.reg, p.into()),
                    reg.reg.into(),
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

    fn load_address_to_register(&self, reg: RegisterDef) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .to_token_loc([
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg.reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
            ])
            .collect())
    }
}

#[derive(Debug, Clone)]
struct LocalVariable {}
