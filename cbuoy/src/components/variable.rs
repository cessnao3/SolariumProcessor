use jib::{
    cpu::{DataType, Register},
    text::CharacterError,
};
use jib_asm::{
    argument::ArgumentType,
    instructions::{OpAdd, OpLd, OpLdn},
    AsmToken, FromLiteral,
};

use crate::{
    tokenizer::Token,
    types::{Type, TypeError},
};

use super::{
    expression::{Expression, Literal},
    AsmGenState, CodeLocation, ErrorToken,
};

pub enum VariableError {
    Type(TypeError),
    Character(CharacterError),
    InvalidInitializerType(Type),
    MismatchingType(DataType, DataType),
}

impl From<TypeError> for VariableError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}

impl From<CharacterError> for VariableError {
    fn from(value: CharacterError) -> Self {
        Self::Character(value)
    }
}

pub trait Variable: Expression {
    fn init(&self) -> Result<Vec<AsmToken>, VariableError>;

    fn byte_size(&self) -> Result<usize, VariableError> {
        Ok(self.get_type()?.byte_count()?)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableInitializer {
    Literal(Literal),
    Array(Type, Vec<Literal>),
    Text(String),
}

impl VariableInitializer {
    pub fn get_type(&self) -> Result<Type, TypeError> {
        match self {
            Self::Literal(l) => Ok(l.get_type()),
            Self::Array(t, vals) => Ok(Type::Array {
                base: Box::new(t.clone()),
                size: vals.len(),
            }),
            Self::Text(s) => Ok(Type::Array {
                base: Box::new(DataType::U8.into()),
                size: s.len() + 1,
            }),
        }
    }

    pub fn get_tokens(&self) -> Result<Vec<AsmToken>, VariableError> {
        let vals = match self {
            Self::Literal(l) => l.to_tokens(),
            Self::Text(s) => s
                .chars()
                .chain(['\0'])
                .map(jib::text::character_to_byte)
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flat_map(|c| Literal::U8(c).to_tokens())
                .collect::<Vec<_>>(),
            Self::Array(t, vals) => {
                let base = t.base_primitive()?;
                let mut v = Vec::new();

                for l in vals.iter() {
                    let l_type = l.base_type();
                    if l_type == base {
                        v.extend(l.to_tokens())
                    } else {
                        return Err(VariableError::MismatchingType(base, l_type));
                    }
                }

                v
            }
        };

        Ok(vals)
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    tok: Token,
    var_type: Type,
    base_offset: i32,
    init: Option<VariableInitializer>,
}

impl LocalVariable {
    pub fn new(tok: Token, t: Type, base_offset: i32) -> Self {
        Self {
            tok,
            var_type: t,
            base_offset,
            init: None,
        }
    }
}

impl Expression for LocalVariable {
    fn get_type(&self) -> Result<Type, TypeError> {
        Ok(self.var_type.clone())
    }

    fn load_to(
        &self,
        reg: Register,
        _spare: Register,
        _state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        let base_type = self.get_base_primitive()?;

        let mut res = self.load_address(reg)?;
        res.push(AsmToken::OperationLiteral(Box::new(OpLd::new(
            ArgumentType::new(reg, base_type),
            reg.into(),
        ))));

        Ok(res)
    }

    fn load_address(&self, reg: Register) -> Result<Vec<AsmToken>, ErrorToken> {
        Ok(vec![
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg,
                jib::cpu::DataType::U32,
            )))),
            AsmToken::from_literal(self.base_offset),
            AsmToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(reg, jib::cpu::DataType::U32),
                reg.into(),
                Register::ArgumentBase.into(),
            ))),
        ])
    }
}

impl CodeLocation for LocalVariable {
    fn get_token(&self) -> &Token {
        &self.tok
    }
}

impl Variable for LocalVariable {
    fn init(&self) -> Result<Vec<AsmToken>, VariableError> {
        panic!()
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    tok: Token,
    var_type: Type,
    var_label: String,
}

impl GlobalVariable {
    pub fn new(tok: Token, name: &str, t: Type) -> Self {
        Self {
            tok,
            var_type: t,
            var_label: format!("STATIC_VAR_LBL_{name}"),
        }
    }
}

impl Expression for GlobalVariable {
    fn get_type(&self) -> Result<Type, TypeError> {
        Ok(self.var_type.clone())
    }

    fn load_to(
        &self,
        reg: Register,
        _spare: Register,
        _state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        let res = self
            .load_address(reg)?
            .into_iter()
            .chain([AsmToken::OperationLiteral(Box::new(OpLd::new(
                ArgumentType::new(reg, jib::cpu::DataType::U32),
                reg.into(),
            )))])
            .collect();
        Ok(res)
    }

    fn load_address(&self, reg: Register) -> Result<Vec<AsmToken>, ErrorToken> {
        Ok(vec![
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg,
                jib::cpu::DataType::U32,
            )))),
            AsmToken::LoadLoc(self.var_label.clone()),
        ])
    }
}

impl CodeLocation for GlobalVariable {
    fn get_token(&self) -> &Token {
        &self.tok
    }
}

impl Variable for GlobalVariable {
    fn init(&self) -> Result<Vec<AsmToken>, VariableError> {
        panic!()
    }
}
