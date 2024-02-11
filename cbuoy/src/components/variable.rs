use jasm::{
    argument::ArgumentType,
    instructions::{OpAdd, OpLd, OpLdn},
    AssemblerToken, FromLiteral,
};
use jib::{cpu::{DataType, Register}, text::CharacterError};

use crate::types::{SpType, SpTypeError};

use super::{
    addressable::Addressable,
    expression::{Expression, ExpressionError, ExpressionLValue, Literal},
};

pub enum VariableError {
    Type(SpTypeError),
    Character(CharacterError),
    InvalidInitializerType(SpType),
    MismatchingType(DataType, DataType),
}

impl From<SpTypeError> for VariableError {
    fn from(value: SpTypeError) -> Self {
        Self::Type(value)
    }
}

impl From<CharacterError> for VariableError {
    fn from(value: CharacterError) -> Self {
        Self::Character(value)
    }
}

pub trait Variable: ExpressionLValue {
    fn init(&self) -> Result<Vec<AssemblerToken>, VariableError>;

    fn byte_size(&self) -> Result<usize, VariableError> {
        Ok(self.get_type().byte_count()?)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableInitializer {
    Literal(Literal),
    Array(SpType, Vec<Literal>),
    Text(String),
}

impl VariableInitializer {
    pub fn get_type(&self) -> SpType {
        match self {
            Self::Literal(l) => l.get_type(),
            Self::Array(t, vals) => SpType::Array { base: Box::new(t.clone()), size: vals.len() },
            Self::Text(s) => SpType::Array { base: Box::new(DataType::U8.into()), size: s.len() + 1 },
        }
    }

    pub fn get_tokens(&self) -> Result<Vec<AssemblerToken>, VariableError> {
        let vals = match self {
            Self::Literal(l) => l.to_tokens(),
            Self::Text(s) => {
                s.chars().chain(['\0'])
                    .map(jib::text::character_to_byte)
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flat_map(|c| Literal::U8(c).to_tokens())
                    .collect::<Vec<_>>()
            }
            Self::Array(t, vals) => {
                let base = t.base_primitive()?;
                let mut v = Vec::new();

                for l in vals.iter() {
                    let l_type = l.base_type();
                    if l_type == base {
                        v.extend(l.to_tokens())
                    } else {
                        return Err(VariableError::MismatchingType(base, l_type))
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
    var_type: SpType,
    base_offset: i32,
    init: VariableInitializer,
}

impl Expression for LocalVariable {
    fn get_type(&self) -> SpType {
        self.var_type.clone()
    }

    fn load_to(&self, reg: Register, _spare: Register) -> Result<Vec<AssemblerToken>, ExpressionError> {
        let base_type = self.get_type().base_primitive()?;

        let mut res = self.load_address(reg);
        res.push(AssemblerToken::OperationLiteral(Box::new(OpLd::new(
            ArgumentType::new(reg, base_type),
            reg.into(),
        ))));

        Ok(res)
    }
}

impl Addressable for LocalVariable {
    fn load_address(&self, reg: Register) -> Vec<AssemblerToken> {
        vec![
            AssemblerToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg,
                jib::cpu::DataType::U32,
            )))),
            AssemblerToken::from_literal(self.base_offset),
            AssemblerToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(reg, jib::cpu::DataType::U32),
                reg.into(),
                Register::ArgumentBase.into(),
            ))),
        ]
    }
}

impl ExpressionLValue for LocalVariable {}

impl Variable for LocalVariable {
    fn init(&self) -> Result<Vec<AssemblerToken>, VariableError> {
        panic!()
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    var_type: SpType,
    var_label: String,
}

impl Expression for GlobalVariable {
    fn get_type(&self) -> SpType {
        self.var_type.clone()
    }

    fn load_to(&self, reg: Register, _spare: Register) -> Result<Vec<AssemblerToken>, ExpressionError> {
        let res = self
            .load_address(reg)
            .into_iter()
            .chain([AssemblerToken::OperationLiteral(Box::new(OpLd::new(
                ArgumentType::new(reg, jib::cpu::DataType::U32),
                reg.into(),
            )))])
            .collect();
        Ok(res)
    }
}

impl Addressable for GlobalVariable {
    fn load_address(&self, reg: Register) -> Vec<AssemblerToken> {
        vec![
            AssemblerToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg,
                jib::cpu::DataType::U32,
            )))),
            AssemblerToken::LoadLoc(self.var_label.clone()),
        ]
    }
}

impl ExpressionLValue for GlobalVariable {}

impl Variable for GlobalVariable {
    fn init(&self) -> Result<Vec<AssemblerToken>, VariableError> {
        panic!()
    }
}
