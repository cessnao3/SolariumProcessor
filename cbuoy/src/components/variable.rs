use jasm::{
    argument::ArgumentType,
    instructions::{OpAdd, OpLd, OpLdn},
    Token,
};
use jib::cpu::{DataType, Register};

use crate::types::{SpType, SpTypeError};

use super::{
    addressable::Addressable,
    expression::{Expression, ExpressionError, Literal},
};

pub enum VariableError {
    Type(SpTypeError),
}

impl From<SpTypeError> for VariableError {
    fn from(value: SpTypeError) -> Self {
        Self::Type(value)
    }
}

pub trait Variable: Addressable + Expression {
    fn init(&self) -> Result<Vec<Token>, VariableError>;

    fn byte_size(&self) -> Result<usize, VariableError> {
        Ok(self.get_type().word_count()?)
    }
}

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
}

pub struct LocalVariable {
    var_type: SpType,
    base_offset: i32,
    init: VariableInitializer,
}

impl Expression for LocalVariable {
    fn get_type(&self) -> SpType {
        self.var_type.clone()
    }

    fn load_to(&self, reg: Register, _spare: Register) -> Result<Vec<Token>, ExpressionError> {
        let base_type = if let Some(dt) = self.get_type().base_primitive() {
            dt
        } else {
            return Err(ExpressionError::ExpressionRequiresPrimitive(
                self.get_type(),
            ));
        };

        let mut res = self.get_address(reg);
        res.push(Token::OperationLiteral(Box::new(OpLd::new(
            ArgumentType::new(reg, base_type),
            reg.into(),
        ))));

        Ok(res)
    }
}

impl Addressable for LocalVariable {
    fn get_address(&self, reg: Register) -> Vec<Token> {
        vec![
            Token::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg,
                jib::cpu::DataType::U32,
            )))),
            Token::literal_i32(self.base_offset),
            Token::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(reg, jib::cpu::DataType::U32),
                reg.into(),
                Register::ArgumentBase.into(),
            ))),
        ]
    }
}

impl Variable for LocalVariable {
    fn init(&self) -> Result<Vec<Token>, VariableError> {


        panic!()
    }
}

pub struct GlobalVariable {
    var_type: SpType,
    var_label: String,
}

impl Expression for GlobalVariable {
    fn get_type(&self) -> SpType {
        self.var_type.clone()
    }

    fn load_to(&self, reg: Register, _spare: Register) -> Result<Vec<Token>, ExpressionError> {
        let res = self
            .get_address(reg)
            .into_iter()
            .chain([Token::OperationLiteral(Box::new(OpLd::new(
                ArgumentType::new(reg, jib::cpu::DataType::U32),
                reg.into(),
            )))])
            .collect();
        Ok(res)
    }
}

impl Addressable for GlobalVariable {
    fn get_address(&self, reg: Register) -> Vec<Token> {
        vec![
            Token::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg,
                jib::cpu::DataType::U32,
            )))),
            Token::LoadLoc(self.var_label.clone()),
        ]
    }
}

impl Variable for GlobalVariable {
    fn init(&self) -> Result<Vec<Token>, VariableError> {
        panic!()
    }
}
