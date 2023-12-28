use jasm::{
    argument::ArgumentType,
    instructions::{OpAdd, OpBnot, OpConv, OpLd, OpLdi, OpLdn, OpNeg, OpNoop, OpNot},
    Token,
};
use jib::cpu::{DataType, Register};

use crate::types::SpType;

#[derive(Debug, Clone)]
pub enum ExpressionError {
    ExpressionRequiresPrimitive(SpType),
}

pub trait Expression {
    fn get_type(&self) -> SpType;

    fn load_to(&self, reg: Register, spare: Register) -> Result<Vec<Token>, ExpressionError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    F32(f32),
}

impl Literal {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Self::U8(v) => vec![*v],
            Self::I8(v) => vec![*v as u8],
            Self::U16(v) => v.to_be_bytes().to_vec(),
            Self::I16(v) => v.to_be_bytes().to_vec(),
            Self::U32(v) => v.to_be_bytes().to_vec(),
            Self::I32(v) => v.to_be_bytes().to_vec(),
            Self::F32(v) => v.to_be_bytes().to_vec(),
        }
    }
}

impl From<u8> for Literal {
    fn from(value: u8) -> Self {
        Self::U8(value)
    }
}

impl Expression for Literal {
    fn get_type(&self) -> SpType {
        let init = match self {
            Self::U8(_) => SpType::Primitive { base: DataType::U8 },
            Self::I8(_) => SpType::Primitive { base: DataType::I8 },
            Self::U16(_) => SpType::Primitive {
                base: DataType::U16,
            },
            Self::I16(_) => SpType::Primitive {
                base: DataType::I16,
            },
            Self::U32(_) => SpType::Primitive {
                base: DataType::U32,
            },
            Self::I32(_) => SpType::Primitive {
                base: DataType::I32,
            },
            Self::F32(_) => SpType::Primitive {
                base: DataType::F32,
            },
        };

        SpType::Constant {
            base: Box::new(init),
        }
    }

    fn load_to(
        &self,
        reg: Register,
        _spare: Register,
    ) -> Result<Vec<Token>, ExpressionError> {
        let (lit_token, lit_type) = match self {
            Self::U8(val) => (
                Token::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::I8),
                    *val as u16,
                ))),
                None,
            ),
            Self::I8(val) => (
                Token::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    (*val as i16) as u16,
                ))),
                None,
            ),
            Self::I16(val) => (
                Token::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    *val as u16,
                ))),
                None,
            ),
            Self::U16(val) => (
                Token::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    (*val as i16) as u16,
                ))),
                None,
            ),
            Self::U32(val) => (Token::Literal4(*val), Some(DataType::U32)),
            Self::I32(val) => (Token::Literal4(*val as u32), Some(DataType::I32)),
            Self::F32(val) => (Token::Literal4(val.to_bits()), Some(DataType::F32)),
        };

        if let Some(dt) = lit_type {
            Ok(vec![
                Token::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(reg, dt)))),
                lit_token,
            ])
        } else {
            Ok(vec![lit_token])
        }
    }
}

pub struct BinaryExpression {
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Dereference,
    Positive,
    Negative,
    Not,
    BitwiseNot,
    AddressOf,
}

pub struct UnaryExpression {
    expr: Box<dyn Expression>,
    operator: UnaryOperator,
}

impl Expression for UnaryExpression {
    fn get_type(&self) -> SpType {
        self.expr.get_type()
    }

    fn load_to(&self, reg: Register, spare: Register) -> Result<Vec<Token>, ExpressionError> {
        let mut res = self.expr.load_to(reg, spare)?;
        let mut reg_type = ArgumentType::new(reg, self.expr.get_type().base_primitive().unwrap());
        match self.operator {
            UnaryOperator::Dereference => {
                res.push(Token::OperationLiteral(Box::new(OpLd::new(
                    reg_type,
                    reg.into(),
                ))));
            }
            UnaryOperator::AddressOf => panic!("not supported (yet?)"),
            UnaryOperator::Positive => (),
            UnaryOperator::Negative => {
                res.push(Token::OperationLiteral(Box::new(OpNeg::new(
                    reg_type,
                    reg.into(),
                ))));
            }
            UnaryOperator::Not => {
                res.push(Token::OperationLiteral(Box::new(OpNot::new(
                    reg.into(),
                    reg.into(),
                ))));
            }
            UnaryOperator::BitwiseNot => {
                res.push(Token::OperationLiteral(Box::new(OpBnot::new(
                    reg_type,
                    reg.into(),
                ))));
            }
        }
        Ok(res)
    }
}

pub struct AsExpression {
    expr: Box<dyn Expression>,
    new_type: Box<SpType>,
}

impl Expression for AsExpression {
    fn get_type(&self) -> SpType {
        self.new_type.as_ref().clone()
    }

    fn load_to(&self, reg: Register, spare: Register) -> Result<Vec<Token>, ExpressionError> {
        let from_type = if let Some(dt) = self.expr.get_type().base_primitive() {
            dt
        } else {
            return Err(ExpressionError::ExpressionRequiresPrimitive(
                self.expr.get_type(),
            ));
        };

        let to_type = if let Some(dt) = self.new_type.base_primitive() {
            dt
        } else {
            return Err(ExpressionError::ExpressionRequiresPrimitive(
                *self.new_type.clone(),
            ));
        };

        let mut res = self.expr.load_to(reg, spare)?;
        res.push(Token::OperationLiteral(Box::new(OpConv::new(
            ArgumentType::new(reg, to_type),
            ArgumentType::new(reg, from_type),
        ))));

        Ok(res)
    }
}
