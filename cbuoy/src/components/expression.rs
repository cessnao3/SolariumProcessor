use jasm::{
    argument::ArgumentType,
    instructions::{OpAdd, OpBand, OpBnot, OpBool, OpBor, OpBshl, OpBshr, OpBxor, OpConv, OpDiv, OpLd, OpLdi, OpLdn, OpMul, OpNeg, OpNoop, OpNot, OpRem, OpSub},
    AssemblerToken, FromLiteral,
};
use jib::cpu::{DataType, Register};

use crate::types::{SpType, SpTypeError};

use super::addressable::Addressable;

#[derive(Debug, Clone)]
pub enum ExpressionError {
    TypeError(SpTypeError),
}

impl From<SpTypeError> for ExpressionError {
    fn from(value: SpTypeError) -> Self {
        Self::TypeError(value)
    }
}

pub trait Expression {
    fn get_type(&self) -> SpType;

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
    ) -> Result<Vec<AssemblerToken>, ExpressionError>;
}

pub trait ExpressionLValue: Expression + Addressable {}

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

    pub fn base_type(&self) -> DataType {
        match self {
            Self::U8(_) => DataType::U8,
            Self::I8(_) => DataType::I8,
            Self::U16(_) => DataType::U16,
            Self::I16(_) => DataType::I16,
            Self::U32(_) => DataType::U32,
            Self::I32(_) => DataType::I32,
            Self::F32(_) => DataType::F32,
        }
    }

    pub fn to_tokens(&self) -> Vec<AssemblerToken> {
        match self {
            Self::U8(v) => vec![AssemblerToken::from_literal(*v)],
            Self::I8(v) => vec![AssemblerToken::from_literal(*v)],
            Self::U16(v) => vec![AssemblerToken::from_literal(*v)],
            Self::I16(v) => vec![AssemblerToken::from_literal(*v)],
            Self::U32(v) => vec![AssemblerToken::from_literal(*v)],
            Self::I32(v) => vec![AssemblerToken::from_literal(*v)],
            Self::F32(v) => vec![AssemblerToken::from_literal(*v)],
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
    ) -> Result<Vec<AssemblerToken>, ExpressionError> {
        let (lit_token, lit_type) = match self {
            Self::U8(val) => (
                AssemblerToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::I8),
                    *val as u16,
                ))),
                None,
            ),
            Self::I8(val) => (
                AssemblerToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    (*val as i16) as u16,
                ))),
                None,
            ),
            Self::I16(val) => (
                AssemblerToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    *val as u16,
                ))),
                None,
            ),
            Self::U16(val) => (
                AssemblerToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    (*val as i16) as u16,
                ))),
                None,
            ),
            Self::U32(val) => (AssemblerToken::Literal4(*val), Some(DataType::U32)),
            Self::I32(val) => (AssemblerToken::Literal4(*val as u32), Some(DataType::I32)),
            Self::F32(val) => (AssemblerToken::Literal4(val.to_bits()), Some(DataType::F32)),
        };

        if let Some(dt) = lit_type {
            Ok(vec![
                AssemblerToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(reg, dt)))),
                lit_token,
            ])
        } else {
            Ok(vec![lit_token])
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Bshl,
    Bshr,
    Band,
    Bor,
    Bxor,
    Land,
    Lor,
}

pub struct BinaryExpression {
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>,
    operator: BinaryOperator,
}

impl BinaryExpression {
    pub fn new(
        operator: BinaryOperator,
        lhs: Box<dyn Expression>,
        rhs: Box<dyn Expression>,
    ) -> Result<Self, SpTypeError> {
        if lhs.get_type() != rhs.get_type() {
            Err(SpTypeError::TypeMismatch(lhs.get_type(), rhs.get_type()))
        } else {
            Ok(Self { lhs, rhs, operator })
        }
    }
}

impl Expression for BinaryExpression {
    fn get_type(&self) -> SpType {
        self.lhs.get_type()
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
    ) -> Result<Vec<AssemblerToken>, ExpressionError> {
        let TEMP_REG = Register::GeneralPurpose(32);
        let mut res = self.lhs.load_to(reg, spare)?;
        res.extend(self.rhs.load_to(TEMP_REG, spare)?);

        /*
            Add,
        Sub,
        Mul,
        Div,
        Rem,
        Bshl,
        Bshr,
        Band,
        Bor,
        Land,
        Lor, */

        let reg_type = ArgumentType::new(reg, self.lhs.get_type().base_primitive()?);
        match self.operator {
            BinaryOperator::Add => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpAdd::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Sub => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpSub::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Mul => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpMul::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Div => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpDiv::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Rem => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpRem::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Bshl => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBshl::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Bshr => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBshr::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Band => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBand::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Bor => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBor::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Bxor => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBxor::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
            }
            BinaryOperator::Land => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBand::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
                // TODO - Short-Circuiting Versions!
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBool::new(reg.into(), reg.into()))));
            }
            BinaryOperator::Lor => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBor::new(
                    reg_type,
                    reg.into(),
                    TEMP_REG.into(),
                ))));
                // TODO - Short-Circuiting Versions!
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBool::new(reg.into(), reg.into()))));
            }
        }
        Ok(res)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Positive,
    Negative,
    Not,
    BitwiseNot,
}

pub struct UnaryExpression {
    expr: Box<dyn Expression>,
    operator: UnaryOperator,
}

impl UnaryExpression {
    pub fn new(operator: UnaryOperator, expr: Box<dyn Expression>) -> Result<Self, SpTypeError> {
        Ok(Self { expr, operator })
    }
}

impl Expression for UnaryExpression {
    fn get_type(&self) -> SpType {
        self.expr.get_type()
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
    ) -> Result<Vec<AssemblerToken>, ExpressionError> {
        let mut res = self.expr.load_to(reg, spare)?;
        let reg_type = ArgumentType::new(reg, self.expr.get_type().base_primitive()?);
        match self.operator {
            UnaryOperator::Positive => (),
            UnaryOperator::Negative => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpNeg::new(
                    reg_type,
                    reg.into(),
                ))));
            }
            UnaryOperator::Not => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpNot::new(
                    reg.into(),
                    reg.into(),
                ))));
            }
            UnaryOperator::BitwiseNot => {
                res.push(AssemblerToken::OperationLiteral(Box::new(OpBnot::new(
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

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
    ) -> Result<Vec<AssemblerToken>, ExpressionError> {
        let from_type = self.expr.get_type().base_primitive()?;
        let to_type = self.new_type.base_primitive()?;

        let mut res = self.expr.load_to(reg, spare)?;
        res.push(AssemblerToken::OperationLiteral(Box::new(OpConv::new(
            ArgumentType::new(reg, to_type),
            ArgumentType::new(reg, from_type),
        ))));

        Ok(res)
    }
}
