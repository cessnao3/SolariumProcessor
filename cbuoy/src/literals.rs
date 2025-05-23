use std::{fmt::Display, sync::LazyLock};

use jib::cpu::{DataType, OperationError, OperatorManager, convert_types};
use jib_asm::{ArgumentType, AsmToken};
use regex::Regex;

use crate::{
    TokenError,
    expressions::{BinaryOperation, Expression, RegisterDef, UnaryOperation},
    tokenizer::Token,
    typing::Type,
};

static OPERATIONS: LazyLock<OperatorManager> = LazyLock::new(OperatorManager::default);

#[derive(Debug, Clone)]
pub struct Literal {
    token: Token,
    value: LiteralValue,
}

impl Literal {
    pub fn new(token: Token, value: LiteralValue) -> Self {
        Self { token, value }
    }

    pub fn get_value(&self) -> &LiteralValue {
        &self.value
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LiteralValue {
    U8(u8),
    U16(u16),
    U32(u32),
    I8(i8),
    I16(i16),
    I32(i32),
    F32(f32),
}

impl LiteralValue {
    pub fn get_dtype(&self) -> DataType {
        match &self {
            Self::U8(_) => DataType::U8,
            Self::U16(_) => DataType::U16,
            Self::U32(_) => DataType::U32,
            Self::I8(_) => DataType::I8,
            Self::I16(_) => DataType::I16,
            Self::I32(_) => DataType::I32,
            Self::F32(_) => DataType::F32,
        }
    }

    pub fn as_u32(&self) -> u32 {
        match *self {
            Self::U8(x) => x as u32,
            Self::U16(x) => x as u32,
            Self::U32(x) => x,
            Self::I8(x) => (x as i32) as u32,
            Self::I16(x) => (x as i32) as u32,
            Self::I32(x) => x as u32,
            Self::F32(x) => x.to_bits(),
        }
    }

    pub fn as_size(&self) -> Option<i32> {
        match *self {
            Self::U8(x) => Some(x as i32),
            Self::U16(x) => Some(x as i32),
            Self::U32(x) => Some(x as i32),
            Self::I8(x) => Some(x as i32),
            Self::I16(x) => Some(x as i32),
            Self::I32(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_asm_literal(&self) -> AsmToken {
        match *self {
            Self::U8(x) => AsmToken::Literal1(x),
            Self::I8(x) => AsmToken::Literal1(x as u8),
            Self::U16(x) => AsmToken::Literal2(x),
            Self::I16(x) => AsmToken::Literal2(x as u16),
            Self::U32(x) => AsmToken::Literal4(x),
            Self::I32(x) => AsmToken::Literal4(x as u32),
            Self::F32(x) => AsmToken::Literal4(x.to_bits()),
        }
    }

    pub fn from_u32(x: u32, dtype: DataType) -> Self {
        match dtype {
            DataType::U8 => Self::U8(x as u8),
            DataType::U16 => Self::U16(x as u16),
            DataType::U32 => Self::U32(x),
            DataType::I8 => Self::I8((x as i32) as i8),
            DataType::I16 => Self::I16((x as i32) as i16),
            DataType::I32 => Self::I32(x as i32),
            DataType::F32 => Self::F32(f32::from_bits(x)),
        }
    }

    pub fn unary(a: Self, op: UnaryOperation) -> Result<Self, OperationError> {
        let x = a.as_u32();
        let dt = a.get_dtype();

        let res = match op {
            UnaryOperation::BitNot => OPERATIONS.get_bitwise(dt)?.bnot(x)?.val,
            UnaryOperation::Plus => x,
            UnaryOperation::Minus => OPERATIONS.get_arith(dt).neg(x)?.val,
            UnaryOperation::Not => {
                if x == 0 {
                    1
                } else {
                    0
                }
            }
        };

        Ok(Self::from_u32(res, dt))
    }

    pub fn operation(lhs: Self, rhs: Self, op: BinaryOperation) -> Result<Self, OperationError> {
        let dt: DataType = if lhs.get_dtype() > rhs.get_dtype() {
            lhs.get_dtype()
        } else {
            rhs.get_dtype()
        };

        let a = lhs.as_u32();
        let b = rhs.as_u32();

        fn u32_to_bool(x: u32) -> u32 {
            if x != 0 { 1 } else { 0 }
        }

        let res = match op {
            BinaryOperation::Plus => OPERATIONS.get_arith(dt).add(a, b)?.val,
            BinaryOperation::Minus => OPERATIONS.get_arith(dt).sub(a, b)?.val,
            BinaryOperation::Product => OPERATIONS.get_arith(dt).mul(a, b)?.val,
            BinaryOperation::Divide => OPERATIONS.get_arith(dt).div(a, b)?.val,
            BinaryOperation::Greater => OPERATIONS.get_relative(dt).gt(a, b)? as u32,
            BinaryOperation::GreaterEqual => OPERATIONS.get_relative(dt).geq(a, b)? as u32,
            BinaryOperation::Less => OPERATIONS.get_relative(dt).lt(a, b)? as u32,
            BinaryOperation::LessEqual => OPERATIONS.get_relative(dt).leq(a, b)? as u32,
            BinaryOperation::Equals => OPERATIONS.get_relative(dt).eq(a, b)? as u32,
            BinaryOperation::NotEquals => OPERATIONS.get_relative(dt).neq(a, b)? as u32,
            BinaryOperation::And => u32_to_bool(OPERATIONS.get_bitwise(dt)?.band(a, b)?.val),
            BinaryOperation::Or => u32_to_bool(OPERATIONS.get_bitwise(dt)?.bor(a, b)?.val),
            BinaryOperation::BitAnd => OPERATIONS.get_bitwise(dt)?.band(a, b)?.val,
            BinaryOperation::BitOr => OPERATIONS.get_bitwise(dt)?.bor(a, b)?.val,
            BinaryOperation::BitXor => OPERATIONS.get_bitwise(dt)?.bxor(a, b)?.val,
        };

        Ok(Self::from_u32(res, dt))
    }

    pub fn convert(&self, dt: DataType) -> Self {
        let val = convert_types(self.as_u32(), self.get_dtype(), dt);

        match dt {
            DataType::U8 => Self::U8(val as u8),
            DataType::I8 => Self::I8((val as i32) as i8),
            DataType::U16 => Self::U16(val as u16),
            DataType::I16 => Self::I16((val as i32) as i16),
            DataType::U32 => Self::U32(val),
            DataType::I32 => Self::I32(val as i32),
            DataType::F32 => Self::F32(f32::from_bits(val)),
        }
    }
}

impl Expression for Literal {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(Type::Primitive(self.value.get_dtype()))
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let op_lit = match self.value {
            LiteralValue::U32(x) => jib_asm::AsmToken::Literal4(x),
            LiteralValue::U16(x) => jib_asm::AsmToken::Literal2(x),
            LiteralValue::U8(x) => jib_asm::AsmToken::Literal1(x),
            LiteralValue::I32(x) => jib_asm::AsmToken::Literal4(x as u32),
            LiteralValue::I16(x) => jib_asm::AsmToken::Literal2(x as u16),
            LiteralValue::I8(x) => jib_asm::AsmToken::Literal1(x as u8),
            LiteralValue::F32(x) => jib_asm::AsmToken::Literal4(x.to_bits()),
        };
        let op_load = AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
            reg.reg,
            self.value.get_dtype(),
        ))));

        Ok(self
            .get_token()
            .to_asm_iter([op_load, op_lit, AsmToken::AlignInstruction])
            .into_iter()
            .collect())
    }

    fn simplify(&self) -> Option<Literal> {
        Some(self.clone())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            LiteralValue::F32(x) => write!(f, "{x}f32"),
            LiteralValue::U8(x) => write!(f, "{x}u8"),
            LiteralValue::U16(x) => write!(f, "{x}u16"),
            LiteralValue::U32(x) => write!(f, "{x}u32"),
            LiteralValue::I8(x) => write!(f, "{x}i8"),
            LiteralValue::I16(x) => write!(f, "{x}i16"),
            LiteralValue::I32(x) => write!(f, "{x}i32"),
        }
    }
}

impl TryFrom<Token> for Literal {
    type Error = TokenError;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        static LITERAL_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(r"^(((?<inum>(0x)?\d+)(?<itype>[ui](8|(16)|(32)))?)|((?<fnum>(\d+(\.\d*))|(\.\d+))f32)|(?<f32>\d*\.\d+))$").unwrap()
        });

        let res = if let Some(m) = LITERAL_REGEX.captures(value.get_value()) {
            if let Some(inum) = m.name("inum") {
                let mut num = inum.as_str();

                let radix = if let Some(n) = num.strip_prefix("0x") {
                    num = n;
                    16
                } else {
                    10
                };

                if let Some(itype) = m.name("itype") {
                    let val = match itype.as_str() {
                        "u8" => u8::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::U8(x))),
                        "u16" => u16::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::U16(x))),
                        "u32" => u32::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::U32(x))),
                        "i8" => i8::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::I8(x))),
                        "i16" => i16::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::I16(x))),
                        "i32" => i32::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::I32(x))),
                        x => {
                            return Err(value
                                .clone()
                                .into_err(format!("unknown integer type {}", x)));
                        }
                    };

                    match val {
                        Some(v) => Ok(v),
                        None => {
                            return Err(value.clone().into_err(format!(
                                "unable to convert {} as {} literal",
                                num,
                                itype.as_str()
                            )));
                        }
                    }
                } else {
                    match i32::from_str_radix(num, radix) {
                        Ok(x) => Ok(LiteralValue::I32(x)),
                        Err(_) => Err(value
                            .clone()
                            .into_err(format!("unable to get i32 value from {}", inum.as_str()))),
                    }
                }
            } else if let Some(fnum) = m.name("fnum") {
                if let Ok(fv) = fnum.as_str().parse::<f32>() {
                    Ok(LiteralValue::F32(fv))
                } else {
                    Err(value
                        .clone()
                        .into_err(format!("unable to get float value from {}", fnum.as_str())))
                }
            } else if let Some(fnum) = m.name("f32") {
                match fnum.as_str().parse() {
                    Ok(x) => Ok(LiteralValue::F32(x)),
                    Err(_) => Err(value
                        .clone()
                        .into_err(format!("unable to get float value from {}", fnum.as_str()))),
                }
            } else {
                Err(value.clone().into_err("cannot parse as a literal"))
            }
        } else {
            Err(value.clone().into_err("cannot parse as a literal"))
        }?;

        Ok(Literal {
            token: value,
            value: res,
        })
    }
}
