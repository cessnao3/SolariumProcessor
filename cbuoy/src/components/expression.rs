use std::fmt::Display;

use jib::cpu::{DataType, Register};
use jib_asm::{
    argument::ArgumentType,
    instructions::{
        OpAdd, OpBand, OpBnot, OpBor, OpBshl, OpBshr, OpBxor, OpConv, OpDiv, OpJmp, OpJmpri, OpLd,
        OpLdi, OpLdn, OpMul, OpNeg, OpNot, OpRem, OpSub, OpTeq, OpTneq, OpTnz, OpTz,
    },
    AsmToken, AsmTokenLoc, FromLiteral, LocationInfo,
};

use crate::{
    tokenizer::Token,
    types::{Type, TypeError},
};

use super::{AsmGenState, ErrorToken};

#[derive(Debug, Clone)]
pub enum ExpressionError {
    TypeError(TypeError),
    NotAddressable,
}

impl From<TypeError> for ExpressionError {
    fn from(value: TypeError) -> Self {
        Self::TypeError(value)
    }
}

impl Display for ExpressionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeError(t) => write!(f, "{}", t),
            Self::NotAddressable => write!(f, "Not Addressable"),
        }
    }
}

pub trait Expression {
    fn get_type(&self) -> Result<Type, TypeError>;

    fn get_type_tok(&self) -> Result<Type, ErrorToken> {
        ErrorToken::test(&self.get_token(), self.get_type())
    }

    fn get_base_primitive(&self) -> Result<DataType, ErrorToken> {
        ErrorToken::test(&self.get_token(), self.get_type_tok()?.base_primitive())
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
        state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken>;

    fn load_address(&self, _reg: Register) -> Result<Vec<AsmToken>, ErrorToken> {
        Err(ErrorToken::new(
            self.get_token(),
            &ExpressionError::NotAddressable.to_string(),
        ))
    }

    fn get_token(&self) -> Token;
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

    pub fn to_tokens(&self) -> Vec<AsmToken> {
        match self {
            Self::U8(v) => vec![AsmToken::from_literal(*v)],
            Self::I8(v) => vec![AsmToken::from_literal(*v)],
            Self::U16(v) => vec![AsmToken::from_literal(*v)],
            Self::I16(v) => vec![AsmToken::from_literal(*v)],
            Self::U32(v) => vec![AsmToken::from_literal(*v)],
            Self::I32(v) => vec![AsmToken::from_literal(*v)],
            Self::F32(v) => vec![AsmToken::from_literal(*v)],
        }
    }

    pub fn get_type(&self) -> Type {
        let init = match self {
            Self::U8(_) => Type::Primitive { base: DataType::U8 },
            Self::I8(_) => Type::Primitive { base: DataType::I8 },
            Self::U16(_) => Type::Primitive {
                base: DataType::U16,
            },
            Self::I16(_) => Type::Primitive {
                base: DataType::I16,
            },
            Self::U32(_) => Type::Primitive {
                base: DataType::U32,
            },
            Self::I32(_) => Type::Primitive {
                base: DataType::I32,
            },
            Self::F32(_) => Type::Primitive {
                base: DataType::F32,
            },
        };

        Type::Constant {
            base: Box::new(init),
        }
    }
}

impl From<u8> for Literal {
    fn from(value: u8) -> Self {
        Self::U8(value)
    }
}

pub struct LiteralExpression {
    tok: Token,
    literal: Literal,
}

impl LiteralExpression {
    pub fn new(tok: Token, literal: Literal) -> Self {
        Self { tok, literal }
    }
}

impl Expression for LiteralExpression {
    fn get_type(&self) -> Result<Type, TypeError> {
        Ok(self.literal.get_type())
    }

    fn load_to(
        &self,
        reg: Register,
        _spare: Register,
        _state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        let (lit_token, lit_type) = match self.literal {
            Literal::U8(val) => (
                AsmToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::I8),
                    val as u16,
                ))),
                None,
            ),
            Literal::I8(val) => (
                AsmToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    (val as i16) as u16,
                ))),
                None,
            ),
            Literal::I16(val) => (
                AsmToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    val as u16,
                ))),
                None,
            ),
            Literal::U16(val) => (
                AsmToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(reg, DataType::U8),
                    (val as i16) as u16,
                ))),
                None,
            ),
            Literal::U32(val) => (AsmToken::Literal4(val), Some(DataType::U32)),
            Literal::I32(val) => (AsmToken::Literal4(val as u32), Some(DataType::I32)),
            Literal::F32(val) => (AsmToken::Literal4(val.to_bits()), Some(DataType::F32)),
        };

        if let Some(dt) = lit_type {
            Ok(vec![
                AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(reg, dt)))),
                lit_token,
            ])
        } else {
            Ok(vec![lit_token])
        }
    }

    fn get_token(&self) -> Token {
        self.tok.clone()
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
    Eq,
    Neq,
}

pub struct BinaryExpression {
    tok: Token,
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>,
    operator: BinaryOperator,
}

impl BinaryExpression {
    pub fn new(
        tok: Token,
        operator: BinaryOperator,
        lhs: Box<dyn Expression>,
        rhs: Box<dyn Expression>,
    ) -> Result<Self, ErrorToken> {
        let mut tl = ErrorToken::test(&tok, lhs.get_type())?;
        let mut tr = ErrorToken::test(&tok, rhs.get_type())?;

        if let Ok(t) = tl.base_primitive() {
            tl = Type::Primitive { base: t };
        }

        if let Ok(t) = tr.base_primitive() {
            tr = Type::Primitive { base: t };
        }

        if tl != tr {
            Err(ErrorToken::new(
                tok,
                &TypeError::TypeMismatch(tl, tr).to_string(),
            ))
        } else {
            Ok(Self {
                tok,
                lhs,
                rhs,
                operator,
            })
        }
    }
}

impl Expression for BinaryExpression {
    fn get_type(&self) -> Result<Type, TypeError> {
        self.lhs.get_type()
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
        state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        let mut res = self.lhs.load_to(reg, spare, state)?;

        let load_val_b = self.rhs.load_to(state.temporary_register(), spare, state)?;
        let mut uses_val_b = true;

        let reg_type = ArgumentType::new(reg, self.lhs.get_base_primitive()?);
        let test_code = match self.operator {
            BinaryOperator::Add => {
                vec![AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Sub => {
                vec![AsmToken::OperationLiteral(Box::new(OpSub::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Mul => {
                vec![AsmToken::OperationLiteral(Box::new(OpMul::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Div => {
                vec![AsmToken::OperationLiteral(Box::new(OpDiv::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Rem => {
                vec![AsmToken::OperationLiteral(Box::new(OpRem::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Bshl => {
                vec![AsmToken::OperationLiteral(Box::new(OpBshl::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Bshr => {
                vec![AsmToken::OperationLiteral(Box::new(OpBshr::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Band => {
                vec![AsmToken::OperationLiteral(Box::new(OpBand::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Bor => {
                vec![AsmToken::OperationLiteral(Box::new(OpBor::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Bxor => {
                vec![AsmToken::OperationLiteral(Box::new(OpBxor::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Land | BinaryOperator::Lor => {
                uses_val_b = false;

                let test_token =
                    AsmToken::OperationLiteral(if self.operator == BinaryOperator::Land {
                        Box::new(OpTz::new(reg.into()))
                    } else {
                        Box::new(OpTnz::new(reg.into()))
                    });

                // TODO - Switch to using labels if needed
                let load_val_b_new = self.rhs.load_to(reg, spare, state)?;
                let mut test_code_vals = Vec::new();

                // Determine if the value to load is less than the max allowed immediate.
                // If so, use as a literal
                // Otherwise, create the short circuit state and jump to that
                if load_val_b_new.len() < u16::MAX as usize {
                    test_code_vals.push(test_token);
                    test_code_vals.push(AsmToken::OperationLiteral(Box::new(OpJmpri::new(
                        load_val_b_new.len() as u16,
                    ))));
                    test_code_vals.extend(load_val_b_new);
                } else {
                    let label_val = format!("switch_short_circuit_val_{}", state.label_num);
                    state.label_num += 1;

                    test_code_vals.push(AsmToken::OperationLiteral(Box::new(OpLdn::new(
                        ArgumentType::new(state.temporary_register(), DataType::U32),
                    ))));
                    test_code_vals.push(AsmToken::LoadLoc(label_val.clone()));
                    test_code_vals.push(test_token);
                    test_code_vals.push(AsmToken::OperationLiteral(Box::new(OpJmp::new(
                        state.temporary_register().into(),
                    ))));
                    test_code_vals.extend(load_val_b_new);
                    test_code_vals.push(AsmToken::CreateLabel(label_val));
                }
                test_code_vals
            }
            BinaryOperator::Eq => {
                vec![AsmToken::OperationLiteral(Box::new(OpTeq::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
            BinaryOperator::Neq => {
                vec![AsmToken::OperationLiteral(Box::new(OpTneq::new(
                    reg_type,
                    reg.into(),
                    state.temporary_register().into(),
                )))]
            }
        };

        if uses_val_b {
            res.extend(load_val_b);
        }

        res.extend(test_code);

        Ok(res)
    }

    fn get_token(&self) -> Token {
        self.tok.clone()
    }
}

pub struct AssignmentExpression {
    tok: Token,
    rval: Box<dyn Expression>,
    lval: Box<dyn Expression>,
}

impl AssignmentExpression {
    pub fn new(tok: Token, rval: Box<dyn Expression>, lval: Box<dyn Expression>) -> Self {
        // TODO - Check type error?
        Self { tok, rval, lval }
    }
}

impl Expression for AssignmentExpression {
    fn get_type(&self) -> Result<Type, TypeError> {
        self.rval.get_type()
    }

    fn load_address(&self, _reg: Register) -> Result<Vec<AsmToken>, ErrorToken> {
        Err(ErrorToken::new(
            self.get_token(),
            &ExpressionError::NotAddressable.to_string(),
        ))
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
        state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        panic!("assignment not implemented");
    }

    fn get_token(&self) -> Token {
        self.tok.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Positive,
    Negative,
    Not,
    BitwiseNot,
    AddressOf,
    Dereference,
}

pub struct UnaryExpression {
    tok: Token,
    expr: Box<dyn Expression>,
    operator: UnaryOperator,
}

impl UnaryExpression {
    pub fn new(
        tok: Token,
        operator: UnaryOperator,
        expr: Box<dyn Expression>,
    ) -> Result<Self, TypeError> {
        Ok(Self {
            tok,
            expr,
            operator,
        })
    }
}

impl Expression for UnaryExpression {
    fn get_type(&self) -> Result<Type, TypeError> {
        match self.operator {
            UnaryOperator::AddressOf => Ok(Type::Pointer {
                base: Box::new(self.expr.get_type()?),
            }),
            UnaryOperator::Dereference => match self.expr.get_type()? {
                Type::Pointer { base } => Ok(*base.clone()),
                t => Err(TypeError::CannotDereference(t)),
            },
            _ => self.expr.get_type(),
        }
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
        state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        let mut res = self.expr.load_to(reg, spare, state)?;
        let reg_type = ArgumentType::new(reg, self.get_base_primitive()?);
        match self.operator {
            UnaryOperator::Positive => (),
            UnaryOperator::Negative => {
                res.push(AsmToken::OperationLiteral(Box::new(OpNeg::new(
                    reg_type,
                    reg.into(),
                ))));
            }
            UnaryOperator::Not => {
                res.push(AsmToken::OperationLiteral(Box::new(OpNot::new(
                    reg.into(),
                    reg.into(),
                ))));
            }
            UnaryOperator::BitwiseNot => {
                res.push(AsmToken::OperationLiteral(Box::new(OpBnot::new(
                    reg_type,
                    reg.into(),
                ))));
            }
            UnaryOperator::AddressOf => {
                res.extend(self.expr.load_address(reg)?);
            }
            UnaryOperator::Dereference => {
                res.extend(self.expr.load_address(reg)?);
                res.push(AsmToken::OperationLiteral(Box::new(OpLd::new(
                    reg_type,
                    reg.into(),
                ))));
            }
        }
        Ok(res)
    }

    fn load_address(&self, reg: Register) -> Result<Vec<AsmToken>, ErrorToken> {
        match self.operator {
            UnaryOperator::Dereference => match self.expr.get_type_tok()? {
                Type::Pointer { .. } => self.expr.load_address(reg),
                _ => Err(ErrorToken::new(
                    self.tok.clone(),
                    ExpressionError::NotAddressable,
                )),
            },
            _ => Err(ErrorToken::new(
                self.tok.clone(),
                ExpressionError::NotAddressable,
            )),
        }
    }

    fn get_token(&self) -> Token {
        self.tok.clone()
    }
}

pub struct AsExpression {
    tok: Token,
    expr: Box<dyn Expression>,
    new_type: Box<Type>,
}

impl Expression for AsExpression {
    fn get_type(&self) -> Result<Type, TypeError> {
        Ok(self.new_type.as_ref().clone())
    }

    fn load_to(
        &self,
        reg: Register,
        spare: Register,
        state: &mut AsmGenState,
    ) -> Result<Vec<AsmToken>, ErrorToken> {
        let from_type = self.expr.get_base_primitive()?;
        let to_type = ErrorToken::test(&self.tok, self.new_type.base_primitive())?;

        let mut res = self.expr.load_to(reg, spare, state)?;
        res.push(AsmToken::OperationLiteral(Box::new(OpConv::new(
            ArgumentType::new(reg, to_type),
            ArgumentType::new(reg, from_type),
        ))));

        Ok(res)
    }

    fn get_token(&self) -> Token {
        self.tok.clone()
    }
}
