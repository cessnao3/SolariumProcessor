use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
    sync::LazyLock,
};

use jib::cpu::{DataType, Register};
use jib_asm::{ArgumentType, AsmToken, Instruction, OpAdd, OpConv, OpPopr, OpPush, OpSub};

use crate::{
    TokenError,
    compiler::CompilingState,
    literals::{Literal, LiteralValue},
    tokenizer::{Token, TokenIter, get_identifier},
    typing::Type,
};

#[derive(Debug, Clone, Copy)]
pub struct RegisterDef {
    pub reg: Register,
    pub spare: Register,
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterDefError {
    RegisterEqualToSpare,
}

impl Display for RegisterDefError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "register and spare location cannot be the same")
    }
}

impl RegisterDef {
    pub const FN_BASE: Register = Register::first_gp_register();
    pub const SPARE: Register = Self::increment(Self::FN_BASE);
    pub const FIRST_USEABLE: Register = Self::increment(Self::SPARE);

    const fn increment(reg: Register) -> Register {
        let next = Register::GeneralPurpose(reg.get_index() + 1);
        assert!(next.get_index() <= Register::last_register().get_index());
        next
    }

    pub fn new(reg: Register) -> Result<Self, RegisterDefError> {
        if reg < Self::FIRST_USEABLE || reg > Register::last_register() {
            Err(RegisterDefError::RegisterEqualToSpare)
        } else {
            Ok(Self {
                reg,
                spare: Self::SPARE,
            })
        }
    }

    pub fn increment_register(&self) -> Option<Self> {
        Self::new(Register::GeneralPurpose(self.reg.get_index() + 1)).ok()
    }

    pub fn push_spare(&self) -> AsmToken {
        AsmToken::OperationLiteral(Box::new(OpPush::new(self.spare.into())))
    }

    pub fn pop_spare(&self) -> AsmToken {
        AsmToken::OperationLiteral(Box::new(OpPopr::new(self.spare.into())))
    }
}

impl Default for RegisterDef {
    fn default() -> Self {
        Self::new(Self::FIRST_USEABLE).unwrap()
    }
}

pub trait Expression: Debug + Display {
    fn get_token(&self) -> &Token;

    fn get_type(&self) -> Result<Type, TokenError>;

    fn get_primitive_type(&self) -> Result<DataType, TokenError> {
        if let Some(t) = self.get_type()?.primitive_type() {
            Ok(t)
        } else {
            Err(self
                .get_token()
                .clone()
                .into_err("unable to obtain primitive type from expression"))
        }
    }

    fn load_address_to_register(
        &self,
        _reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        Err(self
            .get_token()
            .clone()
            .into_err("expression does not have a mappable address in memory"))
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError>;

    fn simplify(&self) -> Option<Literal> {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperation {
    BitNot,
    Not,
    Plus,
    Minus,
}

impl UnaryOperation {
    pub const ALL: &[Self] = &[Self::BitNot, Self::Not, Self::Plus, Self::Minus];
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::BitNot => "~",
            Self::Not => "!",
            Self::Plus => "+",
            Self::Minus => "-",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    token: Token,
    op: UnaryOperation,
    base: Rc<dyn Expression>,
}

impl UnaryExpression {
    pub fn new(token: Token, op: UnaryOperation, base: Rc<dyn Expression>) -> Self {
        Self { token, op, base }
    }
}

impl Expression for UnaryExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Some(Type::Primitive(t)) = self.base.get_type()?.base_type() {
            Ok(Type::Primitive(t))
        } else {
            Err(self
                .token
                .clone()
                .into_err("expression is not a primitive data type for unary expression"))
        }
    }

    fn load_value_to_register(
        &self,
        _reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        todo!()
    }

    fn simplify(&self) -> Option<Literal> {
        LiteralValue::unary(*self.base.simplify()?.get_value(), self.op)
            .map_or(None, |x| Some(Literal::new(self.token.clone(), x)))
    }
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.base)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperation {
    Plus,
    Minus,
    Product,
    Divide,
    And,
    Or,
    Equals,
    NotEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BitAnd,
    BitOr,
    BitXor,
}

impl BinaryOperation {
    pub const ALL: &[Self] = &[
        Self::Plus,
        Self::Minus,
        Self::Product,
        Self::Divide,
        Self::And,
        Self::Or,
        Self::Equals,
        Self::NotEquals,
        Self::Greater,
        Self::GreaterEqual,
        Self::Less,
        Self::LessEqual,
        Self::BitAnd,
        Self::BitOr,
        Self::BitXor,
    ];
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Product => "*",
            Self::Divide => "/",
            Self::And => "&&",
            Self::Or => "||",
            Self::Equals => "==",
            Self::NotEquals => "!=",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    token: Token,
    operation: BinaryOperation,
    lhs: Rc<dyn Expression>,
    rhs: Rc<dyn Expression>,
}

impl BinaryExpression {
    pub fn new(
        token: Token,
        op: BinaryOperation,
        lhs: Rc<dyn Expression>,
        rhs: Rc<dyn Expression>,
    ) -> Self {
        Self {
            token,
            operation: op,
            lhs,
            rhs,
        }
    }
}

impl Expression for BinaryExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Some(Type::Primitive(a)) = self.lhs.get_type()?.base_type() {
            if let Some(Type::Primitive(b)) = self.rhs.get_type()?.base_type() {
                Ok(Type::Primitive(Type::coerce_type(a, b)))
            } else {
                Err(self
                    .rhs
                    .get_token()
                    .clone()
                    .into_err("cannot convert expression to expression primitive type"))
            }
        } else {
            Err(self
                .lhs
                .get_token()
                .clone()
                .into_err("cannot convert expression to expression primitive type"))
        }
    }

    fn simplify(&self) -> Option<Literal> {
        let rhs_val = self.rhs.simplify()?;
        let lhs_val = self.lhs.simplify()?;

        if let Ok(v) =
            LiteralValue::operation(*lhs_val.get_value(), *rhs_val.get_value(), self.operation)
        {
            Some(Literal::new(self.token.clone(), v))
        } else {
            None
        }
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        if let Some(dt) = self.get_type()?.primitive_type() {
            let mut asm = self.lhs.load_value_to_register(reg)?;
            let reg_a = reg.reg;

            let reg_b;
            if let Some(reg_next) = reg.increment_register() {
                asm.extend_from_slice(&self.rhs.load_value_to_register(reg_next)?);
                reg_b = reg_next.reg;
            } else {
                asm.push(self.get_token().to_asm(reg.push_spare()));
                asm.extend_from_slice(&self.rhs.load_value_to_register(reg)?);
                asm.push(self.get_token().to_asm(reg.pop_spare()));
                reg_b = reg.spare;
            }

            if let Some(p) = self.lhs.get_type()?.primitive_type() {
                if p != dt {
                    asm.push(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                        OpConv::new(ArgumentType::new(reg_a, dt), ArgumentType::new(reg_a, p)),
                    ))));
                }
            } else {
                panic!("type error!")
            }

            if let Some(p) = self.rhs.get_type()?.primitive_type() {
                if p != dt {
                    asm.push(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                        OpConv::new(ArgumentType::new(reg_b, dt), ArgumentType::new(reg_b, p)),
                    ))));
                }
            } else {
                panic!("type error")
            }

            let reg_type = ArgumentType::new(reg.reg, dt);

            let x: Box<dyn Instruction> = match self.operation {
                BinaryOperation::Plus => Box::new(OpAdd::new(reg_type, reg_a.into(), reg_b.into())),
                BinaryOperation::Minus => {
                    Box::new(OpSub::new(reg_type, reg_a.into(), reg_b.into()))
                }
                x => todo!("operation {x} not yet supported"),
            };

            asm.push(self.get_token().to_asm(AsmToken::OperationLiteral(x)));

            Ok(asm)
        } else {
            Err(self
                .get_token()
                .clone()
                .into_err("non-primitive types not yet supported"))
        }
    }
}

impl Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.operation, self.rhs)
    }
}

pub fn parse_expression(
    tokens: &mut TokenIter,
    state: &CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
    // if keyword
    // if literal?
    // if paren

    static UNARY_STR: LazyLock<HashMap<String, UnaryOperation>> = LazyLock::new(|| {
        UnaryOperation::ALL
            .iter()
            .map(|x| (x.to_string(), *x))
            .collect()
    });
    static BINARY_STR: LazyLock<HashMap<String, BinaryOperation>> = LazyLock::new(|| {
        BinaryOperation::ALL
            .iter()
            .map(|x| (x.to_string(), *x))
            .collect()
    });
    //static KEYWORDS: LazyLock<HashSet<String>> = LazyLock::new(|| HashSet::new()); // TODO - Update with actual keywords // TODO - Update tokenizer regex with keywords and expressions

    let first = tokens.next()?;

    let expr: Rc<dyn Expression> = if let Some(op) = UNARY_STR.get(first.get_value()) {
        Rc::new(UnaryExpression::new(
            first,
            *op,
            parse_expression(tokens, state)?,
        ))
    } else if get_identifier(&first).is_ok() {
        state.get_variable(&first)?.clone()
    } else if let Ok(lit) = Literal::try_from(first.clone()) {
        Rc::new(lit)
    } else {
        panic!("unknown value {}", first.get_value());
    };

    let return_expr = if let Some(next) = tokens.peek() {
        if let Some(op) = BINARY_STR.get(next.get_value()) {
            let op_token = tokens.next()?;
            Rc::new(BinaryExpression::new(
                op_token,
                *op,
                expr,
                parse_expression(tokens, state)?,
            ))
        } else {
            expr
        }
    } else {
        expr
    };

    if let Some(lit) = return_expr.simplify() {
        Ok(Rc::new(lit))
    } else {
        Ok(return_expr)
    }
}
