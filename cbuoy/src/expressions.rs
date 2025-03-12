use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
    sync::LazyLock,
};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, Instruction, OpAdd, OpConv, OpPopr, OpPush, OpSub, OpTeq, OpTneq,
};

use crate::{
    TokenError,
    compiler::{CompilingState, ScopeManager},
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
    pub const SPARE: Register = Self::increment_const(Self::FN_BASE);
    pub const FIRST_USEABLE: Register = Self::increment_const(Self::SPARE);

    const fn increment_const(reg: Register) -> Register {
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

    fn increment(&self) -> Option<Self> {
        Self::new(Register::GeneralPurpose(self.reg.get_index() + 1)).ok()
    }

    pub fn increment_token(&self, token: &Token) -> Result<Self, TokenError> {
        match self.increment() {
            Some(x) => Ok(x),
            None => Err(token.clone().into_err("cannot find valid register vlaue")),
        }
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

/*
#[derive(Debug, Clone)]
struct DereferenceExpression {
    base: Rc<dyn Expression>,
}

impl Expression for DereferenceExpression {
    fn get_type(&self) -> Result<Type, TokenError> {}
}

#[derive(Debug, Clone)]
struct AddressOfExpression {
    base: Rc<dyn Expression>,
}
    */

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
            if let Some(reg_next) = reg.increment() {
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

            let op_instructions: Vec<Box<dyn Instruction>> = match self.operation {
                BinaryOperation::Plus => {
                    vec![Box::new(OpAdd::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryOperation::Minus => {
                    vec![Box::new(OpSub::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryOperation::NotEquals => {
                    vec![Box::new(OpTeq::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryOperation::Equals => {
                    vec![Box::new(OpTneq::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                x => todo!("operation {x} not yet supported"),
            };

            asm.extend(
                self.get_token().to_asm_iter(
                    op_instructions
                        .into_iter()
                        .map(|x| AsmToken::OperationLiteral(x)),
                ),
            );

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

#[derive(Debug, Clone)]
struct AsExpression {
    token: Token,
    data_type: Type,
    expr: Rc<dyn Expression>,
}

impl Expression for AsExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.data_type.clone())
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let mut asm = self.expr.load_value_to_register(reg)?;
        if let Some(dt) = self.data_type.primitive_type() {
            let src_type = self.expr.get_primitive_type()?;

            if dt != src_type {
                asm.push(
                    self.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                            ArgumentType::new(reg.reg, dt),
                            ArgumentType::new(reg.reg, src_type),
                        )))),
                );
            }
            Ok(asm)
        } else {
            Err(self.token.clone().into_err("unable to perform conversion"))
        }
    }
}

impl Display for AsExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}):{}", self.expr, self.data_type)
    }
}

pub fn parse_expression(
    tokens: &mut TokenIter,
    state: &CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
    // if keyword
    // if literal?
    // if paren
    // function

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
            .collect() // TODO - work for values
    });

    let first = tokens.next()?;

    let expr: Rc<dyn Expression> = if let Some(op) = UNARY_STR.get(first.get_value()) {
        Rc::new(UnaryExpression::new(
            first,
            *op,
            parse_expression(tokens, state)?,
        ))
    } else if first.get_value() == "(" {
        let inner = parse_expression(tokens, state)?;
        tokens.expect(")")?;
        inner
    } else if get_identifier(&first).is_ok() {
        state.get_variable(&first)?.clone()
    } else {
        match Literal::try_from(first.clone()) {
            Ok(lit) => Rc::new(lit),
            Err(e) => return Err(e),
        }
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
        } else if next.get_value() == ":" {
            let token = tokens.expect(":")?;
            Rc::new(AsExpression {
                data_type: Type::read_type(tokens, state)?,
                token,
                expr,
            })
        } else if next.get_value() == "(" {
            panic!("function calls not yet supported");
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
