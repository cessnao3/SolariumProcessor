use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
    sync::LazyLock,
};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, Instruction, OpAdd, OpCall, OpConv, OpCopy, OpLd, OpPopr, OpPush,
    OpSav, OpSub, OpTeq, OpTneq,
};

use crate::{
    TokenError,
    compiler::{CompilingState, Statement},
    functions::FunctionDefinition,
    literals::{Literal, LiteralValue},
    tokenizer::{Token, TokenIter, get_identifier},
    typing::{Function, StructDefinition, StructField, Type},
    utilities::load_to_register,
    variables::LocalVariable,
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
}

impl Default for RegisterDef {
    fn default() -> Self {
        Self::new(Self::FIRST_USEABLE).unwrap()
    }
}

impl Display for RegisterDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reg)
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
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        Err(self.get_token().clone().into_err(format!(
            "expression does not have a mappable address in memory - cannot load to {reg}"
        )))
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

#[derive(Debug, Clone)]
struct DereferenceExpression {
    token: Token,
    base: Rc<dyn Expression>,
}

impl Expression for DereferenceExpression {
    fn get_type(&self) -> Result<Type, TokenError> {
        if let Type::Pointer(dt) = self.base.get_type()? {
            Ok(dt.as_ref().clone())
        } else {
            Err(self
                .base
                .get_token()
                .clone()
                .into_err("cannot dereference a non-pointer type"))
        }
    }

    fn get_token(&self) -> &Token {
        &self.token
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        self.base.load_value_to_register(reg)
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let mut asm = self.base.load_value_to_register(reg)?;
        let dt = self.get_primitive_type()?;

        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                    ArgumentType::new(reg.reg, dt),
                    reg.reg.into(),
                )))),
        );

        Ok(asm)
    }
}

impl Display for DereferenceExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*{}", self.base)
    }
}

#[derive(Debug, Clone)]
struct AddressOfExpression {
    token: Token,
    base: Rc<dyn Expression>,
}

impl Expression for AddressOfExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(Type::Pointer(Box::new(self.base.get_type()?)))
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        self.base.load_address_to_register(reg)
    }
}

impl Display for AddressOfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*{}", self.base)
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
            if let Some(reg_next) = reg.increment() {
                asm.extend_from_slice(&self.rhs.load_value_to_register(reg_next)?);
                reg_b = reg_next.reg;
            } else {
                asm.push(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                    OpPush::new(reg.reg.into()),
                ))));
                asm.extend_from_slice(&self.rhs.load_value_to_register(reg)?);
                asm.push(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                    OpCopy::new(reg.spare.into(), reg.reg.into()),
                ))));
                asm.push(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                    OpPopr::new(reg.reg.into()),
                ))));
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

            let rhs_type = self.rhs.get_type()?;
            let lhs_type = self.lhs.get_type()?;

            if rhs_type.is_pointer() && lhs_type.is_pointer() {
                return Err(self
                    .get_token()
                    .clone()
                    .into_err("cannot add one pointer to another pointer directly"));
            } else if rhs_type.is_pointer() {
                // pointer arithmetic?
            } else if lhs_type.is_pointer() {
                // pointer arithmetic?
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
                self.get_token()
                    .to_asm_iter(op_instructions.into_iter().map(AsmToken::OperationLiteral)),
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

#[derive(Debug, Clone)]
struct DotExpression {
    token: Token,
    field: Token,
    s_expression: Rc<dyn Expression>,
}

impl DotExpression {
    fn get_struct(&self) -> Result<Rc<StructDefinition>, TokenError> {
        if let Type::Struct(s) = self.s_expression.get_type()? {
            Ok(s)
        } else {
            Err(self
                .token
                .clone()
                .into_err("left-hand expression to dot is not a structure"))
        }
    }

    fn get_field(&self) -> Result<Rc<StructField>, TokenError> {
        let st = self.get_struct()?;
        if let Some(f) = st.get_field(self.field.get_value()) {
            Ok(f)
        } else {
            Err(self.field.clone().into_err(format!(
                "no field with name found for structure type {}",
                st.get_name()
            )))
        }
    }
}

impl Expression for DotExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.get_field()?.dtype.clone())
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let mut asm = self.s_expression.load_address_to_register(reg)?;
        asm.extend(
            self.token
                .to_asm_iter(load_to_register(reg.spare, self.get_field()?.offset as u32)),
        );
        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    ArgumentType::new(reg.reg, DataType::U32),
                    reg.reg.into(),
                    reg.spare.into(),
                )))),
        );
        Ok(asm)
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let mut asm = self.load_address_to_register(reg)?;
        let dt = self.get_primitive_type()?;

        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                    ArgumentType::new(reg.reg, dt),
                    reg.reg.into(),
                )))),
        );

        Ok(asm)
    }
}

impl Display for DotExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.s_expression, self.field.get_value())
    }
}

#[derive(Debug, Clone)]
struct AssignmentExpression {
    token: Token,
    addr_expr: Rc<dyn Expression>,
    val_expr: Rc<dyn Expression>,
}

impl Display for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.addr_expr, self.val_expr)
    }
}

impl Expression for AssignmentExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        self.addr_expr.get_type()
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let tok = self.addr_expr.get_token();

        let mut asm = Vec::new();
        asm.push(self.token.to_asm(AsmToken::CreateLabel(format!(
            "assignment_at_{}_{}",
            self.token.get_loc().line,
            self.token.get_loc().column
        ))));
        asm.push(self.token.to_asm(AsmToken::Comment(format!(
            "assignment) of {} to {}",
            self.addr_expr, self.val_expr
        ))));

        if let Some(dest_dtype) = self.addr_expr.get_type()?.primitive_type() {
            let val_dtype = self.val_expr.get_primitive_type()?;

            let val_def = reg;
            let addr_def = val_def.increment_token(tok)?;

            asm.extend(self.val_expr.load_value_to_register(val_def)?);
            asm.extend(self.addr_expr.load_address_to_register(addr_def)?);

            if val_dtype != dest_dtype {
                asm.push(tok.to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                    ArgumentType::new(val_def.reg, dest_dtype),
                    ArgumentType::new(val_def.reg, val_dtype),
                )))));
            }

            asm.push(tok.to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                ArgumentType::new(addr_def.reg, dest_dtype),
                val_def.reg.into(),
            )))));

            Ok(asm)
        } else {
            Err(tok
                .clone()
                .into_err("currently struct assignment is unsupported"))
        }
    }
}

#[derive(Debug, Clone)]
struct FunctionCallExpression {
    token: Token,
    args: Vec<Rc<dyn Expression>>,
    func: Rc<dyn Expression>,
}

impl Display for FunctionCallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.func)?;
        for p in self.args.iter() {
            write!(f, "{p},")?;
        }
        write!(f, ")")
    }
}

impl Expression for FunctionCallExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Type::Function(f) = self.func.get_type()? {
            Ok(f.return_type
                .clone()
                .unwrap_or(Type::Primitive(DataType::U32)))
        } else {
            Err(self
                .token
                .clone()
                .into_err("cannot call a non-function type"))
        }
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let func = if let Type::Function(f) = self.func.get_type()? {
            f.clone()
        } else {
            return Err(self
                .token
                .clone()
                .into_err("caller value is not a function type"));
        };

        let mut asm = vec![
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpPush::new(
                    Register::ArgumentBase.into(),
                )))),
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpPush::new(
                    RegisterDef::FN_BASE.into(),
                )))),
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                    reg.reg.into(),
                    Register::StackPointer.into(),
                )))),
        ];

        let func_loc = reg.increment_token(&self.token)?;
        asm.extend(self.func.load_value_to_register(func_loc)?);

        let next_load = func_loc.increment_token(&self.token)?;
        let mut current_offset = 0;

        for (p, e) in func.parameters.iter().zip(self.args.iter()) {
            let var = Rc::new(LocalVariable::new(
                self.token.clone(),
                p.dtype.clone(),
                reg.reg.into(),
                current_offset,
                None,
            )?);

            let assign = Rc::new(AssignmentExpression {
                addr_expr: var,
                token: self.token.clone(),
                val_expr: e.clone(),
            });

            asm.extend(assign.load_value_to_register(next_load)?);
            current_offset += p.dtype.byte_size();
        }

        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                    Register::ArgumentBase.into(),
                    reg.reg.into(),
                )))),
        );

        asm.extend(
            self.token
                .to_asm_iter(load_to_register(next_load.reg, func.param_size() as u32)),
        );

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(Register::StackPointer, DataType::U32),
                Register::StackPointer.into(),
                next_load.reg.into(),
            ))),
            AsmToken::OperationLiteral(Box::new(OpCopy::new(
                RegisterDef::FN_BASE.into(),
                reg.reg.into(),
            ))),
            AsmToken::OperationLiteral(Box::new(OpCall::new(func_loc.reg.into()))),
        ]));

        asm.extend(
            self.token
                .to_asm_iter(load_to_register(reg.reg, func.param_size() as u32)),
        );

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpSub::new(
                ArgumentType::new(Register::StackPointer, DataType::U32),
                Register::StackPointer.into(),
                reg.reg.into(),
            ))),
            AsmToken::OperationLiteral(Box::new(OpPopr::new(RegisterDef::FN_BASE.into()))),
            AsmToken::OperationLiteral(Box::new(OpPopr::new(Register::ArgumentBase.into()))),
        ]));

        Ok(asm)
    }
}

pub fn parse_expression(
    tokens: &mut TokenIter,
    state: &CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
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

    let first = tokens.next()?;

    let mut expr: Rc<dyn Expression> = if let Some(op) = UNARY_STR.get(first.get_value()) {
        Rc::new(UnaryExpression::new(
            first,
            *op,
            parse_expression(tokens, state)?,
        ))
    } else if first.get_value() == "*" {
        Rc::new(DereferenceExpression {
            token: first,
            base: parse_expression(tokens, state)?,
        })
    } else if first.get_value() == "&" {
        Rc::new(AddressOfExpression {
            token: first,
            base: parse_expression(tokens, state)?,
        })
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

    loop {
        if let Some(next) = tokens.peek() {
            if let Some(op) = BINARY_STR.get(next.get_value()) {
                let op_token = tokens.next()?;
                expr = Rc::new(BinaryExpression::new(
                    op_token,
                    *op,
                    expr,
                    parse_expression(tokens, state)?,
                ));
            } else if next.get_value() == "." {
                let dot_token = tokens.expect(".")?;
                let ident_token = tokens.next()?;

                expr = Rc::new(DotExpression {
                    field: ident_token,
                    token: dot_token,
                    s_expression: expr,
                })
            } else if next.get_value() == ":" {
                let token = tokens.expect(":")?;
                expr = Rc::new(AsExpression {
                    data_type: Type::read_type(tokens, state)?,
                    token,
                    expr,
                })
            } else if next.get_value() == "=" {
                let token = tokens.expect("=")?;
                expr = Rc::new(AssignmentExpression {
                    token,
                    addr_expr: expr,
                    val_expr: parse_expression(tokens, state)?,
                })
            } else if next.get_value() == "(" {
                let call_val = tokens.expect("(")?;
                tokens.expect(")")?;

                expr = Rc::new(FunctionCallExpression {
                    token: call_val,
                    func: expr,
                    args: Vec::new(),
                });
            } else {
                break;
            }
        } else {
            break;
        }
    }

    if let Some(lit) = expr.simplify() {
        Ok(Rc::new(lit))
    } else {
        Ok(expr)
    }
}
