use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
    sync::LazyLock,
};

use jib::cpu::{DataType, Register, convert_types};
use jib_asm::{
    ArgumentType, AsmToken, Instruction, OpAdd, OpBand, OpBool, OpBor, OpBxor, OpCall, OpConv,
    OpCopy, OpDiv, OpLd, OpLdi, OpLdn, OpMul, OpPopr, OpPush, OpSav, OpSub, OpTeq, OpTg, OpTge,
    OpTl, OpTle, OpTneq,
};

use crate::{
    TokenError,
    compiler::{CompilingState, Statement},
    literals::{Literal, LiteralValue},
    tokenizer::{Token, TokenIter, get_identifier},
    typing::{StructDefinition, StructField, Type},
    utilities::{MemcpyStatement, load_to_register},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        //let

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperation {
    Arithmetic(BinaryArithmeticOperation),
    Assignment,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Arithmetic(op) => write!(f, "{op}"),
            Self::Assignment => write!(f, "="),
        }
    }
}

impl BinaryOperation {
    pub const ALL: &[BinaryOperation] = &Self::get_all();
    const DISTINCT_VALS: &[BinaryOperation] = &[BinaryOperation::Assignment];

    const LEN_VAL: usize =
        BinaryArithmeticOperation::ALL.len() + BinaryOperation::DISTINCT_VALS.len();
    const fn get_all() -> [Self; Self::LEN_VAL] {
        let mut vals = [Self::Assignment; Self::LEN_VAL];

        let mut i = 0;
        while i < BinaryArithmeticOperation::ALL.len() {
            vals[i] = Self::Arithmetic(BinaryArithmeticOperation::ALL[i]);
            i += 1;
        }

        i = 0;
        while i < Self::DISTINCT_VALS.len() {
            vals[BinaryArithmeticOperation::ALL.len() + i] = Self::DISTINCT_VALS[i];
            i += 1;
        }

        vals
    }

    pub const fn get_priority(&self) -> i32 {
        match self {
            Self::Arithmetic(op) => op.get_priority(),
            Self::Assignment => 100,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryArithmeticOperation {
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

impl BinaryArithmeticOperation {
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

    pub const fn get_priority(&self) -> i32 {
        match self {
            Self::Plus => -5,
            Self::Minus => -5,
            Self::Product => -10,
            Self::Divide => -10,
            Self::And => 0,
            Self::Or => 0,
            Self::Equals => 0,
            Self::NotEquals => 0,
            Self::Greater => 0,
            Self::GreaterEqual => 0,
            Self::Less => 0,
            Self::LessEqual => 0,
            Self::BitAnd => 0,
            Self::BitOr => 0,
            Self::BitXor => 0,
        }
    }
}

impl Display for BinaryArithmeticOperation {
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
pub struct BinaryArithmeticExpression {
    token: Token,
    operation: BinaryArithmeticOperation,
    lhs: Rc<dyn Expression>,
    rhs: Rc<dyn Expression>,
}

impl BinaryArithmeticExpression {
    pub fn new(
        token: Token,
        op: BinaryArithmeticOperation,
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

impl Expression for BinaryArithmeticExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Ok(t) = self.lhs.get_type()
            && t.is_pointer()
        {
            Ok(t)
        } else if let Ok(t) = self.rhs.get_type()
            && t.is_pointer()
        {
            Ok(t)
        } else if let Some(Type::Primitive(a)) = self.lhs.get_type()?.base_type() {
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

            let type_a = self.lhs.get_type()?;
            let type_b = self.rhs.get_type()?;

            let pointer_instructions = if self.operation == BinaryArithmeticOperation::Plus
                || self.operation == BinaryArithmeticOperation::Minus
            {
                if type_a.is_pointer() && type_b.is_pointer() {
                    Err(self
                        .get_token()
                        .clone()
                        .into_err("cannot add one pointer to another pointer directly"))
                } else {
                    fn get_base_size(t: &Type) -> Option<usize> {
                        match t {
                            Type::Pointer(base) => Some(base.byte_size()),
                            _ => None,
                        }
                    }

                    fn generate_ptr_mul_asm(
                        size: usize,
                        incr_val: Register,
                        spare: Register,
                    ) -> Result<Vec<AsmToken>, TokenError> {
                        let mut insts = vec![AsmToken::OperationLiteral(Box::new(OpPush::new(
                            spare.into(),
                        )))];

                        if size <= u16::MAX as usize {
                            let reg_bt = ArgumentType::new(spare, DataType::U16);
                            insts.push(AsmToken::OperationLiteral(Box::new(OpLdi::new(
                                reg_bt,
                                size as u16,
                            ))));
                        } else {
                            let reg_bt = ArgumentType::new(spare, DataType::U32);
                            insts.push(AsmToken::OperationLiteral(Box::new(OpLdn::new(reg_bt))));
                            insts.push(AsmToken::Literal4(size as u32));
                            insts.push(AsmToken::AlignInstruction);
                        }
                        insts.push(AsmToken::OperationLiteral(Box::new(OpMul::new(
                            ArgumentType::new(incr_val, DataType::I32),
                            spare.into(),
                            incr_val.into(),
                        ))));
                        insts.push(AsmToken::OperationLiteral(Box::new(OpPopr::new(
                            spare.into(),
                        ))));

                        Ok(insts)
                    }

                    if let Some(sa) = get_base_size(&type_a) {
                        generate_ptr_mul_asm(sa, reg_b, reg_a)
                    } else if let Some(sb) = get_base_size(&type_b) {
                        generate_ptr_mul_asm(sb, reg_a, reg_b)
                    } else {
                        Ok(Vec::new())
                    }
                }?
            } else {
                Vec::new()
            };

            asm.extend(
                pointer_instructions
                    .into_iter()
                    .map(|x| self.token.to_asm(x)),
            );

            let reg_type = ArgumentType::new(reg.reg, dt);

            let op_instructions: Vec<Box<dyn Instruction>> = match self.operation {
                BinaryArithmeticOperation::NotEquals => {
                    vec![Box::new(OpTneq::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Equals => {
                    vec![Box::new(OpTeq::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Greater => {
                    vec![Box::new(OpTg::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::GreaterEqual => {
                    vec![Box::new(OpTge::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Less => {
                    vec![Box::new(OpTl::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::LessEqual => {
                    vec![Box::new(OpTle::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Plus => {
                    vec![Box::new(OpAdd::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Minus => {
                    vec![Box::new(OpSub::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Product => {
                    vec![Box::new(OpMul::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Divide => {
                    vec![Box::new(OpDiv::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::And => {
                    vec![
                        Box::new(OpBool::new(reg_a.into(), reg_a.into())),
                        Box::new(OpBool::new(reg_b.into(), reg_b.into())),
                        Box::new(OpBand::new(reg_type, reg_a.into(), reg_b.into())),
                    ]
                }
                BinaryArithmeticOperation::Or => {
                    vec![
                        Box::new(OpBor::new(reg_type, reg_a.into(), reg_b.into())),
                        Box::new(OpBool::new(reg_b.into(), reg_b.into())),
                    ]
                }
                BinaryArithmeticOperation::BitAnd => {
                    vec![Box::new(OpBand::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::BitOr => {
                    vec![Box::new(OpBor::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::BitXor => {
                    vec![Box::new(OpBxor::new(reg_type, reg_a.into(), reg_b.into()))]
                }
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

impl Display for BinaryArithmeticExpression {
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

    fn simplify(&self) -> Option<Literal> {
        if let Some(dt) = self.data_type.primitive_type() {
            let lit = self.expr.simplify()?;
            let val = lit.get_value();
            let res = LiteralValue::from_u32(convert_types(val.as_u32(), val.get_dtype(), dt), dt);

            Some(Literal::new(lit.get_token().clone(), res))
        } else {
            None
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
        asm.push(self.token.to_asm(AsmToken::Comment(format!(
            "assignment of {} with {}",
            self.addr_expr, self.val_expr
        ))));

        let val_def = reg;
        let addr_def = val_def.increment_token(tok)?;

        if let Some(dest_dtype) = self.addr_expr.get_type()?.primitive_type() {
            let val_dtype = self.val_expr.get_primitive_type()?;

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
        } else if let Ok(val_type) = self.val_expr.get_type()
            && let Ok(target_type) = self.addr_expr.get_type()
        {
            if val_type == target_type {
                asm.extend(self.addr_expr.load_address_to_register(addr_def)?);
                asm.extend(self.val_expr.load_address_to_register(val_def)?);

                let mem = MemcpyStatement::new(
                    self.token.clone(),
                    val_def.reg,
                    addr_def.reg,
                    val_type.byte_size(),
                );

                asm.extend(mem.get_exec_code()?);

                Ok(asm)
            } else {
                Err(tok.clone().into_err(format!(
                    "mismatch of types assigning {val_type} to {target_type}"
                )))
            }
        } else {
            Err(tok
                .clone()
                .into_err("unable to determine valid types for input values"))
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
                .to_asm(AsmToken::Comment(format!("calling {}", self.func))),
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpPush::new(
                    Register::ArgumentBase.into(),
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

        if self.args.len() != func.parameters.len() {
            return Err(self.token.clone().into_err(format!(
                "expected {} arguments for {}, found {}",
                func.parameters.len(),
                self.func,
                self.args.len()
            )));
        }

        for (p, e) in func.parameters.iter().zip(self.args.iter()) {
            if let Some(p_name) = p.name.clone() {
                let var = Rc::new(LocalVariable::new(
                    p_name,
                    p.dtype.clone(),
                    reg.reg,
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
            } else {
                return Err(self
                    .token
                    .clone()
                    .into_err("unable to find name for parameter at"));
            }
        }

        asm.extend(
            self.token
                .to_asm_iter(load_to_register(next_load.reg, func.param_size() as u32)), // TODO - Add return value here?
        );

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(Register::StackPointer, DataType::U32),
                Register::StackPointer.into(),
                next_load.reg.into(),
            ))),
            AsmToken::OperationLiteral(Box::new(OpCopy::new(
                Register::ArgumentBase.into(),
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
            AsmToken::OperationLiteral(Box::new(OpPopr::new(Register::ArgumentBase.into()))),
        ]));

        Ok(asm)
    }
}

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

fn parse_expression_without_binary_expressions(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
    let first = tokens.next()?;

    let mut expr: Rc<dyn Expression> = if let Some(op) = UNARY_STR.get(first.get_value()) {
        Rc::new(UnaryExpression::new(
            first,
            *op,
            parse_expression_without_binary_expressions(tokens, state)?,
        ))
    } else if first.get_value() == "*" {
        Rc::new(DereferenceExpression {
            token: first,
            base: parse_expression_without_binary_expressions(tokens, state)?,
        })
    } else if first.get_value() == "&" {
        Rc::new(AddressOfExpression {
            token: first,
            base: parse_expression_without_binary_expressions(tokens, state)?,
        })
    } else if first.get_value() == "(" {
        let inner = parse_expression(tokens, state)?;
        tokens.expect(")")?;
        inner
    } else if get_identifier(&first).is_ok() {
        state.get_variable(&first)?.clone()
    } else if first.get_value().starts_with('"') {
        state.add_string_literal(&first)?
    } else {
        Rc::new(Literal::try_from(first.clone())?)
    };

    while let Some(next) = tokens.peek() {
        if BINARY_STR.contains_key(next.get_value()) {
            break;
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
        } else if next.get_value() == "(" {
            let call_val = tokens.expect("(")?;

            let mut args = Vec::new();

            while !tokens.expect_peek(")") {
                args.push(parse_expression(tokens, state)?);

                if tokens.expect_peek(",") {
                    tokens.expect(",")?;
                }
            }

            tokens.expect(")")?;

            expr = Rc::new(FunctionCallExpression {
                token: call_val,
                func: expr,
                args,
            });
        } else {
            break;
        }
    }

    Ok(expr)
}

enum BinaryExpressionMatching {
    Expression(Rc<dyn Expression>),
    BinaryOperation(BinaryOperation, Token),
}

fn process_binary_expressions(
    exprs: &[BinaryExpressionMatching],
) -> Result<Rc<dyn Expression>, TokenError> {
    if exprs.len() == 1
        && let Some(BinaryExpressionMatching::Expression(e)) = exprs.first()
    {
        Ok(e.clone())
    } else if !exprs.is_empty() && exprs.len() % 2 == 1 {
        let priorities: HashMap<BinaryOperation, i32> =
            HashMap::from_iter(BinaryOperation::ALL.iter().map(|x| (*x, x.get_priority())));

        let mut highest_priority = None;

        for (i, val) in exprs.iter().enumerate() {
            if let BinaryExpressionMatching::BinaryOperation(op, _) = val {
                let priority = priorities.get(op).copied().unwrap_or(0);

                if let Some((highest, _)) = highest_priority
                    && priority > highest
                {
                    highest_priority = Some((priority, i));
                } else if highest_priority.is_none() {
                    highest_priority = Some((priority, i));
                }
            }
        }

        if let Some((_, index)) = highest_priority
            && let BinaryExpressionMatching::BinaryOperation(op, token) = &exprs[index]
        {
            let a = process_binary_expressions(&exprs[..index])?;
            let b = process_binary_expressions(&exprs[(index + 1)..])?;

            match *op {
                BinaryOperation::Arithmetic(opa) => Ok(Rc::new(BinaryArithmeticExpression::new(
                    token.clone(),
                    opa,
                    a,
                    b,
                ))),
                BinaryOperation::Assignment => Ok(Rc::new(AssignmentExpression {
                    token: token.clone(),
                    addr_expr: a,
                    val_expr: b,
                })),
            }
        } else {
            Err(TokenError {
                token: None,
                msg: "unable to determine proper location for expressions".to_string(),
            })
        }
    } else {
        Err(TokenError {
            token: None,
            msg: "no binary expressions provided to process".to_string(),
        })
    }
}

pub fn parse_expression(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
    let mut main_expressions: Vec<BinaryExpressionMatching> =
        vec![BinaryExpressionMatching::Expression(
            parse_expression_without_binary_expressions(tokens, state)?,
        )];

    while let Some(next) = tokens.peek()
        && let Some(op) = BINARY_STR.get(next.get_value())
    {
        main_expressions.push(BinaryExpressionMatching::BinaryOperation(
            *op,
            tokens.next()?,
        ));
        main_expressions.push(BinaryExpressionMatching::Expression(
            parse_expression_without_binary_expressions(tokens, state)?,
        ));
    }

    let expr = process_binary_expressions(&main_expressions)?;

    if let Some(lit) = expr.simplify() {
        Ok(Rc::new(lit))
    } else {
        Ok(expr)
    }
}
