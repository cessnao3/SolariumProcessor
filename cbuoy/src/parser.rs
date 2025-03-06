use jib::cpu::{DataType, OperationError, OperatorManager};
use jib_asm::{AsmToken, AsmTokenLoc, LocationInfo, argument::ArgumentType};
use regex::Regex;

use crate::{
    tokenizer::{Token, TokenError, TokenIter, TokenLocation, get_identifier, tokenize},
    typing::{PrimitiveType, Type},
};
use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    fmt::{Debug, Display},
    rc::Rc,
    sync::LazyLock,
};

pub fn parse(s: &str) -> Result<Vec<AsmTokenLoc>, TokenError> {
    let mut state = CompilingState::default();
    let tokens = tokenize(s)?;
    let mut token_iter = TokenIter::from(&tokens);

    while let Some(next) = token_iter.peek().map(|v| v.get_value().to_string()) {
        if next == "global" {
            parse_global_statement(&mut token_iter, &mut state)?;
        } else {
            return Err(token_iter
                .next()?
                .clone()
                .into_err("unknown token provided".to_owned()));
        }
    }

    fn blank_token_loc(tok: AsmToken) -> AsmTokenLoc {
        AsmTokenLoc {
            tok,
            loc: LocationInfo::default(),
        }
    }

    let init_asm = vec![
        AsmToken::ChangeAddress(0x2000),
        AsmToken::Comment("Initialization".into()),
    ];

    let mut asm: Vec<_> = init_asm
        .into_iter()
        .map(|tok| blank_token_loc(tok))
        .collect();

    asm.extend_from_slice(&state.asm_init);
    asm.push(blank_token_loc(AsmToken::Comment("Static".into())));
    asm.extend_from_slice(&state.asm_static);
    asm.push(blank_token_loc(AsmToken::Comment("Functions".into())));
    asm.extend_from_slice(&state.asm_func);

    Ok(asm)
}

trait Expression: Debug + Display {
    fn get_token(&self) -> &Token;

    fn get_type(&self) -> Result<Type, TokenError>;

    fn load_value_to_register(
        &self,
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError>;

    fn simplify(&self) -> Option<Literal>;
}

#[derive(Debug, Clone)]
struct Literal {
    token: Rc<Token>,
    value: LiteralValue,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum LiteralValue {
    U8(u8),
    U16(u16),
    U32(u32),
    I8(i8),
    I16(i16),
    I32(i32),
    F32(f32),
}

static OPERATIONS: LazyLock<OperatorManager> = LazyLock::new(|| OperatorManager::default());

impl LiteralValue {
    pub fn get_dtype(&self) -> PrimitiveType {
        match &self {
            Self::U8(_) => PrimitiveType::U8,
            Self::U16(_) => PrimitiveType::U16,
            Self::U32(_) => PrimitiveType::U32,
            Self::I8(_) => PrimitiveType::I8,
            Self::I16(_) => PrimitiveType::I16,
            Self::I32(_) => PrimitiveType::I32,
            Self::F32(_) => PrimitiveType::F32,
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

    pub fn from_u32(x: u32, dtype: PrimitiveType) -> Self {
        match dtype {
            PrimitiveType::U8 => Self::U8(x as u8),
            PrimitiveType::U16 => Self::U16(x as u16),
            PrimitiveType::U32 => Self::U32(x as u32),
            PrimitiveType::I8 => Self::I8((x as i32) as i8),
            PrimitiveType::I16 => Self::I16((x as i32) as i16),
            PrimitiveType::I32 => Self::I32(x as i32),
            PrimitiveType::F32 => Self::F32(f32::from_bits(x)),
        }
    }

    pub fn unary(a: Self, op: UnaryOperation) -> Result<Self, OperationError> {
        let x = a.as_u32();
        let dt = a.get_dtype().into();

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

        Ok(Self::from_u32(res, dt.into()))
    }

    pub fn operation(lhs: Self, rhs: Self, op: BinaryOperation) -> Result<Self, OperationError> {
        let dt: DataType = (if lhs.get_dtype() > rhs.get_dtype() {
            lhs.get_dtype()
        } else {
            rhs.get_dtype()
        })
        .into();

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

        Ok(Self::from_u32(res, dt.into()))
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
        reg: jib::cpu::Register,
        _spare: jib::cpu::Register,
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
            reg,
            self.value.get_dtype().into(),
        ))));

        Ok([op_load, op_lit]
            .map(|x| self.get_token().to_asm(x))
            .to_vec())
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
            Regex::new(r"^(((?<inum>\d+)(?<itype>[ui](8|(16)|(32)))?)|((?<fnum>(\d+(\.\d*))|(\.\d+))f32)|(?<f32>\d*\.\d+))$").unwrap()
        });

        let res = if let Some(m) = LITERAL_REGEX.captures(value.get_value()) {
            if let Some(inum) = m.name("inum") {
                if let Some(itype) = m.name("itype") {
                    let num = inum.as_str();
                    let val = match itype.as_str() {
                        "u8" => num.parse().map_or(None, |x| Some(LiteralValue::U8(x))),
                        "u16" => num.parse().map_or(None, |x| Some(LiteralValue::U16(x))),
                        "u32" => num.parse().map_or(None, |x| Some(LiteralValue::U32(x))),
                        "i8" => num.parse().map_or(None, |x| Some(LiteralValue::I8(x))),
                        "i16" => num.parse().map_or(None, |x| Some(LiteralValue::I16(x))),
                        "i32" => num.parse().map_or(None, |x| Some(LiteralValue::I32(x))),
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
                    match inum.as_str().parse() {
                        Ok(x) => Ok(LiteralValue::I32(x)),
                        Err(_) => Err(value
                            .clone()
                            .into_err(format!("unmable to get i32 value from {}", inum.as_str()))),
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
            token: value.into(),
            value: res,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOperation {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnaryOperation {
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
struct UnaryExpression {
    token: Token,
    op: UnaryOperation,
    base: Rc<dyn Expression>,
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
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        panic!()
    }

    fn simplify(&self) -> Option<Literal> {
        LiteralValue::unary(self.base.simplify()?.value, self.op).map_or(None, |x| {
            Some(Literal {
                token: self.token.clone().into(),
                value: x,
            })
        })
    }
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.base)
    }
}

#[derive(Debug, Clone)]
struct BinaryExpression {
    token: Rc<Token>,
    operation: BinaryOperation,
    rhs: Rc<dyn Expression>,
    lhs: Rc<dyn Expression>,
}

impl Expression for BinaryExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Some(Type::Primitive(a)) = self.lhs.get_type()?.base_type() {
            if let Some(Type::Primitive(b)) = self.rhs.get_type()?.base_type() {
                Ok(Type::Primitive(PrimitiveType::coerced(a, b)))
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

        if let Ok(v) = LiteralValue::operation(lhs_val.value, rhs_val.value, self.operation) {
            Some(Literal {
                token: self.token.clone(),
                value: v,
            })
        } else {
            None
        }
    }

    fn load_value_to_register(
        &self,
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        todo!()
    }
}

impl Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.operation, self.rhs)
    }
}

fn parse_expression(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
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
    static KEYWORDS: LazyLock<HashSet<String>> = LazyLock::new(|| HashSet::new()); // TODO - Update with actual keywords // TODO - Update tokenizer regex with keywords and expressions

    let first = tokens.next()?;

    let expr: Rc<dyn Expression> = if let Some(op) = UNARY_STR.get(first.get_value()) {
        Rc::new(UnaryExpression {
            base: parse_expression(tokens, state)?,
            token: first,
            op: *op,
        })
    } else if let Ok(id) = get_identifier(&first) {
        todo!()
    } else if let Ok(lit) = Literal::try_from(first.clone()) {
        Rc::new(lit)
    } else {
        panic!("unknown value {}", first.get_value());
    };

    if let Some(next) = tokens.peek() {
        if let Some(op) = BINARY_STR.get(next.get_value()) {
            let op_token = tokens.next()?;
            Ok(Rc::new(BinaryExpression {
                lhs: expr,
                rhs: parse_expression(tokens, state)?,
                token: op_token.into(),
                operation: *op,
            }))
        } else {
            Ok(expr)
        }
    } else {
        Ok(expr)
    }
}

fn parse_global_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<(), TokenError> {
    tokens.expect("global")?;
    let name_token = tokens.next()?;

    tokens.expect(":")?;

    let mut type_tokens = Vec::new();
    while let Some(t) = tokens.next_if(|s| s != ";" && s != "=") {
        type_tokens.push(t.to_owned());
    }

    let init_expr = if tokens.expect_peek("=") {
        tokens.next()?;
        let mut expr_tokens = Vec::new();
        while let Some(t) = tokens.next_if(|s| s != ";") {
            expr_tokens.push(t);
        }
        Some(parse_expression(&mut TokenIter::from(&expr_tokens), state)?)
    } else {
        None
    };

    tokens.expect(";")?;

    let dtype = match PrimitiveType::try_from(type_tokens.first().unwrap().to_owned()) {
        Ok(t) => Type::Primitive(t),
        Err(e) => {
            return Err(TokenError {
                token: Some(type_tokens.first().unwrap().to_owned()),
                msg: e.to_string(),
            });
        }
    };

    state.add_global_var(name_token, dtype, init_expr)
}

trait Variable: Debug {
    fn get_token(&self) -> &Token;
    fn load_to_register(
        &self,
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;

    fn load_address_to_register(
        &self,
        reg: jib::cpu::Register,
        spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;

    fn get_name(&self) -> &str {
        self.get_token().get_value()
    }
}

#[derive(Debug, Clone)]
struct GlobalVariable {
    name: Token,
    dtype: Type,
    id: usize,
    label: Rc<str>,
}

impl GlobalVariable {
    pub fn access_label(&self) -> &str {
        self.label.as_ref()
    }

    pub fn get_name(&self) -> &str {
        self.name.get_value()
    }

    fn to_token_loc<T: IntoIterator<Item = AsmToken>>(
        &self,
        it: T,
    ) -> impl Iterator<Item = AsmTokenLoc> {
        it.into_iter().map(|x| self.name.to_asm(x))
    }
}

impl Variable for GlobalVariable {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn load_to_register(
        &self,
        reg: jib::cpu::Register,
        _spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        if let Type::Primitive(p) = self.dtype {
            let vals = [
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLd::new(
                    ArgumentType::new(reg, p.into()),
                    reg.into(),
                ))),
            ];
            Ok(self.to_token_loc(vals.into_iter()).collect())
        } else {
            Err(self.name.clone().into_err(format!(
                "unable to load value for {} to register",
                self.dtype
            )))
        }
    }

    fn load_address_to_register(
        &self,
        reg: jib::cpu::Register,
        _spare: jib::cpu::Register,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .to_token_loc([
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
            ])
            .collect())
    }
}

#[derive(Debug, Default)]
struct CompilingState {
    asm_static: Vec<jib_asm::AsmTokenLoc>,
    asm_init: Vec<jib_asm::AsmTokenLoc>,
    asm_func: Vec<jib_asm::AsmTokenLoc>,
    global_variables: HashMap<Rc<str>, Rc<dyn Variable>>,
    current_id: usize,
}

impl CompilingState {
    pub fn get_next_id(&mut self) -> usize {
        let current = self.current_id;
        self.current_id += 1;
        current
    }

    pub fn add_global_var(
        &mut self,
        name: Token,
        dtype: Type,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<(), TokenError> {
        let id = self.get_next_id();
        let name_str: Rc<str> = get_identifier(&name)?.into();

        let var = Rc::new(GlobalVariable {
            name: name.clone(),
            dtype,
            id,
            label: format!("global_variable_{}_{}", id, name).into(),
        });

        self.asm_static
            .push(name.to_asm(AsmToken::CreateLabel(var.access_label().into())));

        let init_literal = if let Some(expr) = &init_expr {
            if let Some(simplified) = expr.simplify() {
                Some(simplified.value.as_asm_literal())
            } else {
                None
            }
        } else {
            None
        };

        let literal_value = init_expr
            .clone()
            .map_or(None, |x| x.simplify())
            .map(|x| x.value.as_u32());

        let needs_init = literal_value.is_none() && init_expr.is_some();
        let literal_value = literal_value.unwrap_or(0);

        if let Some(a) = init_literal.clone() {
            self.asm_static.push(name.to_asm(a));
        } else {
            let mut needed_size = var.dtype.byte_size();
            while needed_size > 0 {
                let tok = if needed_size >= 4 {
                    needed_size -= 4;
                    AsmToken::Literal4(literal_value)
                } else if needed_size >= 2 {
                    needed_size -= 2;
                    AsmToken::Literal2(literal_value as u16)
                } else {
                    needed_size -= 1;
                    AsmToken::Literal1(literal_value as u8)
                };

                self.asm_static.push(name.to_asm(tok));
            }
        }

        if init_expr.is_some() && init_literal.is_none() {
            todo!();
        }

        match self.global_variables.entry(name_str.clone()) {
            Entry::Occupied(_) => {
                return Err(name.into_err(format!(
                    "global variable already exists with name \"{name_str}\""
                )));
            }
            Entry::Vacant(e) => e.insert(var),
        };

        // TODO - Check if the expression can be converted to the raw byte values and loaded directly
        // TODO - Add support for initializing expressions

        Ok(())
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Variable>, TokenError> {
        todo!()
    }
}

struct VariableStatement {
    name: String,
    data_type: Type,
    loc: TokenLocation,
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{Token, TokenLocation};

    use super::{Literal, LiteralValue};

    #[test]
    fn test_literal_parsing() {
        let tokens = [
            ("0u32", Some(LiteralValue::U32(0))),
            ("23834i8", None),
            ("1i8", Some(LiteralValue::I8(1))),
            ("23834i16", Some(LiteralValue::I16(23834))),
            ("394i23", None),
            ("38905u32", Some(LiteralValue::U32(38905))),
            ("389u35", None),
            ("3.234i8", None),
            ("3.2f32", Some(LiteralValue::F32(3.2))),
            (".3f32", Some(LiteralValue::F32(0.3))),
            ("32.34f32", Some(LiteralValue::F32(32.34))),
            ("32.34", Some(LiteralValue::F32(32.34))),
            ("394", Some(LiteralValue::I32(394))),
        ];

        for (s, expected) in tokens {
            let t = Token::new(s, TokenLocation { line: 0, column: 0 });
            let lit: Result<Literal, _> = t.try_into();

            println!("{:?} <=> {:?}", lit, expected);
            assert_eq!(lit.is_ok(), expected.is_some());
            if let Ok(v) = lit {
                if let Some(ev) = expected {
                    assert_eq!(v.value.get_dtype(), ev.get_dtype());
                    assert_eq!(v.value.as_u32(), ev.as_u32());
                } else {
                    panic!("unexpected value mismatch");
                }
            }
        }
    }
}
