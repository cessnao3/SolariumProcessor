use std::collections::hash_map::Entry;
use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;
use std::{collections::HashMap, fmt::Display};

use jib::cpu::DataType;

use crate::compiler::CompilingState;
use crate::expressions::parse_expression;
use crate::tokenizer::{Token, TokenError, TokenIter, get_identifier, is_identifier};

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(DataType),
    Pointer(Rc<Self>),
    Array(usize, Rc<Self>),
    Struct(Rc<StructDefinition>),
}

impl Type {
    pub fn read_type(tokens: &mut TokenIter, state: &CompilingState) -> Result<Self, TokenError> {
        let t = tokens.next()?;
        if t.get_value() == "*" {
            Ok(Self::Pointer(Rc::new(Self::read_type(tokens, state)?)))
        } else if t.get_value() == "[" {
            let expr = parse_expression(tokens, state)?;
            if let Some(lit) = expr.simplify() {
                let s = lit.get_value().as_size().unwrap_or_default();
                if s > 0 {
                    Ok(Self::Array(
                        s as usize,
                        Rc::new(Self::read_type(tokens, state)?),
                    ))
                } else {
                    Err(t.into_err("computed size is {s} and is not > 0"))
                }
            } else {
                Err(t.into_err("array must have a constant value"))
            }
        } else if is_identifier(t.get_value()) {
            Ok(Self::Struct(state.get_struct(&t)?))
        } else if let Ok(p) = DataType::try_from(t.get_value().as_ref()) {
            Ok(Self::Primitive(p))
        } else {
            Err(t.into_err("unknown type token found"))
        }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            Self::Primitive(p) => p.byte_size(),
            Self::Pointer(_) => DataType::U32.byte_size(),
            Self::Array(size, t) => size * t.byte_size(),
            Self::Struct(s) => s.fields.values().map(|v| v.dtype.byte_size()).sum(),
        }
    }

    pub fn base_type(&self) -> Option<Type> {
        match self {
            Self::Primitive(p) => Some(Self::Primitive(*p)),
            Self::Pointer(_) => Some(Self::Primitive(DataType::U32)),
            Self::Array(_, t) => Some(t.as_ref().clone()),
            Self::Struct(_) => None,
        }
    }

    pub fn primitive_type(&self) -> Option<DataType> {
        if let Some(Self::Primitive(p)) = self.base_type() {
            Some(p)
        } else {
            None
        }
    }

    pub fn ref_type(&self) -> Type {
        Self::Pointer(Rc::new(self.clone()))
    }

    pub fn alignment(&self) -> usize {
        match self {
            Self::Primitive(p) => p.byte_size(),
            Self::Pointer(_) => DataType::U32.byte_size(),
            Self::Array(_, t) => t.alignment(),
            Self::Struct(_) => DataType::U32.byte_size(),
        }
    }

    pub fn coerce_type(a: DataType, b: DataType) -> DataType {
        a.max(b)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{p}"),
            Self::Array(size, t) => write!(f, "[{size}]{t}"),
            Self::Pointer(t) => write!(f, "*{t}"),
            Self::Struct(s) => write!(f, "{}", s.name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    name: Token,
    fields: HashMap<Rc<str>, StructField>,
}

impl StructDefinition {
    pub fn read_definition(
        tokens: &mut TokenIter,
        state: &CompilingState,
    ) -> Result<Self, TokenError> {
        tokens.expect("struct")?;
        let name = tokens.next()?;
        get_identifier(&name)?;

        let mut s = Self {
            name,
            fields: HashMap::new(),
        };

        tokens.expect("{")?;
        let mut offset = 0;

        while !tokens.expect_peek("}") {
            let field_token = tokens.next()?;
            let field_name = get_identifier(&field_token)?;
            tokens.expect(":")?;

            let dtype = Type::read_type(tokens, state)?;
            let dtype_size = dtype.byte_size();
            tokens.expect(";")?;

            match s.fields.entry(field_name) {
                Entry::Vacant(e) => e.insert(StructField { offset, dtype }),
                Entry::Occupied(_) => {
                    return Err(field_token.into_err("field with name already exists"));
                }
            };

            offset += dtype_size;
        }

        tokens.expect("}")?;

        Ok(s)
    }

    pub fn get_token(&self) -> &Token {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub offset: usize,
    pub dtype: Type,
}

#[derive(Debug, Clone)]
pub struct UnknownPrimitiveError {
    token: Token,
}

impl fmt::Display for UnknownPrimitiveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "unable to find matching primitive type from \"{}\"",
            self.token.get_value()
        )
    }
}

impl From<UnknownPrimitiveError> for TokenError {
    fn from(value: UnknownPrimitiveError) -> Self {
        let msg = value.to_string();
        value.token.into_err(msg)
    }
}

impl TryFrom<TokenIter<'_>> for DataType {
    type Error = TokenError;

    fn try_from(value: TokenIter) -> Result<Self, Self::Error> {
        let mut value = value;

        let first = value.next()?;

        if value.peek().is_some() {
            Err(TokenError {
                token: None,
                msg: "UNKNOWN!".to_owned(),
            })
        } else {
            Ok(DataType::try_from(first)?)
        }
    }
}

impl TryFrom<Token> for DataType {
    type Error = UnknownPrimitiveError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        static PRIMITIVE_MAP: LazyLock<HashMap<String, DataType>> =
            LazyLock::new(|| DataType::ALL.iter().map(|v| (v.to_string(), *v)).collect());

        if let Some(p) = PRIMITIVE_MAP.get(value.get_value()) {
            Ok(*p)
        } else {
            Err(UnknownPrimitiveError { token: value })
        }
    }
}
