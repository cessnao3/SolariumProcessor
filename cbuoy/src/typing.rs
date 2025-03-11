use std::collections::hash_map::Entry;
use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;
use std::{collections::HashMap, fmt::Display};

use jib::cpu::DataType;

use crate::tokenizer::{Token, TokenError, TokenIter};

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(DataType),
    Pointer(Rc<Self>),
    Array(usize, Rc<Self>),
    Struct(Rc<str>, HashMap<Rc<str>, StructField>),
}

impl Type {
    pub fn byte_size(&self) -> usize {
        match self {
            Self::Primitive(p) => p.byte_size(),
            Self::Pointer(_) => DataType::U32.byte_size(),
            Self::Array(size, t) => size * t.byte_size(),
            Self::Struct(_, fields) => fields.values().map(|v| v.dtype.byte_size()).sum(),
        }
    }

    pub fn base_type(&self) -> Option<Type> {
        match self {
            Self::Primitive(p) => Some(Self::Primitive(*p)),
            Self::Pointer(_) => Some(Self::Primitive(DataType::U32)),
            Self::Array(_, t) => Some(t.as_ref().clone()),
            Self::Struct(_, _) => None,
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
            Self::Struct(_, _) => DataType::U32.byte_size(),
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
            Self::Struct(name, _) => write!(f, "{name}"),
        }
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
