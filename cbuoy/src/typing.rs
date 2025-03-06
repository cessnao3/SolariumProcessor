use std::collections::hash_map::Entry;
use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;
use std::{collections::HashMap, fmt::Display};

use crate::tokenizer::{Token, TokenError, TokenIter};

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Pointer(Rc<Self>),
    Array(usize, Rc<Self>),
    Struct(Rc<String>, HashMap<String, Rc<Self>>),
}

impl Type {
    pub fn byte_size(&self) -> usize {
        match self {
            Self::Primitive(p) => p.byte_size(),
            Self::Pointer(_) => PrimitiveType::U32.byte_size(),
            Self::Array(size, t) => size * t.byte_size(),
            Self::Struct(_, types) => types.values().map(|v| v.byte_size()).sum(),
        }
    }

    pub fn base_type(&self) -> Option<Type> {
        match self {
            Self::Primitive(p) => Some(Self::Primitive(*p)),
            Self::Pointer(_) => Some(Self::Primitive(PrimitiveType::U32)),
            Self::Array(_, t) => Some(t.as_ref().clone()),
            Self::Struct(_, _) => None,
        }
    }

    pub fn ref_type(&self) -> Type {
        Self::Pointer(Rc::new(self.clone()))
    }

    pub fn alignment(&self) -> usize {
        match self {
            Self::Primitive(p) => p.alignment(),
            Self::Pointer(_) => PrimitiveType::U32.byte_size(),
            Self::Array(_, t) => t.alignment(),
            Self::Struct(_, _) => PrimitiveType::U32.alignment(),
        }
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
struct StructType {
    fields: Vec<Rc<StructField>>,
    field_map: HashMap<Rc<str>, Rc<StructField>>,
}

impl StructType {
    pub fn new<T: IntoIterator<Item = (Token, Type)>>(entries: T) -> Result<Self, TokenError> {
        let mut current_offset = 0;
        let mut fields = Vec::new();
        let mut field_map = HashMap::new();

        for (name, dtype) in entries {
            let alignment = dtype.alignment();
            current_offset += alignment - current_offset % alignment;

            let fname: Rc<str> = name.get_value().into();
            let f = Rc::new(StructField {
                offset: current_offset,
                name: fname.clone(),
                dtype,
            });

            match field_map.entry(fname.clone()) {
                Entry::Occupied(_) => {
                    return Err(
                        name.into_err(format!("duplicate field name provided for \"{fname}\""))
                    );
                }
                Entry::Vacant(e) => {
                    e.insert(f.clone());
                }
            }

            fields.push(f.clone());
        }

        Ok(Self { fields, field_map })
    }

    pub fn alignment(&self) -> usize {
        static MAX_ALIGNMENT: LazyLock<usize> = LazyLock::new(|| {
            PrimitiveType::ALL
                .iter()
                .map(|v| v.alignment())
                .max()
                .unwrap()
        });
        *MAX_ALIGNMENT
    }
}

#[derive(Debug, Clone)]
struct StructField {
    offset: usize,
    name: Rc<str>,
    dtype: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    F32,
}

impl PrimitiveType {
    pub const ALL: &[PrimitiveType] = &[
        Self::U8,
        Self::I8,
        Self::U16,
        Self::I16,
        Self::U32,
        Self::I32,
        Self::F32,
    ];

    pub fn is_signed(&self) -> bool {
        matches!(self, Self::U8 | Self::U16 | Self::U32)
    }

    pub fn coerced_type(&self, other: Self) -> Self {
        if *self > other { *self } else { other }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::U32 | Self::I32 | Self::F32 => 4,
        }
    }

    pub fn alignment(&self) -> usize {
        // TODO - Move to jib::cpu::DataType
        self.byte_size()
    }

    pub fn coerced(a: Self, b: Self) -> Self {
        if a > b { a } else { b }
    }
}

impl From<PrimitiveType> for jib::cpu::DataType {
    fn from(value: PrimitiveType) -> Self {
        match value {
            PrimitiveType::U8 => Self::U8,
            PrimitiveType::U16 => Self::U16,
            PrimitiveType::U32 => Self::U32,
            PrimitiveType::I8 => Self::I8,
            PrimitiveType::I16 => Self::I16,
            PrimitiveType::I32 => Self::I32,
            PrimitiveType::F32 => Self::F32,
        }
    }
}

impl From<jib::cpu::DataType> for PrimitiveType {
    fn from(value: jib::cpu::DataType) -> Self {
        match value {
            jib::cpu::DataType::U8 => Self::U8,
            jib::cpu::DataType::U16 => Self::U16,
            jib::cpu::DataType::U32 => Self::U32,
            jib::cpu::DataType::I8 => Self::I8,
            jib::cpu::DataType::I16 => Self::I16,
            jib::cpu::DataType::I32 => Self::I32,
            jib::cpu::DataType::F32 => Self::F32,
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::F32 => write!(f, "f32"),
        }
    }
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

impl TryFrom<TokenIter<'_>> for PrimitiveType {
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
            Ok(PrimitiveType::try_from(first)?)
        }
    }
}

impl TryFrom<Token> for PrimitiveType {
    type Error = UnknownPrimitiveError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        static PRIMITIVE_MAP: LazyLock<HashMap<String, PrimitiveType>> = LazyLock::new(|| {
            PrimitiveType::ALL
                .iter()
                .map(|v| (v.to_string(), *v))
                .collect()
        });

        if let Some(p) = PRIMITIVE_MAP.get(value.get_value()) {
            Ok(*p)
        } else {
            Err(UnknownPrimitiveError { token: value })
        }
    }
}
