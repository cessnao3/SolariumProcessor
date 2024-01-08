use jib::cpu::DataType;

#[derive(Debug, Clone)]
pub enum SpTypeError {
    ArraySizeError(String),
    ArrayTypeError(String),
    NoTypeFound(String),
    InvalidTypeName(String),
    EmptyType(String),
    UnableToModifyBuiltin,
    CannotOverrideType { new_type: SpType, old_type: SpType },
    MissingTypeSize(SpType),
    MissingTypeName(SpType),
    InvalidWhitespace(String),
    NoBasePrimitiveForType(SpType),
    FieldNotFound(String, StructDef),
}

impl std::fmt::Display for SpTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArraySizeError(t) => write!(f, "unable to find valid array size for '{t}'"),
            Self::ArrayTypeError(t) => write!(f, "unable to find valid array type for '{t}'"),
            Self::NoTypeFound(t) => write!(f, "no type found for name '{t}'"),
            Self::InvalidTypeName(t) => write!(f, "'{t}' is not a valid type name"),
            Self::EmptyType(t) => write!(f, "type specification empty for '{t}'"),
            Self::UnableToModifyBuiltin => write!(f, "unable to modify builtin type"),
            Self::CannotOverrideType { new_type, old_type } => {
                write!(f, "unable to override '{new_type}' with '{old_type}'")
            }
            Self::MissingTypeSize(t) => write!(f, "missing type size for '{t}'"),
            Self::MissingTypeName(t) => write!(f, "missing type name specification for '{t}'"),
            Self::InvalidWhitespace(t) => write!(f, "unexpected whitespace found in '{t}'"),
            Self::NoBasePrimitiveForType(t) => write!(f, "no base primitive defined for '{t}'"),
            Self::FieldNotFound(field, def) => write!(f, "no field '{field}' in struct {}", def.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Box<SpType>)>,
}

impl StructDef {
    pub fn new<T: IntoIterator<Item = (String, Box<SpType>)>>(name: &str, fields: T) -> Self {
        Self { name: name.into(), fields: fields.into_iter().collect() }
    }

    pub fn byte_size(&self) -> Result<usize, SpTypeError> {
        return self.fields.iter().map(|x| x.1.byte_count()).sum()
    }

    pub fn offset_of(&self, s: &str) -> Result<usize, SpTypeError> {
        let mut offset = 0;
        for f in self.fields.iter() {
            if f.0 == s {
                return Ok(offset);
            } else {
                offset += f.1.byte_count()?;
            }
        }

        Err(SpTypeError::FieldNotFound(s.into(), self.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpType {
    OpaqueType {
        name: String,
    },
    Primitive {
        base: DataType,
    },
    Alias {
        name: String,
        base: Box<SpType>,
    },
    Array {
        base: Box<SpType>,
        size: usize,
    },
    Struct(StructDef),
    Pointer {
        base: Box<SpType>,
    },
    Constant {
        base: Box<SpType>,
    },
    Function {
        ret: Box<SpType>,
        args: Vec<SpType>,
    },
}

impl SpType {
    pub fn is_valid_name(s: &str) -> bool {
        // Ensure that the first character is alphabetic and that the only characters are ascii-alphanumeric/_/-
        if !s
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        {
            false
        } else if let Some(c) = s.chars().next() {
            c.is_ascii_alphabetic()
        } else {
            false
        }
    }

    pub fn byte_count(&self) -> Result<usize, SpTypeError> {
        match self {
            Self::OpaqueType { .. } => Err(SpTypeError::MissingTypeSize(self.clone())),
            Self::Primitive { base, .. } => Ok(base.byte_size()),
            Self::Array { base, size } => Ok(base.byte_count()? * size),
            Self::Struct(def) => def.byte_size(),
            Self::Pointer { .. } => Ok(DataType::U32.byte_size()),
            Self::Function { .. } => Ok(DataType::U32.byte_size()),
            Self::Constant { base } => base.byte_count(),
            Self::Alias { base, .. } => base.byte_count(),
        }
    }

    pub fn base_primitive(&self) -> Result<DataType, SpTypeError> {
        match self {
            Self::Pointer { .. } => Ok(DataType::U32),
            Self::Array { .. } => Ok(DataType::U32),
            Self::Primitive { base } => Ok(*base),
            Self::Alias { base, .. } => base.base_primitive(),
            Self::Constant { base } => base.base_primitive(),
            _ => Err(SpTypeError::NoBasePrimitiveForType(self.clone())),
        }
    }

    pub fn as_const(&self) -> SpType {
        if let Self::Constant { base } = self {
            self.clone()
        } else {
            Self::Constant {
                base: Box::new(self.clone()),
            }
        }
    }

    pub fn as_mut(&self) -> SpType {
        if let Self::Constant { base } = self {
            *base.clone()
        } else {
            self.clone()
        }
    }
}

impl From<DataType> for SpType {
    fn from(value: DataType) -> Self {
        SpType::Primitive { base: value }
    }
}

impl std::fmt::Display for SpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpaqueType { name } => write!(f, "{name}"),
            Self::Primitive { base } => write!(f, "{base}"),
            Self::Array { base, size } => write!(f, "[{size}]{base}"),
            Self::Struct(def) => write!(f, "{}", def.name),
            Self::Pointer { base, .. } => write!(f, "*{base}"),
            Self::Constant { base, .. } => write!(f, "${base}"),
            Self::Function { ret, args } => write!(
                f,
                "^{ret}({})",
                args.iter()
                    .map(|t| format!("{t}"))
                    .reduce(|a, b| format!("{a}, {b}"))
                    .unwrap_or(String::new())
            ),
            Self::Alias { name, base } => write!(f, "{name} /* {base} */"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpTypeDict {
    types: std::collections::HashMap<String, SpType>,
}

impl SpTypeDict {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse_type(&self, t: &str) -> Result<SpType, SpTypeError> {
        match t.chars().next() {
            Some('*') => Ok(SpType::Pointer {
                base: Box::new(self.parse_type(&t[1..])?),
            }),
            Some('$') => Ok(SpType::Constant {
                base: Box::new(self.parse_type(&t[1..])?),
            }),
            Some('[') => {
                if let Some(ind) = t.find(']') {
                    let size = match t[1..ind].parse::<u16>() {
                        Ok(v) => v,
                        Err(_) => return Err(SpTypeError::ArraySizeError(t.into())),
                    };

                    Ok(SpType::Array {
                        base: Box::new(self.parse_type(&t[ind + 1..])?),
                        size: size as usize,
                    })
                } else {
                    Err(SpTypeError::ArrayTypeError(t.into()))
                }
            }
            Some(_) => {
                if t.chars().any(|c| c.is_whitespace()) {
                    Err(SpTypeError::InvalidWhitespace(t.into()))
                } else if let Some(t) = self.types.get(t) {
                    Ok(t.clone())
                } else {
                    Err(SpTypeError::NoTypeFound(t.into()))
                }
            }
            None => Err(SpTypeError::EmptyType(t.into())),
        }
    }

    pub fn add_type(&mut self, t: SpType) -> Result<(), SpTypeError> {
        let name = match &t {
            SpType::OpaqueType { name } => name,
            SpType::Alias { name, .. } => name,
            SpType::Struct(def) => &def.name,
            _ => return Err(SpTypeError::MissingTypeName(t.clone())),
        };

        if name == "void" {
            return Err(SpTypeError::UnableToModifyBuiltin);
        }

        // TODO - Check that each struct field is valid!

        if !SpType::is_valid_name(name) {
            return Err(SpTypeError::InvalidTypeName(name.into()));
        }

        if let Some(existing_type) = self.types.get(name) {
            match existing_type {
                SpType::OpaqueType { .. } => match t {
                    SpType::OpaqueType { .. } => (),
                    SpType::Struct { .. } => (),
                    _ => {
                        return Err(SpTypeError::CannotOverrideType {
                            new_type: t.clone(),
                            old_type: existing_type.clone(),
                        })
                    }
                },
                _ => {
                    return Err(SpTypeError::CannotOverrideType {
                        new_type: t.clone(),
                        old_type: existing_type.clone(),
                    })
                }
            }
        }

        self.types.insert(name.to_string(), t);

        Ok(())
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }
}

impl Default for SpTypeDict {
    fn default() -> Self {
        let mut s = Self {
            types: std::collections::HashMap::new(),
        };

        let type_vals = [
            ("u8", DataType::U8),
            ("i8", DataType::I8),
            ("u16", DataType::U16),
            ("i16", DataType::I16),
            ("u32", DataType::U32),
            ("i32", DataType::I32),
            ("f32", DataType::F32),
        ];

        s.types.insert(
            "void".into(),
            SpType::OpaqueType {
                name: "void".to_string(),
            },
        );

        for (n, t) in type_vals {
            s.types.insert(n.into(), SpType::Primitive { base: t });
        }

        s
    }
}
