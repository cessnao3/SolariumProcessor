use jib::cpu::DataType;

#[derive(Debug, Clone)]
pub enum TypeError {
    ArraySizeError(String),
    ArrayTypeError(String),
    NoTypeFound(String),
    InvalidTypeName(String),
    EmptyType(String),
    UnableToModifyBuiltin,
    CannotOverrideType { new_type: Type, old_type: Type },
    MissingTypeSize(Type),
    MissingTypeName(Type),
    InvalidWhitespace(String),
    NoBasePrimitiveForType(Type),
    FieldNotFound(String, StructDef),
    TypeMismatch(Type, Type),
    CannotDereference(Type),
    ParenthesisError,
    UnexpectedCharacters(String),
}

impl std::fmt::Display for TypeError {
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
            Self::FieldNotFound(field, def) => {
                write!(f, "no field '{field}' in struct {}", def.name)
            }
            Self::TypeMismatch(a, b) => write!(f, "types {a} and {b} do not match"),
            Self::CannotDereference(t) => write!(f, "cannot dereference type '{t}'"),
            Self::ParenthesisError => write!(f, "parenthesis error"),
            Self::UnexpectedCharacters(s) => write!(f, "unexpected characters \"{s}\""),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Box<Type>)>,
}

impl StructDef {
    pub fn new<T: IntoIterator<Item = (String, Box<Type>)>>(name: &str, fields: T) -> Self {
        Self {
            name: name.into(),
            fields: fields.into_iter().collect(),
        }
    }

    pub fn byte_size(&self) -> Result<usize, TypeError> {
        return self.fields.iter().map(|x| x.1.byte_count()).sum();
    }

    pub fn offset_of(&self, s: &str) -> Result<usize, TypeError> {
        let mut offset = 0;
        for f in self.fields.iter() {
            if f.0 == s {
                return Ok(offset);
            } else {
                offset += f.1.byte_count()?;
            }
        }

        Err(TypeError::FieldNotFound(s.into(), self.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    OpaqueType { name: String },
    Primitive { base: DataType },
    Alias { name: String, base: Box<Type> },
    Array { base: Box<Type>, size: usize },
    Struct(StructDef),
    Pointer { base: Box<Type> },
    Constant { base: Box<Type> },
    Function { ret: Option<Box<Type>>, args: Vec<Type> },
}

impl Type {
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

    pub fn byte_count(&self) -> Result<usize, TypeError> {
        match self {
            Self::OpaqueType { .. } => Err(TypeError::MissingTypeSize(self.clone())),
            Self::Primitive { base, .. } => Ok(base.byte_size()),
            Self::Array { base, size } => Ok(base.byte_count()? * size),
            Self::Struct(def) => def.byte_size(),
            Self::Pointer { .. } => Ok(DataType::U32.byte_size()),
            Self::Function { .. } => Ok(DataType::U32.byte_size()),
            Self::Constant { base } => base.byte_count(),
            Self::Alias { base, .. } => base.byte_count(),
        }
    }

    pub fn base_primitive(&self) -> Result<DataType, TypeError> {
        match self {
            Self::Pointer { .. } => Ok(DataType::U32),
            Self::Array { .. } => Ok(DataType::U32),
            Self::Primitive { base } => Ok(*base),
            Self::Alias { base, .. } => base.base_primitive(),
            Self::Constant { base } => base.base_primitive(),
            _ => Err(TypeError::NoBasePrimitiveForType(self.clone())),
        }
    }

    pub fn as_const(&self) -> Type {
        if let Self::Constant { base } = self {
            self.clone()
        } else {
            Self::Constant {
                base: Box::new(self.clone()),
            }
        }
    }

    pub fn as_mut(&self) -> Type {
        if let Self::Constant { base } = self {
            *base.clone()
        } else {
            self.clone()
        }
    }

    pub fn is_func(&self) -> bool {
        match self {
            Self::Function { .. } => true,
            Self::Constant { base } => base.is_func(),
            _ => false,
        }
    }
}

impl From<DataType> for Type {
    fn from(value: DataType) -> Self {
        Type::Primitive { base: value }
    }
}

impl std::fmt::Display for Type {
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
                "^({}){}",
                args.iter()
                    .map(|t| format!("{t}"))
                    .reduce(|a, b| format!("{a}, {b}"))
                    .unwrap_or(String::new()),
                ret.as_ref().map(|r| format!(" {r}")).unwrap_or("".into())
            ),
            Self::Alias { name, base } => write!(f, "{name} /* {base} */"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDict {
    types: std::collections::HashMap<String, Type>,
}

impl TypeDict {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse_type(&self, t: &str) -> Result<Type, TypeError> {
        match t.chars().next() {
            Some('*') => Ok(Type::Pointer {
                base: Box::new(self.parse_type(&t[1..])?),
            }),
            Some('$') => Ok(Type::Constant {
                base: Box::new(self.parse_type(&t[1..])?),
            }),
            Some('[') => {
                if let Some(ind) = t.find(']') {
                    let size = match t[1..ind].parse::<u16>() {
                        Ok(v) => v,
                        Err(_) => return Err(TypeError::ArraySizeError(t.into())),
                    };

                    Ok(Type::Array {
                        base: Box::new(self.parse_type(&t[ind + 1..])?),
                        size: size as usize,
                    })
                } else {
                    Err(TypeError::ArrayTypeError(t.into()))
                }
            }
            Some('^') => {
                if let Some(i1) = t.find('(') {
                    let space_vals = t[1..i1].trim();
                    if !space_vals.is_empty() {
                        return Err(TypeError::UnexpectedCharacters(space_vals.into()));
                    }

                    if let Some(i2) = t.find(')') {
                        if i1 >= i2 {
                            Err(TypeError::ParenthesisError)
                        } else {
                            let ret_str = t[(i2 + 1)..].trim();
                            let arg_type_str = &t[(i1 + 1)..i2].trim();
                            let ret_type = if ret_str.is_empty() || ret_str == "void" {
                                None
                            } else {
                                Some(self.parse_type(ret_str)?)
                            };

                            let arg_types = if arg_type_str.contains(',') {
                                arg_type_str
                                    .split(',')
                                    .map(|s| self.parse_type(s.trim()))
                                    .collect::<Result<Vec<_>, _>>()?
                            } else if arg_type_str.is_empty() {
                                Vec::new()
                            } else {
                                vec![self.parse_type(arg_type_str)?]
                            };

                            return Ok(Type::Function {
                                ret: ret_type.map(|t| Box::new(t)),
                                args: arg_types,
                            });
                        }
                    } else {
                        return Err(TypeError::ParenthesisError);
                    }
                } else {
                    return Err(TypeError::ParenthesisError);
                }
            }
            Some(_) => {
                if t.chars().any(|c| c.is_whitespace()) {
                    Err(TypeError::InvalidWhitespace(t.into()))
                } else if let Some(t) = self.types.get(t) {
                    Ok(t.clone())
                } else {
                    Err(TypeError::NoTypeFound(t.into()))
                }
            }
            None => Err(TypeError::EmptyType(t.into())),
        }
    }

    pub fn add_type(&mut self, t: Type) -> Result<(), TypeError> {
        let name = match &t {
            Type::OpaqueType { name } => name,
            Type::Alias { name, .. } => name,
            Type::Struct(def) => &def.name,
            _ => return Err(TypeError::MissingTypeName(t.clone())),
        };

        if name == "void" {
            return Err(TypeError::UnableToModifyBuiltin);
        }

        // TODO - Check that each struct field is valid!

        if !Type::is_valid_name(name) {
            return Err(TypeError::InvalidTypeName(name.into()));
        }

        if let Some(existing_type) = self.types.get(name) {
            match existing_type {
                Type::OpaqueType { .. } => match t {
                    Type::OpaqueType { .. } => (),
                    Type::Struct { .. } => (),
                    _ => {
                        return Err(TypeError::CannotOverrideType {
                            new_type: t.clone(),
                            old_type: existing_type.clone(),
                        })
                    }
                },
                _ => {
                    return Err(TypeError::CannotOverrideType {
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

impl Default for TypeDict {
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
            Type::OpaqueType {
                name: "void".to_string(),
            },
        );

        for (n, t) in type_vals {
            s.types.insert(n.into(), Type::Primitive { base: t });
        }

        s
    }
}
