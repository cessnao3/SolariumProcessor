use core::fmt;

pub struct OperationValue {
    pub val: u32,
    pub carry: bool,
}

impl From<(u32, bool)> for OperationValue {
    fn from(value: (u32, bool)) -> Self {
        Self {
            val: value.0,
            carry: value.1,
        }
    }
}

impl From<f32> for OperationValue {
    fn from(value: f32) -> Self {
        Self {
            val: value.to_bits(),
            carry: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationError {
    DivideByZero,
    UnuspportedOperation,
}

impl fmt::Display for OperationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DivideByZero => write!(f, "Divide By Zero"),
            Self::UnuspportedOperation => write!(f, "Unsupported Operation"),
        }
    }
}

pub trait ArithmeticOperations {
    fn add(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn sub(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn mul(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn div(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn rem(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn neg(&self, a: u32) -> Result<OperationValue, OperationError>;
}

pub trait RelationalOperations {
    fn gt(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn geq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn lt(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn leq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn eq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn neq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
}

pub trait BinaryOperations {
    fn band(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bxor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bsftr(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bsftl(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bnot(&self, a: u32) -> Result<OperationValue, OperationError>;
}

macro_rules! define_arith_for_type {
    ($sname:ident, $tname:ident) => {
        impl ArithmeticOperations for $sname {
            fn add(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_add(b as $tname);
                Ok((((res.0 as i32) as u32), res.1).into())
            }

            fn sub(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_sub(b as $tname);
                Ok(((res.0 as i32) as u32, res.1).into())
            }

            fn mul(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_mul(b as $tname);
                Ok((((res.0 as i32) as u32), res.1).into())
            }

            fn div(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                if b == 0 {
                    return Err(OperationError::DivideByZero);
                }
                let res = (a as $tname) / (b as $tname);
                Ok((((res as i32) as u32), false).into())
            }

            fn rem(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                if b == 0 {
                    return Err(OperationError::DivideByZero);
                }
                let res = (a as $tname) % (b as $tname);
                Ok((((res as i32) as u32), false).into())
            }

            fn neg(&self, a: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_neg();
                Ok((((res.0 as i32) as u32), res.1).into())
            }
        }
    };
}

macro_rules! define_bitwise_for_type {
    ($sname:ident, $tname:ident) => {
        impl BinaryOperations for $sname {
            fn band(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname) & (b as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname) | (b as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bxor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname) ^ (b as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bsftr(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_shr(b);
                Ok(((res.0 as i32) as u32, res.1).into())
            }

            fn bsftl(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_shl(b);
                Ok(((res.0 as i32) as u32, res.1).into())
            }

            fn bnot(&self, a: u32) -> Result<OperationValue, OperationError> {
                let res = !(a as $tname);
                Ok(((res as i32) as u32, false).into())
            }
        }
    };
}

macro_rules! define_rel_for_type {
    ($sname:ident, $tname:ident) => {
        impl RelationalOperations for $sname {
            fn gt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) > (b as $tname))
            }

            fn geq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) >= (b as $tname))
            }

            fn lt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) < (b as $tname))
            }

            fn leq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) <= (b as $tname))
            }

            fn eq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) == (b as $tname))
            }

            fn neq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) != (b as $tname))
            }
        }
    };
}

pub struct IntegerU8Operations;
define_arith_for_type!(IntegerU8Operations, u8);
define_bitwise_for_type!(IntegerU8Operations, u8);
define_rel_for_type!(IntegerU8Operations, u8);

pub struct IntegerU16Operations;
define_arith_for_type!(IntegerU16Operations, u16);
define_bitwise_for_type!(IntegerU16Operations, u16);
define_rel_for_type!(IntegerU16Operations, u16);

pub struct IntegerU32Operations;
define_arith_for_type!(IntegerU32Operations, u32);
define_bitwise_for_type!(IntegerU32Operations, u32);
define_rel_for_type!(IntegerU32Operations, u32);

pub struct IntegerI8Operations;
define_arith_for_type!(IntegerI8Operations, i8);
define_bitwise_for_type!(IntegerI8Operations, i8);
define_rel_for_type!(IntegerI8Operations, i8);

pub struct IntegerI16Operations;
define_arith_for_type!(IntegerI16Operations, i16);
define_bitwise_for_type!(IntegerI16Operations, i16);
define_rel_for_type!(IntegerI16Operations, i16);

pub struct IntegerI32Operations;
define_arith_for_type!(IntegerI32Operations, i32);
define_bitwise_for_type!(IntegerI32Operations, i32);
define_rel_for_type!(IntegerI32Operations, i32);

pub struct FloatOperations;
impl ArithmeticOperations for FloatOperations {
    fn add(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let r = f32::from_bits(a) + f32::from_bits(b);
        Ok(r.into())
    }

    fn sub(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let r = f32::from_bits(a) - f32::from_bits(b);
        Ok(r.into())
    }

    fn mul(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let r = f32::from_bits(a) * f32::from_bits(b);
        Ok(r.into())
    }

    fn div(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let bf = f32::from_bits(b);
        if bf == 0.0f32 {
            return Err(OperationError::DivideByZero);
        }
        let r = f32::from_bits(a) / bf;
        Ok(r.into())
    }

    fn rem(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let bf = f32::from_bits(b);
        if bf == 0.0f32 {
            return Err(OperationError::DivideByZero);
        }
        let r = f32::from_bits(a) % bf;
        Ok(r.into())
    }

    fn neg(&self, a: u32) -> Result<OperationValue, OperationError> {
        let bf = f32::from_bits(a);
        Ok((-bf).into())
    }
}

impl RelationalOperations for FloatOperations {
    fn gt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) > f32::from_bits(b))
    }

    fn geq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) >= f32::from_bits(b))
    }

    fn lt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) < f32::from_bits(b))
    }

    fn leq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) <= f32::from_bits(b))
    }

    fn eq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) == f32::from_bits(b))
    }

    fn neq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) != f32::from_bits(b))
    }
}
