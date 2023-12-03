use sol32::cpu::{Register, DataType};

use crate::immediate::ImmediateError;

pub enum ArgumentError {
    Immediate(ImmediateError),
    UnknownRegister(String),
    UnknownDataType(String),
}

impl From<ImmediateError> for ArgumentError {
    fn from(value: ImmediateError) -> Self {
        ArgumentError::Immediate(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArgumentRegister {
    reg: Register,
}

impl ArgumentRegister {
    const REGISTER_MASK: u8 = 0x1F;

    pub fn to_byte(&self) -> u8 {
        (self.reg.get_index() as u8) & Self::REGISTER_MASK
    }
}

impl TryFrom<&str> for ArgumentRegister {
    type Error = ArgumentError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let reg = match value {
            "$pc" => Register::ProgramCounter,
            "$stat" => Register::Status,
            "$sp" => Register::StackPointer,
            "$ovf" => Register::Overflow,
            "$ret" => Register::Return,
            "$arg" => Register::ArgumentBase,
            _ => {
                if let Ok(val) = value.parse() {
                    Register::GeneralPurpose(val)
                } else {
                    return Err(ArgumentError::UnknownRegister(value.to_string()));
                }
            }
        };

        Ok(Self { reg })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArgumentType {
    reg: ArgumentRegister,
    data_type: DataType,
}

impl ArgumentType {
    const DT_OFFSET: i32 = 5;
    const DT_MASK: u8 = 0xE0;

    pub fn to_byte(&self) -> u8 {
        ((self.data_type.get_id() << Self::DT_OFFSET) & Self::DT_MASK) | self.reg.to_byte()
    }
}

impl TryFrom<&str> for ArgumentType {
    type Error = ArgumentError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some((s_reg, s_dt)) = value.split_once(':') {
            let reg = ArgumentRegister::try_from(s_reg)?;

            let dt = match s_dt {
                "u8" => DataType::U8,
                "u16" => DataType::U16,
                "u32" => DataType::U32,
                "i8" => DataType::I8,
                "i16" => DataType::I16,
                "i32" => DataType::I32,
                "f32" => DataType::F32,
                _ => return Err(ArgumentError::UnknownDataType(s_dt.to_string())),
            };

            Ok(Self { reg, data_type: dt })
        } else {
            Err(ArgumentError::UnknownRegister(value.to_string()))
        }
    }
}
