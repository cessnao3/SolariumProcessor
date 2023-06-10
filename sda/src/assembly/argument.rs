use super::asm_regex::{ARG_HEX_REGEX, ARG_LABEL_REGEX, ARG_NUMBER_REGEX, ARG_REGISTER_REGEX};
use sproc::cpu::Register;

pub enum ArgumentError {
    U16(Argument),
    U8(Argument),
    Register(Argument),
    ParseIntError(String),
    ParseRegisterError(String),
}

impl std::fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::U16(v) => write!(f, "uanble to convert {v} to u16"),
            Self::U8(v) => write!(f, "uanble to convert {v} to u8"),
            Self::Register(v) => write!(f, "register value {v} exceeds number of registers"),
            Self::ParseIntError(e) => write!(f, "integer parse error: {e}"),
            Self::ParseRegisterError(e) => write!(f, "uanble to parse \"{e}\" as register"),
        }
    }
}

#[derive(Clone)]
pub enum Argument {
    UnsignedNumber(u32),
    SignedNumber(i32),
    Label(String),
    Text(String),
}

impl Argument {
    pub fn to_u16(&self) -> Result<u16, ArgumentError> {
        match &self {
            Argument::UnsignedNumber(v) => {
                let uval = *v as u16;
                if uval as u32 == *v {
                    Ok(uval)
                } else {
                    Err(ArgumentError::U16(self.clone()))
                }
            }
            Argument::SignedNumber(v) => {
                let ival = *v as i16;
                if ival as i32 == *v {
                    Ok(ival as u16)
                } else {
                    Err(ArgumentError::U16(self.clone()))
                }
            }
            _ => Err(ArgumentError::U16(self.clone())),
        }
    }

    pub fn to_u8(&self) -> Result<u8, ArgumentError> {
        match &self {
            Argument::UnsignedNumber(v) => {
                let uval = *v as u8;
                if uval as u32 == *v {
                    Ok(uval)
                } else {
                    Err(ArgumentError::U8(self.clone()))
                }
            }
            Argument::SignedNumber(v) => {
                let ival = *v as i8;
                if ival as i32 == *v {
                    Ok(ival as u8)
                } else {
                    Err(ArgumentError::U8(self.clone()))
                }
            }
            _ => Err(ArgumentError::U8(self.clone())),
        }
    }

    pub fn to_register_val(&self) -> Result<Register, ArgumentError> {
        let reg_val = match self.to_u8() {
            Ok(v) => v,
            Err(e) => return Err(e),
        } as usize;

        if reg_val < sproc::cpu::SolariumProcessor::NUM_REGISTERS {
            Ok(Register::GeneralPurpose(reg_val))
        } else {
            Err(ArgumentError::Register(self.clone()))
        }
    }
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Argument::Label(s) => write!(f, "Label({0:})", s),
            Argument::UnsignedNumber(v) => write!(f, "Unsigned({0:})", v),
            Argument::SignedNumber(v) => write!(f, "Signed({0:})", v),
            Argument::Text(s) => write!(f, "Text(\"{0:}\")", s),
        }
    }
}

impl std::str::FromStr for Argument {
    type Err = ArgumentError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if ARG_NUMBER_REGEX.is_match(s) {
            if s.starts_with('-') {
                match s.parse::<i32>() {
                    Ok(v) => Ok(Argument::SignedNumber(v)),
                    Err(e) => Err(ArgumentError::ParseIntError(e.to_string())),
                }
            } else {
                match s.parse::<u32>() {
                    Ok(v) => Ok(Argument::UnsignedNumber(v)),
                    Err(e) => Err(ArgumentError::ParseIntError(e.to_string())),
                }
            }
        } else if ARG_HEX_REGEX.is_match(s) {
            match u32::from_str_radix(&s[2..], 16) {
                Ok(v) => Ok(Argument::UnsignedNumber(v)),
                Err(e) => Err(ArgumentError::ParseIntError(e.to_string())),
            }
        } else if ARG_LABEL_REGEX.is_match(s) {
            Ok(Argument::Label(s.to_string()))
        } else if ARG_REGISTER_REGEX.is_match(s) {
            let reg_str = &s[1..];

            let reg_ind = match reg_str {
                "pc" => 0,
                "stat" => 1,
                "sp" => 2,
                "ret" => 4,
                "arg" => 5,
                _ => return Err(ArgumentError::ParseRegisterError(reg_str.to_string())),
            };

            Ok(Argument::SignedNumber(reg_ind))
        } else {
            panic!();
        }
    }
}
