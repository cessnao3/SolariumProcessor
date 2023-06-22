use sproc::cpu::Register;

use once_cell::sync::Lazy;
use regex::Regex;

#[derive(Clone, Debug)]
pub enum ArgumentError {
    U16(Argument),
    U8(Argument),
    Register(Argument),
    ParseIntError(String),
    ParseRegisterError(String),
    UnknownArgumentType(String),
    UnknownEscapeCharacter(char),
    CharacterNotAscii(char),
    UnexpectedQuote,
}

impl std::fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::U16(v) => write!(f, "unable to convert {v} to u16"),
            Self::U8(v) => write!(f, "unable to convert {v} to u8"),
            Self::Register(v) => write!(f, "register value {v} exceeds number of registers"),
            Self::ParseIntError(e) => write!(f, "integer parse error: {e}"),
            Self::ParseRegisterError(e) => write!(f, "unable to parse \"{e}\" as register"),
            Self::UnknownArgumentType(a) => write!(f, "unable to determine argument type for \"{a}\""),
            Self::UnknownEscapeCharacter(c) => write!(f, "unknown escape code character '{c}"),
            Self::CharacterNotAscii(c) => write!(f, "not-ascii character '{c}' provided"),
            Self::UnexpectedQuote => write!(f, "unexpected non-escaped quotation character"),
        }
    }
}

#[derive(Clone, Debug)]
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
            Ok(v) => v as usize,
            Err(e) => return Err(e),
        };

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
        static ARG_REGISTER_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new(&format!("^{0:}$", r"(\$[\w]+)")).unwrap());
        static ARG_LABEL_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new(&format!("^{0:}$", r"([a-z][a-z0-9_A-Z]+)")).unwrap());
        static ARG_NUMBER_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new(&format!("^{0:}$", r"([\-|+]?[\d]+)")).unwrap());
        static ARG_HEX_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new(&format!("^{0:}$", r"(0x[a-f0-9A-Z]{1,4})")).unwrap());

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
                "exc" => 3,
                "ret" => 4,
                "arg" => 5,
                _ => return Err(ArgumentError::ParseRegisterError(reg_str.to_string())),
            };

            Ok(Argument::SignedNumber(reg_ind))
        } else if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
            let txt = &s[1..(s.len() - 1)];

            let mut chrs = Vec::new();
            let mut was_escape = false;
            for c in txt.chars() {
                if !c.is_ascii() {
                    return Err(ArgumentError::CharacterNotAscii(c));
                } else if c == '\\' {
                    was_escape = true;
                } else if c == '"' && !was_escape {
                    return Err(ArgumentError::UnexpectedQuote);
                } else if was_escape {
                    chrs.push(match c {
                        'n' => '\n',
                        '0' => '\0',
                        '\\' => '\\',
                        '"' => '"',
                        c => return Err(ArgumentError::UnknownEscapeCharacter(c)),
                    });
                } else {
                    chrs.push(c)
                }
            }

            Ok(Argument::Text(chrs.iter().collect()))
        } else {
            Err(ArgumentError::UnknownArgumentType(s.to_string()))
        }
    }
}