use std::collections::HashMap;

use sol32::cpu::{Opcode, Processor, Register, DataType};

#[derive(Debug, Clone)]
enum AssemblerError {
    UnknownLabel(String),
    UnknownInstruction(String),
    ArgumentCountMismatch(String),
    ImmediateError(String, String),
    InvalidLabel(String),
    UnknownRegister(String),
    UnknownDataType(String),
}

#[derive(Debug, Clone)]
struct AssemblerErrorLoc {
    err: AssemblerError,
    loc: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Argument {
    reg: Register,
    data_type: Option<DataType>,
}

impl Argument {
    pub fn to_byte(&self) -> u8 {
        ((self.data_type.map_or(0, |d| d.get_id()) << 5) & 0xE0) | ((self.reg.get_index() as u8) & 0x1F)
    }
}

impl TryFrom<&str> for Argument {
    type Error = AssemblerError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        fn parse_reg(s: &str) -> Result<Register, AssemblerError> {
            let reg = match s {
                "$pc" => Register::ProgramCounter,
                "$stat" => Register::Status,
                "$sp" => Register::StackPointer,
                "$ovf" => Register::Overflow,
                "$ret" => Register::Return,
                "$arg" => Register::ArgumentBase,
                _ => {
                    if let Ok(val) = s.parse() {
                        Register::GeneralPurpose(val)
                    } else {
                        return Err(AssemblerError::UnknownRegister(s.to_string()));
                    }
                }
            };

            Ok(reg)
        }

        if let Some((s_reg, s_dt)) = value.split_once(':') {
            let reg = parse_reg(s_reg)?;

            let dt = match s_dt {
                "u8" => DataType::U8,
                "u16" => DataType::U16,
                "u32" => DataType::U32,
                "i8" => DataType::I8,
                "i16" => DataType::I16,
                "i32" => DataType::I32,
                "f32" => DataType::F32,
                _ => return Err(AssemblerError::UnknownDataType(s_dt.to_string())),
            };

            Ok(Self { reg, data_type: Some(dt) })
        } else {
            let reg = parse_reg(value)?;
            Ok(Self { reg, data_type: None })
        }
    }
}

enum Instruction {
    None(Opcode),
    Single(Opcode, Argument),
    SingleType(Opcode, Argument),
}

enum Token {
    ChangeAddress(u32),
    Operation,
    CreateLabel(String),
    LoadLoc(String),
    Immediate1(u8),
    Immediate2(u16),
    Immediate4(u32),
}

impl Token {
    pub fn to_bytes(&self) -> Option<Vec<u8>> {
        match self {
            Self::Operation => Some(Vec::new()),
            Self::Immediate1(v) => Some(vec![*v]),
            Self::Immediate2(v) => Some(v.to_be_bytes().into_iter().collect()),
            Self::Immediate4(v) => Some(v.to_be_bytes().into_iter().collect()),
            _ => None,
        }
    }
}

macro_rules! gen_read_immediate {
    ($fnname:ident, $t:ident) => {
        fn $fnname(op: &str, arg: &str) -> Result<$t, AssemblerError> {
            let res = if arg.starts_with("0x") || arg.starts_with("0X") {
                $t::from_str_radix(arg, 16)
            } else {
                arg.parse::<$t>()
            };

            match res {
                Ok(v) => Ok(v),
                Err(_) => Err(AssemblerError::ImmediateError(op.to_string(), arg.to_string())),
            }
        }
    };
}

impl TryFrom<&str> for Token {
    type Error = AssemblerError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let s = value.trim();
        let words = s.split_whitespace().collect::<Vec<_>>();

        let first: &str = if let Some(w) = words.first() {
            *w
        } else {
            return Err(AssemblerError::UnknownInstruction(s.to_string()));
        };

        let tok = if first.starts_with('.') {
            let op = &first[1..];

            if words.len() != 2 {
                return Err(AssemblerError::ArgumentCountMismatch(s.to_string()));
            }

            let arg = words[1];

            gen_read_immediate!(parse_imm_i8, i8);
            gen_read_immediate!(parse_imm_u8, u8);
            gen_read_immediate!(parse_imm_i16, i16);
            gen_read_immediate!(parse_imm_u16, u16);
            gen_read_immediate!(parse_imm_i32, i32);
            gen_read_immediate!(parse_imm_u32, u32);

            match op {
                "oper" => Token::ChangeAddress(parse_imm_u32(op, arg)?),
                "loadloc" => Token::LoadLoc(arg.to_string()),
                "u8" => Token::Immediate1(parse_imm_u8(op, arg)?),
                "u16" => Token::Immediate2(parse_imm_u16(op, arg)?),
                "u32" => Token::Immediate4(parse_imm_u32(op, arg)?),
                "i8" => Token::Immediate1(parse_imm_i8(op, arg)? as u8),
                "i16" => Token::Immediate2(parse_imm_i16(op, arg)? as u16),
                "i32" => Token::Immediate4(parse_imm_i32(op, arg)? as u32),
                "f32" => Token::Immediate4(match arg.parse::<f32>() {
                    Ok(v) => v,
                    Err(_) => return Err(AssemblerError::ImmediateError(op.to_string(), arg.to_string())),
                }.to_bits()),
            }
        } else if first.starts_with(':') {
            if words.len() != 1 {
                return Err(AssemblerError::ArgumentCountMismatch(s.to_string()));
            }

            Token::CreateLabel(first[1..].to_string())
        } else {
            Token::Operation
        };

        Ok(tok)
    }
}

struct ParserState {
    tokens: Vec<Token>,
    labels: HashMap<String, u32>,
    current_loc: usize,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            labels: HashMap::new(),
            current_loc: 0,
        }
    }
}

pub fn parse_text(txt: &str) -> Result<Vec<u8>, AssemblerErrorLoc> {
    parse_lines(&txt.lines().collect::<Vec<_>>())
}

pub fn parse_lines(txt: &[&str]) -> Result<Vec<u8>, AssemblerErrorLoc> {

}

pub fn assemble(txt: &[Token]) -> Result<Vec<u8>, AssemblerError> {

}
