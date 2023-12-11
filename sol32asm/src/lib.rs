mod argument;
mod immediate;
pub mod instructions;

use core::fmt;
use std::{collections::HashMap, rc::Rc};

use instructions::{
    InstructionError, OpAdd, OpBand, OpBool, OpBor, OpBshl, OpBshr, OpBxor, OpCall, OpConv, OpCopy,
    OpDiv, OpHalt, OpInt, OpIntr, OpJmp, OpJmpr, OpJmpri, OpLoad, OpLoadNext, OpLoadi, OpLoadr,
    OpLoadri, OpMul, OpNoop, OpNot, OpPop, OpPopr, OpPush, OpRem, OpReset, OpRet, OpRetInt, OpSave,
    OpSaver, OpSub, OpTeq, OpTgeq, OpTgt, OpTleq, OpTlt, OpTneq, OpTnz, OpTz, ToInstruction,
};

use immediate::{
    parse_imm_i16, parse_imm_i32, parse_imm_i8, parse_imm_u16, parse_imm_u32, parse_imm_u8,
    ImmediateError,
};

#[derive(Debug, Clone)]
pub enum AssemblerError {
    UnknownLabel(String),
    UnknownInstruction(String),
    Instruction(InstructionError),
    ArgumentCountMismatch(usize, usize),
    InvalidLabel(String),
    Immediate(ImmediateError),
    BadLabel(String),
    DuplicateLabel(String),
}

impl fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownLabel(l) => write!(f, "Unknown Label {l}"),
            Self::UnknownInstruction(i) => write!(f, "Unknown Instruction {i}"),
            Self::ArgumentCountMismatch(num, expected) => {
                write!(f, "Argument Count Expected {expected}, found {num}")
            }
            Self::InvalidLabel(l) => write!(f, "Invalid Label {l}"),
            Self::Instruction(i) => write!(f, "Instruction Error => {i}"),
            Self::Immediate(i) => write!(f, "Immediate Error => {i}"),
            Self::BadLabel(l) => write!(f, "Bad Label '{l}'"),
            Self::DuplicateLabel(l) => write!(f, "Duplicate Label '{l}'"),
        }
    }
}

impl From<ImmediateError> for AssemblerError {
    fn from(value: ImmediateError) -> Self {
        Self::Immediate(value)
    }
}

impl From<InstructionError> for AssemblerError {
    fn from(value: InstructionError) -> Self {
        Self::Instruction(value)
    }
}

#[derive(Debug, Clone)]
pub struct LocationInfo {
    pub line: usize,
    pub full_line: Option<String>,
}

#[derive(Debug, Clone)]
pub struct AssemblerErrorLoc {
    pub err: AssemblerError,
    pub loc: LocationInfo,
}

impl fmt::Display for AssemblerErrorLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line {} - {}", self.loc.line, self.err)?;
        if let Some(s) = self.loc.full_line.as_ref() {
            write!(f, " - \"{}\"", s)?;
        }

        Ok(())
    }
}

type FnInst = fn(Vec<String>) -> Result<Rc<dyn ToInstruction>, InstructionError>;

#[derive(Clone)]
pub enum Token {
    ChangeAddress(u32),
    Operation(FnInst, Vec<String>),
    CreateLabel(String),
    LoadLoc(String),
    Immediate1(u8),
    Immediate2(u16),
    Immediate4(u32),
}

#[derive(Clone)]
pub struct TokenLoc {
    pub tok: Token,
    pub loc: LocationInfo,
}

struct TokenList {
    tokens: Vec<TokenLoc>,
    inst: HashMap<String, FnInst>,
    label_regex: regex::Regex,
}

impl TokenList {
    pub fn new() -> Self {
        let inst = HashMap::from([
            (
                "noop".into(),
                (|a| Ok(Rc::new(OpNoop::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "reset".into(),
                (|a| Ok(Rc::new(OpReset::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "retint".into(),
                (|a| Ok(Rc::new(OpRetInt::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "ret".into(),
                (|a| Ok(Rc::new(OpRet::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "halt".into(),
                (|a| Ok(Rc::new(OpHalt::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "int".into(),
                (|a| Ok(Rc::new(OpInt::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "intr".into(),
                (|a| Ok(Rc::new(OpIntr::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "call".into(),
                (|a| Ok(Rc::new(OpCall::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "push".into(),
                (|a| Ok(Rc::new(OpPush::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "pop".into(),
                (|a| Ok(Rc::new(OpPop::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "popr".into(),
                (|a| Ok(Rc::new(OpPopr::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "jmp".into(),
                (|a| Ok(Rc::new(OpJmp::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "jmpr".into(),
                (|a| Ok(Rc::new(OpJmpr::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "jmpri".into(),
                (|a| Ok(Rc::new(OpJmpri::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "ldi".into(),
                (|a| Ok(Rc::new(OpLoadi::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "ldri".into(),
                (|a| Ok(Rc::new(OpLoadri::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "not".into(),
                (|a| Ok(Rc::new(OpNot::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "bool".into(),
                (|a| Ok(Rc::new(OpBool::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tz".into(),
                (|a| Ok(Rc::new(OpTz::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tnz".into(),
                (|a| Ok(Rc::new(OpTnz::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "copy".into(),
                (|a| Ok(Rc::new(OpCopy::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "sav".into(),
                (|a| Ok(Rc::new(OpSave::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "savr".into(),
                (|a| Ok(Rc::new(OpSaver::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "ld".into(),
                (|a| Ok(Rc::new(OpLoad::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "ldr".into(),
                (|a| Ok(Rc::new(OpLoadr::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "ldn".into(),
                (|a| Ok(Rc::new(OpLoadNext::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "conv".into(),
                (|a| Ok(Rc::new(OpConv::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "add".into(),
                (|a| Ok(Rc::new(OpAdd::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "sub".into(),
                (|a| Ok(Rc::new(OpSub::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "mul".into(),
                (|a| Ok(Rc::new(OpMul::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "div".into(),
                (|a| Ok(Rc::new(OpDiv::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "rem".into(),
                (|a| Ok(Rc::new(OpRem::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "band".into(),
                (|a| Ok(Rc::new(OpBand::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "bor".into(),
                (|a| Ok(Rc::new(OpBor::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "bxor".into(),
                (|a| Ok(Rc::new(OpBxor::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "bshl".into(),
                (|a| Ok(Rc::new(OpBshl::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "bshr".into(),
                (|a| Ok(Rc::new(OpBshr::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "teq".into(),
                (|a| Ok(Rc::new(OpTeq::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tneq".into(),
                (|a| Ok(Rc::new(OpTneq::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tgt".into(),
                (|a| Ok(Rc::new(OpTgt::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tgeq".into(),
                (|a| Ok(Rc::new(OpTgeq::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tlt".into(),
                (|a| Ok(Rc::new(OpTlt::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
            (
                "tleq".into(),
                (|a| Ok(Rc::new(OpTleq::try_from(a)?) as Rc<dyn ToInstruction>)) as FnInst,
            ),
        ]);

        Self {
            tokens: Vec::new(),
            inst,
            label_regex: regex::Regex::new("^[a-z](a-z0-9_)*").unwrap(),
        }
    }

    fn trim_line(line: &str) -> &str {
        let s = line.trim();
        if let Some(ind) = s.find(';') {
            &s[..ind]
        } else {
            s
        }
    }

    pub fn parse_line(&mut self, line: &str, loc: LocationInfo) -> Result<(), AssemblerError> {
        // Trim Comments
        let s = Self::trim_line(line);

        let words = s.split_whitespace().collect::<Vec<_>>();

        let first: &str = if let Some(w) = words.first() {
            w
        } else {
            return Ok(()); // Empty Instruction
        };

        let tok = if let Some(op) = first.strip_prefix('.') {
            let args = &words[1..];

            if args.len() != 1 {
                return Err(AssemblerError::ArgumentCountMismatch(args.len(), 1));
            }

            let arg = args[0];

            match op {
                "oper" => Token::ChangeAddress(parse_imm_u32(arg)?),
                "loadloc" => Token::LoadLoc(arg.to_string()),
                "u8" => Token::Immediate1(parse_imm_u8(arg)?),
                "u16" => Token::Immediate2(parse_imm_u16(arg)?),
                "u32" => Token::Immediate4(parse_imm_u32(arg)?),
                "i8" => Token::Immediate1(parse_imm_i8(arg)? as u8),
                "i16" => Token::Immediate2(parse_imm_i16(arg)? as u16),
                "i32" => Token::Immediate4(parse_imm_i32(arg)? as u32),
                "f32" => Token::Immediate4(
                    match arg.parse::<f32>() {
                        Ok(v) => v,
                        Err(_) => return Err(ImmediateError(arg.to_string()).into()),
                    }
                    .to_bits(),
                ),
                _ => return Err(AssemblerError::UnknownInstruction(op.to_string())),
            }
        } else if let Some(lbl) = first.strip_prefix(':') {
            if !self.label_regex.is_match(lbl) {
                return Err(AssemblerError::BadLabel(lbl.to_string()));
            }

            if words.len() != 1 {
                return Err(AssemblerError::ArgumentCountMismatch(words.len(), 1));
            }

            Token::CreateLabel(lbl.to_string())
        } else if let Some(inst_fn) = self.inst.get(words[0]) {
            let args = &words[1..];
            Token::Operation(*inst_fn, args.iter().map(|s| s.to_string()).collect())
        } else {
            return Err(AssemblerError::UnknownInstruction(line.into()));
        };

        self.tokens.push(TokenLoc { tok, loc });

        Ok(())
    }

    pub fn add_token(&mut self, tok: TokenLoc) {
        self.tokens.push(tok)
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, AssemblerErrorLoc> {
        let mut state = ParserState::new();

        for t in self.tokens.iter() {
            match &t.tok {
                Token::ChangeAddress(new_addr) => {
                    if *new_addr < state.addr {
                        panic!("cannot backup address");
                    } else {
                        state.addr = *new_addr;
                    }
                }
                Token::Immediate1(i) => {
                    state.add_bytes(&[*i]);
                }
                Token::Immediate2(i) => {
                    state.add_bytes(&i.to_be_bytes());
                }
                Token::Immediate4(i) => {
                    state.add_bytes(&i.to_be_bytes());
                }
                Token::CreateLabel(lbl) => {
                    if state.labels.contains_key(lbl) {
                        return Err(AssemblerErrorLoc { err: AssemblerError::DuplicateLabel(lbl.to_string()), loc: t.loc.clone() });
                    }
                    state.labels.insert(lbl.into(), state.addr);
                }
                Token::LoadLoc(lbl) => {
                    state.loadloc_vals.insert(state.addr, lbl.into());
                    state.add_bytes(&0u32.to_be_bytes());
                }
                Token::Operation(func, args) => {
                    state.oper_vals.insert(state.addr, (*func, args.to_owned(), t.loc.clone()));
                    state.add_bytes(&0u32.to_be_bytes());
                }
            }
        }

        for (addr, lbl) in state.loadloc_vals {
            if let Some(loc) = state.labels.get(&lbl) {
                for (i, val) in loc.to_be_bytes().iter().enumerate() {
                    state.values.insert(addr + i as u32, *val);
                }
            } else {
                panic!("no label for {lbl} found");
            }
        }

        for (addr, (inst_fn, args, loc)) in state.oper_vals {
            let mut new_args = Vec::new();

            for a in args.iter() {
                let na = if let Some(v) = state.labels.get(a) {
                    format!("{v}")
                } else {
                    a.to_string()
                };

                new_args.push(na);
            }

            let val = match inst_fn(new_args) {
                Ok(v) => v,
                Err(err) => return Err(AssemblerErrorLoc { err: err.into(), loc }),
            };

            let inst = val.to_instruction();

            for (i, val) in inst.iter().enumerate() {
                state.values.insert(addr + i as u32, *val);
            }
        }

        let mut bytes = Vec::new();

        if let Some(max_addr) = state.values.keys().max() {
            bytes.resize(*max_addr as usize + 1, 0);

            for (a, v) in state.values {
                bytes[a as usize] = v;
            }
        }

        Ok(bytes)
    }
}

#[derive(Default)]
pub struct ParserState {
    addr: u32,
    labels: HashMap<String, u32>,
    values: HashMap<u32, u8>,
    loadloc_vals: HashMap<u32, String>,
    oper_vals: HashMap<u32, (FnInst, Vec<String>, LocationInfo)>,
}

impl ParserState {
    pub fn new() -> Self {
        Self::default()
    }

    fn add_bytes(&mut self, vals: &[u8]) {
        self.align_boundary(vals.len() as u32);
        for v in vals {
            self.values.insert(self.addr, *v);
            self.addr += 1;
        }
    }

    fn align_boundary(&mut self, val: u32) {
        if val > 0 && self.addr % val != 0 {
            self.addr += val - (self.addr % val);
        }
    }
}

pub fn parse_text(txt: &str) -> Result<Vec<u8>, AssemblerErrorLoc> {
    parse_lines(&txt.lines().collect::<Vec<_>>())
}

pub fn parse_lines(txt: &[&str]) -> Result<Vec<u8>, AssemblerErrorLoc> {
    let mut state = TokenList::new();
    for (i, l) in txt.iter().enumerate() {
        let loc: LocationInfo = LocationInfo { line: i + 1, full_line: Some(l.to_string()) };
        if let Err(e) = state.parse_line(&l.to_lowercase(), loc.clone()) {
            return Err(AssemblerErrorLoc {
                err: e,
                loc,
            });
        }
    }

    state.to_bytes()
}

pub fn assemble(tokens: &[TokenLoc]) -> Result<Vec<u8>, AssemblerErrorLoc> {
    let mut state = TokenList::new();
    for t in tokens {
        state.add_token(t.clone());
    }
    state.to_bytes()
}
