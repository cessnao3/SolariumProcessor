pub mod argument;
mod immediate;
pub mod instructions;

use core::fmt;
use std::{collections::HashMap, rc::Rc};

use instructions::{
    Instruction, InstructionError, OpAdd, OpBand, OpBnot, OpBool, OpBor, OpBshl, OpBshr, OpBxor,
    OpCall, OpConv, OpCopy, OpDiv, OpHalt, OpInt, OpIntoff, OpInton, OpIntr, OpJmp, OpJmpr,
    OpJmpri, OpLd, OpLdi, OpLdn, OpLdr, OpLdri, OpMul, OpNeg, OpNoop, OpNot, OpPop, OpPopr, OpPush,
    OpRem, OpReset, OpRet, OpRetInt, OpSav, OpSavr, OpSub, OpTeq, OpTg, OpTge, OpTl, OpTle, OpTneq,
    OpTnz, OpTz,
};

use jib::cpu::{Opcode, Processor, ProcessorError};

use immediate::{
    parse_imm_i16, parse_imm_i32, parse_imm_i8, parse_imm_u16, parse_imm_u32, parse_imm_u8,
    ImmediateError,
};

#[derive(Debug, Clone)]
pub enum AssemblerError {
    UnknownLabel(String),
    UnknownInstruction(String, Option<usize>),
    Instruction(InstructionError),
    ArgumentCountMismatch(usize, usize),
    CannotBackupAddress(u32),
    InvalidLabel(String),
    Immediate(ImmediateError),
    BadLabel(String),
    DuplicateLabel(String),
    Character(jib::text::CharacterError),
    AddressTaken(u32),
    Parser(ParseError),
    Processor(ProcessorError),
}

impl fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownLabel(l) => write!(f, "Unknown Label {l}"),
            Self::UnknownInstruction(i, None) => write!(f, "Unknown Instruction {i}"),
            Self::UnknownInstruction(i, Some(len)) => {
                write!(f, "Unknown Instruction {i} with length {len}")
            }
            Self::ArgumentCountMismatch(num, expected) => {
                write!(f, "Argument Count Expected {expected}, found {num}")
            }
            Self::InvalidLabel(l) => write!(f, "Invalid Label {l}"),
            Self::Instruction(i) => write!(f, "Instruction Error => {i}"),
            Self::Immediate(i) => write!(f, "Immediate Error => {i}"),
            Self::BadLabel(l) => write!(f, "Bad Label '{l}'"),
            Self::DuplicateLabel(l) => write!(f, "Duplicate Label '{l}'"),
            Self::Character(c) => write!(f, "Character Error => {c}"),
            Self::AddressTaken(addr) => write!(f, "Address 0x{addr:08x} Taken"),
            Self::Parser(e) => write!(f, "Parser Error - {e}"),
            Self::Processor(e) => write!(f, "Processor Error - {e}"),
            Self::CannotBackupAddress(addr) => {
                write!(f, "Cannot Backup Address - Already Passed {addr}")
            }
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

impl From<jib::text::CharacterError> for AssemblerError {
    fn from(value: jib::text::CharacterError) -> Self {
        Self::Character(value)
    }
}

impl From<ProcessorError> for AssemblerError {
    fn from(value: ProcessorError) -> Self {
        Self::Processor(value)
    }
}

impl From<ParseError> for AssemblerError {
    fn from(value: ParseError) -> Self {
        Self::Parser(value)
    }
}

#[derive(Debug, Clone)]
pub struct LocationInfo {
    pub line: usize,
    pub full_line: Option<String>,
    pub base_loc: Option<Box<LocationInfo>>,
}

impl fmt::Display for LocationInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.line)?;
        if let Some(txt) = &self.full_line {
            write!(f, " {}", txt)
        } else {
            Ok(())
        }
    }
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

type FnInst = fn(Vec<String>) -> Result<Rc<dyn Instruction>, InstructionError>;
type FnDisp = fn([u8; 4]) -> Option<String>;

pub trait FromLiteral<T> {
    fn from_literal(v: T) -> Self;
}

pub enum Token {
    ChangeAddress(u32),
    Operation(FnInst, Vec<String>),
    OperationLiteral(Box<dyn Instruction>),
    CreateLabel(String),
    LoadLoc(String),
    Literal1(u8),
    Literal2(u16),
    Literal4(u32),
    LiteralText(String),
    AlignInstruction,
}

impl FromLiteral<u8> for Token {
    fn from_literal(v: u8) -> Self {
        Self::Literal1(v)
    }
}

impl FromLiteral<i8> for Token {
    fn from_literal(v: i8) -> Self {
        Self::Literal1(v as u8)
    }
}

impl FromLiteral<u16> for Token {
    fn from_literal(v: u16) -> Self {
        Self::Literal2(v)
    }
}

impl FromLiteral<i16> for Token {
    fn from_literal(v: i16) -> Self {
        Self::Literal2(v as u16)
    }
}

impl FromLiteral<u32> for Token {
    fn from_literal(v: u32) -> Self {
        Self::Literal4(v)
    }
}

impl FromLiteral<i32> for Token {
    fn from_literal(v: i32) -> Self {
        Self::Literal4(v as u32)
    }
}

impl FromLiteral<f32> for Token {
    fn from_literal(v: f32) -> Self {
        Self::Literal4(v.to_bits())
    }
}

pub struct TokenLoc {
    pub tok: Token,
    pub loc: LocationInfo,
}

pub struct InstructionList {
    inst_map: HashMap<String, FnInst>,
    name_map: HashMap<Opcode, String>,
    disp_map: HashMap<Opcode, FnDisp>,
}

macro_rules! create_instruction_map {
    ($($op:ident),*) => {
        Vec::<(Opcode, String, FnInst, FnDisp)>::from([
            $( { (
                $op::OP,
                $op::name().into(),
                (|a| Ok(Rc::new($op::try_from(a)?) as Rc<dyn Instruction>)) as FnInst,
                (|b| { let res = $op::try_from(b); if let Ok(s) = res { Some(s.to_string()) } else { None } }) as FnDisp )
            } ),*
        ])
    };
}

impl InstructionList {
    pub fn get_name_for_opcode(&self, op: &Opcode) -> Option<&String> {
        self.name_map.get(op)
    }

    pub fn get_instruction(&self, s: &str) -> Option<&FnInst> {
        self.inst_map.get(s)
    }

    pub fn get_display(&self, inst: [u8; 4]) -> Option<String> {
        let op = Opcode::from(inst[0]);
        if let Some(f) = self.disp_map.get(&op) {
            f(inst)
        } else {
            None
        }
    }

    pub fn get_display_inst(&self, inst: u32) -> Option<String> {
        self.get_display(inst.to_be_bytes())
    }
}

impl Default for InstructionList {
    fn default() -> Self {
        let inst = create_instruction_map!(
            OpAdd, OpBand, OpBnot, OpBool, OpBor, OpBshl, OpBshr, OpBxor, OpCall, OpConv, OpCopy,
            OpDiv, OpHalt, OpInt, OpIntoff, OpInton, OpIntr, OpJmp, OpJmpr, OpJmpri, OpLd, OpLdi,
            OpLdn, OpLdr, OpLdri, OpMul, OpNeg, OpNoop, OpNot, OpPop, OpPopr, OpPush, OpRem,
            OpReset, OpRet, OpRetInt, OpSav, OpSavr, OpSub, OpTeq, OpTg, OpTge, OpTl, OpTle,
            OpTneq, OpTnz, OpTz
        );

        let inst_map = inst.iter().map(|(_, n, f, _)| (n.to_owned(), *f)).collect();
        let name_map = inst.iter().map(|(o, n, _, _)| (*o, n.to_owned())).collect();
        let disp_map = inst.iter().map(|(o, _, _, d)| (*o, *d)).collect();

        Self {
            inst_map,
            name_map,
            disp_map,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseError {
    UnknownEscapeCode(char),
    WithinQuote,
    WithinEscape,
    ExpectedSpaceBetweenQuote,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownEscapeCode(c) => write!(f, "Unknown escape code '\\{c}'"),
            Self::WithinQuote => write!(f, "Parser ended within a quote"),
            Self::WithinEscape => write!(f, "Parser ending with an unfinished escape code"),
            Self::ExpectedSpaceBetweenQuote => write!(f, "Expected space between quote"),
        }
    }
}

pub struct TokenList {
    tokens: Vec<TokenLoc>,
    inst: InstructionList,
    label_regex: regex::Regex,
}

impl TokenList {
    fn trim_line(line: &str) -> &str {
        let s = line.trim();
        if let Some(ind) = s.find(';') {
            &s[..ind]
        } else {
            s
        }
    }

    fn split_asm_delim(s: &str) -> Result<Vec<String>, ParseError> {
        let mut within_quote = false;
        let mut is_escape = false;
        let mut last_was_quote = false;

        let mut so_far = Vec::<char>::new();
        let mut words = Vec::new();

        for c in s.chars() {
            if last_was_quote {
                if c.is_whitespace() {
                    last_was_quote = false;
                } else {
                    return Err(ParseError::ExpectedSpaceBetweenQuote);
                }
            } else if within_quote {
                if is_escape {
                    if c == '\\' {
                        so_far.push('\\');
                    } else if c == 'n' {
                        so_far.push('\n');
                    } else if c == '"' {
                        so_far.push('"')
                    } else {
                        return Err(ParseError::UnknownEscapeCode(c));
                    }

                    is_escape = false;
                } else if c == '\\' {
                    is_escape = true;
                } else if c == '"' {
                    within_quote = false;
                    last_was_quote = true;
                    if !so_far.is_empty() {
                        words.push(so_far.iter().collect());
                        so_far.clear();
                    }
                } else {
                    so_far.push(c);
                }
            } else if c == '"' {
                if !so_far.is_empty() {
                    return Err(ParseError::ExpectedSpaceBetweenQuote);
                }
                within_quote = true;
            } else if c.is_whitespace() {
                if !so_far.is_empty() {
                    words.push(so_far.iter().collect());
                    so_far.clear();
                }
            } else {
                so_far.push(c);
            }
        }

        if within_quote {
            return Err(ParseError::WithinQuote);
        } else if is_escape {
            return Err(ParseError::WithinEscape);
        }

        if !so_far.is_empty() {
            words.push(so_far.into_iter().collect());
        }

        Ok(words)
    }

    pub fn parse_line(&mut self, line: &str, loc: LocationInfo) -> Result<(), AssemblerError> {
        // Trim Comments
        let s = Self::trim_line(line);

        let words = Self::split_asm_delim(s)?;

        let first: &str = if let Some(w) = words.first() {
            w
        } else {
            return Ok(()); // Empty Instruction
        };

        let tok = if let Some(op) = first.strip_prefix('.') {
            let args = &words[1..];

            if args.is_empty() {
                match op {
                    "align" => Token::AlignInstruction,
                    _ => {
                        return Err(AssemblerError::UnknownInstruction(
                            op.to_string(),
                            Some(args.len()),
                        ))
                    }
                }
            } else if args.len() == 1 {
                let arg = &args[0];

                match op {
                    "oper" => {
                        let addr = if let Some(r) = arg.strip_prefix('#') {
                            Processor::interrupt_address(jib::cpu::Interrupt::Hardware(
                                parse_imm_u32(r)?,
                            ))?
                        } else if let Some(r) = arg.strip_prefix('@') {
                            Processor::interrupt_address(jib::cpu::Interrupt::Software(
                                parse_imm_u32(r)?,
                            ))?
                        } else {
                            parse_imm_u32(arg)?
                        };

                        Token::ChangeAddress(addr)
                    }
                    "loadloc" => Token::LoadLoc(arg.into()),
                    "text" => Token::LiteralText(arg.into()),
                    "u8" => Token::Literal1(parse_imm_u8(arg)?),
                    "u16" => Token::Literal2(parse_imm_u16(arg)?),
                    "u32" => Token::Literal4(parse_imm_u32(arg)?),
                    "i8" => Token::Literal1(parse_imm_i8(arg)? as u8),
                    "i16" => Token::Literal2(parse_imm_i16(arg)? as u16),
                    "i32" => Token::Literal4(parse_imm_i32(arg)? as u32),
                    "f32" => Token::Literal4(
                        match arg.parse::<f32>() {
                            Ok(v) => v,
                            Err(_) => return Err(ImmediateError(arg.to_string()).into()),
                        }
                        .to_bits(),
                    ),
                    _ => {
                        return Err(AssemblerError::UnknownInstruction(
                            op.to_string(),
                            Some(args.len()),
                        ))
                    }
                }
            } else {
                return Err(AssemblerError::ArgumentCountMismatch(args.len(), 1));
            }
        } else if let Some(lbl) = first.strip_prefix(':') {
            if !self.label_regex.is_match(lbl) {
                return Err(AssemblerError::BadLabel(lbl.to_string()));
            }

            if words.len() != 1 {
                return Err(AssemblerError::ArgumentCountMismatch(words.len(), 1));
            }

            Token::CreateLabel(lbl.to_string())
        } else if let Some(inst_fn) = self.inst.get_instruction(&words[0]) {
            let args = &words[1..];
            Token::Operation(*inst_fn, args.iter().map(|s| s.to_string()).collect())
        } else {
            return Err(AssemblerError::UnknownInstruction(line.into(), None));
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
            let loc = t.loc.clone();

            match &t.tok {
                Token::AlignInstruction => state.align_boundary(Processor::BYTES_PER_WORD),
                Token::OperationLiteral(op) => {
                    state.add_bytes(&op.to_u32().to_be_bytes(), loc)?;
                }
                Token::ChangeAddress(new_addr) => {
                    if *new_addr < state.addr {
                        return Err(AssemblerErrorLoc {
                            err: AssemblerError::CannotBackupAddress(*new_addr),
                            loc,
                        });
                    } else {
                        state.addr = *new_addr;
                    }
                }
                Token::LiteralText(s) => {
                    for c in s.chars() {
                        let bv = match jib::text::character_to_byte(c) {
                            Ok(v) => v,
                            Err(e) => {
                                return Err(AssemblerErrorLoc {
                                    err: AssemblerError::from(e),
                                    loc,
                                })
                            }
                        };
                        state.add_bytes(&[bv], loc.clone())?;
                    }
                    state.add_bytes(&[0], loc.clone())?;
                }
                Token::Literal1(i) => {
                    state.add_bytes(&[*i], loc)?;
                }
                Token::Literal2(i) => {
                    state.add_bytes(&i.to_be_bytes(), loc)?;
                }
                Token::Literal4(i) => {
                    state.add_bytes(&i.to_be_bytes(), loc)?;
                }
                Token::CreateLabel(lbl) => {
                    if state.labels.contains_key(lbl) {
                        return Err(AssemblerErrorLoc {
                            err: AssemblerError::DuplicateLabel(lbl.to_string()),
                            loc,
                        });
                    }
                    state.labels.insert(lbl.into(), state.addr);
                }
                Token::LoadLoc(lbl) => {
                    state.add_delay(DelayToken::LoadLoc { label: lbl.into() }, t.loc.clone())?;
                }
                Token::Operation(func, args) => {
                    state.add_delay(
                        DelayToken::Operation {
                            inst: *func,
                            args: args.to_owned(),
                        },
                        t.loc.clone(),
                    )?;
                }
            }
        }

        state.process_delays()?;

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

impl Default for TokenList {
    fn default() -> Self {
        Self {
            tokens: Vec::new(),
            inst: InstructionList::default(),
            label_regex: regex::Regex::new("^[a-z](a-z0-9_)*").unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
enum DelayToken {
    LoadLoc { label: String },
    Operation { inst: FnInst, args: Vec<String> },
}

#[derive(Default)]
struct ParserState {
    addr: u32,
    labels: HashMap<String, u32>,
    values: HashMap<u32, u8>,
    delay_vals: HashMap<u32, (DelayToken, LocationInfo)>,
}

impl ParserState {
    pub fn new() -> Self {
        Self::default()
    }

    fn add_delay(&mut self, delay: DelayToken, loc: LocationInfo) -> Result<(), AssemblerErrorLoc> {
        let base = self.add_bytes(&0u32.to_be_bytes(), loc.clone())?;

        if self.delay_vals.insert(base, (delay, loc.clone())).is_some() {
            return Err(AssemblerErrorLoc {
                err: AssemblerError::AddressTaken(base),
                loc,
            });
        }

        Ok(())
    }

    fn add_bytes(&mut self, vals: &[u8], loc: LocationInfo) -> Result<u32, AssemblerErrorLoc> {
        self.align_boundary(vals.len() as u32);
        let base = self.addr;
        for v in vals {
            if self.values.insert(self.addr, *v).is_some() {
                return Err(AssemblerErrorLoc {
                    err: AssemblerError::AddressTaken(self.addr),
                    loc,
                });
            }
            self.addr += 1;
        }
        Ok(base)
    }

    fn align_boundary(&mut self, val: u32) {
        if val > 0 && self.addr % val != 0 {
            self.addr += val - (self.addr % val);
        }
    }

    fn process_delays(&mut self) -> Result<(), AssemblerErrorLoc> {
        for (addr, (tok, loc)) in self.delay_vals.iter() {
            let insert_value = match tok {
                DelayToken::LoadLoc { label } => {
                    if let Some(loc) = self.labels.get(label) {
                        *loc
                    } else {
                        return Err(AssemblerErrorLoc {
                            err: AssemblerError::UnknownLabel(label.into()),
                            loc: loc.clone(),
                        });
                    }
                }
                DelayToken::Operation { inst, args } => {
                    // Create new arguments to get relative values for any label parameters
                    let mut new_args = Vec::new();

                    for a in args.iter() {
                        let na = if let Some(v) = self.labels.get(a) {
                            format!("{}", (*v as i32) - (*addr as i32))
                        } else {
                            a.to_string()
                        };

                        new_args.push(na);
                    }

                    // Call the instruction function and obtain the resulting parameters
                    let val = match inst(new_args) {
                        Ok(v) => v,
                        Err(err) => {
                            return Err(AssemblerErrorLoc {
                                err: err.into(),
                                loc: loc.clone(),
                            })
                        }
                    };

                    // Replace the
                    val.to_u32()
                }
            };

            for (i, b) in insert_value.to_be_bytes().iter().enumerate() {
                self.values.insert(addr + i as u32, *b);
            }
        }

        self.delay_vals.clear();

        Ok(())
    }
}

pub fn assemble_text(txt: &str) -> Result<Vec<u8>, AssemblerErrorLoc> {
    assemble_lines(&txt.lines().collect::<Vec<_>>())
}

pub fn assemble_lines(txt: &[&str]) -> Result<Vec<u8>, AssemblerErrorLoc> {
    let mut state = TokenList::default();

    for (i, l) in txt.iter().enumerate() {
        let loc = LocationInfo {
            line: i + 1,
            full_line: Some(l.to_string()),
            base_loc: None,
        };
        if let Err(e) = state.parse_line(&l.to_lowercase(), loc.clone()) {
            return Err(AssemblerErrorLoc { err: e, loc });
        }
    }

    state.to_bytes()
}

pub fn assemble_tokens<T>(tokens: T) -> Result<Vec<u8>, AssemblerErrorLoc>
where
    T: IntoIterator<Item = TokenLoc>,
{
    let mut state = TokenList::default();
    for t in tokens.into_iter() {
        state.add_token(t);
    }
    state.to_bytes()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_counter() {
        let txt = include_str!("../../examples/jasm/counter.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().is_empty());
    }

    #[test]
    fn test_hello_world() {
        let txt = include_str!("../../examples/jasm/hello_world.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().is_empty());
    }

    #[test]
    fn test_infinite_counter() {
        let txt = include_str!("../../examples/jasm/infinite_counter.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().is_empty());
    }

    #[test]
    fn test_serial_echo() {
        let txt = include_str!("../../examples/jasm/serial_echo.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().is_empty());
    }

    #[test]
    fn test_thread_test() {
        let txt = include_str!("../../examples/jasm/thread_test.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().is_empty());
    }
}
