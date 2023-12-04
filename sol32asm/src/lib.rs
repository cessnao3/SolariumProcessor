mod argument;
mod immediate;
mod instruction;

use std::{collections::HashMap, rc::Rc};

use instruction::{
    InstructionError, OpAdd, OpBand, OpBool, OpBor, OpBshl, OpBshr, OpBxor, OpCall, OpConv, OpCopy,
    OpDiv, OpHalt, OpInt, OpIntr, OpJmp, OpJmpr, OpJmpri, OpLoad, OpLoadi, OpLoadr, OpLoadri,
    OpMul, OpNoop, OpNot, OpPop, OpPopr, OpPush, OpRem, OpReset, OpRet, OpRetInt, OpSave, OpSaver,
    OpSub, OpTeq, OpTgeq, OpTgt, OpTleq, OpTlt, OpTneq, OpTnz, OpTz, ToInstruction,
};

use immediate::{
    parse_imm_i16, parse_imm_i32, parse_imm_i8, parse_imm_u16, parse_imm_u32, parse_imm_u8,
    ImmediateError,
};

#[derive(Debug, Clone)]
enum AssemblerError {
    UnknownLabel(String),
    UnknownInstruction(String),
    Instruction(InstructionError),
    ArgumentCountMismatch(usize, usize),
    ImmediateError(String),
    InvalidLabel(String),
    UnknownRegister(String),
    UnknownDataType(String),
}

impl From<ImmediateError> for AssemblerError {
    fn from(value: ImmediateError) -> Self {
        Self::ImmediateError(value.0)
    }
}

impl From<InstructionError> for AssemblerError {
    fn from(value: InstructionError) -> Self {
        Self::Instruction(value)
    }
}

#[derive(Debug, Clone)]
struct AssemblerErrorLoc {
    err: AssemblerError,
    loc: usize,
}

type FnInst = fn(Vec<String>) -> Result<Rc<dyn ToInstruction>, InstructionError>;

#[derive(Clone)]
enum Token {
    ChangeAddress(u32),
    Operation(Rc<dyn ToInstruction>),
    CreateLabel(String),
    LoadLoc(String),
    Immediate1(u8),
    Immediate2(u16),
    Immediate4(u32),
}

impl Token {
    pub fn to_bytes(&self) -> Option<Vec<u8>> {
        match self {
            Self::Operation(inst) => Some(Vec::new()),
            Self::Immediate1(v) => Some(vec![*v]),
            Self::Immediate2(v) => Some(v.to_be_bytes().into_iter().collect()),
            Self::Immediate4(v) => Some(v.to_be_bytes().into_iter().collect()),
            _ => None,
        }
    }
}

struct TokenList {
    tokens: Vec<Token>,
    inst: HashMap<String, FnInst>,
}

impl TokenList {
    pub fn new() -> Self {
        fn convert_type<T: ToInstruction + TryFrom<Vec<String>>>(
            s: Vec<String>,
        ) -> Result<Rc<dyn ToInstruction>, T::Error> {
            let v = T::try_from(s)?;
            let r = Rc::new(v);
            Ok(r)
        }

        let inst = HashMap::from([
            ("noop".into(), convert_type::<OpNoop> as FnInst),
            ("reset".into(), convert_type::<OpReset> as FnInst),
            ("retint".into(), convert_type::<OpRetInt> as FnInst),
            ("ret".into(), convert_type::<OpRet> as FnInst),
            ("halt".into(), convert_type::<OpHalt> as FnInst),
            ("int".into(), convert_type::<OpInt> as FnInst),
            ("intr".into(), convert_type::<OpIntr> as FnInst),
            ("call".into(), convert_type::<OpCall> as FnInst),
            ("push".into(), convert_type::<OpPush> as FnInst),
            ("pop".into(), convert_type::<OpPop> as FnInst),
            ("popr".into(), convert_type::<OpPopr> as FnInst),
            ("jmp".into(), convert_type::<OpJmp> as FnInst),
            ("jmpr".into(), convert_type::<OpJmpr> as FnInst),
            ("jmpri".into(), convert_type::<OpJmpri> as FnInst),
            ("loadi".into(), convert_type::<OpLoadi> as FnInst),
            ("loadri".into(), convert_type::<OpLoadri> as FnInst),
            ("not".into(), convert_type::<OpNot> as FnInst),
            ("bool".into(), convert_type::<OpBool> as FnInst),
            ("tz".into(), convert_type::<OpTz> as FnInst),
            ("tnz".into(), convert_type::<OpTnz> as FnInst),
            ("copy".into(), convert_type::<OpCopy> as FnInst),
            ("save".into(), convert_type::<OpSave> as FnInst),
            ("saver".into(), convert_type::<OpSaver> as FnInst),
            ("load".into(), convert_type::<OpLoad> as FnInst),
            ("loadr".into(), convert_type::<OpLoadr> as FnInst),
            ("conv".into(), convert_type::<OpConv> as FnInst),
            ("add".into(), convert_type::<OpAdd> as FnInst),
            ("sub".into(), convert_type::<OpSub> as FnInst),
            ("mul".into(), convert_type::<OpMul> as FnInst),
            ("div".into(), convert_type::<OpDiv> as FnInst),
            ("rem".into(), convert_type::<OpRem> as FnInst),
            ("band".into(), convert_type::<OpBand> as FnInst),
            ("bor".into(), convert_type::<OpBor> as FnInst),
            ("bxor".into(), convert_type::<OpBxor> as FnInst),
            ("bshl".into(), convert_type::<OpBshl> as FnInst),
            ("bshr".into(), convert_type::<OpBshr> as FnInst),
            ("teq".into(), convert_type::<OpTeq> as FnInst),
            ("tneq".into(), convert_type::<OpTneq> as FnInst),
            ("tgt".into(), convert_type::<OpTgt> as FnInst),
            ("tgeq".into(), convert_type::<OpTgeq> as FnInst),
            ("tlt".into(), convert_type::<OpTlt> as FnInst),
            ("tleq".into(), convert_type::<OpTleq> as FnInst),
        ]);

        Self {
            tokens: Vec::new(),
            inst,
        }
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), AssemblerError> {
        let s = line.trim();
        let words = s.split_whitespace().collect::<Vec<_>>();

        let first: &str = if let Some(w) = words.first() {
            *w
        } else {
            return Err(AssemblerError::UnknownInstruction(s.to_string()));
        };

        let tok = if first.starts_with('.') {
            let op = &first[1..];
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
                        Err(_) => return Err(AssemblerError::ImmediateError(arg.to_string())),
                    }
                    .to_bits(),
                ),
                _ => return Err(AssemblerError::UnknownInstruction(op.to_string())),
            }
        } else if first.starts_with(':') {
            if words.len() != 1 {
                return Err(AssemblerError::ArgumentCountMismatch(words.len(), 1));
            }

            Token::CreateLabel(first[1..].to_string())
        } else if let Some(inst_fn) = self.inst.get(words[0]) {
            let args = &words[1..];
            let val = inst_fn(args.iter().map(|s| s.to_string()).collect())?;
            Token::Operation(val)
        } else {
            return Err(AssemblerError::UnknownInstruction(line.into()));
        };

        self.tokens.push(tok);

        Ok(())
    }

    pub fn add_token(&mut self, tok: Token) {
        self.tokens.push(tok)
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, AssemblerError> {
        let mut state = ParserState::new();

        for t in self.tokens.iter() {
            match t {
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
                    // TODO - Duplicate label
                    state.labels.insert(lbl.into(), state.addr);
                }
                Token::LoadLoc(lbl) => {
                    state.loadloc_vals.insert(state.addr, lbl.into());
                    state.add_bytes(&0u32.to_be_bytes());
                }
                Token::Operation(op) => {
                    state.add_bytes(&op.to_instruction());
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

        let mut bytes = Vec::new();

        if let Some(max_addr) = state.values.keys().max() {
            bytes.resize(*max_addr as usize, 0);

            for (a, v) in state.values {
                bytes[a as usize] = v;
            }
        }

        Ok(bytes)
    }
}

pub struct ParserState {
    addr: u32,
    labels: HashMap<String, u32>,
    values: HashMap<u32, u8>,
    loadloc_vals: HashMap<u32, String>,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            addr: 0,
            labels: HashMap::new(),
            values: HashMap::new(),
            loadloc_vals: HashMap::new(),
        }
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
        match state.parse_line(l) {
            Err(e) => return Err(AssemblerErrorLoc { err: e, loc: i + 1 }),
            _ => (),
        };
    }

    match state.to_bytes() {
        Ok(v) => Ok(v),
        Err(e) => Err(AssemblerErrorLoc { err: e, loc: 0 }),
    }
}

pub fn assemble(tokens: &[Token]) -> Result<Vec<u8>, AssemblerError> {
    let mut state = TokenList::new();
    for t in tokens {
        state.add_token(t.clone());
    }
    state.to_bytes()
}
