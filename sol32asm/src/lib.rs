mod argument;
mod immediate;
mod instruction;

use std::collections::HashMap;

use instruction::{
    OpAdd, OpBand, OpBool, OpBor, OpBshl, OpBshr, OpBxor, OpCall, OpConv, OpCopy, OpDiv, OpHalt,
    OpInt, OpIntr, OpJmp, OpJmpr, OpJmpri, OpLoad, OpLoadi, OpLoadr, OpLoadri, OpMul, OpNoop,
    OpNot, OpPop, OpPopr, OpPush, OpRem, OpReset, OpRet, OpRetInt, OpSave, OpSaver, OpSub, OpTeq,
    OpTgeq, OpTgt, OpTleq, OpTlt, OpTneq, OpTnz, OpTz, ToInstruction, InstructionError,
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

/*
static INSTRUCTIONS: Lazy<HashMap<i32, fn(&[&str]) -> Result<dyn ToInstruction, InstructionError>>> = Lazy::new(|| {
    Mutex::new(HashMap::from([
        ("noop", OpNoop::try_from),
        ("reset", Box::<dyn ToInstruction>::new(OpReset)),
        ("retint", Box::<dyn ToInstruction>::new(OpRetInt)),
        ("ret", Box::<dyn ToInstruction>::new(OpRet)),
        ("halt", Box::<dyn ToInstruction>::new(OpHalt)),
        ("int", Box::<dyn ToInstruction>::new(OpInt)),
        ("intr", Box::<dyn ToInstruction>::new(OpIntr)),
        ("call", Box::<dyn ToInstruction>::new(OpCall)),
        ("push", Box::<dyn ToInstruction>::new(OpPush)),
        ("pop", Box::<dyn ToInstruction>::new(OpPop)),
        ("popr", Box::<dyn ToInstruction>::new(OpPopr)),
        ("jmp", Box::<dyn ToInstruction>::new(OpJmp)),
        ("jmpr", Box::<dyn ToInstruction>::new(OpJmpr)),
        ("jmpri", Box::<dyn ToInstruction>::new(OpJmpri)),
        ("loadi", Box::<dyn ToInstruction>::new(OpLoadi)),
        ("loadri", Box::<dyn ToInstruction>::new(OpLoadri)),
        ("not", Box::<dyn ToInstruction>::new(OpNot)),
        ("bool", Box::<dyn ToInstruction>::new(OpBool)),
        ("tz", Box::<dyn ToInstruction>::new(OpTz)),
        ("tnz", Box::<dyn ToInstruction>::new(OpTnz)),
        ("copy", Box::<dyn ToInstruction>::new(OpCopy)),
        ("save", Box::<dyn ToInstruction>::new(OpSave)),
        ("saver", Box::<dyn ToInstruction>::new(OpSaver)),
        ("load", Box::<dyn ToInstruction>::new(OpLoad)),
        ("loadr", Box::<dyn ToInstruction>::new(OpLoadr)),
        ("conv", Box::<dyn ToInstruction>::new(OpConv)),
        ("add", Box::<dyn ToInstruction>::new(OpAdd)),
        ("sub", Box::<dyn ToInstruction>::new(OpSub)),
        ("mul", Box::<dyn ToInstruction>::new(OpMul)),
        ("div", Box::<dyn ToInstruction>::new(OpDiv)),
        ("rem", Box::<dyn ToInstruction>::new(OpRem)),
        ("band", Box::<dyn ToInstruction>::new(OpBand)),
        ("bor", Box::<dyn ToInstruction>::new(OpBor)),
        ("bxor", Box::<dyn ToInstruction>::new(OpBxor)),
        ("bshl", Box::<dyn ToInstruction>::new(OpBshl)),
        ("bshr", Box::<dyn ToInstruction>::new(OpBshr)),
        ("teq", Box::<dyn ToInstruction>::new(OpTeq)),
        ("tneq", Box::<dyn ToInstruction>::new(OpTneq)),
        ("tgt", Box::<dyn ToInstruction>::new(OpTgt)),
        ("tgeq", Box::<dyn ToInstruction>::new(OpTgeq)),
        ("tlt", Box::<dyn ToInstruction>::new(OpTlt)),
        ("tleq", Box::<dyn ToInstruction>::new(OpTleq)),
    ]))
});
*/

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

type FnInst = fn(&[&str]) -> Result<Box<dyn ToInstruction>, InstructionError>;

enum Token {
    ChangeAddress(u32),
    Operation(Box<dyn ToInstruction>),
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

struct ParserState {
    tokens: Vec<Token>,
    labels: HashMap<String, u32>,
    current_loc: usize,
    inst: HashMap<String, FnInst>,
}

impl ParserState {
    pub fn new() -> Self {
        fn convert_type<'a, T: ToInstruction + TryFrom<&'a [&'a str]>>(s: &[&str]) -> Result<Box<dyn ToInstruction>, T::Error> {
            Ok(Box::new(T::try_from(s)?))
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
            labels: HashMap::new(),
            current_loc: 0,
            inst,
        }
    }

    fn parse_line(&mut self, line: &str) -> Result<(), AssemblerError> {
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
            }
        } else if first.starts_with(':') {
            if words.len() != 1 {
                return Err(AssemblerError::ArgumentCountMismatch(words.len(), 1));
            }

            Token::CreateLabel(first[1..].to_string())
        } else if let Some(inst_fn) = self.inst.get(words[0]) {
            let args = &words[1..];
            let val = inst_fn(args)?;
            Token::Operation(val)
        } else {
            return Err(AssemblerError::UnknownInstruction(line.into()));
        };

        self.tokens.push(tok);

        Ok(())
    }
}

pub fn parse_text(txt: &str) -> Result<Vec<u8>, AssemblerErrorLoc> {
    parse_lines(&txt.lines().collect::<Vec<_>>())
}

pub fn parse_lines(txt: &[&str]) -> Result<Vec<u8>, AssemblerErrorLoc> {
    for (i, l) in txt.iter().enumerate() {

    }
}

pub fn assemble(txt: &[Token]) -> Result<Vec<u8>, AssemblerError> {
    Ok(Vec::new())
}
