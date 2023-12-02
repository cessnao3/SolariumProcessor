use sol32::cpu::{Opcode, Processor};

enum AssemblerError {
    UnknownLabel(String),
}

struct AssemblerErrorLoc {
    err: AssemblerError,
    loc: usize,
}

enum Instruction {
    Arguments()
}

enum Token {
    Operation,
    CreateLabel(String),
    LoadLoc(String),
    Immediate1(u8),
    Immediate2(u16),
    Immediate4(u32),
}

struct ParserState {
    tokens: Vec<Token>,
    current_loc: usize,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current_loc: 0,
        }
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), AssemblerError> {
        // .oper pos / .loadloc label
        // .u32/.u16/.u8/.i32/.i16/.i8/.f32 -> Will add in-place, but next will be aligned to 1/2/4-byte boundary
        // :location_label
        // commands!
    }
}

pub fn assemble(txt: &str) -> Result<(), AssemblerErrorLoc> {
    assemble_lines(&txt.lines().collect::<Vec<_>>())
}

pub fn assemble_lines(lines: &[&str]) -> Result<(), AssemblerErrorLoc> {
    Ok(())
}
