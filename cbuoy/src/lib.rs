use jib_asm::{AssemblerErrorLoc, TokenLoc};

mod components;
mod parser;
mod tokenizer;
mod types;

pub fn compile(s: &str) -> Result<Vec<u8>, String> {
    let state = match parser::parse(s) {
        Ok(s) => s,
        Err(e) => return Err(format!("Parse error - {e}")),
    };

    Ok(state.generate_code())
}

pub fn assemble(_s: &str) -> Result<Vec<TokenLoc>, AssemblerErrorLoc> {
    panic!("compiling to assembly not yet fully supported");
}
