use jasm::{AssemblerErrorLoc, TokenLoc};

mod components;
mod parser;
mod tokenizer;
mod types;

pub fn compile(s: &str) -> Result<Vec<u32>, String> {
    match parser::parse(s) {
        Ok(_) => (),
        Err(e) => return Err(format!("Parse error - {e}")),
    }
    Err("compiling not yet fully supported".into())
}

pub fn assemble(_s: &str) -> Result<Vec<TokenLoc>, AssemblerErrorLoc> {
    panic!("compiling to assembly not yet fully supported");
}
