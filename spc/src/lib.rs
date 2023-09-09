mod components;
mod parser;
mod tokenizer;
mod types;

pub fn compile(s: &str) -> Result<Vec<u16>, String> {
    match parser::parse(s) {
        Ok(_) => (),
        Err(e) => return Err(format!("parse error - {e}")),
    }
    Err("compiling not yet fully supported".into())
}

pub fn assemble(_s: &str) -> Result<Vec<sda::AssemblerCommand>, String> {
    Err("compiling to assembly not yet fully supported".into())
}
