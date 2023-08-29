mod components;
mod parser;
mod types;

pub fn compile(s: &str) -> Vec<u16> {
    parser::parse(s);
    Vec::new()
}

pub fn assemble(s: &str) -> Vec<sda::AssemblerCommand>
{
    Vec::new()
}
