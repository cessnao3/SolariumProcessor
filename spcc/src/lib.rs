mod compiler;
mod lexer;
mod tokenizer;

pub use crate::compiler::compile;

#[cfg(test)]
mod tests {}
