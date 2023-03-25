use crate::lexer::lexer;
use crate::tokenizer::tokenize;

pub fn compile(text: &str) -> Result<Vec<String>, String> {
    let tokens = match tokenize(text) {
        Ok(v) => v,
        Err(e) => return Err(format!("Tokenizer Error: {0:}", e)),
    };

    match lexer(tokens) {
        Ok(v) => {
            Ok(v)
        }
        Err(e) => {
            println!("Error: {0:}", e);
            Err(e)
        }
    }
}
