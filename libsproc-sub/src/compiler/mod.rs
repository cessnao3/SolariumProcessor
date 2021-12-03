use crate::tokenizer::tokenize;
use crate::lexer::lexer;

pub fn compile(text: &str) -> Result<Vec<String>, String>
{
    let tokens = match tokenize(text)
    {
        Ok(v) => v,
        Err(e) => return Err(format!("Tokenizer Error: {0:}", e))
    };

    let assembly_result;

    match lexer(tokens)
    {
        Ok(v) =>
        {
            assembly_result = Ok(v);
        },
        Err(e) => {
            println!("Error: {0:}", e);
            assembly_result = Err(e);
        }
    };

    return assembly_result;
}
