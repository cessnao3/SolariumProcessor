use crate::tokenizer::tokenize;
use crate::lexer::lexer;

pub fn compile(text: &str) -> Result<Vec<String>, String>
{
    let tokens = match tokenize(text)
    {
        Ok(v) => v,
        Err(e) => return Err(format!("Tokenizer Error: {0:}", e))
    };

    println!("Tokens:");
    for t in &tokens
    {
        println!("  {0:}", t.to_string());
    }

    println!("Compiling...");
    match lexer(tokens)
    {
        Ok(v) =>
        {
            for s in v
            {
                println!("  {0:}", s);
            }
        },
        Err(e) => println!("Error: {0:}", e)
    };

    return Ok(Vec::new());
}
