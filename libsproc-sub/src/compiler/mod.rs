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

    let assembly_result;

    println!("Compiling...");
    match lexer(tokens)
    {
        Ok(v) =>
        {
            for s in v.iter()
            {
                println!("  {0:}", s);
            }
            assembly_result = Ok(v);
        },
        Err(e) => {
            println!("Error: {0:}", e);
            assembly_result = Err(e);
        }
    };

    return assembly_result;
}
