use crate::tokenizer::tokenize;

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

    return Ok(Vec::new());
}
