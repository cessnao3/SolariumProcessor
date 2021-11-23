mod keyword;
mod symbol;

#[derive(Debug, Clone)]
pub enum Token
{
    Keyword(keyword::Keyword),
    Symbol(symbol::Symbol),
    IntegerLiteral(i16),
    Name(String)
}

pub fn tokenize(line: &str) -> Result<Vec<Token>, String>
{
    let mut so_far: Vec<char> = Vec::new();

    for c in line.chars()
    {

    }

    return Err("not implemented".to_string());
}
