#[derive(Debug, Copy, Clone)]
pub enum Keyword
{
    If,
    Else,
    While,
    Func
}

impl ToString for Keyword
{
    fn to_string(&self) -> String
    {
        match &self
        {
            Keyword::If => "if".to_string(),
            Keyword::Else => "else".to_string(),
            Keyword::While => "while".to_string(),
            Keyword::Func => "func".to_string()
        }
    }
}
