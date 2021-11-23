#[derive(Debug, Clone, Copy)]
pub enum Symbol
{
    Plus,
    Minus,
    Star,
    Divide,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Assignment,
    Equal,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace
}

impl ToString for Symbol
{
    fn to_string(&self) -> String
    {
        return match self
        {
            Symbol::Plus => "+".to_string(),
            Symbol::Minus => "-".to_string(),
            Symbol::Star => "*".to_string(),
            Symbol::Divide => "/".to_string(),
            Symbol::Greater => ">".to_string(),
            Symbol::Less => "<".to_string(),
            Symbol::GreaterEqual => ">=".to_string(),
            Symbol::LessEqual => "<=".to_string(),
            Symbol::Equal => "==".to_string(),
            Symbol::Assignment => "=".to_string(),
            Symbol::OpenParen => "(".to_string(),
            Symbol::CloseParen => ")".to_string(),
            Symbol::OpenBrace => "{".to_string(),
            Symbol::CloseBrace => "}".to_string()
        };
    }
}
