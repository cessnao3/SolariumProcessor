use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, Clone, Copy, EnumIter)]
pub enum Symbol
{
    Plus,
    Minus,
    Star,
    Divide,
    Modulus,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Assignment,
    AddressAssignment,
    Equal,
    NotEqual,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    BooleanNot,
    BooleanAnd,
    BooleanOr,
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    Comma
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
            Symbol::Modulus => "%".to_string(),
            Symbol::Greater => ">".to_string(),
            Symbol::Less => "<".to_string(),
            Symbol::GreaterEqual => ">=".to_string(),
            Symbol::LessEqual => "<=".to_string(),
            Symbol::Equal => "==".to_string(),
            Symbol::NotEqual => "!=".to_string(),
            Symbol::Assignment => "=".to_string(),
            Symbol::AddressAssignment => ":=".to_string(),
            Symbol::OpenParen => "(".to_string(),
            Symbol::CloseParen => ")".to_string(),
            Symbol::OpenBrace => "{".to_string(),
            Symbol::CloseBrace => "}".to_string(),
            Symbol::Semicolon => ";".to_string(),
            Symbol::BooleanNot => "!".to_string(),
            Symbol::BooleanAnd => "&&".to_string(),
            Symbol::BooleanOr => "||".to_string(),
            Symbol::BitwiseNot => "~".to_string(),
            Symbol::BitwiseAnd => "&".to_string(),
            Symbol::BitwiseOr => "|".to_string(),
            Symbol::Comma => ",".to_string()
        };
    }
}

impl Symbol
{
    pub fn get_symbol_list() -> Vec<(String, Symbol)>
    {
        let mut symbol_list: Vec<Symbol> = Symbol::iter().collect();

        symbol_list.sort_by(|a, b| {
            let a_s = a.to_string();
            let b_s = b.to_string();

            return if a_s.len() == b_s.len()
            {
                std::cmp::Ordering::Equal
            }
            else if a_s.len() > b_s.len()
            {
                std::cmp::Ordering::Less
            }
            else
            {
                std::cmp::Ordering::Greater
            };
        });

        return symbol_list
            .iter()
            .map(|v| (v.to_string(), *v))
            .collect();
    }

    pub fn try_match_symbol(input: &str) -> Option<(Symbol, usize)>
    {
        for (symbol_text, symbol) in &Symbol::get_symbol_list()
        {
            if input.len() >= symbol_text.len() && &input[0..symbol_text.len()] == *symbol_text
            {
                return Some((*symbol, symbol_text.len()));
            }
        }

        return None;
    }
}
