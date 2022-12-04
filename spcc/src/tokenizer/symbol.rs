#[derive(Clone, Copy)]
pub enum Symbol {
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
    OpenBracket,
    CloseBracket,
    Semicolon,
    BooleanNot,
    BooleanAnd,
    BooleanOr,
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
    Comma,
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        return (match self {
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Star => "*",
            Symbol::Divide => "/",
            Symbol::Modulus => "%",
            Symbol::Greater => ">",
            Symbol::Less => "<",
            Symbol::GreaterEqual => ">=",
            Symbol::LessEqual => "<=",
            Symbol::Equal => "==",
            Symbol::NotEqual => "!=",
            Symbol::Assignment => "=",
            Symbol::AddressAssignment => ":=",
            Symbol::OpenParen => "(",
            Symbol::CloseParen => ")",
            Symbol::OpenBrace => "{",
            Symbol::CloseBrace => "}",
            Symbol::Semicolon => ";",
            Symbol::BooleanNot => "!",
            Symbol::BooleanAnd => "&&",
            Symbol::BooleanOr => "||",
            Symbol::BitwiseNot => "~",
            Symbol::BitwiseAnd => "&",
            Symbol::BitwiseOr => "|",
            Symbol::ShiftLeft => "<<",
            Symbol::ShiftRight => ">>",
            Symbol::Comma => ",",
            Symbol::OpenBracket => "[",
            Symbol::CloseBracket => "]",
        })
        .to_string();
    }
}

impl Symbol {
    fn enum_iter() -> &'static [Symbol] {
        static VALUES: [Symbol; 29] = [
            Symbol::Plus,
            Symbol::Minus,
            Symbol::Star,
            Symbol::Divide,
            Symbol::Modulus,
            Symbol::Greater,
            Symbol::Less,
            Symbol::GreaterEqual,
            Symbol::LessEqual,
            Symbol::Assignment,
            Symbol::AddressAssignment,
            Symbol::Equal,
            Symbol::NotEqual,
            Symbol::OpenParen,
            Symbol::CloseParen,
            Symbol::OpenBrace,
            Symbol::CloseBrace,
            Symbol::OpenBracket,
            Symbol::CloseBracket,
            Symbol::Semicolon,
            Symbol::BooleanNot,
            Symbol::BooleanAnd,
            Symbol::BooleanOr,
            Symbol::BitwiseNot,
            Symbol::BitwiseAnd,
            Symbol::BitwiseOr,
            Symbol::Comma,
            Symbol::ShiftLeft,
            Symbol::ShiftRight,
        ];

        return &VALUES;
    }

    pub fn get_symbol_list() -> Vec<(String, Symbol)> {
        let mut symbol_list: Vec<Symbol> = Symbol::enum_iter().iter().map(|v| *v).collect();

        symbol_list.sort_by(|a, b| {
            let a_s = a.to_string();
            let b_s = b.to_string();

            return if a_s.len() == b_s.len() {
                std::cmp::Ordering::Equal
            } else if a_s.len() > b_s.len() {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            };
        });

        return symbol_list.iter().map(|v| (v.to_string(), *v)).collect();
    }

    pub fn try_match_symbol(input: &str) -> Option<(Symbol, usize)> {
        for (symbol_text, symbol) in &Symbol::get_symbol_list() {
            if input.len() >= symbol_text.len() && &input[0..symbol_text.len()] == *symbol_text {
                return Some((*symbol, symbol_text.len()));
            }
        }

        return None;
    }
}
