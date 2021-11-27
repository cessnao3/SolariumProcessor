mod keyword;
mod symbol;
mod word_literal;
mod name;
mod string_literal;

mod utils;

use utils::is_separator;

#[derive(Debug, Clone)]
pub enum Token
{
    Keyword(keyword::Keyword),
    Symbol(symbol::Symbol),
    WordLiteral(u16),
    StringLiteral(string_literal::StringLiteral),
    Name(String)
}

impl ToString for Token
{
    fn to_string(&self) -> String
    {
        return match self
        {
            Token::Keyword(k) => format!("Keyword({0:})", k.to_string()),
            Token::Symbol(s) => format!("Symbol({0:})", s.to_string()),
            Token::WordLiteral(v) => format!("WordLiteral({0:})", v),
            Token::Name(n) => format!("Name({0:})", n),
            Token::StringLiteral(s) => format!("String(\"{0:}\")", s.to_string())
        };
    }
}

pub fn tokenize(line: &str) -> Result<Vec<Token>, String>
{
    let mut tokens: Vec<Token> = Vec::new();
    let chars: Vec<char> = line.chars().collect();

    let mut i = 0usize;
    while i < line.len()
    {
        // Extract the current character
        let c = chars[i];

        // Check for ascii
        if !c.is_ascii()
        {
            return Err(format!("Only ASCII characters are allowed"));
        }

        // Define the current reference val
        let next_str = &line[i..line.len()];

        // Skip if the current character is a separator
        if c.is_whitespace()
        {
            i += 1;
        }
        else if let Some(len) = try_clear_comment(next_str)
        {
            i += len;
        }
        else if let Some((symb, len)) = symbol::Symbol::try_match_symbol(next_str)
        {
            tokens.push(Token::Symbol(symb));
            i += len;
        }
        else if let Some((key, len)) = keyword::Keyword::try_match_keyword(next_str)
        {
            tokens.push(Token::Keyword(key));
            i += len;
        }
        else if let Some((val, len)) = word_literal::try_match_integer_literal(next_str)
        {
            tokens.push(Token::WordLiteral(val));
            i += len;
        }
        else if let Some((name, len)) = name::try_match_name(next_str)
        {
            tokens.push(Token::Name(name));
            i += len;
        }
        else if let Some((strval, len)) = string_literal::StringLiteral::try_string_literal(next_str)
        {
            tokens.push(Token::StringLiteral(strval));
            i += len;
        }
        else
        {
            let mut j = i + 1;
            while j < chars.len()
            {
                if is_separator(chars[j])
                {
                    break;
                }

                j += 1;
            }

            return Err(format!("unable to generate token for \"{0:}\"", &line[i..j]));
        }
    }

    return Ok(tokens);
}

fn try_clear_comment(input: &str) -> Option<usize>
{
    let mut len = 0usize;

    for (i, c) in input.chars().enumerate()
    {
        if (i == 0 || i == 1) && c != '/'
        {
            return None;
        }
        else if c == '\n' || c == '\r'
        {
            break;
        }
        else
        {
            len += 1;
        }
    }

    return Some(len);
}
