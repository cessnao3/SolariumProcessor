mod keyword;
mod symbol;
mod word_literal;
mod name;
mod string_literal;

mod utils;

use utils::is_separator;
pub use keyword::Keyword;
pub use symbol::Symbol;
pub use string_literal::StringLiteral;

#[derive(Debug, Clone)]
pub enum Token
{
    Keyword(keyword::Keyword),
    Symbol(symbol::Symbol),
    WordLiteral(u16),
    StringLiteral(string_literal::StringLiteral),
    VariableName(String),
    FunctionName(String)
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
            Token::VariableName(n) => format!("VarName({0:})", n),
            Token::FunctionName(n) => format!("FuncName({0:})", n),
            Token::StringLiteral(s) => format!("String(\"{0:}\")", s.to_string())
        };
    }
}

fn next_token(line: &str) -> Result<Option<(Token, usize)>, String>
{
    for (i, c) in line.chars().enumerate()
    {
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
            continue;
        }
        else if let Some(len) = try_clear_comment(next_str)
        {
            return match next_token(&line[i + len..line.len()])
            {
                Ok(Some((token, next_len))) => Ok(Some((token, next_len + i + len))),
                Ok(None) => Ok(None),
                Err(e) => Err(e)
            };
        }
        else if let Some((symb, len)) = symbol::Symbol::try_match_symbol(next_str)
        {
            return Ok(Some((Token::Symbol(symb), i + len)));
        }
        else if let Some((key, len)) = keyword::Keyword::try_match_keyword(next_str)
        {
            return Ok(Some((Token::Keyword(key), i + len)));
        }
        else if let Some((val, len)) = word_literal::try_match_integer_literal(next_str)
        {
            return Ok(Some((Token::WordLiteral(val), i + len)));
        }
        else if let Some((name, len)) = name::try_match_name(next_str)
        {
            // Attempt to get the next token and see if it is a parenthesis (for a function call)
            // Otherwise, is a variable name
            let token = match next_token(&line[i + len .. line.len()])
            {
                Ok(Some((Token::Symbol(Symbol::OpenParen), _))) => Token::FunctionName(name),
                Ok(_) => Token::VariableName(name),
                Err(e) => return Err(e)
            };

            return Ok(Some((token, i + len)));
        }
        else if let Some((strval, len)) = string_literal::StringLiteral::try_string_literal(next_str)
        {
            return Ok(Some((Token::StringLiteral(strval), i + len)));
        }
        else
        {
            let mut curr = 0;
            for (j, cj) in line[i..line.len()].chars().enumerate()
            {
                if is_separator(cj)
                {
                    break;
                }
                curr = j;
            }

            return Err(format!("unable to generate token for \"{0:}\"", &line[i..curr]));
        }
    }

    return Ok(None);
}

pub fn tokenize(line: &str) -> Result<Vec<Token>, String>
{
    let mut tokens: Vec<Token> = Vec::new();

    let mut i = 0usize;
    while i < line.len()
    {
        match next_token(&line[i..line.len()])
        {
            Ok(Some((tok, delta))) =>
            {
                tokens.push(tok);
                i += delta;
            }
            Ok(None) => break,
            Err(e) => return Err(e)
        };
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
