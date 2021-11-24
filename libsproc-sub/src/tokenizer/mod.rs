use regex::Regex;

use lazy_static::lazy_static;

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

fn is_separator(c: char) -> bool
{
    lazy_static!
    {
        static ref separator_strings: Vec<char> = symbol::Symbol::get_symbol_list()
            .iter()
            .filter(|(s, _)| s.len() == 1)
            .map(|(s, _)| s.chars().next().unwrap())
            .collect();
    }

    return
        c.is_ascii_whitespace() ||
        separator_strings.contains(&c);
}

fn try_match_integer_literal(input: &str) -> Option<u16>
{
    lazy_static!
    {
        pub static ref regex_signed_int: Regex = Regex::new(r"$-[\d]+").unwrap();
    }

    if input.len() == 0
    {
        return None;
    }

    let first_char = input.chars().next().unwrap();

    if first_char == '-'
    {
        let mut current = 1usize;

        for c in input[1..input.len()].chars()
        {
            if !c.is_numeric()
            {
                break;
            }
            else
            {
                current += 1;
            }
        }

        return match &input[0..current].parse::<i16>()
        {
            Ok(v) => Some(*v as u16),
            Err(_) => None
        };
    }
    else if first_char.is_numeric()
    {
        let mut current = 1usize;
        for c in input[1..input.len()].chars()
        {
            if !c.is_numeric()
            {
                break;
            }
            else
            {
                current += 1;
            }
        }
    }

    return None;
}

pub fn tokenize(line: &str) -> Result<Vec<Token>, String>
{
    let mut current_token: Option<Token> = None;
    let mut tokens: Vec<Token> = Vec::new();

    for (i, c) in line.chars().enumerate()
    {
        // Complete the current token if needed
        if is_separator(c) && current_token.is_some()
        {
            tokens.push(current_token.unwrap());
            current_token = None;
        }

        // Add the current value to the token list

    }

    return Err("not implemented".to_string());
}
