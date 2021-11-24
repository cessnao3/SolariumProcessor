use std::net::ToSocketAddrs;

use crate::tokenizer::{is_separator, keyword};

#[derive(Debug, Copy, Clone)]
pub enum Keyword
{
    If,
    Else,
    While,
    Func,
    Static,
    Const,
    Auto
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
            Keyword::Func => "func".to_string(),
            Keyword::Static => "static".to_string(),
            Keyword::Const => "const".to_string(),
            Keyword::Auto => "auto".to_string()
        }
    }
}

impl Keyword
{
    pub fn check_from_string(input: &str) -> Option<Keyword>
    {
        fn matches_keyword(input: &str, keyword: &str) -> bool
        {
            // Ensure that the input length is long enough
            if input.len() < keyword.len()
            {
                return false;
            }

            // Ensure that all characters in the string are valid
            if &input[0..keyword.len()] != keyword
            {
                return false;
            }

            // If there is another character, it must be a separator
            let char_vec: Vec<char> = input.chars().collect();
            if input.len() > keyword.len() && !is_separator(char_vec[keyword.len()])
            {
                return false;
            }

            // Return true if all else succeeds
            return true;
        }

        let keyword_vals = vec![
            Keyword::If,
            Keyword::Else,
            Keyword::While,
            Keyword::Func,
            Keyword::Static,
            Keyword::Const,
            Keyword::Auto
        ];

        let keyword_checks: Vec<(Keyword, String)> = keyword_vals
            .iter()
            .map(|v| (*v, v.to_string()))
            .collect();

        for (key, key_str) in keyword_checks
        {
            if matches_keyword(input, &key_str)
            {
                return Some(key);
            }
        }

        return None;
    }
}
