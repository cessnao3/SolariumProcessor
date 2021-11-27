use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::tokenizer::utils::is_separator;

#[derive(Debug, Copy, Clone, EnumIter)]
pub enum Keyword
{
    If,
    Else,
    While,
    Func,
    Static,
    Const,
    Auto,
    Extern
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
            Keyword::Auto => "auto".to_string(),
            Keyword::Extern => "extern".to_string()
        }
    }
}

impl Keyword
{
    pub fn try_match_keyword(input: &str) -> Option<(Keyword, usize)>
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

        let keyword_vals: Vec<Keyword> = Keyword::iter().collect();

        let keyword_checks: Vec<(Keyword, String)> = keyword_vals
            .iter()
            .map(|v| (*v, v.to_string()))
            .collect();

        for (key, key_str) in keyword_checks
        {
            if matches_keyword(input, &key_str)
            {
                return Some((key, key_str.len()));
            }
        }

        return None;
    }
}
