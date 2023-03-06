use crate::tokenizer::utils::is_separator;

#[derive(Debug, Copy, Clone)]
pub enum Keyword {
    If,
    Else,
    While,
    Func,
    Static,
    Const,
    Auto,
    Extern,
    Return,
}

impl ToString for Keyword {
    fn to_string(&self) -> String {
        match self {
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Func => "func",
            Keyword::Static => "static",
            Keyword::Const => "const",
            Keyword::Auto => "auto",
            Keyword::Extern => "extern",
            Keyword::Return => "return",
        }
        .to_string()
    }
}

impl Keyword {
    fn enum_iter() -> &'static [Keyword] {
        static VALUES: [Keyword; 9] = [
            Keyword::If,
            Keyword::Else,
            Keyword::While,
            Keyword::Func,
            Keyword::Static,
            Keyword::Const,
            Keyword::Auto,
            Keyword::Extern,
            Keyword::Return,
        ];

        &VALUES
    }

    pub fn try_match_keyword(input: &str) -> Option<(Keyword, usize)> {
        fn matches_keyword(input: &str, keyword: &str) -> bool {
            // Ensure that the input length is long enough
            if input.len() < keyword.len() {
                return false;
            }

            // Ensure that all characters in the string are valid
            if &input[0..keyword.len()] != keyword {
                return false;
            }

            // If there is another character, it must be a separator
            let char_vec: Vec<char> = input.chars().collect();
            if input.len() > keyword.len() && !is_separator(char_vec[keyword.len()]) {
                return false;
            }

            // Return true if all else succeeds
            true
        }

        let keyword_vals = Keyword::enum_iter().to_vec();
        let keyword_checks: Vec<(Keyword, String)> =
            keyword_vals.iter().map(|v| (*v, v.to_string())).collect();

        for (key, key_str) in keyword_checks {
            if matches_keyword(input, &key_str) {
                return Some((key, key_str.len()));
            }
        }

        None
    }
}
