#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    value: String,
    line: usize,
    column: usize,
}

impl Token {
    pub fn new(line: usize, column: usize, value: String) -> Self {
        Self {
            value,
            line,
            column,
        }
    }

    pub fn get_value(&self) -> &str {
        &self.value
    }

    pub fn get_line(&self) -> usize {
        self.line
    }

    pub fn get_column(&self) -> usize {
        self.column
    }

    pub fn is_comment(&self) -> bool {
        self.value.starts_with("//") || self.value.starts_with("/*")
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "[{}:{}] {}", self.line, self.column, self.value)
    }
}

pub struct TokenIter {
    tokens: Vec<Token>,
    ind: usize,
}

impl TokenIter {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ind: 0,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.ind < self.tokens.len() {
            let i = self.ind;
            self.ind += 1;
            Some(self.tokens[i].clone())
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        if self.ind < self.tokens.len() {
            Some(self.tokens[self.ind].clone())
        } else {
            None
        }
    }

    pub fn expect(&mut self) -> Result<Token, TokenIterError> {
        if let Some(val) = self.next() {
            Ok(val)
        } else {
            Err(TokenIterError::EndOfStream)
        }
    }

    pub fn expect_with_value(&mut self, tok: &str) -> Result<(), TokenIterError> {
        let val = self.expect()?;

        if val.get_value() != tok {
            return Err(TokenIterError::TokenMismatch(val, tok.to_string()));
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum TokenIterError {
    EndOfStream,
    TokenMismatch(Token, String),
}

impl std::fmt::Display for TokenIterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfStream => write!(f, "unexpected end of tokens"),
            Self::TokenMismatch(tok, expected) => write!(f, "expected token with value '{expected}', found '{tok}'"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
    EndError,
    EmptyToken,
    MissingLocation,
    ExistingLocation,
}

impl std::fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndError => write!(f, "unexpected characters remaining in build buffer"),
            Self::EmptyToken => write!(f, "unexpected empty token"),
            Self::MissingLocation => write!(f, "token missing location"),
            Self::ExistingLocation => write!(f, "token already has location"),
        }
    }
}

struct TokenBuilder {
    loc: Option<(usize, usize)>,
    chrs: Vec<char>,
}

impl TokenBuilder {
    pub fn new() -> Self {
        Self {
            loc: None,
            chrs: Vec::new(),
        }
    }

    pub fn set_loc(&mut self, line: usize, col: usize) -> Result<(), TokenizeError> {
        if self.chrs.is_empty() {
            self.loc = Some((line, col));
            Ok(())
        } else {
            Err(TokenizeError::ExistingLocation)
        }
    }

    pub fn push(&mut self, c: char) -> Result<(), TokenizeError> {
        if self.loc.is_some() {
            self.chrs.push(c);
            Ok(())
        } else {
            Err(TokenizeError::MissingLocation)
        }
    }

    pub fn build(&self) -> Result<Token, TokenizeError> {
        if let Some((line, col)) = self.loc {
            if !self.chrs.is_empty() {
                Ok(Token::new(line, col, self.chrs.iter().collect::<String>()))
            } else {
                Err(TokenizeError::EmptyToken)
            }
        } else {
            Err(TokenizeError::MissingLocation)
        }
    }

    pub fn build_and_reset(&mut self) -> Result<Token, TokenizeError> {
        let tok = self.build()?;
        self.reset();
        Ok(tok)
    }

    pub fn is_empty(&self) -> bool {
        self.chrs.is_empty()
    }

    pub fn reset(&mut self) {
        self.chrs.clear();
        self.loc = None;
    }

    pub fn len(&self) -> usize {
        self.chrs.len()
    }

    pub fn starts_with(&self, s: &str) -> bool {
        if self.chrs.len() < s.len() {
            return false;
        }

        for (ci, cs) in self.chrs.iter().zip(s.chars()) {
            if *ci != cs {
                return false;
            }
        }

        true
    }

    pub fn ends_with(&self, s: &str) -> bool {
        if self.chrs.len() < s.len() {
            return false;
        }

        for (ci, cs) in self.chrs.iter().rev().zip(s.chars().rev()) {
            if *ci != cs {
                return false;
            }
        }

        true
    }
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut within_block_comment = false;
    let mut within_line_comment = false;

    let mut builder = TokenBuilder::new();
    let mut tokens = Vec::new();

    let mut it = s.lines().enumerate().flat_map(|(line_idx, l)| l.char_indices().map(move |(char_idx, c)| (line_idx, char_idx, c))).peekable();

    let double_char_separators = [
        "&&",
        "||",
    ];

    let single_char_separators = [
        '+',
        '-',
        '.',
        ',',
        '*',
        '/',
        '&',
        '|',
        '[',
        ']',
        '(',
        ')',
        '{',
        '}',
        ':',
        ';',
    ];

    let all_separator_init_char = single_char_separators.iter()
        .copied()
        .chain(double_char_separators.iter().filter_map(|s| s.chars().next()))
        .collect::<std::collections::BTreeSet<_>>();

    while let Some((idx_line, idx_col, c)) = it.next() {
        // Mark a line differerence if no characters are remaining or if the character lines differer from the current read character line
        let (next_line_different, peek_c) = if let Some(val) = it.peek() {
            let line_diff = val.0 != idx_line;
            (line_diff, if line_diff { None } else { Some(val.2) })
        } else {
            (true, None)
        };

        if builder.is_empty() {
            builder.set_loc(idx_line, idx_col)?;

            if c.is_whitespace() {
                continue;
            }

            builder.push(c)?;

            within_line_comment = false;
            within_block_comment = false;

            if let Some(c2) = peek_c {
                let double_check = [c, c2].into_iter().collect::<String>();

                if double_char_separators.contains(&double_check.as_str()) {
                    // Clear the next character from the buffer
                    it.next();

                    // Create the double-character separator
                    builder.push(c2)?;

                    assert!(builder.starts_with(&double_check));

                    tokens.push(builder.build_and_reset()?);
                    continue;
                } else if double_check == "//" {
                    within_line_comment = true;
                    continue;
                } else if double_check == "/*" {
                    within_block_comment = true;
                    continue;
                }
            }

            let next_char_is_sep = if let Some(c2) = peek_c {
                !within_line_comment && (c2.is_whitespace() || all_separator_init_char.contains(&c2))
            } else {
                false
            };

            if single_char_separators.contains(&c) || next_char_is_sep {
                tokens.push(builder.build_and_reset()?);
                continue;
            }
        } else if within_block_comment {
            builder.push(c)?;

            if builder.len() >= 4 && builder.ends_with("*/") {
                tokens.push(builder.build_and_reset()?);
            } else if next_line_different {
                builder.push('\n')?;
            }
        } else {
            builder.push(c)?;

            let next_char_is_sep = if let Some(c2) = peek_c {
                !within_line_comment && (c2.is_whitespace() || all_separator_init_char.contains(&c2))
            } else {
                false
            };

            if next_line_different || next_char_is_sep {
                tokens.push(builder.build_and_reset()?);
            }
        }
    }

    if builder.is_empty() {
        Ok(tokens)
    } else {
        Err(TokenizeError::EndError)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let test_code = [
            "fn func_name_ptr(a, b): ret_type = 3943;",
            "var a: int; // This is a comment test!",
            "/* This\nis a block comment\n*/",
            "fn block_test(): ret_type { return 0; }",
        ].iter().map(|i| i.to_string()).reduce(|a, b| format!("{a}\n{b}")).unwrap_or("".into());

        let tokens = tokenize(&test_code);
        assert!(tokens.is_ok());

        let tokens = tokens.unwrap();

        let expected_tokens = [
            ("fn", 0, 0),
            ("func_name_ptr", 0, 3),
            ("(", 0, 16),
            ("a", 0, 17),
            (",", 0, 18),
            ("b", 0, 20),
            (")", 0, 21),
            (":", 0, 22),
            ("ret_type", 0, 24),
            ("=", 0, 33),
            ("3943", 0, 35),
            (";", 0, 39),
            ("var", 1, 0),
            ("a", 1, 4),
            (":", 1, 5),
            ("int", 1, 7),
            (";", 1, 10),
            ("// This is a comment test!", 1, 12),
            ("/* This\nis a block comment\n*/", 2, 0),
            ("fn", 5, 0),
            ("block_test", 5, 3),
            ("(", 5, 13),
            (")", 5, 14),
            (":", 5, 15),
            ("ret_type", 5, 17),
            ("{", 5, 26),
            ("return", 5, 28),
            ("0", 5, 35),
            (";", 5, 36),
            ("}", 5, 38),
        ].into_iter().map(|(val, line, col)| Token::new(line, col, val.into())).collect::<Vec<_>>();

        for (tok, exp_tok) in tokens.iter().zip(expected_tokens.iter()) {
            assert_eq!(tok, exp_tok);
        }

        assert_eq!(tokens.len(), expected_tokens.len());
    }

    #[test]
    fn test_tokenizer_struct() {
        let test_code = "struct s1 { var1: u16, var2: u56 } struct s2;";
        let expected_tokens = [
            "struct",
            "s1",
            "{",
            "var1",
            ":",
            "u16",
            ",",
            "var2",
            ":",
            "u56",
            "}",
            "struct",
            "s2",
            ";",
        ];

        match tokenize(test_code) {
            Ok(v) => {
                for (tok, exp_tok) in v.iter().zip(expected_tokens.iter()) {
                    assert_eq!(tok.get_value(), *exp_tok);
                }
                assert_eq!(v.len(), expected_tokens.len());
            },
            Err(e) => panic!("{e}")
        }
    }
}
