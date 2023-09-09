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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "[{}:{}] {}", self.line, self.column, self.value)
    }
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
    CreateError(String),
    EndError,
    EmptyToken,
    MissingLocation,
    ExistingLocation,
}

impl std::fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CreateError(s) => write!(f, "create error - {}", s),
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

            if !c.is_whitespace() {
                builder.push(c)?;
            }

            within_line_comment = false;
            within_block_comment = false;

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
            ];

            if let Some(c2) = peek_c {
                let double_check = [c, c2].into_iter().collect::<String>();

                if double_char_separators.contains(&double_check.as_str()) {
                    // Clear the next character from the buffer
                    it.next();

                    // Create the double-character separator
                    builder.push(c2)?;
                    tokens.push(builder.build_and_reset()?);
                } else if double_check == "//" {
                    within_line_comment = true;
                } else if double_check == "/*" {
                    within_block_comment = true;
                }
            } else if single_char_separators.contains(&c) {
                tokens.push(builder.build_and_reset()?);
            }
        } else if within_block_comment {
            builder.push(c)?;

            if builder.len() >= 4 && builder.ends_with("*/") {
                tokens.push(builder.build_and_reset()?);
            }
        } else {
            if within_line_comment || !c.is_whitespace() {
                builder.push(c)?;
            }

            if next_line_different || c.is_whitespace() {
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
