pub struct Token {
    value: String,
    line: usize,
    column: usize,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "[{}:{}] {}", self.line, self.column, self.value)
    }
}

struct TokenBuilder {
    chars: Vec<char>,
    line: usize,
    column: usize,
}

impl TokenBuilder {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            chars: Vec::new(),
            line,
            column,
        }
    }

    pub fn update(&mut self, line: usize, column: usize, c: char) -> Option<Token> {
        let mut res = None;

        if line != self.line && !self.is_block_comment_start() {
            // TODO - Clear the token!
        }

        // TODO = Update the character

        if self.is_block_comment_start() && self.is_block_comment_end() {
            // TODO - Clear
        }
    }

    pub fn is_block_comment_start(&self) -> bool {
        self.starts_with("/*")
    }

    pub fn is_block_comment_end(&self) -> bool {
        self.ends_with("*/") && self.chars.len() >= 4
    }

    pub fn is_line_comment_start(&self) -> bool {
        self.starts_with("//")
    }

    pub fn starts_with(&self, s: &str) -> bool {
        if s.len() > self.chars.len() {
            false
        } else {
            for (c, sc) in self.chars.iter().zip(s.chars()) {
                if *c != sc {
                    return false;
                }
            }

            true
        }
    }

    pub fn ends_with(&self, s: &str) -> bool {
        if s.len() > self.chars.len() {
            false
        } else {
            for (c, sc) in self.chars.iter().rev().zip(s.chars().rev()) {
                if *c != sc {
                    return false;
                }
            }

            true
        }
    }

    pub fn is_separator(&self) -> bool {
        let sep_chars = ['.', '(', ')', '{', '}'];

        if let Some(c) = self.chars.first() {
            sep_chars.contains(c) || c.is_whitespace()
        } else {
            false
        }
    }
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, String> {
    let mut within_block_comment = false;

    let mut current = TokenBuilder::new(0, 0);
    let mut tokens = Vec::new();

    for (current_line, l) in s.lines().enumerate() {
        for (current_col, c) in l.char_indices() {
            if c.is_whitespace() {
                // TODO - Reset token value
                continue;
            }


        }
    }

    Err(format!("unable to parse {s}"))
}
