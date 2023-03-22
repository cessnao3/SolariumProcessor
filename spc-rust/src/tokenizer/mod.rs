pub enum TokenValue {
    Name(String),
    Symbol(String),
    Keyword(String),
    Number(String),
    CharacterLiteral(String)
}

enum Keyword {
    While,
    Function,
    As,
    If,
    Else,
    Var,
    Sizeof
}

impl ToString for Keyword {
    fn to_string(&self) -> String {
        match self {
            Self::While => "while",
            Self::Function => "fun",
            Self::As => "as",
            Self::If => "if",
            Self::Else => "else",
            Self::Var => "var",
            Self::Sizeof => "sizeof"
        }.to_string()
    }
}

impl Keyword {
    fn try_parse(in_str: &str) -> Option<&str> {
        let mut last_index = None;
        let mut keyword = None;

        let keyword_vals = vec![
            Keyword::While,
            Keyword::Function,
            Keyword::As,
            Keyword::If,
            Keyword::Else,
            Keyword::Var,
            Keyword::Sizeof
        ];

        for k in keyword_vals.iter() {
            if in_str.starts_with(&k.to_string()) {
                keyword = Some(k);
                last_index = Some(k.to_string().len());
                break;
            }
        }

        let remaining = &in_str[last_index?..];

        // TODO - Ensure that the next character is a valid space or separator character
    }
}

const KEYWORDS: [&str; 7] = [
    "while",
    "fn",
    "as",
    "if",
    "else",
    "var",
    "sizeof",
];

const SYMBOLS: [&str; 18] = [ // TODO - Generate this list from existing parameters?
    "<",
    ">",
    "=",
    ">=",
    "<=",
    "==",
    "!=",
    "!",
    "*",
    "&",
    "{",
    "}",
    "(",
    ")",
    ",",
    ".",
    "[", // ??
    "]", // ??
];

pub struct Token {
    pub value: String,
    pub line: usize,
    pub column: usize,
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut v = Vec::new();

    let mut tok_init = None;

    let mut n_col = 0;
    let mut n_line = 1;

    for (i, c) in s.char_indices() {
        if c == '\n' {
            n_line += 1;
            n_col = 0;
        }

        n_col += 1;

        if tok_init.is_none() {
            if c.is_whitespace() {
                continue;
            } else {
                tok_init = Some(i);
            }
        }

        if let Some(init_i) = tok_init {
            // Extract the current point
            let so_far = &s[init_i..i];

            // Compile regexes for identifiers/numerics/etc?


            // Check for identifier

            // Check for number + decimal

            // Check for operators
        }
    }

    v
}
