use std::{collections::HashSet, fmt, iter::Peekable, rc::Rc, slice::Iter, sync::LazyLock};

use jib::cpu::DataType;
use jib_asm::{AsmToken, AsmTokenLoc};
use regex::Regex;

use crate::expressions::{BinaryOperation, UnaryOperation};

fn strip_comments(s: &str) -> String {
    let mut lines = Vec::new();

    for l in s.lines() {
        lines.push(if let Some(i) = l.find("//") {
            &l[0..i]
        } else {
            l
        });
    }

    lines.join("\n")
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenError> {
    // Define the maximum count for operators
    const MAX_OPERATOR_CHAR_COUNT: usize = 2;

    // Define operator values
    static OPERATORS: LazyLock<HashSet<String>> = LazyLock::new(|| {
        let mut vals = HashSet::new();

        for o in BinaryOperation::ALL {
            vals.insert(o.to_string());
        }

        for o in UnaryOperation::ALL {
            vals.insert(o.to_string());
        }

        for o in ["(", ")", ",", ";", ":", "{", "}", "[", "]", "."] {
            vals.insert(o.to_string());
        }

        assert!(vals.iter().map(|x| x.len()).max().unwrap_or(0) <= MAX_OPERATOR_CHAR_COUNT);

        vals
    });
    static OPERATOR_STARTS: LazyLock<HashSet<char>> = LazyLock::new(|| {
        let mut vals = HashSet::new();

        for o in OPERATORS.iter() {
            if let Some(c) = o.chars().next() {
                vals.insert(c);
            }
        }

        vals
    });

    // Remove comments
    let s = strip_comments(s);

    // Loop through and process tokens
    let mut tokens = Vec::new();

    struct TokenStart {
        i: usize,
        string_char: Option<char>,
        last_was_escape: bool,
    }

    impl TokenStart {
        fn new(i: usize) -> Self {
            Self {
                i,
                string_char: None,
                last_was_escape: false,
            }
        }

        fn new_str(i: usize, c: char) -> Self {
            Self {
                i,
                string_char: Some(c),
                last_was_escape: false,
            }
        }

        fn set_escape(&mut self, c: char) {
            self.last_was_escape = self.string_char.is_some() && c == '\\';
        }
    }

    for (line_num, l) in s.lines().enumerate() {
        let mut current_state = None;

        let get_loc = |start: usize| TokenLocation {
            line: line_num,
            column: start,
        };

        fn convert_escape_characters(s: Token, string_char: char) -> Result<Token, TokenError> {
            let mut within_escape = false;
            let mut result = Vec::new();

            for c in s.get_value().chars() {
                if within_escape {
                    let res = match c {
                        'n' => '\n',
                        '\\' => '\\',
                        '\'' if string_char == '\'' => '\'',
                        '"' if string_char == '"' => '"',
                        _ => {
                            return Err(s.into_err(format!(
                                "unable to convert {c} into valid escape sequence"
                            )));
                        }
                    };
                    result.push(res);
                    within_escape = false;
                } else if c == '\\' {
                    within_escape = true;
                } else {
                    result.push(c);
                }
            }

            Ok(Token::new(
                &result.into_iter().collect::<String>(),
                *s.get_loc(),
            ))
        }

        for (col_num, c) in l.char_indices() {
            if let Some(TokenStart {
                i: start_index,
                string_char: Some(string_char),
                last_was_escape,
            }) = current_state
            {
                if c == string_char && !last_was_escape {
                    let init_tok =
                        Token::new(&l[start_index..=col_num].to_string(), get_loc(start_index));
                    tokens.push(convert_escape_characters(init_tok, string_char)?);

                    current_state = None;
                }
            } else if let Some(TokenStart {
                i: start_index,
                string_char: None,
                ..
            }) = current_state
            {
                let prev = &l[start_index..col_num];
                let current = &l[start_index..=col_num];

                if c.is_whitespace() {
                    tokens.push(Token::new(&l[start_index..col_num], get_loc(start_index)));

                    current_state = None;
                } else if c == '\'' || c == '"' {
                    tokens.push(Token::new(&l[start_index..col_num], get_loc(start_index)));
                    current_state = Some(TokenStart::new_str(col_num, c));
                } else if (OPERATORS.contains(prev) && !OPERATORS.contains(current))
                    || (!OPERATORS.contains(current) && OPERATOR_STARTS.contains(&c))
                {
                    tokens.push(Token::new(prev, get_loc(start_index)));

                    current_state = Some(TokenStart::new(col_num));
                }
            } else if c == '\'' || c == '"' {
                current_state = Some(TokenStart::new_str(col_num, c));
            } else if !c.is_whitespace() {
                current_state = Some(TokenStart::new(col_num));
            }

            if let Some(st) = &mut current_state {
                st.set_escape(c);
            }
        }

        if let Some(TokenStart {
            i: start,
            string_char: None,
            ..
        }) = current_state
        {
            let current = &l[start..];
            tokens.push(Token::new(current, get_loc(start)));
        } else if let Some(TokenStart {
            i: start,
            string_char: Some(_),
            ..
        }) = current_state
        {
            let current = &l[start..];

            return Err(TokenError {
                token: Some(Token::new(
                    current,
                    TokenLocation {
                        line: line_num,
                        column: start,
                    },
                )),
                msg: format!("\"{current}\" not a valid token"),
            });
        }
    }

    // Return the resulting tokens
    Ok(tokens)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct TokenLocation {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    value: Rc<str>,
    loc: TokenLocation,
}

impl Token {
    pub fn new(value: &str, loc: TokenLocation) -> Self {
        Self {
            value: value.into(),
            loc,
        }
    }

    pub fn get_value(&self) -> &str {
        self.value.as_ref()
    }

    pub fn get_loc(&self) -> &TokenLocation {
        &self.loc
    }

    pub fn into_err<T: AsRef<str>>(self, msg: T) -> TokenError {
        TokenError {
            token: Some(self),
            msg: msg.as_ref().into(),
        }
    }

    pub fn to_asm(&self, asm_token: AsmToken) -> AsmTokenLoc {
        AsmTokenLoc {
            tok: asm_token,
            loc: jib_asm::LocationInfo {
                line: self.get_loc().line,
                column: self.get_loc().column,
                text: Some(self.value.clone()),
            },
        }
    }

    pub fn to_asm_iter<T: IntoIterator<Item = AsmToken>>(
        &self,
        iter: T,
    ) -> impl IntoIterator<Item = AsmTokenLoc> {
        iter.into_iter().map(|t| self.to_asm(t))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}:{}]=\"{}\"",
            self.loc.line + 1,
            self.loc.column + 1,
            self.value
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EndOfTokenStream;

pub struct TokenIter<'a> {
    iter: Peekable<Iter<'a, Token>>,
    last_token: Option<&'a Token>,
}

impl<'a> From<&'a Vec<Token>> for TokenIter<'a> {
    fn from(value: &'a Vec<Token>) -> Self {
        Self {
            iter: value.iter().peekable(),
            last_token: None,
        }
    }
}

impl TokenIter<'_> {
    pub fn peek(&mut self) -> Option<Token> {
        self.iter.peek().copied().map(|t| t.to_owned())
    }

    pub fn next(&mut self) -> Result<Token, EndOfTokenStream> {
        if let Some(t) = self.iter.next() {
            self.last_token = Some(t);
            Ok(t.to_owned())
        } else {
            Err(EndOfTokenStream)
        }
    }

    pub fn expect(&mut self, s: &str) -> Result<Token, TokenError> {
        let next = self.next().map(|v| v.to_owned());

        if let Ok(t) = next {
            if t.get_value() != s {
                let msg = format!("expected \"{}\", found \"{}\"", s, t.get_value());
                Err(t.into_err(msg))
            } else {
                Ok(t)
            }
        } else if let Some(prev) = self.last_token {
            let msg = format!("expected \"{}\" after \"{}\"", s, prev.get_value());
            Err(prev.to_owned().into_err(msg))
        } else {
            Err(EndOfTokenStream.into())
        }
    }

    pub fn expect_peek(&mut self, s: &str) -> bool {
        if let Some(t) = self.peek() {
            t.get_value() == s
        } else {
            false
        }
    }

    pub fn next_if<F: Fn(&str) -> bool>(&mut self, pred: F) -> Option<Token> {
        if let Some(s) = self.peek() {
            if pred(s.get_value()) {
                Some(self.next().unwrap())
            } else {
                None
            }
        } else {
            None
        }
    }
}

static RESERVED_KEYWORDS: LazyLock<HashSet<String>> = LazyLock::new(|| {
    let keywords = [
        "if", "while", "else", "global", "def", "fn", "fnint", "asmfn", "const", "struct", "void",
        "break", "brkpt", "return",
    ]
    .map(|v| v.to_string());
    let primitives = DataType::ALL.iter().map(|v| v.to_string());
    keywords.into_iter().chain(primitives).collect()
});

#[derive(Debug, Clone)]
pub struct IdentifierError {
    pub(crate) token: Token,
}

pub fn is_identifier<T: AsRef<str>>(s: T) -> bool {
    static IDENT_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[a-zA-Z_]\w*$").unwrap());
    IDENT_REGEX.is_match(s.as_ref()) && !RESERVED_KEYWORDS.contains(s.as_ref())
}

pub fn get_identifier(t: &Token) -> Result<&str, IdentifierError> {
    if is_identifier(t.get_value()) {
        Ok(t.get_value())
    } else {
        Err(IdentifierError {
            token: t.to_owned(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct TokenError {
    pub token: Option<Token>,
    pub msg: String,
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)?;
        if let Some(t) = &self.token {
            write!(f, " @ {t}")
        } else {
            Ok(())
        }
    }
}

impl From<EndOfTokenStream> for TokenError {
    fn from(_: EndOfTokenStream) -> Self {
        Self {
            token: None,
            msg: "end of token stream".into(),
        }
    }
}

impl From<IdentifierError> for TokenError {
    fn from(value: IdentifierError) -> Self {
        let msg = format!("\"{}\" is not a valid identifier", value.token.get_value());
        Self {
            token: Some(value.token),
            msg,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::is_identifier;

    use super::{Token, TokenLocation, tokenize};

    #[test]
    fn operators() {
        let expected_operators = [
            "=", "==", "!=", "+", "-", "/", "&", "&&", "|", "||", ">", "<", ">=", "<=",
        ];
        let tokens = tokenize(&expected_operators.join("\n"));
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), expected_operators.len());

        for (i, (expected, token)) in expected_operators.iter().zip(tokens.iter()).enumerate() {
            assert_eq!(token.get_loc().line, i);
            assert_eq!(token.get_loc().column, 0);
            assert_eq!(token.get_value(), *expected);
        }
    }

    #[test]
    fn definition_statement() {
        let input_string = "global a: u16 = 255 + 5;";
        let expected_token_values = ["global", "a", ":", "u16", "=", "255", "+", "5", ";"];

        let tokens = tokenize(input_string);
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), expected_token_values.len());

        for (tok, expected) in tokens.iter().zip(expected_token_values) {
            assert_eq!(tok.get_value(), expected);
        }
    }

    #[test]
    fn definition_statement_typing() {
        let input_string = "global c: u32 = (-1) : u32;";
        let expected_token_values = [
            "global", "c", ":", "u32", "=", "(", "-", "1", ")", ":", "u32", ";",
        ];

        let tokens = tokenize(input_string);
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), expected_token_values.len());

        for (tok, expected) in tokens.iter().zip(expected_token_values) {
            assert_eq!(tok.get_value(), expected);
        }
    }

    #[test]
    fn duplicate_operators() {
        let input_text = "===\n&&&&\n|&&||=|&&&";
        let expected_tokens = [
            Token::new("==".into(), TokenLocation { line: 0, column: 0 }),
            Token::new("=".into(), TokenLocation { line: 0, column: 2 }),
            Token::new("&&".into(), TokenLocation { line: 1, column: 0 }),
            Token::new("&&".into(), TokenLocation { line: 1, column: 2 }),
            Token::new("|".into(), TokenLocation { line: 2, column: 0 }),
            Token::new("&&".into(), TokenLocation { line: 2, column: 1 }),
            Token::new("||".into(), TokenLocation { line: 2, column: 3 }),
            Token::new("=".into(), TokenLocation { line: 2, column: 5 }),
            Token::new("|".into(), TokenLocation { line: 2, column: 6 }),
            Token::new("&&".into(), TokenLocation { line: 2, column: 7 }),
            Token::new("&".into(), TokenLocation { line: 2, column: 9 }),
        ];
        let tokens = tokenize(&input_text);

        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        assert_eq!(expected_tokens.len(), tokens.len());

        for (expected, token) in expected_tokens.iter().zip(tokens.iter()) {
            assert_eq!(expected, token);
        }
    }

    #[test]
    fn identifiers() {
        let identifiers = [
            ("_asdf", true),
            ("0o34lksdfjaklsdjf", false),
            ("_", true),
            ("a", true),
            ("Z", true),
            ("4", false),
            ("l", true),
            ("laskdfj49438945_", true),
            ("ASDKLFJ", true),
            ("A_0494", true),
            ("4laksdfj", false),
            ("+47894", false),
            ("%", false),
        ];

        for (txt, is_match) in identifiers {
            assert_eq!(is_identifier(txt), is_match);
        }
    }
}
