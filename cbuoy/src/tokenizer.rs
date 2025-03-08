use std::{collections::HashSet, fmt, iter::Peekable, rc::Rc, slice::Iter, sync::LazyLock};

use jib::cpu::DataType;
use jib_asm::{AsmToken, AsmTokenLoc};
use regex::Regex;

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
    // Remove comments
    let s = strip_comments(s);

    // Define the splitting regex
    static SPLIT_REGEX: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"(\w+)|:|;|\{|\}|\(|\)|\+|\-|\*|/|(&&?)|(\|\|?)|([<>!=]=?)|\[|\]").unwrap()
    });

    struct TokenMatch<'a> {
        s: &'a str,
        start: usize,
        end: usize,
        loc: Option<TokenLocation>,
    }

    impl From<TokenMatch<'_>> for Token {
        fn from(value: TokenMatch) -> Self {
            Self {
                value: value.s.into(),
                loc: value.loc.unwrap(),
            }
        }
    }

    let mut tokens: Vec<TokenMatch> = SPLIT_REGEX
        .find_iter(&s)
        .map(|m| TokenMatch {
            s: m.as_str(),
            start: m.start(),
            end: m.end(),
            loc: None,
        })
        .collect::<Vec<_>>();

    let mut last_end = None;
    for t in tokens.iter() {
        let last_index = last_end.unwrap_or(0);
        let check = s[last_index..t.start].trim();

        let line = s.chars().filter(|c| *c == '\n').count();

        let mut col_val = 0;
        for (i, c) in s.char_indices().take(t.start) {
            if c == '\n' {
                col_val = i;
            }
        }

        let column = t.start - col_val;

        if !check.is_empty() {
            return Err(TokenError {
                msg: format!("unexpected token \"{}\" found", check),
                token: Some(Token {
                    value: check.into(),
                    loc: TokenLocation { line, column },
                }),
            });
        }

        last_end = Some(t.end);
    }

    // TODO - Check that the spaces between words are just whitespace

    // Determine Locations
    let mut char_iter = s.chars();
    let mut current_index = 0;
    let mut current_line = 0;
    let mut current_col = 0;

    for t in tokens.iter_mut() {
        while current_index < t.start && current_index < s.len() {
            if Some('\n') == char_iter.next() {
                current_line += 1;
                current_col = 0;
            } else {
                current_col += 1;
            }

            current_index += 1;
        }

        t.loc = Some(TokenLocation {
            line: current_line,
            column: current_col,
        });
    }

    // Return the resulting tokens
    Ok(tokens
        .into_iter()
        .map(|t| Token::new(t.s, t.loc.unwrap()))
        .collect())
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
        &self.value
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
    let keywords = ["if", "while", "else", "global", "def", "var", "fn"].map(|v| v.to_string());
    let primitives = DataType::ALL.iter().map(|v| v.to_string());
    keywords.into_iter().chain(primitives).collect()
});

#[derive(Debug, Clone)]
pub struct IdentifierError {
    pub(crate) token: Token,
}

pub fn is_identifier(s: &str) -> bool {
    static IDENT_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[a-zA-Z_]\w*$").unwrap());
    IDENT_REGEX.is_match(s) && !RESERVED_KEYWORDS.contains(s)
}

pub fn get_identifier(t: &Token) -> Result<String, IdentifierError> {
    if is_identifier(t.get_value()) {
        Ok(t.get_value().to_owned())
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
        let msg = format!("\"{}\" is not a valid token", value.token.get_value());
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
