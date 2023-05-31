use std::collections::HashMap;
use std::fmt::Display;

use super::components::BaseStatement;
use super::types::SpType;

pub fn is_valid_name(s: &str) -> bool {
    // Ensure that the first character is alphabetic and that the only characters are ascii-alphanumeric/_/-
    if !s.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_') {
        false
    } else if let Some(c) = s.chars().next() {
        c.is_ascii_alphabetic()
    } else {
        false
    }
}

pub fn parse(s: &str) -> Result<(), ParseError> {
    // Define the state
    let mut state = ParserState::new();
    let mut current = s.trim_start();

    while !current.is_empty() {
        // Find the first word
        let (first_word, remaining) = if let Some(ind) = current.find(char::is_whitespace) {
            (&current[..ind], &current[ind..])
        }
        else {
            (current, "")
        };

        current = match first_word {
            "fn" => parse_fn_statement(remaining, &mut state)?,
            "asmfn" => parse_asmfn_statement(remaining, &mut state)?,
            "def" => parse_def_statement(remaining, &mut state)?,
            "struct" => parse_struct_statement(remaining, &mut state)?,
            "//" => skip_to_next_line(s)?,
            "/*" => skip_to_end_of_comment_block(s)?,
            word => return Err(ParseError::new(0, 0, &format!("unknown start of base expression {word}"))),
        };
    }

    Ok(())
}

fn parse_fn_statement<'a>(s: &'a str, state: &mut ParserState) -> Result<&'a str, ParseError> {
    panic!("not implemented");
}

fn parse_asmfn_statement<'a>(s: &'a str, state: &mut ParserState) -> Result<&'a str, ParseError> {
    panic!("not implemented");
}

fn parse_struct_statement<'a>(s: &'a str, state: &mut ParserState) -> Result<&'a str, ParseError> {
    // Find open brace
    let struct_name;
    let fields_string;
    let remaining;

    if let Some(open_ind) = s.find(s) {
        struct_name = s[..open_ind].trim();

        if let Some(close_ind) = s.find('}') {
            if close_ind < open_ind {
                return Err(ParseError::new(0, 0, "struct unexpected closing brace before open brace"));
            }

            fields_string = s[open_ind+1..close_ind].trim();
            remaining = s[close_ind+1..].trim_start();
        } else {
            return Err(ParseError::new(0, 0, "no closing brace found"));
        }

        if !SpType::is_valid_name(struct_name) {
            return Err(ParseError::new(0, 0, &format!("struct name `{struct_name}` is not a valid type name")));
        }
    } else {
        return Err(ParseError::new(0, 0, "unable to find open brace for struct definition"));
    }

    let fields = fields_string
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.split_once(':'))
        .collect::<Option<Vec<_>>>();

    if fields.is_none() {
        return Err(ParseError::new(0, 0, &format!("unable to parse field entries for {struct_name}")));
    }

    let fields = fields.unwrap();

    if fields.is_empty() {
        return Err(ParseError::new(0, 0, &format!("no fields provided for {struct_name}")));
    }

    fn to_field_entry(e: (&str, &str), parser: &ParserState) -> Result<(String, Box<SpType>), ParseError> {
        if !is_valid_name(e.0) {
            Err(ParseError::new(0, 0, &format!("field `{}` is not a valid name", e.0)))
        }
        else if let Ok(t) = parser.get_type(e.1) {
            Ok((e.0.to_string(), Box::new(t)))
        } else {
            Err(ParseError::new(0, 0, &format!("no type `{}` found for field `{}`", e.1, e.0)))
        }
    }

    let fields_parsed = fields
        .iter()
        .map(|i| to_field_entry(*i, state))
        .collect::<Result<Vec<_>, _>>()?;

    let type_val = SpType::Struct {
        name: struct_name.to_string(),
        fields: fields_parsed,
    };

    if state.get_type(struct_name).is_ok() {
        return Err(ParseError::new(0, 0, &format!("cannot create struct `{struct_name}` - type with name already exists!")));
    } else {
        state.types.insert(struct_name.to_string(), type_val);
    }

    Ok(remaining)
}

fn parse_def_statement<'a>(s: &'a str, state: &mut ParserState) -> Result<&'a str, ParseError> {
    if let Some(type_split) = s.find(':') {
        let name = s[..type_split].trim();

        if !is_valid_name(name) {
            return Err(ParseError::new(0, 0, &format!("variable name `{name}` not valid")));
        }

        let end_ind = s.find(';').unwrap();

        let remaining = &s[type_split+1..end_ind];
        let type_name;
        let expression;

        if let Some(expr_ind) = remaining.find('=') {
            type_name = remaining[..expr_ind].trim();
            expression = Some(remaining[expr_ind+1..].trim());
        } else {
            type_name = remaining.trim();
            expression = None;
        }

        let t = match state.get_type(type_name) {
            Err(s) => return Err(ParseError::new(0, 0, &format!("error getting type - {s}"))),
            Ok(v) => v,
        };

        if expression.is_some() {
            return Err(ParseError::new(0, 0, "expressions not yet supported!"));
        }

        println!("Defining type {name} as type {}", t.to_string());

        Ok(s[end_ind+1..].trim_start())
    } else {
        Err(ParseError::new(0, 0, "no type provided in statement!"))
    }
}

fn skip_to_next_line(s: &str) -> Result<&str, ParseError> {
    if let Some(i) = s.find('\n') {
        Ok(&s[i+1..])
    }
    else {
        Ok("")
    }
}

fn skip_to_end_of_comment_block(s: &str) -> Result<&str, ParseError> {
    if let Some(i) = s.find("*/") {
        Ok(&s[i+2..])
    } else {
        Err(ParseError::new(0, 0, "no end comment block provided!"))
    }
}

pub struct ParserState {
    pub statement: Vec<Box<dyn BaseStatement>>,
    pub types: HashMap<String, SpType>,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn get_type(&self, t: &str) -> Result<SpType, ParseError> {
        match t.chars().next() {
            Some('*') => Ok(SpType::Pointer { base: Box::new(self.get_type(&t[1..])?) }),
            Some('$') => Ok(SpType::Constant { base: Box::new(self.get_type(&t[1..])?) }),
            Some('[') => {
                if let Some(ind) = t.find(']') {
                    let size = match t[1..ind].parse::<u16>() {
                        Ok(v) => v,
                        Err(_) => return Err(ParseError::new(0, 0, &format!("unable to get array size for `{t}`"))),
                    };

                    Ok(SpType::Array { base: Box::new(self.get_type(&t[ind+1..])?), size: size as usize })
                } else {
                    Err(ParseError::new(0, 0, &format!("unable to find array type for `{t}`")))
                }
            }
            Some(_) => {
                if t.chars().any(|c| c.is_whitespace()) {
                    Err(ParseError::new(0, 0, &format!("type `{t}` should not contain whitespace")))
                } else if let Some(t) = self.types.get(t) {
                    Ok(t.clone())
                } else {
                    Err(ParseError::new(0, 0, &format!("no type found for `{t}`")))
                }
            }
            None => Err(ParseError::new(0, 0, &format!("type `{t}` should not be empty")))
        }
    }
}

impl Default for ParserState {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ParseError {
    line: usize,
    col: usize,
    msg: String,
}

impl ParseError {
    pub fn new(l: usize, c: usize, msg: &str) -> Self {
        Self {
            line: l,
            col: c,
            msg: msg.to_string(),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{} {}", self.line, self.col, self.msg)
    }
}
