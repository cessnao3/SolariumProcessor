use std::collections::HashMap;

use super::components::BaseStatement;
use super::types::SpType;

static KEY_FN: &str = "fn";
static KEY_ASM_FN: &str = "asmfn";
static KEY_CONST: &str = "const";
static KEY_VAR: &str = "var";

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

    pub fn get_type(&self, t: &str) -> Result<SpType, String> {
        match t.chars().next() {
            Some('*') => Ok(SpType::Pointer { base: Box::new(self.get_type(&t[1..])?) }),
            Some('$') => Ok(SpType::Constant { base: Box::new(self.get_type(&t[1..])?) }),
            Some('[') => {
                if let Some(ind) = t.find(']') {
                    let size = match t[1..ind].parse::<u16>() {
                        Ok(v) => v,
                        Err(_) => return Err(format!("unable to get array size for `{t}`")),
                    };

                    Ok(SpType::Array { base: Box::new(self.get_type(&t[ind+1..])?), size: size as usize })
                } else {
                    Err(format!("unable to find array type for `{t}`"))
                }
            }
            Some(_) => {
                if t.chars().any(|c| c.is_whitespace()) {
                    Err(format!("type `{t}` should not contain whitespace"))
                } else if let Some(t) = self.types.get(t) {
                    Ok(t.clone())
                } else {
                    Err(format!("no type found for `{t}`"))
                }
            }
            None => Err(format!("type `{t}` should not be empty"))
        }
    }
}

impl Default for ParserState {
    fn default() -> Self {
        Self::new()
    }
}

pub fn parse(s: &str) -> Result<(), String> {
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
            "fn" => parse_fn_statement(remaining, &mut state),
            "asmfn" => parse_asmfn_statement(remaining, &mut state),
            "def" => parse_def_statement(remaining, &mut state),
            "//" => parse_comment(s),
            "/*" => parse_comment_block(s),
            word => return Err(format!("unknown start of base expression {word}"))
        };
    }

    Ok(())
}

fn parse_fn_statement<'a>(s: &'a str, state: &mut ParserState) -> &'a str {
    panic!("not implemented");
}

fn parse_asmfn_statement<'a>(s: &'a str, state: &mut ParserState) -> &'a str {
    panic!("not implemented");
}

fn parse_def_statement<'a>(s: &'a str, state: &mut ParserState) -> &'a str {
    if let Some(type_split) = s.find(':') {
        let name = s[..type_split].trim();

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
            Err(s) => panic!("error getting type - {s}"),
            Ok(v) => v,
        };

        if expression.is_some() {
            panic!("expressions not yet supported!");
        }

        println!("Defining type {name} as type {}", t.to_string());

        &s[end_ind+1..]
    } else {
        panic!("no type provided!")
    }
}

fn parse_comment(s: &str) -> &str {
    if let Some(i) = s.find('\n') {
        &s[i+1..]
    }
    else {
        ""
    }
}

fn parse_comment_block(s: &str) -> &str {
    if let Some(i) = s.find("*/") {
        &s[i+2..]
    } else {
        panic!("no end comment block provided!")
    }
}
