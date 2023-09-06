use std::collections::HashMap;
use std::fmt::Display;

use super::components::BaseStatement;
use super::types::{SpType, BuiltinTypes};

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
    parse_with_state(s, &mut state)
}

fn parse_with_state(s: &str, state: &mut ParserState) -> Result<(), ParseError> {
    let mut current = s.trim_start();

    while !current.is_empty() {
        // Trim the start of the current value
        current = current.trim_start();

        // Find the first word
        let (first_word, remaining) = if let Some(ind) = current.find(char::is_whitespace) {
            (&current[..ind], &current[ind..])
        }
        else {
            (current, "")
        };

        current = match first_word {
            "fn" => parse_fn_statement(remaining, state)?,
            "asmfn" => parse_asmfn_statement(remaining, state)?,
            "def" => parse_def_statement(remaining, state)?,
            "struct" => parse_struct_statement(remaining, state)?,
            "//" => skip_to_next_line(s)?,
            "/*" => skip_to_end_of_comment_block(s)?,
            word => return Err(ParseError::new(0, 0, &format!("unknown start of base expression {word}"))),
        };
    }

    assert_eq!(current.len(), 0);

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

    if let Some(first_ind) = s.find(|c| c == '{' || c == ';') {
        struct_name = s[..first_ind].trim();

        if !SpType::is_valid_name(struct_name) {
            return Err(ParseError::new(0, 0, &format!("struct name `{struct_name}` is not a valid type name")));
        }

        if let Some(';') = s.chars().nth(first_ind) {
            match state.get_type(struct_name) {
                Ok(SpType::OpaqueType { .. }) => (),
                Ok(SpType::Struct { .. }) => (),
                Err(_) => { state.types.insert(struct_name.to_string(), SpType::OpaqueType { name: struct_name.to_string() }); },
                Ok(_) => return Err(ParseError { line: 0, col: 0, msg: format!("unexpected provided type for given type value for {struct_name}") }),
            }

            return Ok(&s[(first_ind+1)..]);
        }

        if let Some(close_ind) = s.find('}') {
            if close_ind < first_ind {
                return Err(ParseError::new(0, 0, "struct unexpected closing brace before open brace"));
            }

            fields_string = s[first_ind+1..close_ind].trim();
            remaining = s[close_ind+1..].trim_start();
        } else {
            return Err(ParseError::new(0, 0, "no closing brace found"));
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

    let fields = fields.unwrap().iter()
        .map(|(s1, s2)| (s1.trim(), s2.trim()))
        .collect::<Vec<_>>();

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

    match state.get_type(struct_name) {
        Ok(SpType::OpaqueType { .. }) => state.types.insert(struct_name.to_string(), type_val),
        Ok(_) => return Err(ParseError::new(0, 0, &format!("cannot create struct `{struct_name}` - type with name already exists!"))),
        _ => state.types.insert(struct_name.to_string(), type_val),
    };

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

        println!("Defining type {name} as type {t}");

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
        let mut s = Self {
            statement: Vec::new(),
            types: HashMap::new(),
        };

        s.types.insert("void".into(), SpType::OpaqueType { name: "void".to_string() });
        s.types.insert("u16".into(), SpType::Primitive{ base: BuiltinTypes::U16 });
        s.types.insert("i16".into(), SpType::Primitive{ base: BuiltinTypes::I16 });

        s
    }
}

#[derive(Debug)]
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

#[cfg(test)]
mod test
{
    use super::*;

    #[test]
    fn test_parse_struct()
    {
        let mut state = ParserState::new();
        let struct_string = "
        type_name
        {
            var1: u16,
            var2: i16,
        }
        ".trim();

        let res = parse_struct_statement(struct_string, &mut state);

        assert!(res.is_ok());
        assert!(state.types.contains_key("type_name"));

        if let Ok(SpType::Struct { name, fields }) = state.get_type("type_name") {
            assert_eq!(name, "type_name");
            assert_eq!(fields.len(), 2);

            let f1 = &fields[0];

            assert_eq!(f1.0, "var1");
            assert_eq!(*f1.1, state.get_type("u16").unwrap());

            let f2 = &fields[1];

            assert_eq!(f2.0, "var2");
            assert_eq!(*f2.1, state.get_type("i16").unwrap());
        } else {
            panic!("unable to get expected type");
        }
    }

    #[test]
    fn test_parse_struct_multiple()
    {
        let simple_types = [
            "u16",
            "i16",
            "*void",
        ];

        let mut initial_types = Vec::new();

        let mut state = ParserState::new();

        for i in 0..simple_types.len() {
            let type_name = format!("type_{i}");
            let s = format!(
                "{type_name} {{ {} }}",
                (0..=i)
                    .map(|j| format!("var{j}: {}", simple_types[j]))
                    .reduce(|a, b| format!("{a}, {b}"))
                    .unwrap_or(String::new()));
            match parse_struct_statement(&s, &mut state) {
                Ok(r) => assert_eq!(r, ""),
                Err(e) => panic!("{e}")
            }

            match &state.get_type(&type_name) {
                Ok(t) => match t {
                    st @ SpType::Struct { name, fields } => {
                        assert_eq!(*name, type_name);
                        assert_eq!(fields.len(), i + 1);

                        for (j, (f_name, f_type)) in fields.iter().enumerate() {
                            assert_eq!(f_name, &format!("var{j}"));
                            assert_eq!(f_type.to_string(), simple_types[j]);
                        }

                        initial_types.push(st.clone())
                    }
                    v => panic!("unknown type `{v}` parsed"),
                }
                Err(e) => panic!("{e}")
            }
        }

        let join_name = "type_joined";
        let join_struct_string = format!("{join_name} {{ {} }}", initial_types.iter().enumerate().map(|(i, t)| format!("var{i}: {t}")).reduce(|a, b| format!("{a}, {b}")).unwrap_or(String::new()));

        match parse_struct_statement(&join_struct_string, &mut state) {
            Ok(v) => assert_eq!(v, ""),
            Err(e) => panic!("{e}")
        }

        if let Ok(SpType::Struct { name, fields }) = state.get_type(join_name) {
            assert_eq!(name, join_name);
            assert_eq!(fields.len(), initial_types.len());
            for (i, (t, (f_name, f_type))) in std::iter::zip(initial_types, fields).enumerate() {
                assert_eq!(f_name, format!("var{i}"));
                assert_eq!(f_type.to_string(), t.to_string())
            }
        } else {
            panic!("unable to find struct with name {join_name}");
        }
    }

    #[test]
    fn test_parse_struct_opaque()
    {
        let mut state = ParserState::new();

        let init_type_len = state.types.len();

        let struct_dec = "struct type_1; struct type_2; struct type_1; struct type_2; struct type_2;";
        assert!(parse_with_state(struct_dec, &mut state).is_ok());

        assert_eq!(state.types.len(), 2 + init_type_len);

        if let Ok(SpType::OpaqueType { name }) = state.get_type("type_1") {
            assert_eq!(name, "type_1");
        }

        if let Ok(SpType::OpaqueType { name }) = state.get_type("type_2") {
            assert_eq!(name, "type_2");
        }

        let struct_def = "struct type_1 { f: *type_2 }";
        assert!(parse_with_state(struct_def, &mut state).is_ok());

        assert_eq!(state.types.len(), 2 + init_type_len);

        if let Ok(SpType::Struct { name, fields }) = state.get_type("type_1") {
            assert_eq!(name, "type_1");
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "f");
            assert_eq!(fields[0].1, Box::new(SpType::Pointer { base: Box::new(SpType::OpaqueType { name: "type_2".to_string() }) }));
        }

        if let Ok(SpType::OpaqueType { name }) = state.get_type("type_2") {
            assert_eq!(name, "type_2");
        }
    }
}
