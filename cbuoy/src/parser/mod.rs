use std::collections::HashMap;
use std::fmt::Display;

use crate::components::expression::{Expression, Literal, UnaryExpression, UnaryOperator};
use crate::components::{
    AsmFunction, BaseStatement, CompilerState, DefinitionStatement, SpcFunction,
};
use crate::tokenizer::{tokenize, Token, TokenIter, TokenIterError, TokenizeError};
use crate::types::{SpType, SpTypeDict};
use crate::types::{SpTypeError, StructDef};

pub fn parse(s: &str) -> Result<(), ParseError> {
    parse_with_state(s, &mut ParserState::new())
}

fn parse_with_state(s: &str, state: &mut ParserState) -> Result<(), ParseError> {
    let mut tokens = TokenIter::new(
        tokenize(s)?
            .into_iter()
            .filter(|t| !t.is_comment())
            .collect::<Vec<_>>(),
    );

    while let Some(tok) = tokens.next() {
        let base: Option<Box<dyn BaseStatement>> = match tok.get_value() {
            "fn" => Some(Box::new(parse_fn_statement(&mut tokens, state)?)),
            "asmfn" => Some(Box::new(parse_asmfn_statement(&mut tokens, state)?)),
            "def" => Some(Box::new(parse_def_statement(&mut tokens, state)?)),
            "struct" => {
                parse_struct_statement(&mut tokens, state)?;
                None
            }
            word => {
                return Err(ParseError::new(format!(
                    "unknown start of base expression {word}"
                )))
            }
        };

        if let Some(base_statement) = base {
            state.statements.push(base_statement);
        }
    }

    Ok(())
}

fn parse_fn_statement(
    _tokens: &mut TokenIter,
    _state: &mut ParserState,
) -> Result<SpcFunction, ParseError> {
    panic!("not implemented");
}

fn parse_asmfn_statement(
    _tokens: &mut TokenIter,
    _state: &mut ParserState,
) -> Result<AsmFunction, ParseError> {
    panic!("not implemented");
}

fn parse_struct_statement(
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<(), ParseError> {
    // Find open brace
    let first_tok = tokens.expect()?;
    let struct_name = first_tok.get_value().to_string();

    if !SpType::is_valid_name(&struct_name) || struct_name == "void" {
        return Err(ParseError::new_tok(
            first_tok,
            format!("struct name `{struct_name}` is not a valid type name"),
        ));
    }

    if let Some(end_check) = tokens.next() {
        if end_check.get_value() == ";" {
            match state.compiler.types.parse_type(&struct_name) {
                Ok(SpType::Struct { .. }) => (),
                Ok(SpType::OpaqueType { .. }) => (),
                Err(_) => {
                    state.compiler.types.add_type(SpType::OpaqueType {
                        name: struct_name.to_string(),
                    })?;
                }
                Ok(_) => {
                    return Err(ParseError::new_tok(
                        end_check,
                        format!("unexpected provided type for given type value for {struct_name}"),
                    ))
                }
            }

            return Ok(());
        } else if end_check.get_value() != "{" {
            return Err(ParseError::new_tok(
                end_check,
                format!("expected semicolon or open brace after struct for '{struct_name}'"),
            ));
        }
    }

    let mut fields = Vec::new();

    while let Some(end_tok) = tokens.peek() {
        if end_tok.get_value() == "}" {
            break;
        }

        let field_name = tokens.expect()?;
        tokens.expect_with_value(":")?;

        let mut type_vec = vec![tokens.expect()?];

        while let Some(v) = tokens.peek() {
            if v.get_value() == "," || v.get_value() == "}" {
                break;
            }

            type_vec.push(tokens.expect()?);
        }

        //expect_token_with_name(tokens, ",")?; -> Check ENDING!

        let type_string_combined = type_vec
            .iter()
            .map(|n| n.get_value().to_string())
            .reduce(|a, b| format!("{a}{b}"))
            .unwrap_or(String::new());

        let sp_type = if let Ok(t) = state.compiler.types.parse_type(&type_string_combined) {
            Box::new(t)
        } else {
            return Err(ParseError::new_tok(
                type_vec[0].clone(),
                format!("no type `{type_string_combined}` found for field `{field_name}`"),
            ));
        };

        fields.push((field_name.get_value().to_string(), sp_type));

        if let Some(t) = tokens.peek() {
            if t.get_value() == "," {
                tokens.expect()?;
            } else if t.get_value() == "}" {
                break;
            } else {
                return Err(ParseError::new_tok(
                    t,
                    format!("unexpected token found in struct definition '{struct_name}'"),
                ));
            }
        } else {
            break;
        }
    }

    tokens.expect_with_value("}")?;

    if fields.is_empty() {
        return Err(ParseError::new_tok(
            first_tok,
            format!("no fields provided for {struct_name}"),
        ));
    }

    let type_val = SpType::Struct(StructDef::new(&struct_name, fields));

    match state.compiler.types.parse_type(&struct_name) {
        Ok(SpType::OpaqueType { .. }) => (),
        Ok(_) => {
            return Err(ParseError::new_tok(
                first_tok,
                format!("cannot create struct `{struct_name}` - type with name already exists!"),
            ))
        }
        _ => (),
    };

    state.compiler.types.add_type(type_val)?;

    Ok(())
}

fn parse_base_expression(
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<Box<dyn Expression>, ParseError> {
    if let Some(t) = tokens.next() {
        panic!("not implemented");
    } else {
        Err(ParseError::new("no tokens provided to expression!".into()))
    }
}

fn is_identifier(s: &str) -> bool {
    panic!("not supported");
}

fn parse_literal(t: &Token) -> Result<Literal, ParseError> {
    panic!("not supported");
}

fn parse_expression(
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<Box<dyn Expression>, ParseError> {
    let mut unary_map = HashMap::new();
    unary_map.insert("+", UnaryOperator::Positive);
    unary_map.insert("-", UnaryOperator::Negative);
    //unary_map.insert("&", UnaryOperator::AddressOf);
    //unary_map.insert("*", UnaryOperator::Dereference);
    unary_map.insert("!", UnaryOperator::Not);
    unary_map.insert("~", UnaryOperator::BitwiseNot);

    let first = if let Some(t) = tokens.next() {
        t
    } else {
        return Err(ParseError::new("no tokens provided to expression!".into()));
    };

    if first.get_value() == "(" {
        let expr = parse_base_expression(tokens, state);

        if let Some(t) = tokens.next() {
            if t.get_value() == ")" {
                expr
            } else {
                Err(ParseError::new_tok(t, "expected ending parenthesis".into()))
            }
        } else {
            Err(ParseError::new("expected ending parenthesis".into()))
        }
    } else if let Some(op) = unary_map.get(first.get_value()) {
        Ok(Box::new(UnaryExpression::new(
            *op,
            parse_expression(tokens, state)?,
        )?))
    } else if first.get_value() == "*" {
        panic!("dereference not yet supported")
    } else if first.get_value() == "&" {
        panic!("address of not yet supported")
    } else if is_identifier(first.get_value()) {
        if let Some(var) = state.compiler.get_variable_expr(first.get_value()) {
            if var.get_type().is_func() {
                panic!("check for function call?");
            } else {
                Ok(var)
            }
        } else {
            Err(ParseError::new_tok(
                first,
                "unknown variable nameprovided".into(),
            ))
        }
    } else if let Ok(lit) = parse_literal(&first) {
        panic!("not implemented");
    } else {
        Err(ParseError::new_tok(
            first,
            "unknown token type provided".into(),
        ))
    }
}

fn parse_def_statement(
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<DefinitionStatement, ParseError> {
    let init_tok = tokens.expect()?;
    let name = init_tok.get_value().to_string();

    if !SpType::is_valid_name(&name) {
        return Err(ParseError::new_tok(
            init_tok,
            format!("variable name '{name}' not valid"),
        ));
    }

    if let Some(i) = tokens.next() {
        if i.get_value() != ":" {
            return Err(ParseError::new_tok(
                i,
                format!("expect colon after the variable name '{name}'"),
            ));
        }
    } else {
        return Err(ParseError::new_tok(
            init_tok,
            format!("expected colon and type name after '{name}' in definition"),
        ));
    }

    let mut type_tokens = Vec::new();
    while let Some(i) = tokens.peek() {
        if i.get_value() == ";" {
            break;
        } else if i.get_value() == "=" {
            tokens.next(); // Read the = token to get ready for expression reading
            break;
        } else {
            tokens.next(); // Read the token
            type_tokens.push(i);
        }
    }

    let type_name = type_tokens
        .iter()
        .map(|i| i.get_value().to_string())
        .reduce(|a, b| format!("{a}{b}"))
        .unwrap_or(String::new());
    let type_val = state.compiler.types.parse_type(&type_name)?;

    let mut expr_tokens = Vec::new();

    while let Some(i) = tokens.peek() {
        if i.get_value() != ";" {
            expr_tokens.push(tokens.next().unwrap());
        } else {
            break;
        }
    }

    tokens.expect_with_value(";")?;

    // TODO - This works for base statements, but not anything else :-(
    let mut def_statement = DefinitionStatement::new(&name, type_val);

    if !expr_tokens.is_empty() {
        let mut expr_iter = TokenIter::new(expr_tokens);
        def_statement.set_init(parse_expression(&mut expr_iter, state)?);
    }

    Ok(def_statement)
}

pub struct ParserState {
    pub statements: Vec<Box<dyn BaseStatement>>,
    pub compiler: CompilerState,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}

impl Default for ParserState {
    fn default() -> Self {
        Self {
            statements: Vec::new(),
            compiler: CompilerState::default(),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    tok: Option<Token>,
    msg: String,
}

impl ParseError {
    pub fn new(msg: String) -> Self {
        Self { tok: None, msg }
    }

    pub fn new_tok(tok: Token, msg: String) -> Self {
        Self {
            tok: Some(tok),
            msg,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(tok) = &self.tok {
            write!(
                f,
                "{}:{} ({}) => ",
                tok.get_line(),
                tok.get_column(),
                tok.get_value()
            )?;
        }
        write!(f, "{}", self.msg)
    }
}

impl From<TokenIterError> for ParseError {
    fn from(value: TokenIterError) -> Self {
        Self::new(format!("tokiter: {value}"))
    }
}

impl From<SpTypeError> for ParseError {
    fn from(value: SpTypeError) -> Self {
        Self::new(format!("type: {value}"))
    }
}

impl From<TokenizeError> for ParseError {
    fn from(value: TokenizeError) -> Self {
        Self::new(format!("tokenizer: {value}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_struct() {
        for use_trailing_comma in [true, false] {
            let mut state = ParserState::new();
            let expected_name = "type_name";
            let struct_string = format!(
                "struct {expected_name}\n{{\n    var1: u16,\n    var2: i16{}\n}}",
                if use_trailing_comma { "," } else { "" }
            );

            if let Err(e) = parse_with_state(&struct_string, &mut state) {
                panic!("{e}");
            }

            assert!(state.compiler.types.parse_type(expected_name).is_ok());

            if let Ok(SpType::Struct(def)) = state.compiler.types.parse_type(expected_name) {
                assert_eq!(def.name, expected_name);
                assert_eq!(def.fields.len(), 2);

                let f1 = &def.fields[0];

                assert_eq!(f1.0, "var1");
                assert_eq!(*f1.1, state.compiler.types.parse_type("u16").unwrap());

                let f2 = &def.fields[1];

                assert_eq!(f2.0, "var2");
                assert_eq!(*f2.1, state.compiler.types.parse_type("i16").unwrap());
            } else {
                panic!("unable to get expected type");
            }
        }
    }

    #[test]
    fn test_parse_struct_multiple() {
        let simple_types = ["u16", "i16", "*void"];

        let mut initial_types = Vec::new();

        let mut state = ParserState::new();

        for i in 0..simple_types.len() {
            let type_name = format!("type_{i}");
            let s = format!(
                "struct {type_name} {{ {} }}",
                (0..=i)
                    .map(|j| format!("var{j}: {}", simple_types[j]))
                    .reduce(|a, b| format!("{a}, {b}"))
                    .unwrap_or(String::new())
            );
            if let Err(e) = parse_with_state(&s, &mut state) {
                panic!("{e}");
            }

            match &state.compiler.types.parse_type(&type_name) {
                Ok(t) => match t {
                    st @ SpType::Struct(def) => {
                        assert_eq!(*def.name, type_name);
                        assert_eq!(def.fields.len(), i + 1);

                        for (j, (f_name, f_type)) in def.fields.iter().enumerate() {
                            assert_eq!(f_name, &format!("var{j}"));
                            assert_eq!(f_type.to_string(), simple_types[j]);
                        }

                        initial_types.push(st.clone())
                    }
                    v => panic!("unknown type `{v}` parsed"),
                },
                Err(e) => panic!("{e}"),
            }
        }

        let join_name = "type_joined";
        let join_struct_string = format!(
            "struct {join_name} {{ {} }}",
            initial_types
                .iter()
                .enumerate()
                .map(|(i, t)| format!("var{i}: {t}"))
                .reduce(|a, b| format!("{a}, {b}"))
                .unwrap_or(String::new())
        );

        if let Err(e) = parse_with_state(&join_struct_string, &mut state) {
            panic!("{e}");
        }

        if let Ok(SpType::Struct(def)) = state.compiler.types.parse_type(join_name) {
            assert_eq!(def.name, join_name);
            assert_eq!(def.fields.len(), initial_types.len());
            for (i, (t, (f_name, f_type))) in std::iter::zip(initial_types, def.fields).enumerate()
            {
                assert_eq!(f_name, format!("var{i}"));
                assert_eq!(f_type.to_string(), t.to_string())
            }
        } else {
            panic!("unable to find struct with name {join_name}");
        }
    }

    #[test]
    fn test_parse_struct_opaque() {
        let mut state = ParserState::new();

        let init_type_len = state.compiler.types.len();

        let struct_dec =
            "struct type_1; struct type_2; struct type_1; struct type_2; struct type_2;";
        if let Err(e) = parse_with_state(struct_dec, &mut state) {
            panic!("{e}");
        }

        assert_eq!(state.compiler.types.len(), 2 + init_type_len);

        if let Ok(SpType::OpaqueType { name }) = state.compiler.types.parse_type("type_1") {
            assert_eq!(name, "type_1");
        }

        if let Ok(SpType::OpaqueType { name }) = state.compiler.types.parse_type("type_2") {
            assert_eq!(name, "type_2");
        }

        let struct_def = "struct type_1 { f: *type_2 }";
        if let Err(e) = parse_with_state(struct_def, &mut state) {
            panic!("{e}");
        }

        assert_eq!(state.compiler.types.len(), 2 + init_type_len);

        if let Ok(SpType::Struct(def)) = state.compiler.types.parse_type("type_1") {
            assert_eq!(def.name, "type_1");
            assert_eq!(def.fields.len(), 1);
            assert_eq!(def.fields[0].0, "f");
            assert_eq!(
                def.fields[0].1,
                Box::new(SpType::Pointer {
                    base: Box::new(SpType::OpaqueType {
                        name: "type_2".to_string()
                    })
                })
            );
        }

        if let Ok(SpType::OpaqueType { name }) = state.compiler.types.parse_type("type_2") {
            assert_eq!(name, "type_2");
        }
    }

    #[test]
    fn test_parse_struct_void() {
        let mut state = ParserState::new();

        let init_type_len = state.compiler.types.len();

        let s = "struct void;";

        assert!(parse_with_state(s, &mut state).is_err());
        assert_eq!(state.compiler.types.len(), init_type_len);
    }

    #[test]
    fn test_parse_line_comment() {
        let comments = "// This is a comment!
        // This is a comment, too

        // This is also a comment?";

        let mut state = ParserState::new();

        if let Err(e) = parse_with_state(comments, &mut state) {
            panic!("{e}");
        }
    }

    #[test]
    fn test_parse_block_comment() {
        let comments = "// This is a comment!
        /* This is a
        multi-line
        block comment */
        /* This is a single-line block comment */
        /* And this is as well
        */";

        if let Err(e) = parse_with_state(comments, &mut ParserState::new()) {
            panic!("{e}");
        }
    }

    #[test]
    #[should_panic]
    fn test_parse_block_comment_invalid_location() {
        let comments = "*/ /* This is a block comment! */";
        if let Err(e) = parse_with_state(comments, &mut ParserState::new()) {
            panic!("{e}");
        }
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_line_block_comment() {
        let comments = "///*
        This is in a comment, but it really isn't!
        */";

        if let Err(e) = parse_with_state(comments, &mut ParserState::new()) {
            panic!("{e}");
        }
    }
}
