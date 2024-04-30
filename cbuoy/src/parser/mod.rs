use std::collections::HashMap;
use std::fmt::Display;
use std::sync::OnceLock;

use regex::Regex;

use crate::components::expression::{
    BinaryExpression, BinaryOperator, Expression, Literal, UnaryExpression, UnaryOperator,
};
use crate::components::{
    AsmFunction, BaseStatement, CodeFunction, CompilerState, DefinitionStatement,
    ExpressionStatement, Statement,
};
use crate::tokenizer::{tokenize, Token, TokenIter, TokenIterError, TokenizeError};
use crate::types::{StructDef, TypeError};
use crate::types::{Type, TypeDict};

pub fn parse(s: &str) -> Result<(), ParseError> {
    parse_with_state(s, &mut ParserState::default())
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
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<CodeFunction, ParseError> {
    // Get function name
    let name_tok = tokens.expect()?;
    let name = name_tok.get_value();
    if !is_identifier(name) {
        return Err(ParseError::new_tok(
            name_tok,
            "fn name must be an identifier".into(),
        ));
    }

    // Expect a parenthesis
    tokens.expect_value("(")?;

    let mut parameters = Vec::new();
    while !tokens.peek_expect(")") {
        let name = tokens.expect()?;
        if !is_identifier(name.get_value()) {
            return Err(ParseError::new_tok(
                name,
                "argument name is not a valid identifier".into(),
            ));
        }

        tokens.expect_value(":")?;

        let mut type_tokens = Vec::new();
        while !tokens.peek_expect(",") && !tokens.peek_expect(")") {
            type_tokens.push(tokens.expect()?);
        }

        let arg_type = state.compiler.types.parse_type(
            &type_tokens
                .iter()
                .map(|t| t.get_value())
                .collect::<Vec<_>>()
                .join(" "),
        )?;
        parameters.push((name.get_value().to_owned(), arg_type));
    }

    // Get Return Type
    let mut ret_tokens = Vec::new();

    while !tokens.peek_expect("{") {
        ret_tokens.push(tokens.expect()?);
    }

    let ret_type = if ret_tokens.is_empty() {
        None
    } else {
        Some(
            state.compiler.types.parse_type(
                &ret_tokens
                    .iter()
                    .map(|t| t.get_value())
                    .collect::<Vec<_>>()
                    .join(" "),
            )?,
        )
    };

    tokens.expect_value("{");

    let mut statements = Vec::new();
    while !tokens.peek_expect("}") {
        statements.push(parse_statement(tokens, state)?);
    }

    tokens.expect_value("}");

    Ok(CodeFunction::new(parameters, ret_type, statements))
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

    if !Type::is_valid_name(&struct_name) || struct_name == "void" {
        return Err(ParseError::new_tok(
            first_tok,
            format!("struct name `{struct_name}` is not a valid type name"),
        ));
    }

    if let Some(end_check) = tokens.next() {
        if end_check.get_value() == ";" {
            match state.compiler.types.parse_type(&struct_name) {
                Ok(Type::Struct { .. }) => (),
                Ok(Type::OpaqueType { .. }) => (),
                Err(_) => {
                    state.compiler.types.add_type(Type::OpaqueType {
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
        tokens.expect_value(":")?;

        let mut type_vec = vec![tokens.expect()?];

        while let Some(v) = tokens.peek() {
            if v.get_value() == "," || v.get_value() == "}" {
                break;
            }

            type_vec.push(tokens.expect()?);
        }

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

    tokens.expect_value("}")?;

    if fields.is_empty() {
        return Err(ParseError::new_tok(
            first_tok,
            format!("no fields provided for {struct_name}"),
        ));
    }

    let type_val = Type::Struct(StructDef::new(&struct_name, fields));

    match state.compiler.types.parse_type(&struct_name) {
        Ok(Type::OpaqueType { .. }) => (),
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
    let mut binary_map = HashMap::new();
    binary_map.insert("+", BinaryOperator::Add);
    binary_map.insert("-", BinaryOperator::Sub);
    binary_map.insert("*", BinaryOperator::Mul);
    binary_map.insert("/", BinaryOperator::Div);
    binary_map.insert("%", BinaryOperator::Rem);
    binary_map.insert("<<", BinaryOperator::Bshl);
    binary_map.insert(">>", BinaryOperator::Bshr);
    binary_map.insert("&", BinaryOperator::Band);
    binary_map.insert("|", BinaryOperator::Bor);
    binary_map.insert("^", BinaryOperator::Bxor);
    binary_map.insert("&&", BinaryOperator::Land);
    binary_map.insert("||", BinaryOperator::Lor);

    let init_expr = parse_expression(tokens, state)?;

    if let Some(pt) = tokens.peek() {
        let pt_val = pt.get_value();
        if pt_val == "=" {
            tokens.expect()?;
            panic!("Assignment not yet supported :-(");
        } else if let Some(op) = binary_map.get(pt_val) {
            tokens.expect()?;
            let right_expr = parse_expression(tokens, state)?;
            return Ok(Box::new(BinaryExpression::new(*op, init_expr, right_expr)?));
        }
    }

    Ok(init_expr)
}

fn parse_statement(
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<Box<dyn Statement>, ParseError> {
    if let Some(pt) = tokens.peek() {
        let pt_val = pt.get_value();

        if pt_val == "while" {
            tokens.expect()?;
            panic!("while statement not yet supported");
        } else if pt_val == "if" {
            tokens.expect()?;
            panic!("if statement not yet supported");
        } else if pt_val == "def" {
            tokens.expect()?;
            let var_name = tokens.expect()?;
            tokens.expect_value(":")?;

            let mut type_tokens = Vec::new();
            while !tokens.peek_expect("=") && !tokens.peek_expect(";") {
                type_tokens.push(tokens.expect()?.get_value().to_owned());
            }

            let var_type = state.compiler.types.parse_type(&type_tokens.join(" "))?;

            let mut spacer = tokens.expect()?;
            let var_expr = if spacer.get_value() == "=" {
                let expr = parse_base_expression(tokens, state)?;
                spacer = tokens.expect()?;
                Some(expr)
            } else {
                None
            };

            if spacer.get_value() == ";" {
                panic!("Create the variable defintion");
            } else {
                return Err(ParseError::new_tok(spacer, "unknown spacing token in variable definition".into()));
            }

        } else if pt_val == "return" {
            tokens.expect()?;
            if tokens.peek_expect(";") {
                tokens.expect()?;
                panic!("Immediate Return")
            } else {
                let expr = parse_base_expression(tokens, state)?;
                tokens.expect_value(";")?;
                panic!("Return with Value");
            }
        } else if pt_val == "{" {
            tokens.expect()?;
        }
    }

    // Default to just a base expression followed by a semicolon
    let expr = Box::new(ExpressionStatement::new(parse_base_expression(
        tokens, state,
    )?));
    tokens.expect_value(";")?;
    Ok(expr)
}

static IDENTIFIER_REGEX: OnceLock<Regex> = OnceLock::new();

fn is_identifier(s: &str) -> bool {
    let r = IDENTIFIER_REGEX.get_or_init(|| Regex::new(r"^[a-zA-Z_][\w]*$").unwrap());
    r.is_match(s)
}

fn parse_literal(t: &Token) -> Result<Literal, ParseError> {
    macro_rules! gen_parse_literal_type {
        ($fn_name:ident, $literal_type:ty, $enum_type:ident) => {
            fn $fn_name(val: &str) -> Option<Result<Literal, String>> {
                if val.ends_with(stringify!($literal_type)) {
                    let sp = val.trim_end_matches(stringify!($literal_type));
                    Some(match sp.parse::<$literal_type>() {
                        Ok(v) => Ok(Literal::$enum_type(v)),
                        _ => Err(format!("unable to parse as {}", stringify!($literal_type))),
                    })
                } else {
                    None
                }
            }
        };
    }

    gen_parse_literal_type!(parse_literal_u8, u8, U8);
    gen_parse_literal_type!(parse_literal_u16, u16, U16);
    gen_parse_literal_type!(parse_literal_u32, u32, U32);
    gen_parse_literal_type!(parse_literal_i8, i8, I8);
    gen_parse_literal_type!(parse_literal_i16, i16, I16);
    gen_parse_literal_type!(parse_literal_i32, i32, I32);
    gen_parse_literal_type!(parse_literal_f32, f32, F32);

    let fn_vec = [
        parse_literal_u8,
        parse_literal_u16,
        parse_literal_u32,
        parse_literal_i8,
        parse_literal_i16,
        parse_literal_i32,
        parse_literal_f32,
    ];

    for f in fn_vec {
        if let Some(res) = f(t.get_value()) {
            return match res {
                Ok(v) => Ok(v),
                Err(e) => Err(ParseError::new_tok(t.clone(), e)),
            };
        }
    }

    if let Ok(v) = t.get_value().parse::<u32>() {
        Ok(Literal::U32(v))
    } else {
        Err(ParseError::new_tok(
            t.clone(),
            "unable to determine appropriate literal type".into(),
        ))
    }
}

fn parse_expression(
    tokens: &mut TokenIter,
    state: &mut ParserState,
) -> Result<Box<dyn Expression>, ParseError> {
    let mut unary_map = HashMap::new();
    unary_map.insert("+", UnaryOperator::Positive);
    unary_map.insert("-", UnaryOperator::Negative);
    unary_map.insert("!", UnaryOperator::Not);
    unary_map.insert("~", UnaryOperator::BitwiseNot);
    unary_map.insert("&", UnaryOperator::AddressOf);
    unary_map.insert("*", UnaryOperator::Dereference);

    let first = if let Some(t) = tokens.next() {
        t
    } else {
        return Err(ParseError::new("no tokens provided to expression!".into()));
    };

    if first.get_value() == "(" {
        let expr = parse_base_expression(tokens, state);

        if tokens.expect_value(")").is_ok() {
            expr
        } else {
            Err(ParseError::new("expected ending parenthesis".into()))
        }
    } else if let Some(op) = unary_map.get(first.get_value()) {
        Ok(Box::new(UnaryExpression::new(
            *op,
            parse_expression(tokens, state)?,
        )?))
    } else if is_identifier(first.get_value()) {
        if let Some(var) = state.compiler.get_variable_expr(first.get_value()) {
            if var.get_type()?.is_func() {
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
        Ok(Box::new(lit))
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

    if !Type::is_valid_name(&name) {
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

    tokens.expect_value(";")?;

    // TODO - This works for base statements, but not anything else :-(
    let mut def_statement = DefinitionStatement::new(&name, type_val);

    if !expr_tokens.is_empty() {
        let mut expr_iter = TokenIter::new(expr_tokens);
        def_statement.set_init(parse_expression(&mut expr_iter, state)?);
    }

    Ok(def_statement)
}

#[derive(Default)]
pub struct ParserState {
    pub statements: Vec<Box<dyn BaseStatement>>,
    pub compiler: CompilerState,
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

impl From<TypeError> for ParseError {
    fn from(value: TypeError) -> Self {
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
            let mut state = ParserState::default();
            let expected_name = "type_name";
            let struct_string = format!(
                "struct {expected_name}\n{{\n    var1: u16,\n    var2: i16{}\n}}",
                if use_trailing_comma { "," } else { "" }
            );

            if let Err(e) = parse_with_state(&struct_string, &mut state) {
                panic!("{e}");
            }

            assert!(state.compiler.types.parse_type(expected_name).is_ok());

            if let Ok(Type::Struct(def)) = state.compiler.types.parse_type(expected_name) {
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

        let mut state = ParserState::default();

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
                    st @ Type::Struct(def) => {
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

        if let Ok(Type::Struct(def)) = state.compiler.types.parse_type(join_name) {
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
        let mut state = ParserState::default();

        let init_type_len = state.compiler.types.len();

        let struct_dec =
            "struct type_1; struct type_2; struct type_1; struct type_2; struct type_2;";
        if let Err(e) = parse_with_state(struct_dec, &mut state) {
            panic!("{e}");
        }

        assert_eq!(state.compiler.types.len(), 2 + init_type_len);

        if let Ok(Type::OpaqueType { name }) = state.compiler.types.parse_type("type_1") {
            assert_eq!(name, "type_1");
        }

        if let Ok(Type::OpaqueType { name }) = state.compiler.types.parse_type("type_2") {
            assert_eq!(name, "type_2");
        }

        let struct_def = "struct type_1 { f: *type_2 }";
        if let Err(e) = parse_with_state(struct_def, &mut state) {
            panic!("{e}");
        }

        assert_eq!(state.compiler.types.len(), 2 + init_type_len);

        if let Ok(Type::Struct(def)) = state.compiler.types.parse_type("type_1") {
            assert_eq!(def.name, "type_1");
            assert_eq!(def.fields.len(), 1);
            assert_eq!(def.fields[0].0, "f");
            assert_eq!(
                def.fields[0].1,
                Box::new(Type::Pointer {
                    base: Box::new(Type::OpaqueType {
                        name: "type_2".to_string()
                    })
                })
            );
        }

        if let Ok(Type::OpaqueType { name }) = state.compiler.types.parse_type("type_2") {
            assert_eq!(name, "type_2");
        }
    }

    #[test]
    fn test_parse_struct_void() {
        let mut state = ParserState::default();

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

        let mut state = ParserState::default();

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

        if let Err(e) = parse_with_state(comments, &mut ParserState::default()) {
            panic!("{e}");
        }
    }

    #[test]
    #[should_panic]
    fn test_parse_block_comment_invalid_location() {
        let comments = "*/ /* This is a block comment! */";
        if let Err(e) = parse_with_state(comments, &mut ParserState::default()) {
            panic!("{e}");
        }
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_line_block_comment() {
        let comments = "///*
        This is in a comment, but it really isn't!
        */";

        parse_with_state(comments, &mut ParserState::default()).unwrap();
    }

    #[test]
    fn test_base_variables() {
        let code = "def var_name: i32;
        def array_name: [5]i32;
        def int_test: u16 = 3u16;
        def ptr_test: *u16 = 6u32;
        def void_test: *void = 0u32;
        def void_er: void;";

        parse_with_state(code, &mut ParserState::default()).unwrap();
    }

    #[test]
    fn test_function_pointer() {
        let code = "def func_ptr: ^(*u8, *u16, *u32)u16 = 3049u16; def single_ptr: ^()void; def testPtr: ^() * i16;";

        parse_with_state(code, &mut ParserState::default()).unwrap();
    }
}
