use super::components::BaseStatement;

static KEY_FN: &str = "fn";
static KEY_ASM_FN: &str = "asmfn";
static KEY_CONST: &str = "const";
static KEY_VAR: &str = "var";

pub struct ParserState {
    pub statement: Vec<Box<dyn BaseStatement>>
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            statement: Vec::new()
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
            "const" => parse_var_statement(remaining, &mut state, true),
            "var" => parse_var_statement(remaining, &mut state, false),
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

fn parse_var_statement<'a>(s: &'a str, state: &mut ParserState, constant: bool) -> &'a str {
    panic!("not implemented");
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
        &s[i+1..]
    } else {
        ""
    }
}
