// Argument Parameter Static Values
use regex::Regex;

use once_cell::sync::Lazy;

// Argument type static values
static REGISTER_ARG_RESTR: &str = r"(\$[\w]+)";
static LABEL_ARG_RESTR: &str = r"([a-z][a-z0-9_A-Z]+)";
static NUMBER_ARG_RESTR: &str = r"([\-|+]?[\d]+)";
static HEX_ARG_RESTR: &str = r"(0x[a-f0-9A-Z]{1,4})";

// Define argument splitting values
static ARGUMENT_SPLIT_STR: &str = r"(,\s*)";

// Define the command type values
static INSTRUCTION_REGEX_STR: &str = r"([\w]+)";
static LABEL_REGEX_STR: &str = r"(:[\w][\w\d_]*)";
static COMMAND_REGEX_STR: &str = r"(\.[\w]+)";

// Turn resulting values into regex values
pub static ARG_REGISTER_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(&format!("^{0:}$", REGISTER_ARG_RESTR)).unwrap());

pub static ARG_LABEL_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(&format!("^{0:}$", LABEL_ARG_RESTR)).unwrap());
pub static ARG_NUMBER_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(&format!("^{0:}$", NUMBER_ARG_RESTR)).unwrap());

pub static ARG_HEX_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(&format!("^{0:}$", HEX_ARG_RESTR)).unwrap());

// Argument options regex string
static ARGUMENT_REGEX_STR: Lazy<String> = Lazy::new(|| {
    format!(
        "({0:}|{1:}|{2:}|{3:})",
        REGISTER_ARG_RESTR, LABEL_ARG_RESTR, NUMBER_ARG_RESTR, HEX_ARG_RESTR
    )
});

// Define how to split the resulting string
pub static ARGUMENT_SPLIT_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(ARGUMENT_SPLIT_STR).unwrap());

// Define the argument list values
static ARG_LIST_STRING: Lazy<String> = Lazy::new(|| {
    format!(
        "(({0:}({1:}{0:})*)?)",
        ARGUMENT_REGEX_STR.as_str(),
        ARGUMENT_SPLIT_STR
    )
});

// Define the options for starting values
static STARTING_REGEX_STR: Lazy<String> = Lazy::new(|| {
    format!(
        "({0:}|{1:}|{2:})",
        INSTRUCTION_REGEX_STR, LABEL_REGEX_STR, COMMAND_REGEX_STR
    )
});

// Define the regex for the overall parameters
pub static VALID_LINE_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(&format!(
        r#"^(?P<command>{0:})(\s+((?P<args>{1:})|("(?P<text>[[:ascii:]]*)")))?$"#,
        STARTING_REGEX_STR.as_str(),
        ARG_LIST_STRING.as_str()
    ))
    .unwrap()
});
