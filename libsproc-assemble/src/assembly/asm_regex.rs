// Argument Parameter Static Values
use regex::Regex;

use lazy_static::lazy_static;

// Argument type static values
static REGISTER_ARG_RESTR: &str = r"(\$[\w]+)";
static LABEL_ARG_RESTR: &str = r"([a-z][a-z0-9_]+)";
static NUMBER_ARG_RESTR: &str = r"([\-|+]?[\d]+)";
static HEX_ARG_RESTR: &str = r"(0x[a-f0-9]{1,4})";

// Define argument splitting values
static ARGUMENT_SPLIT_STR: &str = r"(,\s*)";

// Define the command type values
static INSTRUCTION_REGEX_STR: &str = r"([\w]+)";
static LABEL_REGEX_STR: &str = r"(:[\w][\w\d_]*)";
static COMMAND_REGEX_STR: &str = r"(\.[\w]+)";

lazy_static!
{
    // Turn resulting values into regex values
    pub static ref ARG_REGISTER_REGEX: Regex = Regex::new(&format!(
        "^{0:}$",
        REGISTER_ARG_RESTR)).unwrap();
    pub static ref ARG_LABEL_REGEX: Regex = Regex::new(&format!(
        "^{0:}$",
        LABEL_ARG_RESTR)).unwrap();
    pub static ref ARG_NUMBER_REGEX: Regex = Regex::new(&format!(
        "^{0:}$",
        NUMBER_ARG_RESTR)).unwrap();
    pub static ref ARG_HEX_REGEX: Regex = Regex::new(&format!(
        "^{0:}$",
        HEX_ARG_RESTR)).unwrap();

    // Argument options regex string
    static ref ARGUMENT_REGEX_STR: String = format!(
        "({0:}|{1:}|{2:}|{3:})",
        REGISTER_ARG_RESTR,
        LABEL_ARG_RESTR,
        NUMBER_ARG_RESTR,
        HEX_ARG_RESTR);

    // Define how to split the resulting string
    pub static ref ARGUMENT_SPLIT_REGEX: Regex = Regex::new(ARGUMENT_SPLIT_STR).unwrap();

    // Define the argument list values
    static ref ARG_LIST_STRING: String = format!(
        "(({0:}({1:}{0:})*)?)",
        ARGUMENT_REGEX_STR.to_string(),
        ARGUMENT_SPLIT_STR);

    // Define the options for starting values
    static ref STARTING_REGEX_STR: String = format!(
        "({0:}|{1:}|{2:})",
        INSTRUCTION_REGEX_STR,
        LABEL_REGEX_STR,
        COMMAND_REGEX_STR);

    // Define the regex for the overall parameters
    pub static ref VALID_LINE_REGEX: Regex = Regex::new(&format!(
        r"^(?P<command>{0:})(\s+(?P<args>{1:}))?$",
        STARTING_REGEX_STR.to_string(),
        ARG_LIST_STRING.to_string())).unwrap();
}
