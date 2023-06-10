use std::str::FromStr;

use crate::instructions::{Argument, ArgumentError, INSTRUCTION_MAP, OpcodeParseError, InstructionFunction};
use sproc::common::MemoryWord;

use once_cell::sync::Lazy;
use regex::Regex;

pub enum AssemblerCommand {
    Oper(usize),
    Load(MemoryWord),
    LoadLoc(String),
    LoadText(String),
}

#[derive(Clone, Debug)]
pub enum AssemblerCommandError {
    ArgumentCount { expected: usize, actual: usize },
    UnknownCommand,
    ArgumentType(Argument),
    ArgumentError(ArgumentError),
}

impl std::fmt::Display for AssemblerCommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArgumentCount { expected, actual } => write!(f, "command expected {expected} arguments, got {actual}"),
            Self::UnknownCommand => write!(f, "unknown command provided"),
            Self::ArgumentType(a) => write!(f, "unexpected argument type with {a}"),
            Self::ArgumentError(e) => write!(f, "{e}"),
        }
    }
}

impl TryFrom<ParsedInformation> for AssemblerCommand {
    type Error = AssemblerCommandError;
    fn try_from(cmd: ParsedInformation) -> Result<Self, Self::Error> {
        if cmd.first == "oper" {
            if cmd.arguments.len() != 1 {
                Err(AssemblerCommandError::ArgumentCount { actual: cmd.arguments.len(), expected: 1 })
            } else if let Argument::UnsignedNumber(v) = cmd.arguments[0] {
                Ok(AssemblerCommand::Oper(v as usize))
            } else {
                Err(AssemblerCommandError::ArgumentType(cmd.arguments[0].clone()))
            }
        } else if cmd.first == "load" {
            if cmd.arguments.len() != 1 {
                return Err(AssemblerCommandError::ArgumentCount { actual: cmd.arguments.len(), expected: 1 });
            }

            let value_to_load = match cmd.arguments[0].to_u16() {
                Ok(v) => MemoryWord::from(v),
                Err(e) => return Err(AssemblerCommandError::ArgumentError(e)),
            };

            Ok(AssemblerCommand::Load(value_to_load))
        } else if cmd.first == "loadloc" {
            if cmd.arguments.len() != 1 {
                return Err(AssemblerCommandError::ArgumentCount { actual: cmd.arguments.len(), expected: 1 });
            }

            let arg_label = match &cmd.arguments[0] {
                Argument::Label(label) => label.clone(),
                arg => return Err(AssemblerCommandError::ArgumentType(arg.clone())),
            };

            Ok(AssemblerCommand::LoadLoc(arg_label))
        } else if cmd.first == "loadtext" {
            if cmd.arguments.len() != 1 {
                return Err(AssemblerCommandError::ArgumentCount { actual: cmd.arguments.len(), expected: 1 });
            } else if let Argument::Text(text) = &cmd.arguments[0] {
                // Construct memory words from the text values
                Ok(AssemblerCommand::LoadText(text.clone()))
            } else {
                Err(AssemblerCommandError::ArgumentType(cmd.arguments[0].clone()))
            }
        } else {
            return Err(AssemblerCommandError::UnknownCommand);
        }
    }
}

struct ParsedInformation {
    first: String,
    arguments: Vec<Argument>,
    line_type: LineType,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum LineType {
    Command,
    Label,
    Instruction,
}

#[derive(Clone, Debug)]
pub struct LineInformation {
    pub line_number: usize,
    pub text: String,
}

impl std::fmt::Display for LineInformation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] {}", self.line_number, self.text)
    }
}

impl std::str::FromStr for ParsedInformation {
    type Err = ParseErrorInner;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        if !value.is_empty() {
            let command;
            let remaining;

            if let Some((cmd, rest)) = value.split_once(' ') {
                command = cmd;
                remaining = Some(rest.trim());
            } else {
                command = value;
                remaining = None;
            }

            static INSTRUCTION_REGEX: Lazy<Regex> = Lazy::new(|| {
                Regex::new(r"^[[:alpha:]]+$").unwrap()
            });
            static LABEL_REGEX: Lazy<Regex> = Lazy::new(|| {
                Regex::new(r"^:[[:alpha:]]([[:alnum:]]|-|_)*$").unwrap()
            });
            static COMMAND_REGEX: Lazy<Regex> = Lazy::new(|| {
                Regex::new(r"^\.[[:alpha:]]+$").unwrap()
            });

            let inst;
            let inst_type;

            if LABEL_REGEX.is_match(command) {
                inst = &command[1..];
                inst_type = LineType::Label;
            } else if COMMAND_REGEX.is_match(command) {
                inst = &command[1..];
                inst_type = LineType::Command;
            } else if INSTRUCTION_REGEX.is_match(command) {
                inst = command;
                inst_type = LineType::Instruction;
            } else {
                return Err(ParseErrorInner::UnknownLineType);
            }

            let args;
            if let Some(rem) = remaining {
                let mut split_vals = Vec::new();
                let mut last_split = 0;

                let mut last_was_escape = false;
                let mut within_quote = false;

                for (i, c) in rem.char_indices() {
                    if c == '"' && !last_was_escape {
                        within_quote = !within_quote;
                    }

                    if c == ',' && !within_quote {
                        split_vals.push(rem[last_split..i].trim());
                        last_split = i + 1;
                    }

                    if c == '\\' {
                        last_was_escape = !last_was_escape;
                    } else {
                        last_was_escape = false;
                    }
                }

                if last_split < rem.len() {
                    split_vals.push(rem[last_split..].trim())
                }

                args = split_vals.into_iter().map(Argument::from_str).collect::<Result<Vec<_>, _>>()?;
            } else {
                args = Vec::new();
            }

            Ok(Self {
                first: inst.to_lowercase(),
                arguments: args,
                line_type: inst_type,
            })
        } else {
            Err(ParseErrorInner::EmptyLine)
        }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub info: LineInformation,
    pub err: ParseErrorInner,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {} ({})", self.info.line_number, self.err, self.info.text)
    }
}

#[derive(Clone, Debug)]
pub enum ParseErrorInner {
    EmptyLine,
    UnknownLineType,
    ArgumentError(ArgumentError),
    CommandError(AssemblerCommandError),
    UnknownInstruction,
    OpcodeParseError(OpcodeParseError),
    LabelArgumentsNotEmpty,
}

impl std::fmt::Display for ParseErrorInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyLine => write!(f, "empty line"),
            Self::UnknownLineType => write!(f, "unknown line type"),
            Self::ArgumentError(e) => write!(f, "{e}"),
            Self::CommandError(e) => write!(f, "{e}"),
            Self::UnknownInstruction => write!(f, "unknown instruction"),
            Self::OpcodeParseError(e) => write!(f, "{e}"),
            Self::LabelArgumentsNotEmpty => write!(f, "label expects zero arguments"),
        }
    }
}

impl From<ArgumentError> for ParseErrorInner {
    fn from(value: ArgumentError) -> Self {
        ParseErrorInner::ArgumentError(value)
    }
}

impl From<AssemblerCommandError> for ParseErrorInner {
    fn from(value: AssemblerCommandError) -> Self {
        ParseErrorInner::CommandError(value)
    }
}

impl From<OpcodeParseError> for ParseErrorInner {
    fn from(value: OpcodeParseError) -> Self {
        Self::OpcodeParseError(value)
    }
}

#[derive(Clone)]
pub struct CreateInstructionData {
    pub create_func: InstructionFunction,
    pub default_args: Vec<Argument>,
}

pub enum ParsedValue {
    Instruction(CreateInstructionData),
    Command(AssemblerCommand),
    Label(String),
}

impl TryFrom<ParsedInformation> for ParsedValue {
    type Error = ParseErrorInner;
    fn try_from(value: ParsedInformation) -> Result<Self, Self::Error> {
        Ok(match value.line_type {
            LineType::Command => ParsedValue::Command(AssemblerCommand::try_from(value)?),
            LineType::Instruction => {
                if let Some(f) = INSTRUCTION_MAP.get(&value.first) {
                    ParsedValue::Instruction(CreateInstructionData { default_args: value.arguments, create_func: *f } )
                } else {
                    return Err(ParseErrorInner::UnknownInstruction);
                }
            },
            LineType::Label => {
                if !value.arguments.is_empty() {
                    return Err(ParseErrorInner::LabelArgumentsNotEmpty)
                }
                ParsedValue::Label(value.first)
            }
        })
    }
}

pub fn parse(lines: &[&str]) -> Result<Vec<(LineInformation, ParsedValue)>, ParseError> {
    let mut entries = Vec::new();

    for (i, l_in) in lines.iter().enumerate() {
        // Define the line number
        let line_num = i + 1;

        // Cleanup the resulting string value and remove comments
        let l: &str = match l_in.find(';') {
            Some(len) => &l_in[..len],
            None => l_in,
        }.trim();

        // Skip if empty
        if l.is_empty() {
            continue;
        }

        // Define the line information
        let line_info = LineInformation { line_number: line_num, text: l.to_string() };

        // Parse the resulting line
        let cmd = match ParsedInformation::from_str(l) {
            Ok(i) => i,
            Err(e) => return Err(ParseError { info: line_info, err: e }),
        };

        // Match the resulting parameters
        match ParsedValue::try_from(cmd) {
            Ok(v) => entries.push((line_info, v)),
            Err(e) => return Err(ParseError { info: line_info, err: e }),
        };
    }

    Ok(entries)
}
