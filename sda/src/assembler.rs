use crate::instructions::{Argument, OpcodeParseError};
use crate::parser::{ParsedValue, LineInformation, AssemblerCommand, ParseError, ParseErrorInner, CreateInstructionData};
use sproc::common::{MemoryWord, InstructionError};
use sproc::text::{character_to_word, CharacterError};

use std::collections::HashMap;

const MAX_ADDRESSABLE_VALUE: usize = (2usize).pow(16);

enum AssembleResult {
    Word(MemoryWord),
    Opcode(LineInformation, CreateInstructionData),
    GetLabel(LineInformation, String),
}

struct AssemblyState {
    map: HashMap<usize, AssembleResult>,
    labels: HashMap<String, usize>,
    next_loc: usize,
}

impl AssemblyState {
    fn new() -> Self {
        Self { map: HashMap::new(), labels: HashMap::new(), next_loc: 0 }
    }

    fn move_value(&mut self, new_val: usize) -> Result<(), AssemblerErrorInner> {
        if new_val < self.next_loc {
            return Err(AssemblerErrorInner::InvalidNewValue{ new: new_val, existing: self.next_loc });
        }

        self.next_loc = new_val;
        Ok(())
    }

    fn add_word(&mut self, word: MemoryWord) -> Result<(), AssemblerErrorInner> {
        self.add_res(AssembleResult::Word(word))
    }

    fn add_opcode(&mut self, l: &LineInformation, op: CreateInstructionData) -> Result<(), AssemblerErrorInner> {
        self.add_res(AssembleResult::Opcode(l.clone(), op))
    }

    fn add_label(&mut self, s: &str) -> Result<(), AssemblerErrorInner> {
        if self.labels.contains_key(s) {
            Err(AssemblerErrorInner::DuplicateLabel(s.to_string()))
        } else {
            self.labels.insert(s.to_string(), self.next_loc);
            Ok(())
        }
    }

    fn add_load_label(&mut self, l: &LineInformation, s: &str) -> Result<(), AssemblerErrorInner> {
        self.add_res(AssembleResult::GetLabel(l.clone(), s.to_string()))
    }

    fn add_res(&mut self, r: AssembleResult) -> Result<(), AssemblerErrorInner> {
        if let std::collections::hash_map::Entry::Vacant(e) = self.map.entry(self.next_loc) {
            if self.next_loc >= MAX_ADDRESSABLE_VALUE {
                Err(AssemblerErrorInner::AddressTooLarge(self.next_loc))
            } else {
                e.insert(r);
                self.next_loc += 1;
                Ok(())
            }
        } else {
            Err(AssemblerErrorInner::DuplicateAddress(self.next_loc))
        }
    }

    fn to_memory_vector(&self) -> Result<Vec<MemoryWord>, AssemblerError> {
        let max_index = match self.map.keys().max() {
            Some(v) => v,
            None => return Ok(Vec::new()),
        };

        let mut data_vec = Vec::new();
        data_vec.resize(max_index + 1, MemoryWord::default());

        for (i, val) in self.map.iter() {
            match val {
                AssembleResult::GetLabel(loc, lbl) => {
                    if let Some(dest) = self.labels.get(lbl) {
                        data_vec[0] = MemoryWord::from(*dest as u16);
                    } else {
                        return Err(AssemblerError { info: loc.clone(), err: AssemblerErrorInner::MissingLabel(lbl.clone()) });
                    }
                }
                AssembleResult::Opcode(loc, op) => {
                    // Create new arguments to update for assembly locations
                    let new_args = op.default_args.iter().map(|a| {
                        match a {
                            Argument::Label(s) => match self.labels.get(s) {
                                Some(data_index) => {
                                    let delta_index = *data_index as i32 - *i as i32;
                                    Ok(Argument::SignedNumber(delta_index))
                                }
                                None => Err(AssemblerError { info: loc.clone(), err: AssemblerErrorInner::MissingLabel(s.to_string()) }),
                            },
                            a => Ok(a.clone()),
                        }
                    }).collect::<Result<Vec<_>, _>>()?;

                    // Helper function for inner errors
                    fn inner_create_fnc(op: &CreateInstructionData, args: &[Argument]) -> Result<MemoryWord, AssemblerErrorInner> {
                        // Create the new instruction
                        Ok(MemoryWord::try_from((op.create_func)(args)?.to_instruction()?)?)
                    }

                    // Add the resulting parameter
                    data_vec[*i] = match inner_create_fnc(op, &new_args) {
                        Ok(v) => v,
                        Err(e) => return Err(AssemblerError { info: loc.clone(), err: e }),
                    }
                }
                AssembleResult::Word(w) => data_vec[*i] = *w,
            };
        }

        Ok(data_vec)
    }
}

fn assemble_individual(state: &mut AssemblyState, l: &LineInformation, p: &ParsedValue) -> Result<(), AssemblerErrorInner> {
    match p {
        ParsedValue::Command(cmd) => match cmd {
            AssemblerCommand::Oper(new_offset) => {
                state.move_value(*new_offset)?;
            }
            AssemblerCommand::Load(w) => {
                state.add_word(*w)?;
            }
            AssemblerCommand::LoadLoc(lbl) => {
                state.add_load_label(l, lbl)?;
            }
            AssemblerCommand::LoadText(txt) => {
                for c in txt.chars() {
                    state.add_word(character_to_word(c)?)?;
                }
                state.add_word(MemoryWord::default())?;
            }
        },
        ParsedValue::Label(label) => {
            state.add_label(label)?;
        },
        ParsedValue::Instruction(inst) => {
            state.add_opcode(l, inst.clone())?;
        }
    }

    Ok(())
}

pub fn assemble(parsed: &[(LineInformation, ParsedValue)]) -> Result<Vec<MemoryWord>, AssemblerError> {
    let mut state = AssemblyState::new();

    for (l, p) in parsed {
        match assemble_individual(&mut state, l, p) {
            Ok(()) => (),
            Err(e) => return Err(AssemblerError { info: l.clone(), err: e }),
        };
    }

    state.to_memory_vector()
}


#[derive(Clone, Debug)]
pub struct AssemblerError {
    pub info: LineInformation,
    pub err: AssemblerErrorInner,
}

impl std::fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {} ({})", self.info.line_number, self.err, self.info.text)
    }
}

impl From<ParseError> for AssemblerError {
    fn from(value: ParseError) -> Self {
        Self {
            info: value.info,
            err: AssemblerErrorInner::from(value.err),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AssemblerErrorInner {
    ParseError(ParseErrorInner),
    InstructionError(InstructionError),
    CharacterError(CharacterError),
    OpcodeParseError(OpcodeParseError),
    DuplicateLabel(String),
    InvalidNewValue{ new: usize, existing: usize },
    DuplicateAddress(usize),
    AddressTooLarge(usize),
    MissingLabel(String),
}

impl std::fmt::Display for AssemblerErrorInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError(e) => write!(f, "{e}"),
            Self::InstructionError(e) => write!(f, "{e}"),
            Self::CharacterError(e) => write!(f, "{e}"),
            Self::OpcodeParseError(e) => write!(f, "{e}"),
            Self::DuplicateLabel(l) => write!(f, "duplicate label \"{l}\" provided"),
            Self::InvalidNewValue { new, existing } => write!(f, "unable to move next location from {existing} to {new}"),
            Self::DuplicateAddress(addr) => write!(f, "cannot add duplicate value to address {addr}"),
            Self::AddressTooLarge(addr) => write!(f, "address {addr} exceeds bounds"),
            Self::MissingLabel(l) => write!(f, "unable to find point associated with label \"{l}\""),
        }
    }
}

impl From<ParseErrorInner> for AssemblerErrorInner {
    fn from(value: ParseErrorInner) -> Self {
        Self::ParseError(value)
    }
}

impl From<OpcodeParseError> for AssemblerErrorInner {
    fn from(value: OpcodeParseError) -> Self {
        Self::OpcodeParseError(value)
    }
}

impl From<InstructionError> for AssemblerErrorInner {
    fn from(value: InstructionError) -> Self {
        Self::InstructionError(value)
    }
}

impl From<CharacterError> for AssemblerErrorInner {
    fn from(value: CharacterError) -> Self {
        Self::CharacterError(value)
    }
}
