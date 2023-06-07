use super::{InstructionData, ToInstructionData};

use crate::assembly::{argument::Argument, error::AssemblerError};

#[derive(Copy, Clone)]
struct ImmedateByteValues {
    val: u8,
}

impl ImmedateByteValues {
    pub fn new(val: u8) -> Self {
        Self { val }
    }

    pub fn get_low_nybble(&self) -> u8 {
        self.val & 0xF
    }

    pub fn get_high_nybble(&self) -> u8 {
        (self.val & 0xF0) >> 4
    }
}

#[derive(Clone, Copy)]
pub struct ImmediateSingleInstruction {
    opcode: u8,
}

impl ImmediateSingleInstruction {
    pub fn new(opcode: u8) -> ImmediateSingleInstruction {
        ImmediateSingleInstruction { opcode }
    }
}

impl ToInstructionData for ImmediateSingleInstruction {
    fn to_instruction_data(&self, args: &[Argument]) -> Result<InstructionData, AssemblerError> {
        if args.len() != 1 {
            return Err(AssemblerError::ArgumentCount { expected: 1, actual: args.len() });
        }

        let immediate_val = match args[0].to_u8() {
            Ok(v) => ImmedateByteValues::new(v),
            Err(e) => return Err(AssemblerError::ArgumentError(e)),
        };

        Ok(InstructionData {
            opcode: 0,
            arg0: self.opcode,
            arg1: immediate_val.get_high_nybble(),
            arg2: immediate_val.get_low_nybble(),
        })
    }
}

#[derive(Clone, Copy)]
pub struct ImmediateRegisterInstruction {
    opcode: u8,
}

impl ImmediateRegisterInstruction {
    pub fn new(opcode: u8) -> Self {
        Self { opcode }
    }
}

impl ToInstructionData for ImmediateRegisterInstruction {
    fn to_instruction_data(&self, args: &[Argument]) -> Result<InstructionData, AssemblerError> {
        if args.len() != 2 {
            return Err(AssemblerError::ArgumentCount { expected: 2, actual: args.len() });
        }

        let reg = match args[0].to_register_val() {
            Ok(v) => v,
            Err(s) => return Err(AssemblerError::ArgumentError(s)),
        };

        let immediate_val = match args[1].to_u8() {
            Ok(v) => ImmedateByteValues::new(v),
            Err(e) => return Err(AssemblerError::ArgumentError(e)),
        };

        Ok(InstructionData {
            opcode: self.opcode,
            arg0: immediate_val.get_high_nybble(),
            arg1: immediate_val.get_low_nybble(),
            arg2: reg.get_index() as u8,
        })
    }
}
