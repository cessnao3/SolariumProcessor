use super::{InstructionData, ToInstructionData};

use crate::assembly::{argument::Argument, error::AssemblerError};

#[derive(Clone, Copy)]
pub struct SingleInstruction {
    opcode: u8,
}

impl SingleInstruction {
    pub fn new(opcode: u8) -> Self {
        Self { opcode }
    }
}

impl ToInstructionData for SingleInstruction {
    fn to_instruction_data(&self, args: &[Argument]) -> Result<InstructionData, AssemblerError> {
        if !args.is_empty() {
            return Err(AssemblerError::ArgumentCount { expected: 0, actual: args.len() });
        }

        Ok(InstructionData {
            opcode: 0,
            arg0: 0,
            arg1: 0,
            arg2: self.opcode,
        })
    }
}
