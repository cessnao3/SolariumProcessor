use super::{InstructionData, ToInstructionData};

use crate::assembly::argument::Argument;

#[derive(Clone, Copy)]
pub struct SingleInstruction
{
    opcode: u8
}

impl SingleInstruction
{
    pub fn new(opcode: u8) -> SingleInstruction
    {
        return SingleInstruction
        {
            opcode
        };
    }
}

impl ToInstructionData for SingleInstruction
{
    fn to_instruction_data(&self, args: &Vec<Argument>) -> Result<InstructionData, String>
    {
        if args.len() != 0
        {
            return Err(format!(
                "instruction expected no arguments, got {0:}",
                args.len()));
        }

        return Ok(InstructionData
        {
            opcode: 0,
            arg0: 0,
            arg1: 0,
            arg2: self.opcode
        });
    }
}
