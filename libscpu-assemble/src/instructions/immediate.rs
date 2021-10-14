use super::{InstructionData, ToInstructionData};
use super::utils::{ImmedateByteValues, read_register_value};

#[derive(Clone, Copy)]
pub struct ImmediateSingleInstruction
{
    opcode: u8
}

impl ImmediateSingleInstruction
{
    pub fn new(opcode: u8) -> ImmediateSingleInstruction
    {
        return ImmediateSingleInstruction
        {
            opcode
        };
    }
}

impl ToInstructionData for ImmediateSingleInstruction
{
    fn to_instruction_data(&self, args: &Vec<String>) -> Result<InstructionData, String>
    {
        if args.len() != 1
        {
            return Err(format!("instruction expected a 1 argument, got {0:}", args.len()));
        }

        let imm = match args[0].parse::<ImmedateByteValues>()
        {
            Ok(v) => v,
            Err(s) => return Err(s)
        };

        return Ok(InstructionData
        {
            opcode: 0,
            arg0: self.opcode,
            arg1: imm.get_high_nybble(),
            arg2: imm.get_low_nybble()
        });
    }
}

#[derive(Clone, Copy)]
pub struct ImmediateRegisterInstruction
{
    opcode: u8
}

impl ImmediateRegisterInstruction
{
    pub fn new(opcode: u8) -> ImmediateRegisterInstruction
    {
        return ImmediateRegisterInstruction
        {
            opcode
        };
    }
}

impl ToInstructionData for ImmediateRegisterInstruction
{
    fn to_instruction_data(&self, args: &Vec<String>) -> Result<InstructionData, String>
    {
        if args.len() != 2
        {
            return Err(format!("instruction expected 2 arguments, got {0:}", args.len()));
        }

        let reg = match read_register_value(&args[0])
        {
            Ok(v) => v,
            Err(s) => return Err(s)
        };

        let imm = match args[1].parse::<ImmedateByteValues>()
        {
            Ok(v) => v,
            Err(s) => return Err(s)
        };

        return Ok(InstructionData
        {
            opcode: self.opcode,
            arg0: imm.get_high_nybble(),
            arg1: imm.get_low_nybble(),
            arg2: reg
        });
    }
}
