use super::{InstructionData, ToInstructionData};

use crate::assembly::argument::Argument;

#[derive(Copy, Clone)]
struct ImmedateByteValues {
    val: u8,
}

impl ImmedateByteValues {
    pub fn new(val: u8) -> ImmedateByteValues {
        return ImmedateByteValues { val };
    }

    pub fn get_low_nybble(&self) -> u8 {
        return self.val & 0xF;
    }

    pub fn get_high_nybble(&self) -> u8 {
        return (self.val & 0xF0) >> 4;
    }
}

#[derive(Clone, Copy)]
pub struct ImmediateSingleInstruction {
    opcode: u8,
}

impl ImmediateSingleInstruction {
    pub fn new(opcode: u8) -> ImmediateSingleInstruction {
        return ImmediateSingleInstruction { opcode };
    }
}

impl ToInstructionData for ImmediateSingleInstruction {
    fn to_instruction_data(&self, args: &Vec<Argument>) -> Result<InstructionData, String> {
        if args.len() != 1 {
            return Err(format!(
                "instruction expected a 1 argument, got {0:}",
                args.len()
            ));
        }

        let immediate_val = match args[0].to_u8() {
            Ok(v) => ImmedateByteValues::new(v),
            Err(e) => return Err(e),
        };

        return Ok(InstructionData {
            opcode: 0,
            arg0: self.opcode,
            arg1: immediate_val.get_high_nybble(),
            arg2: immediate_val.get_low_nybble(),
        });
    }
}

#[derive(Clone, Copy)]
pub struct ImmediateRegisterInstruction {
    opcode: u8,
}

impl ImmediateRegisterInstruction {
    pub fn new(opcode: u8) -> ImmediateRegisterInstruction {
        return ImmediateRegisterInstruction { opcode };
    }
}

impl ToInstructionData for ImmediateRegisterInstruction {
    fn to_instruction_data(&self, args: &Vec<Argument>) -> Result<InstructionData, String> {
        if args.len() != 2 {
            return Err(format!(
                "instruction expected 2 arguments, got {0:}",
                args.len()
            ));
        }

        let reg = match args[0].to_register_val() {
            Ok(v) => v,
            Err(s) => return Err(s),
        };

        let immediate_val = match args[1].to_u8() {
            Ok(v) => ImmedateByteValues::new(v),
            Err(e) => return Err(e),
        };

        return Ok(InstructionData {
            opcode: self.opcode,
            arg0: immediate_val.get_high_nybble(),
            arg1: immediate_val.get_low_nybble(),
            arg2: reg,
        });
    }
}
