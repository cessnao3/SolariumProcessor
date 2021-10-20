use std::collections::HashMap;

mod immediate;
mod register;
mod single;
mod utils;

use immediate::{ImmediateSingleInstruction, ImmediateRegisterInstruction};
use register::RegisterInstruction;
use single::SingleInstruction;

pub struct InstructionData
{
    opcode: u8,
    arg0: u8,
    arg1: u8,
    arg2: u8
}

impl InstructionData
{
    pub fn combine(&self) -> u16
    {
        assert!(self.opcode & 0xF == self.opcode);
        assert!(self.arg0 & 0xF == self.arg0);
        assert!(self.arg1 & 0xF == self.arg1);
        assert!(self.arg2 & 0xF == self.arg2);

        return
            ((self.opcode as u16) << 12) |
            ((self.arg0 as u16) << 8) |
            ((self.arg1 as u16) << 4) |
            (self.arg2 as u16);
    }
}

pub trait ToInstructionData
{
    fn to_instruction_data(&self, args: &Vec<String>) -> Result<InstructionData, String>;
}

pub fn get_instruction_map() -> HashMap::<String, Box<dyn ToInstructionData>>
{
    let mut instructions: HashMap::<String, Box<dyn ToInstructionData>> = HashMap::new();

    instructions.insert(
        "noop".to_string(),
        Box::new(SingleInstruction::new(0)));
    instructions.insert(
        "inton".to_string(),
        Box::new(SingleInstruction::new(1)));
    instructions.insert(
        "intoff".to_string(),
        Box::new(SingleInstruction::new(2)));
    instructions.insert(
        "reset".to_string(),
        Box::new(SingleInstruction::new(3)));
    instructions.insert(
        "pop".to_string(),
        Box::new(SingleInstruction::new(4)));
    instructions.insert(
        "ret".to_string(),
        Box::new(SingleInstruction::new(5)));
    instructions.insert(
        "jmp".to_string(),
        Box::new(RegisterInstruction::new(1, 1)));
    instructions.insert(
        "jmpr".to_string(),
        Box::new(RegisterInstruction::new(2, 1)));
    instructions.insert(
        "push".to_string(),
        Box::new(RegisterInstruction::new(3, 1)));
    instructions.insert(
        "popr".to_string(),
        Box::new(RegisterInstruction::new(4, 1)));
    instructions.insert(
        "call".to_string(),
        Box::new(RegisterInstruction::new(5, 1)));
    instructions.insert(
        "int".to_string(),
        Box::new(RegisterInstruction::new(6, 1)));
    instructions.insert(
        "jmpri".to_string(),
        Box::new(ImmediateSingleInstruction::new(1)));
    instructions.insert(
        "ld".to_string(),
        Box::new(RegisterInstruction::new(2, 2)));
    instructions.insert(
        "sav".to_string(),
        Box::new(RegisterInstruction::new(3, 2)));
    instructions.insert(
        "ldr".to_string(),
        Box::new(RegisterInstruction::new(4, 2)));
    instructions.insert(
        "savr".to_string(),
        Box::new(RegisterInstruction::new(5, 2)));
    instructions.insert(
        "jz".to_string(),
        Box::new(RegisterInstruction::new(6, 2)));
    instructions.insert(
        "jzr".to_string(),
        Box::new(RegisterInstruction::new(7, 2)));
    instructions.insert(
        "jgz".to_string(),
        Box::new(RegisterInstruction::new(8, 2)));
    instructions.insert(
        "jgzr".to_string(),
        Box::new(RegisterInstruction::new(9, 2)));
    instructions.insert(
        "ldi".to_string(),
        Box::new(ImmediateRegisterInstruction::new(1)));
    instructions.insert(
        "ldui".to_string(),
        Box::new(ImmediateRegisterInstruction::new(2)));
    instructions.insert(
        "ldir".to_string(),
        Box::new(ImmediateRegisterInstruction::new(3)));
    instructions.insert(
        "add".to_string(),
        Box::new(RegisterInstruction::new(4, 3)));
    instructions.insert(
        "sub".to_string(),
        Box::new(RegisterInstruction::new(5, 3)));
    instructions.insert(
        "mul".to_string(),
        Box::new(RegisterInstruction::new(6, 3)));
    instructions.insert(
        "div".to_string(),
        Box::new(RegisterInstruction::new(7, 3)));
    instructions.insert(
        "mod".to_string(),
        Box::new(RegisterInstruction::new(8, 3)));
    instructions.insert(
        "band".to_string(),
        Box::new(RegisterInstruction::new(9, 3)));
    instructions.insert(
        "bor".to_string(),
        Box::new(RegisterInstruction::new(10, 3)));
    instructions.insert(
        "bxor".to_string(),
        Box::new(RegisterInstruction::new(11, 3)));
    instructions.insert(
        "bsftl".to_string(),
        Box::new(RegisterInstruction::new(12, 3)));
    instructions.insert(
        "bsftr".to_string(),
        Box::new(RegisterInstruction::new(13, 3)));

    return instructions;
}
