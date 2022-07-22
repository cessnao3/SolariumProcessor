use std::collections::HashMap;

mod immediate;
mod register;
mod single;

use immediate::{ImmediateSingleInstruction, ImmediateRegisterInstruction};
use register::RegisterInstruction;
use single::SingleInstruction;

use crate::assembly::argument::Argument;

use sproc::common::InstructionData;

pub trait ToInstructionData
{
    fn to_instruction_data(&self, args: &Vec<Argument>) -> Result<InstructionData, String>;
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
        "ari".to_string(),
        Box::new(SingleInstruction::new(3)));
    instructions.insert(
        "aru".to_string(),
        Box::new(SingleInstruction::new(4)));
    instructions.insert(
        "reset".to_string(),
        Box::new(SingleInstruction::new(5)));
    instructions.insert(
        "pop".to_string(),
        Box::new(SingleInstruction::new(6)));
    instructions.insert(
        "ret".to_string(),
        Box::new(SingleInstruction::new(7)));
    instructions.insert(
        "retint".to_string(),
        Box::new(SingleInstruction::new(8)));
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
        "intr".to_string(),
        Box::new(RegisterInstruction::new(7, 1)));
    instructions.insert(
        "tz".to_string(),
        Box::new(RegisterInstruction::new(8, 1)));
    instructions.insert(
        "tnz".to_string(),
        Box::new(RegisterInstruction::new(9, 1)));
    instructions.insert(
        "bool".to_string(),
        Box::new(RegisterInstruction::new(10, 1)));
    instructions.insert(
        "not".to_string(),
        Box::new(RegisterInstruction::new(11, 1)));
    instructions.insert(
        "ldn".to_string(),
        Box::new(RegisterInstruction::new(12, 1)));
    instructions.insert(
        "neg".to_string(),
        Box::new(RegisterInstruction::new(13, 1)));
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
        "copy".to_string(),
        Box::new(RegisterInstruction::new(6, 2)));
    instructions.insert(
        "tg".to_string(),
        Box::new(RegisterInstruction::new(7, 2)));
    instructions.insert(
        "tge".to_string(),
        Box::new(RegisterInstruction::new(8, 2)));
    instructions.insert(
        "tl".to_string(),
        Box::new(RegisterInstruction::new(9, 2)));
    instructions.insert(
        "tle".to_string(),
        Box::new(RegisterInstruction::new(10, 2)));
    instructions.insert(
        "teq".to_string(),
        Box::new(RegisterInstruction::new(11, 2)));
    instructions.insert(
        "bnot".to_string(),
        Box::new(RegisterInstruction::new(12, 2)));
    instructions.insert(
        "arg".to_string(),
        Box::new(RegisterInstruction::new(13, 2)));
    instructions.insert(
        "ldi".to_string(),
        Box::new(ImmediateRegisterInstruction::new(1)));
    instructions.insert(
        "ldui".to_string(),
        Box::new(ImmediateRegisterInstruction::new(2)));
    instructions.insert(
        "ldri".to_string(),
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
        "rem".to_string(),
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
        "bshft".to_string(),
        Box::new(RegisterInstruction::new(12, 3)));

    return instructions;
}
