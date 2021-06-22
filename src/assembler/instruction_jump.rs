use std::collections::HashMap;

use crate::assembler::asm_instruction::{AsmInstruction, AsmInstructionLine};
use crate::cpu::location::Location;

/// Provides basic information for the different types of jump instructions
#[derive(Clone)]
pub struct InstructionJump
{
    pub name: String,
    pub opcode: u8,
    pub num_operands: usize
}

impl InstructionJump
{
    pub fn get_instructions() -> HashMap<String, InstructionJump>
    {
        let instructions = vec![
            InstructionJump
            {
                name: "jmp".to_string(),
                opcode: 0x20,
                num_operands: 0
            },
            InstructionJump
            {
                name: "jne".to_string(),
                opcode: 0x21,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jeq".to_string(),
                opcode: 0x22,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jn".to_string(),
                opcode: 0x23,
                num_operands: 1
            },
            InstructionJump
            {
                name: "jp".to_string(),
                opcode: 0x24,
                num_operands: 1
            },
            InstructionJump
            {
                name: "jge".to_string(),
                opcode: 0x25,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jg".to_string(),
                opcode: 0x26,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jle".to_string(),
                opcode: 0x27,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jl".to_string(),
                opcode: 0x28,
                num_operands: 2
            }
        ];

        let mut inst_map: HashMap<String, InstructionJump> = HashMap::new();

        for inst in instructions.iter()
        {
            if inst_map.contains_key(&inst.name)
            {
                panic!("double instruction found for {0:}", inst.name);
            }

            inst_map.insert(
                inst.name.to_string(),
                inst.clone());
        }

        return inst_map;
    }

    pub fn expected_args(&self) -> usize
    {
        // Provide the number of operands, plus one for the destination location
        return self.num_operands + 1;
    }
}

pub fn check_instruction_jump(asm: &AsmInstructionLine) -> Result<Option<AsmInstruction>, String>
{
    // Define the list of jump values
    let jmp_instructions = InstructionJump::get_instructions();

    // Check that the instruction word is contained
    if jmp_instructions.contains_key(asm.inst_word())
    {
        // Define the instruction return value
        let mut inst_val = AsmInstruction::new();

        // Obtain the jump instruction
        let inst = jmp_instructions.get(asm.inst_word()).unwrap();

        // Extract argument values
        let args = asm.arg_words();
        let num_args = args.len();

        // Ensure that the number of words matches
        if inst.expected_args() != num_args
        {
            return Err(format!(
                "instruction \"{0:}\" expected {1:} arguments; got {2:}",
                asm.get_line(),
                inst.expected_args(),
                num_args));
        }

        // Define the current index
        let mut current_ind: usize = 0;

        // Set the register parameters
        let op_a: Option<Location>;
        if inst.num_operands > 0
        {
            op_a = Some(match_err!(args[current_ind].parse(), asm.get_line()));
            current_ind += 1;
        }
        else
        {
            op_a = None;
        }

        let op_b: Option<Location>;
        if inst.num_operands > 1
        {
            op_b = Some(match_err!(args[current_ind].parse(), asm.get_line()));
            current_ind += 1;
        }
        else
        {
            op_b = None;
        }

        // Set the operand values
        match op_a
        {
            Some(v) => inst_val.arg0 = match_err!(v.to_arg(), asm.get_line()),
            None => ()
        };

        match op_b
        {
            Some(v) => inst_val.arg1 = match_err!(v.to_arg(), asm.get_line()),
            None => ()
        }

        // Determine the last word/location to jump to
        match args[current_ind].parse::<Location>()
        {
            Ok(v) => inst_val.arg2 = match_err!(v.to_arg(), asm.get_line()),
            Err(e) => return Err(format!("Line {0:} error: {1:}", asm.get_line(), e))
        }

        // Save the opcode
        inst_val.opcode = inst.opcode;

        // Return the result
        return Ok(Some(inst_val));
    }
    else
    {
        return Ok(None);
    }
}
