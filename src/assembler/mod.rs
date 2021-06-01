mod register_def;

use self::register_def::RegisterDefinition;

use crate::memory::MemoryWord;
use crate::cpu::processor::
use crate::cpu::registers::Register;

pub fn assemble(lines: Vec<String>) -> Result<Vec<MemoryWord>, String>
{
    // Define the assembled result
    let mut assembled: Vec<MemoryWord> = Vec::new();

    // Define the available jump instructions
    let jmp_instructions: Vec<String> = vec!{
        "jmp",
        "jne",
        "jnz",
        "jeq",
        "jez",
        "jn",
        "jp",
        "jge",
        "jg",
        "jl",
        "jle"
    }.iter()
        .map(|v| v.to_string())
        .collect();

    // Iterate over each line
    for l in lines.iter()
    {
        // Check for an empty parameter
        if l.trim().len() == 0
        {
            continue;
        }

        // Extract words
        let words: Vec<String> = l
            .split(' ')
            .map(|v| v.trim().to_ascii_lowercase())
            .filter(|v| v.len() > 0)
            .collect();

        // Define the individual components for each argument
        let mut opcode = 0u8;
        let mut arg0 = 0u8;
        let mut arg1 = 0u8;
        let mut arg2 = 0u8;

        // Define the instruction type
        let inst_word = &words[0];

        // Define the opcode and parameters for the different
        if inst_word == "noop"
        {
            // Skip parameters and leave at 0
        }
        else if inst_word == "copy"
        {
            // Check that there are sufficient words
            if words.len() != 3
            {
                return Err(format!("expected three arguments for a copy in line \"{0:}\"", l));
            }

            // Extract the source and destination values
            let src_reg;
            match words[1].parse::<RegisterDefinition>()
            {
                Ok(v) => src_reg = v,
                Err(e) => return Err(e)
            }

            let dst_reg;
            match words[2].parse::<RegisterDefinition>()
            {
                Ok(v) => dst_reg = v,
                Err(e) => return Err(e)
            }

            // Add the argument values
            if src_reg.is_immediate
            {
                arg0 |= 1;
            }

            if dst_reg.is_immediate
            {
                arg0 |= 2;
            }

            // Add the parameters to the given argument
            arg1 |= src_reg.index;
            arg1 |= dst_reg.index << 4;
        }
        else if jmp_instructions.contains(&inst_word)
        {
            // Determine the number of operands to search for
            let num_operands: usize;

            // Determine whether the second operand is zero
            let second_op_is_zero: bool;

            // Define the opcode based on the input word
            if inst_word == "jmp"
            {
                opcode = 0x20;
                num_operands = 0;
                second_op_is_zero = false;
            }
            else if inst_word == "jne"
            {
                opcode = 0x21;
                num_operands = 2;
                second_op_is_zero = false;
            }
            else if inst_word == "jnz"
            {
                opcode = 0x21;
                num_operands = 1;
                second_op_is_zero = true;
            }
            else if inst_word == "jeq"
            {
                opcode = 0x22;
                num_operands = 2;
                second_op_is_zero = false;
            }
            else if inst_word == "jez"
            {
                opcode = 0x22;
                num_operands = 1;
                second_op_is_zero = true;
            }
            else if inst_word == "jn"
            {
                opcode = 0x23;
                num_operands = 1;
                second_op_is_zero = false;
            }
            else if inst_word == "jp"
            {
                opcode = 0x24;
                num_operands = 1;
                second_op_is_zero = false;
            }
            else
            {
                panic!();
            }

            // Define the current index
            let mut current_ind: usize = 1;

            // Set the register parameters
            let op_a: Option<RegisterDefinition>;
            if num_operands > 0
            {
                match words[current_ind].parse()
                {
                    Ok(v)  => op_a = Some(v),
                    Err(e) => return Err(e)
                };
                current_ind += 1;
            }
            else
            {
                op_a = None;
            }

            let op_b: Option<RegisterDefinition>;
            if num_operands > 1
            {
                match words[current_ind].parse()
                {
                    Ok(v) => op_b = Some(v),
                    Err(e) => return Err(e)
                };
                current_ind += 1;
            }
            else if second_op_is_zero
            {
                op_b = Some(RegisterDefinition::new(
                    Register::Zero.to_index() as u8,
                    true).unwrap());
            }
            else
            {
                op_b = None;
            }
        }
        else
        {
            return Err(format!("unknown instruction line \"{0:}\" provided", l));
        }

        // Combine into an overall word
        let instruction: MemoryWord =
            opcode as MemoryWord |
                (arg0 as MemoryWord) << 8 |
                (arg1 as MemoryWord) << 16 |
                (arg2 as MemoryWord) << 24;

        // Add to the instruction to the assembled parameters
        assembled.push(instruction);
    }

    // Return the assembled results
    return Ok(assembled);
}
