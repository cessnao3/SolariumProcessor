mod register_def;
mod inst_jump;

use self::register_def::RegisterDefinition;

use crate::memory::MemoryWord;
use crate::cpu::registers::Register;
use crate::assembler::inst_jump::InstructionJump;

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
        let opcode = 0u8;
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
            // Obtain the jump instruction
            let inst;
            match InstructionJump::get_by_name(&inst_word)
            {
                Ok(v) => inst = v,
                Err(e) => return Err(e)
            };

            // Ensure that the number of words matches
            if inst.expected_words() != words.len()
            {
                return Err(format!(
                    "instruction \"{0:}\" expected {1:} operands; got {2:}",
                    l,
                    inst.expected_words(),
                    words.len()));
            }

            // Define the current index
            let mut current_ind: usize = 1;

            // Set the register parameters
            let op_a: Option<RegisterDefinition>;
            if inst.num_operands > 0
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
            if inst.num_operands > 1
            {
                match words[current_ind].parse()
                {
                    Ok(v) => op_b = Some(v),
                    Err(e) => return Err(e)
                };
                current_ind += 1;
            }
            else if inst.second_is_zero
            {
                op_b = Some(RegisterDefinition::new(
                    Register::Zero.to_index() as u8,
                    true).unwrap());
            }
            else
            {
                op_b = None;
            }

            // Set the operand values
            match op_a
            {
                Some(v) => arg1 |= v.index,
                None => ()
            };

            match op_b
            {
                Some(v) => arg1 |= v.index << 4,
                None => ()
            }

            // Determine the last word
            let jump_word = &words[current_ind];

            // Check if we can parse the jump word directly
            match jump_word.parse::<i8>()
            {
                Ok(v) =>
                    {
                        arg0 |= 1;
                        arg2 = v as u8;
                    },
                Err(_) => match jump_word.parse::<RegisterDefinition>()
                {
                    Ok(v) => arg2 = v.index,
                    Err(_) => return Err(format!("unknown jump destination for {0:}", l))
                }
            };
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
