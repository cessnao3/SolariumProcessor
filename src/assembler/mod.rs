mod inst_jump;
mod location;

use self::location::Location;

use crate::memory::MemoryWord;
use crate::assembler::inst_jump::InstructionJump;

pub fn assemble(lines: Vec<&str>) -> Result<Vec<MemoryWord>, String>
{
    // Define the assembled result
    let mut assembled: Vec<MemoryWord> = Vec::new();

    // TODO - Add keyword replacement for zero register? Other registers?

    // Define the available jump instructions
    let jmp_instructions: Vec<String> = vec!{
        "jmp",
        "jne",
        "jeq",
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
            // Skip parameters and leave output at zero
        }
        else if inst_word == "copy"
        {
            // Check that there are sufficient words
            if words.len() != 3
            {
                return Err(format!("expected three arguments for a copy in line \"{0:}\"", l));
            }

            // Extract the source and destination values
            let src_loc = match words[1].parse::<Location>()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            let dst_loc = match words[2].parse::<Location>()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            // Add the location values to the arguments
            arg1 = match src_loc.to_arg()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            arg2 = match dst_loc.to_arg()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            }
        }
        else if jmp_instructions.contains(&inst_word)
        {
            // Obtain the jump instruction
            let inst;
            match InstructionJump::get_by_name(&inst_word)
            {
                Ok(v) => inst = v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
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
            let op_a: Option<Location>;
            if inst.num_operands > 0
            {
                match words[current_ind].parse()
                {
                    Ok(v)  => op_a = Some(v),
                    Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
                };
                current_ind += 1;
            }
            else
            {
                op_a = None;
            }

            let op_b: Option<Location>;
            if inst.num_operands > 1
            {
                match words[current_ind].parse()
                {
                    Ok(v) => op_b = Some(v),
                    Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
                };
                current_ind += 1;
            }
            else
            {
                op_b = None;
            }

            // Set the operand values
            match op_a
            {
                Some(v) =>
                    {
                        arg0 = match v.to_arg()
                        {
                            Ok(a) => a,
                            Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
                        }
                    },
                None => ()
            };

            match op_b
            {
                Some(v) =>
                    {
                        match v.to_arg()
                        {
                            Ok(a) => arg1 = a,
                            Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
                        }
                    }
                None => ()
            }

            // Determine the last word/location to jump to
            match words[current_ind].parse::<Location>()
            {
                Ok(v) =>
                    {
                        match v.to_arg()
                        {
                            Ok(a) => arg2 = a,
                            Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
                        }
                    },
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
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
