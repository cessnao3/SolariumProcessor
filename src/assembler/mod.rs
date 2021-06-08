use self::inst_jump::InstructionJump;

use crate::cpu::location::Location;
use crate::memory::MemoryWord;
use std::collections::HashMap;

mod inst_jump;

macro_rules! match_err
{
    ($a: expr, $l: expr) =>
    {
        // Match the input value argument
        match $a
        {
            Ok(v) => v,
            Err(e) => return Err(format!("Line {0:} error: {1:}", $l, e))
        };
    }
}

pub fn assemble(lines: Vec<&str>) -> Result<Vec<MemoryWord>, String>
{
    // Define the offset address
    let mut offset_address: MemoryWord = 0;

    // Define a dictionary of addresses to provide the resulting assembled parameters
    let mut address_dict = HashMap::<MemoryWord, MemoryWord>::new();

    // TODO - Add keyword replacement for zero register? Other registers?

    // TODO - Add dictionary to map locations, labels

    // Define the available jump instructions
    let jmp_instructions = InstructionJump::get_instructions();

    let arithmetic_instructions: Vec<String> = vec!{
        "add",
        "sub",
        "mul",
        "div",
        "mod"
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
            .trim()
            .split(' ')
            .map(|v| v.trim().to_ascii_lowercase())
            .filter(|v| v.len() > 0)
            .collect();

        // Determine the number of arguments (will always be >= 0 due to check above for empty)
        let num_args = words.len() - 1;

        // Define the individual components for each argument
        let opcode: u8;
        let mut arg0 = 0u8;
        let mut arg1 = 0u8;
        let mut arg2 = 0u8;

        // Define the instruction type
        let inst_word = &words[0];

        // Define the opcode and parameters for the different
        if inst_word == "noop"
        {
            // Skip parameters and leave output at zero
            opcode = 0x00;
        }
        else if inst_word == "push"
        {
            // Ensure that there are two arguments
            if num_args != 1
            {
                return Err(format!("{0:} expected 1 argument, got {1:}", inst_word, num_args));
            }

            // Extract the resulting value to push
            let loc_src = match_err!(Location::from_arg(arg0), l);

            // Extract the resulting value
            opcode = 0x10;
            arg0 = match_err!(loc_src.to_arg(), l);
        }
        else if inst_word == "pop"
        {
            // Check the number of arguments
            if num_args > 0
            {
                return Err(format!("{0:} expected 0 arguments, got {1:}", inst_word, num_args));
            }
            else
            {
                opcode = 0x11;
            }
        }
        else if inst_word == "copy"
        {
            // Check that there are sufficient words
            if num_args != 2
            {
                return Err(format!("expected 2 arguments for a copy in line \"{0:}\"", l));
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

            // Save the opcode
            opcode = 0x30;

            // Add the location values to the arguments
            arg0 = match src_loc.to_arg()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            arg1 = match dst_loc.to_arg()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            }
        }
        else if arithmetic_instructions.contains(&inst_word)
        {
            // Ensure that we have the correct number of arguments
            if num_args != 3
            {
                return Err(format!(
                    "arithmetic instructions expected 3 arguments, got {:0}",
                    num_args))
            }

            // Check the opcode instruction
            opcode = if inst_word == "add"
            {
                0x40
            }
            else if inst_word == "sub"
            {
                0x41
            }
            else if inst_word == "mul"
            {
                0x42
            }
            else if inst_word == "div"
            {
                0x43
            }
            else if inst_word == "mod"
            {
                0x44
            }
            else
            {
                return Err(format!("no instruction for \"{0:}\" found", inst_word));
            };

            // Get the input/output locations
            let loc_a = match words[1].parse::<Location>()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            let loc_b = match words[2].parse::<Location>()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            let loc_c = match words[3].parse::<Location>()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            arg0 = match loc_a.to_arg()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            arg1 = match loc_b.to_arg()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
            };

            arg2 = match loc_c
            {
                Location::Value(_) => return Err(format!("Line {0:} error: destination may not be immediate", l)),
                loc => match loc.to_arg()
                {
                    Ok(v) => v,
                    Err(e) => return Err(format!("Line {0:} error: {1:}", l, e))
                }
            };
        }
        else if jmp_instructions.contains_key(inst_word)
        {
            // Obtain the jump instruction
            let inst = jmp_instructions.get(inst_word).unwrap();

            // Ensure that the number of words matches
            if inst.expected_args() != num_args
            {
                return Err(format!(
                    "instruction \"{0:}\" expected {1:} arguments; got {2:}",
                    l,
                    inst.expected_args(),
                    num_args));
            }

            // Define the current index
            let mut current_ind: usize = 1;

            // Set the register parameters
            let op_a: Option<Location>;
            if inst.num_operands > 0
            {
                op_a = Some(match_err!(words[current_ind].parse(), l));
                current_ind += 1;
            }
            else
            {
                op_a = None;
            }

            let op_b: Option<Location>;
            if inst.num_operands > 1
            {
                op_b = Some(match_err!(words[current_ind].parse(), l));
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

            // Save the opcode
            opcode = inst.opcode;
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
        if address_dict.contains_key(&offset_address)
        {
            return Err(format!("address {0:X} is already provided", offset_address));
        }
        else
        {
            // Add the instruction
            address_dict.insert(
                offset_address,
                instruction);

            // Increment the offset address
            offset_address += 1;
        }
    }

    // Construct the resulting addresses
    let mut assembled: Vec<MemoryWord> = Vec::new();
    for (address, val) in address_dict.iter()
    {
        // Define the address index
        let address_index = *address as usize;

        // Add zeros for any address
        while address_index >= assembled.len()
        {
            assembled.push(0);
        }

        // Save the address value
        assembled[address_index] = *val;
    }

    // Return the assembled results
    return Ok(assembled);
}
