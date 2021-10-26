mod instructions;
mod assembly;

use instructions::get_instruction_map;

use std::collections::HashMap;

use assembly::asm_regex::{VALID_LINE_REGEX, ARGUMENT_SPLIT_REGEX};

use assembly::argument::Argument;


const MAX_ADDRESSABLE_VALUE: usize = (2usize).pow(16);

enum LineValue
{
    Assembly(LineInformation),
    Load(u16),
    LoadLabelLoc(usize, String)
}

struct LineInformation
{
    pub instruction: String,
    pub arguments: Vec<Argument>,
    pub line_number: usize,
    pub data_index: usize
}

impl LineInformation
{
    pub fn new(
        instruction: String,
        arguments: Vec<Argument>,
        line_number: usize,
        data_index: usize) -> LineInformation
    {
        return Self
        {
            instruction,
            arguments,
            line_number,
            data_index
        };
    }

    pub fn update_arguments_for_labels(&self, label_map: &HashMap<String, usize>) -> Result<Vec<Argument>, String>
    {
        return self.arguments
            .iter()
            .map(|arg|
            {
                let new_arg = match arg
                {
                    Argument::Label(s) =>
                    {
                        match label_map.get(s)
                        {
                            Some(data_index) =>
                            {
                                let delta_index = *data_index as i32 - self.data_index as i32;
                                Argument::SignedNumber(delta_index)
                            },
                            None => return Err(format!("unable to find value for label {0:}", s))
                        }
                    },
                    a => a.clone()
                };

                return Ok(new_arg);
            })
            .collect();
    }
}


pub fn assemble(lines: Vec<&str>) -> Result<Vec<u16>, String>
{
    let mut data_map = HashMap::<usize, LineValue>::new();
    let mut current_data_index = 0usize;

    let mut label_map = HashMap::<String, usize>::new();

    let instruction_map = get_instruction_map();

    for (i, l_in) in lines.iter().enumerate()
    {
        // Cleanup the resulting string value
        let l: &str = match l_in.find("//")
        {
            Some(len) => &l_in[..len],
            None => l_in
        }.trim();

        // Skip if empty
        if l.is_empty()
        {
            continue;
        }

        // Check that the line matches the expected values
        if !VALID_LINE_REGEX.is_match(l)
        {
            return Err(format!("line {0:} does not match the expected line format \"{1:}\"", i, l));
        }

        // Extract capture groups
        let capture_groups = match VALID_LINE_REGEX.captures(l)
        {
            Some(v) => v,
            None => return Err(format!("line {0:} no command captures found for \"{1:}\"", i, l))
        };

        // Extract parameters
        let command = capture_groups.name("command").unwrap().as_str().to_ascii_lowercase();
        let args = match capture_groups.name("args")
        {
            Some(v) => match ARGUMENT_SPLIT_REGEX
                .split(&v.as_str().to_ascii_lowercase())
                .map(|v| v.parse::<Argument>())
                .collect()
            {
                Ok(v) => v,
                Err(e) => return Err(format!("line {0:} {1:}", i, e.to_string()))
            }
            None => Vec::new()
        };

        // Check the first character of the command to determine how to handle
        let first_char = command.chars().next().unwrap();

        if first_char == '.'
        {
            let command_type = &command[1..];

            if command_type == "oper"
            {
                if args.len() != 1
                {
                    return Err(format!(
                        "line {0:} command {1:} only takes 1 argument",
                        i,
                        command_type));
                }

                let new_offset = match &args[0]
                {
                    Argument::UnsignedNumber(v) => *v as usize,
                    arg => return Err(format!(
                        "line {0:} command {1:} unable to parse {2:} as address",
                        i,
                        command_type,
                        arg.to_string()))
                };

                if new_offset < current_data_index
                {
                    return Err(format!(
                        "line {0:} command {1:} new offset {2:} must be greater or equal to current offset {3:}",
                        i,
                        command_type,
                        new_offset,
                        current_data_index));
                }
                else
                {
                    current_data_index = new_offset;
                }
            }
            else if command_type == "load"
            {
                if args.len() != 1
                {
                    return Err(format!("line {0:} command {1:} only takes 1 argument", i, command_type));
                }

                let value_to_load = match args[0].to_u16()
                {
                    Ok(v) => v,
                    Err(e) => return Err(format!("line {0:} {1:}", i, e))
                };

                data_map.insert(
                    current_data_index,
                    LineValue::Load(value_to_load));
                current_data_index += 1;
            }
            else if command_type == "loadloc"
            {
                if args.len() != 1
                {
                    return Err(format!("line {0:} command {1:} only takes 1 argument", i, command_type));
                }

                let arg_label = match &args[0]
                {
                    Argument::Label(label) => label.clone(),
                    _ => return Err(format!("line {0:} command {1:} may only take a label input", i, command))
                };

                data_map.insert(
                    current_data_index,
                    LineValue::LoadLabelLoc(i, arg_label));
                current_data_index += 1;
            }
            else
            {
                return Err(format!("line {0:} invalid command \"{1:}\" found", i, command_type));
            }
        }
        else if first_char == ':'
        {
            if args.len() > 0
            {
                return Err(format!("line {0:} label types cannot have any arguments", i));
            }

            let label = &command[1..];

            if label_map.contains_key(label)
            {
                return Err(format!("line {0:} label \"{1:}\" already exists", i, label));
            }
            else
            {
                label_map.insert(
                    label.to_string(),
                    current_data_index);
            }
        }
        else
        {
            // Add the data values
            if data_map.contains_key(&current_data_index)
            {
                return Err(format!("line {0:} offset {1:} already filled", i, current_data_index))
            }
            else if current_data_index < MAX_ADDRESSABLE_VALUE
            {
                data_map.insert(
                    current_data_index,
                    LineValue::Assembly(
                        LineInformation::new(
                            command,
                            args,
                            i,
                            current_data_index)));
                current_data_index += 1;
            }
            else
            {
                return Err(format!("line {0:} \"{1:}\" does not match expected syntax", i, l));
            }
        }
    }

    let max_index = match data_map.keys().max()
    {
        Some(v) => v,
        None => return Ok(Vec::new())
    };

    let mut data_vec = Vec::new();
    data_vec.resize(max_index + 1, 0);

    for (data_index, line_value) in data_map
    {
        data_vec[data_index] = match line_value
        {
            LineValue::Assembly(assembly) =>
            {
                // Extract the expected output value
                let inst_val = match instruction_map.get(&assembly.instruction)
                {
                    Some(v) => v,
                    None => return Err(format!("line {0:} no instruction {1:} found", assembly.line_number, assembly.instruction))
                };

                // Define the new arguments to include labels values
                let new_args = match assembly.update_arguments_for_labels(&label_map)
                {
                    Ok(v) => v,
                    Err(e) => return Err(format!("line {0:} {1:}", assembly.line_number, e))
                };

                // Attempt to create the resulting data
                let inst_data = match inst_val.to_instruction_data(&new_args)
                {
                    Ok(v) => v,
                    Err(e) => return Err(format!("line {0:} {1:} -> {2:}", assembly.line_number, assembly.instruction, e))
                };

                inst_data.combine()
            },
            LineValue::LoadLabelLoc(line_number, label) =>
            {
                match label_map.get(&label)
                {
                    Some(v) => *v as u16,
                    None => return Err(format!("line {0:} no label {1:} provided", line_number, label))
                }
            },
            LineValue::Load(data) =>
            {
                data
            }
        };
    }

    return Ok(data_vec);
}

#[cfg(test)]
mod tests {
    use super::assemble;

    #[test]
    fn basic_test()
    {
        let line_test = vec![
            "ld 0, 0",
            "popr 3"
        ];

        let assemble_result = assemble(line_test);

        assert!(assemble_result.is_ok());

        let assemble_ok = assemble_result.unwrap();

        assert!(assemble_ok.len() == 2);
        assert!(assemble_ok[0] == 0x200);
        assert!(assemble_ok[1] == 0x43);
    }

    #[test]
    fn test_noop()
    {
        let line_test = vec![
            "noop",
            "noop"
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 2);
        assert!(binary[0] == 0);
        assert!(binary[1] == 0);
    }

    #[test]
    fn test_inton()
    {
        let line_test = vec![
            "inton",
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 1);
        assert!(binary[0] == 1);
    }

    #[test]
    fn test_intoff()
    {
        let line_test = vec![
            "intoff",
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 1);
        assert!(binary[0] == 2);
    }

    #[test]
    fn test_reset()
    {
        let line_test = vec![
            "reset",
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 1);
        assert!(binary[0] == 3);
    }

    #[test]
    fn test_pop()
    {
        let line_test = vec![
            "pop",
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 1);
        assert!(binary[0] == 4);
    }

    #[test]
    fn test_ret()
    {
        let line_test = vec![
            "ret",
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 1);
        assert!(binary[0] == 5);
    }

    #[test]
    fn test_all_no_args()
    {
        let line_test = vec![
            "noop",
            "inton",
            "intoff",
            "reset",
            "pop",
            "ret"
        ];

        let binary_result = assemble(line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 6);
        assert!(binary[0] == 0);
        assert!(binary[1] == 1);
        assert!(binary[2] == 2);
        assert!(binary[3] == 3);
        assert!(binary[4] == 4);
        assert!(binary[5] == 5);
    }

    #[test]
    fn test_single_arg_register_val()
    {
        // Define the arguments to test
        let args_to_test = vec![
            ("jmp", 1),
            ("jmpr", 2),
            ("push", 3),
            ("popr", 4),
            ("call", 5),
            ("int", 6)
        ];

        for (arg, opcode) in args_to_test
        {
            assert!(opcode & 0xF == opcode);

            for reg in 0..16
            {
                assert!(reg & 0xF == reg);

                let assembly_text = vec![
                    format!("{0:} {1:}", arg, reg),
                    format!("{0:} {1:#X}", arg, reg),
                    format!("{0:} {1:#x}", arg, reg)
                ];

                let expected_result = ((opcode << 4) | reg) as u16;

                let binary_result = assemble(assembly_text.iter().map(|s| s.as_ref()).collect());

                assert!(binary_result.is_ok());

                let binary = binary_result.unwrap();

                assert!(binary.len() == 3);
                for bin_val in binary
                {
                    assert!(bin_val == expected_result);
                }
            }
        }

        assert!(true);
    }

    #[test]
    fn test_jmpri()
    {
        let opcode = 1u16;

        for i in 0..=u8::MAX
        {
            let expected_result = (opcode << 8) | i as u16;

            let code = vec![
                format!("jmpri {0:}", i),
                format!("jmpri {0:#X}", i),
                format!("jmpri {0:#x}", i),
                format!("jmpri {0:}", i as i8)
            ];

            let binary_result = assemble(code.iter().map(|v| v.as_ref()).collect());

            assert!(binary_result.is_ok());

            let binary = binary_result.unwrap();

            assert!(binary.len() == 4);
            for bin_val in binary
            {
                assert!(bin_val == expected_result);
            }
        }
    }
}
