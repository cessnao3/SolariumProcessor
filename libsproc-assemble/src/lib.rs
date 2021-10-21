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
    Load(u16)
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


pub fn assemble(lines: Vec<String>) -> Result<Vec<u16>, String>
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
    fn basic_test() {
        let line_test: Vec<String> = vec![
            "ld 0, 0".to_string(),
            "popr 3".to_string()
        ];

        let assemble_result = assemble(line_test);

        assert!(assemble_result.is_ok());

        let assemble_ok = assemble_result.unwrap();

        assert!(assemble_ok.len() == 2);
        assert!(assemble_ok[0] == 0x200);
        assert!(assemble_ok[1] == 0x43);
    }
}
