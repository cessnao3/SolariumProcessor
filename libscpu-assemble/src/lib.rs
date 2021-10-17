mod instructions;

use instructions::get_instruction_map;

use std::{collections::HashMap, num::ParseIntError};

use regex::Regex;

const MAX_ADDRESSABLE_VALUE: usize = (2usize).pow(16);

enum LineValue
{
    Assembly(LineInformation),
    Load(u16)
}

struct LineInformation
{
    pub instruction: String,
    pub arguments: Vec<String>,
    pub line_number: usize
}

impl LineInformation
{
    pub fn new(instruction: String, arguments: Vec<String>, line_number: usize) -> LineInformation
    {
        return Self
        {
            instruction,
            arguments,
            line_number
        };
    }
}

fn read_load_value(input: &str) -> Result<u16, ParseIntError>
{
    let num_hex_regex = Regex::new(r"^0x(?P<digit>[0-9a-f]+)$").unwrap();
    let num_neg_regex = Regex::new(r"^\-[\d]+$").unwrap();
    //let num_pos_regex = Regex::new(r"^+?[\d]+$").unwrap();

    let immediate;

    if num_hex_regex.is_match(input)
    {
        immediate = u16::from_str_radix(
            &input[2..],
            16)?;
    }
    else if num_neg_regex.is_match(input)
    {
        immediate = input.parse::<i16>()? as u16;
    }
    else
    {
        immediate = input.parse::<u16>()?;
    }

    return Ok(immediate);
}


pub fn assemble(lines: Vec<String>) -> Result<Vec<u16>, String>
{
    let line_regex = Regex::new(r"^(?P<instruction>[\w]+)(?P<rest>\s+\-?[\w\d]+(,\s*\-?[\w\d]+)*)*$").unwrap();
    let command_regex = Regex::new(r"^\.(?P<command>[\w]+)\s+(?P<args>[\w\d\s]*)$").unwrap();
    let args_split_regex = Regex::new(r",\s*").unwrap();

    let mut data_values = HashMap::<usize, LineValue>::new();
    let mut data_offset = 0usize;

    let instruction_map = get_instruction_map();

    for (i, l_in) in lines.iter().enumerate()
    {
        // Cleanup the resulting string value
        let l: &str = match l_in.find("//")
        {
            Some(len) => &l_in[..len],
            None => l_in
        }.trim();

        // Check for line validity
        if l.is_empty()
        {
            continue;
        }
        else if command_regex.is_match(l)
        {
            // Extract the command captures
            let caps = match command_regex.captures(l)
            {
                Some(v) => v,
                None => return Err(format!("line {0:} no command captures found for \"{1:}\"", i, l))
            };

            // Extract the resulting instruction values
            let cmd = caps.name("command").unwrap().as_str().to_ascii_lowercase();
            let args = match caps.name("args")
            {
                Some(v) => args_split_regex
                    .split(&v.as_str().trim().to_ascii_lowercase())
                    .map(|v| v.to_string())
                    .collect(),
                None => Vec::new()
            };

            // Check argument value
            if cmd == "oper"
            {
                if args.len() != 1
                {
                    return Err(format!("line {0:} command {1:} only takes 1 argument", i, cmd));
                }

                let new_offset = match args[0].parse::<usize>()
                {
                    Ok(v) => v,
                    Err(_) => return Err(format!("line {0:} command {1:} unable to parse {2:} as address", i, cmd, args[0]))
                };

                if new_offset < data_offset
                {
                    return Err(format!("line {0:} command {1:} new offset {2:} must be greater or equal to current offset {3:}", i, cmd, new_offset, data_offset));
                }
                else
                {
                    data_offset = new_offset;
                }
            }
            else if cmd == "load"
            {
                if args.len() != 1
                {
                    return Err(format!("line {0:} command {1:} only takes 1 argument", i, cmd));
                }

                let value_to_load = match read_load_value(&args[0])
                {
                    Ok(v) => v,
                    Err(e) => return Err(format!(
                        "line {0:} unable to parse \"{1:}\" as memory value: {2:}",
                        i,
                        args[0],
                        e.to_string()))
                };

                data_values.insert(
                    data_offset,
                    LineValue::Load(value_to_load));
                data_offset += 1;
            }
            else
            {
                return Err(format!("line {0:} invalid command \"{1:}\" found", i, cmd));
            }
        }
        else if line_regex.is_match(l)
        {
            // Extract the captures
            let caps = match line_regex.captures(l)
            {
                Some(v) => v,
                None => return Err(format!("line {0:} no captures found for \"{1:}\"", i, l))
            };

            // Extract the instruction string and the argument string
            let inst = caps.name("instruction").unwrap().as_str().to_ascii_lowercase();
            let args = match caps.name("rest")
            {
                Some(v) => args_split_regex
                    .split(&v.as_str().trim().to_ascii_lowercase())
                    .map(|v| v.to_string())
                    .collect(),
                None => Vec::new()
            };

            // Add the data values
            if data_values.contains_key(&data_offset)
            {
                return Err(format!("line {0:} offset {1:} already filled", i, data_offset))
            }
            else if data_offset < MAX_ADDRESSABLE_VALUE
            {
                data_values.insert(
                    data_offset,
                    LineValue::Assembly(
                        LineInformation::new(
                            inst,
                            args,
                            i)));
                data_offset += 1;
            }
            else
            {
                return Err(format!("line {0:} \"{1:}\" does not match expected syntax", i, l));
            }
        }
        else
        {
            return Err(format!("line {0:} \"{1:}\" does not match expected syntax", i, l));
        }
    }

    let max_index = match data_values.keys().max()
    {
        Some(v) => v,
        None => return Ok(Vec::new())
    };

    let default_val = 0u16;

    let data_vec_results: Vec<Result<u16, String>> = (0..(max_index + 1))
        .map(|v|
            {
                let val: u16 = match data_values.get(&v)
                {
                    Some(line_value) =>
                    {
                        match line_value
                        {
                            LineValue::Assembly(assembly) =>
                            {
                                // Extract the expected output value
                                let inst_val = match instruction_map.get(&assembly.instruction)
                                {
                                    Some(v) => v,
                                    None => return Err(format!("line {0:} no instruction {1:} found", assembly.line_number, assembly.instruction))
                                };

                                // Attempt to create the resulting data
                                let inst_data = match inst_val.to_instruction_data(&assembly.arguments)
                                {
                                    Ok(v) => v,
                                    Err(e) => return Err(format!("line {0:} {1:} -> {2:}", assembly.line_number, assembly.instruction, e))
                                };

                                inst_data.combine()
                            },
                            LineValue::Load(data) =>
                            {
                                *data
                            }
                        }
                    },
                    None => default_val
                };

                return Ok(val);
            })
        .collect();

    let mut data_vec = Vec::new();
    for v in data_vec_results
    {
        match v
        {
            Ok(val) => data_vec.push(val),
            Err(e) => return Err(e)
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
