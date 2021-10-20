mod instructions;

use instructions::get_instruction_map;

use std::{collections::HashMap, num::ParseIntError};

use regex::Regex;

use lazy_static::lazy_static;


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
    pub line_number: usize,
    pub data_index: usize
}

impl LineInformation
{
    pub fn new(instruction: String, arguments: Vec<String>, line_number: usize, data_index: usize) -> LineInformation
    {
        return Self
        {
            instruction,
            arguments,
            line_number,
            data_index
        };
    }

    pub fn arguments_for_labels(&self, label_map: &HashMap<String, usize>) -> Vec<String>
    {
        return self.arguments
            .iter()
            .map(|v|
            {
                return match label_map.get(v)
                {
                    Some(data_index) =>
                    {
                        let delta_index = *data_index as i32 - self.data_index as i32;
                        delta_index.to_string()
                    },
                    None => v.to_string()
                };
            })
            .collect();
    }
}

fn read_load_value(input: &str) -> Result<u16, ParseIntError>
{
    lazy_static!
    {
        static ref NUM_HEX_REGEX: Regex = Regex::new(r"^0x(?P<digit>[0-9a-f]+)$").unwrap();
        static ref NUM_NEG_REGEX: Regex = Regex::new(r"^\-[\d]+$").unwrap();
        //static ref NUM_POS_REGEX: Regex = Regex::new(r"^+?[\d]+$").unwrap();
    }

    let immediate;

    if NUM_HEX_REGEX.is_match(input)
    {
        immediate = u16::from_str_radix(
            &input[2..],
            16)?;
    }
    else if NUM_NEG_REGEX.is_match(input)
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
    lazy_static!
    {
        static ref REGISTER_VALID_STR: String = r"(\$[\w]+)".to_string();
        static ref LABEL_VALID_STR: String = r"([a-z][a-z0-9_]+)".to_string();
        static ref NUMBER_VALID_STR: String = r"([\-|+]?[\d]+)".to_string();
        static ref HEX_VALID_STR: String = r"(0x[a-f0-9]{1,4})".to_string();
        static ref ARGUMENT_REGEX_STR: String = format!(
            "({0:}|{1:}|{2:}|{3:})",
            REGISTER_VALID_STR.to_string(),
            LABEL_VALID_STR.to_string(),
            NUMBER_VALID_STR.to_string(),
            HEX_VALID_STR.to_string());
        static ref ARGUMENT_SPLIT_STR: String = r"(,\s*)".to_string();

        static ref ARG_LIST_STRING: String = format!(
            "(({0:}({1:}{0:})*)?)",
            ARGUMENT_REGEX_STR.to_string(),
            ARGUMENT_SPLIT_STR.to_string());

        static ref LINE_REGEX: Regex = Regex::new(&format!(
            "^(?P<instruction>[\\w]+)(\\s+(?P<args>{0:}))?$",
            ARG_LIST_STRING.to_string())).unwrap();
        static ref COMMAND_REGEX: Regex = Regex::new(r"^\.(?P<command>[\w]+)\s+(?P<args>[\w\d\s]*)$").unwrap();
        static ref LABEL_REGEX: Regex = Regex::new(&format!("^:(?P<tag>{0:})$", LABEL_VALID_STR.to_string())).unwrap();
        static ref ARGS_SPLIT_REGEX: Regex = Regex::new(&ARGUMENT_SPLIT_STR).unwrap();
    }

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

        // Check for line validity
        if l.is_empty()
        {
            continue;
        }
        else if COMMAND_REGEX.is_match(l)
        {
            // Extract the command captures
            let caps = match COMMAND_REGEX.captures(l)
            {
                Some(v) => v,
                None => return Err(format!("line {0:} no command captures found for \"{1:}\"", i, l))
            };

            // Extract the resulting instruction values
            let cmd = caps.name("command").unwrap().as_str().to_ascii_lowercase();
            let args = match caps.name("args")
            {
                Some(v) => ARGS_SPLIT_REGEX
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

                if new_offset < current_data_index
                {
                    return Err(format!("line {0:} command {1:} new offset {2:} must be greater or equal to current offset {3:}", i, cmd, new_offset, current_data_index));
                }
                else
                {
                    current_data_index = new_offset;
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

                data_map.insert(
                    current_data_index,
                    LineValue::Load(value_to_load));
                current_data_index += 1;
            }
            else
            {
                return Err(format!("line {0:} invalid command \"{1:}\" found", i, cmd));
            }
        }
        else if LABEL_REGEX.is_match(l)
        {
            let label = LABEL_REGEX
                .captures(l)
                .unwrap()
                .name("tag")
                .unwrap()
                .as_str()
                .to_ascii_lowercase();

            if label_map.contains_key(&label)
            {
                return Err(format!("line {0:} label \"{1:}\" already exists", i, label));
            }
            else
            {
                label_map.insert(
                    label,
                    current_data_index);
            }
        }
        else if LINE_REGEX.is_match(l)
        {
            // Extract the captures
            let caps = match LINE_REGEX.captures(l)
            {
                Some(v) => v,
                None => return Err(format!("line {0:} no captures found for \"{1:}\"", i, l))
            };

            // Extract the instruction string and the argument string
            let inst = caps.name("instruction").unwrap().as_str().to_ascii_lowercase();
            let args = match caps.name("args")
            {
                Some(v) => ARGS_SPLIT_REGEX
                    .split(&v.as_str().to_ascii_lowercase())
                    .map(|v| v.to_string())
                    .collect(),
                None => Vec::new()
            };

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
                            inst,
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
        else
        {
            return Err(format!("line {0:} \"{1:}\" does not match expected syntax", i, l));
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
                let new_args = assembly.arguments_for_labels(&label_map);

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
