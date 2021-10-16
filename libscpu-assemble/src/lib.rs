mod instructions;

use instructions::get_instruction_map;

use std::collections::HashMap;

use regex::Regex;

fn cleanup_lines(lines: Vec<String>) -> Vec<String>
{
    let mut new_lines = Vec::new();

    for line in lines.iter()
    {
        let result_string: &str = match line.find("//")
        {
            Some(len) => &line[..len],
            None => line
        }.trim();

        if result_string.len() > 0
        {
            new_lines.push(result_string.to_string());
        }
    }

    return new_lines;
}

pub fn assemble(lines: Vec<String>) -> Result<Vec<u16>, String>
{
    let line_regex = Regex::new(r"^(?P<instruction>[\w]+)(?P<rest>\s+\-?[\w\d]+(,\s*\-?[\w\d]+)*)*$").unwrap();
    let args_split_regex = Regex::new(r",\s*").unwrap();

    let mut data_values = HashMap::<u16, u16>::new();
    let mut data_offset = 0u16;

    let instruction_map = get_instruction_map();

    for (i, l) in cleanup_lines(lines).iter().enumerate()
    {
        // Check for line validity
        if l.is_empty()
        {
            continue;
        }
        else if !line_regex.is_match(l)
        {
            return Err(format!("line {0:} \"{1:}\" does not match expected syntax", i, l));
        }

        // Extract the captures
        let caps = match line_regex.captures(l)
        {
            Some(v) => v,
            None => return Err(format!("line {0:} no captures found for \"{1:}\"", i, l))
        };

        // Extract the instruction string and the argument string
        let inst = caps.name("instruction").unwrap().as_str().to_lowercase();
        let args = match caps.name("rest")
        {
            Some(v) => args_split_regex
                .split(&v.as_str().trim().to_lowercase())
                .map(|v| v.to_string())
                .collect(),
            None => Vec::new()
        };

        // Extract the expected output value
        let inst_val = match instruction_map.get(&inst)
        {
            Some(v) => v,
            None => return Err(format!("line {0:} no instruction {1:} found", i, inst))
        };

        // Attempt to create the resulting data
        let inst_data = match inst_val.to_instruction_data(&args)
        {
            Ok(v) => v,
            Err(e) => return Err(format!("line {0:} {1:} -> {2:}", i, inst, e))
        };

        // Add the resulting instruction value
        if data_values.contains_key(&data_offset)
        {
            return Err(format!("line {0:} offset {1:} already filled", i, data_offset))
        }


        data_values.insert(
            data_offset,
            inst_data.combine());
        data_offset += 1;
    }

    let max_index = match data_values.keys().max()
    {
        Some(v) => v,
        None => return Ok(Vec::new())
    };

    let default_val = 0u16;

    let data_vec: Vec<u16> = (0..(max_index + 1))
        .map(|v| *data_values.get(&v).unwrap_or(&default_val))
        .collect();

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
