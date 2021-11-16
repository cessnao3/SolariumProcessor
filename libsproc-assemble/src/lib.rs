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
        // Cleanup the resulting string value and remove comments
        let l: &str = match l_in.find(";")
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
mod tests
{
    use std::collections::HashMap;

    use super::assemble;

    const NUM_REGISTERS: usize = 16;

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
    fn test_all_no_args()
    {
        let line_test = vec![
            ("noop", 0),
            ("inton", 1),
            ("intoff", 2),
            ("reset", 3),
            ("pop", 4),
            ("ret", 5),
            ("retint", 6)
        ];

        let binary_result = assemble(line_test.iter().map(|v| v.0).collect());

        assert    !(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == line_test.len());
        for (i, (_, opcode)) in line_test.iter().enumerate()
        {
            assert!(binary[i] == *opcode);
        }
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
            ("int", 6),
            ("intr", 7),
            ("tz", 8),
            ("tgz", 9),
            ("tlz", 10)
        ];

        for (arg, opcode) in args_to_test
        {
            assert!(opcode & 0xF == opcode);

            for reg in 0..NUM_REGISTERS
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

                assert!(binary.len() == assembly_text.len());
                for bin_val in binary
                {
                    assert!(bin_val == expected_result);
                }
            }
        }
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

            assert!(binary.len() == code.len());
            for bin_val in binary
            {
                assert!(bin_val == expected_result);
            }
        }
    }

    #[test]
    fn test_double_arg_register_val()
    {
        // Define the arguments to test
        let args_to_test = vec![
            ("ld", 2),
            ("sav", 3),
            ("ldr", 4),
            ("savr", 5),
            ("copy", 6),
            ("tg", 7),
            ("tge", 8),
            ("tl", 9),
            ("tle", 10)
        ];

        for (arg, opcode) in args_to_test
        {
            assert!(opcode & 0xF == opcode);

            for reg1 in 0..NUM_REGISTERS
            {
                assert!(reg1 & 0xF == reg1);

                for reg2 in 0..NUM_REGISTERS
                {
                    assert!(reg2 & 0xF == reg2);

                    let assembly_text = vec![
                        format!("{0:} {1:}, {2:}", arg, reg1, reg2),
                        format!("{0:} {1:#x}, {2:#X}", arg, reg1, reg2)
                    ];

                    let expected_result = (opcode << 8) | ((reg2 as u16) << 4) | (reg1 as u16);

                    let binary_result = assemble(assembly_text.iter().map(|s| s.as_ref()).collect());

                    assert!(binary_result.is_ok());

                    let binary = binary_result.unwrap();

                    assert!(binary.len() == assembly_text.len());
                    for bin_val in binary
                    {
                        assert!(bin_val == expected_result);
                    }
                }
            }
        }
    }

    #[test]
    fn test_triple_arg_register_val()
    {
        // Define the arguments to test
        let args_to_test = vec![
            ("add", 4),
            ("sub", 5),
            ("mul", 6),
            ("div", 7),
            ("mod", 8),
            ("band", 9),
            ("bor", 10),
            ("bxor", 11),
            ("bsftl", 12),
            ("bsftr", 13)
        ];

        for (arg, opcode) in args_to_test
        {
            assert!(opcode & 0xF == opcode);

            for reg1 in 0..NUM_REGISTERS
            {
                assert!(reg1 & 0xF == reg1);

                for reg2 in 0..NUM_REGISTERS
                {
                    assert!(reg2 & 0xF == reg2);

                    for reg3 in 0..NUM_REGISTERS
                    {
                        let assembly_text = vec![
                            format!("{0:} {1:}, {2:}, {3:}", arg, reg1, reg2, reg3),
                            format!("{0:} {1:#x}, {2:#X}, {3:}", arg, reg1, reg2, reg3)
                        ];

                        let expected_result = (opcode << 12) | ((reg3 as u16) << 8) | ((reg2 as u16) << 4) | (reg1 as u16);

                        let binary_result = assemble(assembly_text.iter().map(|s| s.as_ref()).collect());

                        assert!(binary_result.is_ok());

                        let binary = binary_result.unwrap();

                        assert!(binary.len() == assembly_text.len());
                        for bin_val in binary
                        {
                            assert!(bin_val == expected_result);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_ld_immediate()
    {
        let instruction_vals = vec![
            ("ldi", 1),
            ("ldui", 2),
            ("ldir", 3)
        ];

        for (instruction, opcode) in instruction_vals
        {
            assert!(opcode & 0xF == opcode);

            for reg in 0..NUM_REGISTERS
            {
                assert!(reg & 0xF == reg);

                for immediate_val in 0..=u8::MAX
                {
                    let expected_result = (opcode << 12) | ((immediate_val as u16) << 4) | (reg as u16);

                    let code = vec![
                        format!("{0:} {1:}, {2:}", instruction, reg, immediate_val),
                        format!("{0:} {1:#X}, {2:#X}", instruction, reg, immediate_val),
                        format!("{0:} {1:#x}, {2:#x}", instruction, reg, immediate_val),
                        format!("{0:} {1:}, {2:}", instruction, reg, immediate_val as i8)
                    ];

                    let binary_result = assemble(code.iter().map(|v| v.as_ref()).collect());

                    assert!(binary_result.is_ok());

                    let binary = binary_result.unwrap();

                    assert!(binary.len() == code.len());
                    for bin_val in binary
                    {
                        assert!(bin_val == expected_result);
                    }
                }
            }
        }
    }

    #[test]
    fn test_infinite_counter_program()
    {
        // Define the assembly code
        let assembly_lines = vec![
            "; Define the starting location",
            ".loadloc start",
            "",
            "; Move to the starting location",
            ".oper 0x20",
            ":start",
            "",
            ":register_reset",
            "ldir 5, addloc",
            "add $pc, $pc, 5",
            "reset",
            "ldi $sp, 22",
            "ldi $ret, 33",
            "",
            "; Load initial values",
            "ldi 6, 1",
            "ldi 7, 0",
            "",
            ":loop ; define the main loop",
            "add 7, 7, 6",
            "jmpri loop",
            "",
            "; define the addition value",
            ":addloc",
            ".load 1"
        ];

        // Determine the expected values
        let mut expected_result = HashMap::<usize, u16>::new();
        expected_result.insert(0, 0x20);
        expected_result.insert(0x20, 0x3095);
        expected_result.insert(0x21, 0x4500);
        expected_result.insert(0x22, 0x3);
        expected_result.insert(0x23, 0x1161);
        expected_result.insert(0x24, 0x1212);
        expected_result.insert(0x25, 0x1016);
        expected_result.insert(0x26, 0x1007);
        expected_result.insert(0x27, 0x4677);
        expected_result.insert(0x28, 0x1FF);
        expected_result.insert(0x29, 1);

        // Assemble the program
        let binary_result = assemble(assembly_lines);
        assert!(binary_result.is_ok());
        let binary = binary_result.unwrap();

        // Check assembled size
        let expected_size = match expected_result.keys().max()
        {
            Some(v) => *v + 1,
            None => 0
        };

        assert!(binary.len() == expected_size);

        // Check assembled values
        for (index, word) in binary.iter().enumerate()
        {
            let resulting_val = match expected_result.get(&index)
            {
                Some(v) => *v,
                None => 0
            };

            assert!(resulting_val == *word)
        }
    }

    #[test]
    fn test_register_shortcuts()
    {
        // Define the arguments to test
        let args_to_test = vec![
            ("push", 3)
        ];

        // Define the shortcuts to test
        let reg_shortcuts = vec![
            ("$pc", 0),
            ("$sp", 1),
            ("$ret", 2)
        ];

        // Iterate over selected values
        for (arg, opcode) in args_to_test
        {
            assert!(opcode & 0xF == opcode);

            for (shortcut, reg) in &reg_shortcuts
            {
                assert!(*reg & 0xF == *reg);

                let assembly_text = vec![
                    format!("{0:} {1:}", arg, shortcut),
                ];

                let expected_result = ((opcode as u16) << 4) | *reg;

                let binary_result = assemble(assembly_text.iter().map(|s| s.as_ref()).collect());

                assert!(binary_result.is_ok());

                let binary = binary_result.unwrap();

                assert!(binary.len() == assembly_text.len());
                for bin_val in binary
                {
                    assert!(bin_val == expected_result);
                }
            }
        }
    }

    #[test]
    fn test_invalid_arg_count()
    {
        let instructions_to_test = vec![
            ("reset", 0),
            ("jmp", 1),
            ("jmpri", 1),
            ("ldr", 2),
            ("ldi", 2),
            ("ldir", 2),
            ("add", 3),
            (".load", 1),
            (".oper", 1)
        ];

        for (inst, arg_num) in instructions_to_test
        {
            for i in 0..5
            {
                let arg_vals = (0..i).map(|_| "0".to_string()).collect::<Vec<String>>().join(", ");
                let line_val = format!("{0:} {1:}", inst.to_string(), arg_vals);

                let lines = vec![
                    line_val.trim()
                ];

                let assembly_result = assemble(lines);

                if i == arg_num
                {
                    assert!(assembly_result.is_ok());
                }
                else
                {
                    assert!(assembly_result.is_err());
                }
            }
        }
    }

    #[test]
    fn test_invalid_lines()
    {
        let bad_lines = vec![
            "asdf 45",
            "ld 1",
            "ld 1 2",
            "sav 1 2",
            "ld 0, 17",
            "sav 20, 34",
            "sav x1, 1",
            "sav 0x11, 1",
            ".super 1",
            ".oper 0x11111",
            ".loadloc asdf"
        ];

        for line in bad_lines
        {
            let new_lines = vec![
                line
            ];

            let assembly_result = assemble(new_lines);

            assert!(assembly_result.is_err());
        }
    }
}
