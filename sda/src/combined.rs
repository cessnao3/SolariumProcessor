use super::assembler::assemble;
use super::assembler::AssemblerError;
use super::parser::parse;
use sproc::common::MemoryWord;

pub fn assemble_text(lines: &[&str]) -> Result<Vec<MemoryWord>, AssemblerError> {
    let parsed = parse(lines)?;
    assemble(&parsed)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::assemble_text;

    const NUM_REGISTERS: usize = 16;
    
    fn assemble_lines(lines: &[&str]) -> Result<Vec<u16>, AssemblerError> {
        Ok(assemble_text(lines)?.into_iter().map(|v| v.get()).collect())
    }

    #[test]
    fn basic_test() {
        let line_test = vec!["ld 0, 0", "popr 3"];

        let assemble_result = assemble_lines(&line_test);

        assert!(assemble_result.is_ok());

        let assemble_ok = assemble_result.unwrap();

        assert!(assemble_ok.len() == 2);
        assert!(assemble_ok[0] == 0x200);
        assert!(assemble_ok[1] == 0x43);
    }

    #[test]
    fn test_noop() {
        let line_test = vec!["noop", "noop"];

        let binary_result = assemble_lines(&line_test);

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == 2);
        assert!(binary[0] == 0);
        assert!(binary[1] == 0);
    }

    #[test]
    fn test_all_no_args() {
        let line_test = vec![
            ("noop", 0),
            ("inton", 1),
            ("intoff", 2),
            ("reset", 3),
            ("pop", 4),
            ("ret", 5),
            ("retint", 6),
            ("halt", 7),
        ];

        let binary_result = assemble_lines(&line_test.iter().map(|v| v.0).collect::<Vec<_>>());

        assert!(binary_result.is_ok());

        let binary = binary_result.unwrap();

        assert!(binary.len() == line_test.len());
        for (i, (_, opcode)) in line_test.iter().enumerate() {
            assert!(binary[i] == *opcode);
        }
    }

    #[test]
    fn test_single_arg_register_val() {
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
            ("tnz", 9),
            ("bool", 10),
            ("not", 11),
            ("ldn", 12),
            ("neg", 13),
        ];

        for (arg, opcode) in args_to_test {
            assert!(opcode & 0xF == opcode);

            for reg in 0..NUM_REGISTERS {
                assert!(reg & 0xF == reg);

                let assembly_text = vec![
                    format!("{0:} {1:}", arg, reg),
                    format!("{0:} {1:#X}", arg, reg),
                    format!("{0:} {1:#x}", arg, reg),
                ];

                let expected_result = ((opcode << 4) | reg) as u16;

                let binary_result =
                    assemble_lines(&assembly_text.iter().map(|s| s.as_ref()).collect::<Vec<_>>());

                assert!(binary_result.is_ok());

                let binary = binary_result.unwrap();

                assert!(binary.len() == assembly_text.len());
                for bin_val in binary {
                    assert!(bin_val == expected_result);
                }
            }
        }
    }

    #[test]
    fn test_jmpri() {
        let opcode = 1u16;

        for i in 0..=u8::MAX {
            let expected_result = (opcode << 8) | i as u16;

            let code = vec![
                format!("jmpri {0:}", i),
                format!("jmpri {0:#X}", i),
                format!("jmpri {0:#x}", i),
                format!("jmpri {0:}", i as i8),
            ];

            let binary_result = assemble_lines(&code.iter().map(|v| v.as_ref()).collect::<Vec<_>>());

            assert!(binary_result.is_ok());

            let binary = binary_result.unwrap();

            assert!(binary.len() == code.len());
            for bin_val in binary {
                assert!(bin_val == expected_result);
            }
        }
    }

    #[test]
    fn test_double_arg_register_val() {
        // Define the arguments to test
        let args_to_test = vec![
            ("ld", 2),
            ("sav", 3),
            ("ldr", 4),
            ("savr", 5),
            ("cpy", 6),
            ("tg", 7),
            ("tgs", 8),
            ("tl", 9),
            ("tls", 10),
            ("teq", 11),
        ];

        for (arg, opcode) in args_to_test {
            assert!(opcode & 0xF == opcode);

            for reg1 in 0..NUM_REGISTERS {
                assert!(reg1 & 0xF == reg1);

                for reg2 in 0..NUM_REGISTERS {
                    assert!(reg2 & 0xF == reg2);

                    let assembly_text = vec![
                        format!("{0:} {1:}, {2:}", arg, reg1, reg2),
                        format!("{0:} {1:#x}, {2:#X}", arg, reg1, reg2),
                    ];

                    let expected_result = (opcode << 8) | ((reg2 as u16) << 4) | (reg1 as u16);

                    let binary_result =
                        assemble_lines(&assembly_text.iter().map(|s| s.as_ref()).collect::<Vec<_>>());

                    assert!(binary_result.is_ok());

                    let binary = binary_result.unwrap();

                    assert!(binary.len() == assembly_text.len());
                    for bin_val in binary {
                        assert!(bin_val == expected_result);
                    }
                }
            }
        }
    }

    #[test]
    fn test_triple_arg_register_val() {
        // Define the arguments to test
        let args_to_test = vec![
            ("add", 3),
            ("sub", 4),
            ("mul", 5),
            ("div", 6),
            ("mod", 7),
            ("muls", 8),
            ("divs", 9),
            ("mods", 10),
            ("band", 11),
            ("bor", 12),
            ("bxor", 13),
            ("bshft", 14),
            ("ashft", 15),
        ];

        for (arg, opcode) in args_to_test {
            assert!(opcode & 0xF == opcode);

            for reg1 in 0..NUM_REGISTERS {
                assert!(reg1 & 0xF == reg1);

                for reg2 in 0..NUM_REGISTERS {
                    assert!(reg2 & 0xF == reg2);

                    for reg3 in 0..NUM_REGISTERS {
                        let assembly_text = vec![
                            format!("{0:} {1:}, {2:}, {3:}", arg, reg1, reg2, reg3),
                            format!("{0:} {1:#x}, {2:#X}, {3:}", arg, reg1, reg2, reg3),
                        ];

                        let expected_result = (opcode << 12)
                            | ((reg3 as u16) << 8)
                            | ((reg2 as u16) << 4)
                            | (reg1 as u16);

                        let binary_result =
                            assemble_lines(&assembly_text.iter().map(|s| s.as_ref()).collect::<Vec<_>>());

                        assert!(binary_result.is_ok());

                        let binary = binary_result.unwrap();

                        assert!(binary.len() == assembly_text.len());
                        for bin_val in binary {
                            assert!(bin_val == expected_result);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_ld_immediate() {
        let instruction_vals = vec![("ldi", 1), ("ldri", 2)];

        for (instruction, opcode) in instruction_vals {
            assert!(opcode & 0xF == opcode);

            for reg in 0..NUM_REGISTERS {
                assert!(reg & 0xF == reg);

                for immediate_val in 0..=u8::MAX {
                    let expected_result =
                        (opcode << 12) | ((immediate_val as u16) << 4) | (reg as u16);

                    let code = vec![
                        format!("{0:} {1:}, {2:}", instruction, reg, immediate_val),
                        format!("{0:} {1:#X}, {2:#X}", instruction, reg, immediate_val),
                        format!("{0:} {1:#x}, {2:#x}", instruction, reg, immediate_val),
                        format!("{0:} {1:}, {2:}", instruction, reg, immediate_val as i8),
                    ];

                    let binary_result =
                        assemble_lines(&code.iter().map(|v| v.as_ref()).collect::<Vec<_>>());

                    assert!(binary_result.is_ok());

                    let binary = binary_result.unwrap();

                    assert!(binary.len() == code.len());
                    for bin_val in binary {
                        assert!(bin_val == expected_result);
                    }
                }
            }
        }
    }

    #[test]
    fn test_infinite_counter_program() {
        // Define the assembly code
        let assembly_lines = vec![
            "; Define the starting location",
            ".loadloc start",
            "",
            "; Move to the starting location",
            ".oper 0x20",
            ":start",
            "ldn $sp",
            ".load 0x400",
            "",
            ":register_reset",
            "ldri 5, addloc",
            "add $pc, $pc, 5",
            "reset",
            "ldi $sp, 22",
            "ldi 8, 33",
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
            ".load 1",
        ];

        // Determine the expected values
        let mut expected_result = HashMap::<usize, u16>::new();
        expected_result.insert(0, 0x20);
        expected_result.insert(0x20, 0xC2);
        expected_result.insert(0x21, 0x0400);
        expected_result.insert(0x22, 0x2095);
        expected_result.insert(0x23, 0x3500);
        expected_result.insert(0x24, 0x3);
        expected_result.insert(0x25, 0x1162);
        expected_result.insert(0x26, 0x1218);
        expected_result.insert(0x27, 0x1016);
        expected_result.insert(0x28, 0x1007);
        expected_result.insert(0x29, 0x3677);
        expected_result.insert(0x2A, 0x1FF);
        expected_result.insert(0x2B, 1);

        // Assemble the program
        let binary_result = assemble_lines(&assembly_lines);
        assert!(binary_result.is_ok());
        let binary = binary_result.unwrap();

        // Check assembled size
        let expected_size = match expected_result.keys().max() {
            Some(v) => *v + 1,
            None => 0,
        };

        assert!(binary.len() == expected_size);

        // Check assembled values
        for (index, word) in binary.iter().enumerate() {
            let resulting_val = match expected_result.get(&index) {
                Some(v) => *v,
                None => 0,
            };

            if resulting_val != *word {
                println!("Location {index:x} does not match - expected {resulting_val:x} != {word:x}")
            }

            assert_eq!(resulting_val, *word)
        }
    }

    #[test]
    fn test_register_shortcuts() {
        // Define the arguments to test
        let args_to_test = vec![("push", 3)];

        // Define the shortcuts to test
        let reg_shortcuts = vec![
            ("$pc", 0),
            ("$stat", 1),
            ("$sp", 2),
            ("$exc", 3),
            ("$ret", 4),
            ("$arg", 5),
        ];

        // Iterate over selected values
        for (arg, opcode) in args_to_test {
            assert!(opcode & 0xF == opcode);

            for (shortcut, reg) in &reg_shortcuts {
                assert!(*reg & 0xF == *reg);

                let assembly_text = vec![format!("{0:} {1:}", arg, shortcut)];

                let expected_result = ((opcode as u16) << 4) | *reg;

                let binary_result =
                    assemble_lines(&assembly_text.iter().map(|s| s.as_ref()).collect::<Vec<_>>());

                assert!(binary_result.is_ok());

                let binary = binary_result.unwrap();

                assert!(binary.len() == assembly_text.len());
                for bin_val in binary {
                    assert!(bin_val == expected_result);
                }
            }
        }
    }

    #[test]
    fn test_invalid_arg_count() {
        let instructions_to_test = vec![
            ("reset", 0),
            ("jmp", 1),
            ("jmpri", 1),
            ("ldr", 2),
            ("ldi", 2),
            ("ldri", 2),
            ("add", 3),
            (".load", 1),
            (".oper", 1),
        ];

        for (inst, arg_num) in instructions_to_test {
            for i in 0..5 {
                let arg_vals = (0..i)
                    .map(|_| "0".to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let line_val = format!("{0:} {1:}", inst, arg_vals);

                let lines = vec![line_val.trim()];

                let assembly_result = assemble_lines(&lines);

                if i == arg_num {
                    assert!(assembly_result.is_ok());
                } else {
                    assert!(assembly_result.is_err());
                }
            }
        }
    }

    #[test]
    fn test_load_text() {
        let text_val = "hello, world!";
        let text_to_load = vec![format!(".loadtext \"{0:}\"", text_val)];

        let expected_output: Vec<u16>;
        {
            let mut expected_words: Vec<_> = match text_val
                .chars()
                .map(sproc::text::character_to_word)
                .collect()
            {
                Ok(v) => v,
                Err(e) => panic!("{}", e.to_string()),
            };
            expected_words.push(MemoryWord::default());

            expected_output = expected_words.iter().map(|v| v.get()).collect();
        }

        let assembly_result =
            assemble_lines(&text_to_load.iter().map(|v| v.as_str()).collect::<Vec<_>>());

        assert!(assembly_result.is_ok());

        let data = assembly_result.unwrap();

        assert_eq!(data.len(), expected_output.len());

        for i in 0..data.len() {
            assert_eq!(data[i], expected_output[i]);
        }
    }

    #[test]
    fn test_invalid_lines() {
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
            ".loadloc asdf",
        ];

        for line in bad_lines {
            let new_lines = vec![line];

            let assembly_result = assemble_lines(&new_lines);

            assert!(assembly_result.is_err());
        }
    }
}
