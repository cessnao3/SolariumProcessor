mod compiler;
mod tokenizer;
mod lexer;

use std::io::Write;

use crate::compiler::compile;

use sda::assemble;

fn get_example_program() -> String
{
    let text_bytes = include_bytes!("../test.sub");
    return match std::str::from_utf8(text_bytes)
    {
        Ok(v) => v.to_string(),
        Err(e) => panic!("UTF-8 Error: {0:}", e.to_string())
    };
}

fn main()
{
    let program = get_example_program();
    match compile(&program)
    {
        Ok(v) =>
        {
            println!("Compilation Successful: {0:} lines of assembly generated", v.len());

            {
                let mut file = std::fs::File::create("test.smc").unwrap();
                file.write_all(v.join("\n").as_bytes()).unwrap();
            }

            match assemble(v.iter().map(|v| v.as_str()).collect())
            {
                Ok(_) => println!("Assembly Successful!"),
                Err(e) => println!("Assembly Error: {0:}", e)
            };
        },
        Err(e) => println!("Compilation Error: {0:}", e)
    };
}
