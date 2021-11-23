mod compiler;
mod tokenizer;

use crate::compiler::compile;

fn get_example_program() -> String
{
    let text_bytes = include_bytes!("../../examples/hello_world.sub");
    return match std::str::from_utf8(text_bytes)
    {
        Ok(v) => v.to_string(),
        Err(e) => panic!("UTF-8 Error: {0:}", e.to_string())
    };
}

fn main()
{
    let program = get_example_program();
    compile(&program);
}
