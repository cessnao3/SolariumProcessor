use cbuoy::compile;

fn main() {
    let code = include_str!("../../examples/test.cb");

    let code = match compile(code) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Parser Error: {e}");
            std::process::exit(2);
        }
    };

    println!("Compiled into {} words", code.len());
}
