use cbuoy::compile;

fn main() {
    let code = match std::fs::read_to_string("examples/test.spc") {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Unable to open provided file... {e}");
            std::process::exit(1);
        }
    };

    let code = match compile(&code) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Parser Error: {e}");
            std::process::exit(2);
        }
    };

    println!("Compiled into {} words", code.len());
}
