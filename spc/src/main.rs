mod components;
mod parser;
mod types;

fn main() {
    let code = match std::fs::read_to_string("examples/test.spc") {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Unable to open provided file... {e}");
            std::process::exit(1);
        }
    };

    match parser::parse(&code) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("Parser Error: {e}");
            std::process::exit(2);
        }
    };
}
