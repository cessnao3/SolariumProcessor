use cbuoy::{TokenError, parse};

static INPUT_TEXT: &str = include_str!("../../cbuoy/examples/test.cb");

fn main() {
    match parse(INPUT_TEXT) {
        Ok(asm) => {
            println!("Success!");
            for t in asm {
                println!("{}", t.tok);
            }
        }
        Err(e) => print_error(INPUT_TEXT, &e),
    }
}

fn print_error(txt: &str, err: &TokenError) {
    eprintln!("Error: {}", err.msg);

    if let Some(t) = &err.token {
        let line_num = t.get_loc().line;
        let display_num = line_num + 1;
        let line = txt.lines().skip(line_num).next().unwrap();
        eprintln!("{display_num} >> {line}");
        eprint!("{display_num}    ");
        for _ in 0..t.get_loc().column {
            eprint!(" ");
        }
        for _ in 0..t.get_value().len() {
            eprint!("^");
        }
        eprintln!();
    }
}
