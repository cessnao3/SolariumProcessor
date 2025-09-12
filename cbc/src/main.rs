use cbuoy::{TokenError, parse};
use jib_asm::assemble_tokens;

static INPUT_TEXT: &str = include_str!("../../cbuoy/examples/test.cb");

fn main() -> std::process::ExitCode {
    let asm = match parse(INPUT_TEXT) {
        Ok(asm) => asm,
        Err(e) => {
            print_error(INPUT_TEXT, &e);
            return 1.into();
        }
    };

    match assemble_tokens(asm) {
        Ok(r) => {
            println!(";Program Start: {:04x}", r.start_address);
            println!(";Labels:");
            let mut locs = r.labels.iter().map(|(k, v)| (*v, k)).collect::<Vec<_>>();
            locs.sort_by(|a, b| a.0.cmp(&b.0));
            for (v, k) in locs {
                println!(";  {v:04x} => {k}");
            }
            println!(";Debug:");
            for (addr, cmt) in r.debug.iter() {
                println!(";  {addr:04x} => {cmt}");
            }
        }
        Err(e) => {
            eprintln!("{e}");
            return 2.into();
        }
    }

    std::process::ExitCode::SUCCESS
}

fn print_error(txt: &str, err: &TokenError) {
    eprintln!("Error: {}", err.msg);

    if let Some(t) = &err.token {
        let line_num = t.get_loc().line;
        let display_num = line_num + 1;
        let line = txt.lines().nth(line_num).unwrap();
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
