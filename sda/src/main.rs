mod assembler;

mod instructions;
mod assembly;

use std::io::Write;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
   /// Filename of the input
   #[clap(short, long, value_parser)]
   file: String,

   /// Provide Hex Output
   #[clap(short, long, value_parser, default_value_t = false)]
   binary_output: bool,

   /// Output File
   #[clap(short, long, value_parser)]
   output_file: Option<String>
}

fn main()
{
    let args = Args::parse();

    let lines = match std::fs::read_to_string(&args.file)
    {
        Ok(s) =>
        {
            s
                .split('\n')
                .map(|i| i.split('\r'))
                .flatten()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
        },
        Err(_) =>
        {
            eprintln!("Unable to read input file {}", args.file);
            std::process::exit(1);
        }
    };

    let result = match assembler::assemble(&lines.iter().map(|v| v.as_ref()).collect::<Vec<_>>())
    {
        Ok(v) => v,
        Err(e) =>
        {
            eprintln!("Unable to assemble {} - {}", args.file, e);
            std::process::exit(1);
        }
    };

    let byte_result = if args.binary_output
    {
        result
            .iter()
            .map(|v| [(v & 0xF) as u8, ((v & 0xF0) >> 8) as u8])
            .flatten()
            .collect::<Vec<_>>()
    }
    else
    {
        result
            .iter()
            .map(|v| format!("0x{:04X}", v))
            .collect::<Vec<_>>()
            .join("\n")
            .into_bytes()
    };

    if let Some(output_file) = args.output_file
    {
        match std::fs::write(
            &output_file,
            byte_result)
        {
            Ok(()) => (),
            Err(e) =>
            {
                eprintln!("Unable to write to {} - {}", output_file, e.to_string());
                std::process::exit(1);
            }
        }
    }
    else
    {
        match std::io::stdout().write_all(&byte_result)
        {
            Ok(()) => (),
            Err(e) =>
            {
                eprintln!("Unable to write to stdout - {}", e.to_string());
                std::process::exit(1);
            }
        }
    }
}
