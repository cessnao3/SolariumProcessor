use libscpu_assemble::assemble;

fn main()
{
    let line_test: Vec<String> = vec![
        "ld 0,0,  34".to_string(),
        "pushr 3".to_string()
    ];

    match assemble(line_test)
    {
        Err(e) => eprintln!("{0:}", e),
        _ => ()
    };
}
