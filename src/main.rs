use std::{env, io::Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    let stdin = std::io::stdin();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            break;
        }

        if let Some(expr) = parser::parse(input) {
            println!("{expr:?}");
            match codegen::compile(&expr, true) {
                Ok(Some(code)) => {
                    println!("OK");
                    let argc = args.len();
                    let argv = args.clone();
                    let fn_returns: i32 = unsafe { codegen::run_code(code, argc, argv) };
                    println!("= {fn_returns:?}");
                }
                Ok(None) => println!("OK"),
                Err(err) => println!("{err:?}"),
            }
        } else {
            break;
        }
    }
}
