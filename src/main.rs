use std::io::Write;

fn main() {
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
            match codegen::codegen(&expr) {
                Ok(_) => println!("OK"),
                Err(err) => println!("{err:?}"),
            }
        } else {
            break;
        }
    }
}
