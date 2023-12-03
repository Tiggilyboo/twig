use parser::grammar::Expr;
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
        } else {
            break;
        }
    }
}
