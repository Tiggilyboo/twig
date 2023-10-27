use std::io;
use wasmtime::Engine;

mod parser;
use parser::Token;

fn main() {
    let stdin = io::stdin();

    loop {
        let mut buf = String::new();
        match stdin.read_line(&mut buf) {
            Err(err) => {
                println!("{err}");
                break;
            }
            _ => (),
        }

        let tokens: Option<Vec<Token>> = match parser::parse(&buf) {
            Err(err) => {
                println!("{err}");
                None
            }
            Ok(tokens) => Some(tokens),
        };
        if let Some(tokens) = tokens {
            for t in tokens {
                println!("{t:?}")
            }
        }
    }

    let engine = Engine::default();
}
