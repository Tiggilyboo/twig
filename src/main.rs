mod frontend;
mod middleware;
mod jit;

use std::io::Read;
use std::mem;

use frontend::*;
use jit::*;
use middleware::*;


fn main() {
    let mut fe = Frontend::from_language(tree_sitter_commonlisp::language())
        .expect("Unable to initialize frontend");

    let mut stdin = std::io::stdin();
    let mut buffer = String::new();

    loop {
        let mut chunk: [u8; 1024] = [0; 1024];

        match stdin.read(&mut chunk[..]) {
            Ok(len) => {
                let utf8_chunk = String::from_utf8(chunk[0..len].to_vec()).unwrap();
                buffer.push_str(&utf8_chunk);

                let expression: Expression;
                if let Some(parsed_exp) = fe.parse(&buffer) {
                    println!("=> {:#?}", parsed_exp);
                    expression = parsed_exp;
                } else {
                    return;
                }

                match Middleware::process_expression(&expression) {
                    Ok(mut middle) => {
                        println!("Compiling...");
                        match middle.compile() {
                            Ok(_) => println!("Done."),
                            Err(err) => println!("Error: {}", err),
                        }
                    },
                    Err(err) => println!("{}", err),
                }
 
                /*
                
                match jit.compile() {
                    Ok(main_fptr) => {
                        let ret: isize = unsafe {
                            run_ptr(main_fptr, 42)
                        };
                        
                        println!("Executing... {} -> {:?}", 0, ret);
                    },
                    Err(err) => println!("{:?}", err),
                }*/

            },
            Err(error) => panic!("{:?}", error),
        }
        fe.reset();
        buffer.clear();
    }

}
