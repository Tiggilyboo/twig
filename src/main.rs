use std::{env, io::Write, path::Path};

unsafe fn run_code<I, O>(code_ptr: *const u8, input: I) -> O {
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = std::mem::transmute::<_, fn(I) -> O>(code_ptr);
    // And now we can call it!
    code_fn(input)
}

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
                    let fn_returns: i32 = unsafe { run_code(code, args.clone()) };
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
