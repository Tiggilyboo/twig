mod frontend;
mod middleware;
mod jit;

use log::info;
use std::io::Read;
use std::mem;

use frontend::*;
use jit::*;

unsafe fn run_ptr<I, O>(ptr: *const u8, input: I) -> O {
    let code_fn = mem::transmute::<_, fn(I) -> O>(ptr);
    code_fn(input)
}

fn main() {
    env_logger::init();

    let mut fe = Frontend::from_language(tree_sitter_commonlisp::language())
        .expect("Unable to initialize frontend");

    let mut stdin = std::io::stdin();
    let mut buffer = String::new();

    loop {
        let mut chunk: [u8; 1024] = [0; 1024];

        match stdin.read(&mut chunk[..]) {
            Ok(len) => {
                if len == 0 {
                    info!("Empty expression");
                    return;
                }
                let utf8_chunk = String::from_utf8(chunk[0..len].to_vec()).unwrap();
                buffer.push_str(&utf8_chunk);

                let expression: Expression;
                if let Some(parsed_exp) = fe.parse(&buffer) {
                    info!("=> {:#?}", parsed_exp);
                    expression = parsed_exp;
                } else {
                    return;
                }
                let mut jit = JIT::default();

                info!("Compiling...");
                match jit.compile(&expression) {
                    Ok(main_fptr) => {
                        info!("Done.");
                        let ret: isize = unsafe {
                            run_ptr(main_fptr, 0)
                        };
                        println!("-> {:?}", ret);
                    },
                    Err(err) => panic!("Error: {}", err),
                }
            },
            Err(error) => panic!("{:?}", error),
        }
        fe.reset();
        buffer.clear();
    }

}
