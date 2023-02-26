use std::io::Read;
use std::mem;

mod frontend;
use frontend::*;

mod jit;
use jit::*;

unsafe fn run_ptr<I, O>(ptr: *const u8, input: I) -> O {
    let code_fn = mem::transmute::<_, fn(I) -> O>(ptr);
    code_fn(input)
}

fn main() {
    let mut jit = JIT::default();
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
                if let Some(parsed_grammar) = fe.parse(&buffer) {
                    println!("=> {:#?}", parsed_grammar);
                }

                /*
                println!("Compiling...");
                
                match jit.compile(parsed_grammar) {
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
