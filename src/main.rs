extern crate cranelift;
extern crate cranelift_module;
extern crate cranelift_jit;

use std::io::Read;
use std::mem;
use std::env;
use std::collections::HashSet;

mod ts;
use ts::*;

mod jit;
use jit::*;

unsafe fn run_ptr<I, O>(ptr: *const u8, input: I) -> O {
    let code_fn = mem::transmute::<_, fn(I) -> O>(ptr);
    code_fn(input)
}

fn main() {
    let mut jit = JIT::default();
    let mut frontend = Frontend::from_language(tree_sitter_commonlisp::language()).unwrap();

    let mut stdin = std::io::stdin();
    let mut buffer = String::new();

    loop {
        let mut chunk: [u8; 1024] = [0; 1024];

        match stdin.read(&mut chunk[..]) {
            Ok(len) => {
                let utf8_chunk = String::from_utf8(chunk[0..len].to_vec()).unwrap();
                buffer.push_str(&utf8_chunk);
                frontend.parse(&buffer);

                let matches = frontend.query("
(list_lit . (sym_lit) @function.builtin (#cl-standard-function? @function.builtin))
(list_lit . (sym_lit) @function.macro (#cl-standard-macro? @function.macro))
").unwrap();

                let names = matches.capture_names();
                for i in 0..matches.pattern_count() {
                    println!("name: {}", &names[i]);
                    let m = matches.capture_index_for_name(&names[i]).unwrap() as usize;
                    for qp in matches.general_predicates(m).iter() {
                        println!("{:?}", qp);
                    }
                    println!("start: {}", matches.start_byte_for_pattern(m));
                }

                /*
                match jit.compile() {
                    Ok(main_fptr) => {
                        let ret: isize = unsafe {
                            run_ptr(main_fptr, 42)
                        };
                        
                        println!("Executing... {} -> {:?}", 0, ret);
                    },
                    Err(err) => panic!("{}", err),
                }*/

            },
            Err(error) => panic!("{:?}", error),
        }
        frontend.reset();
        buffer.clear();
    }

}
