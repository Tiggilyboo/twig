extern crate cranelift;
extern crate cranelift_module;
extern crate cranelift_jit;

use std::mem;

mod ts;
use ts::*;

mod jit;
use jit::*;

unsafe fn run_ptr<I, O>(ptr: *const u8, input: I) -> O {
    let code_fn = mem::transmute::<_, fn(I) -> O>(ptr);
    code_fn(input)
}

fn main() {
    let mut frontend = Frontend::default();
    frontend.parse("()");

    /*
    let mut jit = JIT::default();

    jit.create_data("test", "Testing 123".as_bytes().to_vec()).unwrap();

    match jit.compile() {
        Ok(main_fptr) => {
            let ret: isize = unsafe {
                run_ptr(main_fptr, 42)
            };
            
            println!("Executing... {} -> {:?}", 0, ret);
        },
        Err(err) => panic!("{}", err),
    }*/
}
