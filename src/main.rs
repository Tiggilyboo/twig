use wamr::WamrError;

extern crate wamr_sys;
extern crate alloc;

mod wamr;
mod module;

const MAIN_MODULE_NAME: &str = "module-test";
const STACK_SIZE: u32 = 16 * 1024;
const HEAP_SIZE: u32 = 16 * 1024;

fn main() -> Result<(), WamrError> {
    let args: Vec<String> = std::env::args().collect();
    let mut runtime = wamr::Runtime::init()?;
    
    let mut module = runtime.load_module(MAIN_MODULE_NAME)?;
    module.instantiate(args, STACK_SIZE, HEAP_SIZE)?;

    println!("Done");
    Ok(())
}
