use std::ffi::{c_char, CStr};

use cranelift_codegen::ir::{types::Type, AbiParam};
use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;

use crate::CompileErr;

fn pointer_type(module: &dyn Module) -> Type {
    module.target_config().pointer_type()
}

pub fn register_builtins<'a>(
    module: &'a mut dyn Module,
) -> Result<Vec<(Option<Type>, FuncId)>, CompileErr> {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(pointer_type(module)));
    let extern_f_id = module
        .declare_function("out", cranelift_module::Linkage::Import, &sig)
        .map_err(|e| CompileErr::Err(e.to_string()))?;

    Ok(vec![(None, extern_f_id)])
}

pub unsafe extern "C" fn print_string(string: *const c_char) {
    let cstr = CStr::from_ptr(string);
    print!("{}", cstr.to_str().unwrap())
}
