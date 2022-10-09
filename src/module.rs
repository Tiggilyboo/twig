use std::{ffi::{CString, CStr}, os::raw::c_char, cell::Cell};

use wamr_sys::*;

use crate::wamr::{WamrError, WamrErrorKind};

pub struct Module {
    name: String,
    module: Cell<*mut WASMModuleCommon>,
    instance: Option<WASMModuleInstanceCommon>,
    exec_env: Option<WASMExecEnv>,
}

impl Module {
    pub fn from(name: &str, module: &mut WASMModuleCommon) -> Self {
        Self {
            module: Cell::new(module),
            name: name.to_string(),
            instance: None,
            exec_env: None,
        }
    }

    pub fn module(&self) -> Cell<*mut WASMModuleCommon> {
        self.module.clone()
    }

    pub fn instantiate(&mut self, args: Vec<String>, stack_size: u32, heap_size: u32, ) -> Result<(), WamrError> {
        let mut err_buf: Vec<i8> = vec![32; 128];
        
        println!("Instantiating module '{}' with args {:?}", self.name, args);

        let c_args = args  
            .iter().map(|a| CString::new(a.clone()).unwrap().as_ptr())
            .collect::<Vec<*const c_char>>();

        let module_env: *mut WASMExecEnv;
        let module_ptr: *mut WASMModuleCommon = self.module.get();
        let module_inst: *mut WASMModuleInstanceCommon;

        unsafe {
            let null_ptr = 0 as *mut *const i8;

            println!("Setting WASI args...");
            wasm_runtime_set_wasi_args(
                module_ptr,
                null_ptr,
                0,
                null_ptr,
                0,
                null_ptr,
                0,
                c_args.as_ptr() as *mut *mut i8,
                c_args.len() as i32);

            println!("Instantiating module instance...");
            module_inst = wasm_runtime_instantiate(
                module_ptr,
                stack_size,
                heap_size,
                err_buf.as_mut_ptr(),
                err_buf.len() as u32,
            );
            if module_inst.is_null() {
                let msg = CStr::from_ptr(err_buf.as_ptr()).to_string_lossy().to_string();
                return Err(WamrError::message(WamrErrorKind::NullResponse, msg))
            }

            println!("Instantiating execution environment...");
            module_env = wasm_runtime_create_exec_env(module_inst, stack_size);
            if module_env.is_null() {
                let msg = CStr::from_ptr(err_buf.as_ptr()).to_string_lossy().to_string();
                return Err(WamrError::message(WamrErrorKind::NullResponse, msg))
            }
            
            self.instance = Some(*module_inst);
            self.exec_env = Some(*module_env);

            println!("Done.");
        }

        Ok(())
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            module: self.module.clone(),
            instance: self.instance,
            exec_env: self.exec_env,
        }
    }
}
