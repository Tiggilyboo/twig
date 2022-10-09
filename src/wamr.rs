use std::collections::HashMap;
use std::error::Error;
use std::ffi::CStr;
use std::fmt::{self, Display};
use std::fs::File;
use std::io::{Read, BufReader};
use std::path::Path;

use crate::module::*;
use wamr_sys::*;


pub struct Runtime {
    modules: HashMap<String, Module>,
}

impl Runtime{
    pub fn init() -> Result<Self, WamrError> {
        let mut init_args = RuntimeInitArgs::default();

        init_args.instance_port = 1234;
        init_args.mem_alloc_type = mem_alloc_type_t_Alloc_With_System_Allocator;
        let mut ip_buf: [i8; 128] = [0i8; 128];
        let ip = (*b"127.0.0.1\0").map(|u| u as i8);
        for i in 0..ip.len() {
            ip_buf[i] = ip[i];
        }
        init_args.ip_addr = ip_buf;

        let init = unsafe { 
            wasm_runtime_full_init(&mut init_args as *mut RuntimeInitArgs) 
        };
        if !init {
            return Err(WamrError::new(WamrErrorKind::Init));
        }

        Ok(Self {
            modules: HashMap::new(),
        })
    }

    pub fn load_module(&mut self, module_name: &str) -> Result<Module, WamrError> {

        if self.modules.contains_key(module_name) {
            return Err(WamrError::new(WamrErrorKind::ModuleAlreadyLoaded))
        }

        let path = &format!("./modules/{}.wasm", module_name);

        if !Path::new(path).exists() {
            return Err(WamrError::new(WamrErrorKind::FileNotFound));
        }

        let file = File::open(path)?;
        let mut reader = BufReader::new(file);
        let mut buf: Vec<u8> = Vec::new();
        reader.read_to_end(&mut buf)?;
        
        println!("Read module {} with {} bytes", path, buf.len());

        let mut err_buf: Vec<i8> = vec![32; 128];
            
        unsafe {
            let raw_buf_ptr: *mut u8 = buf.as_mut_ptr();
            let raw_module_ptr = wasm_runtime_load(
                raw_buf_ptr, 
                buf.len() as u32, 
                err_buf.as_mut_ptr(), 
                err_buf.len() as u32);

            if !raw_module_ptr.is_null() {
                let loaded_module = Module::from(module_name, &mut *raw_module_ptr);
                if let Some(_) = self.modules.insert(module_name.to_string(), loaded_module.clone()) {
                    Err(WamrError::message(WamrErrorKind::ModuleAlreadyLoaded, module_name.to_string()))
                } else {
                    Ok(loaded_module)
                }
            } else {
                println!("This one!");
                let cstr = CStr::from_ptr(err_buf.as_ptr()).to_string_lossy();
                let error = cstr.to_string();
                Err(WamrError::message(WamrErrorKind::NullResponse, error))
            }
        }
    }

    pub fn unload_module(&mut self, module_name: &str) -> Result<(), WamrError> {
        match self.modules.get(module_name) {
            Some(module) => {
                unsafe {
                    wasm_runtime_unload(module.module().get());
                }
                Ok(())
            },
            None => Err(WamrError::new(WamrErrorKind::ModuleAlreadyLoaded)),
        } 
    }

    pub fn get_module(&self, module_name: &str) -> Result<&Module, WamrError> {
        if let Some(module) = self.modules.get(module_name) {
            Ok(module)
        } else {
            Err(WamrError::new(WamrErrorKind::ModuleNotLoaded))
        }
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        unsafe {
            let keys = self.modules.keys().map(|k| k.clone()).collect::<Vec<String>>();
            for module_name in keys {
                self.unload_module(&module_name);
            }

            wasm_runtime_destroy();
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WamrErrorKind {
    Unhandled,
    Init,
    NullResponse,
    FileIO,
    FileRead,
    FileNotFound,
    ModuleAlreadyLoaded,
    ModuleNotLoaded,
}


#[derive(Debug)]
pub struct WamrError {
    kind: WamrErrorKind,
    message: Option<String>,
}

impl WamrError {
    pub const fn new(kind: WamrErrorKind) -> Self {
        Self { 
            kind, 
            message: None,
        }
    }
    pub const fn message(kind: WamrErrorKind, msg: String) -> Self {
        Self {
            kind,
            message: Some(msg),
        }
    }
    
    pub fn from(err: &impl Error, kind: WamrErrorKind) -> Self {
        Self { 
            kind, 
            message: Some(err.to_string()),
        }
    }

    fn format_err(&self) -> String {
        if let Some(msg) = &self.message {
            format!("[{:?}] {}", self.kind, msg)
        } else {
            format!("[{:?}] {}", self.kind, self.kind_string())
        }
    }

    const fn kind_string(&self) -> &'static str {
        use WamrErrorKind::*;

        match self.kind {
            Init => "unable to init wasm runtime",
            FileNotFound => "unable to find file",
            FileRead => "unable to read file",
            FileIO => "io error",
            NullResponse => "wamr_sys returned null pointer",
            Unhandled => "unhandled error",
            ModuleAlreadyLoaded => "a module with the same name is already loaded",
            ModuleNotLoaded => "module was not loaded",
        }
    }
}

impl Error for WamrError {
    fn description(&self) -> &str {
        self.kind_string()
    }
}

impl Display for WamrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_err().fmt(f)
    }
}

impl From<std::io::Error> for WamrError {
    fn from(err: std::io::Error) -> Self {
        Self::from(&err, WamrErrorKind::FileIO)
    }
}
