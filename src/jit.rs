use core::slice;
use std::collections::HashMap;

use cranelift::{prelude::*, codegen::{ir::{Function, UserFuncName}, Context}};
use cranelift_module::*;
use cranelift_jit::*;

pub struct JIT {
    builder_ctx: FunctionBuilderContext,
    module: JITModule,
    functions: HashMap::<String, FuncId>,
    ctx: Context,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());
        let ctx = module.make_context();

        Self {
            builder_ctx: FunctionBuilderContext::new(),
            functions: HashMap::new(),
            module,
            ctx,
        }
    }
}

impl JIT {
    pub fn compile(&mut self) -> Result<*const u8, String> {
        // TODO: validate frontend, create blocks in middleware, annotate with types, and create clir to compile
        
        self.module.finalize_definitions()
            .map_err(|e| e.to_string())?;

        if let Some(main_function) = self.try_get_function("main") {
            Ok(main_function)
        } else {
            Err("main function not defined".to_string())
        }
    }

    pub fn try_get_function(&self, name: &str) -> Option<*const u8> {
        if let Some(func_id) = self.functions.get(name) {
            Some(self.module.get_finalized_function(*func_id))
        } else {
            None
        }
    }

    pub fn make_function_context(&mut self, function_name: &str, params: Vec<Type>, returns: Vec<Type>) -> Result<(FuncId, FunctionBuilder), String> {
        let mut sig = self.module.make_signature();
        for p in params {
            sig.params.push(AbiParam::new(p));
        }
        for r in returns {
            sig.returns.push(AbiParam::new(r));
        }
        self.ctx.set_disasm(true);
        
        let func_id = self.module.declare_function(function_name, Linkage::Export, &sig)
            .map_err(|e| e.to_string())?;

        self.ctx.func.signature = sig;
        self.ctx.func.name = UserFuncName::user(0, func_id.as_u32());

        let fb = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

        Ok((func_id, fb))
    }
    
    pub fn declare_built_function(&mut self, func_id: &FuncId) -> Result<(), String> {
        self.module.define_function(*func_id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);

        self.functions.insert(self.ctx.func.name.to_string(), *func_id);

        Ok(())
    }
}
