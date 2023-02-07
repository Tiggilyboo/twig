use core::slice;

use cranelift::prelude::*;
use cranelift_module::*;
use cranelift_jit::*;

pub struct JIT {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());

        Self {
            builder_ctx: FunctionBuilderContext::new(),
            data_ctx: DataContext::new(),
            ctx: module.make_context(),
            module,
        }
    }
}

impl JIT {
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        self.data_ctx.define(contents.into_boxed_slice());

        let data_id = self.module.declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module.define_data(data_id, &self.data_ctx).map_err(|e| e.to_string())?;
        self.module.finalize_definitions().map_err(|e| e.to_string())?;

        let buffer = self.module.get_finalized_data(data_id);
        let buffer_slice = unsafe {
            slice::from_raw_parts(buffer.0, buffer.1)
        };

        Ok(buffer_slice)
    }

    pub fn compile(&mut self) -> Result<*const u8, String> {
        // TODO: parse and convert tree-sitter AST, convert to Cranelift IR...
        
        let name = "main";
        let func_id = self.module.declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module.define_function(func_id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.finalize_definitions()
            .map_err(|e| e.to_string())?;

        let func_code = self.module.get_finalized_function(func_id);

        Ok(func_code)
    }
}
