use core::slice;
use std::collections::HashMap;

use log::info;
use cranelift::{prelude::*, codegen::{ir::{Function, UserFuncName}, Context}};
use cranelift_module::*;
use cranelift_jit::*;

use crate::{frontend::Expression, middleware::TranslationResult};
use crate::middleware::FunctionTranslator;

pub struct JIT {
    builder_ctx: FunctionBuilderContext,
    ctx: Context,
    data_ctx: DataContext,
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());
        let ctx = module.make_context();

        Self {
            builder_ctx: FunctionBuilderContext::new(),
            data_ctx: DataContext::new(),
            ctx,
            module,
        }
    }
}

impl JIT {
    pub fn compile(&mut self, expression: &Expression) -> Result<*const u8, String> {
        self.translate(&mut expression.clone())?;

        info!("finalize_definitions");
        self.module.finalize_definitions()
            .map_err(|e| e.to_string())?;
        
        if let Some(main_function) = self.try_get_function("main") {
            Ok(main_function)
        } else {
            Err("main function not defined".to_string())
        }
    }

    pub fn try_get_function(&self, name: &str) -> Option<*const u8> {
        if let Some(FuncOrDataId::Func(func_id)) = self.module.get_name(name) {
            Some(self.module.get_finalized_function(func_id))
        } else {
            None
        }
    }

    pub fn try_get_data(&self, name: &str) -> Option<(*const u8, usize)> {
        if let Some(FuncOrDataId::Data(data_id)) = self.module.get_name(name) {
            Some(self.module.get_finalized_data(data_id))
        } else {
            None
        }
    }

    pub fn make_function_context<'a>(&'a mut self, function_name: &str, linkage: Linkage, params: Vec<Type>, returns: Vec<Type>) -> Result<(FuncId, FunctionBuilder<'a>), String> {
        if self.module.get_name(function_name).is_some() {
            return Err(format!("function '{function_name}' already defined in module"));
        }
        for p in params {
            self.ctx.func.signature.params.push(AbiParam::new(p));
        }
        for r in returns {
            self.ctx.func.signature.returns.push(AbiParam::new(r));
        }

        let func_id = self.module.declare_function(function_name, linkage, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.ctx.func.name = UserFuncName::user(0, func_id.as_u32());

        let mut fb = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
        let block = fb.create_block();

        fb.append_block_params_for_function_params(block);
        fb.switch_to_block(block);

        Ok((func_id, fb))
    }
    
    pub fn declare_built_function(&mut self, func_id: &FuncId) -> Result<(), String> {
        info!("declare_built_function: {func_id}");
        self.module.define_function(*func_id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        Ok(())
    }

    fn translate(&mut self, expression: &mut Expression) -> Result<(), String> {
        let (f_id, mut builder) = self.make_function_context("main", Linkage::Export, vec![], vec![])?;
        {
            let entry_block = builder.current_block().unwrap();
            builder.seal_block(entry_block);

            let mut trans = FunctionTranslator::new(f_id, builder);

            let _trans_results = trans.translate(expression, vec![], None)?;
            let func_returns = trans.finalize_returns(entry_block);

            if func_returns.len() > 0 {
                let ret_values: Vec<Value> = func_returns.into_iter().map(|(v, _)| v.clone()).collect();
                trans.builder.ins().return_(&ret_values);
            }
        }
        self.declare_built_function(&f_id)?;

        Ok(())
    }
}
