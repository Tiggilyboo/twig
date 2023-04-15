use core::slice;
use std::collections::HashMap;

use log::info;
use cranelift::{prelude::*, codegen::{ir::{Function, UserFuncName}, Context}};
use cranelift_module::*;
use cranelift_jit::*;

use crate::frontend::Expression;
use crate::middleware::FunctionTranslator;

pub struct JIT {
    builder_ctx: FunctionBuilderContext,
    ctx: Context,
    data_ctx: DataContext,
    module: JITModule,
    functions: HashMap::<String, FuncId>,
    data: HashMap::<String, DataId>,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());
        let ctx = module.make_context();

        Self {
            builder_ctx: FunctionBuilderContext::new(),
            functions: HashMap::new(),
            data: HashMap::new(),
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
        if let Some(func_id) = self.functions.get(name) {
            Some(self.module.get_finalized_function(*func_id))
        } else {
            None
        }
    }

    pub fn try_get_data(&self, name: &str) -> Option<(*const u8, usize)> {
        if let Some(data_id) = self.data.get(name) {
            Some(self.module.get_finalized_data(*data_id))
        } else {
            None
        }
    }

    pub fn make_anonymous_context(&mut self, params: Vec<Type>, returns: Vec<Type>) -> Result<(FuncId, FunctionBuilder), String> {
        let mut sig = self.module.make_signature();

        for p in params {
            sig.params.push(AbiParam::new(p));
        }
        for r in returns {
            sig.returns.push(AbiParam::new(r));
        }

        let func_id = self.module.declare_anonymous_function(&sig)
            .map_err(|e| e.to_string())?;
        
        let fn_name = format!("fn_{func_id}");
        if !self.functions.contains_key(&fn_name) {
            return Err(format!("anonymous function {fn_name} already exists in this context"));
        }
        self.functions.insert(fn_name, func_id);

        self.ctx.set_disasm(true);
        self.ctx.func.signature = sig;
        
        let mut fb = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
        let block = fb.create_block();

        fb.append_block_params_for_function_params(block);
        fb.append_block_params_for_function_returns(block);
        fb.switch_to_block(block);


        Ok((func_id, fb))
    }

    pub fn make_function_context<'a>(&'a mut self, function_name: &str, linkage: Linkage, params: Vec<Type>, returns: Vec<Type>) -> Result<(FuncId, FunctionBuilder<'a>), String> {
        if self.functions.contains_key(function_name){
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
        self.functions.insert(function_name.into(), func_id);

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

    pub fn define_data_value(&mut self, name: &str, linkage: Linkage, builder: &mut FunctionBuilder) -> Result<Value, String> {
        let data_id = self.module.declare_data(name, linkage, true, false)
            .map_err(|e| e.to_string())?;
        
        self.data.insert(name.to_string(), data_id);

        let global_val = self.module.declare_data_in_func(data_id, builder.func);
        let ptr = self.module.target_config().pointer_type();

        Ok(builder.ins().symbol_value(ptr, global_val))
    }

    pub fn create_data(&mut self, name: &str, linkage: Linkage, data: Vec<u8>) -> Result<DataId, String> {
        self.data_ctx.define(data.into_boxed_slice());

        let id = self.module.declare_data(name, linkage, true, false)
            .map_err(|e| e.to_string())?;

        self.module.define_data(id, &self.data_ctx)
            .map_err(|e| e.to_string())?; 

        // TODO: why clear?
        self.data_ctx.clear();

        self.data.insert(name.to_string(), id);

        Ok(id)
    }

    fn translate(&mut self, expression: &mut Expression) -> Result<(), String> {
        let params = vec![types::I32];
        let returns = vec![types::I32];
        let (f_id, mut builder) = self.make_function_context("main", Linkage::Export, params.clone(), returns.clone())?;
        {
            let entry_block = builder.current_block().unwrap();
            builder.seal_block(entry_block);

            let mut trans = FunctionTranslator::new(f_id, builder);

            for (i, p_type) in params.iter().enumerate() {
                let val = trans.builder.block_params(entry_block)[i];
                let var = trans.declare_var(*p_type, Some(&format!("p{i}")))?;
            }
            let mut return_vars = Vec::new();
            for (i, r_type) in returns.iter().enumerate() {
                let (var, _) = trans.declare_var(*r_type, Some(&format!("r{i}")))?;
                return_vars.push(var);
            }
            let translated_vals: Vec<Value> = trans.translate(expression, vec![])?.iter().map(|(v,t)| *v).collect();
            for (i, var) in return_vars.iter().enumerate() {
                trans.builder.def_var(*var, translated_vals[i])
            }

            trans.builder.ins().return_(&translated_vals);
        }
        self.declare_built_function(&f_id)?;

        Ok(())
    }
}
