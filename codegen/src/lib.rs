use std::{collections::HashMap, io::BufWriter, path::PathBuf};

use cranelift::{
    codegen::{
        ir::{Function, UserExternalName, UserFuncName},
        verifier::VerifierErrors,
        verify_function, Context,
    },
    prelude::{settings::Flags, *},
};
use cranelift_module::{DataId, FuncId, Linkage, Module, ModuleError};
use cranelift_object::{
    object::{write::WritableBuffer, xcoff::FileAux32, File},
    ObjectBuilder, ObjectModule,
};
use parser::*;

#[derive(Debug)]
pub enum CodegenErr {
    Err(String),
    VerifierErr(VerifierErrors),
    InitErr(String),
    ModuleErr(ModuleError),
    IoErr(String),
}

pub struct Compiler {
    module: ObjectModule,
    flags: Flags,
    functions: HashMap<String, Function>,
    variables: HashMap<String, Variable>,
    entity_count: usize,
    debug: bool,
}

#[derive(Debug)]
pub struct CodegenResult {
    values: Vec<(Type, Value)>,
}

impl Default for CodegenResult {
    fn default() -> Self {
        Self {
            values: Vec::with_capacity(0),
        }
    }
}

impl From<Vec<(Type, Value)>> for CodegenResult {
    fn from(values: Vec<(Type, Value)>) -> CodegenResult {
        CodegenResult { values }
    }
}
impl From<(Type, Value)> for CodegenResult {
    fn from(value: (Type, Value)) -> CodegenResult {
        CodegenResult {
            values: vec![value],
        }
    }
}

impl Compiler {
    pub fn new(name: &str, flags: Flags) -> Result<Self, CodegenErr> {
        let isa_builder =
            cranelift_native::builder().map_err(|e| CodegenErr::InitErr(e.to_string()))?;
        let isa = isa_builder
            .finish(flags.clone())
            .map_err(|e| CodegenErr::InitErr(e.to_string()))?;
        let obj_builder = ObjectBuilder::new(isa, name, cranelift_module::default_libcall_names())
            .map_err(|e| CodegenErr::InitErr(e.to_string()))?;
        let module = ObjectModule::new(obj_builder);

        Ok(Self {
            flags,
            module,
            functions: HashMap::new(),
            variables: HashMap::new(),
            entity_count: 0,
            debug: true,
        })
    }

    fn define_var(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        value: &Expr,
    ) -> Result<CodegenResult, CodegenErr> {
        if self.functions.contains_key(name) || self.variables.contains_key(name) {
            return Err(CodegenErr::Err(
                format!("Identifier already defined: {name}").to_string(),
            ));
        }

        match value {
            Expr::Number(numeric) => {
                let var_val = numeric_to_value(builder, numeric).unwrap();
                let var = Variable::new(self.get_next_entity());
                builder.declare_var(var, var_val.0);
                builder.def_var(var, var_val.1);
            }
            _ => {
                return Err(CodegenErr::Err(
                    format!("Unhandled value expression: {value:?}").to_string(),
                ))
            }
        }

        Ok(CodegenResult::default())
    }

    fn define_func(
        &mut self,
        name: String,
        func_type: Option<Type>,
        body: &Expr,
    ) -> Result<(), CodegenErr> {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        let mut func_id = self
            .module
            .declare_function(&name, Linkage::Export, &sig)
            .map_err(|e| CodegenErr::ModuleErr(e))?;

        let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

            // TODO: When is it preferable to make new blocks?
            let block = builder.create_block();
            builder.switch_to_block(block);
            builder.seal_block(block);

            let res = self.codegen_func(&mut builder, body)?;

            // RETURNS
            if let Some(func_type) = func_type {
                let zero = [builder.ins().iconst(func_type, 0)];
                builder.ins().return_(&zero);
            } else {
                builder.ins().return_(&[]);
            }

            builder.seal_all_blocks();
            if self.debug {
                println!("{}", builder.func);
            }
            builder.finalize();
        }

        verify_function(&func, &self.flags).map_err(|e| CodegenErr::VerifierErr(e))?;

        let mut ctx = codegen::Context::for_function(func);
        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CodegenErr::ModuleErr(e))?;

        Ok(())
    }

    fn get_next_entity(&mut self) -> usize {
        let e = self.entity_count;
        self.entity_count += 1;
        e
    }

    fn codegen_func(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
    ) -> Result<CodegenResult, CodegenErr> {
        let func_name = builder.func.name.get_user();
        match expr {
            Expr::Number(numeric) => {
                if let Some(value) = numeric_to_value(builder, numeric) {
                    Ok(CodegenResult::from(value))
                } else {
                    Err(CodegenErr::Err(format!(
                        "Unable to convert numeric to value: {numeric:?}"
                    )))
                }
            }
            Expr::List((), items, ()) => {
                let results: Vec<(Type, Value)> = items
                    .iter()
                    .map(|item| self.codegen_func(builder, item).map(|r| r.values))
                    .flatten()
                    .flatten()
                    .collect();

                Ok(CodegenResult::from(results))
            }
            Expr::Operation(op, operand) => {
                let operand = self.codegen_func(builder, operand.as_ref())?;
                // TODO: vector ops
                let biggest_var_type = operand
                    .values
                    .iter()
                    .max_by(|x, y| x.0.bits().cmp(&y.0.bits()))
                    .map(|v| v.0)
                    .unwrap();

                let mut accumulator = builder.ins().iconst(biggest_var_type, 0);

                if biggest_var_type.is_int() {
                    match op {
                        Operator::Add => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().iadd(accumulator, *val);
                            }
                        }
                        Operator::Sub => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().isub(accumulator, *val);
                            }
                        }
                        Operator::Mul => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().imul(accumulator, *val);
                            }
                        }
                        Operator::Div => {
                            // DIV 0
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().sdiv(accumulator, *val);
                            }
                        }
                        Operator::Mod => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().srem(accumulator, *val);
                            }
                        }
                        _ => {
                            return Err(CodegenErr::Err(format!(
                                "Unhandled operator for {biggest_var_type}: {op:?}"
                            )))
                        }
                    }
                } else if biggest_var_type.is_float() {
                    match op {
                        Operator::Add => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().fadd(accumulator, *val);
                            }
                        }
                        Operator::Sub => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().fsub(accumulator, *val);
                            }
                        }
                        Operator::Mul => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().fmul(accumulator, *val);
                            }
                        }
                        Operator::Div => {
                            // DIV 0
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().fdiv(accumulator, *val);
                            }
                        }
                        Operator::Mod => {
                            for (_, val) in operand.values.iter() {
                                accumulator = builder.ins().srem(accumulator, *val);
                            }
                        }
                        _ => {
                            return Err(CodegenErr::Err(format!(
                                "Unhandled operator for {biggest_var_type}: {op:?}"
                            )))
                        }
                    }
                } else {
                    return Err(CodegenErr::Err(format!(
                        "Unhandled type {biggest_var_type} for operation: {op:?}"
                    )));
                }

                Ok(CodegenResult::from((biggest_var_type, accumulator)))
            }
            Expr::Define((), ident, value) => match value.as_ref() {
                Expr::Number(_) => self.define_var(builder, &ident.name, value),
                Expr::List((), _, ()) => self.define_var(builder, &ident.name, value),
                _ => {
                    let ident_name = &ident.name;
                    Err(CodegenErr::Err(
                        format!("Unable to define {ident_name} in func {func_name:?}: {expr:?}")
                            .to_string(),
                    ))
                }
            },
            _ => Err(CodegenErr::Err(
                format!("Unhandled expression in func {func_name:?}: {expr:?}").to_string(),
            )),
        }
    }

    fn codegen(&mut self, expr: &Expr) -> Result<CodegenResult, CodegenErr> {
        // Entrypoint only allowed to set up new function first
        match expr {
            Expr::List((), items, ()) => {
                let mut results = Vec::new();
                for item in items.iter() {
                    let res = self.codegen(item)?;
                    for v in res.values {
                        results.push(v);
                    }
                }
                Ok(CodegenResult::from(results))
            }
            Expr::Define((), ident, value) => {
                // TODO: Determine return type
                self.define_func(ident.name.to_string(), None, value.as_ref())?;
                Ok(CodegenResult::default())
            }
            _ => Err(CodegenErr::Err(
                format!("Expected function declaration, got: {expr:?}").to_string(),
            )),
        }
    }
}

pub fn compile(expr: &Expr, output: PathBuf) -> Result<(), CodegenErr> {
    let mut flag_builder = settings::builder();
    flag_builder
        .enable("is_pic")
        .map_err(|e| CodegenErr::InitErr(e.to_string()))?;

    let flags = settings::Flags::new(flag_builder);

    let mut compiler = Compiler::new("entry", flags)?;

    compiler.codegen(expr)?;

    let obj = compiler.module.finish();
    let mut file = std::fs::File::create(output).map_err(|e| CodegenErr::IoErr(e.to_string()))?;

    obj.object
        .write_stream(&mut file)
        .map_err(|e| CodegenErr::IoErr(e.to_string()))?;

    Ok(())
}

fn numeric_to_value(builder: &mut FunctionBuilder, numeric: &Numeric) -> Option<(Type, Value)> {
    let raw = numeric.get_raw();

    let i_val = raw.parse::<i64>();
    if i_val.is_ok() {
        return Some((types::I64, builder.ins().iconst(types::I64, i_val.unwrap())));
    }

    let f_val = raw.parse::<f32>();
    if f_val.is_ok() {
        return Some((types::F32, builder.ins().f32const(f_val.unwrap())));
    }

    let f_val = raw.parse::<f64>();
    if f_val.is_ok() {
        return Some((types::F64, builder.ins().f64const(f_val.unwrap())));
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_codegen_add_is_ok() {
        let res = codegen(&Expr::Define(
            (),
            Identifier::from("test"),
            Box::new(Expr::List(
                (),
                vec![Expr::Operation(
                    Operator::Add,
                    Box::new(Expr::List(
                        (),
                        vec![
                            Expr::Number(Numeric::from(3)),
                            Expr::Number(Numeric::from(5)),
                        ],
                        (),
                    )),
                )],
                (),
            )),
        ));
        match res {
            Ok(_) => (),
            Err(ref err) => println!("{err:?}"),
        }

        assert_eq!(res.is_ok(), true);
    }
}
