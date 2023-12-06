use std::{collections::HashMap, path::PathBuf};

use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        verifier::VerifierErrors,
        verify_function,
    },
    prelude::{settings::Flags, *},
};
use cranelift_module::{DataId, FuncId, Linkage, Module, ModuleError};
use cranelift_object::{ObjectBuilder, ObjectModule};
use parser::*;

#[derive(Debug)]
pub enum CompileErr {
    Err(String),
    VerifierErr(VerifierErrors),
    InitErr(String),
    ModuleErr(ModuleError),
    IoErr(String),
    UndefinedIdentifier(String),
    UndefinedType(Definition),
    InvalidOperation(String),
}

pub struct Compiler {
    module: ObjectModule,
    flags: Flags,
    definitions: HashMap<String, Definition>,
    entity_count: usize,
    debug: bool,
}

#[derive(Debug, Clone)]
pub struct Definition {
    val_type: Option<Type>,
    value: DefinedValue,
}

impl Definition {
    pub fn new(val_type: Option<Type>, value: DefinedValue) -> Self {
        Self { val_type, value }
    }

    pub fn combine(definitions: Vec<Definition>) -> Self {
        Definition::from(definitions)
    }

    // Returns all values defined recursively, if a defined value is not a Value, returns None
    fn get_values_recursive(value: &DefinedValue) -> Option<Vec<Value>> {
        match value {
            DefinedValue::Values(values) => {
                let all_values: Vec<Value> = values
                    .iter()
                    .map(|v| Definition::get_values_recursive(v))
                    .fuse()
                    .flatten()
                    .flatten()
                    .collect();

                if all_values.len() != values.len() {
                    None
                } else {
                    Some(all_values)
                }
            }
            DefinedValue::Value(value) => Some(vec![*value]),
            _ => None,
        }
    }

    pub fn get_values(&self) -> Option<Vec<Value>> {
        Definition::get_values_recursive(&self.value)
    }
}

#[derive(Debug, Clone)]
pub enum DefinedValue {
    None,
    Value(Value),
    Values(Vec<DefinedValue>),
    Variable(Variable),
    Function(FuncId),
    Data(DataId),
}

impl Default for Definition {
    fn default() -> Self {
        Self::new(None, DefinedValue::None)
    }
}
impl From<(Type, Value)> for Definition {
    fn from(value: (Type, Value)) -> Self {
        Self::new(Some(value.0), DefinedValue::Value(value.1))
    }
}
impl From<(Type, FuncId)> for Definition {
    fn from(value: (Type, FuncId)) -> Self {
        Self::new(Some(value.0), DefinedValue::Function(value.1))
    }
}
impl From<(Type, DataId)> for Definition {
    fn from(value: (Type, DataId)) -> Self {
        Self::new(Some(value.0), DefinedValue::Data(value.1))
    }
}
impl From<(Type, Variable)> for Definition {
    fn from(value: (Type, Variable)) -> Self {
        Self::new(Some(value.0), DefinedValue::Variable(value.1))
    }
}
impl From<Vec<DefinedValue>> for Definition {
    fn from(values: Vec<DefinedValue>) -> Self {
        Self::new(None, DefinedValue::Values(values))
    }
}
impl From<Vec<Definition>> for Definition {
    fn from(values: Vec<Definition>) -> Self {
        let mut value = Vec::new();
        for v in values.iter() {
            value.push(v.value.clone());
        }

        Self::from(value)
    }
}

impl Compiler {
    pub fn new(name: &str, flags: Flags) -> Result<Self, CompileErr> {
        let isa_builder =
            cranelift_native::builder().map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let isa = isa_builder
            .finish(flags.clone())
            .map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let obj_builder = ObjectBuilder::new(isa, name, cranelift_module::default_libcall_names())
            .map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let module = ObjectModule::new(obj_builder);

        Ok(Self {
            flags,
            module,
            definitions: HashMap::new(),
            entity_count: 0,
            debug: true,
        })
    }

    fn define_var(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        value: &Expr,
    ) -> Result<Definition, CompileErr> {
        if self.definitions.contains_key(name) {
            return Err(CompileErr::Err(
                format!("Identifier already defined: {name}").to_string(),
            ));
        }

        match value {
            Expr::Number(numeric) => {
                let var_val = numeric_to_value(builder, numeric).unwrap();
                let var = Variable::new(self.get_next_entity());
                builder.declare_var(var, var_val.0);
                builder.def_var(var, var_val.1);
                self.definitions
                    .insert(name.to_string(), Definition::from((var_val.0, var)));
            }
            _ => {
                return Err(CompileErr::Err(
                    format!("Unhandled value expression: {value:?}").to_string(),
                ))
            }
        }

        Ok(Definition::default())
    }

    fn define_func(
        &mut self,
        name: String,
        func_type: Option<Type>,
        body: &Expr,
    ) -> Result<(), CompileErr> {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        let mut func_id = self
            .module
            .declare_function(&name, Linkage::Export, &sig)
            .map_err(|e| CompileErr::ModuleErr(e))?;

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

        verify_function(&func, &self.flags).map_err(|e| CompileErr::VerifierErr(e))?;

        let mut ctx = codegen::Context::for_function(func);
        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CompileErr::ModuleErr(e))?;

        Ok(())
    }

    fn get_next_entity(&mut self) -> usize {
        let e = self.entity_count;
        self.entity_count += 1;
        e
    }

    fn codegen_op(
        &mut self,
        builder: &mut FunctionBuilder,
        op: &Operator,
        operand_expr: &Vec<Expr>,
    ) -> Result<Definition, CompileErr> {
        let defined_operands: Vec<Definition> = operand_expr
            .iter()
            .map(|operand| self.codegen_func(builder, operand))
            .flatten()
            .collect();

        let operand = Definition::combine(defined_operands);
        if operand.val_type.is_none() {
            return Err(CompileErr::UndefinedType(operand));
        }
        let accumulator_type = operand.val_type.unwrap();

        let values = operand.get_values();
        if values.is_none() {
            return Err(CompileErr::InvalidOperation(format!("{operand_expr:?}")));
        }
        let values = values.unwrap();

        let mut accumulator = builder.ins().iconst(accumulator_type, 0);

        if accumulator_type.is_int() {
            match op {
                Operator::Add => {
                    for val in values.iter() {
                        accumulator = builder.ins().iadd(accumulator, *val);
                    }
                }
                Operator::Sub => {
                    for val in values.iter() {
                        accumulator = builder.ins().isub(accumulator, *val);
                    }
                }
                Operator::Mul => {
                    for val in values.iter() {
                        accumulator = builder.ins().imul(accumulator, *val);
                    }
                }
                Operator::Div => {
                    // DIV 0
                    for val in values.iter() {
                        accumulator = builder.ins().sdiv(accumulator, *val);
                    }
                }
                Operator::Mod => {
                    for val in values.iter() {
                        accumulator = builder.ins().srem(accumulator, *val);
                    }
                }
                _ => {
                    return Err(CompileErr::Err(format!(
                        "Unhandled operator for {accumulator_type}: {op:?}"
                    )))
                }
            }
        } else if accumulator_type.is_float() {
            match op {
                Operator::Add => {
                    for val in values.iter() {
                        accumulator = builder.ins().fadd(accumulator, *val);
                    }
                }
                Operator::Sub => {
                    for val in values.iter() {
                        accumulator = builder.ins().fsub(accumulator, *val);
                    }
                }
                Operator::Mul => {
                    for val in values.iter() {
                        accumulator = builder.ins().fmul(accumulator, *val);
                    }
                }
                Operator::Div => {
                    // DIV 0
                    for val in values.iter() {
                        accumulator = builder.ins().fdiv(accumulator, *val);
                    }
                }
                Operator::Mod => {
                    for val in values.iter() {
                        accumulator = builder.ins().srem(accumulator, *val);
                    }
                }
                _ => {
                    return Err(CompileErr::Err(format!(
                        "Unhandled operator for {accumulator_type}: {op:?}"
                    )))
                }
            }
        } else {
            return Err(CompileErr::Err(format!(
                "Unhandled type {accumulator_type} for operation: {op:?}"
            )));
        }

        Ok(Definition::from((accumulator_type, accumulator)))
    }

    fn codegen_func(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
    ) -> Result<Definition, CompileErr> {
        let func_name = builder.func.name.get_user();
        match expr {
            Expr::Identifier(identifier) => {
                if let Some(defined) = self.definitions.get(&identifier.name) {
                    Ok(defined.clone())
                } else {
                    Err(CompileErr::UndefinedIdentifier(identifier.name.to_string()))
                }
            }
            Expr::Number(numeric) => {
                if let Some(value) = numeric_to_value(builder, numeric) {
                    Ok(Definition::from(value))
                } else {
                    Err(CompileErr::Err(format!(
                        "Unable to convert numeric to value: {numeric:?}"
                    )))
                }
            }
            Expr::List((), items, ()) => {
                let results: Vec<Definition> = items
                    .iter()
                    .map(|item| self.codegen_func(builder, item))
                    .flatten()
                    .collect();

                Ok(Definition::from(results))
            }
            Expr::Operation((), op, operand, ()) => self.codegen_op(builder, op, operand),
            Expr::Define((), ident, value) => match value.as_ref() {
                Expr::Number(_) => self.define_var(builder, &ident.name, value),
                Expr::List((), _, ()) => self.define_var(builder, &ident.name, value),
                _ => {
                    let ident_name = &ident.name;
                    Err(CompileErr::Err(
                        format!("Unable to define {ident_name} in func {func_name:?}: {expr:?}")
                            .to_string(),
                    ))
                }
            },
            _ => Err(CompileErr::Err(
                format!("Unhandled expression in func {func_name:?}: {expr:?}").to_string(),
            )),
        }
    }

    fn codegen(&mut self, expr: &Expr) -> Result<Definition, CompileErr> {
        match expr {
            Expr::List((), items, ()) => {
                let mut results = Vec::new();
                for item in items.iter() {
                    let res = self.codegen(item)?;
                    results.push(res);
                }
                Ok(Definition::from(results))
            }
            Expr::Define((), ident, value) => {
                // TODO: Determine return type
                self.define_func(ident.name.to_string(), None, value.as_ref())?;
                Ok(Definition::default())
            }
            _ => Err(CompileErr::Err(
                format!("Expected function declaration, got: {expr:?}").to_string(),
            )),
        }
    }
}

pub fn compile(expr: &Expr, output: PathBuf) -> Result<(), CompileErr> {
    let mut flag_builder = settings::builder();
    flag_builder
        .enable("is_pic")
        .map_err(|e| CompileErr::InitErr(e.to_string()))?;

    let flags = settings::Flags::new(flag_builder);

    let mut compiler = Compiler::new("entry", flags)?;

    compiler.codegen(expr)?;

    let obj = compiler.module.finish();
    let mut file = std::fs::File::create(output).map_err(|e| CompileErr::IoErr(e.to_string()))?;

    obj.object
        .write_stream(&mut file)
        .map_err(|e| CompileErr::IoErr(e.to_string()))?;

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
