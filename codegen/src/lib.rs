use std::io::Write as _;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use cranelift::{
    codegen::{
        ir::{immediates::Offset32, Function, UserFuncName},
        verifier::VerifierErrors,
        verify_function, Context,
    },
    prelude::{settings::Flags, *},
};
use cranelift_module::{DataDescription, Linkage, Module, ModuleError};
use cranelift_object::{ObjectBuilder, ObjectModule};
use parser::*;

mod definitions;
use definitions::*;

#[derive(Debug)]
pub enum CompileErr {
    Err(String),
    VerifierErr(VerifierErrors),
    InitErr(String),
    ModuleErr(ModuleError),
    IoErr(String),
    UndefinedIdentifier(String),
    UndefinedType(Definition),
    UndefinedValue(Definition),
    IdentifierExists(String),
    DefinitionExists(Definition),
    InvalidOperation(String),
    TypeMismatch(String),
    VariableErr(String),
    IndexOutOfRange(String),
}

pub struct Compiler {
    flags: Flags,
    module: ObjectModule,
    definitions: HashMap<Symbol, Definition>,
    definition_count: usize,
    symbols: HashMap<String, Symbol>,
    sig_count: u32,
    debug: bool,
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
            definition_count: 0,
            definitions: HashMap::new(),
            symbols: HashMap::new(),
            sig_count: 0,
            debug: true,
        })
    }

    fn get_def(&self, name: &str) -> Option<&Definition> {
        if let Some(index) = self.symbols.get(name) {
            if let Some(def) = self.definitions.get(index) {
                return Some(def);
            }
        }

        None
    }

    fn register_def(&mut self, def: &Definition) -> Result<(), CompileErr> {
        if let Some(existing) = self.definitions.insert(def.symbol(), def.clone()) {
            Err(CompileErr::DefinitionExists(existing.clone()))
        } else {
            Ok(())
        }
    }

    fn register_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), CompileErr> {
        if self.symbols.contains_key(name) {
            Err(CompileErr::IdentifierExists(name.into()))
        } else {
            self.symbols.insert(name.to_string(), symbol);

            Ok(())
        }
    }

    fn is_defined(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    fn get_next_def_count(&mut self) -> usize {
        let def_count = self.definition_count;
        self.definition_count += 1;
        def_count
    }

    fn get_next_sig_count(&mut self) -> u32 {
        let sig_count = self.sig_count;
        self.sig_count += 1;
        sig_count
    }

    fn define_var(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        value: &Expr,
    ) -> Result<Definition, CompileErr> {
        if self.is_defined(name) {
            return Err(CompileErr::Err(
                format!("Identifier already defined: {name}").to_string(),
            ));
        }

        match value {
            Expr::Identifier(identifier) => {
                let ident_def = self.get_def(&identifier.name).cloned();

                if let Some(ident_def) = ident_def {
                    self.register_symbol(&identifier.name, ident_def.symbol())?;
                    Ok(ident_def)
                } else {
                    return Err(CompileErr::UndefinedIdentifier(identifier.name.clone()));
                }
            }
            Expr::Number(numeric) => {
                let var_val = numeric_to_value(builder, numeric).unwrap();
                let var = Variable::new(self.get_next_def_count());
                builder.declare_var(var, var_val.0);
                builder.def_var(var, var_val.1);

                let def = Definition::new(var_val.0, DefinedValue::Variable(var));
                self.register_def(&def)?;
                self.register_symbol(name, def.symbol())?;

                Ok(def)
            }
            _ => Err(CompileErr::Err(
                format!("Unhandled variable value type: {value:?}").to_string(),
            )),
        }
    }

    fn define_func_expr(&mut self, define_expr: &DefineFunc) -> Result<Definition, CompileErr> {
        match define_expr {
            DefineFunc {
                ty,
                identifier,
                params,
                body,
                ..
            } => {
                let returns = vec![return_type_to_ir(ty)];

                self.define_func(
                    &identifier.name,
                    &returns,
                    &params,
                    body.as_ref(),
                    Linkage::Export,
                )
            }
        }
    }

    fn define_func(
        &mut self,
        name: &str,
        returns: &Vec<Type>,
        params: &Vec<Param>,
        body: &Expr,
        linkage: Linkage,
    ) -> Result<Definition, CompileErr> {
        let mut sig = Signature::new(self.module.isa().default_call_conv());

        for p in params {
            let pty_ir = return_type_to_ir(&p.ty);
            sig.params.push(AbiParam::new(pty_ir));
        }
        for r in returns {
            sig.returns.push(AbiParam::new(*r));
        }

        let func_id = self
            .module
            .declare_function(&name, linkage, &sig)
            .map_err(|e| CompileErr::ModuleErr(e))?;

        let sig_index = self.get_next_sig_count();
        let mut func = Function::with_name_signature(UserFuncName::user(0, sig_index), sig);
        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_context);
        let res: Definition;

        {
            let entry_block = builder.create_block();

            for p in params {
                let ir_ty = return_type_to_ir(&p.ty);
                let param_val = builder.append_block_param(entry_block, ir_ty);
                let param_def = Definition::new(ir_ty, DefinedValue::Value(param_val));

                self.register_def(&param_def)?;
                self.register_symbol(&p.identifier.name, param_def.symbol())?;
            }

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            res = self.codegen_func(&mut builder, body)?;
            self.register_def(&res)?;
            self.register_symbol(name, res.symbol())?;

            // Determine how to return the function result
            if res.val_type.is_invalid() {
                builder.ins().return_(&[]);
            } else {
                let resolved_values = self.resolve_values(&mut builder, &body, &res)?;

                if returns.len() != resolved_values.len() {
                    return Err(CompileErr::TypeMismatch(format!(
                        "{name}: expected {} return types, got {}",
                        returns.len(),
                        resolved_values.len()
                    )));
                }
                for i in 0..resolved_values.len() {
                    if returns[i] != resolved_values[i].0 {
                        let resolved_types: Vec<&Type> =
                            resolved_values.iter().map(|(rt, _)| rt).collect();
                        return Err(CompileErr::TypeMismatch(format!(
                            "{name}: expected {returns:?} but got {resolved_types:?}"
                        )));
                    }
                }

                let resolved_values: Vec<Value> =
                    resolved_values.iter().map(|(_, rv)| *rv).collect();
                builder.ins().return_(&resolved_values);
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

        Ok(res)
    }

    fn pointer_type(&self) -> Type {
        self.module.target_config().pointer_type()
    }

    fn resolve_value(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        defined: &Definition,
        stack_index: Option<usize>,
    ) -> Result<Option<(Type, Value)>, CompileErr> {
        match &defined.value {
            DefinedValue::None => Ok(None),
            DefinedValue::Value(value) => Ok(Some((defined.val_type, *value))),
            DefinedValue::Variable(var) => builder
                .try_use_var(*var)
                .map(|v| Some((defined.val_type, v)))
                .map_err(|e| CompileErr::VariableErr(e.to_string())),
            DefinedValue::Data(data_id) => {
                let pointer = self.pointer_type();
                let data = self.module.declare_data_in_func(*data_id, builder.func);
                let value = builder.ins().global_value(pointer, data);
                Ok(Some((defined.val_type, value)))
            }
            DefinedValue::Function(func_id) => {
                let pointer = self.pointer_type();
                let func_ref = self.module.declare_func_in_func(*func_id, builder.func);
                let value = builder.ins().func_addr(pointer, func_ref);
                Ok(Some((defined.val_type, value)))
            }
            DefinedValue::Stack { slot, items } => {
                let pointer = self.pointer_type();

                if let Some(stack_index) = stack_index {
                    if let Some(item) = items.get(stack_index) {
                        let stack_value = builder.ins().stack_load(item.0, *slot, item.1);
                        Ok(Some((item.0, stack_value)))
                    } else {
                        Err(CompileErr::IndexOutOfRange(format!("{expr:?}").to_string()))
                    }
                } else {
                    // Resolve to ref if no stack index given
                    let stack_ref = builder.ins().stack_addr(pointer, *slot, Offset32::new(0));
                    Ok(Some((defined.val_type, stack_ref)))
                }
            }
        }
    }

    fn resolve_values(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        defined: &Definition,
    ) -> Result<Vec<(Type, Value)>, CompileErr> {
        let mut values = Vec::new();
        match &defined.value {
            DefinedValue::Stack { slot, items } => {
                for (item_ty, offset) in items {
                    let v = builder.ins().stack_load(*item_ty, *slot, *offset);
                    values.push((*item_ty, v));
                }
            }
            _ => {
                if let Some(v) = self.resolve_value(builder, expr, defined, None)? {
                    values.push(v);
                }
            }
        }

        Ok(values)
    }

    fn codegen_op_components(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        op: &Operator,
        operands: Vec<Definition>,
    ) -> Result<(Type, Vec<Value>), CompileErr> {
        let mut accumulator_values: Vec<Value> = Vec::new();
        let mut accumulator_type: Option<Type> = None;

        for defined_op in operands.iter() {
            if let Some(op_res) = self.resolve_value(builder, expr, &defined_op, None)? {
                if let Some(acc_type) = accumulator_type {
                    if let Some(new_acc_type) =
                        get_max_operable_type(op, acc_type, defined_op.val_type)
                    {
                        accumulator_values.push(op_res.1);
                        accumulator_type = Some(new_acc_type);
                    } else {
                        let defined_op_type = defined_op.val_type;
                        return Err(CompileErr::InvalidOperation(
                            format!("Can't {op:?} for {accumulator_type:?} and {defined_op_type}")
                                .to_string(),
                        ));
                    }
                } else {
                    accumulator_values.push(op_res.1);
                    accumulator_type = Some(defined_op.val_type);
                }
            } else {
                return Err(CompileErr::InvalidOperation(
                    format!("Can't {op:?}: {expr:?}").to_string(),
                ));
            }
        }

        match accumulator_type {
            Some(acc_type) => Ok((acc_type, accumulator_values)),
            None => Err(CompileErr::InvalidOperation(
                format!("{operands:?}").to_string(),
            )),
        }
    }

    fn codegen_op(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        op: &Operator,
        operand_expr: &Vec<Expr>,
    ) -> Result<Definition, CompileErr> {
        let defined_operands: Vec<Definition> = operand_expr
            .iter()
            .map(|operand| self.codegen_func(builder, operand))
            .flatten()
            .collect();

        println!("op: {op:?}, operands: {defined_operands:?}");

        let (accumulator_type, values) =
            self.codegen_op_components(builder, expr, op, defined_operands)?;

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
                if let Some(defined) = self.get_def(&identifier.name) {
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

                self.codegen_stack(builder, expr, results.iter().collect())
            }
            Expr::Operation((), op, operand, ()) => self.codegen_op(builder, expr, op, operand),
            Expr::Function(def_expr) => self.define_func_expr(def_expr),
            Expr::Variable(def_var) => {
                self.define_var(builder, &def_var.identifier.name, def_var.body.as_ref())
            }
            _ => Err(CompileErr::Err(
                format!("Unhandled expression in func {func_name:?}: {expr:?}").to_string(),
            )),
        }
    }

    fn codegen_stack(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        stack_items: Vec<&Definition>,
    ) -> Result<Definition, CompileErr> {
        let data_bits: u32 = stack_items.iter().map(|i| i.bits()).sum();
        let data_size = data_bits + 7 / 8;
        let data = StackSlotData::new(StackSlotKind::ExplicitSlot, data_size);
        let slot = builder.create_sized_stack_slot(data);

        let mut offset: i32 = 0;
        let mut stored_items: Vec<(Type, Offset32)> = Vec::new();
        for item in stack_items {
            if let Some((item_ty, item_value)) = self.resolve_value(builder, expr, &item, None)? {
                builder
                    .ins()
                    .stack_store(item_value, slot, Offset32::new(offset));

                stored_items.push((item.val_type, Offset32::new(offset)));

                offset += item.bytes() as i32;
            } else {
                return Err(CompileErr::UndefinedValue(item.clone()));
            }
        }

        let def = Definition::new(
            self.pointer_type(),
            DefinedValue::Stack {
                slot,
                items: stored_items,
            },
        );

        Ok(def)
    }

    fn codegen(&mut self, expr: &Expr) -> Result<Definition, CompileErr> {
        match expr {
            Expr::Function(def_expr) => {
                return self.define_func_expr(def_expr);
            }
            _ => (),
        }

        Err(CompileErr::Err(
            format!("Expected function declaration like (i:main ()), got: {expr:?}").to_string(),
        ))
    }
}

pub fn compile(
    expr: &Expr,
    output: PathBuf,
    execute: bool,
) -> Result<Option<Compiler>, CompileErr> {
    let mut flag_builder = settings::builder();

    flag_builder
        .enable("is_pic")
        .map_err(|e| CompileErr::InitErr(e.to_string()))?;

    let flags = settings::Flags::new(flag_builder);

    let mut compiler = Compiler::new("entry", flags)?;

    compiler.codegen(expr)?;

    if execute {
        let obj = compiler.module.finish();

        // assemble
        let obj_data = obj.emit().map_err(|e| CompileErr::IoErr(e.to_string()))?;

        let mut object_path = output.clone();
        object_path.set_extension("o");

        let exec_path = output;
        // TODO: other OS exec pathing

        let mut file = std::fs::File::create(object_path.clone())
            .map_err(|e| CompileErr::IoErr(e.to_string()))?;

        file.write_all(&obj_data)
            .map_err(|e| CompileErr::IoErr(e.to_string()))?;

        link(&object_path, &exec_path).map_err(|e| CompileErr::IoErr(e.to_string()))?;

        Ok(None)
    } else {
        Ok(Some(compiler))
    }
}

fn link(obj_file: &Path, output: &Path) -> Result<(), std::io::Error> {
    use std::io::{Error, ErrorKind};
    use std::process::Command;

    let status = Command::new("cc")
        .args(&[&obj_file, Path::new("-o"), output])
        .status()?;

    if !status.success() {
        Err(Error::new(ErrorKind::Other, "unable to link using cc"))
    } else {
        Ok(())
    }
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

fn get_max_operable_type(op: &Operator, t1: Type, t2: Type) -> Option<Type> {
    match op {
        Operator::And | Operator::Or => {
            return None;
        }
        _ => (),
    }

    if t1 == t2 {
        Some(t1)
    } else if (t1.is_int() && t2.is_int()) || (t1.is_float() && t2.is_float()) {
        if t1.bits() > t2.bits() {
            Some(t1)
        } else {
            Some(t2)
        }
    } else {
        None
    }
}

fn return_type_to_ir(ret_type: &ReturnType) -> Type {
    match ret_type {
        ReturnType::Integer => types::I64,
        ReturnType::Float => types::F64,
    }
}
