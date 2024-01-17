use std::io::Write as _;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use cranelift_codegen::ir::{
    condcodes, AbiParam, InstBuilder, Signature, StackSlotData, StackSlotKind,
};
use cranelift_codegen::settings::Flags;
use cranelift_codegen::CompileError;
use cranelift_codegen::{
    entity::EntityRef,
    ir::types,
    ir::{immediates::Offset32, Function, UserFuncName},
    ir::{Type, Value},
    settings,
    settings::*,
    verifier::VerifierErrors,
    verify_function,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncOrDataId, Linkage, Module, ModuleError};
use parser::*;

mod definitions;
use definitions::*;

mod operation;
use operation::*;

const ENTRY_FUNCTION_NAME: &'static str = "main";

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
    InvalidCondition(String),
    TypeMismatch(String),
    VariableErr(String),
    IndexOutOfRange(String),
}

pub struct Compiler {
    flags: Flags,
    module: JITModule,
    definitions: HashMap<Symbol, Definition>,
    definition_count: usize,
    symbols: HashMap<String, Symbol>,
    sig_count: u32,
    debug: bool,
    pointer_ty: Type,
}

impl Compiler {
    pub fn new(flags: Flags) -> Result<Self, CompileErr> {
        let isa_builder =
            cranelift_native::builder().map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let isa = isa_builder
            .finish(flags.clone())
            .map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let obj_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(obj_builder);
        let pointer_ty = module.target_config().pointer_type();

        Ok(Self {
            flags,
            module,
            definition_count: 0,
            definitions: HashMap::new(),
            symbols: HashMap::new(),
            sig_count: 0,
            debug: true,
            pointer_ty,
        })
    }

    fn def_by_name(&self, name: &str) -> Option<&Definition> {
        if let Some(index) = self.symbols.get(name) {
            if let Some(def) = self.definitions.get(index) {
                println!("found {name} def: {def:?}");
                return Some(def);
            }
        }

        None
    }

    fn try_register_def(&mut self, def: &Definition) -> bool {
        println!("registered: {def:?}");
        return self.definitions.insert(def.symbol(), def.clone()).is_none();
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

    fn return_type_to_ir(&self, ret_type: &ReturnType) -> Type {
        match ret_type {
            ReturnType::Integer => types::I64,
            ReturnType::Float => types::F64,
            ReturnType::String => self.pointer_ty,
            _ => self.pointer_ty,
        }
    }
    fn define_var(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        value: &Expr,
    ) -> Result<Definition, CompileErr> {
        if self.is_defined(name) {
            return Err(CompileErr::Err(format!(
                "Identifier already defined: {name}"
            )));
        }

        match value {
            Expr::Identifier(identifier) => {
                let ident_def = self.def_by_name(&identifier.name).cloned();

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
                self.try_register_def(&def);
                self.register_symbol(name, def.symbol())?;

                Ok(def)
            }
            _ => Err(CompileErr::Err(format!(
                "Unhandled variable value type: {value:?}"
            ))),
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
                let returns = vec![self.return_type_to_ir(ty)];

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
            let pty_ir = self.return_type_to_ir(&p.ty);
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
                let ir_ty = self.return_type_to_ir(&p.ty);
                let param_val = builder.append_block_param(entry_block, ir_ty);
                let param_def = Definition::new(ir_ty, DefinedValue::Value(param_val));

                self.try_register_def(&param_def);
                self.register_symbol(&p.identifier.name, param_def.symbol())?;
            }

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            res = self.codegen_func(&mut builder, body)?;
            self.try_register_def(&res);
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
                let mut return_values: Vec<Value> = Vec::with_capacity(returns.len());
                for i in 0..resolved_values.len() {
                    // cast to fit return type?
                    let (rt, rv) = resolved_values[i];
                    return_values.push(cast_value(&mut builder, rt, returns[i], rv));
                }

                builder.ins().return_(&return_values);
            }

            builder.seal_all_blocks();
            if self.debug {
                println!("{}", builder.func);
            }
            builder.finalize();
        }

        verify_function(&func, &self.flags).map_err(|e| CompileErr::VerifierErr(e))?;

        let mut ctx = cranelift_codegen::Context::for_function(func);
        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CompileErr::ModuleErr(e))?;

        Ok(res)
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
                let pointer = self.pointer_ty;
                let data = self.module.declare_data_in_func(*data_id, builder.func);
                let value = builder.ins().global_value(pointer, data);
                Ok(Some((defined.val_type, value)))
            }
            DefinedValue::Function(func_id) => {
                let pointer = self.pointer_ty;
                let func_ref = self.module.declare_func_in_func(*func_id, builder.func);
                let value = builder.ins().func_addr(pointer, func_ref);
                Ok(Some((defined.val_type, value)))
            }
            DefinedValue::Stack { slot, items } => {
                let pointer = self.pointer_ty;

                if let Some(stack_index) = stack_index {
                    if let Some(item) = items.get(stack_index) {
                        let stack_value = builder.ins().stack_load(item.0, *slot, item.1);
                        Ok(Some((item.0, stack_value)))
                    } else {
                        Err(CompileErr::IndexOutOfRange(format!("{expr:?}")))
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
        let mut accumulator_values: Vec<(Type, Value)> = Vec::new();
        let mut accumulator_type: Option<Type> = None;

        for defined_op in operands.iter() {
            let op_res = self.resolve_values(builder, expr, &defined_op)?;
            for pair in op_res {
                accumulator_values.push(pair);
            }
        }

        cast_to_largest(builder, expr, accumulator_values)
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
            .fuse()
            .flatten()
            .collect();

        if defined_operands.len() != operand_expr.len() {
            return Err(CompileErr::InvalidOperation(format!(
                "unresolbed values in: {expr:?}"
            )));
        }

        println!("op: {op:?}, operands: {defined_operands:?}");

        let (accumulator_type, values) =
            self.codegen_op_components(builder, expr, op, defined_operands)?;

        let mut accumulator = *values.iter().nth(0).unwrap();
        let others = values.iter().skip(1);

        if accumulator_type.is_int() {
            accumulator = IOpAccumulator::from(op).value_func(builder, accumulator, others)?;
        } else if accumulator_type.is_float() {
            accumulator = FOpAccumulator::from(op).value_func(builder, accumulator, others)?;
        } else {
            return Err(CompileErr::InvalidOperation(format!(
                "{op:?} unable to operate on {accumulator_type}"
            )));
        }

        Ok(Definition::from((accumulator_type, accumulator)))
    }

    fn codegen_func_vec(
        &mut self,
        builder: &mut FunctionBuilder,
        expr_vec: &Vec<&Expr>,
    ) -> Result<Vec<Definition>, CompileErr> {
        let results: Vec<Definition> = expr_vec
            .iter()
            .map(|item| self.codegen_func(builder, item))
            .fuse()
            .flatten()
            .collect();

        Ok(results)
    }

    fn codegen_func(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
    ) -> Result<Definition, CompileErr> {
        let func_name = builder.func.name.get_user();
        match expr {
            Expr::Identifier(identifier) => {
                if let Some(defined) = self.def_by_name(&identifier.name) {
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
            Expr::Condition((), comparator, items, ()) => {
                let results = self.codegen_func_vec(builder, &items.iter().collect())?;
                self.codegen_cond(builder, expr, comparator, results.iter().collect())
            }
            Expr::List((), items, ()) => {
                let results = self.codegen_func_vec(builder, &items.iter().collect())?;
                self.codegen_stack(builder, expr, results.iter().collect())
            }
            Expr::Operation((), op, operand, ()) => self.codegen_op(builder, expr, op, operand),
            Expr::Function(def_expr) => self.define_func_expr(def_expr),
            Expr::Variable(def_var) => {
                self.define_var(builder, &def_var.identifier.name, def_var.body.as_ref())
            }
            _ => Err(CompileErr::Err(format!(
                "Unhandled expression in func {func_name:?}: {expr:?}"
            ))),
        }
    }

    fn codegen_cond(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        comparator: &Comparator,
        operands: Vec<&Definition>,
    ) -> Result<Definition, CompileErr> {
        let mut operand_ir = Vec::new();
        for operand in operands {
            if let Some(pair) = self.resolve_value(builder, expr, &operand, None)? {
                operand_ir.push(pair);
            }
        }

        let (ty, operand_values) = cast_to_largest(builder, expr, operand_ir)?;
        let mut cmp = *operand_values.first().unwrap();

        if ty.is_invalid() {
            return Err(CompileErr::InvalidCondition(format!(
                "condition {comparator:?} for {expr:?} has invalid operands"
            )));
        } else if ty.is_int() {
            let cc = condcode_int(comparator);
            for value in operand_values.iter().skip(1) {
                cmp = builder.ins().icmp(cc, cmp, *value);
            }
        } else if ty.is_float() {
            let cc = condcode_float(comparator);
            for value in operand_values.iter().skip(1) {
                cmp = builder.ins().fcmp(cc, cmp, *value);
            }
        } else {
            return Err(CompileErr::InvalidCondition(format!(
                "unable to compare {comparator:?} for {expr:?}"
            )));
        }

        Ok(Definition::new(ty, DefinedValue::Value(cmp)))
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
            if let Some((_item_ty, item_value)) = self.resolve_value(builder, expr, &item, None)? {
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
            self.pointer_ty,
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
                if def_expr.identifier.name.to_lowercase() != ENTRY_FUNCTION_NAME {
                    Err(CompileErr::Err(format!(
                        "Expected function declaration for entry point \"{ENTRY_FUNCTION_NAME}\""
                    )))
                } else {
                    self.define_func_expr(def_expr)
                }
            }
            _ => Err(CompileErr::Err(format!(
                "Expected function declaration like (i:main ()), got: {expr:?}"
            ))),
        }
    }
}

pub fn compile(expr: &Expr, execute: bool) -> Result<Option<*const u8>, CompileErr> {
    let mut flag_builder = settings::builder();

    flag_builder
        .enable("is_pic")
        .map_err(|e| CompileErr::InitErr(e.to_string()))?;

    flag_builder
        .set("use_colocated_libcalls", "false")
        .map_err(|e| CompileErr::Err(e.to_string()))?;

    let flags = settings::Flags::new(flag_builder);

    let mut compiler = Compiler::new(flags)?;

    compiler.codegen(expr)?;

    if execute {
        // FOR AOT: let obj = compiler.module.finish();
        // FOR JIT:
        compiler
            .module
            .finalize_definitions()
            .map_err(|e| CompileErr::ModuleErr(e))?;

        // Find entrypoint
        let main_fn_id = match compiler.module.get_name(ENTRY_FUNCTION_NAME) {
            Some(FuncOrDataId::Func(func_id)) => Some(func_id),
            _ => None,
        };
        if main_fn_id.is_none() {
            return Err(CompileErr::Err(
                "Expected main function to be defined".into(),
            ));
        }

        let code = compiler
            .module
            .get_finalized_function(main_fn_id.unwrap().clone());

        // assemble
        /*let obj_data = obj.emit().map_err(|e| CompileErr::IoErr(e.to_string()))?;
        let mut object_path = output.clone();
        object_path.set_extension("o");

        let exec_path = output;
        let mut file = std::fs::File::create(object_path.clone())
            .map_err(|e| CompileErr::IoErr(e.to_string()))?;

        file.write_all(&obj_data)
            .map_err(|e| CompileErr::IoErr(e.to_string()))?;

        link(&object_path, &exec_path).map_err(|e| CompileErr::IoErr(e.to_string()))?;
        */

        Ok(Some(code))
    } else {
        Ok(None)
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

fn condcode_int(comparator: &Comparator) -> condcodes::IntCC {
    use condcodes::*;

    match comparator {
        Comparator::Eq => IntCC::Equal,
        Comparator::NotEq => IntCC::NotEqual,
        Comparator::Less => IntCC::SignedLessThan,
        Comparator::LessEq => IntCC::SignedLessThanOrEqual,
        Comparator::Greater => IntCC::SignedGreaterThan,
        Comparator::GreaterEq => IntCC::SignedGreaterThanOrEqual,
    }
}

fn condcode_float(comparator: &Comparator) -> condcodes::FloatCC {
    use condcodes::*;

    return match comparator {
        Comparator::Eq => FloatCC::Equal,
        Comparator::NotEq => FloatCC::NotEqual,
        Comparator::Less => FloatCC::LessThan,
        Comparator::LessEq => FloatCC::LessThanOrEqual,
        Comparator::Greater => FloatCC::GreaterThan,
        Comparator::GreaterEq => FloatCC::GreaterThanOrEqual,
    };
}

fn cast_value(builder: &mut FunctionBuilder, from: Type, to: Type, value: Value) -> Value {
    if from == to {
        return value;
    }
    match (from, to) {
        (integer, float) if integer.is_int() && float.is_float() => {
            builder.ins().fcvt_from_sint(to, value)
        }
        (float, integer) if float.is_float() && integer.is_int() => {
            builder.ins().fcvt_to_sint(to, value)
        }
        (integer, smaller_integer)
            if integer.is_int()
                && smaller_integer.is_int()
                && integer.lane_bits() > smaller_integer.lane_bits() =>
        {
            builder.ins().ireduce(smaller_integer, value)
        }
        (smaller_integer, integer)
            if integer.is_int()
                && smaller_integer.is_int()
                && integer.lane_bits() > smaller_integer.lane_bits() =>
        {
            builder.ins().sextend(integer, value)
        }
        (types::F32, types::F64) => builder.ins().fpromote(to, value),
        (types::F64, types::F32) => builder.ins().fdemote(to, value),
        _ => unreachable!("cast from {} to {}", from, to),
    }
}

fn cast_to_largest(
    builder: &mut FunctionBuilder,
    expr: &Expr,
    items: Vec<(Type, Value)>,
) -> Result<(Type, Vec<Value>), CompileErr> {
    if items.len() == 0 {
        return Ok((types::INVALID, Vec::with_capacity(0)));
    }

    let mut largest: Option<Type> = None;

    for (ty, _) in items.iter() {
        if ty.is_ref() || ty.is_special() {
            return Err(CompileErr::TypeMismatch(format!(
                "Unsupported type {ty} in expression: {expr:?}"
            )));
        }
        if let Some(mut largest) = &mut largest {
            if (largest.is_int() && ty.is_int()) || (largest.is_float() && ty.is_float()) {
                if ty.lane_bits() < largest.lane_bits() {
                    largest = *ty;
                }
            } else if largest.is_int() && ty.is_float() {
                largest = *ty;
            }
        } else {
            largest = Some(*ty);
        }
    }
    let largest = largest.unwrap();

    let mut results = Vec::new();
    for (item_ty, item_val) in items {
        results.push(cast_value(builder, item_ty, largest, item_val));
    }

    Ok((largest, results))
}
