use std::{collections::HashMap, path::Path};

use cranelift_codegen::ir::{
    condcodes, AbiParam, InstBuilder, Signature, StackSlotData, StackSlotKind,
};
use cranelift_codegen::settings::Flags;
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
use cranelift_module::{DataDescription, FuncOrDataId, Linkage, Module, ModuleError};
use parser::*;

mod definitions;
use definitions::*;

mod operation;
use operation::*;

mod builtins;

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
    InvalidFunctionSignature(String),
}

pub struct Compiler {
    flags: Flags,
    module: JITModule,
    definitions: HashMap<Symbol, Definition>,
    named_symbols: HashMap<String, Symbol>,
    pointer_ty: Type,
    debug: bool,
    definition_count: usize,
    symbol_count: usize,
    user_sig_count: u32,
}

impl Compiler {
    pub fn new(flags: Flags) -> Result<Self, CompileErr> {
        let isa_builder =
            cranelift_native::builder().map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let isa = isa_builder
            .finish(flags.clone())
            .map_err(|e| CompileErr::InitErr(e.to_string()))?;
        let mut module_builder =
            JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // Register builtins for running defined functions
        module_builder.symbol("print", builtins::print as *const u8);

        let mut module = JITModule::new(module_builder);
        let pointer_ty = module.target_config().pointer_type();

        let mut named_symbols = HashMap::new();
        let mut definitions = HashMap::new();

        // PRINT
        {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(pointer_ty));

            let func_id = module
                .declare_function("print", Linkage::Import, &sig)
                .map_err(|e| CompileErr::ModuleErr(e))?;

            let symbol = Symbol::from(func_id.index());
            let def = Definition::new(symbol, pointer_ty, DefinedValue::Function(func_id));
            named_symbols.insert("print".into(), symbol);
            definitions.insert(symbol, def);
        }

        Ok(Self {
            flags,
            module,
            definitions,
            named_symbols,
            pointer_ty,
            debug: true,
            definition_count: 0,
            user_sig_count: 0,
            symbol_count: 0,
        })
    }

    fn define_data(
        &mut self,
        identifier: &str,
        linkage: Linkage,
        mutable: bool,
        data: Vec<u8>,
    ) -> Result<Definition, CompileErr> {
        if self.is_defined(identifier) {
            return Err(CompileErr::IdentifierExists(format!(
                "identifier '{identifier}' already exists"
            )));
        }
        println!("Defining data: {identifier}");
        let data_id = self
            .module
            .declare_data(identifier, linkage, mutable, false)
            .map_err(|e| CompileErr::ModuleErr(e))?;

        let mut data_desc = DataDescription::new();
        data_desc.define(data.into_boxed_slice());

        self.module
            .define_data(data_id, &data_desc)
            .map_err(|e| CompileErr::ModuleErr(e))?;

        let def = Definition::new(
            self.create_symbol(),
            self.pointer_ty,
            DefinedValue::Data(data_id),
        );
        if self.try_register_def(&def) {
            self.register_symbol(identifier, def.symbol)?;
        } else {
            return Err(CompileErr::DefinitionExists(def));
        }

        Ok(def)
    }

    fn get_def_by_name(&self, name: &str) -> Option<&Definition> {
        if let Some(index) = self.named_symbols.get(name) {
            if let Some(def) = self.definitions.get(index) {
                println!("found {name} def: {def:?}");
                return Some(def);
            }
        }

        None
    }

    fn try_register_def(&mut self, def: &Definition) -> bool {
        println!("registered: {def:?}");
        return self.definitions.insert(def.symbol, def.clone()).is_none();
    }

    fn register_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), CompileErr> {
        println!("registering {name} = {symbol:?}");
        if self.named_symbols.contains_key(name) {
            Err(CompileErr::IdentifierExists(name.into()))
        } else {
            self.named_symbols.insert(name.to_string(), symbol);

            Ok(())
        }
    }

    fn is_defined(&self, name: &str) -> bool {
        self.named_symbols.contains_key(name)
    }

    fn get_next_def_count(&mut self) -> usize {
        let def_count = self.definition_count;
        self.definition_count += 1;
        def_count
    }

    fn get_next_sig_count(&mut self) -> u32 {
        let sig_count = self.user_sig_count;
        self.user_sig_count += 1;
        sig_count
    }

    fn create_symbol(&mut self) -> Symbol {
        let symbol_count = self.symbol_count;
        self.symbol_count += 1;
        Symbol::from(symbol_count)
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
                if let Some(ident_def) = self.get_def_by_name(&identifier.name) {
                    Ok(ident_def.clone())
                } else {
                    return Err(CompileErr::UndefinedIdentifier(identifier.name.clone()));
                }
            }
            Expr::Number(numeric) => {
                let var_val = numeric_to_value(builder, numeric).unwrap();
                let var = Variable::new(self.get_next_def_count());
                builder.declare_var(var, var_val.0);
                builder.def_var(var, var_val.1);

                let symbol = self.create_symbol();
                let def = Definition::new(symbol, var_val.0, DefinedValue::Variable(var));
                self.try_register_def(&def);
                self.register_symbol(name, symbol)?;

                Ok(def)
            }
            Expr::String(string) => {
                self.define_data(name, Linkage::Hidden, false, string.clone().into_bytes())
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
                let symbol = self.create_symbol();
                let param_def = Definition::new(symbol, ir_ty, DefinedValue::Value(param_val));

                self.try_register_def(&param_def);
                self.register_symbol(&p.identifier.name, param_def.symbol)?;
            }

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            res = self.codegen_func(&mut builder, body)?;
            self.try_register_def(&res);
            self.register_symbol(name, res.symbol)?;

            // Determine how to return the function result
            if res.val_type.is_invalid() {
                builder.ins().return_(&[]);
            } else {
                let resolved_values = self.resolve_definitions(&mut builder, &body, &res)?;

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
                println!("> {}", builder.func.name);
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
        defined_type: Type,
        defined_value: &DefinedValue,
        index: Option<usize>,
    ) -> Result<Option<(Type, Value)>, CompileErr> {
        match defined_value {
            DefinedValue::None => Ok(None),
            DefinedValue::Value(value) => Ok(Some((defined_type, *value))),
            DefinedValue::Variable(var) => builder
                .try_use_var(*var)
                .map(|v| Some((defined_type, v)))
                .map_err(|e| CompileErr::VariableErr(e.to_string())),
            DefinedValue::Data(data_id) => {
                let pointer = self.pointer_ty;
                let data = self.module.declare_data_in_func(*data_id, builder.func);
                let value = builder.ins().global_value(pointer, data);
                Ok(Some((defined_type, value)))
            }
            DefinedValue::Function(func_id) => {
                let pointer = self.pointer_ty;
                let func_ref = self.module.declare_func_in_func(*func_id, builder.func);
                let value = builder.ins().func_addr(pointer, func_ref);
                Ok(Some((defined_type, value)))
            }
            DefinedValue::List(defined_values) => {
                if let Some(list_index) = index {
                    if let Some(item) = defined_values.get(list_index) {
                        self.resolve_value(builder, expr, item.0, &item.1, None)
                    } else {
                        Err(CompileErr::IndexOutOfRange(format!(
                            "Unable to find index {list_index} in {expr:?}"
                        )))
                    }
                } else {
                    // No actual pointer to data for this defined type, store it as a stack and return that pointer
                    // replace symbol definition
                    // ??????????

                    Err(CompileErr::IndexOutOfRange(format!(
                        "{expr:?} requires index to find value in {defined_value:?}"
                    )))
                }
            }
            DefinedValue::Stack { slot, items } => {
                let pointer = self.pointer_ty;

                if let Some(stack_index) = index {
                    if let Some(item) = items.get(stack_index) {
                        let stack_value = builder.ins().stack_load(item.0, *slot, item.1);
                        Ok(Some((item.0, stack_value)))
                    } else {
                        Err(CompileErr::IndexOutOfRange(format!("{expr:?}")))
                    }
                } else {
                    // Resolve to ref if no stack index given
                    let stack_ref = builder.ins().stack_addr(pointer, *slot, Offset32::new(0));
                    Ok(Some((self.pointer_ty, stack_ref)))
                }
            }
        }
    }

    fn resolve_definition(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        defined: &Definition,
        index: Option<usize>,
    ) -> Result<Option<(Type, Value)>, CompileErr> {
        self.resolve_value(builder, expr, defined.val_type, &defined.value, index)
    }

    fn resolve_definitions(
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
                if let Some(v) = self.resolve_definition(builder, expr, defined, None)? {
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
            let op_res = self.resolve_definitions(builder, expr, &defined_op)?;
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

        Ok(Definition::new(
            self.create_symbol(),
            accumulator_type,
            DefinedValue::Value(accumulator),
        ))
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
        match expr {
            Expr::Identifier(identifier) => {
                if let Some(defined) = self.get_def_by_name(&identifier.name) {
                    Ok(defined.clone())
                } else {
                    Err(CompileErr::UndefinedIdentifier(identifier.name.to_string()))
                }
            }
            Expr::Number(numeric) => {
                if let Some((value_ty, value)) = numeric_to_value(builder, numeric) {
                    Ok(Definition::new(
                        self.create_symbol(),
                        value_ty,
                        DefinedValue::Value(value),
                    ))
                } else {
                    Err(CompileErr::Err(format!(
                        "Unable to convert numeric to value: {numeric:?}"
                    )))
                }
            }
            Expr::Switch((), (), switch_expr, cases, ()) => {
                let cond_def = self.codegen_func(builder, switch_expr.as_ref())?;
                let cond_val = self.resolve_definition(builder, switch_expr, &cond_def, None)?;
                if cond_val.is_none() {
                    return Err(CompileErr::InvalidCondition(format!(
                        "Switch subject not defined: {switch_expr:?}"
                    )));
                }
                let (cond_ty, _cond_val) = cond_val.unwrap();
                let merge_block = builder.create_block();
                let default_block = builder.create_block();
                let mut default_filled = false;

                builder.append_block_param(merge_block, cond_ty);

                for case in cases {
                    if case.cmp == Comparator::Default {
                        // Default case should only specify a single operand
                        if case.operands.len() != 1 {
                            return Err(CompileErr::InvalidCondition(
                                "Multi condition switches not currently implemented".into(),
                            ));
                        }
                        if default_filled {
                            return Err(CompileErr::InvalidCondition(format!(
                                "Switch may only have one default case: {switch_expr:?}"
                            )));
                        }

                        // Set up default block
                        if let Some(default_expr) = case.operands.first() {
                            builder.switch_to_block(default_block);
                            builder.seal_block(default_block);

                            let else_def = self.codegen_func(builder, default_expr)?;
                            let else_val =
                                self.resolve_definition(builder, default_expr, &else_def, None)?;
                            let merge_block_inputs = if let Some((_, else_value)) = else_val {
                                vec![else_value]
                            } else {
                                vec![]
                            };

                            // Jump to merge block pass it the else return value
                            builder.ins().jump(merge_block, &merge_block_inputs);
                        } else {
                            return Err(CompileErr::InvalidCondition(format!(
                                "Switch default case must specify only one expression: {switch_expr:?}")));
                        }

                        default_filled = true;
                    }
                    // case defines operands to compare against
                    // Should define at least 2
                    else if case.operands.len() != 2 {
                        return Err(CompileErr::InvalidCondition(
                            "Multi condition switches not currently implemented".into(),
                        ));
                    } else {
                        let case_block = builder.create_block();
                        let operand = &case.operands[0];
                        let operand_def = self.codegen_func(builder, &operand)?;

                        let cmp_val = self.codegen_cond(
                            builder,
                            &switch_expr,
                            &case.cmp,
                            vec![&cond_def, &operand_def],
                        )?;

                        builder
                            .ins()
                            .brif(cmp_val, case_block, &[], default_block, &[]);

                        // Fill case block
                        builder.switch_to_block(case_block);
                        builder.seal_block(case_block);
                        let case_expr = &case.operands[1];

                        let case_def = self.codegen_func(builder, &case_expr)?;
                        let case_value =
                            self.resolve_definition(builder, &case_expr, &case_def, None)?;
                        let merge_block_inputs = if let Some((_case_ty, case_value)) = case_value {
                            vec![case_value]
                        } else {
                            vec![]
                        };

                        // jump to merge block with case_value
                        builder.ins().jump(merge_block, &merge_block_inputs);
                    }
                }

                builder.switch_to_block(merge_block);
                builder.seal_block(merge_block);

                let switch_value = builder.block_params(merge_block)[0];
                let symbol = self.create_symbol();
                let switch_def =
                    Definition::new(symbol, cond_ty, DefinedValue::Value(switch_value));

                Ok(switch_def)
            }
            Expr::List((), items, ()) => {
                // Evaluate remaining parameters to call to function
                let item_values = self.codegen_func_vec(builder, &items.iter().collect())?;

                // Extract identifier of function (if any)
                let func_data_id = if let Some(first_item) = items.first() {
                    match first_item {
                        Expr::Identifier(Identifier { name }) => {
                            if let Some(def_symbol) = self.named_symbols.get(name) {
                                if let Some(def) = self.definitions.get(def_symbol) {
                                    match def.value {
                                        DefinedValue::Function(func_id) => {
                                            Some(FuncOrDataId::Func(func_id))
                                        }
                                        DefinedValue::Data(data_id) => {
                                            Some(FuncOrDataId::Data(data_id))
                                        }
                                        _ => None,
                                    }
                                } else {
                                    None
                                }
                            // Check if the symbol is defined in the module
                            } else if let Some(func_or_data_id) = self.module.get_name(name) {
                                Some(func_or_data_id)
                            } else {
                                return Err(CompileErr::UndefinedIdentifier(name.to_string()));
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                match func_data_id {
                    Some(FuncOrDataId::Func(func_id)) => {
                        let (func_name, func_sig_params, func_ret) = {
                            let func_decl = self.module.declarations().get_function_decl(func_id);
                            let func_sig_params = func_decl
                                .signature
                                .params
                                .iter()
                                .map(|p| p.value_type)
                                .collect::<Vec<Type>>();
                            let func_ret = func_decl
                                .signature
                                .returns
                                .iter()
                                .map(|p| p.value_type)
                                .collect::<Vec<Type>>();
                            let func_name = func_decl.name.clone().unwrap_or("anonymous".into());

                            (func_name, func_sig_params, func_ret)
                        };

                        // TODO: Abstract validation of signature to call:
                        // Signature does not match
                        if func_sig_params.len() != items.len() - 1 {
                            return Err(CompileErr::InvalidFunctionSignature(format!(
                                "function {} requires {} parameters, but only {} were passed",
                                func_name,
                                func_sig_params.len(),
                                items.len() - 1
                            )));
                        }

                        let mut func_params = vec![];

                        // Validate parameters
                        for (index, sig_param) in func_sig_params.iter().enumerate() {
                            if let Some(item_definition) = item_values.get(index) {
                                if item_definition.val_type != *sig_param {
                                    return Err(CompileErr::InvalidFunctionSignature(format!(
                                        "function {} argument {} type mismatch {:?} != {:?}",
                                        func_name, index, sig_param, item_definition.val_type
                                    )));
                                }

                                // Try to resolve the definition of the parameter
                                if let Some(param_expr) = items.get(index) {
                                    let resolved_value = self.resolve_definition(builder, param_expr, item_definition, None)
                                        .map_err(|e| CompileErr::InvalidFunctionSignature(format!("function {func_name} parameter {index} from {param_expr:?}: {e:?}")))?;

                                    if let Some((_param_type, param_value)) = resolved_value {
                                        func_params.push(param_value);
                                    } else {
                                        return Err(CompileErr::InvalidFunctionSignature(format!(
                                            "function {func_name} parameter {index} "
                                        )));
                                    }
                                } else {
                                    return Err(CompileErr::InvalidFunctionSignature(format!("function {func_name} parameter {index} could not resolve parsed expression")));
                                }
                            } else {
                                return Err(CompileErr::InvalidFunctionSignature(format!(
                                    "function {} argument {} of type {:?} missing",
                                    func_name, index, sig_param
                                )));
                            }
                        }

                        let func_ref = self.module.declare_func_in_func(func_id, builder.func);
                        let call = builder.ins().call(func_ref, func_params.as_slice());

                        let call_ret = builder.inst_results(call);
                        if call_ret.is_empty() {
                            return Ok(Definition::null());
                        } else {
                            if call_ret.len() != func_ret.len() {
                                return Err(CompileErr::TypeMismatch(format!("{func_name} expected return types: {func_ret:?}, got {call_ret:?}")));
                            }
                            let ret_def_values: Vec<(Type, DefinedValue)> = call_ret
                                .iter()
                                .enumerate()
                                .map(|(i, v)| (func_ret[i], DefinedValue::Value(*v)))
                                .collect();

                            Ok(Definition::new(
                                self.create_symbol(),
                                self.pointer_ty,
                                DefinedValue::List(ret_def_values),
                            ))
                        }
                    }
                    Some(FuncOrDataId::Data(data_id)) => {
                        unimplemented!()
                    }
                    None => self.codegen_stack(builder, expr, item_values.iter().collect()),
                }
            }
            Expr::Operation((), op, operand, ()) => self.codegen_op(builder, expr, op, operand),
            Expr::Function(def_expr) => self.define_func_expr(def_expr),
            Expr::Variable(def_var) => {
                self.define_var(builder, &def_var.identifier.name, def_var.body.as_ref())
            }
            _ => Err(CompileErr::Err(format!(
                "Unhandled expression in func {:?}: {:?}",
                builder.func.name, expr
            ))),
        }
    }

    fn codegen_cond(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        comparator: &Comparator,
        operands: Vec<&Definition>,
    ) -> Result<Value, CompileErr> {
        let mut operand_ir = Vec::new();
        for operand in operands {
            if let Some(pair) = self.resolve_definition(builder, expr, &operand, None)? {
                operand_ir.push(pair);
            }
        }

        let (ty, operand_values) = cast_to_largest(builder, expr, operand_ir)?;
        let mut cmp = *operand_values.first().unwrap();

        if ty.is_invalid() {
            return Err(CompileErr::InvalidCondition(format!(
                "condition {comparator:?} for {expr:?} has invalid operands"
            )));
        } else if comparator == &Comparator::Default {
            // Fallthrough / default condition, always return non-zero (1)
            cmp = builder.ins().iconst(types::I64, 1);
        } else if ty.is_int() {
            if let Some(cc) = condcode_int(comparator) {
                for value in operand_values.iter().skip(1) {
                    cmp = builder.ins().icmp(cc, cmp, *value);
                }
            } else {
                unreachable!()
            }
        } else if ty.is_float() {
            if let Some(cc) = condcode_float(comparator) {
                for value in operand_values.iter().skip(1) {
                    cmp = builder.ins().fcmp(cc, cmp, *value);
                }
            } else {
                unreachable!()
            }
        } else {
            return Err(CompileErr::InvalidCondition(format!(
                "unable to compare {comparator:?} for {expr:?}"
            )));
        }

        Ok(cmp)
    }

    fn codegen_stack(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
        stack_items: Vec<&Definition>,
    ) -> Result<Definition, CompileErr> {
        // Resolve all values before defining the stack
        let mut stack_values = Vec::new();
        for item in stack_items {
            if let Some((item_ty, item_value)) =
                self.resolve_definition(builder, expr, &item, None)?
            {
                stack_values.push((item_ty, item_value));
            } else {
                return Err(CompileErr::UndefinedValue(item.clone()));
            }
        }

        // All values evaluated, now we can define the stack and it's elements

        let data_bits: u32 = stack_values.iter().map(|i| i.0.bits()).sum();
        let data_size = data_bits + 7 / 8;
        let data = StackSlotData::new(StackSlotKind::ExplicitSlot, data_size);
        let slot = builder.create_sized_stack_slot(data);

        let mut offset: i32 = 0;
        let mut stored_items: Vec<(Type, Offset32)> = Vec::new();
        for (item_ty, item_value) in stack_values {
            builder
                .ins()
                .stack_store(item_value, slot, Offset32::new(offset));

            stored_items.push((item_ty, Offset32::new(offset)));

            offset += item_ty.bytes() as i32;
        }

        let def = Definition::new(
            self.create_symbol(),
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

// for aot object compilation
#[allow(unused)]
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

pub unsafe fn run_code<I1, I2, O>(code_ptr: *const u8, input1: I1, input2: I2) -> O {
    let code_fn = std::mem::transmute::<_, fn(I1, I2) -> O>(code_ptr);
    code_fn(input1, input2)
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

fn condcode_int(comparator: &Comparator) -> Option<condcodes::IntCC> {
    use condcodes::*;

    match comparator {
        Comparator::Eq => Some(IntCC::Equal),
        Comparator::NotEq => Some(IntCC::NotEqual),
        Comparator::Less => Some(IntCC::SignedLessThan),
        Comparator::LessEq => Some(IntCC::SignedLessThanOrEqual),
        Comparator::Greater => Some(IntCC::SignedGreaterThan),
        Comparator::GreaterEq => Some(IntCC::SignedGreaterThanOrEqual),
        Comparator::Default => None,
    }
}

fn condcode_float(comparator: &Comparator) -> Option<condcodes::FloatCC> {
    use condcodes::*;

    return match comparator {
        Comparator::Eq => Some(FloatCC::Equal),
        Comparator::NotEq => Some(FloatCC::NotEqual),
        Comparator::Less => Some(FloatCC::LessThan),
        Comparator::LessEq => Some(FloatCC::LessThanOrEqual),
        Comparator::Greater => Some(FloatCC::GreaterThan),
        Comparator::GreaterEq => Some(FloatCC::GreaterThanOrEqual),
        Comparator::Default => None,
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
