use std::collections::HashMap;

use cranelift::{
    codegen::{
        ir::{Function, UserExternalName},
        verifier::VerifierErrors,
        verify_function,
    },
    prelude::*,
};
use parser::*;

#[derive(Debug)]
pub enum CodegenErr {
    Err(String),
    VerifierErr(VerifierErrors),
}

pub struct State {
    functions: HashMap<String, Function>,
    variables: HashMap<String, Variable>,
    entity_count: usize,
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

impl State {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
            entity_count: 0,
        }
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

    fn define_func(&mut self, name: String, body: &Expr) -> Result<(), CodegenErr> {
        let mut sig = Signature::new(isa::CallConv::SystemV);
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let function_name = UserExternalName::new(0, 0);
        let mut func =
            Function::with_name_signature(codegen::ir::UserFuncName::User(function_name), sig);
        {
            let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
            // TODO: When is it preferable to make new blocks?
            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);
            builder.switch_to_block(block);
            builder.seal_block(block);

            let res = self.codegen_func(&mut builder, body)?;

            builder.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&func, &flags);

        if let Err(errors) = res {
            Err(CodegenErr::VerifierErr(errors))
        } else {
            self.functions.insert(name.to_string(), func);
            Ok(())
        }
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
            Expr::Operation(op, operand) => match op {
                Operator::Add => {
                    let operand = self.codegen_func(builder, operand.as_ref())?;
                    // TODO: vector ops
                    let biggest_var_type = operand
                        .values
                        .iter()
                        .max_by(|x, y| x.0.bits().cmp(&y.0.bits()))
                        .map(|v| v.0)
                        .unwrap();

                    let mut accumulator = builder.ins().iconst(biggest_var_type, 0);
                    for (ty, val) in operand.values.iter() {
                        accumulator = builder.ins().iadd(accumulator, *val);
                    }

                    Ok(CodegenResult::from((biggest_var_type, accumulator)))
                }
                _ => Err(CodegenErr::Err(format!("Unhandled operator: {op:?}"))),
            },
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
                self.define_func(ident.name.to_string(), value.as_ref())?;
                Ok(CodegenResult::default())
            }
            _ => Err(CodegenErr::Err(
                format!("Unhandled expression: {expr:?}").to_string(),
            )),
        }
    }
}

pub fn codegen(expr: &Expr) -> Result<(), CodegenErr> {
    let mut state = State::new();

    state.codegen(expr)?;
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
