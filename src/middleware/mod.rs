use std::{collections::HashMap, hash::Hash};
use super::*;
use cranelift::prelude::*;
use cranelift_module::{Linkage, FuncId, FunctionDeclaration};
use log::info;

pub struct FunctionTranslator<'a> {
    pub builder: FunctionBuilder<'a>,
    pub f_id: FuncId,
    vars: HashMap<String, Variable>,
    funcs: HashMap<String, FunctionDeclaration>,
    var_idx: usize,
    fn_idx: usize,
}

impl <'a> FunctionTranslator <'a> {
    pub fn new(f_id: FuncId, builder: FunctionBuilder<'a>) -> Self {
        Self {
            f_id,
            builder,
            vars: HashMap::new(),
            funcs: HashMap::new(),
            var_idx: 0,
            fn_idx: 0,
        }
    }

    // Validate that all literals are of the same discriminant
    fn validate_all_same_type<T>(a: &Vec<T>) -> bool {
        if let Some(first) = a.first() {
            let first_disc = std::mem::discriminant(first);
            
            a.iter().skip(1).all(|l| first_disc == std::mem::discriminant(l))
        } else {
            false
        }
    }

    fn transform_number(&mut self, n_type: Type, alloc: &Vec<u8>) -> Result<Option<Value>, String> {
        fn from_alloc_32(a: &Vec<u8>) -> Result<[u8; 4], String> {
            if a.len() < 4 {
                return Err(format!("Number allocation did not contain enough bytes for 32bit value: {a:?}"))
            }
            Ok([a[0], a[1], a[2], a[3]])
        }
        fn from_alloc_64(a: &Vec<u8>) -> Result<[u8; 8], String> {
            if a.len() < 8 {
                return Err(format!("Number allocation did not contain enough bytes for 32bit value: {a:?}"))
            }
            Ok([a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]])
        }
        let var = self.declare_var(n_type, None)?;
        let val = match n_type {
            types::I32 => {
                let num = i32::from_le_bytes(from_alloc_32(alloc)?);
                self.builder.ins().iconst(n_type, Imm64::new(num as i64))
            },
            types::I64 => {
                let num = i64::from_le_bytes(from_alloc_64(alloc)?);
                self.builder.ins().iconst(n_type, Imm64::new(num))
            },
            types::F32 => {
                let num = f32::from_le_bytes(from_alloc_32(alloc)?);
                self.builder.ins().f32const(Ieee32::from(num))
            },
            types::F64 => {
                let num = f64::from_le_bytes(from_alloc_64(alloc)?);
                self.builder.ins().f64const(Ieee64::from(num))
            },
            _ => unimplemented!()
        };
        self.builder.def_var(var, val);
        self.var_idx += 1;

        Ok(Some(val))
    }

    fn transform_function(&mut self, literal: &Literal, siblings: Vec<&Expression>) -> Result<(), String> {
        info!("transform lit to func: {literal:?}, siblings: {siblings:?}");

        match literal {
            Literal::Identifier(ident, ident_ty) => {
                if self.funcs.contains_key(ident) {
                    return Err(format!("Function '{}' already defined.", ident));
                }
                let mut args = Vec::new();
                let mut body_values = Vec::new();
                if let Some(args_exp) = siblings.split_first() {
                    match args_exp {
                        (Expression::List(sub_exp_args, _), body) => {
                            for arg in sub_exp_args {
                                match arg {
                                    Expression::Value(lit, _) => args.push(lit.clone()),
                                    _ => return Err(format!("Expected argument for function '{}' to be a value, got: {:?}", ident, arg)),
                                }
                            }
                            for exp in body {
                                match self.translate(types::INVALID, exp, body.into()) {
                                    Ok(body_value) => body_values.push(body_value),
                                    Err(err) => return Err(format!("Function '{}' expression {:?}: {}", ident, exp, err)),
                                }
                            }
                        },
                        _ => return Err(format!("Expected arguments for function '{}' to be list, got: {:?}", ident, args_exp))
                    }
                } else {
                    return Err(format!("Expected arguments for function '{}' in list after function definition for: {:?}", ident, literal));
                }

                let mut arg_values = Vec::new();
                for arg in args {
                    if let Some(arg_type) = arg.get_type() {
                        match self.transform_literal(arg_type, &arg, Vec::new()) {
                            Ok(Some(lit_val)) => arg_values.push(lit_val),
                            Ok(None) => return Err(format!("Argument {:?} of type {:?} is not a value type!", arg, arg_type)),
                            Err(err) => return Err(err),
                        }
                    } else {
                        return Err(format!("Argument literal has no type: {:?}", arg))
                    }
                }

                Ok(())
            }
            _ => return Err(format!("Literal {:?} expected to have identifier.", literal))
        }
    }

    fn transform_literal(&mut self, ret_type: Type, literal: &Literal, siblings: Vec<&Expression>) -> Result<Option<Value>, String> {
        info!("transform lit: {literal:?}, siblings: {siblings:?}");

        match literal {
            Literal::Comment(_) => Ok(None),
            Literal::Number(n_type, alloc) => self.transform_number(*n_type, alloc),
            Literal::Symbol(s) => {
                let mut args = Vec::new();
                for sibling in siblings {
                    args.extend(self.translate(ret_type, sibling, vec![])?);
                }
                self.transform_symbol(ret_type, s, args)
            },
            Literal::Identifier(ident, ident_ty) => if self.vars.contains_key(ident) {
                Ok(Some(self.builder.use_var(*self.vars.get(ident).unwrap())))
            } else {
                Err(format!("{ident} ({siblings:?}) is not defined"))
            },
            _ => unimplemented!(),
        }
    }

    fn transform_symbol(&mut self, ret_type: Type, symbol: &Symbol, args: Vec<Value>) -> Result<Option<Value>, String> {
        match symbol {
            Symbol::Operator(op) => self.transform_operation(op, ret_type, args),
            _ => unimplemented!(),
        }
    }

    fn transform_operation(&mut self, op: &Operator, arg_type: Type, args: Vec<Value>) -> Result<Option<Value>, String> {
        let a_len = args.len();

        // TODO vector operations instead of single lane ops

        let mut val_acc = if a_len > 0 {
            args[0]
        } else {
            match arg_type {
                types::I32 | types::I64 => self.builder.ins().iconst(arg_type, 0),
                types::F32 => self.builder.ins().f32const(0f32),
                types::F64 => self.builder.ins().f64const(0f64),
                _ => unimplemented!(),
            }
        };

        for arg in args.iter().skip(1) {
            match arg_type {
                types::I32 | types::I64 => match op {
                    Operator::Add => val_acc = self.builder.ins().iadd(val_acc, arg.clone()),
                    Operator::Sub => val_acc = self.builder.ins().isub(val_acc, arg.clone()),
                    Operator::Mul => val_acc = self.builder.ins().imul(val_acc, arg.clone()),
                    Operator::Div => val_acc = self.builder.ins().sdiv(val_acc, arg.clone()),
                    Operator::Mod => val_acc = self.builder.ins().srem(val_acc, arg.clone()),
                    _ => unimplemented!(),
                },
                types::F32 | types::F64 => match op {
                    Operator::Add => val_acc = self.builder.ins().fadd(val_acc, arg.clone()),
                    Operator::Sub => val_acc = self.builder.ins().fsub(val_acc, arg.clone()),
                    Operator::Mul => val_acc = self.builder.ins().fmul(val_acc, arg.clone()),
                    Operator::Div => val_acc = self.builder.ins().fdiv(val_acc, arg.clone()),
                    Operator::Mod => return Err(format!("{arg_type} cannot do modulus division")),
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            };
        }

        Ok(Some(val_acc))
    }

    pub fn translate(&mut self, ret_type: Type, expression: &Expression, siblings: Vec<&Expression>) -> Result<Vec<Value>, String> {
        match expression {
            Expression::List(children, _) => {
                let mut values: Vec<Value> = Vec::new();
                if let Some((first, others)) = children.split_first() {
                    for cv in self.translate(ret_type, first, others.into_iter().collect())? {
                        values.push(cv);
                    }
                }
                Ok(values)
            },
            Expression::Value(literal, field) => {
                if let Some(field) = field {
                    match field.as_str() {
                        "function_name" => {
                            self.transform_function(literal, siblings)?;
                            return Ok(vec![])
                        },
                        _ => {
                        }
                    }
                }
                if let Some(lit_val) = self.transform_literal(ret_type, literal, siblings)? {
                    Ok(vec![lit_val])
                }
                else {
                    Ok(vec![])
                }
            },
        }
    }

    pub fn declare_var(&mut self, t: Type, name: Option<&str>) -> Result<Variable, String> {
        let fn_name: String = if let Some(name) = name {
            name.into()
        } else {
            format!("var_{}", self.var_idx)
        };

        if let Some(var) = self.vars.get(&fn_name) {
            Ok(*var)
        } else {
            let var = Variable::new(self.var_idx);
            self.vars.insert(fn_name.into(), var);
            self.builder.declare_var(var, t);
            self.var_idx += 1;

            Ok(var)
        }
    }
}

