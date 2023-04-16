use std::{collections::{HashMap, HashSet}, str::FromStr, fmt::{Formatter, Display}};
use super::*;
use cranelift::prelude::*;
use cranelift_module::{Linkage, FuncId};
use log::info;

#[derive(Debug, PartialEq)]
pub struct TranslatorDependency {
    pub ident: String,
    pub dep_type: DependencyType,
    pub exp: Expression,
    pub siblings: Vec<Expression>, 
    // Once the dependency has beeen resolved it has a result
    pub result: Option<Box<TranslationResult>>,
}

#[derive(Debug, PartialEq)]
pub enum DependencyType {
    Function,
    Value,
}

impl ToString for DependencyType {
    fn to_string(&self) -> String {
        match *self {
            DependencyType::Value => "value".into(),
            DependencyType::Function => "function_name".into(),
        }
    }
}
impl FromStr for DependencyType {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "function_name" => Ok(DependencyType::Function),
            "value" => Ok(DependencyType::Value),
            _ => Err(format!("'{value}' did not match a DependencyType"))
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TranslationResult {
    None,
    Value((Value, Type)),
    Dependency(TranslatorDependency),
}

impl TranslationResult {
    pub fn get_value(&self) -> Option<&(Value, Type)> {
        match self {
            TranslationResult::None => None,
            TranslationResult::Dependency(ref dep) => dep.get_value(),
            TranslationResult::Value(v) => Some(v),
        }
    }
}

impl TranslatorDependency {
    pub fn new(exp: Expression, siblings: Vec<Expression>) -> Result<Self, String> {
        let (ident, dep_type) = match &exp {
            Expression::Value(Literal::Identifier(ident, _), Some(field)) => (ident.clone(), DependencyType::from_str(&field)?),
            _ => unimplemented!(),
        };

        Ok(Self {
            exp,
            dep_type,
            ident,
            siblings,
            result: None,
        })
    }

    pub fn resolve(&mut self, result: TranslationResult) {
        self.result = Some(Box::new(result));
    }

    pub fn get_value(&self) -> Option<&(Value, Type)> {
        match &self.result {
            Some(ref result) => {
                let unboxed = result;
                match **unboxed {
                    TranslationResult::Value(ref value) => Some(value),
                    TranslationResult::Dependency(ref dep) => dep.get_value(),
                    TranslationResult::None => None,
                }
            },
            _ => None,
        }
    }
}

impl Display for TranslatorDependency {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct FunctionTranslator<'a> {
    pub f_id: FuncId,
    pub builder: FunctionBuilder<'a>,
    vars: HashMap<String, (Variable, Type)>,
    deps: HashMap<String, TranslatorDependency>,
    returns: HashMap<usize, TranslationResult>,
    var_idx: usize,
}

impl <'a> FunctionTranslator <'a> {
    pub fn new(f_id: FuncId, builder: FunctionBuilder<'a>) -> Self {
        Self {
            f_id,
            builder,
            vars: HashMap::new(),
            deps: HashMap::new(),
            returns: HashMap::new(),
            var_idx: 0,
        }
    }

    fn transform_number(&mut self, n_type: Type, alloc: &Vec<u8>) -> Result<TranslationResult, String> {
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
        self.builder.def_var(var.0, val);
        self.var_idx += 1;

        Ok(TranslationResult::Value((val, n_type)))
    }

    fn transform_literal(&mut self, literal_expression: &'a Expression, siblings: Vec<&'a Expression>, parent: Option<&'a Expression>) -> Result<TranslationResult, String> {
        info!("transform lit: {literal_expression:?}, siblings: {siblings:?}");

        match literal_expression {
            Expression::Value(Literal::Comment(_), _) => Ok(TranslationResult::None),
            Expression::Value(Literal::Number(n_type, alloc), _) => self.transform_number(*n_type, alloc),
            Expression::Value(Literal::Symbol(s), _) => {
                let mut args = Vec::new();
                for sibling in siblings {
                    args.extend(self.translate(sibling, vec![], Some(literal_expression))?);
                }
                self.transform_symbol(s, args)
            },
            Expression::Value(Literal::Identifier(ident, ident_ty), _) => {
                if let Some((var, var_ty)) = self.vars.get(ident) {
                    Ok(TranslationResult::Value((self.builder.use_var(*var), *var_ty)))
                } else {
                   Err(format!("{ident} ({siblings:?}) is not defined"))
                }
            },
            _ => unimplemented!(),
        }
    }

    fn transform_symbol(&mut self, symbol: &Symbol, args: Vec<TranslationResult>) -> Result<TranslationResult, String> {
        match symbol {
            Symbol::Operator(op) => self.transform_operation(op, args),
            _ => unimplemented!(),
        }
    }

    fn transform_operation(&mut self, op: &Operator, args: Vec<TranslationResult>) -> Result<TranslationResult, String> {
        let a_len = args.len();

        // TODO vector operations instead of single lane ops

        if a_len == 0 {
            // Err?
            return Ok(TranslationResult::None);
        }
        let mut val_ty: &Type;
        let mut val_acc: Value;
        match &args[0] {
            TranslationResult::None => return Err(format!("Operator {op} has an empty argument")),
            TranslationResult::Value((arg, arg_type)) => (val_acc, val_ty) = (arg.clone(), arg_type),
            TranslationResult::Dependency(dep) => {
                // all arguments must be resolved before doing the operation (FOR NOW...)
                if let Some((dep_val, dep_ty)) = dep.get_value() {
                    (val_acc, val_ty) = (dep_val.clone(), dep_ty);
                } else {
                    let siblings = dep.siblings.clone();
                    let new_dep = TranslatorDependency::new(Expression::Value(Literal::Symbol(Symbol::Operator(op.clone())), None), siblings)?;
                    return Ok(TranslationResult::Dependency(new_dep))
                }
            },
        };

        for arg_result in args.iter().skip(1) {
            match arg_result {
                TranslationResult::None => continue,
                TranslationResult::Value((arg, arg_type)) => {
                    match *arg_type {
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
                    }
                },
                TranslationResult::Dependency(dep) => {
                    // all arguments must be resolved before doing the operation (FOR NOW...)
                    if let Some((dep_val, dep_ty)) = dep.get_value() {
                        (val_acc, val_ty) = (dep_val.clone(), dep_ty);
                    } else {
                        let siblings = dep.siblings.clone();
                        let new_dep = TranslatorDependency::new(Expression::Value(Literal::Symbol(Symbol::Operator(op.clone())), None), siblings)?;
                        return Ok(TranslationResult::Dependency(new_dep))
                    }
                },
            }
        }

        Ok(TranslationResult::Value((val_acc, val_ty.clone())))
    }

    pub fn translate(&mut self, expression: &'a Expression, siblings: Vec<&'a Expression>, parent: Option<&'a Expression>) -> Result<Vec<TranslationResult>, String> {
        info!("translating: {expression:?} parent: {parent:?}");

        match expression {
            Expression::List(children, _) => {
                let mut values: Vec<TranslationResult> = Vec::new();
                if let Some((first, others)) = children.split_first() {
                    for result in self.translate(first, others.into_iter().collect(), Some(expression))? {
                        values.push(result);
                    }
                }
                Ok(values)
            },
            Expression::Value(_, field) => {
                if let Some(field) = field {
                    match field.as_str() {
                        "function_name" => {
                            // TODO: inefficent clones
                            let dep = TranslatorDependency::new(expression.clone(), siblings.into_iter().cloned().collect())?;
                            self.deps.insert(dep.ident.clone(), dep);
                            return Ok(vec![])
                        },
                        _ => {
                        }
                    }
                }
                match self.transform_literal(expression, siblings, parent) { 
                    Ok(TranslationResult::None) => Ok(vec![]),
                    Ok(result) => {
                        info!("checking func returns for {result:?}");

                        // alter function signature to return this root expression!
                        if parent.is_none() {
                            let idx = self.builder.func.signature.returns.len();


                            match result {
                                TranslationResult::Value((root_val, root_ty)) => {
                                    info!("adding func sig ret: {root_val} of ty {root_ty} ({idx})");
                                    self.builder.func.signature.returns.push(AbiParam::new(root_ty));
                                    self.returns.insert(idx, TranslationResult::Value((root_val, root_ty)));
                                },
                                TranslationResult::Dependency(ref dep) => {
                                    info!("adding func sig ret: {dep}({idx})");
                                    if let Some((dep_val, dep_ty)) = dep.get_value() {
                                        self.builder.func.signature.returns.push(AbiParam::new(dep_ty.clone()));
                                        self.returns.insert(idx, TranslationResult::Value((dep_val.clone(), dep_ty.clone())));
                                    } else {
                                        // dependency not fulfilled yet...?
                                        unreachable!()
                                    }
                                }
                                _ => (),
                            }
                        }
                        Ok(vec![result])
                    },
                    Err(err) => Err(err),
                }
            },
        }
    }

    pub fn declare_var(&mut self, t: Type, name: Option<&str>) -> Result<(Variable, Type), String> {
        let fn_name: String = if let Some(name) = name {
            name.into()
        } else {
            format!("var_{}", self.var_idx)
        };

        if let Some(var) = self.vars.get(&fn_name) {
            Ok(*var)
        } else {
            let var = Variable::new(self.var_idx);
            self.builder.declare_var(var, t);
            self.vars.insert(fn_name.into(), (var, t));
            self.var_idx += 1;

            Ok((var, t))
        }
    }

    pub fn finalize_returns(&mut self, function_block: Block) -> Vec<&(Value, Type)> {
        let mut returns = Vec::new();
        for ret_result in self.returns.values() {
            if let Some(res_val) = ret_result.get_value() {
                returns.push(res_val)
            }
            // TODO: Flag for forcing ALL deps resolved? ERRRR otherwise?
        }

        self.builder.append_block_params_for_function_params(function_block);
        self.builder.append_block_params_for_function_returns(function_block);

        returns
    }
}

