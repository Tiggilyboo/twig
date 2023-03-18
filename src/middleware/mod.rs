mod statement;
use statement::*;

use std::collections::HashMap;
use super::*;
use cranelift::prelude::types;
use cranelift::prelude::*;
use cranelift_module::FunctionDeclaration;

pub struct Middleware {
    backend: JIT,
    builtins: HashMap::<String, FunctionDeclaration>,
}

impl Middleware {
    fn try_transform_invoke(expression: &Expression) -> Option<Statement> {
        let mut invoke_identifier: Option<String> = None;
        let mut invoke_args: Option<Expression> = None;

        match expression {
            Expression::List(expression_children, _) => match expression_children.first() {
                Some(Expression::Value(Literal::Identifier(ident, _), _)) => {
                    invoke_identifier = Some(ident.clone());

                    let mut siblings: Vec<Expression> = expression_children.to_vec();
                    siblings.pop();

                    if siblings.len() > 1 {
                        invoke_args = Some(Expression::List(siblings, None));
                    } else if siblings.len() == 1 {
                        invoke_args = siblings.pop();
                    }
                },
                _ => (),
            },
            _ => (),
        }

        if let Some(identifier) = invoke_identifier {
            Some(Statement::Invoke {
                identifier,
                args: invoke_args,
            })
        } else {
            None
        }
    }

    fn try_transform_operation(expression: &Expression) -> Option<Statement> {
        let mut operator: Option<Operator> = None;
        let mut operation_args: Option<Expression> = None;

        match expression {
            Expression::List(expression_children, _) => match expression_children.first() {
                Some(Expression::Value(Literal::Symbol(Symbol::Operator(op)), _)) => {
                    operator = Some(op.clone());

                    let mut siblings: Vec<Expression> = expression_children.to_vec();
                    siblings.pop();

                    if siblings.len() > 1 {
                        operation_args = Some(Expression::List(siblings, None));
                    } else {
                        operation_args = siblings.pop();
                    }
                },
                _ => {
                    println!("children: {:#?}", expression_children);
                }
            },
            _ => (), 
        }

        if let Some(operator) = operator {
            Some(Statement::Operation(operator, operation_args))
        } else {
            None
        }
    }

    fn transform(expression: &Expression) -> Result<Statement, String> {
        // Recurse child lists (unwrap all ((()))'s)
        match expression {
            Expression::List(expression_children, _) => for child in expression_children {
                if let Ok(statement) = Self::transform(child) {
                    return Ok(statement);
                } else {
                    continue;
                }
            },
            _ => (),
        }

        if let Some(invoke_statement) = Self::try_transform_invoke(expression) {
            Ok(invoke_statement)
        } else if let Some(operation_statement) = Self::try_transform_operation(expression) {
            Ok(operation_statement)
        } else {
            Err("Unable to transform expression.".to_string())
        }
    }

    fn process(&mut self, statement: &Statement) -> Result<(), String> {
        // TODO: Manipulate backend to create statement(s)
        println!("statement: {:#?}", statement);

        match statement {
            Statement::Invoke { identifier, args, } => match identifier.to_lowercase().as_str() {
                "write" => {
                    let mut arg_types = vec![];
                    let mut num_i32_consts = vec![];
                    let mut num_f32_consts = vec![];

                    loop {
                        match args {
                            Some(Expression::List(args_list, _)) => {
                            },
                            Some(Expression::Value(literal, _)) => match literal {
                                Literal::Number(t, n_bytes) => {
                                    arg_types.push(*t);

                                    match *t {
                                        types::I32 => {
                                            let alloc = [n_bytes[0], n_bytes[1], n_bytes[2], n_bytes[3]];
                                            num_i32_consts.push((types::I32, i32::from_le_bytes(alloc)));
                                        },
                                        types::F32 => {
                                            let alloc = [n_bytes[0], n_bytes[1], n_bytes[2], n_bytes[3]];
                                            num_f32_consts.push((types::F32, f32::from_le_bytes(alloc)));
                                        }
                                        _ => unimplemented!(),
                                    }
                                },
                                Literal::Identifier(lit_ident, Some(t)) => {
                                    arg_types.push(*t);
                                },
                                Literal::String(s) => {
                                    unimplemented!()
                                },
                                Literal::Comment(_) => continue,
                                _ => return Err("Expected argument identifier or value".to_string()),

                            },
                            None => break,
                        }
                    }
                    let (f_id, mut fb) = self.backend.make_function_context(&identifier, arg_types, vec![])?;
                    {
                        let block = fb.create_block();
                        fb.switch_to_block(block);

                        for (t, n) in num_i32_consts {
                            // TODO: const when i32 @ n?
                            fb.ins().iconst(t, 0);
                        }
                        
                    }
                },
                _ => println!("Undeclared function: {}", identifier),
            },
            _ => unimplemented!(),
        }

        Ok(())
    }

    pub fn compile(&mut self) -> Result<*const u8, String> {
        unsafe fn run_ptr<I, O>(ptr: *const u8, input: I) -> O {
            let code_fn = mem::transmute::<_, fn(I) -> O>(ptr);
            code_fn(input)
        }

        self.backend.compile()
    }
    
    pub fn process_expression(expression: &Expression) -> Result<Self, String> {
        let backend = JIT::default();
        let statement = Self::transform(expression)?;

        let mut ret = Self {
            backend,
            builtins: HashMap::new(),
        };
        ret.process(&statement)?;

        Ok(ret)
    }
}
