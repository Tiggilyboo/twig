use super::{
    FrontendGrammar,
    CommonLispGrammar,
};
use super::literal::*;
use super::frontend_grammar::*;
use tree_sitter::Node;

#[derive(Debug, Clone)]
pub enum Statement {
    Value(Literal, Option<String>),
    List(Vec<Statement>, Option<String>),
    Define {
        name: String, 
        args: Vec<Literal>,
        body: Vec<Statement>,
    },
}

impl FrontendGrammar<Statement> for CommonLispGrammar<Statement> {
    fn grammar() -> Grammar {
        Grammar::CommonLisp 
    }

    fn from_node(n: &Node, source: &str) -> Option<Statement> {

        fn parse_children(n: &Node, source: &str) -> (Vec::<Statement>, Option<String>) {
            let mut values = Vec::<Statement>::new();
            let mut cursor = n.walk();
            if !cursor.goto_first_child() {
                return (values, None);
            }
            let field_name = if let Some(field_name) = cursor.field_name() {
                Some(field_name.to_string())
            } else {
                None
            };

            loop {
                let c = cursor.node();

                println!("cursor: {:?}", c.to_sexp());
                if let Some(s) = CommonLispGrammar::<Statement>::from_node(&c, source) {
                    values.push(s);
                } 
                else if let Some(l) = CommonLispGrammar::<Literal>::from_node(&c, source) {
                    if let Some(field_name) = cursor.field_name() {
                        values.push(Statement::Value(l, Some(field_name.to_string())));
                    } else {
                        values.push(Statement::Value(l, None));
                    }
                }

                if !cursor.goto_next_sibling() {
                    break;
                }
            }

            (values, field_name)
        }

        fn parse_define(n: &Node, source: &str) -> Option<Statement> {
            let (children, field_name) = parse_children(n, source);
            let mut function_name: Option<String> = None;
            let mut function_args = Vec::<Literal>::new();
            let function_body;

            //println!("parse_define: children = {:#?}, field_name = {:?}", children, field_name);

            // defun_header
            match children.first() {
                Some(Statement::List(defun_header_items, _)) => {
                    for c in defun_header_items {
                        //println!("defun_header child: {:?}", c);

                        match c {
                            Statement::Value(Literal::Identifier(ident), Some(f)) => match f.as_str() {
                                "function_name" => {
                                    function_name = Some(ident.to_string());
                                },
                                _ => {
                                    println!("Unhandled parse of defun_header value: {:?}", c);
                                    continue;
                                }
                            },
                            Statement::List(lambda_list, Some(f)) => match f.as_str() {
                                "open" => {
                                    for lambda in lambda_list {
                                        match lambda {
                                            Statement::Value(lambda_literal, _) => {
                                                function_args.push(lambda_literal.clone());
                                            },
                                            _ => (),
                                        }
                                    }
                                }
                                _ => {
                                    println!("Unhandled parse of defun_header list: {:?}", c);
                                    continue;
                                }
                            },
                            _ => {
                                println!("Unhandled parse of defun_header: {:?}", c);
                                continue;
                            },
                        }
                    }
                },
                _ => return None,
            }

            // parse body
            match children.last() {
                Some(Statement::List(body_statements, Some(body_field_name))) => match body_field_name.as_str() {
                    "open" => {
                        function_body = body_statements;
                    }
                    _ => return None,
                }
                _ => return None,
            }

            // Validate components of function definition
            if function_name.is_none() {
                None
            } else {
                Some(Statement::Define {
                    name: function_name.unwrap(),
                    args: function_args,
                    body: function_body.to_owned(),
                })
            }
            
        }

        match n.kind() {
            "source" | "list_lit" | "kwd_lit" | "defun_header" => {
                let (children, field_name) = parse_children(n, source);
                Some(Statement::List(children, field_name))
            },
            "defun" => parse_define(n, source),
            _ => None,
        } 
    }

    fn validate(grammar: Statement) -> Result<(), String> {
        // TODO: Test for some coherency before passing the statement to backend
        Ok(())
    }
}
