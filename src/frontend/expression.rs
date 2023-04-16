use log::info;

use super::{
    FrontendGrammar,
    CommonLispGrammar,
};
use super::literal::*;
use super::frontend_grammar::*;
use tree_sitter::Node;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Value(Literal, Option<String>),
    List(Vec<Expression>, Option<String>),
}

impl FrontendGrammar<Expression> for CommonLispGrammar<Expression> {
    fn grammar() -> Grammar {
        Grammar::CommonLisp 
    }

    fn from_node(n: &Node, source: &str) -> Option<Expression> {

        fn parse_children(n: &Node, source: &str) -> (Vec::<Expression>, Option<String>) {
            let mut values = Vec::<Expression>::new();
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

                info!("cursor: {:?}", c.to_sexp());
                if let Some(s) = CommonLispGrammar::<Expression>::from_node(&c, source) {
                    values.push(s);
                } 
                else if let Some(l) = CommonLispGrammar::<Literal>::from_node(&c, source) {
                    if let Some(field_name) = cursor.field_name() {
                        values.push(Expression::Value(l, Some(field_name.to_string())));
                    } else {
                        values.push(Expression::Value(l, None));
                    }
                }

                if !cursor.goto_next_sibling() {
                    break;
                }
            }

            (values, field_name)
        }

        match n.kind() {
            "source" 
                | "list_lit" | "kwd_lit" 
                | "defun_header" | "defun"
                | "quoting_lit" => {
                let (children, field_name) = parse_children(n, source);
                Some(Expression::List(children, field_name))
            },
            _ => None,
        } 
    }
}
