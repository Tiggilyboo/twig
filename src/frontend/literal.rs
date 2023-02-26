use core::f32;
use cranelift::codegen::ir::types::*;
use tree_sitter::Node;
use super::operator::*;
use super::comparator::*;
use super::frontend_grammar::*;

#[derive(Debug, Clone)]
pub enum Symbol {
    Operator(Operator),
    Comparator(Comparator),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Symbol(Symbol),
    Number(Type, Vec<u8>),
    Comment(String),
    String(String),
    Identifier(String, Option<Type>),
}

impl FrontendGrammar<Literal> for CommonLispGrammar<Literal> {
    fn grammar() -> Grammar {
        Grammar::CommonLisp
    }

    fn from_node(n: &Node, s: &str) -> Option<Literal> {
        let node_text = &s[n.start_byte()..n.end_byte()];

        match n.kind() {
            "sym_lit" => {
                if let Ok(op) = Operator::try_from(node_text) {
                    Some(Literal::Symbol(Symbol::Operator(op)))
                } else if let Ok(cmp) = Comparator::try_from(node_text) {
                    Some(Literal::Symbol(Symbol::Comparator(cmp)))
                } else {
                    Some(Literal::Identifier(node_text.to_string(), None))
                }
            },
            "kwd_symbol" => {
                Some(Literal::Identifier(node_text.to_string(), None))
            }
            "num_lit" => {
                if let Ok(num_int) = node_text.parse::<i32>() {
                    Some(Literal::Number(I32, num_int.to_le_bytes().to_vec()))
                } else if let Ok(num_float) = node_text.parse::<f32>() {
                    Some(Literal::Number(F32, num_float.to_le_bytes().to_vec()))
                } else {
                    None
                }
            },
            "char_lit" | "str_lit" => Some(Literal::String(node_text.to_string())),
            "comment" => Some(Literal::Comment(node_text.to_string())),
            _ => {
                println!("Unable to parse literal: {} = {}", node_text, n.to_sexp());
                None
            }
        }
    }
}
