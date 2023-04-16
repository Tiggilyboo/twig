use core::f32;
use cranelift::codegen::ir::types::*;
use tree_sitter::Node;
use log::info;
use super::operator::*;
use super::comparator::*;
use super::frontend_grammar::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Operator(Operator),
    Comparator(Comparator),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Symbol(Symbol),
    Number(Type, Vec<u8>),
    Comment(String),
    String(String),
    Identifier(String, Option<Type>),
}

impl Literal {
    pub fn get_identifier(&self) -> Option<&String> {
        match self {
            Self::Identifier(ident, _) => Some(ident),
            _ => None,
        }
    }
    pub fn get_number_type(&self) -> Option<Type> {
        match *self {
            Self::Number(n_ty, _) => Some(n_ty),
            _ => None,
        }
    }
    pub fn to_i32(&self) -> Result<i32, String> {
        match self {
            Self::Number(t, alloc) => match *t {
                I32 => Ok(i32::from_le_bytes([alloc[0], alloc[1], alloc[2], alloc[3]])),
                _ => Err(format!("Literal is of type {}, expected to be i32", t)),
            },
            _ => Err("Literal is not a Number, expected i32".into()),
        }
    }

    pub fn is_i32(&self) -> bool {
        match *self {
            Self::Number(I32, _) => true,
            _ => false,
        }
    }

    pub fn to_f32(&self) -> Result<f32, String> {
        match self {
            Self::Number(t, alloc) => match *t {
                F32 => Ok(f32::from_le_bytes([alloc[0], alloc[1], alloc[2], alloc[3]])),
                _ => Err(format!("Literal is of type {}, expected to be f32", t)),
            },
            _ => Err("Literal is not a Number, expected i32".into()),
        }
    }

    pub fn is_f32(&self) -> bool {
        match *self {
            Self::Number(F32, _) => true,
            _ => false,
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match *self {
            Self::Number(t, _) => Some(t),
            Self::Identifier(_, t) => t,
            _ => None,
        }
    }
}

impl From<i32> for Literal {
    fn from(value: i32) -> Self {
        Self::Number(I32, value.to_le_bytes().into())
    }
}
impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Self::Number(I64, value.to_le_bytes().into())
    }
}
impl From<f32> for Literal {
    fn from(value: f32) -> Self {
        Self::Number(F32, value.to_le_bytes().into())
    }
}
impl From<f64> for Literal {
    fn from(value: f64) -> Self {
        Self::Number(F64, value.to_le_bytes().into())
    }
}
impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self::String(value)
    }
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
                    info!("{:?}", n);
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
                info!("Unable to parse literal: {} = {}", node_text, n.to_sexp());
                None
            }
        }
    }
}
