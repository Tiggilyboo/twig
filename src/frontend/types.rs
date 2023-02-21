use cranelift::codegen::ir::{
    types::*,
    condcodes::IntCC,
    condcodes::FloatCC,
};
use super::operator::*;
use tree_sitter::Node;

pub enum Grammar {
    CommonLisp,
}

pub enum Symbol {
    Operator(Operator),
    ConditionInt(IntCC),
    ConditionFloat(FloatCC),
}

pub enum Literal {
    Identifier(String),
    Number(Type, [u8;8]),
    Symbol(Symbol),
}

impl Literal {
    pub fn from(n: &Node, s: &str) -> Self {
        match n.kind() {
            "sym_lit" => {
                let op = Operator::from(s);
                if op != Operator::default() {
                    Literal::Symbol(Symbol::Operator(op))
                } else {
                    panic!("Unknown sym_lit: {}", s)
                }
            },
            "num_lit" => {
                panic!("Unimplemented!");
            }
            _ => panic!("Unimplemented"),
        }
    }
}

pub trait FrontendGrammar {
    fn grammar() -> Grammar;

    fn literal_from_node(n: &Node, node_text: &str) -> Result<Literal, String>;
}

pub struct CommonLispGrammar;

impl FrontendGrammar for CommonLispGrammar {
    
    fn grammar() -> Grammar { Grammar::CommonLisp } 

    fn literal_from_node(n: &Node, node_text: &str) -> Result<Literal, String> {
        Ok(Literal::from(n, node_text))
    }
}
