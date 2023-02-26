use std::marker::PhantomData;

use tree_sitter::Node;

pub enum Grammar {
    CommonLisp,
}

pub struct CommonLispGrammar<T> where T: Sized {
    p: PhantomData<T>,
}

pub trait FrontendGrammar<T> where T: Sized {
    fn grammar() -> Grammar;
    fn from_node(n: &Node, source: &str) -> Option<T>;
    fn validate(grammar: T) -> Result<(), String>;
}

