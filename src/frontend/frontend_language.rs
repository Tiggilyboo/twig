use std::collections::HashMap;
use tree_sitter::Language;

use super::parse_grammar;
use super::Variable;

const GRAMMAR_COMMON_LISP: &str = include_str!("../../grammars/tree-sitter-commonlisp.json");

pub enum FrontendLanguage {
    CommonLisp,
}

impl FrontendLanguage {
    pub fn ts_grammar(&self) -> HashMap<String, Variable> {
        match self {
            CommonLisp => get_grammar(GRAMMAR_COMMON_LISP),
        }
    }

    pub fn ts_language(&self) -> Language {
        match self {
            CommonLisp => tree_sitter_commonlisp::language(),
        }
    }
}

fn get_grammar(grammar_json: &str) -> HashMap::<String, Variable> {
    match parse_grammar::parse_grammar(grammar_json) {
        Err(err) => panic!("{:?}", err.to_string()),
        Ok(grammar) => {
            let mut variables = HashMap::new();

            for v in grammar.variables.iter() {
                variables.insert(v.name.to_string(), v.clone());
            }

            variables
        },
    }
}
