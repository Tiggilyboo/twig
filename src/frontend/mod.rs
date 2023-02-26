mod frontend_grammar;
mod operator;
mod comparator;
mod literal;
mod statement;

use std::{
    rc::Rc, 
    collections::{
        HashMap,
    },
};

use tree_sitter::{
    Parser, 
    Language,
    Tree,
    Query,
    QueryError,
};

use self::frontend_grammar::{
    FrontendGrammar, 
    CommonLispGrammar,
};
pub use self::statement::*;
pub use self::literal::*;

pub struct Frontend {
    parser: Parser,
    language: Language,
    tree: Option<Rc<Tree>>,
    buffer: String,
}

impl Frontend {
    pub fn from_language(language: Language) -> Result<Self, String> {
        let mut parser = Parser::new();
        parser.set_language(language)
            .map_err(|e| e.to_string())?;

        Ok(Self {
            parser,
            language,
            buffer: String::with_capacity(0),
            tree: None,
        })
    }

    pub fn parse(&mut self, code: &str) -> Option<Statement> {
        self.buffer = String::from(code);
        let new_tree = if let Some(old_tree) = &self.tree {
            self.parser.parse(code, Some(old_tree.as_ref()))
        } else {
            self.parser.parse(code, None)
        };
        if let Some(tree) = new_tree {
            self.tree = Some(Rc::new(tree));
        } else {
            self.tree = None;
        }

        if let Some(tree) = &self.tree {
            let root_node = tree.root_node();
            CommonLispGrammar::<Statement>::from_node(&root_node, &self.buffer) 
        } else {
            None
        }
    }
    
    pub fn query(&self, query: &str) -> Result<Query, QueryError> {
        tree_sitter::Query::new(self.language, query)
    }

    pub fn query_pattern(&self, pattern: &str, names: &[&str]) -> Result<HashMap<String, Vec<usize>>, QueryError> {
        let names_str: Vec<String> = names.iter().map(|n| format!("({})", n)).collect();
        let q_str = format!("[{}] @{}", names_str.join("\n"), pattern);

        let mut ret_map: HashMap<String, Vec<usize>> = HashMap::new();

        let patterns = self.query(&q_str)?;
        let cap_names = patterns.capture_names();
        for i in 0..cap_names.len() {
            let pat_idx = patterns.capture_index_for_name(&cap_names[i]);
            if pat_idx.is_none() {
                continue;
            }
            let pat_idx = pat_idx.unwrap() as usize;

            let start_byte = patterns.start_byte_for_pattern(pat_idx);

            if ret_map.contains_key(&cap_names[i]) {
                ret_map.get_mut(&cap_names[i]).unwrap().push(start_byte);
            } else {
                ret_map.insert(cap_names[i].clone(), vec![start_byte]);
            }
        }

        Ok(ret_map)
    }

    pub fn reset(&mut self) {
        self.tree = None;
        self.parser.reset();
        self.buffer = String::with_capacity(0);
    }
}
