mod parse_grammar;
mod rules;
mod grammars;
mod frontend_language;
mod nfa;

pub use frontend_language::FrontendLanguage;

use std::{
    rc::Rc, 
    collections::{
        HashMap,
        HashSet,
    },
};

use tree_sitter::{
    Parser, 
    Language,
    Tree,
    Query,
    QueryError,
    Node,
};

use grammars::Variable;

use self::rules::Rule;

pub struct Frontend {
    parser: Parser,
    language: Language,
    grammar: HashMap<String, Variable>,
    tree: Option<Rc<Tree>>,
    buffer: String,
}

impl Frontend {
    pub fn from_language(language: FrontendLanguage) -> Result<Self, String> {
        let grammar = language.ts_grammar();
        let language = language.ts_language();

        let mut parser = Parser::new();
        parser.set_language(language)
            .map_err(|e| e.to_string())?;

        Ok(Self {
            parser,
            language,
            grammar,
            buffer: String::with_capacity(0),
            tree: None,
        })
    }

    pub fn parse(&mut self, code: &str) {
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

        self.walk_parsed().iter().for_each(|sexp| println!("{}", sexp));
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

    fn process_node(&self, n: &Node) -> String {
        let node_text = &self.buffer[n.start_byte()..n.end_byte()];

        if n.is_error() {
            format!("{} Error", node_text)
        }
        else if n.is_extra() {
            format!("{} Extra", node_text)
        }
        else if n.is_missing() {
            format!("{} Missing", node_text)
        }
        else if !n.is_named() {
            format!("{} Unnamed", node_text)
        }
        else if let Some(variable) = self.grammar.get(n.kind()) {
            let rule = match &variable.rule {
                Rule::String(s) => s.clone(),
                Rule::Blank => "Blank".to_string(),
                Rule::Seq(s) => format!("{:?}", s),
                Rule::Symbol(s) => format!("{:?}", s),
                Rule::Choice(c) => format!("{:?}", c),
                Rule::Repeat(c) => format!("{:?}", c),
                Rule::Pattern(p) => format!("{:?}", p),
                Rule::NamedSymbol(ns) => format!("{:?}", ns),
                Rule::Metadata { params, rule } => format!("@ {:?} = {:?}", params, rule),
            };

            format!("{} [{}]", node_text, rule)
        }
        else {
            format!("{} Unknown: {}", node_text, n.kind())
        }
        //println!("{}, [{}, {}] = {}, \"{}\"", n.kind(), n.start_position(), n.end_position(), n.to_sexp(), node_text);
    }

    fn walk_parsed(&self) -> Vec<String> {
        if let Some(tree) = &self.tree {
            let mut cursor = tree.walk();
            let mut nodes = vec![];

            loop {
                // goto leaf
                if cursor.goto_first_child() {
                    continue;
                }
                
                let node = cursor.node();

                if cursor.goto_next_sibling() {
                    nodes.push(node);
                    continue;
                }

                loop {
                    nodes.push(cursor.node());

                    if !cursor.goto_parent() {
                        // back at root, done
                        return nodes.iter().map(|n| self.process_node(n)).collect();
                    }

                    let node = cursor.node();
                    
                    if cursor.goto_next_sibling() {
                        nodes.push(node);
                        break;
                    }
                }
            }
        }
        else {
            return vec![]
        }
    }
}
