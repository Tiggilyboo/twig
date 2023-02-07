extern crate tree_sitter;
extern crate tree_sitter_commonlisp;

use tree_sitter::{
    Parser, 
    Language,
    Node,
    Tree,
    TreeCursor,
};

pub struct Frontend {
    parser: Parser,
    language: Language,
    tree: Option<Tree>,
}

impl Default for Frontend {
    fn default() -> Self {
        Frontend::from_language(tree_sitter_commonlisp::language())
            .unwrap()
    }
}

impl Frontend {
    pub fn from_language(language: Language) -> Result<Self, String> {
        let mut parser = Parser::new();
        parser.set_language(language)
            .map_err(|e| e.to_string())?;

        Ok(Self {
            parser,
            language,
            tree: None,
        })
    }

    pub fn parse(&mut self, code: &str) {
        if let Some(new_tree) = self.parser.parse(code, self.tree.as_ref()) {
            self.tree = Some(new_tree);
        } else {
            self.tree = None;
        }

        self.walk_parsed().iter().for_each(|sexp| println!("{}", sexp));
    }
    
    pub fn walk_parsed(&mut self) -> Vec<String> {
        if let Some(tree) = &mut self.tree {
            let mut cursor = tree.walk();
            let mut nodes = Vec::new();
            let mut break_outer = false;
            
            loop {
                let node = cursor.node();

                if cursor.goto_first_child() || cursor.goto_next_sibling() {
                    nodes.push(node);
                }

                if break_outer {
                    break;
                }

                loop {
                    if !cursor.goto_parent() {
                        break_outer = true;
                        break;
                    }

                    if cursor.goto_next_sibling() {
                        break;
                    }
                }

                nodes.push(node);
            }

            nodes.iter().map(|n| n.to_sexp()).collect()
        } else {
            vec![]
        }
    }
}
