extern crate tree_sitter;
extern crate tree_sitter_commonlisp;

use std::{rc::{
    Weak,
    Rc,
}, any::Any};

use tree_sitter::{
    Parser, 
    Language,
    Tree,
};

pub struct Frontend {
    parser: Parser,
    language: Language,
    tree: Option<Rc<Tree>>,
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
    
    pub fn walk_parsed(&self) -> Vec<String> {

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
                    println!("sibling");
                    nodes.push(node);
                    continue;
                }

                loop {
                    println!("inner loop");
                    nodes.push(cursor.node());

                    if !cursor.goto_parent() {
                        // back at root, done
                        return nodes.iter().map(|n| format!("{}, {}, {}", n.id(), n.kind(), n.to_sexp())).collect();
                    }

                    let node = cursor.node();
                    
                    if cursor.goto_next_sibling() {
                        println!("inner sibling");
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
