mod frontend_grammar;
mod operator;
mod comparator;
mod literal;
mod expression;

use std::rc::Rc;

use tree_sitter::{
    Parser, 
    Language,
    Tree,
};

use self::frontend_grammar::{
    FrontendGrammar, 
    CommonLispGrammar,
};
pub use self::expression::*;
pub use self::literal::*;
pub use self::operator::*;
pub use self::comparator::*;

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

    pub fn parse(&mut self, code: &str) -> Option<Expression> {
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
            CommonLispGrammar::<Expression>::from_node(&root_node, &self.buffer) 
        } else {
            None
        }
    }

    pub fn reset(&mut self) {
        self.tree = None;
        self.parser.reset();
        self.buffer = String::with_capacity(0);
    }
}
