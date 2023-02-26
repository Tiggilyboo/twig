use crate::frontend::{
    Expression,
    Literal,
    Operator,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Invoke {
        identifier: String,
        args: Option<Expression>,
    },
    Define {
        identifier: String,
        args: Option<Expression>,
        body: Expression,
    },
    Condition {
        test: Expression,
        truthy: Option<Expression>,
        falsey: Option<Expression>, 
    },
    Operation(Operator, Vec<Literal>),
}

