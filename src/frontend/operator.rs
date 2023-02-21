
pub enum Operator {
    None,

    Add,
    Mul,
    Sub,
    Div,
    Mod,
    Exp,
}

impl Default for Operator {
    fn default() -> Self {
        Operator::None
    }
}

impl From<&str> for Operator {
    fn from(s: &str) -> Self {
        match s {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "^" => Operator::Exp,
            _ => Operator::None,
        }
    }
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}
