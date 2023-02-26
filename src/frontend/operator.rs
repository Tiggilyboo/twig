
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    Add,
    Mul,
    Sub,
    Div,
    Mod,
    Exp,
}

impl TryFrom<&str> for Operator {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "+" => Ok(Operator::Add),
            "-" => Ok(Operator::Sub),
            "*" => Ok(Operator::Mul),
            "/" => Ok(Operator::Div),
            "%" => Ok(Operator::Mod),
            "^" => Ok(Operator::Exp),
            _ => Err(format!("Unsupported operator: {}", s)),
        }
    }
}

