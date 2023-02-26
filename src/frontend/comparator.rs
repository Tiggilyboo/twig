
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Comparator {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl TryFrom<&str> for Comparator {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "=" => Ok(Comparator::Equal),
            "/=" => Ok(Comparator::NotEqual),
            ">" => Ok(Comparator::GreaterThan),
            "<" => Ok(Comparator::LessThan),
            ">=" => Ok(Comparator::GreaterThanEqual),
            "<=" => Ok(Comparator::LessThanEqual),
            _ => Err(format!("Unsupported comparator: {}", s)),
        }
    }
}

