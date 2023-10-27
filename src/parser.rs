use core::fmt;
use std::{char, io};

/*
(?= 3 (+ 1 2))
(. "Hello World")
(:Chicken "Bawk")
(:Sum (:a :b :c) (+ a b c))
*/

#[derive(Debug)]
pub enum ParseErr {
    Error(String),
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug)]
pub enum Comparator {
    EqualTo,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(Debug)]
pub enum Token {
    Open,
    Close,
    Numeric(u32),
    Quote,
    AlphaNumeric(char),
    String(String),
    ConditionOpen,
    Condition(Comparator),
    Operation(Operator),
    Define,
    Invoke,
    Delimiter,
}

fn combine_alpha_numeric(tokens: &[Token]) -> Token {
    let mut acc = 0;

    for t in tokens {
        match t {
            Token::AlphaNumeric(an) => {
                if let Some(d) = char::to_digit(*an, 10) {
                    acc = acc * 10 + d;
                } else {
                    return combine_alpha(tokens);
                }
            }
            _ => (),
        }
    }

    Token::Numeric(acc)
}

fn combine_alpha(tokens: &[Token]) -> Token {
    let mut buf = String::with_capacity(tokens.len());

    for t in tokens {
        match t {
            Token::AlphaNumeric(an) => buf.push(*an),
            _ => panic!("Expected alphanumeric token"),
        }
    }

    Token::String(buf)
}

pub fn parse(input: &str) -> Result<Vec<Token>, ParseErr> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chunk: Vec<Token> = Vec::new();

    for ch in input.chars() {
        let cur = match ch {
            '(' => Token::Open,
            ')' => Token::Close,
            '?' => Token::ConditionOpen,
            '=' => Token::Condition(Comparator::EqualTo),
            '>' => Token::Condition(Comparator::Greater),
            '<' => Token::Condition(Comparator::Less),
            '+' => Token::Operation(Operator::Add),
            '-' => Token::Operation(Operator::Sub),
            '/' => Token::Operation(Operator::Div),
            '*' => Token::Operation(Operator::Mul),
            ':' => Token::Define,
            '.' => Token::Invoke,
            '"' => Token::Quote,
            'a'..='z' | 'A'..='Z' | '0'..='9' => Token::AlphaNumeric(ch),
            ' ' | '\t' | '\n' => Token::Delimiter,
            _ => continue,
        };

        match (chunk.last(), &cur) {
            (_, Token::AlphaNumeric(_)) => {
                chunk.push(cur);
            }
            (Some(Token::AlphaNumeric(_)), _) => {
                let combined = combine_alpha_numeric(chunk.as_slice());
                tokens.push(combined);
                tokens.push(cur);
                chunk.clear();
            }
            _ => tokens.push(cur),
        }
    }

    Ok(tokens)
}
