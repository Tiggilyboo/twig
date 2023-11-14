use core::fmt;
use std::char;
use std::path::Iter;
use std::slice::Windows;

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

#[derive(Debug, Clone)]
pub enum Comparator {
    Not,
    EqualTo,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
}

#[derive(Debug, Clone)]
pub enum Token {
    Open,
    Close,
    Numeric(u32),
    Quote,
    AlphaNumeric(char),
    String(String),
    ConditionOpen,
    Comparator(Comparator),
    Operation(Operator),
    Define,
    Dot,
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

fn tokenize(ch: char) -> Option<Token> {
    let cur = match ch {
        '(' => Token::Open,
        ')' => Token::Close,
        '?' => Token::ConditionOpen,
        '!' => Token::Comparator(Comparator::Not),
        '=' => Token::Comparator(Comparator::EqualTo),
        '>' => Token::Comparator(Comparator::Greater),
        '<' => Token::Comparator(Comparator::Less),
        '+' => Token::Operation(Operator::Add),
        '-' => Token::Operation(Operator::Sub),
        '/' => Token::Operation(Operator::Div),
        '*' => Token::Operation(Operator::Mul),
        '%' => Token::Operation(Operator::Mod),
        ':' => Token::Define,
        '.' => Token::Dot,
        '"' => Token::Quote,
        'a'..='z' | 'A'..='Z' | '0'..='9' => Token::AlphaNumeric(ch),
        ' ' | '\t' | '\n' => Token::Delimiter,
        _ => return None,
    };

    Some(cur)
}

pub struct Parser {
    tokens: Vec<Token>,
    stack: Vec<(usize, Token)>,
    lines: Vec<usize>,
    pairs: Vec<(usize, usize)>,
    chunk: Vec<Token>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            stack: Vec::new(),
            lines: Vec::new(),
            pairs: Vec::new(),
            chunk: Vec::new(),
        }
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }

    fn parse_single(
        &mut self,
        pos: usize,
        last: Option<&Token>,
        cur: Option<Token>,
        next: Option<&Token>,
    ) -> Result<Option<Token>, ParseErr> {
        if let Some(cur) = cur {
            match (last, cur.clone(), next) {
                (_, Token::Open, _) => {
                    self.stack.push((pos, cur.clone()));
                    Ok(None)
                }
                (_, Token::Close, _) => match self.stack.pop() {
                    Some((start, _)) => {
                        self.pairs.push((start, pos));
                        Ok(None)
                    }
                    None => {
                        return Err(ParseErr::Error(format!(
                            "Unmatched closing bracket at L{}:{}",
                            self.lines.last().unwrap_or(&0),
                            pos
                        )))
                    }
                },
                (
                    _,
                    Token::Comparator(Comparator::Greater),
                    Some(Token::Comparator(Comparator::EqualTo)),
                ) => Ok(Some(Token::Comparator(Comparator::GreaterEq))),
                (
                    _,
                    Token::Comparator(Comparator::Less),
                    Some(Token::Comparator(Comparator::EqualTo)),
                ) => Ok(Some(Token::Comparator(Comparator::LessEq))),
                (
                    _,
                    Token::Comparator(Comparator::Not),
                    Some(Token::Comparator(Comparator::EqualTo)),
                ) => Ok(Some(Token::Comparator(Comparator::NotEq))),
                (_, Token::AlphaNumeric(first_an), Some(Token::AlphaNumeric(_))) => {
                    if let Some(token) = tokenize(first_an) {
                        self.chunk.push(token);
                    }
                    Ok(None)
                }
                (_, Token::AlphaNumeric(_), Some(Token::Delimiter)) => {
                    if !self.chunk.is_empty() {
                        let combined = combine_alpha_numeric(self.chunk.as_slice());
                        self.chunk.clear();
                        Ok(Some(combined))
                    } else {
                        Ok(None)
                    }
                }
                (_, token, _) => Ok(Some(token)),
            }
        } else {
            Ok(None)
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<(), ParseErr> {
        let mut pos = 0;
        let mut input_iter = input.char_indices();

        let mut last: Option<Token> = None;
        let mut cur: Option<Token> = input_iter.next().map(|(_, ch)| tokenize(ch)).flatten();
        let mut next: Option<Token> = input_iter.next().map(|(_, ch)| tokenize(ch)).flatten();

        while cur.is_some() {
            println!("{:?}, {:?}, {:?}", last, cur, next);

            match self.parse_single(pos, last.as_ref(), cur.clone(), next.as_ref()) {
                Ok(Some(token)) => self.tokens.push(token),
                Ok(None) => (),
                Err(err) => return Err(err),
            }

            last = cur;
            cur = next;
            if let Some(next_iter) = input_iter.next() {
                pos = next_iter.0;
                next = tokenize(next_iter.1);

                if next_iter.1 == '\n' {
                    self.lines.push(pos);
                }
            } else {
                pos += 1;
                next = None;
            }
        }

        let mut stack_error = String::new();
        while !self.stack.is_empty() {
            if let Some((_, token)) = self.stack.pop() {
                stack_error.push_str(&format!("Syntax error: {:?}\n", token))
            }
        }
        if !stack_error.is_empty() {
            return Err(ParseErr::Error(stack_error));
        }

        Ok(())
    }
}

pub fn parse(input: &str) -> Result<Vec<Token>, ParseErr> {
    let mut parser = Parser::new();

    match parser.parse(input) {
        Ok(_) => Ok(parser.tokens()),
        Err(err) => Err(err),
    }
}
