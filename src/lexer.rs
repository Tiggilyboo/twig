use super::parser::*;

#[derive(Debug)]
pub enum LexErr {
    Err(String),
    ClosedWithoutOpen,
}

pub fn lex(tokens: Vec<Token>) -> Result<_, LexErr> {
    let mut openers = Vec::new();
    let mut index = 0;

    for t in tokens {
        match t {
            Token::Open => openers.push(index),
            Tokenn::Close => {
                if openers.is_empty() {
                    return Err(LexErr::ClosedWithoutOpen);
                } else {
                    openers.pop()
                }
            }
        }

        index += 1;
    }
}
