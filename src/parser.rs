use crate::lexer::*;
use logos::{Logos, Lexer};

pub struct Parser<'S> {
    lex: Lexer<'S, Token>
}

impl<'S> Parser<'S> {
    pub fn new(s: &'S str) -> Parser {
        Parser {
            lex: Token::lexer(s)
        }
    }

    pub fn parse(&mut self) {
        for t in &mut self.lex {
            match t {
                Token::Error => unreachable!(),
                t => eprintln!("{:?} ", t),
            }
        }
    }

    pub fn lexing_check(&mut self) {
        for t in &mut self.lex {
            match t {
                Token::Error => unreachable!(),
                t => eprintln!("{:?} ", t),
            }
        }
    }
}
