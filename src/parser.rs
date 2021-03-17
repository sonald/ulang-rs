use crate::lexer::*;
use logos::{Logos, Lexer};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub ulang);

#[derive(Debug)]
pub struct CompilationUnit(Vec<Term>);

#[derive(Debug)]
pub enum Term {
    ClassDefinition,
    FuncDefinition,
}

#[derive(Debug)]
pub struct ClassDefinition {
}

#[derive(Debug)]
pub struct FuncDefinition {
}

#[derive(Debug)]
pub enum Statement {
}

#[derive(Debug)]
pub enum LetStatement {
}

#[derive(Debug)]
pub enum Expr {
}

pub struct UlangParser<'s> {
    lex: Lexer<'s, UlangToken<'s>>
}

impl<'s> UlangParser<'s> {
    pub fn new(s: &'s str) -> UlangParser {
        UlangParser {
            lex: UlangToken::lexer(s)
        }
    }

    pub fn parse(&mut self) {
        let lex = self.lex.clone().spanned().map(|(tok, range)| (range.start, tok, range.end));
        let res = ulang::ulangParser::new().parse(lex);
        println!("{:?}", res);
    }

    pub fn lexing_check(&mut self) {
        for t in self.lex.clone() {
            match t {
                UlangToken::Error => {
                    eprintln!("{:?} ", t);
                    unreachable!()
                },
                t => eprintln!("{:?} ", t),
            }
        }
    }
}
