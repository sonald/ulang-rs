use crate::lexer::*;
use logos::{Logos, Lexer};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub ulang);

pub mod ast {
use std::fmt::{Display, Formatter, Error};

#[derive(Debug)]
pub struct CompilationUnit(pub Vec<Term>);

#[derive(Debug)]
pub enum Term {
    Class(ClassDefinition),
    Func(FuncDefinition),
}

#[derive(Debug)]
pub struct ClassDefinition {
    pub name: String,
    pub funcs: Option<Vec<FuncDefinition>>
}

//TODO: need real type annotation
pub type TypeName = String;
pub type SymbolName = String;

#[derive(Debug)]
pub struct Symbol {
    pub name: SymbolName,
    pub ty: TypeName,
}

#[derive(Debug)]
pub struct StatementBlock(pub Vec<Statement>);

#[derive(Debug)]
pub struct ExpressionList(pub Vec<Expression>);

#[derive(Debug)]
pub struct FuncDefinition {
    pub name: String,
    pub args: Vec<Symbol>,
    pub rettype: Option<TypeName>,
    pub statements: StatementBlock,
}

#[derive(Debug)]
pub enum Statement {
    Let {
        val: SymbolName,
        init: Option<Expression>
    },
    Expr(Expression),
    Loop(StatementBlock),
    Conditional {
        predicate: Expression,
        positive: StatementBlock,
        negative: StatementBlock
    },
    Return(Option<Expression>),
    Break,
    MatchArm {
        condition: Expression,
        stat: StatementBlock,
    },
    Match {
        expr: Expression,
        arms: StatementBlock
    },
    For {
        var: SymbolName,
        expr: SymbolName,
        block: StatementBlock
    },
    Assignment {
        lhs: Expression,
        op: Operator,
        rhs: Expression,
    },
    Invalid
}

#[derive(Debug)]
pub enum Operator {
    // binary
    Plus,
    Minus,
    Divide,
    Multiply,

    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Assign,
    PlusAssign,
    MinusAssign,

    // unary
    Negative,
}

#[derive(Debug)]
pub struct TypeConstruct {
    pub name: String
}

#[derive(Debug)]
pub enum Expression {
    Invalid(String),
    UnaryExpr {
        op: Operator,
        expr: Box<Expression>
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>
    },
    SymbolExpr(SymbolName),
    FieldExpr {
        object: Box<Expression>,
        field: Box<Expression>
    },
    DecimalExpr(i32),
    TypeExpr(TypeConstruct),
    Tuple(ExpressionList),
    CallExpr {
        callee: Box<Expression>,
        args: ExpressionList
    },
    RangeExpr {
        start: Option<Box<Expression>>,
        end: Option<Box<Expression>>,
    },
    IndexExpr {
        object: Box<Expression>,
        index: Box<Expression>
    },
    Block {
        stats: Vec<Statement>,
        expr: Box<Expression>
    }
}

impl Display for CompilationUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for t in &self.0 {
            write!(f, "{}\n", t)?;
        }
        Ok(())
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Term::Class(ref c) => write!(f, "{}\n", c),
            Term::Func(ref g) => write!(f, "{}\n", g),

        }
    }
}

impl Display for FuncDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        //FIXME: terrible way
        let args = self.args.iter().map(|s| format!("{} {}", s.name, s.ty)).collect::<Vec<_>>().join(",");
        let ret = self.rettype.as_ref().map_or("".to_owned(), |r| format!(" -> {}", r));
        write!(f, "fn {} ({}){} \n", self.name, args, ret)?;
        write!(f, "{}", self.statements)
    }
}

impl Display for ClassDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "class {} {{\n", self.name)?;
        if let Some(ref funcs) = self.funcs {
            for g in funcs {
                write!(f, "{}\n", g)?;
            }
        }
        write!(f, "}}")
    }
}


impl Display for StatementBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{{\n")?;
        for s in &self.0 {
            write!(f, "{}", s)?;
        }

        write!(f, "}}")
    }
}

impl Display for ExpressionList {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for s in &self.0 {
            write!(f, "{},", s)?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Statement::Let {val, init} => write!(f, "let {} = {};\n", val, init.as_ref().unwrap()),
            Statement::Loop(v) => write!(f, "loop {}\n", v),
            Statement::Conditional {predicate, positive, negative} => 
                write!(f, "if {} {} else {}\n", predicate, positive, negative),
            Statement::Return(e) => write!(f, "return {};\n", e.as_ref().map_or("".to_owned(), |e| format!("{}", e))),
            Statement::Break => write!(f, "break;\n"),
            Statement::For {var, expr, block} => write!(f, "for {} in {} {}\n", var, expr, block),
            Statement::Match{expr, arms} => write!(f, "match {} {}\n", expr, arms),
            Statement::MatchArm{condition, stat} => write!(f, "{} => {},\n", condition, stat),
            Statement::Assignment{lhs, op, rhs} => write!(f, "{} {} {};\n", lhs, op, rhs),
            Statement::Expr(e) => write!(f, "{}\n", e),
            _ => write!(f, "<S>\n")
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Divide => write!(f, "/"),
            Operator::Multiply => write!(f, "*"),

            Operator::Equal => write!(f, "=="),
            Operator::NotEqual => write!(f, "!="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),

            Operator::Assign => write!(f, "="),
            Operator::PlusAssign => write!(f, "+="),
            Operator::MinusAssign => write!(f, "-="),

            _ => unimplemented!(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Expression::*;
        match self {
            UnaryExpr{op, expr} => write!(f, "{}{}", op, expr),
            BinaryExpr{op, lhs, rhs} => write!(f, "{} {} {}", lhs, op, rhs),
            SymbolExpr(sym) => write!(f, "{}", sym),
            DecimalExpr(d) => write!(f, "{}", d),
            TypeExpr(ty) => write!(f, "{}()", ty.name),
            FieldExpr{object, field} => write!(f, "{}.{}", object, field),
            Tuple(v) => write!(f, "{}", v),
            CallExpr{callee, args} => write!(f, "{}({})", callee, args),
            RangeExpr{start, end} => 
                write!(f, "{}:{}", 
                    start.as_ref().map_or("".to_owned(), |e| format!("{}", e)),
                    end.as_ref().map_or("".to_owned(), |e| format!("{}", e))),
            IndexExpr{object, index} => write!(f, "{}[{}]", object, index),
            Block{stats, expr} => {
                write!(f, "{{\n")?;
                for s in stats {
                    write!(f, "{}", s)?;
                }
                write!(f, "{}\n}}", expr)
            },
            e => write!(f, "<{:?}>", e),
        }
    }
}

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

    pub fn parse(&mut self) -> ast::CompilationUnit {
        let lex = self.lex.clone().spanned().map(|(tok, range)| (range.start, tok, range.end));
        let res = ulang::ulangParser::new().parse(lex);
        //println!("{:?}", res);
        res.unwrap()
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
