use logos::Lexer;
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    // keywords
    #[token("pub")]
    Pub,
    #[token("priv")]
    Priv,
    #[token("fn")]
    Func,
    #[token("class")]
    Class,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("return")]
    Return,
    #[token("match")]
    Match,
    #[token("let")]
    Let,
    #[token("break")]
    Break,
    #[token("loop")]
    Loop,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    
    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,
    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("[")]
    LBRACKET,
    #[token("]")]
    RBRACKET,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("=")]
    Assign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("_")]
    Underscore,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("->")]
    RightArrow,
    #[token("=>")]
    FatArrow,

    #[regex(r"[a-zA-Z_]+", |lex| lex.slice().to_owned())]
    Ident(String),

    #[regex(r"//[^\n]+", |lex| lex.slice().to_owned())]
    LineComment(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Decimal(i32),

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

fn line_comment(lexer: &mut Lexer<Token>) -> Option<String> {
    let s = lexer.slice();
    eprintln!("{}", s);
    Some(s.to_owned())
}

