use logos::Lexer;
use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum UlangToken<'source> {
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
    #[token("continue")]
    Continue,
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
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
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

    #[regex(r"int|string|int32|bool")]
    PrimType(&'source str),

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", |lex| lex.slice())]
    Ident(&'source str),

    #[regex(r"//[^\n]+", line_comment)]
    LineComment(&'source str),

    #[regex(r"[0-9]+")]
    Decimal(&'source str),

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

fn line_comment<'source>(lexer: &mut Lexer<'source, UlangToken<'source>>) -> Option<&'source str> {
    let s = lexer.slice();
    Some(s)
}

