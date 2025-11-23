use chumsky::{Parser, error::Rich, extra, prelude::*};
use internment::Intern;
use std::fmt::Display;

use crate::{ast, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Open(Delimiter),
    Close(Delimiter),
    Ident(ast::Ident),
    Literal(LitToken),
    Op(Op),
    Colon,
    Semicolon,
}

pub type SpannedToken = (Token, Span);

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{}", keyword),
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::Literal(lit_token) => write!(f, "{}", lit_token),
            Token::Open(Delimiter::Parent) => write!(f, "("),
            Token::Open(Delimiter::Brace) => write!(f, "{{"),
            Token::Open(Delimiter::Bracket) => write!(f, "["),
            Token::Close(Delimiter::Parent) => write!(f, ")"),
            Token::Close(Delimiter::Brace) => write!(f, "}}"),
            Token::Close(Delimiter::Bracket) => write!(f, "]"),
            Token::Op(op) => write!(f, "{}", op),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitToken {
    Number(i64),
    Float(Intern<String>),
    String(Intern<String>),
}

impl Display for LitToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitToken::Number(n) => write!(f, "{}", n),
            LitToken::Float(s) => write!(f, "{}", s),
            LitToken::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Delimiter {
    Parent,
    Brace,
    Bracket,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Int,
    Float,
    Bool,
    String,
    Void,
    Nil,
    True,
    False,
    Fn,
    Return,
    Let,
    Var,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Match,
    Pub,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Int => write!(f, "int"),
            Keyword::Float => write!(f, "float"),
            Keyword::Bool => write!(f, "bool"),
            Keyword::String => write!(f, "string"),
            Keyword::Void => write!(f, "void"),
            Keyword::Nil => write!(f, "nil"),
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Return => write!(f, "return"),
            Keyword::Let => write!(f, "let"),
            Keyword::Var => write!(f, "var"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::While => write!(f, "while"),
            Keyword::For => write!(f, "for"),
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Match => write!(f, "match"),
            Keyword::Pub => write!(f, "pub"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    Not,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ThinArrow,
    FatArrow,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Mod => write!(f, "%"),
            Op::Eq => write!(f, "=="),
            Op::Ne => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Gt => write!(f, ">"),
            Op::LtEq => write!(f, "<="),
            Op::GtEq => write!(f, ">="),
            Op::And => write!(f, "&&"),
            Op::Or => write!(f, "||"),
            Op::Not => write!(f, "!"),
            Op::Assign => write!(f, "="),
            Op::AddAssign => write!(f, "+="),
            Op::SubAssign => write!(f, "-="),
            Op::MulAssign => write!(f, "*="),
            Op::DivAssign => write!(f, "/="),
            Op::ThinArrow => write!(f, "->"),
            Op::FatArrow => write!(f, "=>"),
        }
    }
}

pub fn tokenize(program: &str) -> Result<Vec<SpannedToken>, Vec<Rich<'_, char>>> {
    lexer().parse(program).into_result()
}

type Extra<'src> = extra::Full<Rich<'src, char>, (), ()>;

fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<SpannedToken>, Extra<'src>> {
    choice((line_comment().to(None), token().map(Some)))
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .map(|items| items.into_iter().flatten().collect::<Vec<_>>())
        .then_ignore(end())
}

fn token<'src>() -> impl Parser<'src, &'src str, SpannedToken, Extra<'src>> {
    choice((
        keyword(),
        delimiter(),
        literal(),
        ident(),
        op(),
        punctuation(),
    ))
    .map_with(|tok, e| {
        let span = e.span();
        (
            tok,
            Span {
                start: span.start,
                end: span.end,
            },
        )
    })
}

fn keyword<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((
        just("pub").to(Keyword::Pub),
        just("let").to(Keyword::Let),
        just("var").to(Keyword::Var),
        just("if").to(Keyword::If),
        just("else").to(Keyword::Else),
        just("while").to(Keyword::While),
        just("for").to(Keyword::For),
        just("break").to(Keyword::Break),
        just("continue").to(Keyword::Continue),
        just("match").to(Keyword::Match),
        just("fn").to(Keyword::Fn),
        just("return").to(Keyword::Return),
        just("int").to(Keyword::Int),
        just("float").to(Keyword::Float),
        just("bool").to(Keyword::Bool),
        just("string").to(Keyword::String),
        just("void").to(Keyword::Void),
        just("nil").to(Keyword::Nil),
        just("true").to(Keyword::True),
        just("false").to(Keyword::False),
    ))
    .map(Token::Keyword)
}

fn delimiter<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((open_delimiter(), close_delimiter()))
}

fn open_delimiter<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((
        just("(").to(Delimiter::Parent),
        just("{").to(Delimiter::Brace),
        just("[").to(Delimiter::Bracket),
    ))
    .map(Token::Open)
}

fn close_delimiter<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((
        just(")").to(Delimiter::Parent),
        just("}").to(Delimiter::Brace),
        just("]").to(Delimiter::Bracket),
    ))
    .map(Token::Close)
}

fn literal<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((lit_integer(), lit_float(), lit_string())).map(Token::Literal)
}

fn lit_integer<'src>() -> impl Parser<'src, &'src str, LitToken, Extra<'src>> {
    text::int(10)
        .map(|s: &str| s.parse().unwrap())
        .map(LitToken::Number)
}

fn lit_float<'src>() -> impl Parser<'src, &'src str, LitToken, Extra<'src>> {
    text::int(10)
        .then(just('.'))
        .then(text::digits(10))
        .to_slice()
        .map(|s: &str| Intern::new(s.to_string()))
        .map(LitToken::Float)
}

fn lit_string<'src>() -> impl Parser<'src, &'src str, LitToken, Extra<'src>> {
    just("\"")
        .ignore_then(none_of("\"").repeated().collect::<String>())
        .then_ignore(just("\""))
        .map(|s| Intern::new(s))
        .map(LitToken::String)
}

fn ident<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    text::ident()
        .map(|s: &str| ast::Ident(Intern::new(s.to_string())))
        .map(Token::Ident)
}

fn op<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((
        // complex op
        just("==").to(Op::Eq),
        just("!=").to(Op::Ne),
        just("<=").to(Op::LtEq),
        just(">=").to(Op::GtEq),
        just("&&").to(Op::And),
        just("||").to(Op::Or),
        just("+=").to(Op::AddAssign),
        just("-=").to(Op::SubAssign),
        just("*=").to(Op::MulAssign),
        just("/=").to(Op::DivAssign),
        just("->").to(Op::ThinArrow),
        just("=>").to(Op::FatArrow),
        // simple op
        just("+").to(Op::Add),
        just("-").to(Op::Sub),
        just("*").to(Op::Mul),
        just("/").to(Op::Div),
        just("%").to(Op::Mod),
        just("<").to(Op::Lt),
        just(">").to(Op::Gt),
        just("!").to(Op::Not),
        just("=").to(Op::Assign),
    ))
    .map(Token::Op)
}

fn line_comment<'src>() -> impl Parser<'src, &'src str, (), Extra<'src>> {
    just("//").ignore_then(none_of("\n").repeated()).ignored()
}

fn punctuation<'src>() -> impl Parser<'src, &'src str, Token, Extra<'src>> {
    choice((just(":").to(Token::Colon), just(";").to(Token::Semicolon)))
}
