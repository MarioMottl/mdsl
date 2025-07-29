use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
// skip whitespace and comments
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*+[^*/])*\*+\*/")]
pub enum Token {
    // punctuation
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("?")]
    Question,

    // operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,
    #[token("<<=")]
    ShiftLeftAssign,
    #[token(">>=")]
    ShiftRightAssign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    CaretAssign,

    // literals
    // note: float pattern first so you don't lex "123.45" as Integer("123") + Dot + Integer("45")
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        // strip quotes
        lex.slice()[1..lex.slice().len() - 1].to_string()
    })]
    StringLiteral(String),

    #[regex(r"'([^'\\]|\\.)'", |lex| {
        lex.slice()[1..lex.slice().len() - 1].chars().next().unwrap()
    })]
    CharLiteral(char),

    // keywords & identifiers
    #[token("val")]
    Val,
    #[token("var")]
    Var,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("match")]
    Match,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
}

#[allow(unused)]
#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Assignment = 1,
    Ternary = 2,
    Or = 3,
    And = 4,
    Equality = 5,
    Comparison = 6,
    Term = 7,
    Factor = 8,
    Prefix = 9,
    Call = 10,
    Primary = 11,
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        use Token::*;
        match self {
            Equal | EqualEqual | BangEqual => Precedence::Equality,
            Less | LessEqual | Greater | GreaterEqual => Precedence::Comparison,
            Plus | Minus => Precedence::Term,
            Star | Slash | Percent => Precedence::Factor,
            AndAnd => Precedence::And,
            OrOr => Precedence::Or,
            Question => Precedence::Ternary,
            _ => Precedence::Lowest,
        }
    }
}
