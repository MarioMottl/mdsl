use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    fn lex(src: &str) -> Vec<Token> {
        Token::lexer(src).filter_map(|r| r.ok()).collect()
    }

    #[test]
    fn test_simple_var_binding() {
        let input = "var foo = 123;";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Var,
                Token::Identifier, // "foo"
                Token::Equal,
                Token::Integer(123),
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn test_simple_val_binding() {
        let input = "val foo = 123;";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Val,
                Token::Identifier, // "foo"
                Token::Equal,
                Token::Integer(123),
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn test_float_and_ops() {
        let input = "x = 3.14 * (a + b);";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier,
                Token::Equal,
                Token::Float(3.14),
                Token::Star,
                Token::LeftParen,
                Token::Identifier,
                Token::Plus,
                Token::Identifier,
                Token::RightParen,
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn test_string_and_char_literals() {
        let input = r#"'c' "hello\nworld""#;
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::CharLiteral('c'),
                Token::StringLiteral(String::from("hello\\nworld")),
            ]
        );
    }

    #[test]
    fn test_comments_and_whitespace_are_skipped() {
        let input = r#"
            // this is a line comment
            if(x){ }
        "#;
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::LeftParen,
                Token::Identifier,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
            ]
        );
    }
}
