use crate::token::Token;
use logos::Logos;
use std::ops::Range;

#[allow(unused)]
#[derive(Debug)]
pub struct LexError {
    pub span: Range<usize>,
    pub fragment: String,
}

#[derive(Debug)]
pub struct LexResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}

pub fn lex_with_errors(input: &str) -> LexResult {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    while let Some(next) = lexer.next() {
        match next {
            Ok(tok) => tokens.push(tok),
            Err(()) => {
                // logos::Lexer gives us the span() and slice() for *this* token
                let span = lexer.span();
                let fragment = lexer.slice().to_string();

                errors.push(LexError { span, fragment });
                // advance past this bad char so we don't loop forever
                // (logos has already advanced one code‐point, so nothing else
                // to do here)
            }
        }
    }

    LexResult { tokens, errors }
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
                Token::Identifier("foo".into()),
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
                Token::Identifier("foo".into()),
                Token::Equal,
                Token::Integer(123),
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn test_float_and_ops() {
        let input = "x = 8.43 * (a + b);";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".into()),
                Token::Equal,
                Token::Float(8.43),
                Token::Star,
                Token::LeftParen,
                Token::Identifier("a".into()),
                Token::Plus,
                Token::Identifier("b".into()),
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
                Token::StringLiteral("hello\\nworld".into()),
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
                Token::Identifier("x".into()),
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
            ]
        );
    }

    #[test]
    fn test_multi_identifier_lex() {
        let input = "foo bar";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("foo".into()),
                Token::Identifier("bar".into()),
            ]
        );
    }
}
