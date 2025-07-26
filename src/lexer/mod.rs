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

    #[test]
    fn test_unknown_character_is_reported() {
        let LexResult { tokens, errors } = lex_with_errors("foo @ bar");
        assert_eq!(tokens, vec![Token::Identifier, Token::Identifier]);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].fragment, "@");
        assert_eq!(errors[0].span, 4..5); // position of '@'
    }
}
