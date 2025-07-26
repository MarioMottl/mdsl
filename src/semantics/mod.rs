use logos::Span;

use crate::parser::{Ast, Type};

pub type SemanticResult<T> = Result<T, SemanticError>;

#[derive(Debug)]
pub enum SemanticError {
    UndefinedVariable {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
    },
}

pub fn check(ast: &Ast) -> SemanticResult<()> {
    Ok(())
}
