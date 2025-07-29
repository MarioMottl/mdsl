use logos::Span;

pub type SemanticResult<T> = Result<T, SemanticError>;

#[allow(unused)]
#[derive(Debug)]
pub enum SemanticError {
    UndefinedVariable {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: i32,
        found: i32,
        span: Span,
    },
}

#[allow(unused)]
pub fn check(ast: i32) -> SemanticResult<()> {
    Ok(())
}
