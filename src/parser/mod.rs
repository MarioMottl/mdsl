mod ast;

pub use self::ast::{Ast, Type};
use crate::token::Token;
use anyhow::Result;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(),
    Eof,
}

pub fn parse_source(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    println!("Tokens: {tokens:#?}");
    Ok(Ast {})
}
