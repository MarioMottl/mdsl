use crate::token::Token;
use anyhow::Result;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
}

#[derive(Debug)]
pub struct Ast {}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(),
    Eof,
}

pub fn parse_source(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    println!("Tokens: {tokens:#?}");
    unimplemented!();
}
