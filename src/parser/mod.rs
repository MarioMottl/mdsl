use crate::token::{Precedence, Token};
use anyhow::Result;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Prefix {
        op: Token,
        right: Box<Expr>,
    },
    Infix {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Ternary {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Grouping(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Null,
}

#[derive(Debug)]
#[allow(unused)]
pub enum Statement {
    ExprStmt(Expr),
    VarDecl {
        name: String,
        initializer: Option<Expr>,
    },
    ValDecl {
        name: String,
        initializer: Expr,
    },
    If {
        cond: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        cond: Expr,
        body: Box<Statement>,
    },
    For {
        init: Option<Box<Statement>>,
        cond: Option<Expr>,
        post: Option<Expr>,
        body: Box<Statement>,
    },
    Return(Option<Expr>),
    Block(Vec<Statement>),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.pos).cloned().unwrap_or(Token::Null)
    }

    fn next(&mut self) -> Token {
        let tok = self.peek();
        self.pos = (self.pos + 1).min(self.tokens.len());
        tok
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let tok = self.next();
        if tok == expected {
            Ok(())
        } else {
            Err(format!("Expected {expected:?}, found {tok:?}"))
        }
    }
    fn parse_prefix(&mut self) -> Result<Expr, String> {
        match self.next() {
            Token::Identifier(name) => Ok(Expr::Identifier(name)),
            Token::Integer(i) => Ok(Expr::Literal(Literal::Integer(i))),
            Token::Float(f) => Ok(Expr::Literal(Literal::Float(f))),
            Token::StringLiteral(s) => Ok(Expr::Literal(Literal::String(s))),
            Token::CharLiteral(c) => Ok(Expr::Literal(Literal::Char(c))),
            Token::True => Ok(Expr::Literal(Literal::Bool(true))),
            Token::False => Ok(Expr::Literal(Literal::Bool(false))),
            Token::Null => Ok(Expr::Literal(Literal::Null)),
            Token::LeftParen => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect(Token::RightParen)?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            Token::Minus | Token::Bang => {
                let op = self.tokens[self.pos - 1].clone();
                let right = self.parse_expression(Precedence::Prefix)?;
                Ok(Expr::Prefix {
                    op,
                    right: Box::new(right),
                })
            }
            other => Err(format!("Unexpected prefix token: {other:?}")),
        }
    }

    fn parse_infix(&mut self, left: Expr, op: Token) -> Result<Expr, String> {
        let prec = op.precedence();
        if prec == Precedence::Ternary {
            let then_branch = self.parse_expression(Precedence::Lowest)?;
            self.expect(Token::Colon)?;
            let else_branch = self.parse_expression(Precedence::Ternary)?;
            Ok(Expr::Ternary {
                cond: Box::new(left),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            })
        } else {
            // left-associative
            let right = self.parse_expression(prec)?;
            Ok(Expr::Infix {
                left: Box::new(left),
                op,
                right: Box::new(right),
            })
        }
    }

    pub fn parse_expression(&mut self, prec: Precedence) -> Result<Expr, String> {
        let mut left = self.parse_prefix()?;

        while self.peek() != Token::Semicolon && prec < self.peek().precedence() {
            let op = self.next();
            left = self.parse_infix(left, op)?;
        }

        Ok(left)
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Token::Var => self.parse_var_decl(),
            Token::Val => self.parse_val_decl(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Return => self.parse_return(),
            Token::LeftBrace => self.parse_block(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Statement, String> {
        self.next(); // consume 'var'
        // consume identifier as variable name
        let name = if let Token::Identifier(name) = self.next() {
            name
        } else {
            return Err("Expected identifier after 'var'".into());
        };
        self.expect(Token::Equal)?;
        let init = self.parse_expression(Precedence::Lowest)?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::VarDecl {
            name,
            initializer: Some(init),
        })
    }

    fn parse_val_decl(&mut self) -> Result<Statement, String> {
        self.next(); // consume 'val'
        // consume identifier as variable name
        let name = if let Token::Identifier(name) = self.next() {
            name
        } else {
            return Err("Expected identifier after 'val'".into());
        };
        self.expect(Token::Equal)?;
        let init = self.parse_expression(Precedence::Lowest)?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::ValDecl {
            name,
            initializer: init,
        })
    }

    fn parse_if(&mut self) -> Result<Statement, String> {
        self.next(); // 'if'
        self.expect(Token::LeftParen)?;
        let cond = self.parse_expression(Precedence::Lowest)?;
        self.expect(Token::RightParen)?;
        let then_branch = Box::new(self.parse_statement()?);
        let else_branch = if self.peek() == Token::Else {
            self.next();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Statement::If {
            cond,
            then_branch,
            else_branch,
        })
    }

    fn parse_while(&mut self) -> Result<Statement, String> {
        self.next(); // 'while'
        self.expect(Token::LeftParen)?;
        let cond = self.parse_expression(Precedence::Lowest)?;
        self.expect(Token::RightParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::While { cond, body })
    }

    fn parse_for(&mut self) -> Result<Statement, String> {
        self.next(); // 'for'
        self.expect(Token::LeftParen)?;
        let init = if self.peek() != Token::Semicolon {
            Some(Box::new(self.parse_statement()?))
        } else {
            self.next();
            None
        };
        let cond = if self.peek() != Token::Semicolon {
            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        let post = if self.peek() != Token::RightParen {
            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };
        self.expect(Token::RightParen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Statement::For {
            init,
            cond,
            post,
            body,
        })
    }

    fn parse_return(&mut self) -> Result<Statement, String> {
        self.next(); // 'return'
        let expr = if self.peek() != Token::Semicolon {
            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Statement::Return(expr))
    }

    fn parse_block(&mut self) -> Result<Statement, String> {
        self.next(); // '{'
        let mut stmts = Vec::new();
        while self.peek() != Token::RightBrace {
            stmts.push(self.parse_statement()?);
        }
        self.next(); // '}'
        Ok(Statement::Block(stmts))
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::ExprStmt(expr))
    }
}

pub fn parse_source(tokens: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut p = Parser::new(tokens);
    let mut stmts = Vec::new();
    while p.peek() != Token::Null {
        // Skip stray semicolons
        if p.peek() == Token::Semicolon {
            p.next();
            continue;
        }
        stmts.push(p.parse_statement()?);
    }
    Ok(stmts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer_literal() {
        let mut p = Parser::new(vec![Token::Integer(42), Token::Semicolon]);
        let expr = p.parse_expression(Precedence::Lowest).unwrap();
        assert_eq!(expr, Expr::Literal(Literal::Integer(42)));
    }

    #[test]
    fn test_parse_infix_precedence() {
        // 1 + 2 * 3;
        let tokens = vec![
            Token::Integer(1),
            Token::Plus,
            Token::Integer(2),
            Token::Star,
            Token::Integer(3),
            Token::Semicolon,
        ];
        let expr = Parser::new(tokens)
            .parse_expression(Precedence::Lowest)
            .unwrap();
        // expect 1 + (2 * 3)
        assert_eq!(
            expr,
            Expr::Infix {
                left: Box::new(Expr::Literal(Literal::Integer(1))),
                op: Token::Plus,
                right: Box::new(Expr::Infix {
                    left: Box::new(Expr::Literal(Literal::Integer(2))),
                    op: Token::Star,
                    right: Box::new(Expr::Literal(Literal::Integer(3))),
                }),
            }
        );
    }

    #[test]
    fn test_parse_prefix_and_grouping() {
        // -(1 + 2);
        let tokens = vec![
            Token::Minus,
            Token::LeftParen,
            Token::Integer(1),
            Token::Plus,
            Token::Integer(2),
            Token::RightParen,
            Token::Semicolon,
        ];
        let expr = Parser::new(tokens)
            .parse_expression(Precedence::Lowest)
            .unwrap();
        assert_eq!(
            expr,
            Expr::Prefix {
                op: Token::Minus,
                right: Box::new(Expr::Grouping(Box::new(Expr::Infix {
                    left: Box::new(Expr::Literal(Literal::Integer(1))),
                    op: Token::Plus,
                    right: Box::new(Expr::Literal(Literal::Integer(2))),
                }))),
            }
        );
    }

    #[test]
    fn test_parse_ternary() {
        // a ? b : c;
        let tokens = vec![
            Token::Identifier("a".into()),
            Token::Question,
            Token::Identifier("b".into()),
            Token::Colon,
            Token::Identifier("c".into()),
            Token::Semicolon,
        ];
        let expr = Parser::new(tokens)
            .parse_expression(Precedence::Lowest)
            .unwrap();
        assert!(matches!(expr, Expr::Ternary { .. }));
    }

    #[test]
    fn test_parse_var_decl() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("foo".into()),
            Token::Equal,
            Token::Integer(5),
            Token::Semicolon,
        ];
        let stmt = Parser::new(tokens).parse_statement().unwrap();
        assert!(
            matches!(stmt, Statement::VarDecl { name, initializer: Some(Expr::Literal(Literal::Integer(5))) } if name == "foo")
        );
    }

    #[test]
    fn test_parse_if_else() {
        // if (x) 1; else 2;
        let tokens = vec![
            Token::If,
            Token::LeftParen,
            Token::Identifier("x".into()),
            Token::RightParen,
            Token::Integer(1),
            Token::Semicolon,
            Token::Else,
            Token::Integer(2),
            Token::Semicolon,
        ];
        let stmt = Parser::new(tokens).parse_statement().unwrap();
        assert!(
            matches!(stmt, Statement::If { cond: Expr::Identifier(ref c), then_branch: _, else_branch: Some(_) } if c == "x")
        );
    }

    #[test]
    fn test_parse_block() {
        // { 1; 2; }
        let tokens = vec![
            Token::LeftBrace,
            Token::Integer(1),
            Token::Semicolon,
            Token::Integer(2),
            Token::Semicolon,
            Token::RightBrace,
        ];
        let stmt = Parser::new(tokens).parse_statement().unwrap();
        assert!(matches!(stmt, Statement::Block(v) if v.len() == 2));
    }
}
