use crate::ast::{BinaryOperator, Expr, Literal, Statement};
use crate::error::{Result, VeldError};
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement> {
        if self.match_token(&[Token::Fn]) {
            self.function_declaration()
        } else if self.match_token(&[Token::Let]) {
            self.variable_declaration()
        } else {
            self.statement()
        }
    }

    fn function_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected function name")?;

        self.consume(&Token::LParen, "Expected '(' after function name")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                self.consume(&Token::Colon, "Expected ':' after parameter name")?;
                let param_type = self.consume_identifier("Expected parameter type")?;
                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[Token::Arrow]) {
            Some(self.consume_identifier("Expected return type")?)
        } else {
            None
        };

        self.consume(&Token::Equals, "Expected '=' after function signature")?;

        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after function body")?;

        Ok(Statement::FunctionDeclaration {
            name,
            params,
            return_type,
            body,
        })
    }

    // Helper methods
    fn match_token(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.check(token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token: &Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.tokens[self.current] == token
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn consume(&mut self, token: &Token, message: &str) -> Result<Token> {
        if self.check(token) {
            Ok(self.advance())
        } else {
            Err(VeldError::ParserError(message.to_string()))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String> {
        match self.advance() {
            Token::Identifier(s) => Ok(s),
            _ => Err(VeldError::ParserError(message.to_string())),
        }
    }

    fn variable_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected variable name")?;

        let type_annotation = if self.match_token(&[Token::Colon]) {
            Some(self.consume_identifier("Expected type annotation")?)
        } else {
            None
        };

        self.consume(&Token::Equals, "Expected '=' after variable name")?;

        let value = self.expression()?;

        Ok(Statement::VariableDeclaration {
            name,
            type_annotation,
            value: Box::new(value),
        })    }

    fn statement(&mut self) -> Result<Statement> {
        // For now, we'll just handle expression statements
        let expr = self.expression()?;
        Ok(Statement::ExprStatement(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.addition()
    }

    fn addition(&mut self) -> Result<Expr> {
        let mut expr = self.multiplication()?;

        while self.match_token(&[Token::Plus, Token::Minus]) {
            let operator = match self.previous() {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            let right = self.multiplication()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        while self.match_token(&[Token::Star, Token::Slash]) {
            let operator = match self.previous() {
                Token::Star => BinaryOperator::Multiply,
                Token::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            let right = self.primary()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        if let Some(token) = self.tokens.get(self.current).cloned() {
            match token {
                Token::IntegerLiteral(n) => {
                    self.advance();
                    Ok(Expr::Literal(Literal::Integer(n)))
                }
                Token::FloatLiteral(n) => {
                    self.advance();
                    Ok(Expr::Literal(Literal::Float(n)))
                }
                Token::StringLiteral(s) => {
                    self.advance();
                    Ok(Expr::Literal(Literal::String(s)))
                }
                Token::Identifier(name) => {
                    self.advance();
                    // Check if this is a function call
                    if self.match_token(&[Token::LParen]) {
                        let mut arguments = Vec::new();

                        // Parse arguments
                        if !self.check(&Token::RParen) {
                            loop {
                                arguments.push(self.expression()?);
                                if !self.match_token(&[Token::Comma]) {
                                    break;
                                }
                            }
                        }

                        self.consume(&Token::RParen, "Expected ')' after arguments")?;

                        Ok(Expr::FunctionCall {
                            name,
                            arguments,
                        })
                    } else {
                        Ok(Expr::Identifier(name))
                    }
                }
                _ => Err(VeldError::ParserError("Expected expression".to_string())),
            }
        } else {
            Err(VeldError::ParserError("Unexpected end of input".to_string()))
        }
    }}