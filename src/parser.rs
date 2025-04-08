use std::f32::consts::TAU;
use std::thread::park;

use crate::ast::{
    BinaryOperator, Expr, KindMethod, Literal, MethodImpl, Statement, StructMethod, TypeAnnotation,
};
use crate::error::{Result, VeldError};
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    recursive_depth: usize,
    total_steps: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            recursive_depth: 0,
            total_steps: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>> {
        println!("Parser: Starting parsing...");
        let mut statements = Vec::new();
        let mut step_count = 0;
        const MAX_STEPS: usize = 1000; // Adjust as needed

        while !self.is_at_end() {
            step_count += 1;
            if step_count > MAX_STEPS {
                return Err(VeldError::ParserError(format!(
                    "Parser exceeded maximum step count at token position: {}",
                    self.current
                )));
            }

            println!("Parser: Current token: {:?}", self.tokens.get(self.current));
            println!("Parser: Context: {}", self.debug_token_context());

            let stmt = self.declaration()?;
            println!("Parser: Successfully parsed statement: {:?}", stmt);
            statements.push(stmt);
        }

        println!(
            "Parser: Parsing completed successfully with {} statements",
            statements.len()
        );
        Ok(statements)
    }

    pub fn declaration(&mut self) -> Result<Statement> {
        if self.match_token(&[Token::Fn]) {
            self.function_declaration()
        } else if self.match_token(&[Token::Proc]) {
            self.proc_declaration()
        } else if self.match_token(&[Token::Let]) {
            self.variable_declaration()
        } else if self.match_token(&[Token::Struct]) {
            self.struct_declaration()
        } else if self.match_token(&[Token::Kind]) {
            self.kind_declaration()
        } else if self.match_token(&[Token::Impl]) {
            self.implementation_declaration()
        } else {
            self.statement()
        }
    }

    fn function_declaration(&mut self) -> Result<Statement> {
        println!("Function declaration: Starting...");
        let name = self.consume_identifier("Expected function name")?;
        println!("Function declaration: Name = {}", name);

        self.consume(&Token::LParen, "Expected '(' after function name")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                println!("Function declaration: Param name = {}", param_name);
                self.consume(&Token::Colon, "Expected ':' after parameter name")?;
                let param_type = self.parse_type()?;
                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[Token::Arrow]) {
            println!("Function declaration: Parsing return type");
            self.parse_type()?
        } else {
            println!("Function declaration: No return type specified, using Unit");
            TypeAnnotation::Unit
        };

        self.consume(&Token::Equals, "Expected '=' after function signature")?;
        println!("Function declaration: Found equals sign, parsing body");

        // Check for empty function
        if self.check(&Token::End) {
            self.consume(&Token::End, "Expected 'end' after function body")?;
            println!("Function declaration: Empty function body");
            return Ok(Statement::FunctionDeclaration {
                name,
                params,
                return_type: return_type.clone(),
                body: Vec::new(),
                is_proc: return_type == TypeAnnotation::Unit,
            });
        }

        // Try parsing a single expression for single-line function
        if !self.check_statement_start() {
            println!("Function declaration: Single-line function, parsing expression");
            println!(
                "Function declaration: Current token context: {}",
                self.debug_token_context()
            );

            // This is where we need to parse expressions like (1.0 + 2.0).sqrt()
            let expr = self.expression()?;
            println!("Function declaration: Parsed expression: {:?}", expr);

            // For single-line functions, consume semicolon if present but don't require it
            if self.match_token(&[Token::Semicolon]) {
                println!("Function declaration: Found semicolon after expression");
            }

            // Is this at the end or followed by a new declaration?
            if self.is_at_end() || self.check_declaration_start() {
                println!("Function declaration: End of single-line function");
                return Ok(Statement::FunctionDeclaration {
                    name,
                    params,
                    return_type: return_type.clone(),
                    body: vec![Statement::Return(Some(expr))],
                    is_proc: return_type == TypeAnnotation::Unit,
                });
            }

            // Otherwise, it's a multi-line function starting with an expression
            println!("Function declaration: Starting multi-line function body");
            let mut body = vec![Statement::ExprStatement(expr)];

            // Parse the rest of the multi-line body
            while !self.check(&Token::End) && !self.is_at_end() {
                body.push(self.statement()?);
            }

            self.consume(&Token::End, "Expected 'end' after function body")?;
            return Ok(Statement::FunctionDeclaration {
                name,
                params,
                return_type: return_type.clone(),
                body,
                is_proc: return_type == TypeAnnotation::Unit,
            });
        }

        // Multi-line function with statements
        println!("Function declaration: Parsing multi-line function body");
        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after function body")?;

        println!("Function declaration: Completed");
        Ok(Statement::FunctionDeclaration {
            name,
            params,
            return_type: return_type.clone(),
            body,
            is_proc: return_type == TypeAnnotation::Unit,
        })
    }

    fn check_statement_start(&self) -> bool {
        self.check(&Token::If)
            || self.check(&Token::While)
            || self.check(&Token::For)
            || self.check(&Token::Return)
            || self.check(&Token::Let)
    }

    fn check_declaration_start(&self) -> bool {
        self.check(&Token::Fn)
            || self.check(&Token::Proc)
            || self.check(&Token::Struct)
            || self.check(&Token::Kind)
            || self.check(&Token::Impl)
            || self.check(&Token::Let)
    }

    fn proc_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected procedure name")?;

        self.consume(&Token::LParen, "Expected '(' after procedure name")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                self.consume(&Token::Colon, "Expected ':' after parameter name")?;
                let param_type = self.parse_type()?;
                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;
        self.consume(&Token::Equals, "Expected '=' after procedure signature")?;

        // Check for empty procedure
        if self.check(&Token::End) {
            // Empty procedure body
            self.consume(&Token::End, "Expected 'end' after procedure body")?;
            return Ok(Statement::ProcDeclaration {
                name,
                params,
                body: Vec::new(),
            });
        }

        // Try parsing a single expression for single-line procedure
        if !self.check_statement_start() {
            let expr = self.expression()?;

            // For single-line procedures, consume semicolon if present but don't require it
            self.match_token(&[Token::Semicolon]);

            // If we're at end of input or the next token looks like a new declaration,
            // this is a single-line procedure without 'end'
            if self.is_at_end() || self.check_declaration_start() {
                return Ok(Statement::ProcDeclaration {
                    name,
                    params,
                    body: vec![Statement::ExprStatement(expr)],
                });
            }

            // Otherwise, it's a multi-line procedure starting with an expression
            let mut body = vec![Statement::ExprStatement(expr)];

            // Parse the rest of the multi-line body
            while !self.check(&Token::End) && !self.is_at_end() {
                body.push(self.statement()?);
            }

            self.consume(&Token::End, "Expected 'end' after procedure body")?;
            return Ok(Statement::ProcDeclaration { name, params, body });
        }

        // Multi-line procedure
        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after procedure body")?;

        Ok(Statement::ProcDeclaration { name, params, body })
    }

    fn struct_declaration(&mut self) -> Result<Statement> {
        println!("Struct declaration: Starting...");
        let name = self.consume_identifier("Expected struct name")?;
        println!("Struct declaration: Name = {}", name);

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        // Check for parentheses style
        if self.match_token(&[Token::LParen]) {
            println!("Struct declaration: Parentheses style");
            if !self.check(&Token::RParen) {
                loop {
                    let field_name = self.consume_identifier("Expected field name")?;
                    println!("Struct declaration: Field name = {}", field_name);
                    self.consume(&Token::Colon, "Expected ':' after field name")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after struct fields")?;
            self.consume(&Token::Semicolon, "Expected ';' after struct declaration")?;
        } else {
            // Block style struct declaration
            println!("Struct declaration: Block style");
            while !self.check(&Token::End) && !self.is_at_end() {
                println!(
                    "Struct declaration: Parsing field or method, current token: {:?}",
                    self.tokens.get(self.current)
                );

                if self.match_token(&[Token::Fn]) {
                    println!("Struct declaration: Found method");
                    methods.push(self.parse_struct_method(name.clone())?);
                } else if self.match_token(&[Token::Impl]) {
                    println!("Struct declaration: Found impl block");
                    while !self.check(&Token::End) && !self.is_at_end() {
                        if self.match_token(&[Token::Fn]) {
                            methods.push(self.parse_struct_method(name.clone())?);
                        } else {
                            return Err(VeldError::ParserError(
                                "Expected method definition".to_string(),
                            ));
                        }
                    }
                    break;
                } else {
                    // Parse field
                    let field_name = self.consume_identifier("Expected field name")?;
                    println!("Struct declaration: Field name = {}", field_name);
                    self.consume(&Token::Colon, "Expected ':' after field name")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type));

                    // Optional comma after field
                    self.match_token(&[Token::Comma]);
                }
            }

            println!("Struct declaration: End of fields and methods, expecting 'end'");
            self.consume(&Token::End, "Expected 'end' after struct definition")?;
        }

        println!("Struct declaration: Completed");
        Ok(Statement::StructDeclaration {
            name,
            fields,
            methods,
        })
    }

    fn parse_struct_method(&mut self, struct_name: String) -> Result<StructMethod> {
        let method_name = self.consume_identifier("Expected method name")?;

        self.consume(&Token::LParen, "Expected '(' after method name")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                let param_type = if self.match_token(&[Token::Colon]) {
                    self.parse_type()?
                } else {
                    TypeAnnotation::Basic(struct_name.clone())
                };
                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[Token::Arrow]) {
            self.parse_type()?
        } else {
            TypeAnnotation::Unit
        };

        self.consume(&Token::Equals, "Expected '=' after method signature")?;

        // Check for empty method body
        if self.check(&Token::End) {
            // Empty method body
            self.consume(&Token::End, "Expected 'end' after method body")?;
            return Ok(StructMethod {
                name: method_name,
                params,
                return_type,
                body: Vec::new(),
            });
        }

        // Try parsing a single expression for single-line method
        if !self.check_statement_start() && !self.check(&Token::Fn) {
            let expr = self.expression()?;

            // For single-line methods, consume semicolon if present but don't require it
            self.match_token(&[Token::Semicolon]);

            // If we're at end of input or the next token looks like a new method/declaration,
            // this is a single-line method without 'end'
            if self.is_at_end() || self.check(&Token::Fn) || self.check_declaration_start() {
                return Ok(StructMethod {
                    name: method_name,
                    params,
                    return_type,
                    body: vec![Statement::Return(Some(expr))],
                });
            }

            // Otherwise, it's a multi-line method starting with an expression
            let mut body = vec![Statement::ExprStatement(expr)];

            // Parse the rest of the multi-line body
            while !self.check(&Token::End) && !self.is_at_end() && !self.check(&Token::Fn) {
                body.push(self.statement()?);
            }

            if !self.check(&Token::Fn) {
                self.consume(&Token::End, "Expected 'end' after method body")?;
            }

            return Ok(StructMethod {
                name: method_name,
                params,
                return_type,
                body,
            });
        }

        // Multi-line method
        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() && !self.check(&Token::Fn) {
            body.push(self.statement()?);
        }

        if !self.check(&Token::Fn) {
            self.consume(&Token::End, "Expected 'end' after method body")?;
        }

        Ok(StructMethod {
            name: method_name,
            params,
            return_type,
            body,
        })
    }

    fn peek_is_statement_start(&self) -> bool {
        if self.is_at_end() {
            return false;
        }

        match &self.tokens.get(self.current) {
            Some(Token::If) | Some(Token::While) | Some(Token::For) | Some(Token::Return)
            | Some(Token::Let) => true,
            _ => false,
        }
    }

    fn kind_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected kind name")?;
        let mut methods = Vec::new();
        if self.match_token(&[Token::Equals]) {
            self.consume(&Token::Fn, "Expected 'fn' after in method declaration")?;
            let method_name = self.consume_identifier("Expected method name")?;
            self.consume(&Token::LParen, "Expected '(' after method name")?;

            let mut params = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    let param_name = self.consume_identifier("Expected parameter name")?;
                    let param_type = if self.match_token(&[Token::Colon]) {
                        self.parse_type()?
                    } else {
                        TypeAnnotation::Basic("Self".to_string())
                    };
                    params.push((param_name, param_type));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after parameters")?;

            let return_type = if self.match_token(&[Token::Arrow]) {
                self.parse_type()?
            } else {
                TypeAnnotation::Unit
            };

            self.consume(&Token::Semicolon, "Expected ';' after single line method")?;

            methods.push(KindMethod {
                name: method_name,
                params,
                return_type,
                default_impl: None,
            });
        } else {
            while !self.check(&Token::End) && !self.is_at_end() {
                self.consume(
                    &Token::Fn,
                    "Expected 'fn' keyword in kind method declaration",
                )?;
                let method_name = self.consume_identifier("Expected method name")?;

                self.consume(&Token::LParen, "Expected '(' after method name")?;

                let mut params = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        let param_name = self.consume_identifier("Expected parameter name")?;
                        let param_type = if self.match_token(&[Token::Colon]) {
                            self.parse_type()?
                        } else {
                            TypeAnnotation::Basic("Self".to_string())
                        };
                        params.push((param_name, param_type));

                        if !self.match_token(&[Token::Comma]) {
                            break;
                        }
                    }
                }

                self.consume(&Token::RParen, "Expected ')' after parameters")?;

                let return_type = if self.match_token(&[Token::Arrow]) {
                    self.parse_type()?
                } else {
                    TypeAnnotation::Unit
                };

                let default_impl = if self.match_token(&[Token::Equals]) {
                    let mut body = Vec::new();
                    while !self.is_at_end() && !self.check(&Token::Fn) && !self.check(&Token::End) {
                        body.push(self.statement()?);
                    }

                    self.consume(&Token::End, "Expected 'end' after default implementation")?;
                    Some(body)
                } else if self.match_token(&[Token::Dot, Token::Dot, Token::Dot]) {
                    None
                } else {
                    None
                };

                methods.push(KindMethod {
                    name: method_name,
                    params,
                    return_type,
                    default_impl,
                });

                self.match_token(&[Token::Comma]);
            }

            self.consume(&Token::End, "Expected 'end' after kind methods")?;
        }
        Ok(Statement::KindDeclaration { name, methods })
    }

    fn implementation_declaration(&mut self) -> Result<Statement> {
        println!("Implementation declaration: Starting...");
        let type_name = self.consume_identifier("Expected type name for implementation")?;
        println!("Implementation declaration: Type name = {}", type_name);

        let kind_name = if self.match_token(&[Token::Colon]) {
            println!("Implementation declaration: Explicit implementation with kind");
            Some(self.consume_identifier("Expected kind name after ':'")?)
        } else {
            None
        };

        let mut methods = Vec::new();

        // Multi-line implementation
        println!("Implementation declaration: Parsing methods");
        while !self.check(&Token::End) && !self.is_at_end() {
            if self.match_token(&[Token::Fn]) {
                println!("Implementation declaration: Parsing fn method");
                let method = self.parse_impl_method(type_name.clone())?;
                methods.push(method);
            } else {
                return Err(VeldError::ParserError(format!(
                    "Expected 'fn' in implementation block, found {:?}",
                    self.tokens.get(self.current)
                )));
            }
        }

        // Consume the End token that terminates the implementation block
        println!("Implementation declaration: Expecting 'end' token");
        self.consume(&Token::End, "Expected 'end' after implementation block")?;
        println!("Implementation declaration: Successfully consumed 'end' token");

        println!(
            "Implementation declaration: Completed with {} methods",
            methods.len()
        );
        Ok(Statement::Implementation {
            type_name,
            kind_name,
            methods,
        })
    }

    fn parse_impl_method(&mut self, type_name: String) -> Result<MethodImpl> {
        println!("Method Implementation: Starting...");
        let method_name = self.consume_identifier("Expected method name")?;
        println!("Method Implementation: Name = {}", method_name);

        self.consume(&Token::LParen, "Expected '(' after method name")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                let param_type = if self.match_token(&[Token::Colon]) {
                    self.parse_type()?
                } else {
                    // For 'self' parameter without type
                    TypeAnnotation::Basic(type_name.clone())
                };

                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[Token::Arrow]) {
            println!("Method Implementation: Parsing return type");
            self.parse_type()?
        } else {
            println!("Method Implementation: No return type, using Unit");
            TypeAnnotation::Unit
        };

        self.consume(&Token::Equals, "Expected '=' after method signature")?;
        println!("Method Implementation: Found equals sign, parsing method body");

        // For a single-line method implementation
        let expr = self.expression()?;
        println!("Method Implementation: Parsed expression: {:?}", expr);

        // Optional semicolon after expression
        if self.match_token(&[Token::Semicolon]) {
            println!("Method Implementation: Found semicolon after expression");
        }

        println!("Method Implementation: Completed");
        Ok(MethodImpl {
            name: method_name,
            params,
            return_type,
            body: vec![Statement::Return(Some(expr))],
        })
    }

    fn parse_type(&mut self) -> Result<TypeAnnotation> {
        let base_type = self.consume_identifier("Expected type name")?;

        if self.match_token(&[Token::Less]) {
            // Generic type parameters
            let mut type_args = Vec::new();

            if !self.check(&Token::Greater) {
                loop {
                    type_args.push(self.parse_type()?);

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::Greater, "Expected '>' after type arguments")?;

            Ok(TypeAnnotation::Generic {
                base: base_type,
                type_args,
            })
        } else if base_type == "fn" {
            // Function type
            self.consume(&Token::LParen, "Expected '(' in function type")?;

            let mut params = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    params.push(self.parse_type()?);

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after parameter types")?;
            self.consume(&Token::Arrow, "Expected '->' in function type")?;

            let return_type = Box::new(self.parse_type()?);

            Ok(TypeAnnotation::Function {
                params,
                return_type,
            })
        } else {
            // Basic type
            Ok(TypeAnnotation::Basic(base_type))
        }
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
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(&Token::Equals, "Expected '=' after variable name")?;

        let value = self.expression()?;

        Ok(Statement::VariableDeclaration {
            name,
            type_annotation,
            value: Box::new(value),
        })
    }

    fn statement(&mut self) -> Result<Statement> {
        if self.match_token(&[Token::If]) {
            self.if_statement()
        } else if self.match_token(&[Token::While]) {
            self.while_statement()
        } else if self.match_token(&[Token::For]) {
            self.for_statement()
        } else if self.match_token(&[Token::Return]) {
            self.return_statement()
        } else {
            let expr = self.expression()?;
            Ok(Statement::ExprStatement(expr))
        }
    }

    fn if_statement(&mut self) -> Result<Statement> {
        let condition = self.expression()?;

        let mut then_branch = Vec::new();
        while !self.check(&Token::End) && !self.check(&Token::Else) && !self.is_at_end() {
            then_branch.push(self.statement()?);
        }

        let else_branch = if self.match_token(&[Token::Else]) {
            let mut statements = Vec::new();
            while !self.check(&Token::End) && !self.is_at_end() {
                statements.push(self.statement()?);
            }
            Some(statements)
        } else {
            None
        };

        self.consume(&Token::End, "Expected 'end' after if statement")?;

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Statement> {
        let condition = self.expression()?;

        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after while loop")?;

        Ok(Statement::While { condition, body })
    }

    fn for_statement(&mut self) -> Result<Statement> {
        let iterator = self.consume_identifier("Expected iterator variable name")?;

        self.consume(&Token::In, "Expected 'in' after iterator variable")?;

        let iterable = self.expression()?;

        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after for loop")?;

        Ok(Statement::For {
            iterator,
            iterable,
            body,
        })
    }

    fn return_statement(&mut self) -> Result<Statement> {
        let value = if self.check(&Token::End) || self.check(&Token::Else) || self.is_at_end() {
            None // Empty return
        } else {
            Some(self.expression()?)
        };

        Ok(Statement::Return(value))
    }

    fn expression(&mut self) -> Result<Expr> {
        println!("Expression: Starting...");
        println!(
            "Expression: Current token context: {}",
            self.debug_token_context()
        );

        self.recursive_depth += 1;
        if self.recursive_depth > 100 {
            self.recursive_depth -= 1;
            return Err(VeldError::ParserError(
                "Parser recursion limit exceeded".to_string(),
            ));
        }

        let result = self.comparison();
        self.recursive_depth -= 1;

        println!("Expression: Completed with result: {:?}", result);
        result
    }

    fn addition(&mut self) -> Result<Expr> {
        println!("Addition: Starting...");

        let mut expr = self.multiplication()?;
        println!("Addition: Got left operand: {:?}", expr);

        while self.match_token(&[Token::Plus, Token::Minus]) {
            let operator = match self.previous() {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };

            println!("Addition: Found operator: {:?}", operator);
            let right = self.multiplication()?;
            println!("Addition: Got right operand: {:?}", right);

            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        println!("Addition: Completed with result: {:?}", expr);
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr> {
        println!("Multiplication: Starting...");

        let mut expr = self.unary()?;
        println!("Multiplication: Got left operand: {:?}", expr);

        while self.match_token(&[Token::Star, Token::Slash]) {
            let operator = match self.previous() {
                Token::Star => BinaryOperator::Multiply,
                Token::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };

            println!("Multiplication: Found operator: {:?}", operator);
            let right = self.unary()?;
            println!("Multiplication: Got right operand: {:?}", right);

            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        println!("Multiplication: Completed with result: {:?}", expr);
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        // Add unary operators if needed
        self.postfix()
    }

    fn postfix(&mut self) -> Result<Expr> {
        println!("Postfix: Starting...");

        let mut expr = self.primary()?;
        println!("Postfix: Got primary expression: {:?}", expr);

        loop {
            println!(
                "Postfix: Current token: {:?}",
                self.tokens.get(self.current)
            );

            if self.is_at_end() {
                break;
            }

            if self.match_token(&[Token::Dot]) {
                println!("Postfix: Found dot operator for method call or property access");

                // The next token should be an identifier (method/property name)
                let method =
                    self.consume_identifier("Expected property or method name after '.'")?;
                println!("Postfix: Method/property name: {}", method);

                // Check if it's a method call with parentheses
                if self.match_token(&[Token::LParen]) {
                    println!("Postfix: This is a method call with arguments");

                    // Parse the argument list - now handling named arguments
                    let mut args = Vec::new();
                    if !self.check(&Token::RParen) {
                        // Check if we have named arguments (looking for name: value format)
                        if self.check_named_arguments() {
                            println!("Postfix: Parsing named arguments");
                            args = self.parse_named_arguments()?;
                        } else {
                            // Regular positional arguments
                            println!("Postfix: Parsing positional arguments");
                            loop {
                                println!("Postfix: Parsing argument");
                                args.push(self.expression()?);
                                if !self.match_token(&[Token::Comma]) {
                                    break;
                                }
                            }
                        }
                    }

                    self.consume(&Token::RParen, "Expected ')' after method arguments")?;
                    println!("Postfix: Method call arguments parsed");

                    expr = Expr::MethodCall {
                        object: Box::new(expr),
                        method,
                        arguments: args,
                    };
                } else {
                    // Property access without parentheses
                    println!("Postfix: This is a property access (no parentheses)");
                    expr = Expr::MethodCall {
                        object: Box::new(expr),
                        method,
                        arguments: Vec::new(),
                    };
                }
            }
            // Rest of code remains the same...
            else if self.match_token(&[Token::LParen]) {
                // Function call handling - existing code
            } else {
                break;
            }
        }

        println!("Postfix: Completed with result: {:?}", expr);
        Ok(expr)
    }

    fn check_named_arguments(&self) -> bool {
        // We need at least 3 tokens: identifier, colon, and value
        if self.current + 2 >= self.tokens.len() {
            return false;
        }

        // Check for identifier followed by colon pattern
        if let Token::Identifier(_) = self.tokens[self.current] {
            return self.tokens[self.current + 1] == Token::Colon;
        }

        false
    }

    fn parse_named_arguments(&mut self) -> Result<Vec<Expr>> {
        println!("Parsing named arguments");
        let mut args = Vec::new();

        while !self.check(&Token::RParen) {
            // Get argument name
            let arg_name = self.consume_identifier("Expected argument name")?;
            println!("Named argument: {}", arg_name);

            // Expect colon after name
            self.consume(&Token::Colon, "Expected ':' after argument name")?;

            // Get argument value
            let arg_value = self.expression()?;

            // Create a StructCreate expression to represent the named argument
            // This is a simplification - you might want a dedicated NamedArgument AST node
            args.push(Expr::StructCreate {
                struct_name: arg_name,
                fields: vec![("value".to_string(), arg_value)],
            });

            // Break if no more arguments
            if !self.match_token(&[Token::Comma]) {
                break;
            }
        }

        Ok(args)
    }

    fn primary(&mut self) -> Result<Expr> {
        println!("Primary: Starting...");
        println!(
            "Primary: Current token context: {}",
            self.debug_token_context()
        );

        if self.is_at_end() {
            return Err(VeldError::ParserError(
                "Unexpected end of input".to_string(),
            ));
        }

        // Choose based on the current token
        let expr = match self.tokens.get(self.current).cloned() {
            Some(Token::IntegerLiteral(n)) => {
                let value = n;
                self.advance();
                println!("Primary: Integer literal: {}", value);
                Expr::Literal(Literal::Integer(value))
            }
            Some(Token::FloatLiteral(n)) => {
                let value = n;
                self.advance();
                println!("Primary: Float literal: {}", value);
                Expr::Literal(Literal::Float(value))
            }
            Some(Token::StringLiteral(s)) => {
                let s_clone = s;
                self.advance();
                println!("Primary: String literal: {}", s_clone);
                Expr::Literal(Literal::String(s_clone))
            }
            Some(Token::True) => {
                self.advance();
                println!("Primary: Boolean literal: true");
                Expr::Literal(Literal::Boolean(true))
            }
            Some(Token::False) => {
                self.advance();
                println!("Primary: Boolean literal: false");
                Expr::Literal(Literal::Boolean(false))
            }
            Some(Token::Identifier(name)) => {
                let name_clone = name;
                self.advance();
                println!("Primary: Identifier: {}", name_clone);
                Expr::Identifier(name_clone)
            }
            Some(Token::LParen) => {
                self.advance(); // consume the '('
                println!("Primary: Found left parenthesis");

                // Check for empty parenthesis - unit literal
                if self.match_token(&[Token::RParen]) {
                    println!("Primary: Empty parentheses - unit literal");
                    Expr::UnitLiteral
                } else {
                    // Parse the grouped expression
                    println!("Primary: Parsing grouped expression");
                    let expr = self.expression()?;
                    self.consume(&Token::RParen, "Expected ')' after expression")?;
                    println!("Primary: Grouped expression parsed");
                    expr
                }
            }
            // Special case for block ending tokens to make errors clearer
            Some(Token::End) => {
                println!("Primary: Encountered 'end' token in expression context");
                return Err(VeldError::ParserError(
                    "Unexpected 'end' token - expected an expression".to_string(),
                ));
            }
            Some(Token::Semicolon) => {
                println!("Primary: Encountered ';' token in expression context");
                return Err(VeldError::ParserError(
                    "Unexpected ';' token - expected an expression".to_string(),
                ));
            }
            Some(token) => {
                println!("Primary: Unexpected token: {:?}", token);
                return Err(VeldError::ParserError(format!(
                    "Expected expression, got {:?}",
                    token
                )));
            }
            None => {
                return Err(VeldError::ParserError(
                    "Unexpected end of token stream".to_string(),
                ));
            }
        };

        println!("Primary: Completed with result: {:?}", expr);
        Ok(expr)
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<(String, Expr)>> {
        println!(
            "Parsing struct fields, token: {:?}",
            self.tokens.get(self.current)
        );
        let mut fields = Vec::new();

        if !self.check(&Token::RBrace) {
            loop {
                let field_name = self.consume_identifier("Expected field name")?;
                println!("Field name: {}", field_name);
                self.consume(&Token::Colon, "Expected ':' after field name")?;
                let field_value = self.expression()?;

                fields.push((field_name, field_value));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }

                // Allow trailing comma by breaking if the next token is '}'
                if self.check(&Token::RBrace) {
                    break;
                }
            }
        }

        Ok(fields)
    }

    fn comparison(&mut self) -> Result<Expr> {
        println!("Comparison: Starting...");

        let mut expr = self.addition()?;
        println!("Comparison: Got left operand: {:?}", expr);

        while self.match_token(&[
            Token::LessEq,
            Token::GreaterEq,
            Token::Less,
            Token::Greater,
            Token::EqualEqual,
            Token::NotEqual,
            Token::And,
            Token::Or,
        ]) {
            let operator = match self.previous() {
                Token::LessEq => BinaryOperator::LessEq,
                Token::GreaterEq => BinaryOperator::GreaterEq,
                Token::Less => BinaryOperator::Less,
                Token::Greater => BinaryOperator::Greater,
                Token::EqualEqual => BinaryOperator::EqualEqual,
                Token::NotEqual => BinaryOperator::NotEqual,
                Token::And => BinaryOperator::And,
                Token::Or => BinaryOperator::Or,
                _ => unreachable!(),
            };

            println!("Comparison: Found operator: {:?}", operator);
            let right = self.addition()?;
            println!("Comparison: Got right operand: {:?}", right);

            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        println!("Comparison: Completed with result: {:?}", expr);
        Ok(expr)
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        println!("In call(): Primary expression parsed: {:?}", expr);
        println!("Current token context: {}", self.debug_token_context());

        loop {
            if self.is_at_end() {
                println!("In call(): Reached end of tokens");
                break;
            }

            println!(
                "In call(): Checking next token: {:?}",
                self.tokens.get(self.current)
            );

            // Check for method call with dot notation
            if self.match_token(&[Token::Dot]) {
                println!("In call(): Found dot, parsing method call");

                // Get method name - use consume_identifier instead of advance
                let method = self.consume_identifier("Expected method name after '.'")?;
                println!("In call(): Method name: {}", method);

                let mut arguments = Vec::new();

                // Check if there are arguments (method call)
                if self.match_token(&[Token::LParen]) {
                    println!("In call(): Found left paren, parsing arguments");

                    // Parse arguments if not empty
                    if !self.check(&Token::RParen) {
                        loop {
                            println!("In call(): Parsing argument");
                            arguments.push(self.expression()?);

                            if !self.match_token(&[Token::Comma]) {
                                break;
                            }
                        }
                    }

                    self.consume(&Token::RParen, "Expected ')' after arguments")?;
                    println!("In call(): Finished parsing arguments");
                }

                // Create method call expression
                expr = Expr::MethodCall {
                    object: Box::new(expr),
                    method,
                    arguments,
                };
                println!("In call(): Created method call: {:?}", expr);
            }
            // Check for function call on identifier
            else if matches!(expr, Expr::Identifier(_)) && self.match_token(&[Token::LParen]) {
                println!("In call(): Found function call on identifier");

                if let Expr::Identifier(name) = expr {
                    let arguments = self.arguments()?;
                    self.consume(&Token::RParen, "Expected ')' after arguments")?;

                    expr = Expr::FunctionCall { name, arguments };
                    println!("In call(): Created function call: {:?}", expr);
                }
            }
            // Not a call expression, exit the loop
            else {
                println!("In call(): No call syntax found, exiting loop");
                break;
            }
        }

        println!("In call(): Returning expression: {:?}", expr);
        Ok(expr)
    }

    fn arguments(&mut self) -> Result<Vec<Expr>> {
        println!("In arguments(): Parsing arguments");

        let mut args = Vec::new();

        // Check if the argument list is empty
        if self.check(&Token::RParen) {
            println!("In arguments(): Empty argument list");
            return Ok(args); // Return empty vector for empty argument list
        }

        // If we get here, there's at least one argument
        println!("In arguments(): Parsing first argument");
        args.push(self.expression()?);

        // Parse additional arguments
        while self.match_token(&[Token::Comma]) {
            println!("In arguments(): Parsing next argument");
            args.push(self.expression()?);
        }

        println!("In arguments(): Returning {} arguments", args.len());
        Ok(args)
    }

    fn debug_token_context(&self) -> String {
        let start = if self.current > 3 {
            self.current - 3
        } else {
            0
        };
        let end = if self.current + 3 < self.tokens.len() {
            self.current + 3
        } else {
            self.tokens.len()
        };

        let mut context = String::new();
        for i in start..end {
            if i == self.current {
                context.push_str(&format!("[→{:?}] ", self.tokens[i]));
            } else {
                context.push_str(&format!("{:?} ", self.tokens[i]));
            }
        }
        context
    }
}
