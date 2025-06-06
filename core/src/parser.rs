use std::env::VarError;

use crate::ast::{
    Argument, BinaryOperator, EnumVariant, Expr, GenericArgument, ImportItem, KindMethod, Literal,
    MacroExpansion, MacroPattern, MatchArm, MatchPattern, MethodImpl, Statement, StructMethod,
    TypeAnnotation, VarKind,
};
use crate::error::{Result, VeldError};
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum InitializerType {
    Let,    // Immutable by default
    Var,    // Mutable
    Const,  // Cannot be reassigned
    LetMut, // Explicitly mutable let
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    recursive_depth: usize,
    total_steps: usize,
}

impl Parser {
    pub fn get_current_position(&self) -> Option<usize> {
        if self.current < self.tokens.len() {
            Some(self.current)
        } else {
            None
        }
    }
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

    fn is_declaration_keyword(&self) -> bool {
        match self.peek() {
            Token::Let | Token::Var | Token::Const => true,
            _ => false,
        }
    }

    fn declaration(&mut self) -> Result<Statement> {
        if self.is_declaration_keyword() {
            self.variable_declaration()
        } else if self.match_token(&[Token::Enum]) {
            self.enum_declaration()
        } else if self.match_token(&[Token::Mod]) {
            self.module_declaration()
        } else if self.match_token(&[Token::Import]) {
            self.import_declaration()
        } else if self.match_token(&[Token::Macro]) {
            self.macro_declaration()
        } else if self.match_token(&[Token::Pub]) {
            // Handle public declarations
            if self.match_token(&[Token::Fn]) {
                self.function_declaration_with_visibility(true)
            } else if self.match_token(&[Token::Proc]) {
                self.proc_declaration_with_visibility(true)
            } else if self.match_token(&[Token::Struct]) {
                self.struct_declaration_with_visibility(true)
            } else {
                Err(VeldError::ParserError(
                    "Expected function, proc, or struct declaration after 'pub'".to_string(),
                ))
            }
        } else if self.match_token(&[Token::Fn]) {
            self.function_declaration()
        } else if self.match_token(&[Token::Proc]) {
            self.proc_declaration()
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

    fn parse_macro_pattern(&mut self) -> Result<MacroPattern> {
        todo!()
    }
    fn parse_macro_expansion(&mut self) -> Result<MacroExpansion> {
        todo!()
    }

    fn macro_declaration(&mut self) -> Result<Statement> {
        // Consume the tilde after 'macro'
        self.consume(&Token::Tilde, "Expected '~' after 'macro'")?;

        let name = self.consume_identifier("Expected macro name")?;

        // Check for pattern matching style macro
        if self.match_token(&[Token::LBrace]) {
            // Parse pattern matching macro rules
            let mut patterns = Vec::new();

            while !self.check(&Token::RBrace) && !self.is_at_end() {
                // Parse pattern
                let pattern = self.parse_macro_pattern()?;

                // Parse fat arrow and expansion
                self.consume(&Token::FatArrow, "Expected '=>' after macro pattern")?;
                let expansion = self.parse_macro_expansion()?;

                patterns.push((pattern, expansion));

                // Optional comma between patterns
                self.match_token(&[Token::Comma]);
            }

            self.consume(&Token::RBrace, "Expected '}' after macro patterns")?;

            Ok(Statement::MacroDeclaration {
                name,
                patterns,
                body: None,
            })
        } else {
            // Parse simple macro with signature and body
            // For now, parse parameters similar to a function
            self.consume(&Token::LParen, "Expected '(' after macro name")?;

            let mut params = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    let param_name = self.consume_identifier("Expected parameter name")?;
                    params.push(param_name);

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after macro parameters")?;

            // Parse equals and body
            self.consume(&Token::Equals, "Expected '=' after macro parameters")?;

            // Parse macro body as arbitrary statements
            let mut body = Vec::new();
            while !self.check(&Token::End) && !self.is_at_end() {
                body.push(self.statement()?);
            }

            self.consume(&Token::End, "Expected 'end' after macro body")?;

            Ok(Statement::MacroDeclaration {
                name,
                patterns: Vec::new(), // No patterns for simple macros
                body: Some(body),
            })
        }
    }

    fn function_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        println!("Function declaration: Starting...");
        let name = self.consume_identifier("Expected function name")?;
        println!("Function declaration: Name = {}", name);

        self.consume(&Token::LParen, "Expected '(' after function name")?;

        // Parse parameters
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                let param_type = if self.match_token(&[Token::Colon]) {
                    self.parse_type()?
                } else {
                    TypeAnnotation::Basic("infer".to_string())
                };
                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        // Parse optional return type
        let mut return_type = if self.match_token(&[Token::Arrow]) {
            self.parse_type()?
        } else {
            TypeAnnotation::Basic("infer".to_string())
        };

        // Check for function prototype (ends with semicolon)
        if self.match_token(&[Token::Semicolon]) {
            return Ok(Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body: Vec::new(),
                is_proc: false,
                is_public,
            });
        }

        let mut body = Vec::new();

        // Parse function body with various syntaxes
        if self.match_token(&[Token::FatArrow]) {
            // Fat arrow syntax: fn name() => expr or fn name() => do ... end
            if self.match_token(&[Token::Do]) {
                // Block syntax: fn name() => do ... end
                while !self.check(&Token::End) && !self.is_at_end() {
                    body.push(self.statement()?);
                }
                self.consume(&Token::End, "Expected 'end' after function body")?;

                // Infer return type from last statement if needed
                if matches!(return_type, TypeAnnotation::Basic(ref s) if s == "infer") {
                    return_type = if let Some(last_stmt) = body.last() {
                        self.infer_block_return_type(last_stmt)
                            .unwrap_or(TypeAnnotation::Unit)
                    } else {
                        TypeAnnotation::Unit
                    };
                }
            } else {
                // Single expression: fn name() => expr
                let expr = self.expression()?;
                if matches!(return_type, TypeAnnotation::Basic(ref s) if s == "infer") {
                    return_type = self
                        .infer_lambda_return_type(&expr)
                        .unwrap_or(TypeAnnotation::Unit);
                }
                body.push(Statement::Return(Some(expr)));
            }
        } else {
            // Traditional block syntax: fn name() body end or fn name() = body end
            self.match_token(&[Token::Equals]); // Optional equals sign

            while !self.check(&Token::End) && !self.is_at_end() {
                let stmt = self.statement()?;

                // If the last statement is an expression, make it an implicit return
                if self.check(&Token::End) {
                    if let Statement::ExprStatement(expr) = stmt {
                        if matches!(return_type, TypeAnnotation::Basic(ref s) if s == "infer") {
                            return_type = self
                                .infer_lambda_return_type(&expr)
                                .unwrap_or(TypeAnnotation::Unit);
                        }
                        body.push(Statement::Return(Some(expr)));
                    } else {
                        body.push(stmt);
                    }
                } else {
                    body.push(stmt);
                }
            }
            self.consume(&Token::End, "Expected 'end' after function body")?;
        }

        println!("Function declaration: Final return type: {:?}", return_type);

        Ok(Statement::FunctionDeclaration {
            name,
            params,
            return_type,
            body,
            is_proc: false,
            is_public,
        })
    }

    fn lambda_expression(&mut self) -> Result<Expr> {
        println!("Lambda expression Starting...");
        let mut params = Vec::new();
        let mut return_type_anno: Option<TypeAnnotation> = None;
        let mut is_block_demi = false;

        // Handle 'fn' keyword if present
        if self.match_token(&[Token::Fn]) {
            self.consume(&Token::LParen, "Expected '(' after 'fn'")?;

            if !self.check(&Token::RParen) {
                loop {
                    let param_name = self.consume_identifier("Expected parameter name")?;
                    let type_annotation = if self.match_token(&[Token::Colon]) {
                        Some(self.parse_type()?)
                    } else {
                        None
                    };
                    params.push((param_name, type_annotation));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }
            self.consume(&Token::RParen, "Expected ')' after parameters")?;

            // Optional return type annotation
            if self.match_token(&[Token::Arrow]) {
                return_type_anno = Some(self.parse_type()?);
            }

            // Check if this is fn() => syntax or fn() body end syntax
            if !self.check(&Token::FatArrow) {
                // This is fn() body end syntax (block demi lambda)
                is_block_demi = true;
            }
        } else {
            // Handle parameters without 'fn' keyword
            if self.check(&Token::LParen) {
                self.advance(); // Consume '('

                // Check for empty parameter list: () =>
                if self.match_token(&[Token::RParen]) {
                    // Leave params empty
                } else {
                    // Multiple parameters
                    loop {
                        let param_name = self.consume_identifier("Expected parameter name")?;
                        let type_annotation = if self.match_token(&[Token::Colon]) {
                            Some(self.parse_type()?)
                        } else {
                            None
                        };
                        params.push((param_name, type_annotation)); // No type annotation for now

                        if !self.match_token(&[Token::Comma]) {
                            break;
                        }
                    }
                    self.consume(&Token::RParen, "Expected ')' after parameters")?;
                }
            } else {
                // Single parameter without parentheses: x =>
                let param_name = self.consume_identifier("Expected parameter name")?;
                params.push((param_name, None));
            }
        }

        // Handle different lambda body syntaxes
        if is_block_demi {
            // fn() body end syntax (no =>)
            return self.parse_block_demi_lambda(params, return_type_anno);
        } else {
            // Expect fat arrow for other lambda forms
            self.consume(&Token::FatArrow, "Expected '=>' after lambda parameters")?;

            // Check for block syntax with `do`
            if self.match_token(&[Token::Do]) {
                return self.parse_block_lambda(params, return_type_anno);
            }

            // Single expression lambda
            let expr = self.expression()?;
            let inferred_return_type =
                return_type_anno.or_else(|| self.infer_lambda_return_type(&expr));

            println!(
                "Lambda expression: inferred return type: {:?}",
                inferred_return_type
            );

            Ok(Expr::Lambda {
                params,
                body: Box::new(expr),
                return_type: inferred_return_type,
            })
        }
    }

    // Parse block demi lambda: fn() body end (no =>)
    fn parse_block_demi_lambda(
        &mut self,
        params: Vec<(String, Option<TypeAnnotation>)>,
        return_type_anno: Option<TypeAnnotation>,
    ) -> Result<Expr> {
        println!("Parsing block demi lambda with {} parameters", params.len());

        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after block demi lambda body")?;

        // Use provided return type or try to infer from the last statement
        let return_type = return_type_anno.or_else(|| {
            if let Some(last_stmt) = body.last() {
                self.infer_block_return_type(last_stmt)
            } else {
                Some(TypeAnnotation::Unit)
            }
        });

        Ok(Expr::BlockLambda {
            params,
            body,
            return_type,
        })
    }

    // Parse block lambda: params => do ... end
    fn parse_block_lambda(
        &mut self,
        params: Vec<(String, Option<TypeAnnotation>)>,
        return_type_anno: Option<TypeAnnotation>,
    ) -> Result<Expr> {
        println!("Parsing block lambda with {} parameters", params.len());

        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after block lambda body")?;

        // Use provided return type or try to infer from the last statement
        let return_type = return_type_anno.or_else(|| {
            if let Some(last_stmt) = body.last() {
                self.infer_block_return_type(last_stmt)
            } else {
                Some(TypeAnnotation::Unit)
            }
        });

        Ok(Expr::BlockLambda {
            params,
            body,
            return_type,
        })
    }

    fn infer_block_return_type(&self, stmt: &Statement) -> Option<TypeAnnotation> {
        match stmt {
            Statement::ExprStatement(expr) => self.infer_lambda_return_type(expr),
            Statement::Return(Some(expr)) => self.infer_lambda_return_type(expr),
            Statement::Return(None) => Some(TypeAnnotation::Unit),
            _ => Some(TypeAnnotation::Unit),
        }
    }

    fn infer_lambda_return_type(&self, expr: &Expr) -> Option<TypeAnnotation> {
        match expr {
            Expr::Literal(Literal::String(_)) => Some(TypeAnnotation::Basic("str".to_string())),
            Expr::Literal(Literal::Integer(_)) => Some(TypeAnnotation::Basic("i32".to_string())),
            Expr::Literal(Literal::Float(_)) => Some(TypeAnnotation::Basic("f64".to_string())),
            Expr::Literal(Literal::Boolean(_)) => Some(TypeAnnotation::Basic("bool".to_string())),
            Expr::Literal(Literal::Char(_)) => Some(TypeAnnotation::Basic("char".to_string())),
            Expr::Literal(Literal::Unit) | Expr::UnitLiteral => Some(TypeAnnotation::Unit),
            _ => None,
        }
    }

    fn struct_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        println!("Struct declaration: Starting...");
        let name = self.consume_identifier("Expected struct name")?;
        println!("Struct declaration: Name = {}", name);

        println!(
            "Checking for generic parameters, token {:?}",
            self.tokens.get(self.current)
        );
        let generic_params = self.parse_generic_args_if_present()?;
        println!("Parsed generic params: {:?}", generic_params);

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
            is_public,
            generic_params,
        })
    }

    fn function_declaration(&mut self) -> Result<Statement> {
        self.function_declaration_with_visibility(false)
    }

    fn proc_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        println!("Proc declaration: Starting...");
        let name = self.consume_identifier("Expected procedure name")?;

        self.consume(&Token::LParen, "Expected '(' after procedure name")?;

        // Parse parameters
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                let param_type = if self.match_token(&[Token::Colon]) {
                    self.parse_type()?
                } else {
                    TypeAnnotation::Basic("infer".to_string())
                };
                params.push((param_name, param_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        let mut body = Vec::new();

        // Parse proc body - support both traditional and => do syntax
        if self.match_token(&[Token::FatArrow]) {
            self.consume(&Token::Do, "Expected 'do' after '=>' in proc")?;

            while !self.check(&Token::End) && !self.is_at_end() {
                body.push(self.statement()?);
            }
            self.consume(&Token::End, "Expected 'end' after proc body")?;
        } else {
            // Traditional syntax: proc name() body end
            self.match_token(&[Token::Equals]); // Optional equals sign

            while !self.check(&Token::End) && !self.is_at_end() {
                body.push(self.statement()?);
            }
            self.consume(&Token::End, "Expected 'end' after proc body")?;
        }

        Ok(Statement::ProcDeclaration {
            name,
            params,
            body,
            is_public,
        })
    }

    fn block_scope_statement(&mut self) -> Result<Statement> {
        println!("Parsing block scope");

        let mut body = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&Token::End, "Expected 'end' after block scope")?;

        Ok(Statement::BlockScope { body })
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
            || self.check(&Token::Enum)
            || self.check(&Token::Mod)
            || self.check(&Token::Const)
            || self.check(&Token::Var)
    }

    fn proc_declaration(&mut self) -> Result<Statement> {
        self.proc_declaration_with_visibility(false)
    }

    fn struct_declaration(&mut self) -> Result<Statement> {
        println!("Struct declaration: Starting...");
        let name = self.consume_identifier("Expected struct name")?;
        println!("Struct declaration: Name = {}", name);

        println!(
            "Checking for generic parameters, token {:?}",
            self.tokens.get(self.current)
        );
        let generic_params = self.parse_generic_args_if_present()?;
        println!("Parsed generic params: {:?}", generic_params);

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
            is_public: false, // Default visibility
            generic_params,
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

    fn kind_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected kind name")?;
        println!("Kind name: {}", name);

        println!(
            "Before parsing generic params, token: {:?}",
            self.tokens.get(self.current)
        );
        let generic_params = self.parse_generic_args_if_present()?;
        println!(
            "After parsing generic params, token: {:?}",
            self.tokens.get(self.current)
        );

        let mut methods = Vec::new();
        if self.match_token(&[Token::Equals]) {
            println!("Found equals token, parsing single method");

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
                is_public: false, // Default visibility
            });
        } else {
            println!("No equals token, looking for methods or 'end'");

            if self.check(&Token::End) {
                println!("Found end token - empty kind declaration");
                self.advance(); // Consume 'end'
                return Ok(Statement::KindDeclaration {
                    name,
                    methods,
                    is_public: false,
                    generic_params,
                });
            }
            while !self.check(&Token::End) && !self.is_at_end() {
                if !self.match_token(&[Token::Fn]) {
                    println!(
                        "ERROR: Expected 'fn', found: {:?}",
                        self.tokens.get(self.current)
                    );
                    return Err(VeldError::ParserError(
                        "Expected 'fn' keyword in kind method declaration".to_string(),
                    ));
                }

                // self.consume(
                //     &Token::Fn,
                //     "Expected 'fn' keyword in kind method declaration",
                // )?;
                let method_name = self.consume_identifier("Expected method name")?;
                println!("Found method name: {}", method_name);

                self.consume(&Token::LParen, "Expected '(' after method name")?;
                println!(
                    "Found '(' after method name, token: {:?}",
                    self.tokens.get(self.current)
                );

                let mut params = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        let param_name = if self.check(&Token::SelfToken) {
                            self.advance();
                            "self".to_string()
                        } else {
                            self.consume_identifier("Expected parameter name")?
                        };

                        let param_type = if self.match_token(&[Token::Colon]) {
                            self.parse_type()?
                        } else {
                            if param_name == "self" {
                                TypeAnnotation::Basic(name.clone())
                            } else {
                                return Err(VeldError::ParserError(
                                    "Expected type annotation for parameter".to_string(),
                                ));
                            }
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
                    is_public: false, // Default visibility
                });

                self.match_token(&[Token::Comma]);
            }

            self.consume(&Token::End, "Expected 'end' after kind methods")?;
        }
        Ok(Statement::KindDeclaration {
            name,
            methods,
            is_public: false, // Default visibility
            generic_params,
        })
    }

    fn peek_next_token_is(&self, token: &Token) -> bool {
        if self.current + 1 >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.current + 1] {
            t if t == token => true,
            _ => false,
        }
    }

    fn parse_generic_args_if_present(&mut self) -> Result<Vec<GenericArgument>> {
        let mut generic_args = Vec::new();

        if !self.match_token(&[Token::Less]) {
            return Ok(generic_args);
        }

        while !self.check(&Token::Greater) && !self.is_at_end() {
            // Check if we have a named parameter (identifier followed by equals)
            if let Token::Identifier(name) = &self.tokens[self.current].clone() {
                if self.peek_next_token_is(&Token::Equals) {
                    // Named parameter case: "Name = Type"
                    let param_name = name.clone();
                    self.advance(); // Consume identifier
                    self.advance(); // Consume equals
                    let type_annotation = self.parse_type()?;

                    generic_args.push(GenericArgument::named(param_name, type_annotation));
                } else {
                    // Regular type parameter - parse a full type
                    let type_annotation = self.parse_type()?;
                    generic_args.push(GenericArgument::new(type_annotation));
                }
            } else {
                // Just a regular type parameter
                let type_annotation = self.parse_type()?;
                generic_args.push(GenericArgument::new(type_annotation));
            }

            // Check for comma separator
            if !self.match_token(&[Token::Comma]) {
                break;
            }
        }

        self.consume(&Token::Greater, "Expected '>' after generic arguments")?;
        Ok(generic_args)
    }

    fn peek_next_is_identifier(&self) -> bool {
        if self.current + 1 >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.current + 1] {
            Token::Identifier(_) => true,
            _ => false,
        }
    }

    fn parse_implementation_methods(&mut self, type_name: String) -> Result<Vec<MethodImpl>> {
        let mut methods = Vec::new();

        while !self.check(&Token::End) && !self.is_at_end() {
            if !self.match_token(&[Token::Fn]) {
                return Err(VeldError::ParserError(
                    "Expected 'fn' to start method defenition".to_string(),
                ));
            }
            let method = self.parse_impl_method(type_name.clone())?;
            methods.push(method);

            self.match_token(&[Token::Comma]);
        }
        self.consume(&Token::End, "Expected 'end' after implementation methods");
        Ok(methods)
    }

    fn implementation_declaration(&mut self) -> Result<Statement> {
        println!("Implementation declaration: Starting...");

        if self.check(&Token::Impl) && !self.peek_next_is_identifier() {
            let kind_name = self.consume_identifier("Expected kind name")?;
            let generic_args = self.parse_generic_args_if_present()?;
            self.consume(&Token::For, "Expected 'for' after kind name")?;
            let type_name = self.consume_identifier("Expected type name after 'for'")?;
            let methods = self.parse_implementation_methods(type_name.clone())?;
            return Ok(Statement::Implementation {
                type_name,
                kind_name: Some(kind_name),
                methods,
                generic_args,
            });
        }
        self.advance(); // Consume 'impl' token
        let type_name = self.consume_identifier("Expected type name")?;

        let kind_name = if self.match_token(&[Token::LeftArrow]) {
            Some(self.consume_identifier("Expected kind name after '->'")?)
        } else {
            None
        };

        let generic_args = if kind_name.is_some() {
            self.parse_generic_args_if_present()?
        } else {
            Vec::new()
        };

        let methods = self.parse_implementation_methods(type_name.clone())?;

        println!("Implementation declaration: Type name = {}", type_name);

        // Multi-line implementation
        println!("Implementation declaration: Parsing methods");

        // Consume the End token that terminates the implementation block
        println!("Implementation declaration: Expecting 'end' token");
        println!("Implementation declaration: Successfully consumed 'end' token");

        println!(
            "Implementation declaration: Completed with {} methods",
            methods.len()
        );

        Ok(Statement::Implementation {
            type_name,
            kind_name,
            methods,
            generic_args,
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
            is_public: false, // Default visibility
        })
    }

    fn parse_type(&mut self) -> Result<TypeAnnotation> {
        if self.match_token(&[Token::LParen]) {
            let mut types = Vec::new();
            let mut saw_comma = false;

            // Empty tuple: ()
            if self.match_token(&[Token::RParen]) {
                return Ok(TypeAnnotation::Unit);
            }

            // Parse first type
            types.push(self.parse_type()?);

            // If comma, it's a tuple type
            if self.match_token(&[Token::Comma]) {
                saw_comma = true;

                // Parse remaining types if not immediately followed by ')'
                if !self.check(&Token::RParen) {
                    loop {
                        types.push(self.parse_type()?);

                        if !self.match_token(&[Token::Comma]) {
                            break;
                        }

                        // Allow trailing comma by checking if the next token is ')'
                        if self.check(&Token::RParen) {
                            break;
                        }
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after tuple type")?;

            // If there is only one type without a comma, it's a parenthesized type, not a tuple
            if types.len() == 1 && !saw_comma {
                Ok(types[0].clone())
            } else {
                Ok(TypeAnnotation::Tuple(types))
            }
        } else {
            // Handle array type notation
            if self.match_token(&[Token::LBracket]) {
                let element_type = self.parse_type()?;
                self.consume(&Token::RBracket, "Expected ']' after array element type")?;
                return Ok(TypeAnnotation::Array(Box::new(element_type)));
            }

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
        let var_kind = if self.match_token(&[Token::Let]) {
            if self.match_token(&[Token::Mut]) {
                VarKind::LetMut
            } else {
                VarKind::Let
            }
        } else if self.match_token(&[Token::Var]) {
            VarKind::Var
        } else if self.match_token(&[Token::Const]) {
            VarKind::Const
        } else {
            return Err(VeldError::ParserError(
                "Expected item declaration keyword".to_string(),
            ));
        };

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
            var_kind,
            type_annotation,
            value: Box::new(value),
        })
    }

    fn statement(&mut self) -> Result<Statement> {
        println!(
            "Parsing statement, current token: {:?}",
            self.tokens.get(self.current)
        );

        if self.match_token(&[Token::Do]) {
            // This is a block scope, not a lambda
            self.block_scope_statement()
        } else if self.match_token(&[Token::Match]) {
            self.match_statement()
        } else if self.match_token(&[Token::If]) {
            self.if_statement()
        } else if self.match_token(&[Token::While]) {
            self.while_statement()
        } else if self.match_token(&[Token::For]) {
            self.for_statement()
        } else if self.match_token(&[Token::Return]) {
            self.return_statement()
        } else if self.match_token(&[Token::Break]) {
            Ok(Statement::Break)
        } else if self.match_token(&[Token::Continue]) {
            Ok(Statement::Continue)
        } else if self.is_declaration_keyword() {
            // Handle variable declarations (let, var, const)
            self.variable_declaration()
        } else if self.check_assignment() {
            self.assignment_statement()
        } else {
            // Try to parse as expression statement
            let expr = self.expression()?;
            Ok(Statement::ExprStatement(expr))
        }
    }

    fn check_assignment(&self) -> bool {
        if self.current + 1 >= self.tokens.len() {
            return false;
        }

        // Check if current token is identifier and next token is an assignment operator
        match &self.tokens[self.current] {
            Token::Identifier(_) => matches!(
                self.tokens[self.current + 1],
                Token::Equals | Token::PlusEq | Token::MinusEq | Token::StarEq | Token::SlashEq
            ),
            _ => false,
        }
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern> {
        // Check for wildcard pattern (_)
        if self.match_token(&[Token::Identifier("_".to_string())]) {
            return Ok(MatchPattern::Wildcard);
        }

        // Check for literal patterns
        if self.check(&Token::IntegerLiteral(0))
            || self.check(&Token::FloatLiteral(0.0))
            || self.check(&Token::StringLiteral("".to_string()))
            || self.check(&Token::True)
            || self.check(&Token::False)
        {
            let expr = self.primary()?;
            if let Expr::Literal(lit) = expr {
                return Ok(MatchPattern::Literal(lit));
            }
        }

        let ident = self.consume_identifier("Expected identifier in match pattern")?;

        if self.match_token(&[Token::LParen]) {
            let mut fields = Vec::new();

            if !self.check(&Token::RParen) {
                loop {
                    let field_name =
                        self.consume_identifier("Expected field name in struct pattern")?;

                    let field_pattern = if self.match_token(&[Token::Colon]) {
                        let pattern = self.parse_match_pattern()?;
                        Some(Box::new(pattern))
                    } else {
                        None // Shorthand syntax for like 'Person(name)' binds 'name' directly
                    };

                    fields.push((field_name, field_pattern));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after struct pattern")?;

            return Ok(MatchPattern::Struct {
                name: ident,
                fields,
            });
        }

        // Simple variable binding
        Ok(MatchPattern::Identifier(ident))
    }

    fn match_statement(&mut self) -> Result<Statement> {
        let value = self.expression()?;
        let mut arms = Vec::new();
        while !self.check(&Token::End) && !self.is_at_end() {
            let pattern = self.parse_match_pattern()?;

            let gaurd = if self.match_token(&[Token::Where]) {
                Some(self.expression()?)
            } else {
                None
            };

            self.consume(&Token::FatArrow, "Expeected '=>' after match pattern")?;
            let body = self.expression()?;

            arms.push(MatchArm {
                pat: pattern,
                gaurd,
                body,
            });

            self.match_token(&[Token::Comma]);
        }

        self.consume(&Token::End, "Expected 'end' after match statement")?;
        Ok(Statement::Match { value, arms })
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
        self.recursive_depth += 1;
        if self.recursive_depth > 100 {
            self.recursive_depth -= 1;
            return Err(VeldError::ParserError(
                "Parser recursion limit exceeded".to_string(),
            ));
        }

        if self.check_lambda_start() {
            let result = self.lambda_expression();
            self.recursive_depth -= 1;
            return result;
        }

        let result = self.logical();
        self.recursive_depth -= 1;

        println!("Expression: Completed with result: {:?}", result);
        result
    }

    fn check_lambda_start(&self) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.check(&Token::Fn) {
            // Look ahead to see if this is fn(...) => or fn(...) -> ... => pattern
            let mut i = self.current + 1;

            // Skip to opening paren
            if i < self.tokens.len() && self.tokens[i] == Token::LParen {
                // Find matching closing paren
                let mut paren_depth = 1;
                i += 1;

                while i < self.tokens.len() && paren_depth > 0 {
                    match &self.tokens[i] {
                        Token::LParen => paren_depth += 1,
                        Token::RParen => paren_depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }

                // Check for => after params or after optional return type
                if i < self.tokens.len() {
                    if self.tokens[i] == Token::FatArrow {
                        return true;
                    }

                    // Check for -> Type => pattern
                    if self.tokens[i] == Token::Arrow {
                        // Skip over type annotation to look for =>
                        i += 1;
                        while i < self.tokens.len() {
                            if self.tokens[i] == Token::FatArrow {
                                return true;
                            }
                            if matches!(self.tokens[i], Token::Do | Token::End | Token::Semicolon) {
                                break;
                            }
                            i += 1;
                        }
                    }
                }
            }

            // Also check for fn() without => (block demi lambda)
            if i < self.tokens.len() && !matches!(self.tokens[i], Token::FatArrow | Token::Arrow) {
                return true; // This is fn() block_body end syntax
            }
        }

        // Check for identifier => pattern (single param lambda)
        if let Some(Token::Identifier(_)) = self.tokens.get(self.current) {
            return self.tokens.get(self.current + 1) == Some(&Token::FatArrow);
        }

        // Check for () => pattern (no param lambda)
        if self.check(&Token::LParen) {
            let mut i = self.current + 1;
            let mut paren_depth = 1;

            while i < self.tokens.len() && paren_depth > 0 {
                match &self.tokens[i] {
                    Token::LParen => paren_depth += 1,
                    Token::RParen => paren_depth -= 1,
                    _ => {}
                }
                i += 1;
            }

            return i < self.tokens.len() && self.tokens[i] == Token::FatArrow;
        }
        false
    }

    fn logical(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while self.match_token(&[Token::And, Token::Or]) {
            let operator = match self.previous() {
                Token::And => BinaryOperator::And,
                Token::Or => BinaryOperator::Or,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
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
            if self.is_at_end() {
                break;
            }

            if self.match_token(&[Token::As]) {
                println!("Postfix: Found 'as' keyword for type casting");
                let target_type = self.parse_type()?;
                expr = Expr::TypeCast {
                    expr: Box::new(expr),
                    target_type: target_type.clone(),
                };
                println!("Postfix: Completed type cast to {:?}", target_type);
            } else if self.match_token(&[Token::Dot]) {
                // Check for numeric tuple index
                if let Some(Token::IntegerLiteral(idx)) = self.tokens.clone().get(self.current) {
                    self.advance(); // consume the integer
                    expr = Expr::TupleAccess {
                        tuple: Box::new(expr),
                        index: *idx as usize,
                    };
                } else {
                    // Method call or property access
                    let method =
                        self.consume_identifier("Expected property or method name after '.'")?;
                    println!("Postfix: Method/property name: {}", method);

                    if self.match_token(&[Token::LParen]) {
                        // Method call with arguments
                        let mut args = Vec::new();
                        if !self.check(&Token::RParen) {
                            if self.check_named_arguments() {
                                args = self.parse_named_arguments()?;
                            } else {
                                loop {
                                    args.push(Argument::Positional(self.expression()?));
                                    if !self.match_token(&[Token::Comma]) {
                                        break;
                                    }
                                }
                            }
                        }
                        self.consume(&Token::RParen, "Expected ')' after method arguments")?;
                        expr = Expr::MethodCall {
                            object: Box::new(expr),
                            method,
                            arguments: args,
                        };
                    } else {
                        expr = Expr::PropertyAccess {
                            object: Box::new(expr),
                            property: method,
                        };
                    }
                }
            } else if self.match_token(&[Token::LParen]) && matches!(expr, Expr::Identifier(_)) {
                // Function call
                if let Expr::Identifier(name) = expr {
                    println!("Postfix: Function call: {}", name);

                    // Parse arguments
                    let mut args = Vec::new();
                    if !self.check(&Token::RParen) {
                        // Check for named arguments or positional arguments
                        if self.check_named_arguments() {
                            args = self.parse_named_arguments()?;
                        } else {
                            loop {
                                args.push(Argument::Positional(self.expression()?));
                                if !self.match_token(&[Token::Comma]) {
                                    break;
                                }
                            }
                        }
                    }

                    self.consume(&Token::RParen, "Expected ')' after arguments")?;
                    println!("Postfix: Completed function call arguments");

                    expr = Expr::FunctionCall {
                        name,
                        arguments: args,
                    };
                } else {
                    return Err(VeldError::ParserError(
                        "Internal error: expected identifier before '('".to_string(),
                    ));
                }
            } else if self.match_token(&[Token::LBracket]) {
                // Array indexing with [index]
                let index = self.expression()?;
                self.consume(&Token::RBracket, "Expected ']' after array index")?;

                expr = Expr::IndexAccess {
                    object: Box::new(expr),
                    index: Box::new(index),
                }
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

    fn parse_named_arguments(&mut self) -> Result<Vec<Argument>> {
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

            // Create a named argument
            args.push(Argument::Named {
                name: arg_name,
                value: arg_value,
            });

            // Break if no more arguments
            if !self.match_token(&[Token::Comma]) {
                break;
            }
        }

        Ok(args)
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.match_token(&[Token::LParen]) {
            // Empty tuple
            if self.match_token(&[Token::RParen]) {
                return Ok(Expr::UnitLiteral);
            }

            // Parse first element of tuple
            let first = self.expression()?;

            // If comma, it's a tuple
            if self.match_token(&[Token::Comma]) {
                let mut elements = vec![first];

                // Parse remaining elements
                if !self.check(&Token::RParen) {
                    loop {
                        elements.push(self.expression()?);

                        if !self.match_token(&[Token::Comma]) {
                            break;
                        }

                        // Allow trailing comma
                        if self.check(&Token::RParen) {
                            break;
                        }
                    }
                }

                self.consume(&Token::RParen, "Expected ')' after tuple elements")?;
                return Ok(Expr::TupleLiteral(elements));
            } else {
                // Just parenthesized expression
                self.consume(&Token::RParen, "Expected ')' after expression")?;
                return Ok(first);
            }
        }

        if self.is_at_end() {
            return Err(VeldError::ParserError(
                "Unexpected end of input".to_string(),
            ));
        }

        let expr = match self.peek().clone() {
            Token::IntegerLiteral(n) => {
                self.advance();
                Expr::Literal(Literal::Integer(n))
            }
            Token::FloatLiteral(n) => {
                self.advance();
                Expr::Literal(Literal::Float(n))
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expr::Literal(Literal::String(s))
            }
            Token::True => {
                self.advance();
                Expr::Literal(Literal::Boolean(true))
            }
            Token::False => {
                self.advance();
                Expr::Literal(Literal::Boolean(false))
            }
            Token::Identifier(name) => {
                self.advance();
                Expr::Identifier(name)
            }
            Token::Fn => {
                // Handle function expressions: fn(x, y) -> type x + y end
                self.advance(); // consume 'fn'
                self.parse_function_expression()?
            }
            Token::LParen => {
                self.advance(); // consume '('

                // Check for unit literal ()
                if self.match_token(&[Token::RParen]) {
                    Expr::UnitLiteral
                } else {
                    // Parse grouped expression
                    let expr = self.expression()?;
                    self.consume(&Token::RParen, "Expected ')' after expression")?;
                    expr
                }
            }
            Token::LBracket => {
                self.advance(); // consume '['
                self.parse_array_literal()?
            }
            _ => {
                return Err(VeldError::ParserError(format!(
                    "Unexpected token: {:?}",
                    self.peek()
                )));
            }
        };

        Ok(expr)
    }

    fn parse_function_expression(&mut self) -> Result<Expr> {
        // Parse parameters
        self.consume(&Token::LParen, "Expected '(' after 'fn'")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;

                // Type annotation
                let type_annotation = if self.match_token(&[Token::Colon]) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                params.push((param_name, type_annotation));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }
        self.consume(&Token::RParen, "Expected ')' after parameters")?;

        // Return type
        let return_type = if self.match_token(&[Token::Arrow]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Body - expect and expression
        let body = self.expression()?;

        // If there's an 'end' token, consume it
        self.match_token(&[Token::End]);

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
            return_type,
        })
    }

    fn peek(&self) -> &Token {
        if self.is_at_end() {
            // Return a placeholder token if we're at the end
            &Token::IntegerLiteral(0)
        } else {
            &self.tokens[self.current]
        }
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
        let mut expr = self.term()?;

        while self.match_token(&[
            Token::LessEq,
            Token::GreaterEq,
            Token::Less,
            Token::Greater,
            Token::EqualEqual,
            Token::NotEqual,
        ]) {
            let operator = match self.previous() {
                Token::LessEq => BinaryOperator::LessEq,
                Token::GreaterEq => BinaryOperator::GreaterEq,
                Token::Less => BinaryOperator::Less,
                Token::Greater => BinaryOperator::Greater,
                Token::EqualEqual => BinaryOperator::EqualEqual,
                Token::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self.match_token(&[Token::Plus, Token::Minus]) {
            let operator = match self.previous() {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.exponent()?;

        while self.match_token(&[Token::Star, Token::Slash, Token::Modulo]) {
            let operator = match self.previous() {
                Token::Star => BinaryOperator::Multiply,
                Token::Slash => BinaryOperator::Divide,
                Token::Modulo => BinaryOperator::Modulo,
                _ => unreachable!(),
            };
            let right = self.exponent()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn exponent(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        if self.match_token(&[Token::ExpOp]) {
            // For right associativity, we recursively parse the right side at the same precedence level
            let right = self.exponent()?; // Note this is recursive at same level
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator: BinaryOperator::Exponent,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn assignment_statement(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected variable name")?;

        if self.match_token(&[Token::PlusEq]) {
            let value = self.expression()?;
            return Ok(Statement::CompoundAssignment {
                name,
                operator: BinaryOperator::Add,
                value: Box::new(value),
            });
        } else if self.match_token(&[Token::MinusEq]) {
            let value = self.expression()?;
            return Ok(Statement::CompoundAssignment {
                name,
                operator: BinaryOperator::Subtract,
                value: Box::new(value),
            });
        } else if self.match_token(&[Token::StarEq]) {
            let value = self.expression()?;
            return Ok(Statement::CompoundAssignment {
                name,
                operator: BinaryOperator::Multiply,
                value: Box::new(value),
            });
        } else if self.match_token(&[Token::SlashEq]) {
            let value = self.expression()?;
            return Ok(Statement::CompoundAssignment {
                name,
                operator: BinaryOperator::Divide,
                value: Box::new(value),
            });
        } else {
            // Regular assignment
            self.consume(&Token::Equals, "Expected '=' after variable name")?;
            let value = self.expression()?;
            return Ok(Statement::Assignment {
                name,
                value: Box::new(value),
            });
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[Token::LParen]) && matches!(expr, Expr::Identifier(_)) {
                // Function call
                if let Expr::Identifier(name) = expr {
                    // Parse arguments
                    let mut arguments = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            // Check for named arguments (identifier followed by colon)
                            if self.check_named_argument() {
                                let arg_name = self.consume_identifier("Expected argument name")?;
                                self.consume(&Token::Colon, "Expected ':' after argument name")?;
                                let arg_value = self.expression()?;
                                arguments.push(Argument::Named {
                                    name: arg_name,
                                    value: arg_value,
                                });
                            } else {
                                // Normal positional argument
                                let arg = self.expression()?;
                                arguments.push(Argument::Positional(arg));
                            }

                            if !self.match_token(&[Token::Comma]) {
                                break;
                            }
                        }
                    }

                    self.consume(&Token::RParen, "Expected ')' after arguments")?;
                    expr = Expr::FunctionCall { name, arguments };
                }
            } else {
                // FUTURE ENHANCEMENT: Implement method calls on objects with dot notation
                // This will be needed later for expressions like (1.0 + 2.0).sqrt()
                // For now, we're focused on standard function calls
                break;
            }
        }

        Ok(expr)
    }

    fn check_named_argument(&self) -> bool {
        // Look ahead for "identifier: value" pattern
        if self.current + 1 < self.tokens.len() {
            if let Token::Identifier(_) = &self.tokens[self.current] {
                return self.tokens.get(self.current + 1) == Some(&Token::Colon);
            }
        }
        false
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

    fn module_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected module name after 'mod'")?;

        if self.match_token(&[Token::Semicolon]) {
            return Ok(Statement::ModuleDeclaration {
                name,
                body: None,
                is_public: false, // Default visibility
            });
        }

        let mut body = Vec::new();

        while !self.check(&Token::End) && !self.is_at_end() {
            body.push(self.declaration()?);
        }

        self.consume(&Token::End, "Expected 'end' after module body")?;

        Ok(Statement::ModuleDeclaration {
            name,
            body: Some(body),
            is_public: false, // Default visibility
        })
    }

    fn import_declaration(&mut self) -> Result<Statement> {
        // parse import path
        let mut path = Vec::new();

        // First component is required
        path.push(self.consume_identifier("Expected module name after 'import'")?);

        // Parse additional path components if present
        while self.match_token(&[Token::Dot]) {
            if self.match_token(&[Token::Star]) {
                // We got a wildcard import
                let items = vec![ImportItem::All];

                // Check for alias (math.* as m)
                let alias = if self.match_token(&[Token::As]) {
                    Some(self.consume_identifier("Expected alias after 'as'")?)
                } else {
                    None
                };

                self.consume(&Token::Semicolon, "Expected ';' after import")?;

                return Ok(Statement::ImportDeclaration {
                    path,
                    items,
                    alias,
                    is_public: false, // Default visibility
                });
            } else {
                // Add the next path component
                path.push(self.consume_identifier("Expected identifier after '.'")?);
            }
        }

        // Check for selective imports with braces: import math.{sin, cos}
        let items = if self.match_token(&[Token::LBrace]) {
            let mut items = Vec::new();

            loop {
                let name = self.consume_identifier("Expected import item name")?;

                // Check for alias: sqrt as square_root
                let item = if self.match_token(&[Token::As]) {
                    let alias = self.consume_identifier("Expected alias after 'as'")?;
                    ImportItem::Named(name)
                } else {
                    ImportItem::Named(name)
                };
                items.push(item);

                if !self.match_token(&[Token::Comma]) {
                    break;
                }

                // Allow trailing comma
                if self.check(&Token::RBrace) {
                    break;
                }
            }
            self.consume(&Token::RBrace, "Expected '}' after import item(s)")?;
            items
        } else {
            // Simple import of the whole module
            Vec::new()
        };

        let alias = if self.match_token(&[Token::As]) {
            Some(self.consume_identifier("Expected alias after 'as'")?)
        } else {
            None
        };
        self.consume(&Token::Semicolon, "Expected ';' after import statement")?;
        Ok(Statement::ImportDeclaration {
            path,
            items,
            alias,
            is_public: false, // Default visibility
        })
    }
    fn parse_array_literal(&mut self) -> Result<Expr> {
        println!("Parsing array literal");
        let mut elements = Vec::new();

        // Empty array case
        if self.check(&Token::RBracket) {
            self.advance(); // Consume ']'
            return Ok(Expr::ArrayLiteral(elements));
        }

        // Parse array elements
        loop {
            elements.push(self.expression()?);

            if self.match_token(&[Token::Comma]) {
                continue;
            }

            // Allow trailing comma
            if self.check(&Token::RBracket) {
                break;
            }
        }

        self.consume(&Token::RBracket, "Expected ']' after array elements")?;

        Ok(Expr::ArrayLiteral(elements))
    }

    fn enum_declaration(&mut self) -> Result<Statement> {
        println!("Parsing enum declaration...");
        let name = self.consume_identifier("Expected enum name after 'enum'")?;

        let mut variants = Vec::new();

        // Handle single-line enums: enum Color(Red, Green, Blue)
        if self.match_token(&[Token::LParen]) {
            loop {
                let variant_name = self.consume_identifier("Expected variant name")?;
                variants.push(EnumVariant {
                    name: variant_name,
                    fields: None,
                });

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }

            self.consume(&Token::RParen, "Expected ')' after enum variant(s)")?;
        } else {
            // Multi-line enum
            while !self.check(&Token::End) && !self.is_at_end() {
                let variant_name = self.consume_identifier("Expected variant name")?;

                // Check for tuple variant: Red(f64, f64)
                let fields = if self.match_token(&[Token::LParen]) {
                    let mut field_types = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            field_types.push(self.parse_type()?);

                            if !self.match_token(&[Token::Comma]) {
                                break;
                            }
                        }
                    }

                    self.consume(&Token::RParen, "Expected ')' after enum variant fields")?;
                    Some(field_types)
                } else {
                    None
                };
                variants.push(EnumVariant {
                    name: variant_name,
                    fields,
                });

                // Optional comma
                self.match_token(&[Token::Comma]);
            }

            self.consume(&Token::End, "Expected 'end' after enum body")?;
        }

        Ok(Statement::EnumDeclaration {
            name,
            variants,
            is_public: false, // Default visibility
        })
    }
}
