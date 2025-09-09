use std::collections::HashMap;

use crate::ast::UnaryOperator;
use crate::ast::{
    Argument, BinaryOperator, EnumVariant, Expr, GenericArgument, ImportItem, KindMethod, Literal,
    MacroExpansion, MacroPattern, MatchArm, MatchPattern, MethodImpl, Statement, StructField,
    StructMethod, TypeAnnotation, VarKind,
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
        let mut statements = Vec::new();
        let mut step_count = 0;
        const MAX_STEPS: usize = 1000;

        while !self.is_at_end() {
            step_count += 1;
            if step_count > MAX_STEPS {
                return Err(VeldError::ParserError(format!(
                    "Parser exceeded maximum step count at token position: {}",
                    self.current
                )));
            }

            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        Ok(statements)
    }

    pub fn total_steps(&self) -> usize {
        self.total_steps
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
        } else if self.match_token(&[Token::Plex]) {
            self.plex_declaration_with_visibility(false)
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
            } else if self.match_token(&[Token::Kind]) {
                self.kind_declaration_with_visibility(true)
            } else if self.match_token(&[Token::Enum]) {
                self.enum_declaration_with_visibility(true)
            } else if self.match_token(&[Token::Plex]) {
                self.plex_declaration_with_visibility(true)
            } else if self.match_token(&[Token::Mod]) {
                self.module_declaration_with_visibility(true)
            } else if self.match_token(&[Token::Import]) {
                self.import_declaration_with_visibility(true)
            } else if self.peek() == &Token::Let
                || self.peek() == &Token::Var
                || self.peek() == &Token::Const
            {
                // Handle public variable declarations
                self.variable_declaration_with_visibility(true)
            } else {
                Err(VeldError::ParserError(
                    "Expected function, proc, struct, or variable declaration after 'pub'"
                        .to_string(),
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
        self.consume(&Token::LParen, "Expected '(' after macro pattern start")?;

        let mut pattern_tokens = Vec::new();
        while !self.check(&Token::RParen) && !self.is_at_end() {
            pattern_tokens.push(self.advance().clone());
        }

        self.consume(&Token::RParen, "Expected ')' to close macro pattern")?;

        let pattern_str = pattern_tokens
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<_>>()
            .join(" ");

        Ok(MacroPattern(pattern_str))
    }
    fn parse_macro_expansion(&mut self) -> Result<MacroExpansion> {
        let mut statements = Vec::new();

        if self.match_token(&[Token::Do]) {
            while !self.check(&Token::End) && !self.is_at_end() {
                statements.push(self.declaration()?);
            }
            self.consume(&Token::End, "Expected 'end' after macro expansion")?;
        } else {
            // Try to parse a declaration; if that fails, parse an expression as a statement
            if let Ok(stmt) = self.declaration() {
                statements.push(stmt);
            } else if self.is_start_of_expression() {
                let expr = self.expression()?;
                statements.push(Statement::ExprStatement(expr));
                // Do not expect a specific terminator here; allow the macro pattern loop to handle commas/semicolons
                return Ok(MacroExpansion(statements));
            } else {
                return Err(VeldError::ParserError(
                    "Expected statement or expression in macro expansion".to_string(),
                ));
            }
        }
        Ok(MacroExpansion(statements))
    }

    fn macro_declaration(&mut self) -> Result<Statement> {
        // Consume the tilde after 'macro'
        self.consume(&Token::Tilde, "Expected '~' after 'macro'")?;

        let name = self.consume_identifier("Expected macro name")?;

        // If the next token is LParen, treat as pattern-matching macro (Veld style)
        if self.check(&Token::LParen) {
            let mut patterns = Vec::new();
            while !self.check(&Token::End) && !self.is_at_end() {
                // Skip any commas or semicolons before parsing the next pattern
                while self.match_token(&[Token::Comma]) || self.match_token(&[Token::Semicolon]) {}
                if self.check(&Token::End) || self.is_at_end() {
                    break;
                }
                // Parse pattern
                let pattern = self.parse_macro_pattern()?;
                // Parse fat arrow and expansion
                self.consume(&Token::FatArrow, "Expected '=>' after macro pattern")?;
                let expansion = self.parse_macro_expansion()?;
                patterns.push((pattern, expansion));
                // Optional separator between patterns (comma or semicolon)
                while self.match_token(&[Token::Comma]) || self.match_token(&[Token::Semicolon]) {}
            }
            self.consume(&Token::End, "Expected 'end' after macro patterns")?;
            Ok(Statement::MacroDeclaration {
                name,
                patterns,
                body: None,
            })
        } else {
            // Parse simple macro with signature and body (legacy/alternate form)
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

            // Parse macro body as arbitrary statements
            let mut body = Vec::new();

            // Support '=>' for macro bodies (Veld macro syntax)
            if self.match_token(&[Token::FatArrow]) {
                if self.match_token(&[Token::Do]) {
                    // Multiple statements macro body with 'do'/'end'
                    while !self.check(&Token::End) && !self.is_at_end() {
                        body.push(self.declaration()?);
                    }
                    self.consume(&Token::End, "Expected 'end' after macro body")?;
                } else {
                    // Single expression macro body with '=>'
                    let expr = self.expression()?;
                    body.push(Statement::ExprStatement(expr));
                }
            } else {
                return Err(VeldError::ParserError(
                    "Expected '=>' after macro parameters".to_string(),
                ));
            }

            Ok(Statement::MacroDeclaration {
                name,
                patterns: Vec::new(), // No patterns for simple macros
                body: Some(body),
            })
        }
    }

    fn function_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        let name = self.consume_identifier("Expected function name")?;

        // Parse generic type parameters if present
        let generic_params = self.parse_generic_args_if_present()?;

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
                generic_params, // Add generic parameters
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

        Ok(Statement::FunctionDeclaration {
            name,
            params,
            return_type,
            body,
            is_proc: false,
            is_public,
            generic_params,
        })
    }

    fn lambda_expression(&mut self) -> Result<Expr> {
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
        let name = self.consume_identifier("Expected struct name")?;

        let generic_params = self.parse_generic_args_if_present()?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        // Check for parentheses style
        if self.match_token(&[Token::LParen]) {
            if !self.check(&Token::RParen) {
                loop {
                    let field_visibility = self.match_token(&[Token::Pub]);
                    let field_name = self.consume_identifier("Expected field name")?;
                    self.consume(&Token::Colon, "Expected ':' after field name")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type, field_visibility));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after struct fields")?;
            self.consume(&Token::Semicolon, "Expected ';' after struct declaration")?;
        } else {
            // Block style struct declaration
            while !self.check(&Token::End) && !self.is_at_end() {
                if self.match_token(&[Token::Fn]) {
                    methods.push(self.parse_struct_method(name.clone())?);
                } else if self.match_token(&[Token::Impl]) {
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
                    let field_visibility = self.match_token(&[Token::Pub]);
                    let field_name = self.consume_identifier("Expected field name")?;
                    self.consume(&Token::Colon, "Expected ':' after field name")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type, field_visibility));

                    // Optional comma after field
                    self.match_token(&[Token::Comma]);
                }
            }

            self.consume(&Token::End, "Expected 'end' after struct definition")?;
        }

        Ok(Statement::StructDeclaration {
            name,
            fields: fields
                .into_iter()
                .map(|(name, ty, field_visibility)| StructField {
                    name,
                    type_annotation: ty,
                    is_public: field_visibility,
                })
                .collect(),
            methods,
            is_public,
            generic_params,
        })
    }

    fn function_declaration(&mut self) -> Result<Statement> {
        self.function_declaration_with_visibility(false)
    }

    fn proc_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
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
            generic_params: Vec::new(),
        })
    }

    fn block_scope_statement(&mut self) -> Result<Statement> {
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
            || self.check(&Token::Do)
    }

    pub fn check_declaration_start(&self) -> bool {
        self.check(&Token::Fn)
            || self.check(&Token::Proc)
            || self.check(&Token::Struct)
            || self.check(&Token::Kind)
            || self.check(&Token::Impl)
            || self.check(&Token::Let)
            || self.check(&Token::Enum)
            || self.check(&Token::Plex)
            || self.check(&Token::Mod)
            || self.check(&Token::Const)
            || self.check(&Token::Var)
    }

    fn proc_declaration(&mut self) -> Result<Statement> {
        self.proc_declaration_with_visibility(false)
    }

    fn struct_declaration(&mut self) -> Result<Statement> {
        let name = self.consume_identifier("Expected struct name")?;

        let generic_params = self.parse_generic_args_if_present()?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        // Check for parentheses style
        if self.match_token(&[Token::LParen]) {
            if !self.check(&Token::RParen) {
                loop {
                    // Check for pub keyword before field name
                    let field_visibility = self.match_token(&[Token::Pub]);
                    let field_name = self.consume_identifier("Expected field name")?;
                    self.consume(&Token::Colon, "Expected ':' after field name")?;
                    let field_type = self.parse_type()?;
                    // Store visibility along with field name and type
                    fields.push((field_name, field_type, field_visibility));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after struct fields")?;
            self.consume(&Token::Semicolon, "Expected ';' after struct declaration")?;
        } else {
            // Block style struct declaration
            while !self.check(&Token::End) && !self.is_at_end() {
                if self.match_token(&[Token::Fn]) {
                    methods.push(self.parse_struct_method(name.clone())?);
                } else if self.match_token(&[Token::Impl]) {
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
                    let field_visibility = self.match_token(&[Token::Pub]);
                    let field_name = self.consume_identifier("Expected field name")?;
                    self.consume(&Token::Colon, "Expected ':' after field name")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type, field_visibility));

                    // Optional comma after field
                    self.match_token(&[Token::Comma]);
                }
            }

            self.consume(&Token::End, "Expected 'end' after struct definition")?;
        }

        Ok(Statement::StructDeclaration {
            name,
            fields: fields
                .into_iter()
                .map(|(name, ty, field_visibility)| StructField {
                    name,
                    type_annotation: ty,
                    is_public: field_visibility, // Default visibility
                })
                .collect(),
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
                let param_name = self.consume_parameter_name("Expected parameter name")?;
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

        // Single-expression or block-bodied method with '=>'
        if self.match_token(&[Token::FatArrow]) {
            if self.match_token(&[Token::Do]) {
                // Block-bodied method using => do ... end
                let mut body = Vec::new();
                while !self.check(&Token::End) && !self.is_at_end() {
                    body.push(self.statement()?);
                }
                self.consume(&Token::End, "Expected 'end' after method body")?;
                return Ok(StructMethod {
                    name: method_name,
                    params,
                    return_type,
                    body,
                });
            } else {
                // Single-expression method
                let expr = self.expression()?;
                self.match_token(&[Token::Semicolon]);
                return Ok(StructMethod {
                    name: method_name,
                    params,
                    return_type,
                    body: vec![Statement::Return(Some(expr))],
                });
            }
        }

        // Block-bodied method: parse statements until 'end'
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
        self.kind_declaration_with_visibility(false)
    }

    fn module_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        let name = self.consume_identifier("Expected module name after 'mod'")?;

        if self.match_token(&[Token::Semicolon]) {
            return Ok(Statement::ModuleDeclaration {
                name,
                body: None,
                is_public,
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
            is_public,
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

        // Parse comma-separated generic arguments
        if !self.check(&Token::Greater) {
            loop {
                // Parse the type parameter name
                let param_name = self.consume_identifier("Expected type parameter name")?;

                // Check for constraints (T: Constraint)
                let mut constraints = Vec::new();
                if self.match_token(&[Token::Colon]) {
                    // Parse the first constraint, which might be complex like Neg<Output = T>
                    let first_constraint = self.parse_complex_constraint()?;
                    constraints.push(first_constraint);

                    // Parse additional constraints with + separator
                    while self.match_token(&[Token::Plus]) {
                        let next_constraint = self.parse_complex_constraint()?;
                        constraints.push(next_constraint);
                    }

                    generic_args.push(GenericArgument::with_constraints(
                        TypeAnnotation::Basic(param_name),
                        constraints,
                    ));
                }
                // Check for named type parameter (Output = T)
                else if self.match_token(&[Token::Equals]) {
                    let type_annotation = self.parse_type()?;
                    generic_args.push(GenericArgument::named(param_name, type_annotation));
                }
                // Simple type parameter with no constraints
                else {
                    generic_args.push(GenericArgument::new(TypeAnnotation::Basic(param_name)));
                }

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.consume(&Token::Greater, "Expected '>' after generic arguments")?;

        Ok(generic_args)
    }

    // Helper method to parse complex constraints like Neg<Output = T>
    fn parse_complex_constraint(&mut self) -> Result<TypeAnnotation> {
        // Get the base constraint name (e.g., "Neg")
        let base_name = self.consume_identifier("Expected constraint name")?;

        // Check if there are type arguments in angle brackets
        if self.match_token(&[Token::Less]) {
            let mut type_args = Vec::new();

            // Keep parsing type arguments until we reach the closing '>'
            if !self.check(&Token::Greater) {
                loop {
                    // Check for named arguments like "Output = T"
                    if self.peek_next_token_is(&Token::Equals) {
                        let arg_name = self.consume_identifier("Expected argument name")?;
                        self.consume(&Token::Equals, "Expected '=' after argument name")?;

                        // Parse the argument's type
                        let arg_type = self.parse_type()?;

                        // Create a named type argument
                        type_args.push(TypeAnnotation::Generic {
                            base: arg_name,
                            type_args: vec![arg_type],
                        });
                    } else {
                        // Regular type argument
                        type_args.push(self.parse_type()?);
                    }

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(
                &Token::Greater,
                "Expected '>' after constraint type arguments",
            )?;

            // Return the complete constraint with its type arguments
            Ok(TypeAnnotation::Generic {
                base: base_name,
                type_args,
            })
        } else {
            // Simple constraint without type arguments
            Ok(TypeAnnotation::Basic(base_name))
        }
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
            let is_public = self.match_token(&[Token::Pub]);
            if !self.match_token(&[Token::Fn]) {
                return Err(VeldError::ParserError(
                    "Expected 'fn' to start method definition".to_string(),
                ));
            }
            let mut method = self.parse_impl_method(type_name.clone())?;
            method.is_public = is_public;
            methods.push(method);

            self.match_token(&[Token::Comma]);
        }
        let _ = self.consume(&Token::End, "Expected 'end' after implementation methods");
        Ok(methods)
    }

    fn implementation_declaration(&mut self) -> Result<Statement> {
        // Assume 'impl' has already been consumed

        // Parse optional generic parameters after 'impl'
        let generic_params = self.parse_generic_args_if_present()?;

        // Peek ahead to decide which syntax we're dealing with
        let next_token = if self.current < self.tokens.len() {
            &self.tokens[self.current]
        } else {
            return Err(VeldError::ParserError(
                "Unexpected end of input after 'impl'".to_string(),
            ));
        };

        match next_token {
            Token::Identifier(_) => {
                // Parse type name
                let type_name = self.consume_identifier("Expected type name after 'impl'")?;

                // Optionally parse type generics (e.g., Option<T>)
                let _type_generics = self.parse_generic_args_if_present()?; // can be used for validation

                // Check for trait/kind implementation
                if self.match_token(&[Token::LeftArrow]) {
                    let kind_name = Some(self.consume_identifier("Expected kind name after '<-'")?);
                    let generic_args = self.parse_generic_args_if_present()?;
                    let methods = self.parse_implementation_methods(type_name.clone())?;
                    Ok(Statement::Implementation {
                        type_name,
                        kind_name,
                        methods,
                        generic_args,
                    })
                } else if self.match_token(&[Token::For]) {
                    // impl KindName for TypeName
                    let kind_name = Some(type_name.clone());
                    let type_name = self.consume_identifier("Expected type name after 'for'")?;
                    let methods = self.parse_implementation_methods(type_name.clone())?;
                    Ok(Statement::Implementation {
                        type_name,
                        kind_name,
                        methods,
                        generic_args: generic_params,
                    })
                } else {
                    // Inherent impl block
                    let methods = self.parse_implementation_methods(type_name.clone())?;
                    Ok(Statement::InherentImpl {
                        type_name,
                        generic_params,
                        methods,
                    })
                }
            }
            Token::For => Err(VeldError::ParserError(
                "Unexpected 'for' after 'impl'".to_string(),
            )),
            _ => {
                // impl KindName for TypeName
                let kind_name = self.consume_identifier("Expected kind name after 'impl'")?;
                let generic_args = self.parse_generic_args_if_present()?;
                self.consume(&Token::For, "Expected 'for' after kind name")?;
                let type_name = self.consume_identifier("Expected type name after 'for'")?;
                let methods = self.parse_implementation_methods(type_name.clone())?;
                Ok(Statement::Implementation {
                    type_name,
                    kind_name: Some(kind_name),
                    methods,
                    generic_args,
                })
            }
        }
    }

    fn parse_impl_method(&mut self, type_name: String) -> Result<MethodImpl> {
        let method_name = self.consume_identifier("Expected method name")?;

        // Parse optional generic parameters for the method (e.g., <U>)
        let method_generic_params = self.parse_generic_args_if_present()?;

        self.consume(&Token::LParen, "Expected '(' after method name")?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.consume_parameter_name("Expected parameter name")?;
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
            self.parse_type()?
        } else {
            TypeAnnotation::Unit
        };

        // Support '=>' for method bodies (single-expression or block)
        if self.match_token(&[Token::FatArrow]) {
            // Support block body: => do ... end
            if self.match_token(&[Token::Do]) {
                // Parse statements until 'end'
                let mut statements = Vec::new();
                while !self.check(&Token::End) && !self.is_at_end() {
                    statements.push(self.statement()?);
                }
                self.consume(&Token::End, "Expected 'end' after block body")?;

                Ok(MethodImpl {
                    name: method_name,
                    generic_params: method_generic_params.clone(),
                    params,
                    return_type,
                    body: statements,
                    is_public: false, // Will be set by caller
                })
            } else {
                // Single-expression body: => expr
                let expr = self.expression()?;
                // Optional semicolon after expression
                if self.match_token(&[Token::Semicolon]) {}

                Ok(MethodImpl {
                    name: method_name,
                    generic_params: method_generic_params.clone(),
                    params,
                    return_type,
                    body: vec![Statement::ExprStatement(expr)],
                    is_public: false, // Will be set by caller
                })
            }
        } else {
            // Allow method body to start immediately after signature (for inherent impls)
            // Parse as a block until 'end'
            let mut statements = Vec::new();
            while !self.check(&Token::End) && !self.is_at_end() {
                statements.push(self.statement()?);
            }
            self.consume(&Token::End, "Expected 'end' after method body")?;
            Ok(MethodImpl {
                name: method_name,
                generic_params: method_generic_params,
                params,
                return_type,
                body: statements,
                is_public: false, // Will be set by caller
            })
        }
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
                // Check for function type: (T) -> U
                if self.match_token(&[Token::Arrow]) {
                    let return_type = Box::new(self.parse_type()?);
                    Ok(TypeAnnotation::Function {
                        params: vec![types[0].clone()],
                        return_type,
                    })
                } else {
                    Ok(types[0].clone())
                }
            } else {
                Ok(TypeAnnotation::Tuple(types))
            }
        } else if self.match_token(&[Token::LBrace]) {
            // Record type annotation: { field1: Type1, field2: Type2 }
            let mut fields = Vec::new();

            // Empty record: {}
            if self.match_token(&[Token::RBrace]) {
                return Ok(TypeAnnotation::Record { fields });
            }

            // Parse fields
            loop {
                let field_name = self.consume_identifier("Expected field name in record type")?;
                self.consume(
                    &Token::Colon,
                    "Expected ':' after field name in record type",
                )?;
                let field_type = self.parse_type()?;
                fields.push((field_name, field_type));

                if !self.match_token(&[Token::Comma]) {
                    break;
                }

                // Allow trailing comma
                if self.check(&Token::RBrace) {
                    break;
                }
            }

            self.consume(&Token::RBrace, "Expected '}' after record type fields")?;
            Ok(TypeAnnotation::Record { fields })
        } else {
            // Handle array type notation
            if self.match_token(&[Token::LBracket]) {
                let element_type = self.parse_type()?;
                self.consume(&Token::RBracket, "Expected ']' after array element type")?;
                return Ok(TypeAnnotation::Array(Box::new(element_type)));
            }

            if self.match_token(&[Token::Fn]) {
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
                } else {
                    // Basic type
                    Ok(TypeAnnotation::Basic(base_type))
                }
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

    fn consume_parameter_name(&mut self, message: &str) -> Result<String> {
        // Handle 'mut' keyword before parameter names
        if self.match_token(&[Token::Mut]) {
            // After 'mut', expect a parameter name
            match self.advance() {
                Token::Identifier(s) => Ok(format!("mut {}", s)),
                Token::SelfToken => Ok("mut self".to_string()),
                token => Err(VeldError::ParserError(format!(
                    "Expected parameter name after 'mut', got: {:?}",
                    token
                ))),
            }
        } else {
            match self.advance() {
                Token::Identifier(s) => Ok(s),
                Token::SelfToken => Ok("self".to_string()),
                token => Err(VeldError::ParserError(format!(
                    "{}, got: {:?}",
                    message, token
                ))),
            }
        }
    }

    fn variable_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        // Determine the variable kind
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
                "Expected variable declaration keyword".to_string(),
            ));
        };

        // Parse the variable name
        let name = self.consume_identifier("Expected variable name")?;

        // Parse optional type annotation
        let type_annotation = if self.match_token(&[Token::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse initializer
        self.consume(&Token::Equals, "Expected '=' after variable declaration")?;
        let value = Box::new(self.expression()?);

        // Optional semicolon
        self.match_token(&[Token::Semicolon]);

        Ok(Statement::VariableDeclaration {
            name,
            var_kind,
            type_annotation,
            value,
            is_public,
        })
    }

    fn variable_declaration(&mut self) -> Result<Statement> {
        let is_public = self.match_token(&[Token::Pub]);
        self.variable_declaration_with_visibility(is_public)
    }

    fn parse_macro_invocation(&mut self) -> Result<Statement> {
        // Consume the tilde
        self.advance();

        let macro_name = self.consume_identifier("Expected macro name after '~'")?;

        let mut arguments = Vec::new();

        // Parse arguments
        if self.match_token(&[Token::LParen]) {
            // Parenthesized arguments
            if !self.check(&Token::RParen) {
                loop {
                    let arg = self.expression()?;
                    arguments.push(arg);

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after macro arguments")?;
        }

        Ok(Statement::MacroInvocation {
            name: macro_name,
            arguments,
        })
    }

    fn statement(&mut self) -> Result<Statement> {
        if self.check(&Token::Tilde) {
            return self.parse_macro_invocation();
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
            // Print token after expression
            Ok(Statement::ExprStatement(expr))
        }
    }

    fn check_assignment(&self) -> bool {
        if self.current >= self.tokens.len() {
            return false;
        }

        // Look ahead to find an assignment operator
        let mut i = self.current;

        // Handle property access like self.data = value or obj.field = value
        // and array indexing like array[index] = value
        match &self.tokens[i] {
            Token::Identifier(_) | Token::SelfToken => {
                i += 1;

                // Handle array indexing first (e.g., array[index])
                while i < self.tokens.len() && matches!(self.tokens[i], Token::LBracket) {
                    i += 1; // Skip [
                    // Skip through the index expression until we find ]
                    let mut bracket_depth = 1;
                    while i < self.tokens.len() && bracket_depth > 0 {
                        match self.tokens[i] {
                            Token::LBracket => bracket_depth += 1,
                            Token::RBracket => bracket_depth -= 1,
                            _ => {}
                        }
                        i += 1;
                    }
                }

                // Then handle property access chain (e.g., obj.field.subfield)
                while i + 1 < self.tokens.len() && matches!(self.tokens[i], Token::Dot) {
                    i += 1; // Skip dot
                    if i < self.tokens.len() && matches!(self.tokens[i], Token::Identifier(_)) {
                        i += 1; // Skip identifier

                        // Handle array indexing after property access
                        while i < self.tokens.len() && matches!(self.tokens[i], Token::LBracket) {
                            i += 1; // Skip [
                            // Skip through the index expression until we find ]
                            let mut bracket_depth = 1;
                            while i < self.tokens.len() && bracket_depth > 0 {
                                match self.tokens[i] {
                                    Token::LBracket => bracket_depth += 1,
                                    Token::RBracket => bracket_depth -= 1,
                                    _ => {}
                                }
                                i += 1;
                            }
                        }
                    } else {
                        return false; // Invalid property access
                    }
                }

                // Check if next token is an assignment operator
                if i < self.tokens.len() {
                    matches!(
                        self.tokens[i],
                        Token::Equals
                            | Token::PlusEq
                            | Token::MinusEq
                            | Token::StarEq
                            | Token::SlashEq
                    )
                } else {
                    false
                }
            }
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

        // Support qualified (dotted) names in match patterns, e.g., Shape.Circle
        let mut ident = self.consume_identifier("Expected identifier in match pattern")?;
        while self.match_token(&[Token::Dot]) {
            let next = self.consume_identifier("Expected identifier after '.' in match pattern")?;
            ident.push('.');
            ident.push_str(&next);
        }

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

                    if self.check(&Token::RParen) {
                        break;
                    }
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

            self.consume(&Token::FatArrow, "Expected '=>' after match pattern")?;
            let body = if self.match_token(&[Token::Do]) {
                // Parse statements until 'end', allowing a final expression before 'end'
                let mut statements = Vec::new();
                let mut final_expr = None;
                while !self.check(&Token::End) && !self.is_at_end() {
                    // If the next token can start an expression and the following token is 'end',
                    // treat it as a final expression, not a statement.
                    if self.is_start_of_expression()
                        && self.tokens.get(self.current + 1) == Some(&Token::End)
                    {
                        final_expr = Some(Box::new(self.expression()?));
                        break;
                    }
                    if let Ok(stmt) = self.statement() {
                        statements.push(stmt);
                    } else {
                        // Only try to parse a final expression if the next token can start an expression
                        if !self.check(&Token::End) && self.is_start_of_expression() {
                            final_expr = Some(Box::new(self.expression()?));
                        }
                        break;
                    }
                }

                self.consume(&Token::End, "Expected 'end' after match arm block")?;
                Expr::BlockExpression {
                    statements,
                    final_expr,
                }
            } else {
                self.expression()?
            };

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
        self.consume(&Token::Then, "Expected 'then' after if condition")?;

        let mut then_branch = Vec::new();
        while !self.check(&Token::End) && !self.check(&Token::Else) && !self.is_at_end() {
            then_branch.push(self.statement()?);
        }

        // Track all else and else-if branches
        let mut all_branches = Vec::new();
        let mut current_condition = condition;
        let mut current_then_branch = then_branch;

        // Handle all else-if and else branches
        while self.match_token(&[Token::Else]) {
            if self.match_token(&[Token::If]) {
                // This is an else-if branch
                let else_if_condition = self.expression()?;
                self.consume(&Token::Then, "Expected 'then' after else-if condition")?;

                let mut else_if_branch = Vec::new();
                while !self.check(&Token::End) && !self.check(&Token::Else) && !self.is_at_end() {
                    else_if_branch.push(self.statement()?);
                }

                // Save the current branch
                all_branches.push((current_condition, current_then_branch));

                // Update current branch to this else-if
                current_condition = else_if_condition;
                current_then_branch = else_if_branch;
            } else {
                // Regular else branch
                let mut else_branch = Vec::new();
                while !self.check(&Token::End) && !self.is_at_end() {
                    else_branch.push(self.statement()?);
                }

                // Create the final if-statement with all branches
                let mut result = Statement::If {
                    condition: current_condition,
                    then_branch: current_then_branch,
                    else_branch: Some(else_branch),
                };

                // Now work backwards through the saved branches to create nested if-statements
                for (cond, then_br) in all_branches.into_iter().rev() {
                    result = Statement::If {
                        condition: cond,
                        then_branch: then_br,
                        else_branch: Some(vec![result]),
                    };
                }

                self.consume(&Token::End, "Expected 'end' after if statement")?;
                return Ok(result);
            }
        }

        // If we get here, there was no final else branch
        let mut result = Statement::If {
            condition: current_condition,
            then_branch: current_then_branch,
            else_branch: None,
        };

        // Now work backwards through the saved branches to create nested if-statements
        for (cond, then_br) in all_branches.into_iter().rev() {
            result = Statement::If {
                condition: cond,
                then_branch: then_br,
                else_branch: Some(vec![result]),
            };
        }

        self.consume(&Token::End, "Expected 'end' after if statement")?;
        return Ok(result);
    }

    fn while_statement(&mut self) -> Result<Statement> {
        let condition = self.expression()?;

        // Require 'do' after the condition
        self.consume(&Token::Do, "Expected 'do' after while condition")?;

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
        // Require 'do' after for loop header
        self.consume(&Token::Do, "Expected 'do' after for loop header")?;

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

    fn pipeline(&mut self) -> Result<Expr> {
        let mut expr = self.range()?;

        while self.match_token(&[Token::Pipe]) {
            let right = self.range()?;
            expr = Expr::BinaryOp {
                left: Box::new(expr),
                operator: BinaryOperator::Pipe,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn range(&mut self) -> Result<Expr> {
        let expr = self.logical()?;

        // Check for range operators: expr.. or expr..=
        if self.match_token(&[Token::DotDot, Token::DotDotEq]) {
            let inclusive = self.previous() == Token::DotDotEq;
            let start = Some(Box::new(expr));

            // Check if there's an end expression
            let end = if self.check(&Token::Comma)
                || self.check(&Token::RParen)
                || self.check(&Token::RBracket)
                || self.check(&Token::RBrace)
                || self.check(&Token::Semicolon)
                || self.check(&Token::End)
                || self.check(&Token::Let)
                || self.check(&Token::Var)
                || self.check(&Token::Const)
                || self.check(&Token::Fn)
                || self.check(&Token::Proc)
                || self.check(&Token::If)
                || self.check(&Token::While)
                || self.check(&Token::For)
                || self.check(&Token::Return)
                || self.is_at_end()
            {
                None
            } else {
                Some(Box::new(self.logical()?))
            };

            return Ok(Expr::Range {
                start,
                end,
                inclusive,
            });
        }

        Ok(expr)
    }

    pub fn expression(&mut self) -> Result<Expr> {
        self.recursive_depth += 1;
        if self.recursive_depth > 100 {
            self.recursive_depth -= 1;
            return Err(VeldError::ParserError(
                "Parser recursion limit exceeded".to_string(),
            ));
        }

        // Record/ Anonymous struct, looks like: {field1: value1, field2: value2}
        if self.match_token(&[Token::LBrace]) {
            // Parse record fields
            let fields = self.record_fields()?;
            let record_expr = Expr::Record { fields };
            self.recursive_depth -= 1;
            // Allow postfix operations on record expressions
            return self.postfix_with_expr(record_expr);
        };

        if self.check_lambda_start() {
            let result = self.lambda_expression();
            self.recursive_depth -= 1;
            return result;
        }

        let result = self.pipeline();

        self.recursive_depth -= 1;

        result
    }

    fn record_fields(&mut self) -> Result<Vec<(String, Expr)>> {
        let mut fields = Vec::new();

        while !self.check(&Token::RBrace) {
            if self.is_at_end() {
                return Err(VeldError::ParserError(
                    "Unexpected end of input".to_string(),
                ));
            }

            let field_name = self.consume_identifier("Expected field name")?;
            self.consume(&Token::Colon, "Expected ':' after field name")?;
            let field_value = self.expression()?;

            fields.push((format!("{}", field_name), field_value));

            if self.check(&Token::Comma) {
                self.advance();
            } else if !self.check(&Token::RBrace) {
                return Err(VeldError::ParserError(
                    "Expected ',' or '}' after field".to_string(),
                ));
            }
        }

        self.advance();

        Ok(fields)
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

    fn unary(&mut self) -> Result<Expr> {
        // Check for unary operators
        if self.match_token(&[Token::Minus, Token::Bang]) {
            let operator = match self.previous() {
                Token::Minus => UnaryOperator::Negate,
                Token::Bang => UnaryOperator::Not,
                _ => unreachable!(),
            };

            // Recursively parse the operand
            let operand = self.unary()?;

            return Ok(Expr::UnaryOp {
                operator,
                operand: Box::new(operand),
            });
        }

        // If no unary operator, delegate to postfix
        let pos = self.postfix();
        pos
    }

    fn postfix(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        self.postfix_with_expr(expr)
    }

    fn postfix_with_expr(&mut self, mut expr: Expr) -> Result<Expr> {
        loop {
            if self.is_at_end() {
                break;
            }

            // Enum variant creation: Enum.Variant(...)
            // Only treat as enum variant if left side is a bare identifier starting with uppercase (likely a type/enum)
            if let Expr::Identifier(enum_name) = &expr {
                if self.check(&Token::Dot) {
                    // Only treat as enum variant if enum_name starts with uppercase
                    if enum_name
                        .chars()
                        .next()
                        .map(|c| c.is_uppercase())
                        .unwrap_or(false)
                    {
                        if let Some(Token::Identifier(_)) = self.tokens.get(self.current + 1) {
                            if self.tokens.get(self.current + 2) == Some(&Token::LParen) {
                                self.advance(); // consume Dot
                                let variant_name = self
                                    .consume_identifier("Expected enum variant name after '.'")?;
                                self.consume(
                                    &Token::LParen,
                                    "Expected '(' after enum variant name",
                                )?;
                                let mut fields = Vec::new();
                                if !self.check(&Token::RParen) {
                                    loop {
                                        fields.push(self.expression()?);
                                        if !self.match_token(&[Token::Comma]) {
                                            break;
                                        }
                                    }
                                }
                                self.consume(
                                    &Token::RParen,
                                    "Expected ')' after enum variant fields",
                                )?;
                                expr = Expr::EnumVariant {
                                    enum_name: enum_name.clone(),
                                    variant_name,
                                    fields,
                                    type_args: None,
                                };
                                continue;
                            }
                        }
                    }
                }
            }

            // Macro invocation: name~(...)
            if let Expr::Identifier(name) = &expr {
                if self.match_token(&[Token::Tilde]) {
                    self.consume(&Token::LParen, "Expected '(' after macro invocation '~'")?;
                    let mut args = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            args.push(self.expression()?);
                            if !self.match_token(&[Token::Comma]) {
                                break;
                            }
                        }
                    }
                    self.consume(&Token::RParen, "Expected ')' after macro arguments")?;
                    expr = Expr::MacroExpr {
                        name: name.clone(),
                        arguments: args,
                    };
                    continue;
                }
            }

            if self.match_token(&[Token::As]) {
                let target_type = self.parse_type()?;

                expr = Expr::TypeCast {
                    expr: Box::new(expr),
                    target_type: target_type.clone(),
                };
            } else if self.match_token(&[Token::Dot]) {
                // Check for numeric tuple index
                if let Some(Token::IntegerLiteral(idx)) = self.tokens.clone().get(self.current) {
                    self.advance(); // consume the integer
                    expr = Expr::TupleAccess {
                        tuple: Box::new(expr),
                        index: *idx as usize,
                    };
                } else {
                    // Check if this is a method call (property followed by parentheses)
                    let property =
                        self.consume_identifier("Expected property or method name after '.'")?;

                    // Check if the next token is an opening parenthesis for a method call
                    if self.check(&Token::LParen) {
                        self.advance(); // consume LParen
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
                            method: property,
                            arguments: args,
                        };
                    } else {
                        // Just a property access
                        expr = Expr::PropertyAccess {
                            object: Box::new(expr),
                            property,
                        };
                    }
                }
            } else if self.match_token(&[Token::LParen]) {
                // Function call on identifier or property access chain
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
                self.consume(&Token::RParen, "Expected ')' after arguments")?;
                let all_named = !args.is_empty()
                    && args.iter().all(|arg| matches!(arg, Argument::Named { .. }));
                if all_named {
                    // Fix: args is Vec<Argument>, but StructCreate expects Vec<(String, Expr)>
                    let fields: Vec<(String, Expr)> = args
                        .into_iter()
                        .map(|arg| match arg {
                            Argument::Named { name, value } => (name, value),
                            _ => unreachable!(),
                        })
                        .collect();
                    // Fix: struct_name expects String, not Box<Expr>
                    let struct_name = match expr {
                        Expr::Identifier(ref name) => name.clone(),
                        _ => {
                            return Err(VeldError::ParserError(
                                "Struct instantiation expects identifier as struct name"
                                    .to_string(),
                            ));
                        }
                    };
                    expr = Expr::StructCreate {
                        struct_name,
                        fields,
                    };
                } else {
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        arguments: args,
                    };
                }
            } else if self.check(&Token::LParen) && matches!(expr, Expr::Identifier(_)) {
                self.advance(); // now consume LParen
                // Function call
                if let Expr::Identifier(name) = expr {
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

                    // If all arguments are named and there is at least one argument, treat as struct instantiation
                    let all_named = !args.is_empty()
                        && args.iter().all(|arg| matches!(arg, Argument::Named { .. }));
                    if all_named {
                        // Fix: args is Vec<Argument>, but StructCreate expects Vec<(String, Expr)>
                        let fields: Vec<(String, Expr)> = args
                            .into_iter()
                            .map(|arg| match arg {
                                Argument::Named { name, value } => (name, value),
                                _ => unreachable!(),
                            })
                            .collect();
                        expr = Expr::StructCreate {
                            struct_name: name,
                            fields,
                        };
                    } else {
                        expr = Expr::Call {
                            callee: Box::new(Expr::Identifier(name)),
                            arguments: args,
                        };
                    }
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
        let mut args = Vec::new();

        while !self.check(&Token::RParen) {
            // Get argument name
            let arg_name = self.consume_identifier("Expected argument name")?;

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
        if self.match_token(&[Token::Do]) {
            // Parse block expression: do statements... [expr] end
            return self.parse_block_expression();
        }

        // Parse macro variable reference: $identifier
        if self.match_token(&[Token::Dollar]) {
            if let Token::Identifier(name) = self.advance() {
                return Ok(Expr::MacroVar(name));
            } else {
                return Err(VeldError::ParserError(
                    "Expected identifier after '$'".to_string(),
                ));
            }
        }

        if self.match_token(&[Token::If]) {
            // Parse if expression: if condition then expr [else expr] end
            return self.parse_if_expression();
        }

        if self.match_token(&[Token::LParen]) {
            // Empty tuple or grouped expression
            if self.match_token(&[Token::RParen]) {
                return Ok(Expr::UnitLiteral);
            }

            // Parse first element of tuple or grouped expression
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
            Token::LBracket => {
                self.advance(); // consume '['
                self.parse_array_literal()?
            }
            Token::DotDot | Token::DotDotEq => {
                // Handle prefix ranges: ..expr or ..=expr
                let inclusive = self.peek() == &Token::DotDotEq;
                self.advance(); // consume .. or ..=

                let end = if self.check(&Token::Comma)
                    || self.check(&Token::RParen)
                    || self.check(&Token::RBracket)
                    || self.check(&Token::RBrace)
                    || self.check(&Token::Semicolon)
                    || self.check(&Token::End)
                    || self.is_at_end()
                {
                    None
                } else {
                    Some(Box::new(self.logical()?))
                };

                Expr::Range {
                    start: None,
                    end,
                    inclusive,
                }
            }
            Token::SelfToken => {
                self.advance();
                Expr::SelfReference
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

    fn parse_block_expression(&mut self) -> Result<Expr> {
        let mut statements = Vec::new();
        let mut final_expr = None;

        while !self.check(&Token::End) && !self.is_at_end() {
            // Look ahead to see if this might be the final expression
            if self.is_likely_final_expression() {
                final_expr = Some(Box::new(self.expression()?));

                break;
            } else if self.check_statement_start() {
                statements.push(self.statement()?);
            } else {
                // Unexpected token, break out of the block parsing loop
                break;
            }
        }

        self.consume(&Token::End, "Expected 'end' after block expression")?;

        Ok(Expr::BlockExpression {
            statements,
            final_expr,
        })
    }

    fn is_likely_final_expression(&self) -> bool {
        // Heuristic: if the next token after this line would be 'end',
        // then this is likely the final expression

        if self.is_at_end() {
            return false;
        }

        // Simple check: if current token starts an expression and we're not seeing
        // obvious statement keywords, treat it as final expression
        match self.peek() {
            Token::Let
            | Token::Var
            | Token::Const
            | Token::If
            | Token::While
            | Token::For
            | Token::Return
            | Token::Break
            | Token::Continue
            | Token::Do
            | Token::Fn
            | Token::Proc => false,
            _ => {
                // Look ahead to see if 'end' comes after this expression
                let result = self.expression_followed_by_end();

                result
            }
        }
    }

    fn expression_followed_by_end(&self) -> bool {
        // For now, just check if we have relatively few tokens left before 'end'
        let mut i = self.current;
        let mut depth = 0;

        while i < self.tokens.len() {
            match &self.tokens[i] {
                Token::LParen | Token::LBracket | Token::LBrace => depth += 1,
                Token::RParen | Token::RBracket | Token::RBrace => depth -= 1,
                Token::End if depth == 0 => return true,
                Token::Let
                | Token::Var
                | Token::Const
                | Token::If
                | Token::While
                | Token::For
                | Token::Return
                | Token::Break
                | Token::Continue
                | Token::Do
                | Token::Fn
                | Token::Proc
                    if depth == 0 =>
                {
                    return false;
                }
                _ => {}
            }
            i += 1;

            // Don't look too far ahead
            if i - self.current > 10 {
                break;
            }
        }

        false
    }

    // Parse if expression: if condition then expr else expr end
    fn parse_if_expression(&mut self) -> Result<Expr> {
        // Parse condition (we already consumed 'if')
        let condition = self.expression()?;

        self.consume(&Token::Then, "Expected 'then' after if condition")?;

        // Parse then expression
        let then_expr = if self.match_token(&[Token::Do]) {
            self.parse_block_expression()?
        } else {
            // Parse a simple expression (no control flow)
            self.logical()?
        };

        // Parse optional else branch
        let else_expr = if self.match_token(&[Token::Else]) {
            if self.match_token(&[Token::Do]) {
                Some(Box::new(self.parse_block_expression()?))
            } else {
                // Parse a simple expression (no control flow)
                Some(Box::new(self.logical()?))
            }
        } else {
            None
        };

        self.consume(&Token::End, "Expected 'end' after if expression")?;

        Ok(Expr::IfExpression {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr,
        })
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

    fn parse_struct_fields(&mut self) -> Result<Vec<StructField>> {
        let mut fields = Vec::new();

        while !self.check(&Token::End) && !self.check(&Token::RBrace) && !self.is_at_end() {
            let field_visibility = self.match_token(&[Token::Pub]);
            let field_name = self.consume_identifier("Expected field name")?;
            self.consume(&Token::Colon, "Expected ':' after field name")?;
            let field_type = self.parse_type()?;

            fields.push(StructField {
                name: field_name,
                type_annotation: field_type,
                is_public: field_visibility,
            });

            if !self.match_token(&[Token::Comma]) {
                break;
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
        // Parse the left-hand side as an expression to handle property access
        let lhs = self.parse_assignment_target()?;

        if self.match_token(&[Token::PlusEq]) {
            let value = self.expression()?;
            return Ok(Statement::PropertyAssignment {
                target: Box::new(lhs),
                operator: Some(BinaryOperator::Add),
                value: Box::new(value),
            });
        } else if self.match_token(&[Token::MinusEq]) {
            let value = self.expression()?;
            return Ok(Statement::PropertyAssignment {
                target: Box::new(lhs),
                operator: Some(BinaryOperator::Subtract),
                value: Box::new(value),
            });
        } else if self.match_token(&[Token::StarEq]) {
            let value = self.expression()?;
            return Ok(Statement::PropertyAssignment {
                target: Box::new(lhs),
                operator: Some(BinaryOperator::Multiply),
                value: Box::new(value),
            });
        } else if self.match_token(&[Token::SlashEq]) {
            let value = self.expression()?;
            return Ok(Statement::PropertyAssignment {
                target: Box::new(lhs),
                operator: Some(BinaryOperator::Divide),
                value: Box::new(value),
            });
        } else {
            // Regular assignment
            self.consume(&Token::Equals, "Expected '=' after assignment target")?;
            let value = self.expression()?;
            return Ok(Statement::PropertyAssignment {
                target: Box::new(lhs),
                operator: None,
                value: Box::new(value),
            });
        }
    }

    fn parse_assignment_target(&mut self) -> Result<Expr> {
        // Parse self or identifier
        let mut expr = if self.match_token(&[Token::SelfToken]) {
            Expr::SelfReference
        } else {
            let name = self.consume_identifier("Expected identifier or 'self'")?;
            Expr::Identifier(name)
        };

        // Handle array indexing and property access chain
        loop {
            if self.match_token(&[Token::LBracket]) {
                // Array indexing like obj[index]
                let index = self.expression()?;
                self.consume(&Token::RBracket, "Expected ']' after array index")?;
                expr = Expr::IndexAccess {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.match_token(&[Token::Dot]) {
                // Property access like obj.field
                let property = self.consume_identifier("Expected property name after '.'")?;
                expr = Expr::PropertyAccess {
                    object: Box::new(expr),
                    property,
                };
            } else {
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
        let mut args = Vec::new();

        // Check if the argument list is empty
        if self.check(&Token::RParen) {
            return Ok(args); // Return empty vector for empty argument list
        }

        // If we get here, there's at least one argument
        args.push(self.expression()?);

        // Parse additional arguments
        while self.match_token(&[Token::Comma]) {
            args.push(self.expression()?);
        }

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
        self.module_declaration_with_visibility(false)
    }

    fn import_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        // parse import path
        let mut path = vec![self.consume_identifier("Expected module name")?];

        // Handle multi-part module names/paths (e.g., "std.io", "std.math.vec")
        while self.match_token(&[Token::Dot]) {
            if self.match_token(&[Token::Star]) {
                // Import all: import std.math.*
                return Ok(Statement::ImportDeclaration {
                    path,
                    items: vec![ImportItem::All],
                    alias: None,
                    is_public,
                });
            } else if self.check(&Token::LBrace) {
                // Grouped import: import math.{add, subtract}
                break;
            } else {
                path.push(self.consume_identifier("Expected identifier after '.'")?);
            }
        }

        // Handle dot-separated paths like "std.collections.Vec"
        while self.match_token(&[Token::Dot]) {
            path.push(self.consume_identifier("Expected identifier after '.'")?);
        }

        let mut items = Vec::new();
        let mut alias = None;

        // Handle import items if present: import std.io.{read_file, write_file}
        if self.match_token(&[Token::LBrace]) {
            // Parse import items
            loop {
                if self.match_token(&[Token::Star]) {
                    items.push(ImportItem::All);
                } else {
                    let item_name = self.consume_identifier("Expected import item name")?;

                    if self.match_token(&[Token::As]) {
                        let item_alias = self.consume_identifier("Expected alias after 'as'")?;
                        items.push(ImportItem::NamedWithAlias {
                            name: item_name,
                            alias: item_alias,
                        });
                    } else {
                        items.push(ImportItem::Named(item_name));
                    }
                }

                if !self.match_token(&[Token::Comma]) {
                    break;
                }
            }

            self.consume(&Token::RBrace, "Expected '}' after import items")?;
        } else if self.match_token(&[Token::As]) {
            // Handle module alias: import std.io as io
            alias = Some(self.consume_identifier("Expected alias after 'as'")?);
            items.push(ImportItem::All); // Import all items when aliasing
        } else {
            // Simple import: import std.io
            items.push(ImportItem::All);
        }

        Ok(Statement::ImportDeclaration {
            path,
            items,
            alias,
            is_public,
        })
    }

    fn import_declaration(&mut self) -> Result<Statement> {
        self.import_declaration_with_visibility(false)
    }

    fn parse_array_literal(&mut self) -> Result<Expr> {
        let mut elements = Vec::new();

        // Empty array case
        if self.check(&Token::RBracket) {
            self.advance(); // Consume ']'
            return Ok(Expr::ArrayLiteral(elements));
        }

        loop {
            elements.push(self.expression()?);

            if self.match_token(&[Token::Comma]) {
                if self.check(&Token::RBracket) {
                    break;
                }
                continue;
            }

            if self.check(&Token::RBracket) {
                break;
            }
        }

        self.consume(&Token::RBracket, "Expected ']' after array elements")?;

        Ok(Expr::ArrayLiteral(elements))
    }

    fn enum_declaration(&mut self) -> Result<Statement> {
        self.enum_declaration_with_visibility(false)
    }

    fn enum_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        let name = self.consume_identifier("Expected enum name after 'enum'")?;

        // Parse optional generic parameters
        let generic_params = self.parse_generic_args_if_present()?;

        let mut variants = Vec::new();

        // Handle single-line enums: enum Color(Red, Green, Blue)
        if self.match_token(&[Token::LParen]) {
            loop {
                let variant_name = self.consume_identifier("Expected variant name")?;
                variants.push(EnumVariant {
                    name: variant_name,
                    fields: None,
                    methods: HashMap::new(),
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
                    methods: HashMap::new(),
                });

                // Optional comma
                self.match_token(&[Token::Comma]);
            }

            self.consume(&Token::End, "Expected 'end' after enum body")?;
        }

        Ok(Statement::EnumDeclaration {
            name,
            variants,
            is_public,
            generic_params,
        })
    }

    fn kind_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        let name = self.consume_identifier("Expected kind name")?;

        let generic_params = self.parse_generic_args_if_present()?;

        // Check for kind inheritance (e.g., GrowableSequence<T>: Sequence<T>)
        let _parent_kind =
            if self.match_token(&[Token::Colon]) || self.match_token(&[Token::LeftArrow]) {
                // Parse the parent kind name
                let parent_name = self.consume_identifier("Expected parent kind name")?;
                // Parse optional generic parameters for parent kind
                let _parent_generics = self.parse_generic_args_if_present()?;
                Some(parent_name)
            } else {
                None
            };

        let mut methods = Vec::new();
        if self.match_token(&[Token::Equals]) {
            self.consume(&Token::Fn, "Expected 'fn' after in method declaration")?;
            let method_name = self.consume_identifier("Expected method name")?;

            // Parse parameters
            self.consume(&Token::LParen, "Expected '(' after method name")?;
            let mut params = Vec::new();

            if !self.check(&Token::RParen) {
                loop {
                    let param_name = self.consume_parameter_name("Expected parameter name")?;

                    // Special handling for 'self' parameter
                    let param_type = if param_name == "self" && !self.check(&Token::Colon) {
                        // For 'self' parameter without explicit type, use Self
                        TypeAnnotation::Basic("Self".to_string())
                    } else {
                        // For other parameters or when self has explicit type
                        self.consume(&Token::Colon, "Expected ':' after parameter name")?;
                        self.parse_type()?
                    };

                    params.push((param_name, param_type));

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }

            self.consume(&Token::RParen, "Expected ')' after parameters")?;

            // Parse return type
            self.consume(&Token::Arrow, "Expected '->' after parameters")?;
            let return_type = self.parse_type()?;

            // Parse optional default implementation
            let default_impl = if self.match_token(&[Token::Do]) {
                let mut body = Vec::new();
                while !self.check(&Token::End) && !self.is_at_end() {
                    body.push(self.declaration()?);
                }
                self.consume(&Token::End, "Expected 'end' after method body")?;
                Some(body)
            } else {
                None
            };

            methods.push(KindMethod {
                name: method_name,
                params,
                return_type,
                default_impl,
                is_public: true, // Kind methods are public by default
            });
        } else {
            // Multi-line kind definition
            while !self.check(&Token::End) && !self.is_at_end() {
                let method_visibility = self.match_token(&[Token::Pub]);
                self.consume(&Token::Fn, "Expected 'fn' in method declaration")?;
                let method_name = self.consume_identifier("Expected method name")?;

                // Parse parameters
                self.consume(&Token::LParen, "Expected '(' after method name")?;
                let mut params = Vec::new();

                if !self.check(&Token::RParen) {
                    loop {
                        let param_name = self.consume_parameter_name("Expected parameter name")?;

                        // Special handling for 'self' parameter
                        let param_type = if param_name == "self" && !self.check(&Token::Colon) {
                            // For 'self' parameter without explicit type, use Self
                            TypeAnnotation::Basic("Self".to_string())
                        } else {
                            // For other parameters or when self has explicit type
                            self.consume(&Token::Colon, "Expected ':' after parameter name")?;
                            self.parse_type()?
                        };

                        params.push((param_name, param_type));

                        if !self.match_token(&[Token::Comma]) {
                            break;
                        }
                    }
                }

                self.consume(&Token::RParen, "Expected ')' after parameters")?;

                // Parse return type
                self.consume(&Token::Arrow, "Expected '->' after parameters")?;
                let return_type = self.parse_type()?;

                // Parse optional default implementation
                let default_impl = if self.match_token(&[Token::Do]) {
                    let mut body = Vec::new();
                    while !self.check(&Token::End) && !self.is_at_end() {
                        body.push(self.declaration()?);
                    }
                    self.consume(&Token::End, "Expected 'end' after method body")?;
                    Some(body)
                } else {
                    None
                };

                methods.push(KindMethod {
                    name: method_name,
                    params,
                    return_type,
                    default_impl,
                    is_public: method_visibility,
                });
            }

            self.consume(&Token::End, "Expected 'end' after kind body")?;
        }

        Ok(Statement::KindDeclaration {
            name,
            methods,
            is_public,
            generic_params,
        })
    }

    fn plex_declaration_with_visibility(&mut self, is_public: bool) -> Result<Statement> {
        let name = self.consume_identifier("Expected plex name")?;

        // Parse generic parameters if present
        let mut generic_params = Vec::new();
        if self.match_token(&[Token::Less]) {
            if !self.check(&Token::Greater) {
                loop {
                    let param_name = self.consume_identifier("Expected generic parameter name")?;
                    let mut constraints = Vec::new();

                    // Parse constraints if present (e.g., T: Add + Mul)
                    if self.match_token(&[Token::Colon]) {
                        loop {
                            constraints.push(self.parse_type()?);
                            if !self.match_token(&[Token::Plus]) {
                                break;
                            }
                        }
                    }

                    generic_params.push(GenericArgument {
                        name: None,
                        type_annotation: TypeAnnotation::Basic(param_name),
                        constraints,
                    });

                    if !self.match_token(&[Token::Comma]) {
                        break;
                    }
                }
            }
            self.consume(&Token::Greater, "Expected '>' after generic parameters")?;
        }

        self.consume(&Token::Equals, "Expected '=' after plex name")?;
        let type_annotation = self.parse_type()?;

        Ok(Statement::PlexDeclaration {
            name,
            type_annotation,
            is_public,
            generic_params,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Literal, Statement, TypeAnnotation, VarKind};
    use crate::lexer::Lexer;

    fn parse_code(input: &str) -> Vec<Statement> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.collect_tokens().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    #[test]
    fn test_variable_declaration() {
        let input = "let x = 42";
        let statements = parse_code(input);

        assert_eq!(statements.len(), 1);
        match &statements[0] {
            Statement::VariableDeclaration {
                name,
                var_kind,
                type_annotation,
                value,
                is_public,
            } => {
                assert_eq!(name, "x");
                assert!(matches!(var_kind, VarKind::Let));
                assert!(type_annotation.is_none());
                assert!(!is_public);

                match &**value {
                    Expr::Literal(Literal::Integer(val)) => assert_eq!(*val, 42),
                    _ => panic!("Expected integer literal"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_variable_declaration_with_type() {
        let input = "let y: i32 = 10";
        let statements = parse_code(input);

        assert_eq!(statements.len(), 1);
        match &statements[0] {
            Statement::VariableDeclaration {
                name,
                var_kind,
                type_annotation,
                value,
                ..
            } => {
                assert_eq!(name, "y");
                assert!(matches!(var_kind, VarKind::Let));

                match type_annotation {
                    Some(TypeAnnotation::Basic(type_name)) => assert_eq!(type_name, "i32"),
                    _ => panic!("Expected basic type annotation"),
                }

                match &**value {
                    Expr::Literal(Literal::Integer(val)) => assert_eq!(*val, 10),
                    _ => panic!("Expected integer literal"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_mutable_variable() {
        let input = "var counter = 0";
        let statements = parse_code(input);

        assert_eq!(statements.len(), 1);
        match &statements[0] {
            Statement::VariableDeclaration {
                name,
                var_kind,
                value,
                ..
            } => {
                assert_eq!(name, "counter");
                assert!(matches!(var_kind, VarKind::Var));

                match &**value {
                    Expr::Literal(Literal::Integer(val)) => assert_eq!(*val, 0),
                    _ => panic!("Expected integer literal"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }
    #[test]
    fn test_simple_function_declaration() {
        let input = r#"
            fn add(a: i32, b: i32) -> i32
                a + b
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                is_proc,
                is_public,
                ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "a");
                assert_eq!(params[1].0, "b");

                match &return_type {
                    TypeAnnotation::Basic(type_name) => assert_eq!(type_name, "i32"),
                    _ => panic!("Expected basic return type"),
                }

                assert_eq!(body.len(), 1);
                assert!(!is_proc);
                assert!(!is_public);

                match &body[0] {
                    Statement::Return(Some(Expr::BinaryOp { .. })) => (),
                    _ => panic!("Expected return of binary operation expression"),
                }
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_proc_declaration() {
        let input = r#"
            proc greet(name: str)
                let unused = name == "John"
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::ProcDeclaration {
                name,
                params,
                body,
                is_public,
                ..
            } => {
                assert_eq!(name, "greet");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "name");

                match &params[0].1 {
                    TypeAnnotation::Basic(type_name) => assert_eq!(type_name, "str"),
                    _ => panic!("Expected basic parameter type"),
                }

                assert_eq!(body.len(), 1);
                assert!(!is_public);
            }
            _ => panic!("Expected proc declaration"),
        }
    }

    #[test]
    fn test_if_statement() {
        let input = r#"
            if x > 10 then
                y = 1
            else
                y = 2
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                match &condition {
                    Expr::BinaryOp { operator, .. } => {
                        assert!(matches!(operator, BinaryOperator::Greater));
                    }
                    _ => panic!("Expected binary operation"),
                }

                assert_eq!(then_branch.len(), 1);
                assert!(else_branch.is_some());
                assert_eq!(else_branch.as_ref().unwrap().len(), 1);
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_while_loop() {
        let input = r#"
            while i < 10 do
                i = i + 1
                j = i
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::While { condition, body } => {
                match &condition {
                    Expr::BinaryOp { operator, .. } => {
                        assert!(matches!(operator, BinaryOperator::Less));
                    }
                    _ => panic!("Expected binary operation"),
                }

                assert_eq!(body.len(), 2);
            }
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_for_loop() {
        let input = r#"
            for item in collection do
                process(item)
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::For {
                iterator,
                iterable,
                body,
            } => {
                assert_eq!(iterator, "item");

                match &iterable {
                    Expr::Identifier(name) => assert_eq!(name, "collection"),
                    _ => panic!("Expected identifier"),
                }

                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_struct_declaration() {
        let input = r#"
            struct Point
                x: f64,
                y: f64,

                fn distance(self) -> f64
                    (self.x * self.x + self.y * self.y).sqrt()
                end
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::StructDeclaration {
                name,
                fields,
                methods,
                is_public,
                ..
            } => {
                assert_eq!(name, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(methods.len(), 1);
                assert!(!is_public);

                assert_eq!(fields[0].name, "x");
                assert_eq!(fields[1].name, "y");

                assert_eq!(methods[0].name, "distance");
                assert_eq!(methods[0].params.len(), 1);
                assert_eq!(methods[0].params[0].0, "self");
            }
            _ => panic!("Expected struct declaration"),
        }
    }

    #[test]
    fn test_module_declaration() {
        let input = r#"
            mod math
                fn add(a: i32, b: i32) -> i32 => a + b

                fn multiply(a: i32, b: i32) -> i32 => a * b
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::ModuleDeclaration {
                name,
                body,
                is_public,
            } => {
                assert_eq!(name, "math");
                assert!(body.is_some());
                assert_eq!(body.as_ref().unwrap().len(), 2);
                assert!(!is_public);
            }
            _ => panic!("Expected module declaration"),
        }
    }

    #[test]
    fn test_import_declaration() {
        let input = r#"import math.{add, subtract}"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::ImportDeclaration {
                path,
                items,
                alias,
                is_public,
            } => {
                assert_eq!(path, &vec!["math".to_string()]);
                assert_eq!(items.len(), 2);
                assert!(alias.is_none());
                assert!(!is_public);
            }
            _ => panic!("Expected import declaration"),
        }
    }

    #[test]
    fn test_enum_declaration() {
        let input = r#"
            enum Shape
                Circle(f64)
                Rectangle(f64, f64)
                Triangle(f64, f64, f64)
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::EnumDeclaration {
                name,
                variants,
                is_public,
                ..
            } => {
                assert_eq!(name, "Shape");
                assert_eq!(variants.len(), 3);
                assert!(!is_public);

                assert_eq!(variants[0].name, "Circle");
                assert_eq!(variants[1].name, "Rectangle");
                assert_eq!(variants[2].name, "Triangle");

                assert!(variants[0].fields.is_some());
                assert_eq!(variants[0].fields.as_ref().unwrap().len(), 1);
                assert_eq!(variants[1].fields.as_ref().unwrap().len(), 2);
                assert_eq!(variants[2].fields.as_ref().unwrap().len(), 3);
            }
            _ => panic!("Expected enum declaration"),
        }
    }

    #[test]
    fn test_arrow_function() {
        let input = "fn square(x: i32) -> i32 => x * x";

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                ..
            } => {
                assert_eq!(name, "square");
                assert_eq!(params.len(), 1);

                match &return_type {
                    TypeAnnotation::Basic(type_name) => assert_eq!(type_name, "i32"),
                    _ => panic!("Expected basic return type"),
                }

                assert_eq!(body.len(), 1);

                match &body[0] {
                    Statement::Return(Some(Expr::BinaryOp { .. })) => (),
                    other => panic!("Expected return of binary operation, got: {:?}", other),
                }
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_lambda_expression() {
        let input = "let add_one = x => x + 1";

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "add_one");

                match &**value {
                    Expr::Lambda { params, body, .. } => {
                        assert_eq!(params.len(), 1);
                        assert_eq!(params[0].0, "x");

                        match &**body {
                            Expr::BinaryOp { .. } => (),
                            _ => panic!("Expected binary operation"),
                        }
                    }
                    _ => panic!("Expected lambda expression"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_block_lambda() {
        let input = r#"
            let process = data => do
                let result = transform(data)
                filter(result)
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "process");

                match &**value {
                    Expr::BlockLambda { params, body, .. } => {
                        assert_eq!(params.len(), 1);
                        assert_eq!(params[0].0, "data");
                        assert_eq!(body.len(), 2);
                    }
                    _ => panic!("Expected block lambda"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_kind_declaration() {
        let input = r#"
            kind Shape
                fn area(self) -> f64
                fn perimeter(self) -> f64
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::KindDeclaration {
                name,
                methods,
                is_public,
                ..
            } => {
                assert_eq!(name, "Shape");
                assert_eq!(methods.len(), 2);
                assert!(!is_public);

                assert_eq!(methods[0].name, "area");
                assert_eq!(methods[1].name, "perimeter");
            }
            _ => panic!("Expected kind declaration"),
        }
    }

    #[test]
    fn test_implementation() {
        let input = r#"
            impl Circle <- Shape
                fn area(self) -> f64 => 3.14 * self.radius * self.radius
                fn perimeter(self) -> f64 => 2.0 * 3.14 * self.radius
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Implementation {
                type_name,
                kind_name,
                methods,
                ..
            } => {
                assert_eq!(type_name, "Circle");
                assert_eq!(kind_name.as_ref().unwrap(), "Shape");
                assert_eq!(methods.len(), 2);

                assert_eq!(methods[0].name, "area");
                assert_eq!(methods[1].name, "perimeter");
            }
            _ => panic!("Expected implementation"),
        }
    }

    #[test]
    fn test_generic_function() {
        let input = r#"
            fn identity<T>(value: T) -> T
                value
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                generic_params,
                ..
            } => {
                assert_eq!(name, "identity");
                assert_eq!(params.len(), 1);
                assert_eq!(generic_params.len(), 1);

                match &generic_params[0].type_annotation {
                    TypeAnnotation::Basic(name) => assert_eq!(name, "T"),
                    _ => panic!("Expected basic type"),
                }

                match return_type {
                    TypeAnnotation::Basic(name) => assert_eq!(name, "T"),
                    _ => panic!("Expected basic type"),
                }
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_macro_declaration() {
        let input = r#"
            macro~ vec
                () => new_vec(),
                ($elem:expr) => do
                    let mut temp = new_vec()
                    temp.push($elem)
                    temp
                end
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::MacroDeclaration { name, patterns, .. } => {
                assert_eq!(name, "vec");
                assert_eq!(patterns.len(), 2);
            }
            _ => panic!("Expected macro declaration"),
        }
    }

    #[test]
    fn test_macro_invocation() {
        let input = r#"let numbers = vec~(1, 2, 3)"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "numbers");

                match &**value {
                    Expr::MacroExpr { name, arguments } => {
                        assert_eq!(name, "vec");
                        assert_eq!(arguments.len(), 3);
                    }
                    _ => panic!("Expected macro expression"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_array_literal() {
        let input = r#"let numbers = [1, 2, 3, 4, 5]"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "numbers");

                match &**value {
                    Expr::ArrayLiteral(elements) => {
                        assert_eq!(elements.len(), 5);
                    }
                    _ => panic!("Expected array literal"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_struct_instantiation() {
        let input = r#"let p = Point(x: 10.0, y: 20.0)"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "p");

                match &**value {
                    Expr::StructCreate {
                        struct_name,
                        fields,
                    } => {
                        assert_eq!(struct_name, "Point");
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].0, "x");
                        assert_eq!(fields[1].0, "y");
                    }
                    other => panic!("Expected struct creation, got: {:?}", other),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_method_call() {
        let input = r#"let area = circle.area()"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "area");

                match &**value {
                    Expr::MethodCall {
                        object,
                        method,
                        arguments,
                    } => {
                        match &**object {
                            Expr::Identifier(name) => assert_eq!(name, "circle"),
                            _ => panic!("Expected identifier"),
                        }

                        assert_eq!(method, "area");
                        assert_eq!(arguments.len(), 0);
                    }
                    _ => panic!("Expected method call, got {:?}", value),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_property_access() {
        let input = r#"let x_coord = point.x"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "x_coord");

                match &**value {
                    Expr::PropertyAccess { object, property } => {
                        match &**object {
                            Expr::Identifier(name) => assert_eq!(name, "point"),
                            _ => panic!("Expected identifier"),
                        }

                        assert_eq!(property, "x");
                    }
                    _ => panic!("Expected property access"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_enum_variant_creation() {
        let input = r#"let shape = Shape.Circle(5.0)"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "shape");

                match &**value {
                    Expr::EnumVariant {
                        enum_name,
                        variant_name,
                        fields,
                        type_args,
                    } => {
                        assert_eq!(enum_name, "Shape");
                        assert_eq!(variant_name, "Circle");
                        assert_eq!(fields.len(), 1);
                    }
                    _ => panic!("Expected enum variant"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_pipeline_operator() {
        let input = r#"let result = data |> process |> format"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "result");

                match &**value {
                    Expr::BinaryOp { operator, .. } => {
                        assert!(matches!(operator, BinaryOperator::Pipe));
                    }
                    _ => panic!("Expected pipeline operation"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_if_expression() {
        let input = r#"let max = if a > b then a else b end"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "max");

                match &**value {
                    Expr::IfExpression {
                        condition,
                        then_expr,
                        else_expr,
                    } => {
                        match &**condition {
                            Expr::BinaryOp { operator, .. } => {
                                assert!(matches!(operator, BinaryOperator::Greater));
                            }
                            _ => panic!("Expected binary operation"),
                        }

                        match &**then_expr {
                            Expr::Identifier(name) => assert_eq!(name, "a"),
                            _ => panic!("Expected identifier"),
                        }

                        match else_expr {
                            Some(expr) => match &**expr {
                                Expr::Identifier(name) => assert_eq!(name, "b"),
                                _ => panic!("Expected identifier"),
                            },
                            _ => panic!("Expected identifier"),
                        }
                    }
                    _ => panic!("Expected if expression"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_block_expression() {
        let input = r#"
            let result = do
                let temp = x * 2
                temp + y
            end
        "#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "result");

                match &**value {
                    Expr::BlockExpression {
                        statements,
                        final_expr,
                    } => {
                        assert_eq!(statements.len(), 1);
                        assert!(final_expr.is_some());
                    }
                    _ => panic!("Expected block expression"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_type_cast() {
        let input = r#"let integer_value = floating_value as i32"#;

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::VariableDeclaration { name, value, .. } => {
                assert_eq!(name, "integer_value");

                match &**value {
                    Expr::TypeCast { expr, target_type } => {
                        match &**expr {
                            Expr::Identifier(name) => assert_eq!(name, "floating_value"),
                            _ => panic!("Expected identifier"),
                        }

                        match target_type {
                            TypeAnnotation::Basic(name) => assert_eq!(name, "i32"),
                            _ => panic!("Expected basic type"),
                        }
                    }
                    _ => panic!("Expected type cast"),
                }
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_match_statement() {
        let input = r#"
            match shape
                Shape.Circle(radius) => 3.14 * radius * radius,
                Shape.Rectangle(w, h) => w * h,
                Shape.Triangle(a, b, c) => do
                    let s = (a + b + c) / 2.0
                    (s * (s - a) * (s - b) * (s - c)).sqrt()
                end,
            end
        "#;

        // Dump tokens for debugging
        let mut lexer = crate::lexer::Lexer::new(input);
        let _tokens: Vec<_> = lexer.collect_tokens().unwrap();

        let statements = parse_code(input);
        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Match { value, arms } => {
                match value {
                    Expr::Identifier(name) => assert_eq!(name, "shape"),
                    _ => panic!("Expected identifier"),
                }

                assert_eq!(arms.len(), 3);
            }
            _ => panic!("Expected match statement"),
        }
    }
}

// #[ignore = "Anonymous structs are not fully fleshed out yet."]
#[test]
fn test_anon_record() {
    let input = r#"
        let point = {
            x: 10.0,
            y: 20.0,
        }
    "#;

    let mut lexer = crate::lexer::Lexer::new(input);
    let tokens = lexer.collect_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let statements = parser.parse().unwrap();
    assert_eq!(statements.len(), 1);
    assert!(
        matches!(&statements[0], Statement::VariableDeclaration { name, value, .. } if name == "point" && value.is_record())
    );
}

// #[ignore = "Anonymous enums are not fully fleshed out yet, and may not even be in the final design or implemented at all."]
// #[test]
// fn test_anon_enum_no_methods() {
//     // TS-style with '|' syntax as a delimiter for shorter syntax
//     let proposed_syntax1 = r#"
//         let shape = enum
//             Circle(f64) |
//             Rectangle(f64, f64)
//             # specify which enum variants are the instance named shape is (Circle or Rectangle)
//         as Circle(2.0) # Specify value of the instance (Circle with radius 2.0)
//     "#;

//     let proposed_syntax2 = r#"
//         let shape : enum Circle(f64) | Rectangle(f64, f64) = Circle(2.0)
//     "#;

//     let input = proposed_syntax2; // or proposed_syntax1
// }

impl Parser {
    fn is_start_of_expression(&self) -> bool {
        match self.peek() {
            Token::Identifier(_)
            | Token::IntegerLiteral(_)
            | Token::FloatLiteral(_)
            | Token::StringLiteral(_)
            | Token::True
            | Token::False
            | Token::LParen
            | Token::Fn
            | Token::Do
            | Token::If
            | Token::SelfToken => true,
            Token::LBrace => true, // Start of Record instance, let's hope it doesn't break anything
            _ => false,
        }
    }
}
