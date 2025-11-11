use std::collections::HashMap;
use veld_common::ast::{Expr, MacroExpansion, MacroPattern, MacroTemplate, Statement};
use veld_common::source::NodeId;

use crate::{parser::MacroParser, ExpansionContext, ExpansionError, MacroDefinition};

/// Integration point for the macro system with the Veld interpreter/compiler
pub struct MacroSystem {
    expansion_context: ExpansionContext,
    pub builtin_macros: HashMap<String, BuiltinMacro>,
}

/// Represents a built-in macro implemented in Rust
pub struct BuiltinMacro {
    pub name: String,
    pub expand_fn: fn(&[Expr], NodeId) -> Result<Vec<Statement>, ExpansionError>,
    pub description: String,
}

impl MacroSystem {
    pub fn new() -> Self {
        let mut system = Self {
            expansion_context: ExpansionContext::with_builtin_macros(),
            builtin_macros: HashMap::new(),
        };

        system.register_builtin_macros();
        system
    }

    /// Register all built-in macros
    fn register_builtin_macros(&mut self) {
        // vec! macro
        self.builtin_macros.insert(
            "vec".to_string(),
            BuiltinMacro {
                name: "vec".to_string(),
                expand_fn: expand_vec_macro,
                description: "Creates a vector from elements".to_string(),
            },
        );

        // format! macro
        self.builtin_macros.insert(
            "format".to_string(),
            BuiltinMacro {
                name: "format".to_string(),
                expand_fn: expand_format_macro,
                description: "Formats a string with arguments".to_string(),
            },
        );

        // println! macro
        self.builtin_macros.insert(
            "println".to_string(),
            BuiltinMacro {
                name: "println".to_string(),
                expand_fn: expand_println_macro,
                description: "Prints a line to stdout".to_string(),
            },
        );

        // print! macro
        self.builtin_macros.insert(
            "print".to_string(),
            BuiltinMacro {
                name: "print".to_string(),
                expand_fn: expand_print_macro,
                description: "Prints to stdout without newline".to_string(),
            },
        );

        // debug! macro
        self.builtin_macros.insert(
            "debug".to_string(),
            BuiltinMacro {
                name: "debug".to_string(),
                expand_fn: expand_debug_macro,
                description: "Debug prints an expression".to_string(),
            },
        );

        // assert! macro
        self.builtin_macros.insert(
            "assert".to_string(),
            BuiltinMacro {
                name: "assert".to_string(),
                expand_fn: expand_assert_macro,
                description: "Runtime assertion".to_string(),
            },
        );

        // todo! macro
        self.builtin_macros.insert(
            "todo".to_string(),
            BuiltinMacro {
                name: "todo".to_string(),
                expand_fn: expand_todo_macro,
                description: "Placeholder for unimplemented code".to_string(),
            },
        );

        // panic! macro
        self.builtin_macros.insert(
            "panic".to_string(),
            BuiltinMacro {
                name: "panic".to_string(),
                expand_fn: expand_panic_macro,
                description: "Causes an unrecoverable error".to_string(),
            },
        );
    }

    /// Parse and register a user-defined macro from source code
    pub fn parse_and_register_macro(
        &mut self,
        source: &str,
        file_id: NodeId,
    ) -> Result<String, ExpansionError> {
        let mut parser = MacroParser::new(source);
        let (macro_name, patterns_and_templates) = parser.parse_macro_definition()?;

        let mut patterns = Vec::new();
        let mut templates = Vec::new();

        for (pattern_str, template_str) in patterns_and_templates {
            patterns.push(MacroPattern(pattern_str));
            templates.push(MacroTemplate {
                pattern: MacroPattern(template_str.clone()),
                expansion: MacroExpansion(vec![Statement::ExprStatement(Expr::Literal(
                    veld_common::ast::Literal::String(template_str),
                ))]),
            });
        }

        let macro_def =
            MacroDefinition::declarative(macro_name.clone(), patterns, templates, file_id);

        self.expansion_context.define_macro(macro_def)?;
        Ok(macro_name)
    }

    /// Expand a macro call
    pub fn expand_macro_call(
        &mut self,
        name: &str,
        arguments: &[Expr],
        call_site: NodeId,
    ) -> Result<Vec<Statement>, ExpansionError> {
        // Check if it's a builtin macro first
        if let Some(builtin) = self.builtin_macros.get(name) {
            return (builtin.expand_fn)(arguments, call_site);
        }

        // Otherwise use the expansion context
        self.expansion_context
            .expand_macro(name, arguments, call_site)
    }

    /// Check if a macro is defined
    pub fn is_macro_defined(&self, name: &str) -> bool {
        self.builtin_macros.contains_key(name)
            || self.expansion_context.lookup_macro(name).is_some()
    }

    /// Get information about a macro
    pub fn get_macro_info(&self, name: &str) -> Option<String> {
        if let Some(builtin) = self.builtin_macros.get(name) {
            Some(builtin.description.clone())
        } else if let Some(_macro_def) = self.expansion_context.lookup_macro(name) {
            Some(format!("User-defined macro: {}", name))
        } else {
            None
        }
    }

    /// List all available macros
    pub fn list_macros(&self) -> Vec<String> {
        let mut macros: Vec<String> = self.builtin_macros.keys().cloned().collect();
        macros.extend(
            self.expansion_context
                .macros
                .keys()
                .cloned()
                .collect::<Vec<_>>(),
        );
        macros.sort();
        macros
    }

    /// Preprocess a statement to expand any macro calls
    pub fn preprocess_statement(
        &mut self,
        stmt: Statement,
    ) -> Result<Vec<Statement>, ExpansionError> {
        match stmt {
            Statement::MacroInvocation { name, arguments } => {
                self.expand_macro_call(&name, &arguments, NodeId::new())
            }
            Statement::ExprStatement(expr) => {
                let expanded_expr = self.preprocess_expr(expr)?;
                Ok(vec![Statement::ExprStatement(expanded_expr)])
            }
            Statement::VariableDeclaration {
                name,
                var_kind,
                type_annotation,
                value,
                is_public,
            } => {
                let expanded_value = Box::new(self.preprocess_expr(*value)?);
                Ok(vec![Statement::VariableDeclaration {
                    name,
                    var_kind,
                    type_annotation,
                    value: expanded_value,
                    is_public,
                }])
            }
            Statement::MacroDeclaration { name, patterns, .. } => {
                // Register the macro
                let patterns_vec: Vec<MacroPattern> =
                    patterns.into_iter().map(|(p, _)| p).collect();
                let templates_vec: Vec<MacroTemplate> = vec![]; // TODO: extract from patterns

                let macro_def = MacroDefinition::declarative(
                    name.clone(),
                    patterns_vec,
                    templates_vec,
                    NodeId::new(),
                );

                self.expansion_context.define_macro(macro_def)?;
                Ok(vec![]) // Macro definitions don't generate runtime code
            }
            _ => Ok(vec![stmt]), // Pass through other statements
        }
    }

    /// Preprocess an expression to expand any macro calls
    pub fn preprocess_expr(&mut self, expr: Expr) -> Result<Expr, ExpansionError> {
        match expr {
            Expr::MacroExpr { name, arguments } => {
                let expanded_stmts = self.expand_macro_call(&name, &arguments, NodeId::new())?;

                // If the expansion is a single expression statement, return the expression
                if expanded_stmts.len() == 1 {
                    if let Statement::ExprStatement(expanded_expr) = &expanded_stmts[0] {
                        return Ok(expanded_expr.clone());
                    }
                }

                // Otherwise wrap in a block expression
                Ok(Expr::BlockExpression {
                    statements: expanded_stmts,
                    final_expr: None,
                })
            }
            Expr::FunctionCall { name, arguments } => {
                // Check if this is actually a macro call disguised as a function call
                if self.is_macro_defined(&name) {
                    let arg_exprs: Vec<Expr> = arguments
                        .into_iter()
                        .map(|arg| match arg {
                            veld_common::ast::Argument::Positional(expr) => expr,
                            veld_common::ast::Argument::Named { value, .. } => value,
                        })
                        .collect();

                    let expanded_stmts =
                        self.expand_macro_call(&name, &arg_exprs, NodeId::new())?;

                    if expanded_stmts.len() == 1 {
                        if let Statement::ExprStatement(expanded_expr) = &expanded_stmts[0] {
                            return Ok(expanded_expr.clone());
                        }
                    }

                    Ok(Expr::BlockExpression {
                        statements: expanded_stmts,
                        final_expr: None,
                    })
                } else {
                    // Regular function call, preprocess arguments
                    let processed_args = arguments
                        .into_iter()
                        .map(|arg| match arg {
                            veld_common::ast::Argument::Positional(expr) => self
                                .preprocess_expr(expr)
                                .map(veld_common::ast::Argument::Positional),
                            veld_common::ast::Argument::Named { name, value } => self
                                .preprocess_expr(value)
                                .map(|processed_value| veld_common::ast::Argument::Named {
                                    name,
                                    value: processed_value,
                                }),
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(Expr::FunctionCall {
                        name,
                        arguments: processed_args,
                    })
                }
            }
            _ => Ok(expr), // Pass through other expressions
        }
    }
}

// Built-in macro implementations

fn expand_vec_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        // vec~() -> std.vec.Vec.new()
        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::PropertyAccess {
                    object: Box::new(Expr::Identifier("std".to_string())),
                    property: "vec".to_string(),
                }),
                property: "Vec".to_string(),
            }),
            method: "new".to_string(),
            arguments: vec![],
        })])
    } else {
        // vec~(a, b, c) -> { let v = std.vec.Vec.new(); v.push(a); v.push(b); v.push(c); v }
        let mut statements = Vec::new();

        // Create a new Vec
        let vec_creation = Statement::VariableDeclaration {
            name: "__vec".to_string(),
            var_kind: veld_common::ast::VarKind::Var,
            type_annotation: None,
            value: Box::new(Expr::MethodCall {
                object: Box::new(Expr::PropertyAccess {
                    object: Box::new(Expr::PropertyAccess {
                        object: Box::new(Expr::Identifier("std".to_string())),
                        property: "vec".to_string(),
                    }),
                    property: "Vec".to_string(),
                }),
                method: "new".to_string(),
                arguments: vec![],
            }),
            is_public: false,
        };
        statements.push(vec_creation);

        // Push each argument to the vec
        for arg in args {
            let push_call = Statement::ExprStatement(Expr::MethodCall {
                object: Box::new(Expr::Identifier("__vec".to_string())),
                method: "push".to_string(),
                arguments: vec![veld_common::ast::Argument::Positional(arg.clone())],
            });
            statements.push(push_call);
        }

        // Return the vec in a block expression
        Ok(vec![Statement::ExprStatement(Expr::BlockExpression {
            statements,
            final_expr: Some(Box::new(Expr::Identifier("__vec".to_string()))),
        })])
    }
}

fn expand_format_macro(args: &[Expr], call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        return Err(ExpansionError::ArgumentCountMismatch {
            macro_name: "format".to_string(),
            expected: 1,
            got: 0,
            call_site,
        });
    }

    // Get the format string
    let format_str = match &args[0] {
        Expr::Literal(veld_common::ast::Literal::String(s)) => s,
        _ => {
            return Err(ExpansionError::InvalidArgumentType {
                macro_name: "format".to_string(),
                param_name: "format_string".to_string(),
                expected: "string literal".to_string(),
                got: "expression".to_string(),
                call_site,
            });
        }
    };

    let format_args = &args[1..];

    // Count the number of {} placeholders
    let placeholder_count = format_str.matches("{}").count();

    // Validate argument count matches placeholder count
    if placeholder_count != format_args.len() {
        return Err(ExpansionError::ArgumentCountMismatch {
            macro_name: "format".to_string(),
            expected: placeholder_count,
            got: format_args.len(),
            call_site,
        });
    }

    // If no placeholders, just return the format string
    if placeholder_count == 0 {
        return Ok(vec![Statement::ExprStatement(Expr::Literal(
            veld_common::ast::Literal::String(format_str.clone()),
        ))]);
    }

    // Build the concatenated expression
    let mut result_expr = None;
    let mut current_pos = 0;
    let mut arg_index = 0;

    for (i, _) in format_str.match_indices("{}") {
        // Add the string part before this placeholder
        if i > current_pos {
            let str_part = format_str[current_pos..i].to_string();
            let str_literal = Expr::Literal(veld_common::ast::Literal::String(str_part));

            result_expr = Some(match result_expr {
                None => str_literal,
                Some(existing) => Expr::BinaryOp {
                    left: Box::new(existing),
                    operator: veld_common::ast::BinaryOperator::Add,
                    right: Box::new(str_literal),
                },
            });
        }

        // Add the argument - handle strings specially
        if arg_index < format_args.len() {
            let arg_expr = match &format_args[arg_index] {
                // String literals can be used directly
                Expr::Literal(veld_common::ast::Literal::String(_)) => {
                    format_args[arg_index].clone()
                }
                // For all other expressions (including identifiers), call to_string
                // This ensures proper type conversion at runtime
                _ => Expr::MethodCall {
                    object: Box::new(format_args[arg_index].clone()),
                    method: "to_string".to_string(),
                    arguments: vec![],
                },
            };

            result_expr = Some(match result_expr {
                None => arg_expr,
                Some(existing) => Expr::BinaryOp {
                    left: Box::new(existing),
                    operator: veld_common::ast::BinaryOperator::Add,
                    right: Box::new(arg_expr),
                },
            });

            arg_index += 1;
        }

        current_pos = i + 2; // Skip the "{}"
    }

    // Add any remaining string after the last placeholder
    if current_pos < format_str.len() {
        let str_part = format_str[current_pos..].to_string();
        let str_literal = Expr::Literal(veld_common::ast::Literal::String(str_part));

        result_expr = Some(match result_expr {
            None => str_literal,
            Some(existing) => Expr::BinaryOp {
                left: Box::new(existing),
                operator: veld_common::ast::BinaryOperator::Add,
                right: Box::new(str_literal),
            },
        });
    }

    let final_expr = result_expr.expect("Should have at least one placeholder or remaining string");

    Ok(vec![Statement::ExprStatement(final_expr)])
}

fn expand_print_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        // print!() -> std.io.print("")
        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "print".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(Expr::Literal(
                veld_common::ast::Literal::String("".to_string()),
            ))],
        })])
    } else if args.len() == 1 {
        // Check if the argument is a string literal with interpolation
        if let Expr::Literal(veld_common::ast::Literal::String(format_str)) = &args[0] {
            // Check if the string contains interpolation placeholders
            if format_str.contains('{') {
                // Parse and expand interpolation
                let expanded_expr = expand_string_interpolation(format_str)?;
                return Ok(vec![Statement::ExprStatement(Expr::MethodCall {
                    object: Box::new(Expr::PropertyAccess {
                        object: Box::new(Expr::Identifier("std".to_string())),
                        property: "io".to_string(),
                    }),
                    method: "print".to_string(),
                    arguments: vec![veld_common::ast::Argument::Positional(expanded_expr)],
                })]);
            }
        }

        // print!(expr) -> std.io.print(expr)
        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "print".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(args[0].clone())],
        })])
    } else {
        // print!("format", args...) -> print(format("format", args...))
        let format_call = Expr::FunctionCall {
            name: "format".to_string(),
            arguments: args
                .iter()
                .map(|arg| veld_common::ast::Argument::Positional(arg.clone()))
                .collect(),
        };

        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "print".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(format_call)],
        })])
    }
}

fn expand_println_macro(
    args: &[Expr],
    _call_site: NodeId,
) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        // println!() -> std.io.println("")
        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "println".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(Expr::Literal(
                veld_common::ast::Literal::String("".to_string()),
            ))],
        })])
    } else if args.len() == 1 {
        // Check if the argument is a string literal with interpolation
        if let Expr::Literal(veld_common::ast::Literal::String(format_str)) = &args[0] {
            // Check if the string contains interpolation placeholders
            if format_str.contains('{') {
                // Parse and expand interpolation
                let expanded_expr = expand_string_interpolation(format_str)?;
                return Ok(vec![Statement::ExprStatement(Expr::MethodCall {
                    object: Box::new(Expr::PropertyAccess {
                        object: Box::new(Expr::Identifier("std".to_string())),
                        property: "io".to_string(),
                    }),
                    method: "println".to_string(),
                    arguments: vec![veld_common::ast::Argument::Positional(expanded_expr)],
                })]);
            }
        }

        // println!(expr) -> std.io.println(expr)
        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "println".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(args[0].clone())],
        })])
    } else {
        // println!("format", args...) -> println(format("format", args...))
        let format_call = Expr::FunctionCall {
            name: "format".to_string(),
            arguments: args
                .iter()
                .map(|arg| veld_common::ast::Argument::Positional(arg.clone()))
                .collect(),
        };

        Ok(vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "println".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(format_call)],
        })])
    }
}

/// Parse a format string and expand interpolation into a concatenation expression
fn expand_string_interpolation(format_str: &str) -> Result<Expr, ExpansionError> {
    let parts = parse_format_string(format_str)?;

    if parts.is_empty() {
        // Empty string
        return Ok(Expr::Literal(veld_common::ast::Literal::String(
            "".to_string(),
        )));
    }

    if parts.len() == 1 {
        // Single part - return it directly
        return Ok(parts[0].clone());
    }

    // Multiple parts - concatenate them
    let mut result = parts[0].clone();
    for part in parts.into_iter().skip(1) {
        result = Expr::BinaryOp {
            left: Box::new(result),
            operator: veld_common::ast::BinaryOperator::Add,
            right: Box::new(part),
        };
    }

    Ok(result)
}

/// Parse a format string into a list of expressions
/// "Hello, {name}!" -> ["Hello, ", name, "!"]
fn parse_format_string(format_str: &str) -> Result<Vec<Expr>, ExpansionError> {
    let mut parts = Vec::new();
    let mut current_str = String::new();
    let mut chars = format_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            // Check for escaped brace {{
            if chars.peek() == Some(&'{') {
                chars.next(); // consume second {
                current_str.push('{');
                continue;
            }

            // Found interpolation start - save any accumulated string
            if !current_str.is_empty() {
                parts.push(Expr::Literal(veld_common::ast::Literal::String(
                    current_str.clone(),
                )));
                current_str.clear();
            }

            // Parse the expression inside {}
            let mut expr_str = String::new();
            let mut brace_depth = 1;

            while let Some(ch) = chars.next() {
                if ch == '{' {
                    brace_depth += 1;
                    expr_str.push(ch);
                } else if ch == '}' {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    expr_str.push(ch);
                } else {
                    expr_str.push(ch);
                }
            }

            if brace_depth != 0 {
                return Err(ExpansionError::InvalidFormatString {
                    message: "Unclosed brace in format string".to_string(),
                });
            }

            let expr_str = expr_str.trim();
            if expr_str.is_empty() {
                return Err(ExpansionError::InvalidFormatString {
                    message: "Empty interpolation placeholder".to_string(),
                });
            }

            // Parse the expression
            let expr = parse_interpolation_expr(expr_str)?;

            // For string literals, use them directly
            // For other expressions, wrap in a function call that converts to string
            let string_expr = if matches!(expr, Expr::Literal(veld_common::ast::Literal::String(_)))
            {
                expr
            } else {
                // Create a function call to convert value to string
                // The runtime will need to handle this conversion
                Expr::FunctionCall {
                    name: "to_str".to_string(),
                    arguments: vec![veld_common::ast::Argument::Positional(expr)],
                }
            };

            parts.push(string_expr);
        } else if ch == '}' {
            // Check for escaped brace }}
            if chars.peek() == Some(&'}') {
                chars.next(); // consume second }
                current_str.push('}');
                continue;
            }

            return Err(ExpansionError::InvalidFormatString {
                message: "Unmatched closing brace in format string".to_string(),
            });
        } else {
            current_str.push(ch);
        }
    }

    // Add any remaining string
    if !current_str.is_empty() {
        parts.push(Expr::Literal(veld_common::ast::Literal::String(
            current_str,
        )));
    }

    Ok(parts)
}

/// Parse an expression from a string (simple version)
fn parse_interpolation_expr(expr_str: &str) -> Result<Expr, ExpansionError> {
    // For now, we'll do a simple parser that handles:
    // - Identifiers: name
    // - Binary operations: x + y, x * 2, etc.
    // - Property access: obj.field
    // - Comparisons: x > y, x == y
    // - Logical operators: and, or

    // Try to parse as identifier first (most common case)
    let trimmed = expr_str.trim();

    // Check for binary operations (simple left-to-right parsing)
    // Priority order (lowest to highest precedence):
    // 1. Logical operators (and, or)
    // 2. Comparisons (==, !=, <, >, <=, >=)
    // 3. Arithmetic (+, -)
    // 4. Multiplication/Division (*, /, %)

    // Check for logical operators FIRST (lowest precedence)
    if let Some(pos) = trimmed.find(" and ") {
        let left_str = &trimmed[..pos].trim();
        let right_str = &trimmed[pos + 5..].trim();

        let left = parse_interpolation_expr(left_str)?;
        let right = parse_interpolation_expr(right_str)?;

        return Ok(Expr::BinaryOp {
            left: Box::new(left),
            operator: veld_common::ast::BinaryOperator::And,
            right: Box::new(right),
        });
    }

    if let Some(pos) = trimmed.find(" or ") {
        let left_str = &trimmed[..pos].trim();
        let right_str = &trimmed[pos + 4..].trim();

        let left = parse_interpolation_expr(left_str)?;
        let right = parse_interpolation_expr(right_str)?;

        return Ok(Expr::BinaryOp {
            left: Box::new(left),
            operator: veld_common::ast::BinaryOperator::Or,
            right: Box::new(right),
        });
    }

    // Check for comparison operators
    for (op_str, op) in &[
        ("==", veld_common::ast::BinaryOperator::EqualEqual),
        ("!=", veld_common::ast::BinaryOperator::NotEqual),
        ("<=", veld_common::ast::BinaryOperator::LessEq),
        (">=", veld_common::ast::BinaryOperator::GreaterEq),
        ("<", veld_common::ast::BinaryOperator::Less),
        (">", veld_common::ast::BinaryOperator::Greater),
    ] {
        if let Some(pos) = trimmed.find(op_str) {
            let left_str = &trimmed[..pos].trim();
            let right_str = &trimmed[pos + op_str.len()..].trim();

            if !left_str.is_empty() && !right_str.is_empty() {
                let left = parse_interpolation_expr(left_str)?;
                let right = parse_interpolation_expr(right_str)?;

                return Ok(Expr::BinaryOp {
                    left: Box::new(left),
                    operator: op.clone(),
                    right: Box::new(right),
                });
            }
        }
    }

    // Check for arithmetic operators (lowest precedence first)
    for (op_str, op) in &[
        (" + ", veld_common::ast::BinaryOperator::Add),
        (" - ", veld_common::ast::BinaryOperator::Subtract),
    ] {
        if let Some(pos) = trimmed.find(op_str) {
            let left_str = &trimmed[..pos].trim();
            let right_str = &trimmed[pos + op_str.len()..].trim();

            if !left_str.is_empty() && !right_str.is_empty() {
                let left = parse_interpolation_expr(left_str)?;
                let right = parse_interpolation_expr(right_str)?;

                return Ok(Expr::BinaryOp {
                    left: Box::new(left),
                    operator: op.clone(),
                    right: Box::new(right),
                });
            }
        }
    }

    // Check for multiplication/division (higher precedence)
    for (op_str, op) in &[
        (" * ", veld_common::ast::BinaryOperator::Multiply),
        (" / ", veld_common::ast::BinaryOperator::Divide),
        (" % ", veld_common::ast::BinaryOperator::Modulo),
    ] {
        if let Some(pos) = trimmed.find(op_str) {
            let left_str = &trimmed[..pos].trim();
            let right_str = &trimmed[pos + op_str.len()..].trim();

            if !left_str.is_empty() && !right_str.is_empty() {
                let left = parse_interpolation_expr(left_str)?;
                let right = parse_interpolation_expr(right_str)?;

                return Ok(Expr::BinaryOp {
                    left: Box::new(left),
                    operator: op.clone(),
                    right: Box::new(right),
                });
            }
        }
    }

    // Check for parentheses (but make sure they're balanced)
    if trimmed.starts_with('(') {
        let mut depth = 0;
        let mut closes_at_end = false;

        for (i, ch) in trimmed.chars().enumerate() {
            if ch == '(' {
                depth += 1;
            } else if ch == ')' {
                depth -= 1;
                if depth == 0 && i == trimmed.len() - 1 {
                    closes_at_end = true;
                }
            }
        }

        if closes_at_end {
            let inner = &trimmed[1..trimmed.len() - 1];
            return parse_interpolation_expr(inner);
        }
    }

    // Check for property access
    if let Some(dot_pos) = trimmed.find('.') {
        let object_str = &trimmed[..dot_pos].trim();
        let property_str = &trimmed[dot_pos + 1..].trim();

        if !object_str.is_empty() && !property_str.is_empty() {
            let object = parse_interpolation_expr(object_str)?;

            // Check if property access or method call
            if property_str.contains('(') {
                // Method call (simplified - just method name without args for now)
                let method_name = property_str.split('(').next().unwrap().trim();
                return Ok(Expr::MethodCall {
                    object: Box::new(object),
                    method: method_name.to_string(),
                    arguments: vec![],
                });
            } else {
                return Ok(Expr::PropertyAccess {
                    object: Box::new(object),
                    property: property_str.to_string(),
                });
            }
        }
    }

    // Check for string literal
    if (trimmed.starts_with('"') && trimmed.ends_with('"'))
        || (trimmed.starts_with('\'') && trimmed.ends_with('\''))
    {
        let content = &trimmed[1..trimmed.len() - 1];
        return Ok(Expr::Literal(veld_common::ast::Literal::String(
            content.to_string(),
        )));
    }

    // Check for numeric literal
    if let Ok(int_val) = trimmed.parse::<i32>() {
        return Ok(Expr::Literal(veld_common::ast::Literal::Integer(
            int_val as i64,
        )));
    }

    if let Ok(float_val) = trimmed.parse::<f64>() {
        return Ok(Expr::Literal(veld_common::ast::Literal::Float(float_val)));
    }

    // Check for boolean literal
    if trimmed == "true" {
        return Ok(Expr::Literal(veld_common::ast::Literal::Boolean(true)));
    }

    if trimmed == "false" {
        return Ok(Expr::Literal(veld_common::ast::Literal::Boolean(false)));
    }

    // Default: treat as identifier
    if trimmed.chars().all(|c| c.is_alphanumeric() || c == '_') {
        Ok(Expr::Identifier(trimmed.to_string()))
    } else {
        Err(ExpansionError::InvalidFormatString {
            message: format!("Invalid expression in interpolation: {}", trimmed),
        })
    }
}

fn expand_debug_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    if args.len() != 1 {
        return Err(ExpansionError::ArgumentCountMismatch {
            macro_name: "debug".to_string(),
            expected: 1,
            got: args.len(),
            call_site: _call_site,
        });
    }

    // debug!(expr) -> { let val = expr; println!("DEBUG: {} = {:?}", "expr", val); val }
    let expr = &args[0];
    let var_name = "debug_val".to_string();

    let statements = vec![
        Statement::VariableDeclaration {
            name: var_name.clone(),
            var_kind: veld_common::ast::VarKind::Let,
            type_annotation: None,
            value: Box::new(expr.clone()),
            is_public: false,
        },
        Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "println".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(Expr::Literal(
                veld_common::ast::Literal::String(format!("DEBUG: {}", var_name)),
            ))],
        }),
    ];

    Ok(vec![Statement::ExprStatement(Expr::BlockExpression {
        statements,
        final_expr: Some(Box::new(Expr::Identifier(var_name))),
    })])
}

fn expand_assert_macro(args: &[Expr], call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() || args.len() > 2 {
        return Err(ExpansionError::ArgumentCountMismatch {
            macro_name: "assert".to_string(),
            expected: 1,
            got: args.len(),
            call_site,
        });
    }

    let condition = &args[0];
    let message = if args.len() == 2 {
        args[1].clone()
    } else {
        Expr::Literal(veld_common::ast::Literal::String(
            "Assertion failed".to_string(),
        ))
    };

    // assert!(condition) -> if !condition { panic!("Assertion failed") }
    Ok(vec![Statement::If {
        condition: Expr::UnaryOp {
            operator: veld_common::ast::UnaryOperator::Not,
            operand: Box::new(condition.clone()),
        },
        then_branch: vec![Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier("std".to_string())),
                property: "io".to_string(),
            }),
            method: "println".to_string(),
            arguments: vec![veld_common::ast::Argument::Positional(message)],
        })],
        else_branch: None,
    }])
}

fn expand_todo_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    let message = if args.is_empty() {
        Expr::Literal(veld_common::ast::Literal::String(
            "TODO: not yet implemented".to_string(),
        ))
    } else {
        args[0].clone()
    };

    // todo!() -> std.io.println("TODO: not yet implemented")
    Ok(vec![Statement::ExprStatement(Expr::MethodCall {
        object: Box::new(Expr::PropertyAccess {
            object: Box::new(Expr::Identifier("std".to_string())),
            property: "io".to_string(),
        }),
        method: "println".to_string(),
        arguments: vec![veld_common::ast::Argument::Positional(message)],
    })])
}

fn expand_panic_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    let message = if args.is_empty() {
        Expr::Literal(veld_common::ast::Literal::String(
            "explicit panic".to_string(),
        ))
    } else {
        args[0].clone()
    };

    // panic~(msg) -> std.panic(msg)
    Ok(vec![Statement::ExprStatement(Expr::MethodCall {
        object: Box::new(Expr::Identifier("std".to_string())),
        method: "panic".to_string(),
        arguments: vec![veld_common::ast::Argument::Positional(message)],
    })])
}

#[cfg(test)]
mod tests {
    use super::*;
    use veld_common::ast::*;

    #[test]
    fn test_vec_macro_empty() {
        let mut macro_system = MacroSystem::new();
        let result = macro_system
            .expand_macro_call("vec", &[], NodeId::new())
            .unwrap();

        assert_eq!(result.len(), 1);
        match &result[0] {
            Statement::ExprStatement(Expr::MethodCall { object, method, .. }) => {
                assert_eq!(method, "new");
                // Verify it's calling std.vec.Vec.new
                match object.as_ref() {
                    Expr::PropertyAccess { property, .. } => {
                        assert_eq!(property, "Vec");
                    }
                    _ => panic!("Expected property access to Vec"),
                }
            }
            _ => panic!("Expected method call to Vec.new"),
        }
    }

    #[test]
    fn test_vec_macro_with_elements() {
        let mut macro_system = MacroSystem::new();
        let args = vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ];

        let result = macro_system
            .expand_macro_call("vec", &args, NodeId::new())
            .unwrap();

        assert_eq!(result.len(), 1);
        match &result[0] {
            Statement::ExprStatement(Expr::BlockExpression {
                statements,
                final_expr,
            }) => {
                // Should have: vec creation + 3 push calls
                assert_eq!(statements.len(), 4);

                // First statement should be Vec creation
                match &statements[0] {
                    Statement::VariableDeclaration { name, .. } => {
                        assert_eq!(name, "__vec");
                    }
                    _ => panic!("Expected variable declaration for Vec"),
                }

                // Next 3 statements should be push calls
                for i in 1..4 {
                    match &statements[i] {
                        Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
                            assert_eq!(method, "push");
                        }
                        _ => panic!("Expected push method call"),
                    }
                }

                // Final expression should return the vec
                assert!(final_expr.is_some());
            }
            _ => panic!("Expected block expression"),
        }
    }

    #[test]
    fn test_format_macro() {
        let mut macro_system = MacroSystem::new();
        let args = vec![
            Expr::Literal(Literal::String("Hello {}".to_string())),
            Expr::Literal(Literal::String("World".to_string())),
        ];

        let result = macro_system
            .expand_macro_call("format", &args, NodeId::new())
            .unwrap();

        assert_eq!(result.len(), 1);
        match &result[0] {
            Statement::ExprStatement(Expr::BinaryOp { .. }) => {
                // The format macro expands to concatenation expressions
                // for string interpolation with {} placeholders
            }
            _ => panic!("Expected binary operation for string concatenation"),
        }
    }

    #[test]
    fn test_println_macro() {
        let mut macro_system = MacroSystem::new();
        let args = vec![Expr::Literal(Literal::String("Hello".to_string()))];

        let result = macro_system
            .expand_macro_call("println", &args, NodeId::new())
            .unwrap();

        assert_eq!(result.len(), 1);
        match &result[0] {
            Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
                assert_eq!(method, "println");
            }
            _ => panic!("Expected method call"),
        }
    }

    #[test]
    fn test_debug_macro() {
        let mut macro_system = MacroSystem::new();
        let args = vec![Expr::Literal(Literal::Integer(42))];

        let result = macro_system
            .expand_macro_call("debug", &args, NodeId::new())
            .unwrap();

        assert_eq!(result.len(), 1);
        match &result[0] {
            Statement::ExprStatement(Expr::BlockExpression { statements, .. }) => {
                assert_eq!(statements.len(), 2);
            }
            _ => panic!("Expected block expression"),
        }
    }
}
