use std::collections::HashMap;
use veld_ast::{Expr, MacroExpansion, MacroPattern, MacroTemplate, Statement};
use veld_common::source::NodeId;

use crate::{parser::MacroParser, ExpansionContext, ExpansionError, MacroDefinition};

/// Integration point for the macro system with the Veld interpreter/compiler
pub struct MacroSystem {
    expansion_context: ExpansionContext,
    builtin_macros: HashMap<String, BuiltinMacro>,
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
                    veld_ast::Literal::String(template_str),
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
                            veld_ast::Argument::Positional(expr) => expr,
                            veld_ast::Argument::Named { value, .. } => value,
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
                            veld_ast::Argument::Positional(expr) => self
                                .preprocess_expr(expr)
                                .map(veld_ast::Argument::Positional),
                            veld_ast::Argument::Named { name, value } => self
                                .preprocess_expr(value)
                                .map(|processed_value| veld_ast::Argument::Named {
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
        // vec~() -> Vec.new()
        Ok(vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "Vec.new".to_string(),
            arguments: vec![],
        })])
    } else {
        // vec!(a, b, c) -> [a, b, c]
        Ok(vec![Statement::ExprStatement(Expr::ArrayLiteral(
            args.to_vec(),
        ))])
    }
}

fn expand_format_macro(
    args: &[Expr],
    _call_site: NodeId,
) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        return Err(ExpansionError::ArgumentCountMismatch {
            macro_name: "format".to_string(),
            expected: 1,
            got: 0,
            call_site: _call_site,
        });
    }

    // format!("Hello {}", name) -> format("Hello {}", name)
    Ok(vec![Statement::ExprStatement(Expr::FunctionCall {
        name: "format".to_string(),
        arguments: args
            .iter()
            .map(|arg| veld_ast::Argument::Positional(arg.clone()))
            .collect(),
    })])
}

fn expand_println_macro(
    args: &[Expr],
    _call_site: NodeId,
) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        // println!() -> println("")
        Ok(vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "println".to_string(),
            arguments: vec![veld_ast::Argument::Positional(Expr::Literal(
                veld_ast::Literal::String("".to_string()),
            ))],
        })])
    } else if args.len() == 1 {
        // println!(expr) -> println(expr)
        Ok(vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "println".to_string(),
            arguments: vec![veld_ast::Argument::Positional(args[0].clone())],
        })])
    } else {
        // println!("format", args...) -> println(format("format", args...))
        let format_call = Expr::FunctionCall {
            name: "format".to_string(),
            arguments: args
                .iter()
                .map(|arg| veld_ast::Argument::Positional(arg.clone()))
                .collect(),
        };

        Ok(vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "println".to_string(),
            arguments: vec![veld_ast::Argument::Positional(format_call)],
        })])
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
            var_kind: veld_ast::VarKind::Let,
            type_annotation: None,
            value: Box::new(expr.clone()),
            is_public: false,
        },
        Statement::ExprStatement(Expr::FunctionCall {
            name: "println".to_string(),
            arguments: vec![
                veld_ast::Argument::Positional(Expr::Literal(veld_ast::Literal::String(
                    "DEBUG: {}".to_string(),
                ))),
                veld_ast::Argument::Positional(Expr::Identifier(var_name.clone())),
            ],
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
        Expr::Literal(veld_ast::Literal::String("Assertion failed".to_string()))
    };

    // assert!(condition) -> if !condition { panic!("Assertion failed") }
    Ok(vec![Statement::If {
        condition: Expr::UnaryOp {
            operator: veld_ast::UnaryOperator::Not,
            operand: Box::new(condition.clone()),
        },
        then_branch: vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "panic".to_string(),
            arguments: vec![veld_ast::Argument::Positional(message)],
        })],
        else_branch: None,
    }])
}

fn expand_todo_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    let message = if args.is_empty() {
        Expr::Literal(veld_ast::Literal::String(
            "TODO: not yet implemented".to_string(),
        ))
    } else {
        args[0].clone()
    };

    // todo!() -> panic!("TODO: not yet implemented")
    Ok(vec![Statement::ExprStatement(Expr::FunctionCall {
        name: "panic".to_string(),
        arguments: vec![veld_ast::Argument::Positional(message)],
    })])
}

#[cfg(test)]
mod tests {
    use super::*;
    use veld_ast::*;

    #[test]
    fn test_vec_macro_empty() {
        let mut macro_system = MacroSystem::new();
        let result = macro_system
            .expand_macro_call("vec", &[], NodeId::new())
            .unwrap();

        assert_eq!(result.len(), 1);
        match &result[0] {
            Statement::ExprStatement(Expr::FunctionCall { name, .. }) => {
                assert_eq!(name, "Vec.new");
            }
            _ => panic!("Expected function call"),
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
            Statement::ExprStatement(Expr::ArrayLiteral(elements)) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected array literal"),
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
            Statement::ExprStatement(Expr::FunctionCall { name, arguments }) => {
                assert_eq!(name, "format");
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected function call"),
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
            Statement::ExprStatement(Expr::FunctionCall { name, .. }) => {
                assert_eq!(name, "println");
            }
            _ => panic!("Expected function call"),
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
