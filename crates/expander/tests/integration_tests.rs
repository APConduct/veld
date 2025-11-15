use veld_common::ast::*;
use veld_common::source::NodeId;
use veld_expander::integration::MacroSystem;

#[test]
fn test_vec_macro_basic() {
    let mut macro_system = MacroSystem::new();

    // Test empty vec
    let result = macro_system
        .expand_macro_call("vec", &[], NodeId::new())
        .unwrap();
    assert_eq!(result.len(), 1);

    match &result[0] {
        Statement::ExprStatement(Expr::MethodCall { object, method, .. }) => {
            assert_eq!(method, "new");
            match object.as_ref() {
                Expr::PropertyAccess {
                    object: inner_obj,
                    property,
                } => {
                    assert_eq!(property, "Vec");
                    match inner_obj.as_ref() {
                        Expr::PropertyAccess {
                            object: std_obj,
                            property: vec_prop,
                        } => {
                            assert_eq!(vec_prop, "vec");
                            match std_obj.as_ref() {
                                Expr::Identifier(name) => {
                                    assert_eq!(name, "std");
                                }
                                _ => panic!("Expected std identifier"),
                            }
                        }
                        _ => panic!("Expected std.vec property access"),
                    }
                }
                _ => panic!("Expected std.vec.Vec property access"),
            }
        }
        _ => panic!("Expected Vec.new method call"),
    }

    // Test vec with elements
    let args = vec![
        Expr::Literal(Literal::Integer(1)),
        Expr::Literal(Literal::Integer(2)),
        Expr::Literal(Literal::Integer(3)),
    ];

    let result = macro_system
        .expand_macro_call("vec", &args, NodeId::new())
        .unwrap();

    match &result[0] {
        Statement::ExprStatement(Expr::BlockExpression { statements, .. }) => {
            // Should have Vec creation + 3 push calls
            assert_eq!(statements.len(), 4);

            // First statement should be Vec creation
            match &statements[0] {
                Statement::VariableDeclaration { pattern, .. } => match pattern {
                    Pattern::Identifier(name) => {
                        assert_eq!(name, "__vec");
                    }
                    _ => panic!("Expected identifier pattern for Vec"),
                },
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
        }
        _ => panic!("Expected block expression"),
    }
}

#[test]
fn test_format_macro() {
    let mut macro_system = MacroSystem::new();
    let args = vec![
        Expr::Literal(Literal::String("Hello {}!".to_string())),
        Expr::Literal(Literal::String("World".to_string())),
    ];

    let result = macro_system
        .expand_macro_call("format", &args, NodeId::new())
        .unwrap();

    assert_eq!(result.len(), 1);

    match &result[0] {
        Statement::ExprStatement(Expr::BinaryOp { .. }) => {
            // The format macro expands to concatenation expressions
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

    match &result[0] {
        Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
            assert_eq!(method, "println");
        }
        _ => panic!("Expected println method call"),
    }

    // Test println with format string and arguments
    let args = vec![
        Expr::Literal(Literal::String("Hello {}!".to_string())),
        Expr::Literal(Literal::String("World".to_string())),
    ];

    let result = macro_system
        .expand_macro_call("println", &args, NodeId::new())
        .unwrap();

    match &result[0] {
        Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
            assert_eq!(method, "println");
        }
        _ => panic!("Expected println method call for formatted output"),
    }
}

#[test]
fn test_debug_macro() {
    let mut macro_system = MacroSystem::new();
    let args = vec![Expr::Identifier("x".to_string())];

    let result = macro_system
        .expand_macro_call("debug", &args, NodeId::new())
        .unwrap();

    assert_eq!(result.len(), 1);

    match &result[0] {
        Statement::ExprStatement(Expr::BlockExpression {
            statements,
            final_expr,
        }) => {
            assert_eq!(statements.len(), 2);
            assert!(final_expr.is_some());

            // First statement should be variable declaration
            match &statements[0] {
                Statement::VariableDeclaration { pattern, .. } => match pattern {
                    Pattern::Identifier(name) => {
                        assert_eq!(name, "debug_val");
                    }
                    _ => panic!("Expected identifier pattern"),
                },
                _ => panic!("Expected variable declaration"),
            }

            // Second statement should be println call
            match &statements[1] {
                Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
                    assert_eq!(method, "println");
                }
                _ => panic!("Expected println method call"),
            }
        }
        _ => panic!("Expected block expression"),
    }
}

#[test]
fn test_assert_macro() {
    let mut macro_system = MacroSystem::new();
    let args = vec![Expr::Literal(Literal::Boolean(true))];

    let result = macro_system
        .expand_macro_call("assert", &args, NodeId::new())
        .unwrap();

    assert_eq!(result.len(), 1);

    match &result[0] {
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            // Condition should be the negated assertion
            match condition {
                Expr::UnaryOp { .. } => {
                    // Good, it's a negation of the assertion
                }
                _ => panic!("Expected unary negation of assertion"),
            }

            // Then branch should contain println call
            match &then_branch[0] {
                Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
                    assert_eq!(method, "println");
                }
                _ => panic!("Expected println method call"),
            }

            // Should have no else branch
            assert!(else_branch.is_none());
        }
        _ => panic!("Expected if statement"),
    }
}

#[test]
fn test_todo_macro() {
    let mut macro_system = MacroSystem::new();
    let args = vec![Expr::Literal(Literal::String(
        "Not implemented".to_string(),
    ))];

    let result = macro_system
        .expand_macro_call("todo", &args, NodeId::new())
        .unwrap();

    assert_eq!(result.len(), 1);

    match &result[0] {
        Statement::ExprStatement(Expr::MethodCall { method, .. }) => {
            assert_eq!(method, "println");
        }
        _ => panic!("Expected println method call"),
    }
}

#[test]
fn test_macro_not_found() {
    let mut macro_system = MacroSystem::new();
    let result = macro_system.expand_macro_call("unknown_macro", &[], NodeId::new());

    assert!(result.is_err());
}

#[test]
fn test_argument_count_mismatch() {
    let mut macro_system = MacroSystem::new();

    // Test format macro with mismatched placeholders and arguments
    let args = vec![
        Expr::Literal(Literal::String("Hello {} {}!".to_string())), // 2 placeholders
        Expr::Literal(Literal::String("World".to_string())),        // Only 1 argument
    ];

    let result = macro_system.expand_macro_call("format", &args, NodeId::new());
    assert!(result.is_err());
}

#[test]
fn test_preprocess_statement() {
    let mut macro_system = MacroSystem::new();

    let stmt = Statement::ExprStatement(Expr::MacroExpr {
        name: "vec".to_string(),
        arguments: vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
        ],
    });

    let result = macro_system.preprocess_statement(stmt).unwrap();

    match &result[0] {
        Statement::ExprStatement(Expr::BlockExpression { statements, .. }) => {
            // Should have Vec creation + 2 push calls
            assert_eq!(statements.len(), 3);
        }
        _ => panic!("Expected block expression"),
    }
}

#[test]
fn test_preprocess_expr() {
    let mut macro_system = MacroSystem::new();

    let expr = Expr::MacroExpr {
        name: "vec".to_string(),
        arguments: vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
        ],
    };

    let result = macro_system.preprocess_expr(expr).unwrap();

    match result {
        Expr::BlockExpression { statements, .. } => {
            // Should have Vec creation + 2 push calls
            assert_eq!(statements.len(), 3);
        }
        _ => panic!("Expected block expression"),
    }

    // Test non-macro expression (should pass through unchanged)
    let expr = Expr::Literal(Literal::Integer(42));
    let result = macro_system.preprocess_expr(expr.clone()).unwrap();

    match result {
        Expr::Literal(Literal::Integer(42)) => {
            // Pass through unchanged
        }
        _ => panic!("Expected unchanged literal"),
    }
}

#[test]
fn test_nested_macro_calls() {
    let mut macro_system = MacroSystem::new();

    // Test vec macro containing format macro
    let inner_format = Expr::MacroExpr {
        name: "format".to_string(),
        arguments: vec![
            Expr::Literal(Literal::String("Item {}".to_string())),
            Expr::Literal(Literal::Integer(1)),
        ],
    };

    let stmt = Statement::VariableDeclaration {
        pattern: Pattern::Identifier("items".to_string()),
        value: Box::new(Expr::MacroExpr {
            name: "vec".to_string(),
            arguments: vec![
                inner_format,
                Expr::Literal(Literal::String("second".to_string())),
            ],
        }),
        type_annotation: None,
        var_kind: VarKind::Let,
        is_public: false,
    };

    let result = macro_system.preprocess_statement(stmt).unwrap();

    assert_eq!(result.len(), 1);
    match &result[0] {
        Statement::VariableDeclaration { value, .. } => {
            match value.as_ref() {
                Expr::ArrayLiteral(elements) => {
                    assert_eq!(elements.len(), 2);
                }
                Expr::BlockExpression { .. } => {
                    // The inner macro might also expand to a block
                    // This is acceptable behavior
                }
                _ => {
                    // For debugging, let's see what we actually got
                    println!("Actual value: {:?}", value);
                    panic!("Expected array literal or block expression");
                }
            }
        }
        _ => panic!("Expected variable declaration"),
    }
}

#[test]
fn test_macro_system_info() {
    let macro_system = MacroSystem::new();

    // Test that builtin macros are registered
    assert!(macro_system.is_macro_defined("vec"));
    assert!(macro_system.is_macro_defined("format"));
    assert!(macro_system.is_macro_defined("println"));
    assert!(macro_system.is_macro_defined("debug"));
    assert!(macro_system.is_macro_defined("assert"));
    assert!(macro_system.is_macro_defined("todo"));

    // Test that non-existent macro is not defined
    assert!(!macro_system.is_macro_defined("nonexistent"));

    // Test macro listing
    let macros = macro_system.list_macros();
    assert!(macros.len() >= 6); // At least the 6 builtin macros
}

#[test]
fn test_macro_hygiene() {
    let mut macro_system = MacroSystem::new();

    // Test that macro expansion doesn't interfere with existing bindings
    let stmt = Statement::ExprStatement(Expr::MacroExpr {
        name: "debug".to_string(),
        arguments: vec![Expr::Identifier("debug_val".to_string())],
    });

    let result = macro_system.preprocess_statement(stmt).unwrap();

    // The macro should still work even if there's already a variable named debug_val
    assert_eq!(result.len(), 1);
    match &result[0] {
        Statement::ExprStatement(Expr::BlockExpression { .. }) => {
            // Expected behavior
        }
        _ => panic!("Expected block expression from debug macro"),
    }
}
