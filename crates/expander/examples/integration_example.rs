//! Integration example showing how to use the Veld macro system
//! This example demonstrates parsing Veld code, expanding macros, and processing the result

use veld_core::ast::*;
use veld_core::common::source::NodeId;
use veld_expander::MacroSystem;

fn main() {
    println!("Veld Macro System Integration Example");
    println!("=====================================\n");

    // Create a new macro system with built-in macros
    let mut macro_system = MacroSystem::new();

    // Example 1: Basic macro expansion
    println!("1. Basic vec~ macro expansion:");
    demonstrate_vec_macro(&mut macro_system);

    // Example 2: Format macro
    println!("\n2. Format macro expansion:");
    demonstrate_format_macro(&mut macro_system);

    // Example 3: Debug macro
    println!("\n3. Debug macro expansion:");
    demonstrate_debug_macro(&mut macro_system);

    // Example 4: Nested macro calls
    println!("\n4. Nested macro expansion:");
    demonstrate_nested_macros(&mut macro_system);

    // Example 5: Statement preprocessing
    println!("\n5. Statement preprocessing:");
    demonstrate_statement_preprocessing(&mut macro_system);

    // Example 6: Expression preprocessing
    println!("\n6. Expression preprocessing:");
    demonstrate_expression_preprocessing(&mut macro_system);

    // Example 7: Custom macro registration
    println!("\n7. Custom macro registration:");
    demonstrate_custom_macro(&mut macro_system);

    // Example 8: Error handling
    println!("\n8. Error handling:");
    demonstrate_error_handling(&mut macro_system);

    println!("\nIntegration example completed successfully!");
}

fn demonstrate_vec_macro(macro_system: &mut MacroSystem) {
    // Empty vec
    let result = macro_system
        .expand_macro_call("vec", &[], NodeId::new())
        .unwrap();
    println!("  vec~() expands to: {:?}", result);

    // Vec with elements
    let args = vec![
        Expr::Literal(Literal::Integer(1)),
        Expr::Literal(Literal::Integer(2)),
        Expr::Literal(Literal::Integer(3)),
    ];
    let result = macro_system
        .expand_macro_call("vec", &args, NodeId::new())
        .unwrap();
    println!("  vec~(1, 2, 3) expands to: {:?}", result);
}

fn demonstrate_format_macro(macro_system: &mut MacroSystem) {
    let args = vec![
        Expr::Literal(Literal::String("Hello {}!".to_string())),
        Expr::Literal(Literal::String("World".to_string())),
    ];
    let result = macro_system
        .expand_macro_call("format", &args, NodeId::new())
        .unwrap();
    println!(
        "  format!(\"Hello {{}}!\", \"World\") expands to: {:?}",
        result
    );
}

fn demonstrate_debug_macro(macro_system: &mut MacroSystem) {
    let args = vec![Expr::BinaryOp {
        left: Box::new(Expr::Identifier("x".to_string())),
        operator: BinaryOperator::Add,
        right: Box::new(Expr::Literal(Literal::Integer(42))),
    }];
    let result = macro_system
        .expand_macro_call("debug", &args, NodeId::new())
        .unwrap();
    println!("  debug~(x + 42) expands to: {:?}", result);
}

fn demonstrate_nested_macros(macro_system: &mut MacroSystem) {
    // Create nested expression: debug!(vec!(1, 2, 3))
    let inner_expr = Expr::MacroExpr {
        name: "vec".to_string(),
        arguments: vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ],
    };

    let outer_expr = Expr::MacroExpr {
        name: "debug".to_string(),
        arguments: vec![inner_expr],
    };

    let result = macro_system.preprocess_expr(outer_expr).unwrap();
    println!("  debug~(vec~(1, 2, 3)) expands to: {:?}", result);
}

fn demonstrate_statement_preprocessing(macro_system: &mut MacroSystem) {
    // Create a macro invocation statement
    let stmt = Statement::MacroInvocation {
        name: "vec".to_string(),
        arguments: vec![
            Expr::Literal(Literal::Integer(10)),
            Expr::Literal(Literal::Integer(20)),
            Expr::Literal(Literal::Integer(30)),
        ],
    };

    let result = macro_system.preprocess_statement(stmt).unwrap();
    println!(
        "  MacroInvocation(vec, [10, 20, 30]) expands to: {:?}",
        result
    );

    // Test a variable declaration with macro in value
    let stmt = Statement::VariableDeclaration {
        name: "numbers".to_string(),
        var_kind: VarKind::Let,
        type_annotation: None,
        value: Box::new(Expr::MacroExpr {
            name: "vec".to_string(),
            arguments: vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2)),
            ],
        }),
        is_public: false,
    };

    let result = macro_system.preprocess_statement(stmt).unwrap();
    println!("  Variable with macro value expands to: {:?}", result);
}

fn demonstrate_expression_preprocessing(macro_system: &mut MacroSystem) {
    // Function call that's actually a macro
    let expr = Expr::FunctionCall {
        name: "vec".to_string(),
        arguments: vec![
            Argument::Positional(Expr::Literal(Literal::Integer(5))),
            Argument::Positional(Expr::Literal(Literal::Integer(10))),
        ],
    };

    let result = macro_system.preprocess_expr(expr).unwrap();
    println!("  FunctionCall(vec, [5, 10]) expands to: {:?}", result);

    // Regular function call (should pass through)
    let expr = Expr::FunctionCall {
        name: "regular_function".to_string(),
        arguments: vec![Argument::Positional(Expr::Literal(Literal::Integer(42)))],
    };

    let result = macro_system.preprocess_expr(expr).unwrap();
    println!("  Regular function call: {:?}", result);
}

fn demonstrate_custom_macro(macro_system: &mut MacroSystem) {
    // Register a simple custom macro
    let macro_source = r#"
macro~ unless
    ($condition:expr => $body:block) => do
        if !($condition) then
            $body
        end
    end
end
"#;

    match macro_system.parse_and_register_macro(macro_source, NodeId::new()) {
        Ok(name) => println!("  Successfully registered custom macro: {}", name),
        Err(e) => println!("  Failed to register custom macro: {:?}", e),
    }

    // List all available macros
    let macros = macro_system.list_macros();
    println!("  Available macros: {:?}", macros);
}

fn demonstrate_error_handling(macro_system: &mut MacroSystem) {
    // Try to expand a non-existent macro
    match macro_system.expand_macro_call("nonexistent", &[], NodeId::new()) {
        Ok(_) => println!("  Unexpected success"),
        Err(e) => println!("  Expected error for nonexistent macro: {:?}", e),
    }

    // Try to call format! without arguments
    match macro_system.expand_macro_call("format", &[], NodeId::new()) {
        Ok(_) => println!("  Unexpected success"),
        Err(e) => println!("  Expected error for format! without args: {:?}", e),
    }

    // Try to call debug! with wrong number of arguments
    let args = vec![
        Expr::Literal(Literal::Integer(1)),
        Expr::Literal(Literal::Integer(2)),
    ];
    match macro_system.expand_macro_call("debug", &args, NodeId::new()) {
        Ok(_) => println!("  Unexpected success"),
        Err(e) => println!("  Expected error for debug! with wrong args: {:?}", e),
    }
}

// Helper function to create a sample AST for demonstration
fn create_sample_ast() -> Vec<Statement> {
    vec![
        // let numbers = vec!(1, 2, 3, 4, 5)
        Statement::VariableDeclaration {
            name: "numbers".to_string(),
            var_kind: VarKind::Let,
            type_annotation: None,
            value: Box::new(Expr::MacroExpr {
                name: "vec".to_string(),
                arguments: vec![
                    Expr::Literal(Literal::Integer(1)),
                    Expr::Literal(Literal::Integer(2)),
                    Expr::Literal(Literal::Integer(3)),
                    Expr::Literal(Literal::Integer(4)),
                    Expr::Literal(Literal::Integer(5)),
                ],
            }),
            is_public: false,
        },
        // println!("Numbers: {:?}", numbers)
        Statement::ExprStatement(Expr::MacroExpr {
            name: "println".to_string(),
            arguments: vec![
                Expr::Literal(Literal::String("Numbers: {:?}".to_string())),
                Expr::Identifier("numbers".to_string()),
            ],
        }),
        // debug!(numbers.len())
        Statement::ExprStatement(Expr::MacroExpr {
            name: "debug".to_string(),
            arguments: vec![Expr::MethodCall {
                object: Box::new(Expr::Identifier("numbers".to_string())),
                method: "len".to_string(),
                arguments: vec![],
            }],
        }),
        // assert!(numbers.len() > 0, "Numbers should not be empty")
        Statement::ExprStatement(Expr::MacroExpr {
            name: "assert".to_string(),
            arguments: vec![
                Expr::BinaryOp {
                    left: Box::new(Expr::MethodCall {
                        object: Box::new(Expr::Identifier("numbers".to_string())),
                        method: "len".to_string(),
                        arguments: vec![],
                    }),
                    operator: BinaryOperator::Greater,
                    right: Box::new(Expr::Literal(Literal::Integer(0))),
                },
                Expr::Literal(Literal::String("Numbers should not be empty".to_string())),
            ],
        }),
    ]
}

// Function to demonstrate full AST preprocessing
#[allow(dead_code)]
fn demonstrate_full_ast_preprocessing() {
    println!("\nFull AST Preprocessing Example:");
    println!("===============================");

    let mut macro_system = MacroSystem::new();
    let original_ast = create_sample_ast();

    println!("Original AST:");
    for (i, stmt) in original_ast.iter().enumerate() {
        println!("  {}: {:?}", i, stmt);
    }

    println!("\nProcessed AST:");
    for (i, stmt) in original_ast.into_iter().enumerate() {
        match macro_system.preprocess_statement(stmt) {
            Ok(processed_stmts) => {
                println!("  {} -> {} statements:", i, processed_stmts.len());
                for (j, processed_stmt) in processed_stmts.iter().enumerate() {
                    println!("    {}.{}: {:?}", i, j, processed_stmt);
                }
            }
            Err(e) => {
                println!("  {}: Error: {:?}", i, e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integration_example() {
        // This test ensures the example code runs without panicking
        let mut macro_system = MacroSystem::new();

        // Test basic functionality
        let result = macro_system.expand_macro_call("vec", &[], NodeId::new());
        assert!(result.is_ok());

        let args = vec![Expr::Literal(Literal::Integer(42))];
        let result = macro_system.expand_macro_call("debug", &args, NodeId::new());
        assert!(result.is_ok());
    }

    #[test]
    fn test_macro_system_info() {
        let macro_system = MacroSystem::new();

        assert!(macro_system.is_macro_defined("vec"));
        assert!(macro_system.is_macro_defined("format"));
        assert!(macro_system.is_macro_defined("println"));
        assert!(macro_system.is_macro_defined("debug"));
        assert!(macro_system.is_macro_defined("assert"));
        assert!(macro_system.is_macro_defined("todo"));

        assert!(!macro_system.is_macro_defined("nonexistent"));

        let macros = macro_system.list_macros();
        assert!(!macros.is_empty());
        assert!(macros.contains(&"vec".to_string()));
    }

    #[test]
    fn test_preprocessing_pipeline() {
        let mut macro_system = MacroSystem::new();

        // Test expression preprocessing
        let expr = Expr::MacroExpr {
            name: "vec".to_string(),
            arguments: vec![Expr::Literal(Literal::Integer(1))],
        };

        let result = macro_system.preprocess_expr(expr);
        assert!(result.is_ok());

        // Test statement preprocessing
        let stmt = Statement::MacroInvocation {
            name: "vec".to_string(),
            arguments: vec![Expr::Literal(Literal::Integer(1))],
        };

        let result = macro_system.preprocess_statement(stmt);
        assert!(result.is_ok());
    }
}
