//! Migration Example: Upgrading from Simple to Advanced Macros
//!
//! This example shows how to migrate existing macro code from the simple
//! format to the new advanced macro system while maintaining backward compatibility.

use veld_common::ast::*;
use veld_common::source::NodeId;
use veld_expander::{MacroDefinition, MacroStability, MacroSystem};

fn main() {
    println!("Veld Macro Migration Example");
    println!("============================\n");

    // Show how old code continues to work
    demonstrate_backward_compatibility();

    // Show how to gradually upgrade to new features
    demonstrate_gradual_migration();

    // Show advanced features
    demonstrate_advanced_features();

    println!("\nMigration example completed successfully!");
}

/// Old code continues to work without any changes
fn demonstrate_backward_compatibility() {
    println!("1. Backward Compatibility");
    println!("--------------------------");

    // This is how macros were defined before (still works!)
    let old_macro = MacroDefinition::simple(
        "old_style_macro".to_string(),
        vec!["x".to_string(), "y".to_string()],
        vec![
            Statement::VariableDeclaration {
                name: "result".to_string(),
                var_kind: VarKind::Let,
                type_annotation: None,
                value: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Identifier("x".to_string())),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expr::Identifier("y".to_string())),
                }),
                is_public: false,
            },
            Statement::Return(Some(Expr::Identifier("result".to_string()))),
        ],
        NodeId::new(),
    );

    println!("  ✓ Old macro created successfully");
    println!("  ✓ Is simple macro: {}", old_macro.is_simple());
    println!("  ✓ Parameters: {:?}", old_macro.effective_parameters());
    println!(
        "  ✓ Body has {} statements",
        old_macro.effective_body().len()
    );
}

/// Show how to gradually add new features to existing macros
fn demonstrate_gradual_migration() {
    println!("\n2. Gradual Migration");
    println!("--------------------");

    // Step 1: Start with existing macro
    let mut macro_def = MacroDefinition::simple(
        "enhanced_macro".to_string(),
        vec!["value".to_string()],
        vec![Statement::ExprStatement(Expr::Identifier(
            "value".to_string(),
        ))],
        NodeId::new(),
    );

    println!("  Step 1: Created simple macro");

    // Step 2: Add metadata without changing functionality
    macro_def = macro_def
        .with_description("A macro that was gradually enhanced".to_string())
        .with_example("enhanced_macro!(42)".to_string())
        .with_stability(MacroStability::Stable)
        .with_attribute("version".to_string(), "1.0".to_string());

    println!("  Step 2: Added metadata");
    println!("    Description: {:?}", macro_def.metadata.description);
    println!("    Examples: {:?}", macro_def.metadata.examples);
    println!("    Stability: {:?}", macro_def.metadata.stability);

    // Step 3: The macro still works the same way
    println!("  Step 3: Functionality unchanged");
    println!("    Still simple: {}", macro_def.is_simple());
    println!("    Parameters: {:?}", macro_def.effective_parameters());
}

/// Show new advanced features available after migration
fn demonstrate_advanced_features() {
    println!("\n3. Advanced Features");
    println!("--------------------");

    // Declarative macro with pattern matching
    let declarative_macro = MacroDefinition::declarative(
        "advanced_vec".to_string(),
        vec![
            veld_common::ast::MacroPattern("()".to_string()),
            veld_common::ast::MacroPattern("($elem:expr)".to_string()),
            veld_common::ast::MacroPattern("($($elem:expr),+ $(,)?)".to_string()),
        ],
        vec![
            veld_common::ast::MacroTemplate {
                pattern: veld_common::ast::MacroPattern("()".to_string()),
                expansion: veld_common::ast::MacroExpansion(vec![Statement::ExprStatement(
                    Expr::FunctionCall {
                        name: "Vec.new".to_string(),
                        arguments: vec![],
                    },
                )]),
            },
            veld_common::ast::MacroTemplate {
                pattern: veld_common::ast::MacroPattern("($elem:expr)".to_string()),
                expansion: veld_common::ast::MacroExpansion(vec![Statement::ExprStatement(
                    Expr::ArrayLiteral(vec![Expr::MacroVar("elem".to_string())]),
                )]),
            },
            veld_common::ast::MacroTemplate {
                pattern: veld_common::ast::MacroPattern("($($elem:expr),+ $(,)?)".to_string()),
                expansion: veld_common::ast::MacroExpansion(vec![Statement::ExprStatement(
                    Expr::ArrayLiteral(vec![Expr::MacroVar("elem".to_string())]),
                )]),
            },
        ],
        NodeId::new(),
    )
    .with_description("Advanced vector creation with pattern matching".to_string())
    .with_example("advanced_vec~(1, 2, 3)".to_string())
    .with_stability(MacroStability::Stable);

    println!("  ✓ Declarative macro created");
    println!("    Is declarative: {}", declarative_macro.is_declarative());
    println!("    Has patterns: true");

    // Template macro for Nim-style templates
    let template_macro = MacroDefinition::template(
        "debug_template".to_string(),
        vec!["expr".to_string()],
        vec![
            Statement::VariableDeclaration {
                name: "debug_value".to_string(),
                var_kind: VarKind::Let,
                type_annotation: None,
                value: Box::new(Expr::Identifier("expr".to_string())),
                is_public: false,
            },
            Statement::ExprStatement(Expr::FunctionCall {
                name: "println".to_string(),
                arguments: vec![
                    veld_common::ast::Argument::Positional(Expr::Literal(Literal::String(
                        "DEBUG: {}".to_string(),
                    ))),
                    veld_common::ast::Argument::Positional(Expr::Identifier(
                        "debug_value".to_string(),
                    )),
                ],
            }),
        ],
        NodeId::new(),
        false, // not typed
    )
    .with_description("Debug template for printing expressions".to_string())
    .with_example("debug_template~(my_var + 1)".to_string())
    .with_stability(MacroStability::Experimental);

    println!("  ✓ Template macro created");
    println!("    Is template: {}", template_macro.is_template());
    println!("    Is typed: false");

    // Compile-time procedure (placeholder)
    let compile_time_macro = MacroDefinition::compile_time_proc(
        "generate_getters".to_string(),
        vec!["struct_name".to_string(), "fields".to_string()],
        vec![Statement::ExprStatement(Expr::Literal(Literal::String(
            "Generated getter methods".to_string(),
        )))],
        NodeId::new(),
        Some(veld_common::types::Type::Array(Box::new(
            veld_common::types::Type::String,
        ))),
    )
    .with_description("Generate getter methods at compile time".to_string())
    .with_example("generate_getters~(Person, [name, age])".to_string())
    .with_stability(MacroStability::Unstable);

    println!("  ✓ Compile-time procedure created");
    println!(
        "    Is compile-time proc: {}",
        compile_time_macro.is_compile_time_proc()
    );
    println!("    Has return type: true");
}

/// Example showing integration with MacroSystem
fn demonstrate_macro_system_integration() {
    println!("\n4. Macro System Integration");
    println!("---------------------------");

    let mut macro_system = MacroSystem::new();

    // The system works with all types of macros
    println!("  Built-in macros available:");
    for macro_name in macro_system.list_macros() {
        println!("    - {}", macro_name);
    }

    // Test expansion of different macro types
    let args = vec![
        Expr::Literal(Literal::Integer(1)),
        Expr::Literal(Literal::Integer(2)),
        Expr::Literal(Literal::Integer(3)),
    ];

    match macro_system.expand_macro_call("vec", &args, NodeId::new()) {
        Ok(result) => println!(
            "  ✓ Vec macro expanded successfully: {} statements",
            result.len()
        ),
        Err(e) => println!("  ✗ Vec macro expansion failed: {:?}", e),
    }
}

/// Show migration strategies for different scenarios
fn migration_strategies() {
    println!("\n5. Migration Strategies");
    println!("-----------------------");

    println!("  Strategy 1: Keep existing macros as-is");
    println!("    - No changes required");
    println!("    - Add metadata gradually");
    println!("    - Upgrade when needed");

    println!("\n  Strategy 2: Upgrade to declarative macros");
    println!("    - Better pattern matching");
    println!("    - More flexible syntax");
    println!("    - Rust-like macro experience");

    println!("\n  Strategy 3: Use template macros");
    println!("    - Nim-like template system");
    println!("    - Direct AST manipulation");
    println!("    - Good for DSLs");

    println!("\n  Strategy 4: Compile-time procedures");
    println!("    - Full compile-time execution");
    println!("    - Code generation");
    println!("    - Advanced metaprogramming");
}

/// Example of a real migration scenario
fn real_world_migration_example() {
    println!("\n6. Real World Migration");
    println!("-----------------------");

    // Before: Simple logging macro
    let old_log_macro = MacroDefinition::simple(
        "log".to_string(),
        vec!["level".to_string(), "message".to_string()],
        vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "println".to_string(),
            arguments: vec![
                veld_common::ast::Argument::Positional(Expr::Identifier("level".to_string())),
                veld_common::ast::Argument::Positional(Expr::Identifier("message".to_string())),
            ],
        })],
        NodeId::new(),
    );

    println!("  Before: Simple log macro");
    println!("    Type: Simple");
    println!("    Parameters: {:?}", old_log_macro.effective_parameters());

    // After: Enhanced with metadata and kept backward compatible
    let enhanced_log_macro = MacroDefinition::simple(
        "log".to_string(),
        vec!["level".to_string(), "message".to_string()],
        vec![Statement::ExprStatement(Expr::FunctionCall {
            name: "println".to_string(),
            arguments: vec![
                veld_common::ast::Argument::Positional(Expr::Identifier("level".to_string())),
                veld_common::ast::Argument::Positional(Expr::Identifier("message".to_string())),
            ],
        })],
        NodeId::new(),
    )
    .with_description("Logging macro with level and message".to_string())
    .with_example("log~(\"INFO\", \"Application started\")".to_string())
    .with_stability(MacroStability::Stable)
    .with_attribute("category".to_string(), "logging".to_string());

    println!("\n  After: Enhanced log macro");
    println!("    Type: Still simple (backward compatible)");
    println!(
        "    Description: {:?}",
        enhanced_log_macro.metadata.description
    );
    println!(
        "    Example: {:?}",
        enhanced_log_macro.metadata.examples.get(0)
    );
    println!(
        "    Category: {:?}",
        enhanced_log_macro.metadata.attributes.get("category")
    );

    // Evolution: Upgrade to declarative for better patterns
    let evolved_log_macro = MacroDefinition::declarative(
        "log".to_string(),
        vec![
            veld_common::ast::MacroPattern("($level:literal, $message:expr)".to_string()),
            veld_common::ast::MacroPattern("($level:expr, $message:expr)".to_string()),
        ],
        vec![veld_common::ast::MacroTemplate {
            pattern: veld_common::ast::MacroPattern("($level:literal, $message:expr)".to_string()),
            expansion: veld_common::ast::MacroExpansion(vec![Statement::ExprStatement(
                Expr::FunctionCall {
                    name: "log_with_level".to_string(),
                    arguments: vec![],
                },
            )]),
        }],
        NodeId::new(),
    )
    .with_description("Advanced logging macro with pattern matching".to_string())
    .with_example("log~(\"INFO\", format~(\"User {} logged in\", username))".to_string())
    .with_stability(MacroStability::Stable);

    println!("\n  Evolution: Declarative log macro");
    println!("    Type: Declarative (advanced patterns)");
    println!("    Supports: Literal levels and expression levels");
    println!("    Backward compatible: No (but provides better features)");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_migration_compatibility() {
        // Test that old-style macros work
        let old_macro = MacroDefinition::simple(
            "test".to_string(),
            vec!["x".to_string()],
            vec![],
            NodeId::new(),
        );

        assert!(old_macro.is_simple());
        assert!(!old_macro.is_declarative());
        assert!(!old_macro.is_template());
        assert!(!old_macro.is_compile_time_proc());
    }

    #[test]
    fn test_gradual_enhancement() {
        // Test that we can add metadata without breaking functionality
        let enhanced = MacroDefinition::simple(
            "test".to_string(),
            vec!["x".to_string()],
            vec![],
            NodeId::new(),
        )
        .with_description("Test macro".to_string())
        .with_stability(MacroStability::Experimental);

        assert!(enhanced.is_simple()); // Still a simple macro
        assert_eq!(
            enhanced.metadata.description,
            Some("Test macro".to_string())
        );
        assert_eq!(enhanced.metadata.stability, MacroStability::Experimental);
    }

    #[test]
    fn test_new_features() {
        // Test that new features work as expected
        let declarative =
            MacroDefinition::declarative("test".to_string(), vec![], vec![], NodeId::new());

        assert!(declarative.is_declarative());
        assert!(!declarative.is_simple());

        let template = MacroDefinition::template(
            "test".to_string(),
            vec!["x".to_string()],
            vec![],
            NodeId::new(),
            false,
        );

        assert!(template.is_template());
        assert!(!template.is_simple());
    }
}
