//! Tests for constant expression evaluation in the type checker
//!
//! These tests verify that const binary operations work correctly,
//! including arithmetic, comparison, and logical operations.

use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_common::types::checker::TypeChecker;

/// Helper to parse and type-check code
fn parse_and_check(code: &str) -> Result<(), String> {
    let mut lexer = Lexer::new(code);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| format!("Lexer error: {:?}", e))?;

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| format!("Parse error: {}", e))?;

    let mut type_checker = TypeChecker::new();
    type_checker
        .check_program(&ast)
        .map_err(|e| format!("Type error: {}", e))?;

    Ok(())
}

#[test]
fn test_const_addition() {
    let code = r#"
const X = 10 + 20
"#;
    let result = parse_and_check(code);
    if let Err(e) = &result {
        println!("Error: {}", e);
    }
    assert!(result.is_ok());
}

#[test]
fn test_const_subtraction() {
    let code = r#"
const X = 100 - 50
"#;
    let result = parse_and_check(code);
    if let Err(e) = &result {
        println!("Error: {}", e);
    }
    assert!(result.is_ok());
}

#[test]
fn test_const_multiplication() {
    let code = r#"
const X = 6 * 7
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_division() {
    let code = r#"
const X = 100 / 10
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_division_by_zero_error() {
    let code = r#"
const X = 100 / 0
"#;
    let result = parse_and_check(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));
}

#[test]
fn test_const_modulo() {
    let code = r#"
const X = 17 % 5
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_modulo_by_zero_error() {
    let code = r#"
const X = 17 % 0
"#;
    let result = parse_and_check(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Modulo by zero"));
}

#[test]
fn test_const_comparison_equal() {
    let code = r#"
const X = 10 == 10
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_comparison_not_equal() {
    let code = r#"
const X = 10 != 20
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_comparison_less_than() {
    let code = r#"
const X = 5 < 10
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_comparison_less_than_or_equal() {
    let code = r#"
const X = 10 <= 10
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_comparison_greater_than() {
    let code = r#"
const X = 15 > 10
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_comparison_greater_than_or_equal() {
    let code = r#"
const X = 20 >= 15
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_logical_and() {
    let code = r#"
const X = true and false
"#;
    let result = parse_and_check(code);
    if let Err(e) = &result {
        println!("Error: {}", e);
    }
    assert!(result.is_ok());
}

#[test]
fn test_const_logical_or() {
    let code = r#"
const X = true or false
"#;
    let result = parse_and_check(code);
    if let Err(e) = &result {
        println!("Error: {}", e);
    }
    assert!(result.is_ok());
}

#[test]
fn test_const_float_operations() {
    let code = r#"
const X = 3.14 + 2.86
const Y = 10.0 * 2.5
const Z = 100.0 / 4.0
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_nested_operations() {
    let code = r#"
const X = (10 + 5) * 2
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_multiple_operations() {
    let code = r#"
const A = 10
const B = 20
const C = A + B
"#;
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_const_boolean_operations() {
    let code = r#"
const TRUE_VAL = true
const FALSE_VAL = false
const AND_RESULT = TRUE_VAL and FALSE_VAL
const OR_RESULT = TRUE_VAL or FALSE_VAL
const EQUAL_RESULT = TRUE_VAL == FALSE_VAL
"#;
    let result = parse_and_check(code);
    if let Err(e) = &result {
        println!("Error: {}", e);
    }
    assert!(result.is_ok());
}

#[test]
fn test_const_complex_expression() {
    let code = r#"
const X = ((10 + 20) * 3) / 2
"#;
    assert!(parse_and_check(code).is_ok());
}
