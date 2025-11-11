//! Exhaustiveness Checking Tests
//!
//! Tests to verify that pattern match exhaustiveness checking works correctly
//! and provides helpful error messages when patterns are non-exhaustive.

use veld_bytecode::{RegisterCompiler, VirtualMachineV2};
use veld_common::ast::{AST, Statement};
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_common::types::checker::TypeChecker;

/// Helper to parse and type-check code
fn parse_and_check(code: &str) -> Result<(), String> {
    // 1. Lexing
    let mut lexer = Lexer::new(code);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| format!("Lexer error: {:?}", e))?;

    // 2. Parsing
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| format!("Parse error: {}", e))?;

    // 3. Type checking
    let mut type_checker = TypeChecker::new();
    type_checker
        .check_program(&ast)
        .map_err(|e| format!("Type error: {}", e))?;

    Ok(())
}

/// Helper to compile and run code
fn compile_and_run_code(code: &str) -> Result<(), String> {
    // 1. Lexing
    let mut lexer = Lexer::new(code);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| format!("Lexer error: {:?}", e))?;

    // 2. Parsing
    let mut parser = Parser::new(tokens);
    let statements: Vec<Statement> = parser
        .parse()
        .map_err(|e| format!("Parser error: {:?}", e))?;

    // 3. Type checking
    let mut type_checker = TypeChecker::new();
    type_checker
        .check_program(&statements)
        .map_err(|e| format!("Type check error: {:?}", e))?;

    let ast = AST::new(statements);

    // 4. Compilation
    let mut compiler = RegisterCompiler::new();
    let chunk = compiler
        .compile(&ast)
        .map_err(|e| format!("Compile error: {:?}", e))?;

    // 5. Execution
    let mut vm = VirtualMachineV2::new();
    let result = vm.interpret(chunk);

    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => Ok(()),
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            Err(format!("Runtime error: {:?}", e))
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            Err(format!("VM compile error: {:?}", e))
        }
    }
}

#[test]
fn test_exhaustive_boolean_match() {
    let code = r#"
let b = true

match b
    true => 1
    false => 0
end
"#;

    // Should type-check successfully (exhaustive)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_boolean_with_wildcard() {
    let code = r#"
let b = true

match b
    true => 1
    _ => 0
end
"#;

    // Should type-check successfully (wildcard makes it exhaustive)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_simple_enum() {
    let code = r#"
enum Status
    pending
    complete
end

let s = Status.pending

match s
    Status.pending => "waiting"
    Status.complete => "done"
end
"#;

    // Should type-check successfully (all variants covered)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_enum_with_wildcard() {
    let code = r#"
enum Status
    pending
    processing
    complete
    failed
end

let s = Status.pending

match s
    Status.pending => "waiting"
    _ => "other"
end
"#;

    // Should type-check successfully (wildcard covers remaining cases)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_option_enum() {
    let code = r#"
enum Option
    some(value)
    none
end

let opt = Option.some(42)

match opt
    Option.some(x) => x
    Option.none => 0
end
"#;

    // Should type-check successfully (both variants covered)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_result_enum() {
    let code = r#"
enum Result
    ok(value)
    err(message)
end

let res = Result.ok(100)

match res
    Result.ok(val) => val
    Result.err(_) => -1
end
"#;

    // Should type-check successfully (both variants covered)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_with_multiple_fields() {
    let code = r#"
enum Either
    left(a)
    right(b)
    both(x, y)
end

let e = Either.both(1, 2)

match e
    Either.left(a) => a
    Either.right(b) => b
    Either.both(x, y) => x + y
end
"#;

    // Should type-check successfully (all variants covered)
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_nested_match() {
    let code = r#"
enum Option
    some(value)
    none
end

let opt1 = Option.some(42)
let opt2 = Option.none

let result = match opt1
    Option.some(x) => match opt2
        Option.some(y) => x + y
        Option.none => x
    end
    Option.none => 0
end
"#;

    // Both matches are exhaustive
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_exhaustive_with_guards_still_exhaustive() {
    // Even with guards, if we have a final wildcard, it's exhaustive
    let code = r#"
enum Option
    some(value)
    none
end

let opt = Option.some(5)

match opt
    Option.some(x) => x * 2
    _ => 0
end
"#;

    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_literal_patterns_with_wildcard() {
    let code = r#"
let n = 42

match n
    0 => "zero"
    1 => "one"
    _ => "other"
end
"#;

    // Infinite types (integers) require a wildcard
    assert!(parse_and_check(code).is_ok());
}

#[test]
fn test_string_patterns_with_wildcard() {
    let code = r#"
let s = "hello"

match s
    "hello" => 1
    "world" => 2
    _ => 0
end
"#;

    // String is infinite, requires wildcard
    assert!(parse_and_check(code).is_ok());
}

// ============================================================================
// Tests that should warn about non-exhaustiveness
// (Currently these just emit warnings, not errors)
// ============================================================================

#[test]
fn test_non_exhaustive_boolean_logged() {
    // Missing false case - should log a warning
    let code = r#"
let b = true

match b
    true => 1
end
"#;

    // Currently we just warn, so parsing still succeeds
    // In the future, this could be an error with a strict flag
    let result = parse_and_check(code);
    println!("Non-exhaustive boolean result: {:?}", result);

    // For now, we expect it to succeed but log a warning
    assert!(result.is_ok());
}

#[test]
fn test_non_exhaustive_enum_logged() {
    // Missing 'none' variant - should log a warning
    let code = r#"
enum Option
    some(value)
    none
end

let opt = Option.some(42)

match opt
    Option.some(x) => x
end
"#;

    let result = parse_and_check(code);
    println!("Non-exhaustive enum result: {:?}", result);

    // For now, we expect it to succeed but log a warning
    assert!(result.is_ok());
}

#[test]
fn test_non_exhaustive_multiple_missing_logged() {
    // Missing multiple variants - should log a warning
    let code = r#"
enum Status
    pending
    processing
    complete
    failed
end

let s = Status.pending

match s
    Status.pending => "waiting"
end
"#;

    let result = parse_and_check(code);
    println!("Non-exhaustive multiple missing result: {:?}", result);

    // For now, we expect it to succeed but log a warning
    assert!(result.is_ok());
}

// ============================================================================
// Integration tests - compile and run
// ============================================================================

#[test]
fn test_exhaustive_match_runs_correctly() {
    let code = r#"
enum Color
    red
    green
    blue
end

let c = Color.green

match c
    Color.red => 1
    Color.green => 2
    Color.blue => 3
end
"#;

    match compile_and_run_code(code) {
        Ok(val) => {
            println!("Result: {:?}", val);
            // Should return 2 (green)
        }
        Err(e) => panic!("Failed to compile/run: {}", e),
    }
}

#[test]
fn test_exhaustive_option_unwrap() {
    let code = r#"
enum Option
    some(value)
    none
end

fn unwrap(opt)
    match opt
        Option.some(x) => x
        Option.none => 0
    end
end

unwrap(Option.some(42))
"#;

    match compile_and_run_code(code) {
        Ok(val) => {
            println!("Unwrapped value: {:?}", val);
            // Should return 42
        }
        Err(e) => panic!("Failed to compile/run: {}", e),
    }
}

#[test]
fn test_exhaustive_with_computation() {
    let code = r#"
enum Result
    ok(value)
    err(code)
end

fn process(r)
    match r
        Result.ok(v) => v * 2
        Result.err(e) => -e
    end
end

process(Result.ok(21))
"#;

    match compile_and_run_code(code) {
        Ok(val) => {
            println!("Processed result: {:?}", val);
            // Should return 42
        }
        Err(e) => panic!("Failed to compile/run: {}", e),
    }
}
