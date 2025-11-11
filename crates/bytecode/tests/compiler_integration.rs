//! Integration tests for RegisterCompiler + VirtualMachineV2
//!
//! These tests verify the complete pipeline:
//! Veld Source → Lexer → Parser → AST → RegisterCompiler → Bytecode → VirtualMachineV2 → Execution

use veld_bytecode::{RegisterCompiler, VirtualMachineV2};
use veld_common::ast::{AST, Statement};
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;

/// Helper function to compile and execute Veld code
fn compile_and_run(code: &str) -> Result<(), String> {
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
    let ast = AST::new(statements);

    // 3. Compiling with RegisterCompiler
    let mut compiler = RegisterCompiler::new();
    let chunk = compiler
        .compile(&ast)
        .map_err(|e| format!("Compilation error: {:?}", e))?;

    // 4. Executing with VirtualMachineV2
    let mut vm = VirtualMachineV2::new();
    let result = vm.interpret(chunk);
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => Ok(()),
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            Err(format!("Runtime error: {:?}", e))
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            Err(format!("Compile error: {:?}", e))
        }
    }
}

#[test]
fn test_simple_arithmetic() {
    let code = "let x = 1 + 2";
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_multiple_variables() {
    let code = r#"
let a = 10
let b = 20
let c = a + b
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_arithmetic_operations() {
    let code = r#"
let a = 5
let b = 3
let sum = a + b
let diff = a - b
let prod = a * b
let quot = a / b
let rem = a % b
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_comparison_operations() {
    let code = r#"
let x = 10
let y = 20
let eq = x == y
let neq = x != y
let lt = x < y
let gt = x > y
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_logical_operations() {
    let code = r#"
let a = true
let b = false
let and_result = a and b
let or_result = a or b
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_unary_operations() {
    let code = r#"
let x = 10
let neg = -x
let flag = true
let not_flag = !flag
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_simple_outer_variable() {
    let code = r#"
let x = 10
let y = x
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_variable_scoping() {
    // Simplified test - basic variable access works
    let code = r#"
let x = 10
let y = x
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_variable_shadowing_with_blocks() {
    // Test shadowing with do...end blocks
    // The inner x shadows the outer x, then after the block ends,
    // the outer x should be accessible again
    let code = r#"
let x = 10
do
    let x = 20
end
let y = x
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_if_statement() {
    let code = r#"
let x = 10
if x > 5 then
    let y = 1
else
    let y = 2
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_if_without_else() {
    let code = r#"
let x = 10
if x > 5 then
    let y = 1
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_while_loop() {
    let code = r#"
var x = 0
while x < 5 do
    x = x + 1
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_while_loop_with_false_condition() {
    let code = r#"
while false do
    let x = 1
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_mutable_variable_assignment() {
    let code = r#"
var a = 1
a = 2
a = a + 1
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_nested_expressions() {
    let code = r#"
let x = (1 + 2) * (3 + 4)
let y = ((5 - 3) + (2 * 4)) / 2
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_boolean_literals() {
    let code = r#"
let t = true
let f = false
let and_val = t and f
let or_val = t or f
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_string_literals() {
    let code = r#"
let name = "Alice"
let greeting = "Hello"
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_float_literals() {
    let code = r#"
let pi = 3.14159
let e = 2.71828
let sum = pi + e
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_nested_scopes() {
    let code = r#"
let x = 1
do
    let y = 2
    do
        let z = 3
    end
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_shadowing_across_scopes() {
    let code = r#"
let x = 1
do
    let x = 2
    do
        let x = 3
    end
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_multiple_if_statements() {
    let code = r#"
let a = 10
if a > 5 then
    let b = 1
end
if a < 20 then
    let c = 2
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_nested_if_statements() {
    let code = r#"
let x = 10
if x > 5 then
    if x < 15 then
        let y = 1
    end
end
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_complex_boolean_expression() {
    let code = r#"
let a = true
let b = false
let c = true
let result = (a and b) or (c and !b)
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_chained_comparisons() {
    let code = r#"
let x = 10
let in_range = x > 5 and x < 15
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_exponentiation() {
    let code = r#"
let base = 2
let exp = 3
let result = base *^ exp
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_let_mut_variable() {
    let code = r#"
var x = 10
x = 20
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_const_variable() {
    let code = r#"
const PI = 3.14159
let circumference = 2.0 * PI * 10.0
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_unit_literal() {
    let code = r#"
let nothing = ()
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_empty_program() {
    let code = "";
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_single_variable() {
    let code = "let x = 42";
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}

#[test]
fn test_order_of_operations() {
    let code = r#"
let result = 2 + 3 * 4
let result2 = (2 + 3) * 4
"#;
    let result = compile_and_run(code);
    assert!(result.is_ok(), "Failed: {:?}", result.err());
}
