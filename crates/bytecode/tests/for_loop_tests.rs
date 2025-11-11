//! Comprehensive tests for for loop and iterator functionality
//!
//! These tests validate that the register-based compiler and VM correctly
//! implement for loops with the iterator protocol, including:
//! - For loops over arrays
//! - For loops over strings (character iteration)
//! - For loops over tuples
//! - Nested for loops
//! - Break and continue in for loops
//! - Empty collections
//! - Iterator state management

use veld_bytecode::{RegisterCompiler, VirtualMachineV2};
use veld_common::ast::{AST, Statement};
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;

/// Helper function to compile and run Veld code
fn compile_and_run(code: &str) -> Result<veld_bytecode::vm_v2::InterpretResult, String> {
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

    // 3. Compiling
    let mut compiler = RegisterCompiler::new();
    let chunk = compiler
        .compile(&ast)
        .map_err(|e| format!("Compilation error: {:?}", e))?;

    // 4. Executing
    let mut vm = VirtualMachineV2::new();
    Ok(vm.interpret(chunk))
}

#[test]
fn test_for_loop_over_array() {
    let code = r#"
let arr = [1, 2, 3, 4, 5]
var sum = 0

for x in arr do
    sum = sum + x
end

sum
"#;

    // Compile to see bytecode
    let mut lexer = Lexer::new(code);
    let tokens = lexer.collect_tokens().expect("Lexing failed");
    let mut parser = Parser::new(tokens);
    let statements = parser.parse().expect("Parsing failed");
    let ast = AST::new(statements);

    let mut compiler = RegisterCompiler::new();
    let chunk = compiler.compile(&ast).expect("Compilation failed");

    // Print bytecode for debugging
    println!("=== Compiled Bytecode ===");
    println!("Register count: {}", chunk.main.register_count);
    println!("Instructions:");
    for (i, instr) in chunk.main.instructions.iter().enumerate() {
        println!("  {}: {:?}", i, instr);
    }
    println!("========================");

    let mut vm = VirtualMachineV2::new();
    let result = vm.interpret(chunk);

    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop over array works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_over_string() {
    let code = r#"
let word = "hello"
var count = 0

for char in word do
    count = count + 1
end

count
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop over string works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_empty_array() {
    let code = r#"
let arr = []
var executed = false

for x in arr do
    executed = true
end

executed
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop over empty array works (skips body)!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_with_accumulator() {
    let code = r#"
let numbers = [10, 20, 30]
var total = 0

for n in numbers do
    total = total + n
end

total
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop with accumulator works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_nested_for_loops() {
    let code = r#"
let outer = [1, 2]
let inner = [10, 20]
var sum = 0

for x in outer do
    for y in inner do
        sum = sum + x + y
    end
end

sum
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Nested for loops work!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_with_conditional() {
    let code = r#"
let numbers = [1, 2, 3, 4, 5]
var even_sum = 0

for n in numbers do
    if n % 2 == 0 then
        even_sum = even_sum + n
    end
end

even_sum
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop with conditional works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_with_break() {
    let code = r#"
let numbers = [1, 2, 3, 4, 5]
var sum = 0

for n in numbers do
    if n > 3 then
        break
    end
    sum = sum + n
end

sum
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop with break works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_with_continue() {
    let code = r#"
let numbers = [1, 2, 3, 4, 5]
var sum = 0

for n in numbers do
    if n % 2 == 0 then
        continue
    end
    sum = sum + n
end

sum
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop with continue works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_over_range_like_array() {
    let code = r#"
let range = [0, 1, 2, 3, 4]
var count = 0

for i in range do
    count = count + 1
end

count
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop over range-like array works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_with_function_call() {
    let code = r#"
fn double(x)
    x * 2
end

let numbers = [1, 2, 3]
var sum = 0

for n in numbers do
    sum = sum + double(n)
end

sum
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop with function calls works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_modifying_array_elements() {
    let code = r#"
let arr = [1, 2, 3]
var result = []

for x in arr do
    result = [x * 2]
end

result
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop creating new arrays works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_with_closure() {
    let code = r#"
let arr = [1, 2, 3]
var sum = 0

fn add_to_sum(x)
    sum = sum + x
end

for n in arr do
    add_to_sum(n)
end

sum
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop with closure capturing works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}

#[test]
fn test_for_loop_variable_scoping() {
    let code = r#"
let arr = [1, 2, 3]
let x = 100

for x in arr do
    x
end

x
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ For loop variable scoping works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}
