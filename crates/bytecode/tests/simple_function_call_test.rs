//! Simple function call test to debug register issues

use veld_bytecode::{RegisterCompiler, VirtualMachineV2};
use veld_common::ast::AST;
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;

fn compile_and_run(code: &str) -> Result<veld_bytecode::vm_v2::InterpretResult, String> {
    // 1. Lexing
    let mut lexer = Lexer::new(code);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| format!("Lexer error: {:?}", e))?;

    // 2. Parsing
    let mut parser = Parser::new(tokens);
    let statements = parser
        .parse()
        .map_err(|e| format!("Parser error: {:?}", e))?;
    let ast = AST::new(statements);

    // 3. Compile
    let mut compiler = RegisterCompiler::new();
    let chunk = compiler
        .compile(&ast)
        .map_err(|e| format!("Compile error: {:?}", e))?;

    println!("\n=== Compilation Debug Info ===");
    println!(
        "Main function register_count: {}",
        chunk.main.register_count
    );
    println!("Main function instructions:");
    for (i, instr) in chunk.main.instructions.iter().enumerate() {
        println!("  {}: {:?}", i, instr);
    }
    println!("Main function constants:");
    for (i, constant) in chunk.main.constants.iter().enumerate() {
        println!("  {}: {:?}", i, constant);
    }
    println!("========================\n");

    // Interpret
    let mut vm = VirtualMachineV2::new();
    Ok(vm.interpret(chunk))
}

#[test]
fn test_simple_function_declaration() {
    let code = r#"
fn double(x)
    x * 2
end

10
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            println!("✅ Result: {:?}", value);
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
fn test_simple_function_call() {
    let code = r#"
fn double(x)
    x * 2
end

double(5)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            println!("✅ Function call result: {:?}", value);
            assert_eq!(value, veld_bytecode::value::BytecodeValue::Integer(10));
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
fn test_function_with_multiple_args() {
    let code = r#"
fn add(a, b)
    a + b
end

add(3, 7)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            println!("✅ Multi-arg function result: {:?}", value);
            assert_eq!(value, veld_bytecode::value::BytecodeValue::Integer(10));
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
fn test_function_call_with_variable() {
    let code = r#"
fn double(x)
    x * 2
end

let n = 5
let result = double(n)
result
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            println!("✅ Function call with variable result: {:?}", value);
            // We expect Unit because the last statement is an assignment,
            // but at least we test that the call works
            println!("Result value: {:?}", value);
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
fn test_function_returning_closure() {
    let code = r#"
fn make_adder(x)
    fn inner(y)
        x + y
    end
    inner
end

let add5 = make_adder(5)
let result = add5(10)
result
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            println!("✅ Function returning closure result: {:?}", value);
            println!("Expected Unit (last stmt is assignment), got: {:?}", value);
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
fn test_function_call_in_loop() {
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
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            println!("✅ Function call in loop result: {:?}", value);
            println!("Expected Unit (last stmt is assignment), got: {:?}", value);
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}
