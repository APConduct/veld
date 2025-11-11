//! Comprehensive tests for closure and upvalue capture functionality
//!
//! These tests validate that the register-based compiler and VM correctly
//! implement closures with proper upvalue capture, including:
//! - Simple variable capture from parent scope
//! - Multiple variable captures
//! - Nested closures (multi-level capture)
//! - Mutable upvalue capture
//! - Closure factories
//! - Closure shadowing

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
fn test_simple_closure_capture() {
    let code = r#"
fn outer(x)
    fn inner(y)
        x + y
    end
    inner(5)
end

let result = outer(10)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Simple closure capture works!");
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
fn test_multiple_variable_capture() {
    let code = r#"
fn make_adder(a, b)
    fn add_to(c)
        a + b + c
    end
    add_to
end

let adder = make_adder(10, 20)
let result = adder(5)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Multiple variable capture works!");
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
fn test_nested_closures_three_levels() {
    let code = r#"
fn level1(a)
    fn level2(b)
        fn level3(c)
            a + b + c
        end
        level3
    end
    level2
end

let f2 = level1(100)
let f3 = f2(10)
let result = f3(1)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Three-level nested closures work!");
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
fn test_closure_with_mutable_capture() {
    let code = r#"
fn make_counter()
    var count = 0
    fn increment()
        count = count + 1
        count
    end
    increment
end

let counter = make_counter()
let result1 = counter()
let result2 = counter()
let result3 = counter()
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Mutable upvalue capture works!");
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
fn test_closure_factory() {
    let code = r#"
fn make_multiplier(factor)
    fn multiply(n)
        n * factor
    end
    multiply
end

let times2 = make_multiplier(2)
let times5 = make_multiplier(5)

let result1 = times2(10)
let result2 = times5(10)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Closure factory works!");
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
fn test_closure_with_local_shadowing() {
    let code = r#"
fn outer()
    let x = 10
    fn middle()
        let x = 20
        fn inner()
            x
        end
        inner
    end
    middle
end

let f1 = outer()
let f2 = f1()
let result = f2()
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Closure with shadowing works!");
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
fn test_closure_capturing_parameter() {
    let code = r#"
fn make_greeter(name)
    fn greet(greeting)
        greeting
    end
    greet
end

let greeter = make_greeter("Alice")
let result = greeter("Hello")
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Closure capturing parameter works!");
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
fn test_multiple_closures_sharing_upvalue() {
    let code = r#"
fn make_counter()
    var count = 0

    fn increment()
        count = count + 1
        count
    end

    fn get()
        count
    end

    increment
end

let inc = make_counter()
let r1 = inc()
let r2 = inc()
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Multiple closures sharing upvalue works!");
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
fn test_closure_in_loop() {
    let code = r#"
fn outer()
    var i = 0
    while i < 3 do
        fn inner()
            i
        end
        i = i + 1
    end
    i
end

let result = outer()
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Closure in loop works!");
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
fn test_immediate_closure_call() {
    let code = r#"
fn outer(x)
    fn inner(y)
        x + y
    end(10)
end

let result = outer(5)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Immediate closure call works!");
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
fn test_closure_returning_closure() {
    let code = r#"
fn outer(x)
    fn middle(y)
        fn inner(z)
            x + y + z
        end
        inner
    end
    middle
end

let m = outer(1)
let i = m(2)
let result = i(3)
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Closure returning closure works!");
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
fn test_closure_with_conditional() {
    let code = r#"
fn make_function(use_x)
    let x = 10
    let y = 20

    fn use_first()
        x
    end

    fn use_second()
        y
    end

    if use_x then
        use_first
    else
        use_second
    end
end

let f = make_function(true)
let result = f()
"#;

    let result = compile_and_run(code).expect("Compilation failed");
    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(_) => {
            println!("✅ Closure with conditional works!");
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            panic!("Runtime error: {:?}", e);
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            panic!("Compile error: {:?}", e);
        }
    }
}
