use veld_bytecode::{BytecodeCompiler, BytecodeValue, InterpretResult, VirtualMachine};
use veld_common::ast::{AST, Statement};
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;

#[test]
fn test_simple_arithmetic() {
    let code = "return 1 + 2";

    // 1. Lexing
    let mut lexer = Lexer::new(code);
    let tokens = lexer.collect_tokens().expect("Failed to lex code");

    // 2. Parsing
    let mut parser = Parser::new(tokens);
    let statements: Vec<Statement> = parser.parse().expect("Failed to parse code");
    let ast = AST::new(statements);

    // 3. Compiling
    let mut compiler = BytecodeCompiler::new();
    let compilation_result = compiler.compile(&ast);

    assert!(
        compilation_result.errors.is_empty(),
        "Compilation failed with errors: {:?}",
        compilation_result.errors
    );

    // 4. Executing
    let mut vm = VirtualMachine::new();
    let result = vm.interpret(compilation_result.main_chunk);

    match result {
        InterpretResult::Ok(value) => {
            assert_eq!(value, BytecodeValue::Integer(3));
        }
        InterpretResult::RuntimeError(e) => {
            panic!("VM failed with a runtime error: {}", e);
        }
        InterpretResult::CompileError(e) => {
            panic!("VM failed with a compile error: {}", e);
        }
    }
}

#[test]
fn test_local_variables() {
    let code = "let a = 10\nreturn a * 2";

    // 1. Lexing
    let mut lexer = Lexer::new(code);
    let tokens = lexer.collect_tokens().expect("Failed to lex code");

    // 2. Parsing
    let mut parser = Parser::new(tokens);
    let statements: Vec<Statement> = parser.parse().expect("Failed to parse code");
    let ast = AST::new(statements);

    // 3. Compiling
    let mut compiler = BytecodeCompiler::new();
    let compilation_result = compiler.compile(&ast);

    assert!(
        compilation_result.errors.is_empty(),
        "Compilation failed with errors: {:?}",
        compilation_result.errors
    );

    // 4. Executing
    let mut vm = VirtualMachine::new();
    let result = vm.interpret(compilation_result.main_chunk);

    match result {
        InterpretResult::Ok(value) => {
            assert_eq!(value, BytecodeValue::Integer(20));
        }
        InterpretResult::RuntimeError(e) => {
            panic!("VM failed with a runtime error: {}", e);
        }
        InterpretResult::CompileError(e) => {
            panic!("VM failed with a compile error: {}", e);
        }
    }
}

#[test]
fn test_variable_scoping() {
    let code = "let a = 1\ndo\n    let a = 2\nend\nreturn a";

    // 1. Lexing
    let mut lexer = Lexer::new(code);
    let tokens = lexer.collect_tokens().expect("Failed to lex code");

    // 2. Parsing
    let mut parser = Parser::new(tokens);
    let statements: Vec<Statement> = parser.parse().expect("Failed to parse code");
    let ast = AST::new(statements);

    // 3. Compiling
    let mut compiler = BytecodeCompiler::new();
    let compilation_result = compiler.compile(&ast);

    assert!(
        compilation_result.errors.is_empty(),
        "Compilation failed with errors: {:?}",
        compilation_result.errors
    );

    // 4. Executing
    let mut vm = VirtualMachine::new();
    let result = vm.interpret(compilation_result.main_chunk);

    match result {
        InterpretResult::Ok(value) => {
            assert_eq!(value, BytecodeValue::Integer(1));
        }
        InterpretResult::RuntimeError(e) => {
            panic!("VM failed with a runtime error: {}", e);
        }
        InterpretResult::CompileError(e) => {
            panic!("VM failed with a compile error: {}", e);
        }
    }
}
