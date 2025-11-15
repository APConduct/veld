// Test script to verify hover info returns correct types
use std::cell::RefCell;
use std::rc::Rc;
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_common::types::checker::TypeChecker;

fn main() {
    // Initialize tracing for debug output
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .init();

    let source = r#"
import std.io as io

pub fn main() => do
    io.println("Hello, World!")
end

fn no_params() => do
    io.println("No parameters")
end

fn add(a, b) => do
    a + b
end
"#;

    println!("Testing hover info for functions...\n");
    println!("Source code:");
    println!("{}", source);
    println!("\n{}", "=".repeat(60));

    // Lex and parse
    let mut lexer = Lexer::new(source);
    let tokens = match lexer.collect_tokens() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            return;
        }
    };

    let mut parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            return;
        }
    };

    println!("\nParsed {} statements", statements.len());

    // Type check
    let mut type_checker = TypeChecker::new();

    // Register the import alias
    type_checker
        .module_registry()
        .borrow_mut()
        .register_alias("io".to_string(), "std.io".to_string());

    if let Err(e) = type_checker.check_program(&statements) {
        eprintln!("Type check error: {}", e);
    }

    let type_checker_rc = Rc::new(RefCell::new(type_checker));

    println!("\n{}", "=".repeat(60));
    println!("Function Types:");
    println!("{}", "=".repeat(60));

    // Check function types
    let tc = type_checker_rc.borrow();

    // Check main function
    if let Some(main_type) = tc.env().get("main") {
        println!("\nmain: {:?}", main_type);

        if let veld_common::types::Type::Function {
            params,
            return_type,
        } = main_type
        {
            println!("  - Parameters: {} (expected: 0)", params.len());
            println!("  - Return type: {:?}", return_type);

            match return_type.as_ref() {
                veld_common::types::Type::Unit => {
                    println!("  ✓ PASS: Return type is Unit (())")
                }
                veld_common::types::Type::TypeVar(id) => {
                    println!("  ✗ FAIL: Return type is TypeVar({}) instead of Unit", id);
                    println!("         This is the bug we're trying to fix!");
                }
                other => {
                    println!("  ? Return type is: {:?}", other);
                }
            }
        }
    } else {
        println!("\n✗ FAIL: main function not found in type environment");
    }

    // Check no_params function
    if let Some(no_params_type) = tc.env().get("no_params") {
        println!("\nno_params: {:?}", no_params_type);

        if let veld_common::types::Type::Function {
            params,
            return_type,
        } = no_params_type
        {
            println!("  - Parameters: {} (expected: 0)", params.len());
            println!("  - Return type: {:?}", return_type);

            match return_type.as_ref() {
                veld_common::types::Type::Unit => {
                    println!("  ✓ PASS: Return type is Unit (())")
                }
                veld_common::types::Type::TypeVar(id) => {
                    println!("  ✗ FAIL: Return type is TypeVar({}) instead of Unit", id);
                }
                other => {
                    println!("  ? Return type is: {:?}", other);
                }
            }
        }
    } else {
        println!("\n✗ FAIL: no_params function not found in type environment");
    }

    // Check add function
    if let Some(add_type) = tc.env().get("add") {
        println!("\nadd: {:?}", add_type);

        if let veld_common::types::Type::Function {
            params,
            return_type,
        } = add_type
        {
            println!("  - Parameters: {} (expected: 2)", params.len());
            println!("  - Return type: {:?}", return_type);
        }
    } else {
        println!("\n✗ FAIL: add function not found in type environment");
    }

    println!("\n{}", "=".repeat(60));
}
