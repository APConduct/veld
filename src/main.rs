mod ast;
mod error;
mod interpreter;
mod lexer;
mod parser;

use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use error::Result;

fn main() -> Result<()> {
    println!("Veld Language Interpreter v0.1.0");

    // Updated test case using function calls instead of method syntax
    let source = r#"
    fn add(a: i32, b: i32) -> i32 = a + b;

    struct Point
        x: f64,
        y: f64,
    end

    impl Point
        fn distance(self) -> f64 = 0.0
    end

    -- Register built-in standard library functions (will be handled specially in interpreter)
    fn sqrt(x: f64) -> f64 = x *^ 0.5; -- example pow syntax for square root

    -- Using a standard library function instead of method syntax
    fn test() -> f64 = sqrt(1.0 + 2.0);


    "#;

    // Lexical analysis
    let mut lexer = Lexer::new(source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| error::VeldError::LexerError(e))?;

    // Parsing with debugging
    let mut parser = Parser::new(tokens.clone());
    println!("Starting parsing...");

    match parser.parse() {
        Ok(stmts) => {
            println!("Successfully parsed {} statements:", stmts.len());
            for (i, stmt) in stmts.iter().enumerate() {
                println!("Statement {}: {:?}", i + 1, stmt);
            }

            // Run the interpreter if parsing succeeds
            let mut interpreter = Interpreter::new();
            match interpreter.interpret(stmts) {
                Ok(result) => println!("Program result: {:?}", result),
                Err(e) => println!("Runtime error: {:?}", e),
            }
        }
        Err(e) => println!("Parse error: {:?}", e),
    }

    println!("Parsing complete");
    Ok(())
}
