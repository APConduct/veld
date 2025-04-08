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

    // Use a MUCH simpler test case to isolate the issue
    let source = r#"
    fn add(a: i32, b: i32) -> i32 = a + b;


    struct Point
        x: f64,
        y: f64,
    end

    impl Point
        fn distance(self) -> f64 = 0.0
    end

    fn test() -> f64 = (1.0 + 2.0).sqrt();

    let p = Vector.new(x: 1.0, y: 2.0)


    "#;

    // Lexical analysis
    let mut lexer = Lexer::new(source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| error::VeldError::LexerError(e))?;

    println!("\nTokens:");
    for token in tokens.iter() {
        println!("{:?}", token);
    }

    // Parsing with debugging
    let mut parser = Parser::new(tokens.clone());
    println!("Starting parsing...");

    match parser.parse() {
        Ok(stmts) => {
            println!("Successfully parsed {} statements:", stmts.len());
            for (i, stmt) in stmts.iter().enumerate() {
                println!("Statement {}: {:?}", i + 1, stmt);
            }
        }
        Err(e) => println!("Parse error: {:?}", e),
    }

    println!("Parsing complete");
    Ok(())
}
