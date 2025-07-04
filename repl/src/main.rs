mod repl;

use crate::repl::Repl; // Add this line
use std::env;
use std::fs;
use veld_core::error::{Result, VeldError};
use veld_core::interpreter::Interpreter;
use veld_core::lexer::Lexer;
use veld_core::parser::Parser;

fn main() -> Result<()> {
    // Get command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        // No arguments, run in REPL mode
        println!("Starting Veld in REPL mode...");
        let mut repl = Repl::new();
        repl.run()
    } else if args.len() == 2 {
        // One argument - interpret the file
        return run_file(&args[1]);
    } else {
        println!("Usage: veld [filename]");
        println!("       veld           # Run in REPL mode");
        Ok(())
    }
}

fn run_file(filename: &str) -> Result<()> {
    println!("Veld Language Interpreter v0.1.2");
    println!("Running file: {}", filename);

    // Read the file
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            println!("Error reading file: {}", e);
            return Ok(());
        }
    };

    // Lexical analysis
    let mut lexer = Lexer::new(&source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| VeldError::LexerError(e))?;

    // Parsing
    let mut parser = Parser::new(tokens.clone());
    println!("Starting parsing...");

    match parser.parse() {
        Ok(stmts) => {
            // Run the interpreter if parsing succeeds
            let mut interpreter = Interpreter::new("../..");
            match interpreter.interpret(stmts) {
                Ok(result) => println!("Program result: {:?}", result),
                Err(e) => println!("Runtime error: {:?}", e),
            }
        }
        Err(e) => println!("Parse error: {:?}", e),
    }

    println!("Execution complete");
    Ok(())
}
