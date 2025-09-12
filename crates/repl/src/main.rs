mod repl;

use crate::repl::Repl;
use colored::Colorize;
use std::env;
use std::fs;
use tracing::Level;
use veld_core::interpreter::Interpreter;
use veld_core::lexer::Lexer;
use veld_core::parser::Parser;
use veld_error::{Result, VeldError};

fn main() -> Result<()> {
    // Always run in a new thread with a larger stack on all platforms
    let handle = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024) // 16 MB stack
        .spawn(|| {
            tracing_subscriber::fmt::init();

            // Get command line arguments
            let args: Vec<String> = env::args().collect();

            if args.len() == 1 {
                let mut repl = Repl::new();
                repl.run()
                    .map_err(|e| veld_error::VeldError::RuntimeError(format!("{e}")))?;
                Ok(())
            } else if args.len() == 2 {
                // One argument - interpret the file
                run_file(&args[1])
            } else {
                println!("Usage: veld [filename]");
                println!("       veld           # Run in REPL mode");
                Ok(())
            }
        })
        .unwrap();

    // Propagate any error from the thread
    handle.join().unwrap()
}

fn run_file(filename: &str) -> Result<()> {
    tracing::span!(tracing::Level::TRACE, "run_file", filename = filename);
    tracing::event!(tracing::Level::TRACE, "Running file: {}", filename);
    tracing::info!(
        "{}",
        "Veld Language Interpreter v0.1.4".bold().bright_blue()
    );
    tracing::info!(
        "{} {}",
        "Running file: ".bright_green(),
        format!("{}", filename.italic().yellow())
    );

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
    tracing::event!(Level::TRACE, "Starting parsing...");

    match parser.parse() {
        Ok(stmts) => {
            // Run the interpreter if parsing succeeds
            let mut interpreter = Interpreter::new("../..");
            match interpreter.interpret(stmts) {
                Ok(result) => tracing::info!(
                    "{} {}",
                    "Program result:".bright_green(),
                    format!("{:?}", result).italic().bright_yellow()
                ),
                Err(e) => tracing::error!("Runtime error: {:?}", e),
            }
        }
        Err(e) => tracing::error!("Parse error: {:?}", e),
    }

    tracing::event!(Level::INFO, "{}", "Execution complete".bright_blue());
    Ok(())
}
