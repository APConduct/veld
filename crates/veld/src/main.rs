use colored::Colorize;
use std::env;
use std::fs;
use tracing::Level;
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_error::{Result, VeldError};
use veld_interpreter::interpreter::Interpreter;
use veld_repl::Repl;

fn main() -> Result<()> {
    // Always run in a new thread with a larger stack on all platforms
    let handle = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024) // 16 MB stack
        .spawn(|| {
            tracing_subscriber::fmt::init();

            // Get command line arguments
            let args: Vec<String> = env::args().collect();

            if args.len() == 1 {
                if args[1] == "build" {
                    // TODO: Implement bytecode compilation
                    println!("Bytecode compilation not implemented yet");
                } else if args[1] == "run" {
                    // TODO: Implement running bytecode
                }
                let mut repl = Repl::new();
                repl.run()
                    .map_err(|e| veld_error::VeldError::RuntimeError(format!("{e}")))?;
                Ok(())
            } else if args.len() == 2 {
                if args[1] == "build" {
                    // TODO: Implement bytecode compilation
                    println!("Bytecode compilation not implemented yet");
                    run_file(&args[1])
                } else if args[1] == "run" {
                    // TODO: Implement running bytecode
                    run_file(&args[1])
                } else if args[1] == "opt" {
                    // TODO: Implement bitcode compilation
                    run_file(&args[1])
                } else {
                    run_file(&args[1])
                }
                // One argument - interpret the file
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
    let _span = tracing::span!(tracing::Level::TRACE, "run_file", filename = filename);
    let _enter = _span.enter();
    tracing::event!(tracing::Level::TRACE, "Running file: {}", filename);

    // Get absolute path
    let abs_filename = fs::canonicalize(filename)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to get absolute path: {}", e)))?
        .to_str()
        .ok_or_else(|| VeldError::RuntimeError("Invalid filename".to_string()))?
        .to_string();

    tracing::event!(tracing::Level::TRACE, "Absolute path: {}", abs_filename);

    // Only tace on debug builds
    #[cfg(debug_assertions)]
    tracing::info!(
        "{}",
        "Veld Language Interpreter v0.1.4".bold().bright_blue()
    );
    #[cfg(debug_assertions)]
    tracing::info!(
        "{} {}",
        "Running file: ".bright_green(),
        format!("{}", filename.italic().yellow())
    );

    // Read the file
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            tracing::error!("Error reading file: {}", e);
            return Ok(());
        }
    };

    // Lexical analysis
    let mut lexer = Lexer::new(&source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| VeldError::LexerError(e))?;

    // Debug print all lexer tokens for inspection
    tracing::debug!("TOKEN COUNT: {}", tokens.len());
    for token in &tokens {
        tracing::debug!("LEXER TOKEN: {:?}", token);
        println!("LEXER TOKEN: {:?}", token);
    }

    // Parsing
    let parser = Parser::new(tokens.clone());
    tracing::event!(Level::TRACE, "Starting parsing...");

    // match parser.parse() {
    match parser.parse_with_source_map(source.as_str(), abs_filename) {
        Ok(ast) => {
            // Debug print all top-level AST statements for inspection
            tracing::debug!("PARSED AST STATEMENTS: {:?}", ast);
            // Always print AST with tracing before interpretation
            tracing::info!("==== DEBUG: AST ====");
            tracing::info!("{:#?}", ast);
            tracing::info!("==== END AST ====");

            // Run the interpreter if parsing succeeds
            let mut interpreter = Interpreter::new("../..");
            match interpreter.interpret_ast(ast) {
                Ok(result) => {
                    #[cfg(debug_assertions)]
                    tracing::info!(
                        "{} {}",
                        "Program result:".bright_green(),
                        format!("{:?}", result).italic().bright_yellow()
                    );
                }
                Err(e) => tracing::error!("Runtime error: {:?}", e),
            }
        }
        Err(e) => tracing::error!("Parse error: {:?}", e),
    }
    #[cfg(debug_assertions)]
    tracing::event!(Level::INFO, "{}", "Execution complete".bright_blue());
    Ok(())
}
