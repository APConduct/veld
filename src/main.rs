mod lexer;
mod parser;
mod ast;
mod interpreter;
mod error;

use error::Result;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::interpreter::Interpreter;

fn main() -> Result<()> {
    println!("Veld Language Interpreter v0.1.0");

    let source = r#"
        fn factorial(n: i32) -> i32 =
            if n <= 1
                return 1
            else
                return n * factorial(n - 1)
            end
        end

        let result = factorial(5)
    "#;

    // Lexical analysis
    let mut lexer = Lexer::new(source);
    let tokens = lexer.collect_tokens().map_err(|e| error::VeldError::LexerError(e))?;

    println!("\nTokens:");
    for token in tokens.iter() {
        println!("{:?}", token);
    }

    // Parsing
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    println!("\nAST:");
    println!("{:#?}", ast);

    // Interpretation
    let mut interpreter = Interpreter::new();
    let result = interpreter.interpret(ast)?;

    println!("\nFinal Result:");
    println!("{:#?}", result);

    Ok(())
}