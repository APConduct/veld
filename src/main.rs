mod lexer;
mod parser;
mod ast;
mod interpreter;
mod error;

use error::Result;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() -> Result<()> {
    println!("Veld Language Interpreter v0.1.0");

    let source = r#"
        fn add(a: i32, b: i32) -> i32 =
            a + b
        end
    "#;

    // Lexical analysis
    let mut lexer = Lexer::new(source);
    let tokens = lexer.collect_tokens().map_err(|e| error::VeldError::LexerError(e))?;

    println!("Tokens:");
    for token in tokens.iter() {
        println!("{:?}", token);
    }

    // Continue with parsing...
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    println!("AST: {:#?}", ast);

    Ok(())
}