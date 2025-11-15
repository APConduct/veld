use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use std::fs;

fn main() {
    let source = fs::read_to_string("test_interpreter_tuple_bug.veld")
        .expect("Failed to read file");

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize().expect("Lexer error");

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Parser error");

    println!("AST:");
    println!("{:#?}", ast);
}
