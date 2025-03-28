use thiserror::Error;

#[derive(Error, Debug)]
pub enum VeldError {
    #[error("Lexer error: {0}")]
    LexerError(String),

    #[error("Parser error: {0}")]
    ParserError(String),

    #[error("Runtime error: {0}")]
    RuntimeError(String),
}

pub type Result<T> = std::result::Result<T, VeldError>;
