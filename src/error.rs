use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VeldError {
    #[error("Lexer error: {0}")]
    LexerError(String),

    #[error("Parser error: {0}")]
    ParserError(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Runtime error: {0}")]
    RuntimeError(String),
}

pub struct ErrorContext {
    pub line: usize,
    pub column: usize,
    pub source_line: String,
}

impl VeldError {
    pub fn with_context(self, context: ErrorContext) -> VeldErrorWithContext{
        VeldErrorWithContext{
            error: self,
            context: Some(context)
        }
    }
}

pub struct VeldErrorWithContext {
    pub error: VeldError,
    pub context: Option<ErrorContext>,
}

impl fmt::Display for VeldErrorWithContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.context {
            Some(ctx) => {
                let error_msg = format!("{}", self.error);
                let ptr = " ".repeat(ctx.column) + "^";
                write!(
                    f,
                    "{}\nLine {}, column {}:\n{}\n{}",
                    error_msg, ctx.line, ctx.column, ctx.source_line, ptr
                )
            }
            None => write!(f, "{}", self.error)
        }
    }
}

impl fmt::Debug for VeldErrorWithContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub type Result<T> = std::result::Result<T, VeldError>;

pub type ContextResult<T> = std::result::Result<T, VeldErrorWithContext>;
