//! Bytecode file compilation and loading
//!
//! This module provides utilities for compiling Veld source code to bytecode files
//! and loading bytecode files for execution.

use std::fs;
use std::path::Path;
use veld_common::bytecode_v2::Chunk;
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_error::{Result, VeldError};

use crate::compiler_v2::RegisterCompiler;
use crate::vm_v2::VirtualMachine;

/// File extension for Veld source files
pub const SOURCE_EXTENSION: &str = "veld";

/// File extension for Veld bytecode files
pub const BYTECODE_EXTENSION: &str = "veldc";

/// Compile a Veld source file to a bytecode file
pub fn compile_to_file<P: AsRef<Path>>(source_path: P, output_path: P) -> Result<()> {
    let source_path = source_path.as_ref();
    let output_path = output_path.as_ref();

    // Read source file
    let source = fs::read_to_string(source_path)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to read source file: {}", e)))?;

    // Compile to bytecode
    let chunk = compile_source(&source, source_path.to_string_lossy().to_string())?;

    // Serialize to bytes
    let bytes = chunk
        .to_bytes()
        .map_err(|e| VeldError::RuntimeError(format!("Failed to serialize bytecode: {}", e)))?;

    // Write to output file
    fs::write(output_path, bytes)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to write bytecode file: {}", e)))?;

    Ok(())
}

/// Compile Veld source code to a bytecode Chunk
pub fn compile_source(source: &str, filename: String) -> Result<Chunk> {
    // Lexical analysis
    let mut lexer = Lexer::new(source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| VeldError::LexerError(e))?;

    // Parsing
    let mut parser = Parser::new(tokens);
    let statements = parser
        .parse()
        .map_err(|e| VeldError::ParserError(format!("{:?}", e)))?;
    let ast = veld_common::ast::AST::new(statements);

    // Compile to bytecode
    let mut compiler = RegisterCompiler::new();
    let chunk = compiler.compile(&ast)?;

    Ok(chunk)
}

/// Load and deserialize a bytecode file
pub fn load_bytecode_file<P: AsRef<Path>>(path: P) -> Result<Chunk> {
    let path = path.as_ref();

    // Read file
    let bytes = fs::read(path)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to read bytecode file: {}", e)))?;

    // Deserialize
    let chunk = Chunk::from_bytes(&bytes)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to load bytecode: {}", e)))?;

    Ok(chunk)
}

/// Execute a bytecode file
pub fn run_bytecode_file<P: AsRef<Path>>(path: P) -> Result<crate::vm_v2::InterpretResult> {
    let chunk = load_bytecode_file(path)?;
    let mut vm = VirtualMachine::new();
    Ok(vm.interpret(chunk))
}

/// Detect file type based on extension and content
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Source,
    Bytecode,
    Unknown,
}

impl FileType {
    /// Detect file type from path and optionally peek at content
    pub fn detect<P: AsRef<Path>>(path: P) -> Self {
        let path = path.as_ref();

        // First try extension
        if let Some(ext) = path.extension() {
            if ext == SOURCE_EXTENSION {
                return FileType::Source;
            } else if ext == BYTECODE_EXTENSION {
                return FileType::Bytecode;
            }
        }

        // Try to read magic bytes
        if let Ok(bytes) = fs::read(path) {
            if bytes.len() >= 8 {
                if Chunk::is_bytecode(&bytes) {
                    return FileType::Bytecode;
                }
                // Check if it looks like text (simple heuristic)
                if bytes.iter().take(512).all(|&b| b.is_ascii() || b > 127) {
                    return FileType::Source;
                }
            }
        }

        FileType::Unknown
    }

    /// Get the canonical extension for this file type
    pub fn extension(&self) -> Option<&'static str> {
        match self {
            FileType::Source => Some(SOURCE_EXTENSION),
            FileType::Bytecode => Some(BYTECODE_EXTENSION),
            FileType::Unknown => None,
        }
    }
}

/// Smart file runner that auto-detects file type
pub fn run_file<P: AsRef<Path>>(path: P) -> Result<crate::vm_v2::InterpretResult> {
    let path = path.as_ref();
    let file_type = FileType::detect(path);

    match file_type {
        FileType::Bytecode => {
            // Load and run bytecode
            run_bytecode_file(path)
        }
        FileType::Source => {
            // Compile and run source
            let source = fs::read_to_string(path).map_err(|e| {
                VeldError::RuntimeError(format!("Failed to read source file: {}", e))
            })?;
            let chunk = compile_source(&source, path.to_string_lossy().to_string())?;
            let mut vm = VirtualMachine::new();
            Ok(vm.interpret(chunk))
        }
        FileType::Unknown => Err(VeldError::RuntimeError(format!(
            "Cannot determine file type for: {}",
            path.display()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_compile_and_load_bytecode() {
        let source = r#"
fn add(a, b)
    a + b
end

add(5, 10)
"#;

        // Create temp source file
        let mut source_file = NamedTempFile::new().unwrap();
        source_file.write_all(source.as_bytes()).unwrap();
        let source_path = source_file.path();

        // Create temp bytecode file
        let bytecode_file = NamedTempFile::new().unwrap();
        let bytecode_path = bytecode_file.path();

        // Compile
        compile_to_file(source_path, bytecode_path).expect("Compilation failed");

        // Load
        let chunk = load_bytecode_file(bytecode_path).expect("Load failed");

        // Verify
        assert_eq!(chunk.main.name, "main");
        assert!(!chunk.main.instructions.is_empty());
    }

    #[test]
    fn test_file_type_detection() {
        // Test extension-based detection
        assert_eq!(FileType::detect("test.veld"), FileType::Source);
        assert_eq!(FileType::detect("test.veldc"), FileType::Bytecode);
        assert_eq!(FileType::detect("test.txt"), FileType::Unknown);
    }

    #[test]
    fn test_run_source_file() {
        let source = r#"
let x = 10
let y = 20
x + y
"#;

        let mut source_file = NamedTempFile::new().unwrap();
        source_file.write_all(source.as_bytes()).unwrap();

        let result = run_file(source_file.path()).expect("Execution failed");

        match result {
            crate::vm_v2::InterpretResult::Ok(value) => {
                assert_eq!(value, crate::value::BytecodeValue::Integer(30));
            }
            _ => panic!("Expected Ok result"),
        }
    }
}
