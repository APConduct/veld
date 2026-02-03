use colored::Colorize;
use std::env;
use std::fs;
use std::path::PathBuf;
use veld_bytecode::{
    BYTECODE_EXTENSION, FileType, SOURCE_EXTENSION, compile_to_file, run_bytecode_file, run_file,
};
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
            // Initialize tracing subscriber (only on debug builds)
            #[cfg(debug_assertions)]
            tracing_subscriber::fmt::init();

            // Get command line arguments
            let args: Vec<String> = env::args().collect();

            match args.len() {
                1 => {
                    // No arguments - run REPL
                    run_repl()
                }
                2 => {
                    // One argument - could be a command or a file
                    match args[1].as_str() {
                        "help" | "--help" | "-h" => {
                            print_help();
                            Ok(())
                        }
                        "version" | "--version" | "-v" => {
                            print_version();
                            Ok(())
                        }
                        _ => {
                            // Assume it's a file to run
                            run_file_auto(&args[1])
                        }
                    }
                }
                3 => {
                    // Two arguments - command with argument
                    match args[1].as_str() {
                        "build" | "compile" => {
                            // Build source to bytecode with automatic output name
                            compile_file(&args[2], None)
                        }
                        "run" => {
                            // Run a specific file (source or bytecode)
                            run_file_auto(&args[2])
                        }
                        "disasm" | "disassemble" => {
                            // Disassemble bytecode file
                            disassemble_file(&args[2])
                        }
                        "ast" | "debug-ast" => {
                            // Print AST for debugging
                            print_ast(&args[2])
                        }
                        _ => {
                            eprintln!("Unknown command: {}", args[1]);
                            print_help();
                            Ok(())
                        }
                    }
                }
                4 => {
                    // Three arguments - command with input and output
                    match args[1].as_str() {
                        "build" | "compile" => {
                            // Build source to bytecode with explicit output
                            compile_file(&args[2], Some(&args[3]))
                        }
                        "-o" | "--output" => {
                            // Alternative syntax: veld -o output.veldc input.veld
                            compile_file(&args[3], Some(&args[2]))
                        }
                        _ => {
                            eprintln!("Unknown command: {}", args[1]);
                            print_help();
                            Ok(())
                        }
                    }
                }
                _ => {
                    eprintln!("Too many arguments");
                    print_help();
                    Ok(())
                }
            }
        })
        .unwrap();

    // Propagate any error from the thread
    handle.join().unwrap()
}

fn print_help() {
    println!("{}", "Veld Programming Language".bold().bright_blue());
    println!("Version 0.1.4\n");
    println!("{}", "USAGE:".bold());
    println!("  veld                            Start interactive REPL");
    println!("  veld <file>                     Run a Veld file (auto-detects type)");
    println!("  veld run <file>                 Run a Veld file (source or bytecode)");
    println!("  veld build <source> [output]    Compile source to bytecode (.veldc)");
    println!("  veld disasm <bytecode>          Disassemble bytecode file");
    println!("  veld ast <source>               Print AST for debugging");
    println!("  veld help                       Show this help message");
    println!("  veld version                    Show version information");
    println!();
    println!("{}", "FILE TYPES:".bold());
    println!("  .veld   Source code files");
    println!("  .veldc  Compiled bytecode files");
    println!();
    println!("{}", "EXAMPLES:".bold());
    println!("  veld script.veld                Run source file");
    println!("  veld program.veldc              Run bytecode file");
    println!("  veld build script.veld          Compile to script.veldc");
    println!("  veld build script.veld out.veldc  Compile to specific output");
    println!("  veld disasm program.veldc       Show bytecode disassembly");
}

fn print_version() {
    println!("{} {}", "Veld".bold().bright_blue(), "v0.1.4");
    println!("Register-based VM with bytecode compilation");
}

fn run_repl() -> Result<()> {
    #[cfg(debug_assertions)]
    println!("{}", "Veld Interactive REPL v0.1.4".bold().bright_blue());

    let mut repl = Repl::new();
    repl.run()
        .map_err(|e| VeldError::RuntimeError(format!("{e}")))?;
    Ok(())
}

fn run_file_auto(filename: &str) -> Result<()> {
    let file_type = FileType::detect(filename);

    #[cfg(debug_assertions)]
    {
        tracing::info!("{}", "Veld Language v0.1.4".bold().bright_blue());
        tracing::info!(
            "{} {} ({})",
            "Running file:".bright_green(),
            filename.italic().yellow(),
            match file_type {
                FileType::Source => "source",
                FileType::Bytecode => "bytecode",
                FileType::Unknown => "unknown type",
            }
        );
    }

    match file_type {
        FileType::Bytecode => {
            // Run bytecode with the new VM
            run_bytecode_with_new_vm(filename)
        }
        FileType::Source => {
            // Try running with new VM first, fall back to interpreter
            run_source_with_new_vm_or_interpreter(filename)
        }
        FileType::Unknown => {
            eprintln!(
                "{} Cannot determine file type for: {}",
                "Error:".red().bold(),
                filename
            );
            Err(VeldError::RuntimeError(format!(
                "Unknown file type: {}",
                filename
            )))
        }
    }
}

fn run_bytecode_with_new_vm(filename: &str) -> Result<()> {
    let result = run_bytecode_file(filename)?;

    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            #[cfg(debug_assertions)]
            tracing::info!(
                "{} {:?}",
                "Program result:".bright_green(),
                value.to_string().italic().bright_yellow()
            );
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            eprintln!("{} {:?}", "Runtime error:".red().bold(), e);
            return Err(VeldError::RuntimeError(format!("{:?}", e)));
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            eprintln!("{} {}", "Compile error:".red().bold(), e);
            return Err(VeldError::CompileError {
                message: e,
                line: None,
                column: None,
            });
        }
    }

    Ok(())
}

fn run_source_with_new_vm_or_interpreter(filename: &str) -> Result<()> {
    // Try the new VM first
    match run_source_with_new_vm(filename) {
        Ok(()) => Ok(()),
        Err(e) => {
            // If new VM fails, fall back to interpreter with warning
            #[cfg(debug_assertions)]
            tracing::warn!("New VM failed ({}), falling back to interpreter", e);

            run_source_with_interpreter(filename)
        }
    }
}

fn run_source_with_new_vm(filename: &str) -> Result<()> {
    let result = run_file(filename)?;

    match result {
        veld_bytecode::vm_v2::InterpretResult::Ok(value) => {
            #[cfg(debug_assertions)]
            tracing::info!(
                "{} {:?}",
                "Program result:".bright_green(),
                value.to_string().italic().bright_yellow()
            );
        }
        veld_bytecode::vm_v2::InterpretResult::RuntimeError(e) => {
            eprintln!("{} {:?}", "Runtime error:".red().bold(), e);
            return Err(VeldError::RuntimeError(format!("{:?}", e)));
        }
        veld_bytecode::vm_v2::InterpretResult::CompileError(e) => {
            eprintln!("{} {}", "Compile error:".red().bold(), e);
            return Err(VeldError::CompileError {
                message: e,
                line: None,
                column: None,
            });
        }
    }

    Ok(())
}

fn run_source_with_interpreter(filename: &str) -> Result<()> {
    // Get absolute path
    let abs_filename = fs::canonicalize(filename)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to get absolute path: {}", e)))?
        .to_str()
        .ok_or_else(|| VeldError::RuntimeError("Invalid filename".to_string()))?
        .to_string();

    // Read the file
    let source = fs::read_to_string(filename)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to read file: {}", e)))?;

    // Lexical analysis
    let mut lexer = Lexer::new(&source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| VeldError::LexerError(e))?;

    // Parsing
    let parser = Parser::new(tokens.clone());

    match parser.parse_with_source_map(source.as_str(), abs_filename) {
        Ok(ast) => {
            // Run the interpreter
            let mut interpreter = Interpreter::new("../..");
            match interpreter.interpret_ast(ast) {
                Ok(result) => {
                    #[cfg(debug_assertions)]
                    tracing::info!(
                        "{} {:?}",
                        "Program result:".bright_green(),
                        result.to_string().italic().bright_yellow()
                    );
                }
                Err(e) => {
                    eprintln!("{} {:?}", "Runtime error:".red().bold(), e);
                    return Err(e);
                }
            }
        }
        Err(e) => {
            eprintln!("{} {:?}", "Parse error:".red().bold(), e);
            return Err(VeldError::ParserError(format!("{:?}", e)));
        }
    }

    #[cfg(debug_assertions)]
    tracing::info!("{}", "Execution complete".bright_blue());

    Ok(())
}

fn compile_file(source_file: &str, output_file: Option<&str>) -> Result<()> {
    // Determine output filename
    let output_path = if let Some(output) = output_file {
        PathBuf::from(output)
    } else {
        // Auto-generate output filename: input.veld -> input.veldc
        let mut path = PathBuf::from(source_file);
        path.set_extension(BYTECODE_EXTENSION);
        path
    };

    println!(
        "{} {} -> {}",
        "Compiling:".bright_green().bold(),
        source_file.italic(),
        output_path.display().to_string().bright_yellow()
    );

    // Compile
    compile_to_file(source_file, output_path.to_str().unwrap())?;

    println!(
        "{} Bytecode written to {}",
        "Success:".bright_green().bold(),
        output_path.display().to_string().bright_yellow()
    );

    Ok(())
}

fn print_ast(filename: &str) -> Result<()> {
    // Read the file
    let source = fs::read_to_string(filename)
        .map_err(|e| VeldError::RuntimeError(format!("Failed to read file: {}", e)))?;

    // Parse the source
    let mut lexer = Lexer::new(&source);
    let tokens = lexer
        .collect_tokens()
        .map_err(|e| VeldError::LexerError(e))?;

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Pretty-print the AST
    println!("{:#?}", ast);

    Ok(())
}

fn disassemble_file(bytecode_file: &str) -> Result<()> {
    use veld_bytecode::load_bytecode_file;

    println!(
        "{} {}",
        "Disassembling:".bright_cyan().bold(),
        bytecode_file.italic()
    );

    let chunk = load_bytecode_file(bytecode_file)?;

    println!("\n{}", "=== BYTECODE DISASSEMBLY ===".bold());
    println!("{}", chunk.disassemble());

    Ok(())
}
