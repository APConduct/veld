use crate::error::{Result, VeldError};
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

pub struct Repl {
    interpreter: Interpreter,
    history: Vec<String>,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new("."),
            history: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        println!("Veld Language REPL v0.1.0");
        println!("Type 'exit' to quit, 'help' for commands");

        let mut input_buffer = String::new();
        let mut multi_line_mode = false;

        loop {
            // Display appropriate prompt
            if multi_line_mode {
                print!("... ");
            } else {
                print!("> ");
            }
            io::stdout().flush().unwrap();

            // Read input
            let mut line = String::new();
            if io::stdin().read_line(&mut line).is_err() {
                println!("Error reading input");
                continue;
            }

            let line = line.trim_end(); // Remove trailing newline but keep leading whitespace

            // Check for empty line in multi-line mode (terminates input)
            if multi_line_mode && line.trim().is_empty() {
                multi_line_mode = false;
                let result = self.evaluate_input(&input_buffer);
                self.handle_result(result);
                input_buffer.clear();
                continue;
            }

            // Handle special commands
            match line.trim() {
                "exit" | "quit" => break,
                "help" => {
                    self.show_help();
                    continue;
                }
                "history" => {
                    self.show_history();
                    continue;
                }
                "clear" => {
                    // Clear screen (ANSI escape sequence)
                    print!("\x1B[2J\x1B[1;1H");
                    continue;
                }
                "" => continue,
                line => {
                    // Check for multi-line input
                    if line.ends_with('\\') {
                        // Remove the continuation character
                        input_buffer.push_str(&line[..line.len() - 1]);
                        input_buffer.push('\n');
                        multi_line_mode = true;
                        continue;
                    }

                    // Add to current buffer
                    input_buffer.push_str(line);

                    // Process the input if not in multi-line mode
                    if !multi_line_mode {
                        // Add to history
                        self.history.push(input_buffer.clone());

                        // Evaluate and handle result
                        let result = self.evaluate_input(&input_buffer);
                        self.handle_result(result);
                        input_buffer.clear();
                    }
                }
            }
        }

        println!("Goodbye!");
        Ok(())
    }

    fn evaluate_input(&mut self, input: &str) -> Result<crate::interpreter::Value> {
        // Lexical analysis
        let mut lexer = Lexer::new(input);
        let tokens = lexer
            .collect_tokens()
            .map_err(|e| VeldError::LexerError(e))?;

        // Parsing
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;

        // Interpretation
        self.interpreter.interpret(statements)
    }

    fn handle_result(&self, result: Result<crate::interpreter::Value>) {
        match result {
            Ok(value) => println!("=> {:?}", value),
            Err(e) => println!("Error: {:?}", e),
        }
    }

    fn show_help(&self) {
        println!("Available commands:");
        println!("  exit, quit - Exit the REPL");
        println!("  help       - Show this help message");
        println!("  history    - Show command history");
        println!("  clear      - Clear the screen");
        println!("  [code]\\    - Continue code on next line");
        println!("  <empty>    - End multi-line input");
    }

    fn show_history(&self) {
        println!("Command history:");
        for (i, cmd) in self.history.iter().enumerate() {
            println!("{}: {}", i + 1, cmd);
        }
    }
}