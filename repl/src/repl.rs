use std::collections::HashMap;
use std::io::{self, Write};
use veld_core::error::{ContextResult, ErrorContext, Result, VeldError, VeldErrorWithContext};
use veld_core::interpreter::Interpreter;
use veld_core::lexer::Lexer;
use veld_core::parser::Parser;

pub struct Repl {
    interpreter: Interpreter,
    history: Vec<String>,
    variables: HashMap<String, String>,
    line_number: usize,
}

impl Repl {
    pub fn new() -> Self {
        let mut variables = HashMap::new();
        variables.insert("multiline_mode".to_string(), "block".to_string());
        variables.insert("debug".to_string(), "false".to_string());

        Self {
            interpreter: Interpreter::new("../.."),
            history: Vec::new(),
            variables,
            line_number: 1,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        println!("Veld Language REPL v0.1.0");
        println!("Type 'exit' to quit, 'help' for commands");

        let mut input_buffer = String::new();
        let mut multi_line_mode = false;
        let mut current_indent = 0;

        loop {
            // Display appropriate prompt
            if multi_line_mode {
                let indent = " ".repeat(current_indent);
                print!("❧ {}", indent);
            } else {
                print!("𝕍[{}]> ", self.line_number);
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
                self.line_number += 1;

                if self.get_var("multiline_mode") == "block"
                    && !input_buffer.trim_end().ends_with("end")
                {
                    input_buffer.push_str("\nend");
                }

                let result = self.evaluate_input(
                    &input_buffer,
                    self.line_number - input_buffer.lines().count(),
                );
                self.handle_result(result);
                input_buffer.clear();
                current_indent = 0;
                continue;
            }

            if !multi_line_mode && line.starts_with(':') {
                self.handle_command(&line[1..].trim());
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
                "" if !multi_line_mode => continue,
                line => {
                    // Check for multi-line input
                    if line.ends_with('\\') {
                        let content = &line[..line.len() - 1];
                        input_buffer.push_str(content);
                        input_buffer.push('\n');
                        multi_line_mode = true;
                        continue;
                    }

                    // Track indentation for multi-line mode
                    if multi_line_mode {
                        if line.trim().starts_with("end") || line.trim().starts_with("else") {
                            current_indent = current_indent.saturating_sub(2);
                        }
                    }

                    // Add to current buffer
                    if multi_line_mode {
                        input_buffer.push_str(line);
                        input_buffer.push('\n');

                        // Adjust indentation for next line
                        if line.trim().ends_with("then")
                            || line.trim().ends_with("do")
                            || line.trim().ends_with("=")
                        {
                            current_indent += 2;
                        } else {
                            input_buffer.push_str(line)
                        }
                    }

                    // Check for syntax that suggests multi-line input
                    if !multi_line_mode && self.is_incomplete(&input_buffer) {
                        multi_line_mode = true;
                        continue;
                    }

                    // Process the input if not in multi-line mode
                    if !multi_line_mode {
                        // Add to history
                        self.history.push(input_buffer.clone());
                        self.line_number += 1;

                        // Evaluate and handle result - use line instead of input_buffer
                        let result = self.evaluate_input(&line, self.line_number - 1);
                        self.handle_result(result);
                        input_buffer.clear();
                    }
                }
            }
        }

        println!("Goodbye!");
        Ok(())
    }

    fn evaluate_input(
        &mut self,
        input: &str,
        start_line: usize,
    ) -> ContextResult<veld_core::interpreter::Value> {
        // If debug is enabled, show the input
        if self.get_var("debug") == "true" {
            println!("DEBUG: Evaluating input:\n{}", input);
        }

        // Lexical analysis
        let mut lexer = Lexer::new(input);
        let tokens_result = lexer.collect_tokens();

        let tokens = match tokens_result {
            Ok(tokens) => tokens,
            Err(e) => {
                // Create simplified context for lexical errors
                return Err(VeldError::LexerError(e).with_context(ErrorContext {
                    line: start_line,
                    column: 0, // TODO - implement getting column info from lexer
                    source_line: input.lines().next().unwrap_or("").to_string(),
                }));
            }
        };

        // Parsing
        let mut parser = Parser::new(tokens);
        let statements_result = parser.parse();

        let statements = match statements_result {
            Ok(statements) => statements,
            Err(e) => {
                // Add context to parser errors
                let line_offset = if let Some(pos) = parser.get_current_position() {
                    let mut line_count = 0;
                    let mut char_count = 0;
                    for line in input.lines() {
                        if char_count + line.len() + 1 > pos {
                            // Found the line
                            let column = pos - char_count;
                            return Err(e.with_context(ErrorContext {
                                line: start_line + line_count,
                                column,
                                source_line: line.to_string(),
                            }));
                        }
                        char_count += line.len() + 1; // +1 for newline
                        line_count += 1;
                    }
                    0
                } else {
                    0
                };

                // Fallback for if we can't determine position
                return Err(e.with_context(ErrorContext {
                    line: start_line,
                    column: 0,
                    source_line: input.lines().next().unwrap_or("").to_string(),
                }));
            }
        };

        // Interpolation
        match self.interpreter.interpret(statements) {
            Ok(result) => Ok(result),
            Err(e) => {
                // Add basic context to runtime errors
                Err(e.with_context(ErrorContext {
                    line: start_line,
                    column: 0, // Runtime errors may not have position info
                    source_line: input.lines().next().unwrap_or("").to_string(),
                }))
            }
        }
    }

    fn handle_result(
        &self,
        result: std::result::Result<veld_core::interpreter::Value, VeldErrorWithContext>,
    ) {
        match result {
            Ok(value) => {
                // Format the output nicely basely based on value type
                self.pretty_print_value(&value);
            }
            Err(e) => {
                // Print the formatted error with context
                println!("\x1B[31m{}\x1B[0m", e); // Red color
            }
        }
    }

    fn pretty_print_value(&self, value: &veld_core::interpreter::Value) {
        match value {
            veld_core::interpreter::Value::Unit => println!("=> ()"),
            veld_core::interpreter::Value::Integer(n) => println!("=> \x1B[33m{}\x1B[0m", n), // Yellow
            veld_core::interpreter::Value::Float(f) => println!("=> \x1B[33m{}\x1B[0m", f), // Yellow
            veld_core::interpreter::Value::Boolean(b) => println!("=> \x1B[35m{}\x1B[0m", b), // Magenta
            veld_core::interpreter::Value::String(s) => println!("=> \x1B[32m\"{}\"\x1B[0m", s), // Green
            veld_core::interpreter::Value::Function { .. } => {
                println!("=> \x1B[36m<function>\x1B[0m")
            } // Cyan
            veld_core::interpreter::Value::Array(elements) => {
                println!(
                    "=> \x1B[34m[{}]\x1B[0m", // Blue for arrays
                    elements
                        .iter()
                        .map(|v| self.value_to_string(v))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            veld_core::interpreter::Value::Struct { name, fields } => {
                println!(
                    "=> \x1B[36m{}:\x1B[0m {{\n    {}\n}}",
                    name,
                    fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, self.value_to_string(v)))
                        .collect::<Vec<_>>()
                        .join(",\n    ")
                );
            }
            veld_core::interpreter::Value::Tuple(elements) => {
                println!(
                    "=> \x1B[34m({})\x1B[0m", // Blue for tuples
                    elements
                        .iter()
                        .map(|v| self.value_to_string(v))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            veld_core::interpreter::Value::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                if fields.is_empty() {
                    println!("=> \x1B[35m{}.{}\x1B[0m", enum_name, variant_name);
                } else {
                    println!(
                        "=> \x1B[35m{}.{}({})\x1B[0m",
                        enum_name,
                        variant_name,
                        fields
                            .iter()
                            .map(|v| self.value_to_string(v))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
            }
            _ => println!("=> {:?}", value),
        }
    }
    fn value_to_string(&self, value: &veld_core::interpreter::Value) -> String {
        match value {
            veld_core::interpreter::Value::Integer(n) => format!("{}", n),
            veld_core::interpreter::Value::Float(f) => format!("{}", f),
            veld_core::interpreter::Value::String(s) => format!("\"{}\"", s),
            veld_core::interpreter::Value::Boolean(b) => format!("{}", b),
            veld_core::interpreter::Value::Unit => "()".to_string(),
            veld_core::interpreter::Value::Function { .. } => "<function>".to_string(),
            veld_core::interpreter::Value::Struct { name, .. } => format!("<{}>", name),
            veld_core::interpreter::Value::Array(elements) => {
                let items: Vec<String> = elements.iter().map(|e| self.value_to_string(e)).collect();
                format!("[{}]", items.join(", "))
            }
            _ => format!("{:?}", value),
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
        println!("Special commands (start with ':'):");
        println!("  :set var=value - Set a REPL variable");
        println!("  :debug on|off  - Toggle debug mode");
        println!("  :type [expr]   - Show type of expression");
    }

    fn show_history(&self) {
        println!("Command history:");
        for (i, cmd) in self.history.iter().enumerate() {
            println!("{}: {}", i + 1, cmd);
        }
    }

    fn is_incomplete(&self, input: &str) -> bool {
        // Simple heuristic - if a line ends with these tokens, it likely continues
        let trimmed = input.trim();
        trimmed.ends_with("=")
            || trimmed.ends_with("then")
            || trimmed.ends_with("do")
            || trimmed.ends_with("else")
            || (trimmed.contains("fn") && !trimmed.contains("end"))
    }

    fn handle_command(&mut self, cmd: &str) {
        let parts: Vec<&str> = cmd.splitn(2, ' ').collect();
        match parts[0] {
            "set" => {
                if parts.len() < 2 {
                    println!("Usage: :set var=value");
                    return;
                }

                let kv: Vec<&str> = parts[1].splitn(2, '=').collect();
                if kv.len() < 2 {
                    println!("Usage: :set var=value");
                    return;
                }

                let key = kv[0].trim();
                let value = kv[1].trim();
                self.variables.insert(key.to_string(), value.to_string());
                println!("Set {} = {}", key, value);
            }
            "debug" => {
                if parts.len() < 2 {
                    println!("Current debug setting: {}", self.get_var("debug"));
                    return;
                }

                match parts[1].trim() {
                    "on" | "true" => {
                        self.variables
                            .insert("debug".to_string(), "true".to_string());
                        println!("Debug mode enabled");
                    }
                    "off" | "false" => {
                        self.variables
                            .insert("debug".to_string(), "false".to_string());
                        println!("Debug mode disabled");
                    }
                    _ => println!("Usage: :debug on|off"),
                }
            }
            "type" => {
                println!("Type checking not yet implemented");
            }
            _ => println!("Unknown command: {}", cmd),
        }
    }

    fn get_var(&self, key: &str) -> String {
        self.variables.get(key).cloned().unwrap_or_default()
    }
}
