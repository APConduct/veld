use colored::Colorize;
use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline, Signal};
use std::collections::HashMap;
use veld_core::error::{ContextResult, ErrorContext, VeldError};
use veld_core::interpreter::Interpreter;
use veld_core::lexer::Lexer;
use veld_core::parser::Parser;

type CommandFn = fn(&mut Repl, &[&str]) -> anyhow::Result<()>;

pub struct Repl {
    interpreter: Interpreter,
    pub commands: HashMap<String, CommandFn>,
    pub history: Vec<String>,
    pub block_stack: Vec<String>,
    pub input_buffer: String,
    variables: HashMap<String, String>,
    line_number: usize,
}

impl Repl {
    pub fn new() -> Self {
        let mut commands = HashMap::new();
        commands.insert("help".to_string(), Self::cmd_help as CommandFn);
        commands.insert("exit".to_string(), Self::cmd_exit as CommandFn);
        commands.insert("quit".to_string(), Self::cmd_exit as CommandFn);
        commands.insert("history".to_string(), Self::show_history as CommandFn);
        commands.insert("vars".to_string(), Self::cmd_vars as CommandFn);
        commands.insert("type".to_string(), Self::cmd_type as CommandFn);

        let mut variables = HashMap::new();
        // variables.insert("multiline_mode".to_string(), "block".to_string());
        variables.insert("debug".to_string(), "false".to_string());

        Self {
            interpreter: Interpreter::new("../.."),
            history: Vec::new(),
            variables,
            line_number: 1,
            commands,
            block_stack: Vec::new(),
            input_buffer: String::new(),
        }
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        tracing::span!(tracing::Level::TRACE, "repl run");
        let mut editor = Reedline::create();

        tracing::info!("{}", "Veld Language REPL v0.2.0".bright_blue());
        tracing::info!(
            "{}",
            "Type ':help' for commands, ':exit' to quit.".bright_yellow()
        );

        loop {
            let prompt_str = self.make_prompt();
            let signal = editor.read_line(&prompt_str);

            let line = match signal {
                Ok(Signal::Success(input)) => input,
                Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => break,
                Err(e) => {
                    println!("Input error: {e}");
                    continue;
                }
            };

            let trimmed = line.trim();

            // Command handling
            if trimmed.starts_with(':') {
                let mut parts = trimmed[1..].split_whitespace();
                if let Some(cmd) = parts.next() {
                    let args: Vec<&str> = parts.collect();
                    if let Some(handler) = self.commands.get(cmd) {
                        if let Err(e) = handler(self, &args) {
                            println!("\x1B[31mError: {e}\x1B[0m");
                        }
                        if cmd == "exit" || cmd == "quit" {
                            break;
                        }
                    } else {
                        println!("Unknown command: {cmd}");
                    }
                }
                continue;
            }

            // Multiline/block support
            if !self.block_stack.is_empty() || self.is_block_start(trimmed) {
                self.input_buffer.push_str(&line);
                self.input_buffer.push('\n');
                self.update_block_stack(trimmed);

                if self.block_stack.is_empty() {
                    // Block complete, evaluate
                    let code = std::mem::take(&mut self.input_buffer);
                    self.evaluate_and_print(&code);
                    self.history.push(code);
                    self.line_number += 1;
                }
                continue;
            }

            // Single-line input
            if trimmed.is_empty() {
                continue;
            }
            self.history.push(line.clone());
            self.line_number += 1;
            self.evaluate_and_print(&line);
        }

        println!("Goodbye!");
        Ok(())
    }

    fn make_prompt(&self) -> DefaultPrompt {
        if !self.block_stack.is_empty() {
            let indent = "  ".repeat(self.block_stack.len());
            DefaultPrompt::new(
                DefaultPromptSegment::Basic(format!("❧ ❧ ❧{} ", indent)),
                DefaultPromptSegment::Empty,
            )
        } else {
            DefaultPrompt::new(
                DefaultPromptSegment::Basic(format!("𝕍[{}]> ", self.line_number)),
                DefaultPromptSegment::Empty,
            )
        }
    }

    fn is_block_start(&self, line: &str) -> bool {
        // Simple heuristic: lines ending with do/then/else/=/{
        let trimmed = line.trim_end();
        trimmed.ends_with("do")
            || trimmed.ends_with("then")
            || trimmed.ends_with("=")
            || trimmed.ends_with("{")
            || trimmed.starts_with("mod ")
            || trimmed.starts_with("struct ")
            || trimmed.starts_with("fn ")
            || trimmed.starts_with("proc ")
            || trimmed.starts_with("kind ")
            || trimmed.starts_with("impl ")
            || trimmed.starts_with("enum ")
    }

    fn update_block_stack(&mut self, line: &str) {
        // Naive: push for block openers, pop for 'end' or '}'
        let trimmed = line.trim();
        if trimmed.ends_with("do")
            || trimmed.ends_with("then")
            || trimmed.ends_with("=")
            || trimmed.ends_with("{")
        {
            self.block_stack.push(trimmed.to_string());
        }
        if trimmed == "end" || trimmed == "}" {
            self.block_stack.pop();
        }
    }

    fn evaluate_and_print(&mut self, code: &str) {
        match self.evaluate_input(code, self.line_number) {
            Ok(val) => self.pretty_print_value(&val),
            Err(e) => println!("\x1B[31m{e}\x1B[0m"),
        }
    }

    fn pretty_print_value(&self, value: &veld_core::interpreter::Value) {
        use veld_core::interpreter::Value::*;
        match value {
            Unit => println!("=> ()"),
            Integer(n) => println!("=> \x1B[33m{n}\x1B[0m"),
            Float(f) => println!("=> \x1B[33m{f}\x1B[0m"),
            Boolean(b) => println!("=> \x1B[35m{b}\x1B[0m"),
            String(s) => println!("=> \x1B[32m\"{s}\"\x1B[0m"),
            Function { .. } => println!("=> \x1B[36m<function>\x1B[0m"),
            Array(elements) => {
                let items = elements
                    .iter()
                    .map(|v| format!("{:?}", v))
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("=> \x1B[34m[{items}]\x1B[0m");
            }
            Struct { name, fields } => {
                let items = fields
                    .iter()
                    .map(|(k, v)| format!("{k}: {:?}", v))
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("=> \x1B[36m{name} {{ {items} }}\x1B[0m");
            }
            Tuple(elements) => {
                let items = elements
                    .iter()
                    .map(|v| format!("{:?}", v))
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("=> \x1B[34m({items})\x1B[0m");
            }
            Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                if fields.is_empty() {
                    println!("=> \x1B[35m{enum_name}.{variant_name}\x1B[0m");
                } else {
                    let items = fields
                        .iter()
                        .map(|v| format!("{:?}", v))
                        .collect::<Vec<_>>()
                        .join(", ");
                    println!("=> \x1B[35m{enum_name}.{variant_name}({items})\x1B[0m");
                }
            }
            _ => println!("=> {:?}", value),
        }
    }

    fn cmd_help(&mut self, _args: &[&str]) -> anyhow::Result<()> {
        println!("Available commands:");
        for cmd in self.commands.keys() {
            println!("  :{cmd}");
        }
        println!("Type code to evaluate it. Multiline blocks are supported.");
        Ok(())
    }

    fn cmd_exit(&mut self, _args: &[&str]) -> anyhow::Result<()> {
        println!("Exiting REPL...");
        Ok(())
    }

    fn cmd_vars(&mut self, _args: &[&str]) -> anyhow::Result<()> {
        // Introspect the current scope of the interpreter and print variables
        let scope = self.interpreter.current_scope_mut();
        // Use a public API to get variable names and values, since `values` is private.
        let vars = scope
            .values() //
            .iter()
            .map(|(name, value)| (name.clone(), value.clone()))
            .collect::<HashMap<_, _>>();
        if vars.is_empty() {
            println!("(No variables in current scope)");
        } else {
            println!("Variables in current scope:");
            for (name, value) in vars {
                // Since get_value_type is private, just print the value using value_to_string
                println!("  {name} = {}", self.value_to_string(&value));
            }
        }
        Ok(())
    }

    fn cmd_type(&mut self, args: &[&str]) -> anyhow::Result<()> {
        let expr = args.join(" ");
        if expr.is_empty() {
            println!("Usage: :type <expression>");
            return Ok(());
        }

        // Lex and parse the expression as a single expression statement
        let input = expr.as_str();
        let mut lexer = Lexer::new(input);
        let tokens_result = lexer.collect_tokens();

        let tokens = match tokens_result {
            Ok(tokens) => tokens,
            Err(e) => {
                println!("\x1B[31mLexer error: {e}\x1B[0m");
                return Ok(());
            }
        };

        let mut parser = Parser::new(tokens);
        let expr_result = parser.expression();

        let _expr_ast = match expr_result {
            Ok(expr) => expr,
            Err(e) => {
                println!("\x1B[31mParse error: {e}\x1B[0m");
                return Ok(());
            }
        };

        // TODO: Use the interpreter's type checker to infer the type
        // This is a placeholder; actual type inference logic should be implemented here.
        // For now, just print a message.
        println!("Type checking not yet implemented for this expression.");

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
                let _line_offset = if let Some(pos) = parser.get_current_position() {
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

    // fn handle_result(
    //     &self,
    //     result: std::result::Result<veld_core::interpreter::Value, VeldErrorWithContext>,
    // ) {
    //     match result {
    //         Ok(value) => {
    //             // Format the output nicely basely based on value type
    //             self.pretty_print_value(&value);
    //         }
    //         Err(e) => {
    //             // Print the formatted error with context
    //             println!("\x1B[31m{}\x1B[0m", e); // Red color
    //         }
    //     }
    // }

    // fn pretty_print_value(&self, value: &veld_core::interpreter::Value) {
    //     match value {
    //         veld_core::interpreter::Value::Unit => println!("=> ()"),
    //         veld_core::interpreter::Value::Integer(n) => println!("=> \x1B[33m{}\x1B[0m", n), // Yellow
    //         veld_core::interpreter::Value::Float(f) => println!("=> \x1B[33m{}\x1B[0m", f), // Yellow
    //         veld_core::interpreter::Value::Boolean(b) => println!("=> \x1B[35m{}\x1B[0m", b), // Magenta
    //         veld_core::interpreter::Value::String(s) => println!("=> \x1B[32m\"{}\"\x1B[0m", s), // Green
    //         veld_core::interpreter::Value::Function { .. } => {
    //             println!("=> \x1B[36m<function>\x1B[0m")
    //         } // Cyan
    //         veld_core::interpreter::Value::Array(elements) => {
    //             println!(
    //                 "=> \x1B[34m[{}]\x1B[0m", // Blue for arrays
    //                 elements
    //                     .iter()
    //                     .map(|v| self.value_to_string(v))
    //                     .collect::<Vec<_>>()
    //                     .join(", ")
    //             );
    //         }
    //         veld_core::interpreter::Value::Struct { name, fields } => {
    //             println!(
    //                 "=> \x1B[36m{}:\x1B[0m {{\n    {}\n}}",
    //                 name,
    //                 fields
    //                     .iter()
    //                     .map(|(k, v)| format!("{}: {}", k, self.value_to_string(v)))
    //                     .collect::<Vec<_>>()
    //                     .join(",\n    ")
    //             );
    //         }
    //         veld_core::interpreter::Value::Tuple(elements) => {
    //             println!(
    //                 "=> \x1B[34m({})\x1B[0m", // Blue for tuples
    //                 elements
    //                     .iter()
    //                     .map(|v| self.value_to_string(v))
    //                     .collect::<Vec<_>>()
    //                     .join(", ")
    //             );
    //         }
    //         veld_core::interpreter::Value::Enum {
    //             enum_name,
    //             variant_name,
    //             fields,
    //         } => {
    //             if fields.is_empty() {
    //                 println!("=> \x1B[35m{}.{}\x1B[0m", enum_name, variant_name);
    //             } else {
    //                 println!(
    //                     "=> \x1B[35m{}.{}({})\x1B[0m",
    //                     enum_name,
    //                     variant_name,
    //                     fields
    //                         .iter()
    //                         .map(|v| self.value_to_string(v))
    //                         .collect::<Vec<_>>()
    //                         .join(", ")
    //                 );
    //             }
    //         }
    //         _ => println!("=> {:?}", value),
    //     }
    // }
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

    // fn show_help(&self) {
    //     println!("Available commands:");
    //     println!("  exit, quit - Exit the REPL");
    //     println!("  help       - Show this help message");
    //     println!("  history    - Show command history");
    //     println!("  clear      - Clear the screen");
    //     println!("  [code]\\    - Continue code on next line");
    //     println!("  <empty>    - End multi-line input");
    //     println!("Special commands (start with ':'):");
    //     println!("  :set var=value - Set a REPL variable");
    //     println!("  :debug on|off  - Toggle debug mode");
    //     println!("  :type [expr]   - Show type of expression");
    // }

    fn show_history(&mut self, _arg: &[&str]) -> anyhow::Result<()> {
        if !self.history.is_empty() {
            println!("Command history:");
        } else {
            println!("No command history.");
        }
        for (i, cmd) in self.history.iter().enumerate() {
            println!("{}: {}", i + 1, cmd);
        }
        Ok(())
    }

    // fn is_incomplete(&self, input: &str) -> bool {
    //     // Simple heuristic - if a line ends with these tokens, it likely continues
    //     let trimmed = input.trim();
    //     trimmed.ends_with("=")
    //         || trimmed.ends_with("then")
    //         || trimmed.ends_with("do")
    //         || trimmed.ends_with("else")
    //         || (trimmed.contains("fn") && !trimmed.contains("end"))
    // }

    // fn handle_command(&mut self, cmd: &str) {
    //     let parts: Vec<&str> = cmd.splitn(2, ' ').collect();
    //     match parts[0] {
    //         "set" => {
    //             if parts.len() < 2 {
    //                 println!("Usage: :set var=value");
    //                 return;
    //             }

    //             let kv: Vec<&str> = parts[1].splitn(2, '=').collect();
    //             if kv.len() < 2 {
    //                 println!("Usage: :set var=value");
    //                 return;
    //             }

    //             let key = kv[0].trim();
    //             let value = kv[1].trim();
    //             self.variables.insert(key.to_string(), value.to_string());
    //             println!("Set {} = {}", key, value);
    //         }
    //         "debug" => {
    //             if parts.len() < 2 {
    //                 println!("Current debug setting: {}", self.get_var("debug"));
    //                 return;
    //             }

    //             match parts[1].trim() {
    //                 "on" | "true" => {
    //                     self.variables
    //                         .insert("debug".to_string(), "true".to_string());
    //                     println!("Debug mode enabled");
    //                 }
    //                 "off" | "false" => {
    //                     self.variables
    //                         .insert("debug".to_string(), "false".to_string());
    //                     println!("Debug mode disabled");
    //                 }
    //                 _ => println!("Usage: :debug on|off"),
    //             }
    //         }
    //         "type" => {
    //             println!("Type checking not yet implemented");
    //         }
    //         _ => println!("Unknown command: {}", cmd),
    //     }
    // }

    fn get_var(&self, key: &str) -> String {
        self.variables.get(key).cloned().unwrap_or_default()
    }
}
