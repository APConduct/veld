use serde_json::json;
use std::cell::RefCell;
use std::rc::Rc;
use veld_common::ast::Statement;
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_common::types::checker::TypeChecker;
use veld_error::VeldError;

/// Represents a diagnostic message (error, warning, etc.)
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
    pub message: String,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

/// Result of analyzing a document
#[derive(Clone)]
pub struct AnalysisResult {
    pub ast: Option<Vec<Statement>>,
    pub diagnostics: Vec<Diagnostic>,
    pub type_checker: Option<Rc<RefCell<TypeChecker>>>,
}

impl std::fmt::Debug for AnalysisResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AnalysisResult")
            .field(
                "ast",
                &self.ast.as_ref().map(|a| format!("{} statements", a.len())),
            )
            .field("diagnostics", &self.diagnostics)
            .field(
                "type_checker",
                &self.type_checker.as_ref().map(|_| "TypeChecker"),
            )
            .finish()
    }
}

/// The main analyzer that processes Veld source code
pub struct Analyzer {
    // Future: could cache type information, symbol tables, etc.
}

impl Analyzer {
    pub fn new() -> Self {
        Self {}
    }

    /// Analyze a Veld source file and return AST, diagnostics, and type information
    pub fn analyze(&self, source: &str) -> AnalysisResult {
        let mut diagnostics = Vec::new();

        // Step 1: Lex the source code
        let mut lexer = Lexer::new(source);
        let tokens = match lexer.collect_tokens() {
            Ok(tokens) => tokens,
            Err(e) => {
                // Lexer error - convert String error to VeldError
                let veld_error = VeldError::LexerError(e);
                diagnostics.push(self.error_to_diagnostic(&veld_error, source));
                return AnalysisResult {
                    ast: None,
                    diagnostics,
                    type_checker: None,
                };
            }
        };

        // Step 2: Parse tokens into AST
        let mut parser = Parser::new(tokens);
        let statements = match parser.parse() {
            Ok(stmts) => stmts,
            Err(e) => {
                // Parser error
                diagnostics.push(self.error_to_diagnostic(&e, source));
                return AnalysisResult {
                    ast: None,
                    diagnostics,
                    type_checker: None,
                };
            }
        };

        // Step 3: Type check the AST
        let mut type_checker = TypeChecker::new();
        if let Err(e) = type_checker.check_program(&statements) {
            // Type error
            diagnostics.push(self.error_to_diagnostic(&e, source));
        }

        AnalysisResult {
            ast: Some(statements),
            diagnostics,
            type_checker: Some(Rc::new(RefCell::new(type_checker))),
        }
    }

    /// Convert a VeldError to a Diagnostic
    fn error_to_diagnostic(&self, error: &VeldError, source: &str) -> Diagnostic {
        // Try to extract position information from the error
        let (line, column, message) = match error {
            VeldError::LexerError(msg) => {
                // Lexer errors might not have position info in the error itself
                // For now, point to the start of the file
                (0, 0, msg.clone())
            }
            VeldError::ParserError(msg) => {
                // Try to extract line/column from error message if present
                // Format is often like "Error at line X, column Y: ..."
                self.extract_position_from_message(msg)
            }
            VeldError::TypeError(msg) => self.extract_position_from_message(msg),
            VeldError::CompileError {
                message,
                line,
                column,
            } => {
                let l = line.unwrap_or(0);
                let c = column.unwrap_or(0);
                (l, c, message.clone())
            }
            _ => (0, 0, format!("{}", error)),
        };

        // Calculate end position (for now, just highlight the whole line)
        let end_line = line;
        let end_column = self.get_line_length(source, line).unwrap_or(column + 1);

        Diagnostic {
            line,
            column,
            end_line,
            end_column,
            message,
            severity: DiagnosticSeverity::Error,
        }
    }

    /// Try to extract position information from an error message
    fn extract_position_from_message(&self, msg: &str) -> (usize, usize, String) {
        // Look for patterns like "at line X, column Y" or "line X column Y"
        if let Some(line_pos) = msg.find("line ") {
            if let Some(line_end) = msg[line_pos + 5..].find(|c: char| !c.is_numeric()) {
                if let Ok(line) = msg[line_pos + 5..line_pos + 5 + line_end].parse::<usize>() {
                    // Found line, now look for column
                    if let Some(col_pos) = msg.find("column ") {
                        if let Some(col_end) = msg[col_pos + 7..].find(|c: char| !c.is_numeric()) {
                            if let Ok(col) =
                                msg[col_pos + 7..col_pos + 7 + col_end].parse::<usize>()
                            {
                                return (line.saturating_sub(1), col, msg.to_string());
                            }
                        }
                    }
                    return (line.saturating_sub(1), 0, msg.to_string());
                }
            }
        }
        (0, 0, msg.to_string())
    }

    /// Get the length of a specific line in the source
    fn get_line_length(&self, source: &str, line: usize) -> Option<usize> {
        source.lines().nth(line).map(|l| l.len())
    }

    /// Get hover information for a position in the source
    pub fn get_hover_info(
        &self,
        ast: &[Statement],
        type_checker: &Rc<RefCell<TypeChecker>>,
        line: usize,
        column: usize,
    ) -> Option<String> {
        // Find identifier at the given position
        let identifier = self.find_identifier_at_position(ast, line, column)?;

        // Look up the type in the type checker's environment
        let mut tc = type_checker.borrow_mut();
        if let Some(type_info) = tc.env().get(&identifier) {
            Some(format!("{}: {}", identifier, self.format_type(&type_info)))
        } else {
            Some(format!("{}: (type unknown)", identifier))
        }
    }

    /// Find an identifier at a specific position in the AST
    fn find_identifier_at_position(
        &self,
        statements: &[Statement],
        line: usize,
        column: usize,
    ) -> Option<String> {
        for stmt in statements {
            if let Some(id) = self.find_identifier_in_statement(stmt, line, column) {
                return Some(id);
            }
        }
        None
    }

    /// Search for identifiers in a statement
    fn find_identifier_in_statement(
        &self,
        statement: &Statement,
        _line: usize,
        _column: usize,
    ) -> Option<String> {
        use veld_common::ast::Statement;

        // For now, return the first identifier we find in the statement
        // A proper implementation would check position ranges
        match statement {
            Statement::VariableDeclaration { name, .. } => Some(name.clone()),
            Statement::FunctionDeclaration { name, .. } => Some(name.clone()),
            Statement::StructDeclaration { name, .. } => Some(name.clone()),
            Statement::TypeDeclaration { name, .. } => Some(name.clone()),
            _ => None,
        }
    }

    /// Format a type for display
    fn format_type(&self, type_info: &veld_common::types::Type) -> String {
        use veld_common::types::Type;

        match type_info {
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::F64 => "f64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "str".to_string(),
            Type::Char => "char".to_string(),
            Type::Unit => "()".to_string(),
            Type::IntegerLiteral(val) => format!("{}", val),
            Type::FloatLiteral(val) => format!("{}", val),
            Type::Number => "Number".to_string(),
            Type::Function {
                params,
                return_type,
            } => {
                let param_str = params
                    .iter()
                    .map(|p| self.format_type(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", param_str, self.format_type(return_type))
            }
            Type::GenericFunction {
                generic_params,
                params,
                return_type,
            } => {
                let generics = generic_params.join(", ");
                let param_str = params
                    .iter()
                    .map(|p| self.format_type(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "fn<{}>({}) -> {}",
                    generics,
                    param_str,
                    self.format_type(return_type)
                )
            }
            Type::Struct { name, .. } => name.clone(),
            Type::Record { fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.format_type(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {} }}", fields_str)
            }
            Type::Enum { name, .. } => name.clone(),
            Type::Union { variants } => {
                let variants_str = variants
                    .iter()
                    .map(|t| self.format_type(t))
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("({})", variants_str)
            }
            Type::Generic { base, type_args } => {
                if type_args.is_empty() {
                    base.clone()
                } else {
                    let args_str = type_args
                        .iter()
                        .map(|t| self.format_type(t))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", base, args_str)
                }
            }
            Type::TypeParam(name) => name.clone(),
            Type::Array(elem_type) => format!("[{}]", self.format_type(elem_type)),
            Type::Tuple(types) => {
                let types_str = types
                    .iter()
                    .map(|t| self.format_type(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", types_str)
            }
            Type::Any => "any".to_string(),
            Type::TypeVar(id) => format!("T{}", id),
            Type::GcRef(inner) => format!("GcRef<{}>", self.format_type(inner)),
            Type::KindSelf(kind) => format!("Self ({})", kind),
            Type::Module(name) => format!("module {}", name),
            Type::StructType(name) => format!("struct {}", name),
            Type::EnumType(name) => format!("enum {}", name),
        }
    }

    /// Find the definition location for a symbol at the given position
    pub fn find_definition(
        &self,
        ast: &[Statement],
        line: usize,
        column: usize,
    ) -> Option<(usize, usize)> {
        // Find identifier at the given position
        let identifier = self.find_identifier_at_position(ast, line, column)?;

        // Search for the definition of this identifier in the AST
        self.find_symbol_definition(ast, &identifier)
    }

    /// Search for where a symbol is defined in the AST
    fn find_symbol_definition(
        &self,
        statements: &[Statement],
        name: &str,
    ) -> Option<(usize, usize)> {
        use veld_common::ast::Statement;

        for (idx, stmt) in statements.iter().enumerate() {
            match stmt {
                Statement::VariableDeclaration { name: var_name, .. } if var_name == name => {
                    // Return line number (using statement index as approximation)
                    return Some((idx, 0));
                }
                Statement::FunctionDeclaration { name: fn_name, .. } if fn_name == name => {
                    return Some((idx, 0));
                }
                Statement::StructDeclaration {
                    name: struct_name, ..
                } if struct_name == name => {
                    return Some((idx, 0));
                }
                Statement::TypeDeclaration {
                    name: type_name, ..
                } if type_name == name => {
                    return Some((idx, 0));
                }
                Statement::EnumDeclaration {
                    name: enum_name, ..
                } if enum_name == name => {
                    return Some((idx, 0));
                }
                // TODO: Search within nested blocks and scopes
                _ => {}
            }
        }
        None
    }

    /// Get completion suggestions for a position
    pub fn get_completions(
        &self,
        ast: &[Statement],
        type_checker: &Rc<RefCell<TypeChecker>>,
        _line: usize,
        _column: usize,
    ) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Add keywords
        completions.extend(self.get_keyword_completions());

        // Add symbols from the current scope
        completions.extend(self.get_symbol_completions(ast, type_checker));

        completions
    }

    /// Get keyword completions
    fn get_keyword_completions(&self) -> Vec<CompletionItem> {
        vec![
            CompletionItem {
                label: "fn".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Function declaration".to_string()),
                documentation: Some("Define a new function".to_string()),
            },
            CompletionItem {
                label: "let".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Variable binding".to_string()),
                documentation: Some("Create an immutable variable".to_string()),
            },
            CompletionItem {
                label: "var".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Mutable variable".to_string()),
                documentation: Some("Create a mutable variable".to_string()),
            },
            CompletionItem {
                label: "struct".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Struct declaration".to_string()),
                documentation: Some("Define a new struct type".to_string()),
            },
            CompletionItem {
                label: "enum".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Enum declaration".to_string()),
                documentation: Some("Define a new enum type".to_string()),
            },
            CompletionItem {
                label: "impl".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Implementation block".to_string()),
                documentation: Some("Implement methods for a type".to_string()),
            },
            CompletionItem {
                label: "match".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Pattern matching".to_string()),
                documentation: Some("Match on a value".to_string()),
            },
            CompletionItem {
                label: "if".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Conditional".to_string()),
                documentation: Some("Conditional expression".to_string()),
            },
            CompletionItem {
                label: "else".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Else branch".to_string()),
                documentation: Some("Alternative branch for if".to_string()),
            },
            CompletionItem {
                label: "while".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("While loop".to_string()),
                documentation: Some("Loop while condition is true".to_string()),
            },
            CompletionItem {
                label: "for".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("For loop".to_string()),
                documentation: Some("Iterate over a collection".to_string()),
            },
            CompletionItem {
                label: "return".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Return statement".to_string()),
                documentation: Some("Return a value from a function".to_string()),
            },
            CompletionItem {
                label: "true".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Boolean true".to_string()),
                documentation: None,
            },
            CompletionItem {
                label: "false".to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Boolean false".to_string()),
                documentation: None,
            },
        ]
    }

    /// Get symbol completions from the AST and type checker
    fn get_symbol_completions(
        &self,
        ast: &[Statement],
        type_checker: &Rc<RefCell<TypeChecker>>,
    ) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Add top-level declarations from AST
        for stmt in ast {
            match stmt {
                Statement::FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    let param_str = params
                        .iter()
                        .map(|(param_name, _)| param_name.clone())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let return_str = format!(" -> {:?}", return_type);
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: CompletionKind::Function,
                        detail: Some(format!("fn({}){}", param_str, return_str)),
                        documentation: None,
                    });
                }
                Statement::StructDeclaration { name, .. } => {
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: CompletionKind::Struct,
                        detail: Some("struct".to_string()),
                        documentation: None,
                    });
                }
                Statement::TypeDeclaration { name, .. } => {
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: CompletionKind::Enum,
                        detail: Some("type".to_string()),
                        documentation: None,
                    });
                }
                Statement::EnumDeclaration { name, .. } => {
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: CompletionKind::Enum,
                        detail: Some("enum".to_string()),
                        documentation: None,
                    });
                }
                Statement::VariableDeclaration { name, .. } => {
                    // Try to get type from type checker
                    let mut tc = type_checker.borrow_mut();
                    let type_str = if let Some(ty) = tc.env().get(name) {
                        format!(": {}", self.format_type(&ty))
                    } else {
                        String::new()
                    };
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: CompletionKind::Variable,
                        detail: Some(format!("let{}", type_str)),
                        documentation: None,
                    });
                }
                _ => {}
            }
        }

        completions
    }
}

#[derive(Debug, Clone)]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionKind,
    pub detail: Option<String>,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum CompletionKind {
    Function = 3,
    Variable = 6,
    Struct = 22,
    Enum = 13,
    Keyword = 14,
}

/// Convert diagnostics to LSP format
pub fn diagnostics_to_lsp(uri: &str, diagnostics: &[Diagnostic]) -> serde_json::Value {
    let lsp_diagnostics: Vec<_> = diagnostics
        .iter()
        .map(|d| {
            json!({
                "range": {
                    "start": {
                        "line": d.line,
                        "character": d.column
                    },
                    "end": {
                        "line": d.end_line,
                        "character": d.end_column
                    }
                },
                "severity": d.severity as i32,
                "source": "veld",
                "message": d.message
            })
        })
        .collect();

    json!({
        "jsonrpc": "2.0",
        "method": "textDocument/publishDiagnostics",
        "params": {
            "uri": uri,
            "diagnostics": lsp_diagnostics
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analyzer_valid_code() {
        let analyzer = Analyzer::new();
        let result = analyzer.analyze("let x = 42");
        assert!(result.ast.is_some());
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn test_analyzer_syntax_error() {
        let analyzer = Analyzer::new();
        let result = analyzer.analyze("let x =");
        assert!(result.ast.is_none());
        assert!(!result.diagnostics.is_empty());
    }

    #[test]
    fn test_position_extraction() {
        let analyzer = Analyzer::new();
        let (line, col, _) =
            analyzer.extract_position_from_message("Error at line 5, column 10: something");
        assert_eq!(line, 4); // 0-indexed
        assert_eq!(col, 10);
    }

    #[test]
    fn test_get_line_length() {
        let analyzer = Analyzer::new();
        let source = "line1\nline2 is longer\nline3";
        assert_eq!(analyzer.get_line_length(source, 0), Some(5));
        assert_eq!(analyzer.get_line_length(source, 1), Some(15));
        assert_eq!(analyzer.get_line_length(source, 2), Some(5));
    }
}
