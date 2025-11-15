use std::collections::HashMap;
use veld_common::ast::{Argument, Expr, Literal, MacroPattern, MacroTemplate, Statement};
use veld_common::source::NodeId;

use veld_common::types::Type;

pub mod integration;
pub mod parser;

pub use integration::MacroSystem;

#[derive(Debug, Clone)]
pub enum ExpansionError {
    // Recursion and limits
    RecursionLimit {
        macro_name: String,
        call_site: NodeId,
    },
    ExpansionTooLarge {
        macro_name: String,
        generated_nodes: usize,
        limit: usize,
    },

    // Macro definition errors
    MacroNotFound {
        name: String,
        call_site: NodeId,
    },
    MacroRedefinition {
        name: String,
        original_def: NodeId,
        new_def: NodeId,
    },

    // Parameter/argument errors
    ArgumentCountMismatch {
        macro_name: String,
        expected: usize,
        got: usize,
        call_site: NodeId,
    },
    InvalidArgumentType {
        macro_name: String,
        param_name: String,
        expected: String,
        got: String,
        call_site: NodeId,
    },

    // Template/pattern errors
    UnboundVariable {
        name: String,
        macro_name: String,
        position: NodeId,
    },
    PatternMatchFailed {
        macro_name: String,
        expected_pattern: String,
        call_site: NodeId,
    },

    // AST/syntax errors in generated code
    InvalidSyntaxGenerated {
        macro_name: String,
        error: String,
        call_site: NodeId,
    },

    // Hygiene violations
    SymbolCollision {
        symbol: String,
        macro_name: String,
        call_site: NodeId,
    },

    // Format string errors
    InvalidFormatString {
        message: String,
    },
}

#[derive(Debug, Clone)]
pub enum MacroKind {
    // Rust-style: pattern matching with repetition
    Declarative {
        patterns: Vec<MacroPattern>,
        templates: Vec<MacroTemplate>,
    },

    // Nim-style: direct AST manipulation
    Template {
        typed: bool, // Can access type information
    },

    // Nim-style: compile-time code execution
    CompileTimeProc {
        return_type: Option<Type>,
    },
}

/// Additional metadata that can be added over time
#[derive(Debug, Clone, Default)]
pub struct MacroMetadata {
    pub description: Option<String>,
    pub examples: Vec<String>,
    pub deprecated: bool,
    pub stability: MacroStability,
    pub attributes: HashMap<String, String>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum MacroStability {
    #[default]
    Stable,
    Unstable,
    Experimental,
    Deprecated,
}

#[derive(Debug, Clone)]
pub struct ExpansionFrame {
    pub macro_name: String,
    pub call_site: NodeId,
    pub expansion_id: u32,
}

#[derive(Debug, Clone)]
pub struct MacroDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Statement>,
    pub node_id: NodeId,

    // NEW: Optional extensions for advanced macro types
    pub kind: Option<MacroKind>,
    pub metadata: MacroMetadata,
}

impl MacroDefinition {
    /// Create a simple macro (backward compatible)
    pub fn simple(
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
        node_id: NodeId,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            node_id,
            kind: None,
            metadata: MacroMetadata::default(),
        }
    }

    /// Create a declarative macro (new functionality)
    pub fn declarative(
        name: String,
        patterns: Vec<MacroPattern>,
        templates: Vec<MacroTemplate>,
        node_id: NodeId,
    ) -> Self {
        Self {
            name,
            parameters: Vec::new(), // Empty for declarative
            body: Vec::new(),       // Empty for declarative
            node_id,
            kind: Some(MacroKind::Declarative {
                patterns,
                templates,
            }),
            metadata: MacroMetadata::default(),
        }
    }

    /// Create a template macro (new functionality)
    pub fn template(
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
        node_id: NodeId,
        typed: bool,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            node_id,
            kind: Some(MacroKind::Template { typed }),
            metadata: MacroMetadata::default(),
        }
    }

    /// Create a compile-time procedure (new functionality)
    pub fn compile_time_proc(
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
        node_id: NodeId,
        return_type: Option<Type>,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            node_id,
            kind: Some(MacroKind::CompileTimeProc { return_type }),
            metadata: MacroMetadata::default(),
        }
    }

    /// Check if this is a simple macro (original behavior)
    pub fn is_simple(&self) -> bool {
        self.kind.is_none()
    }

    /// Check if this is a declarative macro
    pub fn is_declarative(&self) -> bool {
        matches!(self.kind, Some(MacroKind::Declarative { .. }))
    }

    /// Check if this is a template macro
    pub fn is_template(&self) -> bool {
        matches!(self.kind, Some(MacroKind::Template { .. }))
    }

    /// Check if this is a compile-time procedure
    pub fn is_compile_time_proc(&self) -> bool {
        matches!(self.kind, Some(MacroKind::CompileTimeProc { .. }))
    }

    /// Get the effective parameters (handles different macro types)
    pub fn effective_parameters(&self) -> &[String] {
        &self.parameters
    }

    /// Get the effective body (handles different macro types)
    pub fn effective_body(&self) -> &[Statement] {
        &self.body
    }

    /// Add metadata (builder pattern)
    pub fn with_description(mut self, description: String) -> Self {
        self.metadata.description = Some(description);
        self
    }

    /// Add example (builder pattern)
    pub fn with_example(mut self, example: String) -> Self {
        self.metadata.examples.push(example);
        self
    }

    /// Set stability (builder pattern)
    pub fn with_stability(mut self, stability: MacroStability) -> Self {
        self.metadata.stability = stability;
        self
    }

    /// Add custom attribute (builder pattern)
    pub fn with_attribute(mut self, key: String, value: String) -> Self {
        self.metadata.attributes.insert(key, value);
        self
    }
}

#[derive(Debug, Clone)]
pub struct PatternMatcher {
    bindings: HashMap<String, MacroFragment>,
}

#[derive(Debug, Clone)]
pub enum MacroFragment {
    Expression(Expr),
    Statement(Statement),
    Identifier(String),
    Literal(Literal),
    TokenSequence(Vec<String>),
    ExpressionList(Vec<Expr>),
    StatementList(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub enum PatternElement {
    Literal(String),
    Variable {
        name: String,
        fragment_type: FragmentType,
    },
    Repetition {
        pattern: Vec<PatternElement>,
        separator: Option<String>,
        min_count: usize,
        max_count: Option<usize>,
    },
    Optional(Vec<PatternElement>),
    Group(Vec<PatternElement>),
}

#[derive(Debug, Clone)]
pub enum FragmentType {
    Expr,      // $name:expr
    Stmt,      // $name:stmt
    Ident,     // $name:ident
    Literal,   // $name:literal
    TokenTree, // $name:tt
    Item,      // $name:item
    Block,     // $name:block
}

impl PatternMatcher {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn match_pattern(&mut self, pattern: &str, input: &[Expr]) -> Result<bool, ExpansionError> {
        let parsed_pattern = self.parse_pattern(pattern)?;
        self.match_elements(&parsed_pattern, input)
    }

    fn parse_pattern(&self, pattern: &str) -> Result<Vec<PatternElement>, ExpansionError> {
        // Simple pattern parser - in a real implementation this would be more sophisticated
        let mut elements = Vec::new();
        let mut chars = pattern.chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                '$' => {
                    // Parse variable: $name:type
                    let mut var_name = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            var_name.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    // Skip ':'
                    if chars.next() == Some(':') {
                        let mut frag_type = String::new();
                        while let Some(&ch) = chars.peek() {
                            if ch.is_alphabetic() {
                                frag_type.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }

                        let fragment_type = match frag_type.as_str() {
                            "expr" => FragmentType::Expr,
                            "stmt" => FragmentType::Stmt,
                            "ident" => FragmentType::Ident,
                            "literal" => FragmentType::Literal,
                            "tt" => FragmentType::TokenTree,
                            "item" => FragmentType::Item,
                            "block" => FragmentType::Block,
                            _ => FragmentType::TokenTree,
                        };

                        elements.push(PatternElement::Variable {
                            name: var_name,
                            fragment_type,
                        });
                    }
                }
                '(' => {
                    // Handle repetition patterns like $($elem:expr),+
                    let mut group_content = String::new();
                    let mut paren_count = 1;

                    while paren_count > 0 && chars.peek().is_some() {
                        let ch = chars.next().unwrap();
                        match ch {
                            '(' => paren_count += 1,
                            ')' => paren_count -= 1,
                            _ => {}
                        }
                        if paren_count > 0 {
                            group_content.push(ch);
                        }
                    }

                    // Check for repetition operators
                    let repetition_op = chars.peek().copied();
                    match repetition_op {
                        Some('+') | Some('*') | Some('?') => {
                            chars.next(); // consume the operator
                            let inner_pattern = self.parse_pattern(&group_content)?;
                            let (min_count, max_count) = match repetition_op.unwrap() {
                                '+' => (1, None),
                                '*' => (0, None),
                                '?' => (0, Some(1)),
                                _ => unreachable!(),
                            };

                            elements.push(PatternElement::Repetition {
                                pattern: inner_pattern,
                                separator: None, // TODO: parse separator
                                min_count,
                                max_count,
                            });
                        }
                        _ => {
                            let inner_pattern = self.parse_pattern(&group_content)?;
                            elements.push(PatternElement::Group(inner_pattern));
                        }
                    }
                }
                _ if ch.is_whitespace() => {
                    // Skip whitespace
                    continue;
                }
                _ => {
                    // Literal token
                    let mut literal = String::new();
                    literal.push(ch);
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            literal.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    elements.push(PatternElement::Literal(literal));
                }
            }
        }

        Ok(elements)
    }

    fn match_elements(
        &mut self,
        pattern: &[PatternElement],
        input: &[Expr],
    ) -> Result<bool, ExpansionError> {
        let mut input_pos = 0;
        let mut pattern_pos = 0;

        while pattern_pos < pattern.len() && input_pos < input.len() {
            match &pattern[pattern_pos] {
                PatternElement::Variable {
                    name,
                    fragment_type,
                } => {
                    let fragment = match fragment_type {
                        FragmentType::Expr => {
                            if input_pos < input.len() {
                                MacroFragment::Expression(input[input_pos].clone())
                            } else {
                                return Ok(false);
                            }
                        }
                        FragmentType::Ident => {
                            if let Expr::Identifier(ref ident) = input[input_pos] {
                                MacroFragment::Identifier(ident.clone())
                            } else {
                                return Ok(false);
                            }
                        }
                        FragmentType::Literal => {
                            if let Expr::Literal(ref lit) = input[input_pos] {
                                MacroFragment::Literal(lit.clone())
                            } else {
                                return Ok(false);
                            }
                        }
                        _ => MacroFragment::Expression(input[input_pos].clone()),
                    };

                    self.bindings.insert(name.clone(), fragment);
                    input_pos += 1;
                }
                PatternElement::Literal(expected) => {
                    // Match literal tokens - this is simplified
                    if let Expr::Identifier(ref actual) = input[input_pos] {
                        if actual == expected {
                            input_pos += 1;
                        } else {
                            return Ok(false);
                        }
                    } else {
                        return Ok(false);
                    }
                }
                PatternElement::Repetition {
                    pattern: rep_pattern,
                    min_count,
                    max_count,
                    ..
                } => {
                    let mut matched_count = 0;

                    // Try to match the repetition pattern as many times as possible
                    while input_pos < input.len() {
                        let old_pos = input_pos;
                        let old_bindings = self.bindings.clone();

                        if self.match_elements(rep_pattern, &input[input_pos..])? {
                            // Find how many input elements were consumed
                            // This is simplified - in reality we'd need better tracking
                            input_pos += 1; // Assume one element consumed
                            matched_count += 1;

                            if let Some(max) = max_count {
                                if matched_count >= *max {
                                    break;
                                }
                            }
                        } else {
                            // Restore bindings and position
                            self.bindings = old_bindings;
                            input_pos = old_pos;
                            break;
                        }
                    }

                    if matched_count < *min_count {
                        return Ok(false);
                    }
                }
                PatternElement::Optional(opt_pattern) => {
                    let old_pos = input_pos;
                    let old_bindings = self.bindings.clone();

                    if !self.match_elements(opt_pattern, &input[input_pos..])? {
                        // Optional didn't match, restore state
                        self.bindings = old_bindings;
                        input_pos = old_pos;
                    }
                }
                PatternElement::Group(group_pattern) => {
                    if !self.match_elements(group_pattern, &input[input_pos..])? {
                        return Ok(false);
                    }
                }
            }
            pattern_pos += 1;
        }

        Ok(pattern_pos == pattern.len() && input_pos == input.len())
    }

    pub fn get_binding(&self, name: &str) -> Option<&MacroFragment> {
        self.bindings.get(name)
    }
}

pub struct TemplateExpander {
    hygiene_counter: u32,
    current_expansion: Option<String>,
}

impl TemplateExpander {
    pub fn new() -> Self {
        Self {
            hygiene_counter: 0,
            current_expansion: None,
        }
    }

    pub fn expand_template(
        &mut self,
        template: &str,
        bindings: &HashMap<String, MacroFragment>,
    ) -> Result<Vec<Statement>, ExpansionError> {
        let mut result = Vec::new();

        // Parse template and substitute variables
        let expanded = self.substitute_variables(template, bindings)?;

        // For now, create a simple expression statement
        // In a real implementation, this would parse the expanded template
        result.push(Statement::ExprStatement(Expr::Literal(Literal::String(
            expanded,
        ))));

        Ok(result)
    }

    fn substitute_variables(
        &mut self,
        template: &str,
        bindings: &HashMap<String, MacroFragment>,
    ) -> Result<String, ExpansionError> {
        let mut result = String::new();
        let mut chars = template.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '$' {
                let mut var_name = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        var_name.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }

                if let Some(fragment) = bindings.get(&var_name) {
                    result.push_str(&self.fragment_to_string(fragment));
                } else {
                    return Err(ExpansionError::UnboundVariable {
                        name: var_name,
                        macro_name: self.current_expansion.clone().unwrap_or_default(),
                        position: NodeId::new(),
                    });
                }
            } else {
                result.push(ch);
            }
        }

        Ok(result)
    }

    fn fragment_to_string(&self, fragment: &MacroFragment) -> String {
        match fragment {
            MacroFragment::Expression(expr) => self.expr_to_string(expr),
            MacroFragment::Statement(stmt) => self.stmt_to_string(stmt),
            MacroFragment::Identifier(ident) => ident.clone(),
            MacroFragment::Literal(lit) => self.literal_to_string(lit),
            MacroFragment::TokenSequence(tokens) => tokens.join(" "),
            MacroFragment::ExpressionList(exprs) => exprs
                .iter()
                .map(|e| self.expr_to_string(e))
                .collect::<Vec<_>>()
                .join(", "),
            MacroFragment::StatementList(stmts) => stmts
                .iter()
                .map(|s| self.stmt_to_string(s))
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }

    fn expr_to_string(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => self.literal_to_string(lit),
            Expr::Identifier(name) => name.clone(),
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                format!(
                    "({} {} {})",
                    self.expr_to_string(left),
                    operator,
                    self.expr_to_string(right)
                )
            }
            Expr::UnaryOp { operator, operand } => {
                format!("({}{})", operator, self.expr_to_string(operand))
            }
            Expr::FunctionCall { name, arguments } => {
                let args = arguments
                    .iter()
                    .map(|arg| match arg {
                        Argument::Positional(expr) => self.expr_to_string(expr),
                        Argument::Named { name, value } => {
                            format!("{}: {}", name, self.expr_to_string(value))
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", name, args)
            }
            Expr::ArrayLiteral(exprs) => {
                let elements = exprs
                    .iter()
                    .map(|e| self.expr_to_string(e))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", elements)
            }
            _ => format!("expr"),
        }
    }

    fn stmt_to_string(&self, stmt: &Statement) -> String {
        match stmt {
            Statement::ExprStatement(expr) => self.expr_to_string(expr),
            Statement::VariableDeclaration { pattern, value, .. } => {
                let pattern_str = match pattern {
                    veld_common::ast::Pattern::Identifier(name) => name.clone(),
                    _ => "_".to_string(), // Fallback for complex patterns
                };
                format!("let {} = {}", pattern_str, self.expr_to_string(value))
            }
            _ => format!("stmt"),
        }
    }

    fn literal_to_string(&self, lit: &Literal) -> String {
        match lit {
            Literal::Integer(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Boolean(b) => b.to_string(),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Unit => "()".to_string(),
        }
    }

    pub fn make_hygienic(&mut self, name: &str) -> String {
        self.hygiene_counter += 1;
        format!("{}__hygiene_{}", name, self.hygiene_counter)
    }
}

/// ExpansionContext represents the context for macro expansion.
/// It contains information about the current state of macro expansion,
/// including the defined macros, the current expansion depth, the maximum depth,
/// the hygiene counter, the scoped bindings, and the expansion stack.
///
/// # Example:
/// ```
/// use veld_expander::ExpansionContext;
/// use veld_expander::MacroDefinition;
/// use veld_common::source::NodeId;
/// let mut context = ExpansionContext::new();
/// context.define_macro(MacroDefinition::declarative("foo".to_string(), vec![], vec![], NodeId::new())).unwrap();
/// assert_eq!(context.macros().len(), 1);
/// assert_eq!(context.expansion_depth(), 0);
/// assert_eq!(context.max_depth(), 256);
/// assert_eq!(context.hygiene_counter(), 0);
/// assert_eq!(context.scoped_bindings().len(), 1);
/// assert_eq!(context.expansion_stack().len(), 0);
/// ```
pub struct ExpansionContext {
    macros: HashMap<String, MacroDefinition>,
    expansion_depth: usize,
    max_depth: usize,
    hygiene_counter: u32,
    scoped_bindings: Vec<HashMap<String, String>>,
    expansion_stack: Vec<ExpansionFrame>,
}

impl ExpansionContext {
    /// Creates a new expansion context.
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            expansion_depth: 0,
            max_depth: 256,
            hygiene_counter: 0,
            scoped_bindings: vec![HashMap::new()],
            expansion_stack: Vec::new(),
        }
    }

    /// Returns the current expansion depth.
    pub fn expansion_depth(&self) -> usize {
        self.expansion_depth
    }

    /// Returns a reference to the expansion stack.
    pub fn expansion_stack(&self) -> &Vec<ExpansionFrame> {
        &self.expansion_stack
    }

    /// Returns a mutable reference to the expansion stack.
    pub fn expansion_stack_mut(&mut self) -> &mut Vec<ExpansionFrame> {
        &mut self.expansion_stack
    }

    /// Returns the current hygiene counter.
    pub fn hygiene_counter(&self) -> u32 {
        self.hygiene_counter
    }

    /// Returns a reference to the scoped bindings.
    pub fn scoped_bindings(&self) -> &Vec<HashMap<String, String>> {
        &self.scoped_bindings
    }

    /// Returns a mutable reference to the scoped bindings.
    pub fn scoped_bindings_mut(&mut self) -> &mut Vec<HashMap<String, String>> {
        &mut self.scoped_bindings
    }

    /// Returns a reference to the macros.
    pub fn macros(&self) -> &HashMap<String, MacroDefinition> {
        &self.macros
    }

    /// Returns a mutable reference to the macros.
    pub fn macros_mut(&mut self) -> &mut HashMap<String, MacroDefinition> {
        &mut self.macros
    }

    /// Returns the maximum expansion depth.
    pub fn max_depth(&self) -> usize {
        self.max_depth
    }

    /// Defines a new macro.
    pub fn define_macro(&mut self, def: MacroDefinition) -> Result<(), ExpansionError> {
        if let Some(existing) = self.macros.get(&def.name) {
            return Err(ExpansionError::MacroRedefinition {
                name: def.name,
                original_def: existing.node_id,
                new_def: def.node_id,
            });
        }

        self.macros.insert(def.name.clone(), def);
        Ok(())
    }

    /// Looks up a macro by name.
    pub fn lookup_macro(&self, name: &str) -> Option<&MacroDefinition> {
        self.macros.get(name)
    }

    /// Enters a new expansion context.
    pub fn enter_expansion(
        &mut self,
        macro_name: String,
        call_site: NodeId,
    ) -> Result<(), ExpansionError> {
        if self.expansion_depth >= self.max_depth {
            return Err(ExpansionError::RecursionLimit {
                macro_name,
                call_site,
            });
        }

        self.expansion_depth += 1;
        self.expansion_stack.push(ExpansionFrame {
            macro_name,
            call_site,
            expansion_id: self.hygiene_counter,
        });
        self.hygiene_counter += 1;

        Ok(())
    }

    /// Exits the current expansion context.
    pub fn exit_expansion(&mut self) {
        self.expansion_depth -= 1;
        self.expansion_stack.pop();
    }

    /// Expands a macro
    ///
    /// # Example:
    /// ```
    /// use veld_expander::ExpansionContext;
    /// use veld_expander::MacroDefinition;
    /// use veld_common::ast::Expr;
    /// use veld_common::source::NodeId;
    /// use veld_common::ast::Statement;
    /// use veld_common::ast::Literal;
    /// let mut expander = ExpansionContext::new();
    /// // Define a macro first before expanding it
    /// let macro_def = MacroDefinition::simple(
    ///     "my_macro".to_string(),
    ///     vec!["x".to_string(), "y".to_string()],
    ///     vec![Statement::ExprStatement(Expr::Literal(Literal::Integer(1)))],
    ///     NodeId::new()
    /// );
    /// expander.define_macro(macro_def).unwrap();
    /// let expanded = expander.expand_macro(
    ///     "my_macro",
    ///     &[Expr::Literal(Literal::String("x".to_string())), Expr::Literal(Literal::String("y".to_string()))],
    ///     NodeId::new(),
    /// );
    /// assert_eq!(expanded.unwrap(), vec![Statement::ExprStatement(Expr::Literal(Literal::Integer(1)))]);
    /// ```
    pub fn expand_macro(
        &mut self,
        name: &str,
        arguments: &[Expr],
        call_site: NodeId,
    ) -> Result<Vec<Statement>, ExpansionError> {
        let macro_def = self
            .lookup_macro(name)
            .ok_or_else(|| ExpansionError::MacroNotFound {
                name: name.to_string(),
                call_site,
            })?
            .clone();

        self.enter_expansion(name.to_string(), call_site)?;

        let result = match &macro_def.kind {
            None => self.expand_simple_macro(
                name,
                &macro_def.parameters,
                &macro_def.body,
                arguments,
                call_site,
            ),
            Some(MacroKind::Declarative {
                patterns,
                templates,
            }) => self.expand_declarative_macro(name, patterns, templates, arguments, call_site),
            Some(MacroKind::Template { typed: _, .. }) => self.expand_template_macro(
                name,
                &macro_def.parameters,
                &macro_def.body,
                arguments,
                call_site,
            ),
            Some(MacroKind::CompileTimeProc { .. }) => {
                // TODO: Implement compile-time procedure expansion
                Err(ExpansionError::InvalidSyntaxGenerated {
                    macro_name: name.to_string(),
                    error: "Compile-time procedures not yet implemented".to_string(),
                    call_site,
                })
            }
        };

        self.exit_expansion();
        result
    }

    /// Expands a simple macro.
    fn expand_simple_macro(
        &mut self,
        name: &str,
        parameters: &[String],
        body: &[Statement],
        arguments: &[Expr],
        call_site: NodeId,
    ) -> Result<Vec<Statement>, ExpansionError> {
        if parameters.len() != arguments.len() {
            return Err(ExpansionError::ArgumentCountMismatch {
                macro_name: name.to_string(),
                expected: parameters.len(),
                got: arguments.len(),
                call_site,
            });
        }

        // TODO: refactor to substitute the arguments into the body based on parameter names
        Ok(body.to_vec())
    }

    fn expand_declarative_macro(
        &mut self,
        name: &str,
        patterns: &[MacroPattern],
        templates: &[MacroTemplate],
        arguments: &[Expr],
        call_site: NodeId,
    ) -> Result<Vec<Statement>, ExpansionError> {
        // Try to match arguments against patterns
        for (pattern, template) in patterns.iter().zip(templates.iter()) {
            let mut matcher = PatternMatcher::new();

            if matcher.match_pattern(&pattern.0, arguments)? {
                // Pattern matched, expand the template
                let mut expander = TemplateExpander::new();
                expander.current_expansion = Some(name.to_string());

                return expander.expand_template(
                    &template
                        .expansion
                        .0
                        .iter()
                        .map(|stmt| format!("{:?}", stmt))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    &matcher.bindings,
                );
            }
        }

        Err(ExpansionError::PatternMatchFailed {
            macro_name: name.to_string(),
            expected_pattern: patterns.first().map(|p| p.0.clone()).unwrap_or_default(),
            call_site,
        })
    }

    fn expand_template_macro(
        &mut self,
        name: &str,
        parameters: &[String],
        body: &[Statement],
        arguments: &[Expr],
        call_site: NodeId,
    ) -> Result<Vec<Statement>, ExpansionError> {
        if parameters.len() != arguments.len() {
            return Err(ExpansionError::ArgumentCountMismatch {
                macro_name: name.to_string(),
                expected: parameters.len(),
                got: arguments.len(),
                call_site,
            });
        }

        // Create bindings for template parameters
        let mut bindings = HashMap::new();
        for (param, arg) in parameters.iter().zip(arguments.iter()) {
            bindings.insert(param.clone(), MacroFragment::Expression(arg.clone()));
        }

        // Expand the template body
        let mut expander = TemplateExpander::new();
        expander.current_expansion = Some(name.to_string());

        // TODO: refactof to substitute the bindings
        Ok(body.to_vec())
    }

    pub fn apply_hygiene(&mut self, statements: &[Statement]) -> Vec<Statement> {
        // TODO: Implement proper hygiene by renaming local variables
        // to avoid conflicts with the macro call site
        statements.to_vec()
    }
}

// Built-in macro definitions
impl ExpansionContext {
    pub fn with_builtin_macros() -> Self {
        let mut ctx = Self::new();

        // Add vec! macro (declarative)
        let vec_macro = MacroDefinition::declarative(
            "vec".to_string(),
            vec![
                MacroPattern("()".to_string()),
                MacroPattern("($($elem:expr),+ $(,)?)".to_string()),
            ],
            vec![
                MacroTemplate {
                    pattern: MacroPattern("()".to_string()),
                    expansion: veld_common::ast::MacroExpansion(vec![Statement::ExprStatement(
                        Expr::FunctionCall {
                            name: "Vec.new".to_string(),
                            arguments: vec![],
                        },
                    )]),
                },
                MacroTemplate {
                    pattern: MacroPattern("($($elem:expr),+ $(,)?)".to_string()),
                    expansion: veld_common::ast::MacroExpansion(vec![Statement::ExprStatement(
                        Expr::ArrayLiteral(vec![Expr::MacroVar("elem".to_string())]),
                    )]),
                },
            ],
            NodeId::new(),
        )
        .with_description("Creates a vector from elements".to_string())
        .with_example("vec~(1, 2, 3)".to_string())
        .with_stability(MacroStability::Stable);

        ctx.macros.insert("vec".to_string(), vec_macro);

        // Add format! macro (simple template)
        let format_macro = MacroDefinition::template(
            "format".to_string(),
            vec!["fmt".to_string()],
            vec![Statement::ExprStatement(Expr::FunctionCall {
                name: "format".to_string(),
                arguments: vec![],
            })],
            NodeId::new(),
            false, // not typed
        )
        .with_description("Formats a string with arguments".to_string())
        .with_example("format!(\"Hello {}\", name)".to_string())
        .with_stability(MacroStability::Stable);

        ctx.macros.insert("format".to_string(), format_macro);

        ctx
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_matching() {
        let mut matcher = PatternMatcher::new();
        let args = vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
        ];

        assert!(matcher.match_pattern("$a:expr", &args[0..1]).unwrap());
        assert_eq!(matcher.get_binding("a").is_some(), true);
    }

    #[test]
    fn test_macro_expansion() {
        let mut macro_system = crate::integration::MacroSystem::new();
        let args = vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ];

        let result = macro_system.expand_macro_call("vec", &args, NodeId::new());
        assert!(result.is_ok());
    }

    #[test]
    fn test_backward_compatibility() {
        // Test that the original simple macro structure still works
        let simple_macro = MacroDefinition::simple(
            "old_macro".to_string(),
            vec!["x".to_string()],
            vec![Statement::ExprStatement(Expr::Identifier("x".to_string()))],
            NodeId::new(),
        );

        assert!(simple_macro.is_simple());
        assert_eq!(simple_macro.effective_parameters(), &["x"]);
        assert!(simple_macro.metadata.description.is_none());
    }

    #[test]
    fn test_new_declarative_macros() {
        // Test new declarative macro functionality
        let vec_macro = MacroDefinition::declarative(
            "vec".to_string(),
            vec![MacroPattern("()".to_string())],
            vec![],
            NodeId::new(),
        )
        .with_description("Create a vector".to_string())
        .with_example("vec~(1, 2, 3)".to_string())
        .with_stability(MacroStability::Stable);

        assert!(vec_macro.is_declarative());
        assert!(!vec_macro.is_simple());
        assert_eq!(
            vec_macro.metadata.description,
            Some("Create a vector".to_string())
        );
        assert_eq!(
            vec_macro.metadata.examples,
            vec!["vec~(1, 2, 3)".to_string()]
        );
    }

    #[test]
    fn test_template_macros() {
        // Test template macro functionality
        let debug_macro = MacroDefinition::template(
            "debug".to_string(),
            vec!["expr".to_string()],
            vec![Statement::ExprStatement(Expr::Identifier(
                "expr".to_string(),
            ))],
            NodeId::new(),
            false, // not typed
        )
        .with_description("Debug print an expression".to_string())
        .with_stability(MacroStability::Experimental);

        assert!(debug_macro.is_template());
        assert!(!debug_macro.is_simple());
        assert_eq!(debug_macro.effective_parameters(), &["expr"]);
        assert_eq!(debug_macro.metadata.stability, MacroStability::Experimental);
    }
}
