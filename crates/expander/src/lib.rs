use std::collections::HashMap;

use veld_core::{
    ast::{Expr, GenericArgument, MacroPattern, MacroTemplate, Statement},
    common::source::NodeId,
    types::Type,
};

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
        params: Vec<GenericArgument>,
        body: Vec<Statement>,
        typed: bool, // Can access type information
    },

    // Nim-style: compile-time code execution
    CompileTimeProc {
        params: Vec<Expr>,
        body: Vec<Statement>,
        return_type: Option<Type>,
    },
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
}

pub struct ExpansionContext {
    macros: HashMap<String, MacroDefinition>,
    expansion_depth: usize,
    max_depth: usize,
    hygiene_counter: u32,
    scoped_bindings: Vec<HashMap<String, String>>,
    expansion_stack: Vec<ExpansionFrame>,
}

impl ExpansionContext {
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

    pub fn define_macro(&mut self, def: MacroDefinition) {
        self.macros.insert(def.name.clone(), def);
    }

    pub fn lookup_macro(&self, name: &str) -> Option<&MacroDefinition> {
        self.macros.get(name)
    }

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

    pub fn exit_expansion(&mut self) {
        self.expansion_depth -= 1;
        self.expansion_stack.pop();
    }
}
