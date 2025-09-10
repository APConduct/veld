//! Evolutionary design approach for MacroDefinition
//! This shows how we could have extended the original design rather than replacing it

use std::collections::HashMap;
use veld_core::{
    ast::{Expr, GenericArgument, MacroPattern, MacroTemplate, Statement},
    common::source::NodeId,
    types::Type,
};

/// Original MacroDefinition structure (preserved)
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

/// Extended macro capabilities (opt-in)
#[derive(Debug, Clone)]
pub enum MacroKind {
    /// Rust-style declarative macros with pattern matching
    Declarative {
        patterns: Vec<MacroPattern>,
        templates: Vec<MacroTemplate>,
    },

    /// Nim-style template macros
    Template {
        typed: bool, // Can access type information
    },

    /// Compile-time code execution
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

#[derive(Debug, Clone, Default)]
pub enum MacroStability {
    #[default]
    Stable,
    Unstable,
    Experimental,
    Deprecated,
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
            kind: Some(MacroKind::Declarative { patterns, templates }),
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

/// Migration utilities for existing code
impl From<LegacyMacroDefinition> for MacroDefinition {
    fn from(legacy: LegacyMacroDefinition) -> Self {
        Self::simple(legacy.name, legacy.parameters, legacy.body, legacy.node_id)
    }
}

/// The original structure for migration purposes
#[derive(Debug, Clone)]
pub struct LegacyMacroDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Statement>,
    pub node_id: NodeId,
}

/// Extension trait for backward compatibility
pub trait MacroDefinitionExt {
    /// Original behavior preserved
    fn expand_simple(&self, args: &[Expr]) -> Result<Vec<Statement>, String>;

    /// New capabilities
    fn expand_advanced(&self, args: &[Expr]) -> Result<Vec<Statement>, String>;
}

impl MacroDefinitionExt for MacroDefinition {
    fn expand_simple(&self, args: &[Expr]) -> Result<Vec<Statement>, String> {
        // Original simple expansion logic
        if args.len() != self.parameters.len() {
            return Err(format!(
                "Macro {} expects {} arguments, got {}",
                self.name,
                self.parameters.len(),
                args.len()
            ));
        }

        // Simple substitution (original behavior)
        Ok(self.body.clone())
    }

    fn expand_advanced(&self, args: &[Expr]) -> Result<Vec<Statement>, String> {
        match &self.kind {
            None => self.expand_simple(args), // Fall back to original behavior
            Some(MacroKind::Declarative { patterns, templates }) => {
                // Advanced pattern matching
                todo!("Implement declarative macro expansion")
            }
            Some(MacroKind::Template { typed }) => {
                // Template expansion
                if *typed {
                    todo!("Implement typed template expansion")
                } else {
                    todo!("Implement untyped template expansion")
                }
            }
            Some(MacroKind::CompileTimeProc { return_type }) => {
                // Compile-time execution
                todo!("Implement compile-time procedure execution")
            }
        }
    }
}

/// Usage examples showing backward compatibility and new features
#[cfg(test)]
mod examples {
    use super::*;

    #[test]
    fn backward_compatibility() {
        // Original usage still works
        let simple_macro = MacroDefinition::simple(
            "old_macro".to_string(),
            vec!["x".to_string()],
            vec![], // body
            NodeId::new(),
        );

        assert!(simple_macro.is_simple());
        assert_eq!(simple_macro.effective_parameters(), &["x"]);
    }

    #[test]
    fn new_declarative_macros() {
        // New declarative macros
        let vec_macro = MacroDefinition::declarative(
            "vec".to_string(),
            vec![], // patterns
            vec![], // templates
            NodeId::new(),
        )
        .with_description("Create a vector".to_string())
        .with_example("vec!(1, 2, 3)".to_string())
        .with_stability(MacroStability::Stable);

        assert!(vec_macro.is_declarative());
        assert_eq!(vec_macro.metadata.description, Some("Create a vector".to_string()));
    }

    #[test]
    fn migration_from_legacy() {
        // Easy migration path
        let legacy = LegacyMacroDefinition {
            name: "legacy".to_string(),
            parameters: vec!["x".to_string()],
            body: vec![],
            node_id: NodeId::new(),
        };

        let migrated: MacroDefinition = legacy.into();
        assert!(migrated.is_simple());
    }
}

/// Benefits of this evolutionary approach:
///
/// 1. **Backward Compatibility**: Existing code continues to work
/// 2. **Gradual Migration**: Can upgrade macros one at a time
/// 3. **Feature Flags**: New features are opt-in via the `kind` field
/// 4. **Extensibility**: Easy to add new macro types and metadata
/// 5. **Type Safety**: Still gets benefits of type-safe macro kinds
/// 6. **Minimal Breaking Changes**: Only additions, no removals
///
/// Migration path:
/// 1. Add new fields with defaults
/// 2. Provide constructor methods for each type
/// 3. Add extension traits for new behavior
/// 4. Gradually migrate existing macros to new types
/// 5. Eventually deprecate old patterns (but keep them working)
