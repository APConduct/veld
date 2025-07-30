use super::Value;
use crate::error::{Result, VeldError};
use crate::interpreter::VarKind;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Scope {
    values: HashMap<String, Value>,
    var_kinds: HashMap<String, VarKind>,
    scope_level: usize,
}

impl Scope {
    pub fn new(level: usize) -> Self {
        Self {
            values: HashMap::new(),
            var_kinds: HashMap::new(),
            scope_level: level,
        }
    }

    pub fn values_mut(&mut self) -> &mut HashMap<String, Value> {
        &mut self.values
    }

    pub fn vals(&self) -> &HashMap<String, Value> {
        &self.values
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn set_with_kind(&mut self, name: String, value: Value, kind: VarKind) {
        self.values.insert(name.clone(), value);
        self.var_kinds.insert(name, kind);
    }

    fn is_mutable(&self, name: &str) -> bool {
        match self.var_kinds.get(name) {
            Some(VarKind::Var) | Some(VarKind::LetMut) => true,
            _ => false,
        }
    }

    pub fn values(&self) -> Vec<(String, Value)> {
        self.values
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    pub fn declare(&mut self, name: String, value: Value, kind: VarKind) -> Result<()> {
        let span = tracing::debug_span!("declare_variable", name = %name, kind = ?kind);
        let _enter = span.enter();

        // Check for const reassignment
        if let Some(existing_kind) = self.var_kinds.get(&name) {
            match existing_kind {
                VarKind::Const => {
                    return Err(VeldError::RuntimeError(format!(
                        "Cannot redeclare constant '{}'",
                        name
                    )));
                }
                VarKind::Let => {
                    // Let can be shadowed
                }
                VarKind::Var | VarKind::LetMut => {
                    // Mutable variables can be reassigned but not redeclared
                    return Err(VeldError::RuntimeError(format!(
                        "Variable '{}' is already declared in this scope",
                        name
                    )));
                }
            }
        }

        self.values.insert(name.clone(), value);
        self.var_kinds.insert(name, kind);
        Ok(())
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<()> {
        let span = tracing::debug_span!("declare_variable",
            name = %name,
        );
        let _enter = span.enter();

        match self.var_kinds.get(name) {
            Some(VarKind::Var | VarKind::LetMut) => {
                self.values.insert(name.to_string(), value);
                Ok(())
            }
            Some(VarKind::Const) => Err(VeldError::RuntimeError(format!(
                "Cannot assign to constant '{}'",
                name
            ))),
            Some(VarKind::Let) => Err(VeldError::RuntimeError(format!(
                "Cannot assign to immutable variable '{}'",
                name
            ))),
            None => Err(VeldError::RuntimeError(format!(
                "Cannot assign to undefined variable '{}'",
                name
            ))),
        }
    }
}
