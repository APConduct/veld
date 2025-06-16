use crate::error::{Result, VeldError};
use crate::interpreter::Value;
use std::sync::Arc;

// Type for native function implementations
pub type NativeFn = Arc<dyn Fn(Vec<Value>) -> Result<Value> + Send + Sync>;

// Registry to store native function implementations
pub struct NativeFunctionRegistry {
    functions: std::collections::HashMap<String, NativeFn>,
}

impl NativeFunctionRegistry {
    pub fn new() -> Self {
        Self {
            functions: std::collections::HashMap::new(),
        }
    }

    // Register a native function
    pub fn register<F>(&mut self, name: &str, f: F)
    where
        F: Fn(Vec<Value>) -> Result<Value> + 'static + Send + Sync,
    {
        self.functions.insert(name.to_string(), Arc::new(f));
    }

    // Get a native function by name
    pub fn get(&self, name: &str) -> Option<&NativeFn> {
        self.functions.get(name)
    }

    // Check if a function exists
    pub fn contains(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}
