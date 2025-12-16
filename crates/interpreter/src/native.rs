use crate::interpreter::Interpreter;
use crate::interpreter::Value;
use std::collections::HashMap;
use std::sync::Arc;
use veld_error::{Result, VeldError};

// Type for native function implementations
pub type NativeFn = Arc<dyn Fn(&Interpreter, Vec<Value>) -> Result<Value> + Send + Sync>;

// Registry to store native function implementations
pub struct NativeFunctionRegistry {
    functions: std::collections::HashMap<
        String,
        Arc<dyn Fn(&Interpreter, Vec<Value>) -> Result<Value> + Send + Sync>,
    >,
    static_functions:
        HashMap<String, Arc<dyn Fn(&Interpreter, Vec<Value>) -> Result<Value> + Send + Sync>>,
}

impl NativeFunctionRegistry {
    pub fn new() -> Self {
        Self {
            functions: std::collections::HashMap::new(),
            static_functions: HashMap::new(),
        }
    }

    // Register a native function
    pub fn register<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&Interpreter, Vec<Value>) -> Result<Value> + 'static + Send + Sync,
    {
        let _span = tracing::span!(tracing::Level::INFO, "Registering native function");
        let _guard = _span.enter();

        self.functions.insert(name.to_string(), Arc::new(f));
    }

    // Register a static method from the Interpreter
    pub fn register_static<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&Interpreter, Vec<Value>) -> Result<Value> + 'static + Send + Sync,
    {
        let _span = tracing::span!(tracing::Level::INFO, "Registering static method");
        let _guard = _span.enter();

        let func = Arc::new(f);
        self.static_functions.insert(name.to_string(), func);
    }

    // Get a native function by name
    pub fn get(&self, name: &str) -> Option<&NativeFn> {
        self.functions.get(name)
    }

    // Check if a function exists
    pub fn contains(&self, name: &str) -> bool {
        self.functions.contains_key(name) || self.static_functions.contains_key(name)
    }

    // Check if a static function exists
    pub fn contains_static(&self, name: &str) -> bool {
        self.static_functions.contains_key(name)
    }

    // Call a static function
    pub fn call_static(
        &self,
        name: &str,
        interpreter: &Interpreter,
        args: Vec<Value>,
    ) -> Result<Value> {
        if let Some(handler) = self.static_functions.get(name) {
            handler(interpreter, args)
        } else {
            Err(VeldError::RuntimeError(format!(
                "Static function '{}' not found",
                name
            )))
        }
    }
}

// Registry for native methods on built-in types
pub struct NativeMethodRegistry {
    methods:
        HashMap<String, Arc<dyn Fn(&Interpreter, &mut Vec<Value>) -> Result<Value> + Send + Sync>>,
}

impl NativeMethodRegistry {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }

    // Register a native method for a specific type
    pub fn register<F>(&mut self, type_name: &str, method_name: &str, f: F)
    where
        F: Fn(&Interpreter, &mut Vec<Value>) -> Result<Value> + 'static + Send + Sync,
    {
        let _span = tracing::span!(tracing::Level::INFO, "Registering static method");
        let _guard = _span.enter();

        let key = format!("{}:{}", type_name, method_name);
        self.methods.insert(key, Arc::new(f));
    }

    // Get a native method by type and method name
    pub fn get(
        &self,
        type_name: &str,
        method_name: &str,
    ) -> Option<&Arc<dyn Fn(&Interpreter, &mut Vec<Value>) -> Result<Value> + Send + Sync>> {
        let key = format!("{}:{}", type_name, method_name);
        self.methods.get(&key)
    }

    // Check if a method exists for a type
    pub fn has_method(&self, type_name: &str, method_name: &str) -> bool {
        let key = format!("{}:{}", type_name, method_name);
        self.methods.contains_key(&key)
    }

    // Helper for registering string methods
    pub fn register_string_method<F>(&mut self, method_name: &str, handler: F)
    where
        F: Fn(&str) -> String + 'static + Send + Sync,
    {
        let _span = tracing::span!(tracing::Level::INFO, "Registering string method");
        let _guard = _span.enter();

        let method_name_owned = method_name.to_string();
        self.register("str", method_name, move |_, args| {
            if let Some(Value::String(s)) = args.get(0) {
                Ok(Value::String(handler(s.as_str())))
            } else {
                Err(VeldError::RuntimeError(format!(
                    "String method {} called on non-string value",
                    method_name_owned
                )))
            }
        });
    }

    // Helper for string methods with one string parameter
    pub fn register_string_method_with_string_param<F>(&mut self, method_name: &str, handler: F)
    where
        F: Fn(&str, &str) -> String + 'static + Send + Sync,
    {
        let _span = tracing::span!(
            tracing::Level::INFO,
            "Registering string method with string parameter"
        );
        let _guard = _span.enter();

        let method_name_owned = method_name.to_string();
        self.register("str", method_name, move |_, args| {
            if let (Some(Value::String(s)), Some(Value::String(param))) = (args.get(0), args.get(1))
            {
                Ok(Value::String(handler(s.as_str(), param.as_str())))
            } else {
                Err(VeldError::RuntimeError(format!(
                    "String method {} called with invalid arguments",
                    method_name_owned
                )))
            }
        });
    }

    // Helper for string methods that return boolean
    pub fn register_string_bool_method<F>(&mut self, method_name: &str, handler: F)
    where
        F: Fn(&str) -> bool + 'static + Send + Sync,
    {
        let _span = tracing::span!(tracing::Level::INFO, "Registering string method");
        let _guard = _span.enter();

        let method_name_owned = method_name.to_string();
        self.register("str", method_name, move |_, args| {
            if let Some(Value::String(s)) = args.get(0) {
                Ok(Value::Boolean(handler(s.as_str())))
            } else {
                Err(VeldError::RuntimeError(format!(
                    "String method {} called on non-string value",
                    method_name_owned
                )))
            }
        });
    }

    // Helper for string methods with one string parameter that return boolean
    pub fn register_string_bool_method_with_string_param<F>(
        &mut self,
        method_name: &str,
        handler: F,
    ) where
        F: Fn(&str, &str) -> bool + 'static + Send + Sync,
    {
        let _span = tracing::span!(tracing::Level::INFO, "Registering string method");
        let _guard = _span.enter();

        let method_name_owned = method_name.to_string();
        self.register("str", method_name, move |_, args| {
            if let (Some(Value::String(s)), Some(Value::String(param))) = (args.get(0), args.get(1))
            {
                Ok(Value::Boolean(handler(s.as_str(), param.as_str())))
            } else {
                Err(VeldError::RuntimeError(format!(
                    "String method {} called with invalid arguments",
                    method_name_owned
                )))
            }
        });
    }
}
