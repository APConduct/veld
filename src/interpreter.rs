use crate::ast::{
    Argument, BinaryOperator, Expr, Literal, Statement, StructMethod, TypeAnnotation,
};
use crate::error::{Result, VeldError};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Return(Box<Value>),
    Unit,
    Function {
        params: Vec<(String, TypeAnnotation)>,
        body: Vec<Statement>,
        return_type: TypeAnnotation,
    },
    Struct {
        name: String,
        fields: HashMap<String, Value>,
    },
}

impl Value {
    // Helper method to unwrap return values
    fn unwrap_return(self) -> Value {
        match self {
            Value::Return(val) => *val,
            val => val,
        }
    }
}

#[derive(Debug, Default)]
struct Scope {
    values: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    fn set(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
}

pub struct Interpreter {
    scopes: Vec<Scope>,
    structs: HashMap<String, Vec<(String, TypeAnnotation)>>, // struct name -> fields
    struct_methods: HashMap<String, HashMap<String, Value>>, // struct name -> (method name -> method)
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<Value> {
        println!(
            "Starting interpretation with {} statements",
            statements.len()
        );
        let mut last_value = Value::Unit;
        for (i, stmt) in statements.iter().enumerate() {
            println!(
                "Executing statement {}/{}: {:?}",
                i + 1,
                statements.len(),
                stmt
            );
            last_value = self.execute_statement(stmt.clone())?;
            println!(
                "Statement {} completed with result: {:?}",
                i + 1,
                last_value
            );
        }
        println!("Interpretation complete");
        Ok(last_value)
    }

    fn execute_statement(&mut self, statement: Statement) -> Result<Value> {
        println!("Executing statement: {:?}", statement);

        match statement {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                ..
            } => {
                let function = Value::Function {
                    params,
                    body,
                    return_type,
                };
                self.current_scope_mut().set(name, function);
                Ok(Value::Unit)
            }
            Statement::ProcDeclaration { name, params, body } => {
                // Convert to a regular function with Unit return type
                let function = Value::Function {
                    params,
                    body,
                    return_type: TypeAnnotation::Unit,
                };
                self.current_scope_mut().set(name, function);
                Ok(Value::Unit)
            }
            Statement::StructDeclaration {
                name,
                fields,
                methods,
            } => {
                // Register the struct type
                self.structs.insert(name.clone(), fields);

                // Register methods if any
                if !methods.is_empty() {
                    let mut method_map = HashMap::new();

                    for method in methods {
                        let method_value = Value::Function {
                            params: method.params,
                            body: method.body,
                            return_type: method.return_type,
                        };

                        method_map.insert(method.name, method_value);
                    }

                    self.struct_methods.insert(name, method_map);
                }

                Ok(Value::Unit)
            }
            Statement::Implementation {
                type_name,
                kind_name: _,
                methods,
            } => {
                // Just register all methods for now, ignoring kinds
                let method_map = self
                    .struct_methods
                    .entry(type_name.clone())
                    .or_insert_with(HashMap::new);

                for method in methods {
                    let method_value = Value::Function {
                        params: method.params,
                        body: method.body,
                        return_type: method.return_type,
                    };

                    method_map.insert(method.name, method_value);
                }

                Ok(Value::Unit)
            }
            Statement::VariableDeclaration { name, value, .. } => {
                let value = self.evaluate_expression(*value)?;
                let value = value.unwrap_return();
                self.current_scope_mut().set(name, value.clone());
                Ok(value)
            }
            Statement::ExprStatement(expr) => {
                let value = self.evaluate_expression(expr)?;
                Ok(value.unwrap_return())
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.evaluate_expression(condition)?.unwrap_return();

                if self.is_truthy(cond_value) {
                    for stmt in then_branch {
                        let result = self.execute_statement(stmt)?;
                        if matches!(result, Value::Return(_)) {
                            return Ok(result);
                        }
                    }
                } else if let Some(else_statements) = else_branch {
                    for stmt in else_statements {
                        let result = self.execute_statement(stmt)?;
                        if matches!(result, Value::Return(_)) {
                            return Ok(result);
                        }
                    }
                }
                Ok(Value::Unit)
            }
            Statement::While { condition, body } => {
                loop {
                    let cond_result = self.evaluate_expression(condition.clone())?.unwrap_return();
                    if !self.is_truthy(cond_result) {
                        break;
                    }

                    for stmt in body.clone() {
                        let result = self.execute_statement(stmt)?;
                        if matches!(result, Value::Return(_)) {
                            return Ok(result);
                        }
                    }
                }
                Ok(Value::Unit)
            }
            Statement::For {
                iterator,
                iterable,
                body,
            } => {
                // We'll implement this later
                Ok(Value::Unit)
            }
            Statement::Return(expr) => {
                let value = if let Some(e) = expr {
                    self.evaluate_expression(e)?
                } else {
                    Value::Unit // Implicit return of unit
                };
                Ok(Value::Return(Box::new(value)))
            }
            // Skip other declarations for now
            _ => Ok(Value::Unit),
        }
    }

    fn evaluate_expression(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Literal(lit) => Ok(match lit {
                Literal::Integer(n) => Value::Integer(n),
                Literal::Float(n) => Value::Float(n),
                Literal::String(s) => Value::String(s),
                Literal::Boolean(b) => Value::Boolean(b),
                Literal::Unit => Value::Unit,
            }),
            Expr::UnitLiteral => Ok(Value::Unit),
            Expr::Identifier(name) => self
                .get_variable(&name)
                .ok_or_else(|| VeldError::RuntimeError(format!("Undefined variable '{}'", name))),
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expression(*left)?.unwrap_return();
                let right_val = self.evaluate_expression(*right)?.unwrap_return();
                self.evaluate_binary_op(left_val, operator, right_val)
            }
            Expr::FunctionCall { name, arguments } => {
                // Evaluate each argument and handle both named and positional arguments
                let mut arg_values = Vec::new();
                let mut named_args = HashMap::new();

                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            let value = self.evaluate_expression(expr)?;
                            arg_values.push(value);
                        }
                        Argument::Named { name, value } => {
                            // Store named arguments for future use
                            // For now, we'll just evaluate positional arguments
                            let value = self.evaluate_expression(value)?;
                            named_args.insert(name, value);
                        }
                    }
                }

                // Special handling for standard library functions
                if name == "sqrt" {
                    if arg_values.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "sqrt() takes exactly one argument".to_string(),
                        ));
                    }

                    // Extract the argument and compute square root
                    match arg_values[0] {
                        Value::Float(f) => Ok(Value::Float(f.sqrt())),
                        _ => Err(VeldError::RuntimeError(
                            "sqrt() requires a floating-point argument".to_string(),
                        )),
                    }
                } else {
                    // Regular function call
                    self.call_function_with_values(name, arg_values)
                }
            }

            Expr::PropertyAccess { object, property } => {
                let obj_value = self.evaluate_expression(*object)?;
                self.get_property(obj_value, &property)
            }

            Expr::Lambda { .. } => {
                // We'll implement lambdas later
                Err(VeldError::RuntimeError(
                    "Lambdas not yet implemented".into(),
                ))
            }

            Expr::MethodCall {
                object,
                method,
                arguments,
            } => {
                let obj_value = self.evaluate_expression(*object)?;

                // Empty arguments list means it's a property access
                if arguments.is_empty() {
                    self.get_property(obj_value, &method)
                } else {
                    // Evaluate all arguments
                    let mut arg_values = Vec::new();
                    for arg in arguments {
                        // Extract and evaluate the expression from the Argument
                        let expr = match arg {
                            Argument::Positional(expr) => expr,
                            Argument::Named { name: _, value } => value,
                        };
                        let value = self.evaluate_expression(expr)?;
                        arg_values.push(value);
                    }

                    self.call_method_value(obj_value, method, arg_values)
                }
            }

            Expr::StructCreate {
                struct_name,
                fields,
            } => {
                // Check if struct exists
                if !self.structs.contains_key(&struct_name) {
                    return Err(VeldError::RuntimeError(format!(
                        "Undefined struct '{}'",
                        struct_name
                    )));
                }

                let mut field_values = HashMap::new();

                // Evaluate each field value
                for (field_name, field_expr) in fields {
                    let value = self.evaluate_expression(field_expr)?;
                    field_values.insert(field_name, value.unwrap_return());
                }

                Ok(Value::Struct {
                    name: struct_name,
                    fields: field_values,
                })
            }
        }
    }

    // Helper method to call functions with pre-evaluated arguments
    fn call_function_with_values(&mut self, name: String, arg_values: Vec<Value>) -> Result<Value> {
        let function = self
            .get_variable(&name)
            .ok_or_else(|| VeldError::RuntimeError(format!("Undefined function '{}'", name)))?;

        match function {
            Value::Function { params, body, .. } => {
                // Create new scope for function
                self.push_scope();

                // Bind arguments
                if arg_values.len() != params.len() {
                    return Err(VeldError::RuntimeError(format!(
                        "Expected {} arguments but got {}",
                        params.len(),
                        arg_values.len()
                    )));
                }

                // Bind parameters to arguments
                for (i, arg) in arg_values.iter().enumerate() {
                    self.current_scope_mut()
                        .set(params[i].0.clone(), arg.clone());
                }

                // Execute function body
                let mut result = Value::Unit;
                for stmt in body {
                    result = self.execute_statement(stmt)?;
                    if matches!(result, Value::Return(_)) {
                        break;
                    }
                }

                // Remove function scope
                self.pop_scope();

                Ok(result.unwrap_return())
            }
            _ => Err(VeldError::RuntimeError(format!(
                "'{}' is not a function",
                name
            ))),
        }
    }

    fn get_property(&self, object: Value, property: &str) -> Result<Value> {
        match object {
            Value::Struct { name: _, fields } => {
                if let Some(value) = fields.get(property) {
                    Ok(value.clone())
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Property '{}' not found",
                        property
                    )))
                }
            }
            _ => Err(VeldError::RuntimeError(format!(
                "Cannot access property on non-struct value"
            ))),
        }
    }

    fn call_method_value(
        &mut self,
        object: Value,
        method_name: String,
        args: Vec<Value>,
    ) -> Result<Value> {
        // For built-in methods like "sqrt" on numeric values
        match (&object, method_name.as_str()) {
            (Value::Float(f), "sqrt") => {
                if args.is_empty() {
                    Ok(Value::Float(f.sqrt()))
                } else {
                    Err(VeldError::RuntimeError(
                        "sqrt() takes no arguments".to_string(),
                    ))
                }
            }

            // For struct methods
            (Value::Struct { name, .. }, _) => {
                // Create a string representation for the struct's name
                let struct_name = name.clone();

                // Get the method from struct_methods
                let method = self
                    .struct_methods
                    .get(&struct_name)
                    .and_then(|methods| methods.get(&method_name))
                    .cloned()
                    .ok_or_else(|| {
                        VeldError::RuntimeError(format!(
                            "Method '{}' not found on '{}'",
                            method_name, struct_name
                        ))
                    })?;

                match method {
                    Value::Function { params, body, .. } => {
                        self.push_scope();

                        // Bind 'self' to the struct instance
                        self.current_scope_mut().set("self".to_string(), object);

                        // Check argument count (excluding self)
                        if args.len() != params.len() - 1 {
                            return Err(VeldError::RuntimeError(format!(
                                "Method '{}' expects {} arguments but got {}",
                                method_name,
                                params.len() - 1,
                                args.len()
                            )));
                        }

                        // Bind arguments directly (already evaluated)
                        for (i, arg) in args.into_iter().enumerate() {
                            // Start from index 1 to skip self
                            self.current_scope_mut().set(params[i + 1].0.clone(), arg);
                        }

                        // Execute method body
                        let mut result = Value::Unit;
                        for stmt in body {
                            result = self.execute_statement(stmt)?;
                            if matches!(result, Value::Return(_)) {
                                break;
                            }
                        }

                        self.pop_scope();

                        Ok(result.unwrap_return())
                    }
                    _ => Err(VeldError::RuntimeError(
                        "Internal error: method is not a function".to_string(),
                    )),
                }
            }

            _ => Err(VeldError::RuntimeError(format!(
                "Method '{}' not found",
                method_name
            ))),
        }
    }

    fn is_truthy(&self, value: Value) -> bool {
        match value {
            Value::Boolean(b) => b,
            Value::Integer(n) => n != 0,
            Value::Float(f) => f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Unit => false,
            _ => true,
        }
    }
    fn evaluate_binary_op(
        &mut self,
        left: Value,
        operator: BinaryOperator,
        right: Value,
    ) -> Result<Value> {
        match (left, operator, right) {
            // Arithmetic
            (Value::Integer(a), BinaryOperator::Add, Value::Integer(b)) => {
                Ok(Value::Integer(a + b))
            }
            (Value::Integer(a), BinaryOperator::Subtract, Value::Integer(b)) => {
                Ok(Value::Integer(a - b))
            }
            (Value::Integer(a), BinaryOperator::Multiply, Value::Integer(b)) => {
                Ok(Value::Integer(a * b))
            }
            (Value::Integer(a), BinaryOperator::Divide, Value::Integer(b)) => {
                if b == 0 {
                    Err(VeldError::RuntimeError("Division by zero".to_string()))
                } else {
                    Ok(Value::Integer(a / b))
                }
            }
            // Comparison
            (Value::Integer(a), BinaryOperator::LessEq, Value::Integer(b)) => {
                Ok(Value::Boolean(a <= b))
            }
            (Value::Integer(a), BinaryOperator::GreaterEq, Value::Integer(b)) => {
                Ok(Value::Boolean(a >= b))
            }
            (Value::Integer(a), BinaryOperator::Less, Value::Integer(b)) => {
                Ok(Value::Boolean(a < b))
            }
            (Value::Integer(a), BinaryOperator::Greater, Value::Integer(b)) => {
                Ok(Value::Boolean(a > b))
            }
            (Value::Integer(a), BinaryOperator::EqualEqual, Value::Integer(b)) => {
                Ok(Value::Boolean(a == b))
            }
            (Value::Integer(a), BinaryOperator::NotEqual, Value::Integer(b)) => {
                Ok(Value::Boolean(a != b))
            }
            // Boolean logic
            (Value::Boolean(a), BinaryOperator::And, Value::Boolean(b)) => {
                Ok(Value::Boolean(a && b))
            }
            (Value::Boolean(a), BinaryOperator::Or, Value::Boolean(b)) => {
                Ok(Value::Boolean(a || b))
            }
            _ => Err(VeldError::RuntimeError("Invalid operation".to_string())),
        }
    }

    fn call_function(&mut self, name: String, arguments: Vec<Argument>) -> Result<Value> {
        println!("Calling function: {}", name); // Add this debug line

        // Check if this might be a method call on a struct
        if name.contains('.') {
            let parts: Vec<&str> = name.split('.').collect();
            if parts.len() == 2 {
                return self.call_method(parts[0].to_string(), parts[1].to_string(), arguments);
            }
        }

        let function = self
            .get_variable(&name)
            .ok_or_else(|| VeldError::RuntimeError(format!("Undefined function '{}'", name)))?;

        match function {
            Value::Function { params, body, .. } => {
                // Create new scope for function
                self.push_scope();

                // Evaluate and bind arguments
                if arguments.len() != params.len() {
                    return Err(VeldError::RuntimeError(format!(
                        "Expected {} arguments but got {}",
                        params.len(),
                        arguments.len()
                    )));
                }

                // Evaluate arguments and bind them to parameters
                for (i, arg) in arguments.into_iter().enumerate() {
                    // Extract and evaluate the expression from the Argument
                    let expr = match arg {
                        Argument::Positional(expr) => expr,
                        Argument::Named { name: _, value } => value,
                    };
                    let value = self.evaluate_expression(expr)?;
                    let value = value.unwrap_return();
                    self.current_scope_mut().set(params[i].0.clone(), value);
                }

                // Execute function body
                let mut result = Value::Unit;
                for stmt in body {
                    result = self.execute_statement(stmt)?;
                    if matches!(result, Value::Return(_)) {
                        break;
                    }
                }

                // Remove function scope
                self.pop_scope();

                Ok(result.unwrap_return())
            }
            _ => Err(VeldError::RuntimeError(format!(
                "'{}' is not a function",
                name
            ))),
        }
    }

    fn call_method(
        &mut self,
        struct_name: String,
        method_name: String,
        arguments: Vec<Argument>,
    ) -> Result<Value> {
        // First, get the struct instance
        let instance = self.get_variable(&struct_name).ok_or_else(|| {
            VeldError::RuntimeError(format!("Undefined variable '{}'", struct_name))
        })?;

        // Get the method from struct_methods
        let struct_type = match &instance {
            Value::Struct { name, .. } => name.clone(),
            _ => {
                return Err(VeldError::RuntimeError(format!(
                    "'{}' is not a struct",
                    struct_name
                )))
            }
        };

        let method = self
            .struct_methods
            .get(&struct_type)
            .and_then(|methods| methods.get(&method_name))
            .cloned()
            .ok_or_else(|| {
                VeldError::RuntimeError(format!(
                    "Method '{}' not found on '{}'",
                    method_name, struct_type
                ))
            })?;

        match method {
            Value::Function { params, body, .. } => {
                self.push_scope();

                // Bind 'self' to the struct instance
                self.current_scope_mut().set("self".to_string(), instance);

                // Check argument count (excluding self)
                if arguments.len() != params.len() - 1 {
                    return Err(VeldError::RuntimeError(format!(
                        "Method '{}' expects {} arguments but got {}",
                        method_name,
                        params.len() - 1,
                        arguments.len()
                    )));
                }

                // Evaluate and bind remaining arguments
                for (i, arg) in arguments.into_iter().enumerate() {
                    // Extract and evaluate the expression from the Argument
                    let expr = match arg {
                        Argument::Positional(expr) => expr,
                        Argument::Named { name: _, value } => value,
                    };
                    let value = self.evaluate_expression(expr)?;
                    let value = value.unwrap_return();
                    // Start from index 1 to skip self
                    self.current_scope_mut().set(params[i + 1].0.clone(), value);
                }

                // Execute method body
                let mut result = Value::Unit;
                for stmt in body {
                    result = self.execute_statement(stmt)?;
                    if matches!(result, Value::Return(_)) {
                        break;
                    }
                }

                self.pop_scope();

                Ok(result.unwrap_return())
            }
            _ => Err(VeldError::RuntimeError(
                "Internal error: method is not a function".to_string(),
            )),
        }
    }
    // Scope management
    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_variable(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}
