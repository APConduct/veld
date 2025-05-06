use crate::ast::{Argument, BinaryOperator, Expr, ImportItem, Literal, Statement, TypeAnnotation};
use crate::error::{Result, VeldError};
use std::collections::HashMap;
use std::path::Path;
use crate::module::{ModuleManager, ExportedItem};

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
    Array(Vec<Value>),
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
    module_manager: ModuleManager,
    current_module: String,
    imported_modules: HashMap<String, String>, // alias -> module name
}

impl Interpreter {
    pub fn new<P: AsRef<Path>>(root_dir: P) -> Self {
        let mut interpreter = Self {
            scopes: vec![Scope::new()],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
            module_manager: ModuleManager::new(root_dir),
            current_module: "main".to_string(),
            imported_modules: Default::default(),
        };
        
        // Initialize Built-in array methods
        interpreter.initialize_array_methods();
        interpreter
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
                is_public: _,
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
            Statement::ModuleDeclaration {name, body, is_public} => {
                self.execute_module_declaration(name, body, is_public)
            }
            Statement::ImportDeclaration {path, items, alias, is_public} => {
                self.execute_import_declaration(path, items, alias, is_public)
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

            Expr::Lambda { params, body, return_type } => {
                // Convert lambdas to function value
                let param_list = params.iter().map(|(name, type_opt)| {
                    // Convert optional type annotations to Concrete type(s)
                    let type_annotation = if let Some(ty) = type_opt {
                        ty.clone()
                    } else { 
                        // Use placeholder for type inference
                        TypeAnnotation::Basic("any".to_string())
                    };
                    (name.clone(), type_annotation)
                }).collect();
                
                // Create simple statement that returns the body
                let body_stmt = vec![Statement::Return(Some(*body.clone()))];
                
                // Use Unit as default return type if not specified
                let ret_type = return_type.clone().unwrap_or(TypeAnnotation::Unit);
                
                Ok(Value::Function {
                    params: param_list,
                    body: body_stmt,
                    return_type: ret_type,
                })
            }

            Expr::MethodCall {
                object,
                method,
                arguments,
            } => {
                let obj_value = self.evaluate_expression(*object)?;

                // Handle array methods
                match &obj_value {
                    Value::Array(_) => {
                        // Evaluate all arguments
                        let mut arg_values = Vec::new();
                        for arg in arguments {
                            let expr = match arg {
                                Argument::Positional(expr) => expr,
                                Argument::Named { name: _, value } => value,
                            };
                            let value = self.evaluate_expression(expr)?;
                            arg_values.push(value);
                        }
                        
                        // Call the array method directly
                        self.call_method_value(obj_value, method, arg_values)
                    },
                    _ => {
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
            Expr::ArrayLiteral(elements) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate_expression(element)?.unwrap_return());
                }
                Ok(Value::Array(values))
            },
            Expr::IndexAccess {object, index} => {
                let obj_value = self.evaluate_expression(*object)?.unwrap_return();
                let idx_value = self.evaluate_expression(*index)?.unwrap_return();
                
                match obj_value {
                    Value::Array(elements) => {
                        match idx_value {
                            Value::Integer(i) => {
                                if i < 0 || i >= elements.len() as i64 {
                                    return Err(VeldError::RuntimeError(format!("Array index out of bounds: {}", i)));
                                }
                                Ok(elements[i as usize].clone())
                            },
                            _ => Err(VeldError::RuntimeError("Array index must be an integer".to_string())), 
                        }
                    },
                    Value::String(s) => {
                        // Allow indexing into strings
                        match idx_value {
                            Value::Integer(i) => {
                                if i < 0 || i >= s.len() as i64 {
                                    return Err(VeldError::RuntimeError(format!("String index out of bounds: {}", i)));
                                }
                                
                                let char_value = s.chars().nth(i as usize).unwrap().to_string();
                                Ok(Value::String(char_value))
                            },
                            _ => Err(VeldError::RuntimeError("String index must be an integer".to_string())),
                        }
                    },
                    _ => Err(VeldError::RuntimeError("Cannot index into non-array value".to_string())),
                }
            }
        }
    }
    
    fn initialize_array_methods(&mut self) {
        // Create array prototype with methods
        let mut array_methods = HashMap::new();
        
        // push method
        array_methods.insert("push".to_string(), Value::Function { 
            params: vec![
                ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
                ("element".to_string(), TypeAnnotation::Basic("any".to_string())), 
            ],
            body: vec![], // Built-in method with custom implementation
            return_type: TypeAnnotation::Unit,
        });
        
        // pop method
        array_methods.insert("pop".to_string(), Value::Function {
            params: vec![
                ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
            ],
            body: vec![], // Built-in method with custom implementation
            return_type: TypeAnnotation::Basic("any".to_string()),
        });
        
        // length method
        array_methods.insert("len".to_string(), Value::Function {
            params: vec![
                ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
            ],
            body: vec![], // Built-in method with custom implementation
            return_type: TypeAnnotation::Basic("i32".to_string()),
        });
        
        // map method
        array_methods.insert("map".to_string(), Value::Function {
            params: vec![
                ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
                ("fn".to_string(), TypeAnnotation::Basic("any".to_string())),
            ],
            body: vec![], // Built-in method with custom implementation
            return_type: TypeAnnotation::Basic("Array".to_string()),
        });
        
        // array_methods.insert("map".to_string(), Value::Function {
        //     params: vec![
        //         ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
        //         ("callback".to_string(), TypeAnnotation::Basic("function".to_string())),
        //     ],
        //     body: vec![], // Built-in method with custom implementation
        //     return_type: TypeAnnotation::Basic("Array".to_string()),
        // });

        // filter method
        array_methods.insert("filter".to_string(), Value::Function {
            params: vec![
                ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
                ("fn".to_string(), TypeAnnotation::Basic("any".to_string())),
            ],
            body: vec![], // Built-in method with custom implementation
            return_type: TypeAnnotation::Basic("Array".to_string()),
        });
        
        // Store methods in struct_methods HashMap with a special key
        self.struct_methods.insert("Array".to_string(), array_methods);
        
        // array_methods.insert("filter".to_string(), Value::Function {
        //     params: vec![
        //         ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
        //         ("callback".to_string(), TypeAnnotation::Basic("function".to_string())),
        //     ],
        //     body: vec![], // Built-in method with custom implementation
        //     return_type: TypeAnnotation::Basic("Array".to_string()),
        // });
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
            },
            Value::Array(elements) => {
                // Handle array properties
                match property {
                    "len" => Ok(Value::Integer(elements.len() as i64)),
                    "push" | "pop" | "map" | "filter" => {
                        // Return a special function that, when called, 
                        // will invoke the corresponding array method
                        Ok(Value::Function {
                            params: vec![
                                ("self".to_string(), TypeAnnotation::Basic("Array".to_string())),
                                ("arg".to_string(), TypeAnnotation::Unit),
                            ],
                            body: vec![], // Empty body for built-in methods
                            return_type: TypeAnnotation::Basic("any".to_string()),
                        })
                    },
                    _ => Err(VeldError::RuntimeError(format!("Property '{}' not found", property))),
                }
            }
            _ => Err(VeldError::RuntimeError("Cannot access property on non-struct value".to_string())),
        }
    }

    fn call_method_value(
        &mut self,
        object: Value,
        method_name: String,
        args: Vec<Value>,
    ) -> Result<Value> {
        // Special handling for array methods
        if let Value::Array(elements) = &object {
            match method_name.as_str() {
                "push" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "push() takes exactly one argument".to_string(),
                        ));
                    }
                    
                    // Create a mutable clone of the array
                    let mut new_elements = elements.clone();
                    new_elements.push(args[0].clone());
                    
                    return Ok(Value::Array(new_elements));
                },
                "pop" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "pop() takes no arguments".to_string(),
                        ));
                    }
                    
                    // Check if the array is empty
                    return if elements.is_empty() {
                        Err(VeldError::RuntimeError(
                            "pop() called on an empty array".to_string(),
                        ))
                    } else {
                        // Create a mutable clone and pop the last element
                        let mut new_elements = elements.clone();
                        let popped = new_elements.pop().unwrap();

                        // Return the modified array as the parent Value
                        // and the popped value as the return value
                        Ok(popped)
                    }
                },
                "len" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "len() takes no arguments".to_string(),
                        ));
                    }
                    
                    return Ok(Value::Integer(elements.len() as i64));
                },
                "map" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "map() takes exactly one argument".to_string()
                        ));
                    }
                    
                    // Get the function to apply
                    let func = &args[0];
                    
                    // Apply the function to each element
                    let mut result = Vec::new();
                    for element in elements {
                        match func {
                            Value::Function {..} => {
                                // Call the function with the element as argument
                                let arg = vec![element.clone()];
                                let mapped = self.call_function_with_values("lambda".to_string(), // dummy name
                                                                            vec![func.clone(), element.clone()]
                                )?;
                                result.push(mapped);
                            },
                            _ => return Err(VeldError::RuntimeError("map() expects a function argument".to_string())),
                        }
                    }
                    return Ok(Value::Array(result));
                },
                "filter" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "filter() takes exactly one argument".to_string()
                        ));
                    }
                    
                    // Get the function 
                    let func = &args[0];
                    
                    // Apply predicate to each element
                    let mut result = Vec::new();
                    for element in elements {
                        match func {
                            Value::Function {..} => {
                                // Call the function with element
                                let filtered = self.call_function_with_values("lambda".to_string(), // dummy name
                                                                            vec![func.clone(), element.clone()]
                                
                                )?;
                                
                                // If the result is truthy, keep the element
                                if self.is_truthy(filtered) { 
                                    result.push(element.clone());
                                }
                            },
                            _ => return Err(VeldError::RuntimeError("filter() expects a function argument".to_string())),
                        }
                    }
                    return Ok(Value::Array(result));
                },
                _ => (), // Fallthrough to regular method handling
            }
        }
        
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
            (Value::Float(a), BinaryOperator::Exponent, Value::Float(b)) => {
                Ok(Value::Float(a.powf(b)))
            }
            (Value::Integer(a), BinaryOperator::Exponent, Value::Integer(b)) => {
                // Handle integer exponentiation
                if b >= 0 {
                    Ok(Value::Integer(a.pow(b as u32)))
                } else {
                    // Negative exponent requires floating point
                    Ok(Value::Float((a as f64).powf(b as f64)))
                }
            }
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

            (Value::Float(a), BinaryOperator::Add, Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Float(a), BinaryOperator::Subtract, Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Float(a), BinaryOperator::Multiply, Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Float(a), BinaryOperator::Divide, Value::Float(b)) => {
                if b == 0.0 {
                    Err(VeldError::RuntimeError("Division by zero".to_string()))
                } else {
                    Ok(Value::Float(a / b))
                }
            }
            // Also add comparisons for floats
            (Value::Float(a), BinaryOperator::LessEq, Value::Float(b)) => {
                Ok(Value::Boolean(a <= b))
            }

            (Value::Integer(a), BinaryOperator::Add, Value::Float(b)) => {
                Ok(Value::Float(a as f64 + b))
            }
            (Value::Float(a), BinaryOperator::Add, Value::Integer(b)) => {
                Ok(Value::Float(a + b as f64))
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
    
    fn execute_module_declaration(&mut self, name: String, body: Option<Vec<Statement>>, is_public: bool) -> Result<Value> {
        if let Some(statements) = body {
            // Save current module name
            let previous_module = self.current_module.clone();
            self.current_module = name.clone();
            
            // Create new scope for module
            self.push_scope();
            
            // Execute all statements in the module
            let mut result = Value::Unit;
            for stmt in statements.clone() {
                result = self.execute_statement(stmt)?;
                if matches!(result, Value::Return(_)) {
                    break;
                }
            }
            
            // Register the module
            self.module_manager.create_module(&name, statements)?;
            
            // Restore previous context
            self.push_scope();
            self.current_module = previous_module;
            
            Ok(Value::Unit)
        } else { 
            // Just a module declaration referencing a file
            // Try to load the module from filesystem
            self.module_manager.load_module(&[name])?;
            Ok(Value::Unit)
        }
    }
    
    fn execute_import_declaration(&mut self, path: Vec<String>, items: Vec<ImportItem>, alias: Option<String>, is_public: bool) -> Result<Value> {
        let module_path_str = path.join(".");
        
        // First, ensure the module is loaded
        self.module_manager.load_module(&path)?;
        
        // Process imports based on alias or direct imports
        if let Some(alias_name) = alias {
            // Store the module alias -> actual name mapping
            self.imported_modules.insert(alias_name, module_path_str);
        } else { 
            // No alias, so we're importing specific items or the entire module
            
            // Get the exports we want
            let exports = self.module_manager.get_exports(&module_path_str, &items)?;
            
            // Process each export and add to current scope
            for (name, export_item) in exports {
                // Get module to extract statements from 
                let module = self.module_manager.get_module(&module_path_str)
                    .ok_or_else(|| VeldError::RuntimeError("Module not found".to_string()))?;
                
                match export_item {
                    ExportedItem::Function(idx) => {
                        if let Statement::FunctionDeclaration {name: fn_name,params, return_type, body, ..} = &module.statements[idx] {
                            let function = Value::Function {
                                params: params.clone(),
                                body: body.clone(),
                                return_type: return_type.clone(),
                            };
                            self.current_scope_mut().set(name.clone(), function);
                        }
                    },
                    ExportedItem::Struct(idx) => {
                        // Handle struct imports - for now just remember the name
                        if let Statement::StructDeclaration {name: struct_name, fields, ..} = &module.statements[idx] {
                            // Register the struct type in this scope
                            self.structs.insert(name, fields.clone());
                        }
                    },
                    // TODO - Handle other items here
                    _ => {}
                }
            }
        }
        Ok(Value::Unit)
    }
}
