use crate::ast::{
    Argument, BinaryOperator, EnumVariant, Expr, ImportItem, Literal, MatchPattern, Statement,
    TypeAnnotation,
};
use crate::error::{Result, VeldError};
use crate::module::{ExportedItem, ModuleManager};
use crate::types::{Type, TypeChecker};
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Char(char),
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
    Enum {
        enum_name: String,
        variant_name: String,
        fields: Vec<Value>,
    },
    Tuple(Vec<Value>),

    Break,
    Continue,
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
    enums: HashMap<String, Vec<EnumVariant>>,
    pub type_checker: TypeChecker,
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
            enums: HashMap::new(),
            type_checker: TypeChecker::new(),
        };

        // Initialize Built-in array methods
        interpreter.initialize_array_methods();

        if let Err(e) = interpreter.initialize_operator_kinds() {
            eprintln!("Warning: Failed to initialize operator kinds: {}", e);
        }
        interpreter
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<Value> {
        println!(
            "Starting interpretation with {} statements",
            statements.len()
        );

        if let Err(e) = self.type_checker.check_program(&statements) {
            return Err(e);
        }

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
            Statement::Return(expr_opt) => {
                let val = if let Some(e) = expr_opt {
                    self.evaluate_expression(e)?
                } else {
                    Value::Unit
                };
                Ok(Value::Return(Box::new(val)))
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
                generic_args: _,
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
                        match result {
                            Value::Return(_) => return Ok(result),
                            Value::Break => return Ok(Value::Unit),
                            Value::Continue => break,
                            _ => {}
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
                let iterable_value = self.evaluate_expression(iterable.clone())?.unwrap_return();

                match iterable_value {
                    Value::Array(elements) => {
                        for element in elements {
                            self.current_scope_mut().set(iterator.clone(), element);

                            for stmt in body.clone() {
                                let result = self.execute_statement(stmt)?;
                                match result {
                                    Value::Return(_) => return Ok(result),
                                    Value::Break => return Ok(Value::Unit),
                                    Value::Continue => break,
                                    _ => {}
                                }
                            }
                        }
                    }
                    Value::String(s) => {
                        for c in s.chars() {
                            self.current_scope_mut()
                                .set(iterator.clone(), Value::String(c.to_string()));

                            for stmt in body.clone() {
                                let result = self.execute_statement(stmt)?;
                                match result {
                                    Value::Return(_) => return Ok(result),
                                    Value::Break => return Ok(Value::Unit),
                                    Value::Continue => break,
                                    _ => {}
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(VeldError::RuntimeError(format!(
                            "Cannot iterate over value of type {:?}",
                            iterable_value
                        )));
                    }
                }
                Ok(Value::Unit)
            }
            Statement::ModuleDeclaration {
                name,
                body,
                is_public,
            } => self.execute_module_declaration(name, body, is_public),
            Statement::ImportDeclaration {
                path,
                items,
                alias,
                is_public,
            } => self.execute_import_declaration(path, items, alias, is_public),
            Statement::CompoundAssignment {
                name,
                operator,
                value,
            } => {
                let current = self.get_variable(&name).ok_or_else(|| {
                    VeldError::RuntimeError(format!("Undefined variable '{}'", name))
                })?;
                let new_value = self.evaluate_expression(*value)?;
                let result = self.evaluate_binary_op(current, operator, new_value)?;

                self.current_scope_mut().set(name, result.clone());
                Ok(Value::Unit)
            }
            Statement::EnumDeclaration { name, variants, .. } => {
                self.enums.insert(name, variants);
                Ok(Value::Unit)
            }
            Statement::Break => Ok(Value::Break),
            Statement::Continue => Ok(Value::Continue),
            Statement::Match { value, arms } => {
                let match_value = self.evaluate_expression(value)?.unwrap_return();

                for arm in arms {
                    if let Some(bindings) = self.patter_matches(&arm.pat, &match_value)? {
                        if let Some(gaurd) = arm.gaurd {
                            self.push_scope();

                            for (name, val) in &bindings {
                                self.current_scope_mut().set(name.clone(), val.clone());
                            }

                            let guard_result = self.evaluate_expression(gaurd)?.unwrap_return();
                            let gaurd_passed = self.is_truthy(guard_result);

                            self.pop_scope();

                            if !gaurd_passed {
                                continue;
                            }
                        }

                        self.push_scope();

                        for (name, val) in bindings {
                            self.current_scope_mut().set(name, val)
                        }

                        let result = self.evaluate_expression(arm.body)?.unwrap_return();

                        self.pop_scope();
                        return Ok(result);
                    }
                }
                Err(VeldError::RuntimeError(
                    "No match arm matched the value".to_string(),
                ))
            }

            _ => Ok(Value::Unit),
        }
    }

    fn patter_matches(
        &self,
        pattern: &MatchPattern,
        value: &Value,
    ) -> Result<Option<HashMap<String, Value>>> {
        match pattern {
            MatchPattern::Wildcard => Ok(Some(HashMap::new())),
            MatchPattern::Literal(lit) => match (lit, value) {
                (Literal::Integer(a), Value::Integer(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::Float(a), Value::Float(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::String(a), Value::String(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::Boolean(a), Value::Boolean(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::Unit, Value::Unit) => Ok(Some(HashMap::new())),
                _ => Ok(None),
            },
            MatchPattern::Identifier(name) => {
                let mut bindings = HashMap::new();
                bindings.insert(name.clone(), value.clone());
                Ok(Some(bindings))
            }
            MatchPattern::Struct { name, fields } => {
                if let Value::Struct {
                    name: value_name,
                    fields: value_fields,
                } = value
                {
                    if name != value_name {
                        return Ok(None); // Wrong struct name
                    }

                    let mut all_bindings = HashMap::new();

                    for (field_name, field_pattern) in fields {
                        if let Some(field_value) = value_fields.get(field_name) {
                            if let Some(pattern) = field_pattern {
                                if let Some(bindings) =
                                    self.patter_matches(&**pattern, field_value)?
                                {
                                    all_bindings.extend(bindings);
                                } else {
                                    return Ok(None); // Field did not match
                                }
                            } else {
                                all_bindings.insert(field_name.clone(), field_value.clone());
                            }
                        } else {
                            return Ok(None);
                        }
                    }
                    Ok(Some(all_bindings))
                } else {
                    Ok(None) // Not a struct
                }
            }
            // TODO - Implement Enum pattern matching
            _ => Ok(None), // Not implemented yet
        }
    }

    fn evaluate_expression(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Literal(lit) => Ok(match lit {
                Literal::Integer(n) => Value::Integer(n),
                Literal::Float(n) => Value::Float(n),
                Literal::String(s) => Value::String(s),
                Literal::Boolean(b) => Value::Boolean(b),
                Literal::Char(c) => Value::Char(c),
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

            Expr::TypeCast { expr, target_type } => {
                let value = self.evaluate_expression(*expr)?;
                let target = self.type_checker.env.from_annotation(&target_type, None)?;
                self.cast_value(value, &target)
            }

            Expr::Lambda {
                params,
                body,
                return_type,
            } => {
                // Convert lambdas to function value
                let param_list = params
                    .iter()
                    .map(|(name, type_opt)| {
                        // Convert optional type annotations to Concrete type(s)
                        let type_annotation = if let Some(ty) = type_opt {
                            ty.clone()
                        } else {
                            // Use placeholder for type inference
                            TypeAnnotation::Basic("any".to_string())
                        };
                        (name.clone(), type_annotation)
                    })
                    .collect();

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
                    }
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
            }
            Expr::IndexAccess { object, index } => {
                let obj_value = self.evaluate_expression(*object)?.unwrap_return();
                let idx_value = self.evaluate_expression(*index)?.unwrap_return();

                match obj_value {
                    Value::Array(elements) => match idx_value {
                        Value::Integer(i) => {
                            if i < 0 || i >= elements.len() as i64 {
                                return Err(VeldError::RuntimeError(format!(
                                    "Array index out of bounds: {}",
                                    i
                                )));
                            }
                            Ok(elements[i as usize].clone())
                        }
                        _ => Err(VeldError::RuntimeError(
                            "Array index must be an integer".to_string(),
                        )),
                    },
                    Value::String(s) => {
                        // Allow indexing into strings
                        match idx_value {
                            Value::Integer(i) => {
                                if i < 0 || i >= s.len() as i64 {
                                    return Err(VeldError::RuntimeError(format!(
                                        "String index out of bounds: {}",
                                        i
                                    )));
                                }

                                let char_value = s.chars().nth(i as usize).unwrap().to_string();
                                Ok(Value::String(char_value))
                            }
                            _ => Err(VeldError::RuntimeError(
                                "String index must be an integer".to_string(),
                            )),
                        }
                    }
                    _ => Err(VeldError::RuntimeError(
                        "Cannot index into non-array value".to_string(),
                    )),
                }
            }
            Expr::EnumVariant {
                enum_name,
                variant_name,
                fields,
            } => {
                // Check if enum exists
                if !self.enums.contains_key(&enum_name) {
                    return Err(VeldError::RuntimeError(format!(
                        "Undefined enum '{}'",
                        enum_name
                    )));
                }

                // Evaluate fields
                let mut field_values = Vec::new();
                for field in fields {
                    let val = self.evaluate_expression(field)?;
                    field_values.push(val.unwrap_return());
                }

                Ok(Value::Enum {
                    enum_name,
                    variant_name,
                    fields: field_values,
                })
            }
            Expr::TupleLiteral(elements) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate_expression(element)?.unwrap_return());
                }
                Ok(Value::Tuple(values))
            }
            Expr::TupleAccess { tuple, index } => {
                let tuple_val = self.evaluate_expression(*tuple)?.unwrap_return();

                match tuple_val {
                    Value::Tuple(elements) => {
                        if index < elements.len() {
                            Ok(elements[index].clone())
                        } else {
                            Err(VeldError::RuntimeError(format!(
                                "Tuple index out of bounds: {}",
                                index
                            )))
                        }
                    }
                    _ => Err(VeldError::RuntimeError(
                        "Cannot access tuple field on non-tuple value".to_string(),
                    )),
                }
            }
            Expr::MacroExpr { name, arguments } => todo!(),
            Expr::TypeCast { expr, target_type } => todo!(),
        }
    }

    fn cast_value(&self, value: Value, target_type: &Type) -> Result<Value> {
        match (value.clone(), target_type) {
            (Value::Integer(i), Type::I8) => Ok(Value::Integer(i as i8 as i64)),
            (Value::Integer(i), Type::I16) => Ok(Value::Integer(i as i16 as i64)),
            (Value::Integer(i), Type::I32) => Ok(Value::Integer(i as i32 as i64)),
            (Value::Integer(i), Type::I64) => Ok(Value::Integer(i)),

            // Integer to Unsigned types
            (Value::Integer(i), Type::U8) => {
                if i < 0 || i > u8::MAX as i64 {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u8: out of range",
                        i
                    )))
                } else {
                    Ok(Value::Integer(i as u8 as i64))
                }
            }
            (Value::Integer(i), Type::U16) => {
                if i < 0 || i > u16::MAX as i64 {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u16: out of range",
                        i
                    )))
                } else {
                    Ok(Value::Integer(i as u16 as i64))
                }
            }
            (Value::Integer(i), Type::U32) => {
                if i < 0 || i > u32::MAX as i64 {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u32: out of range",
                        i
                    )))
                } else {
                    Ok(Value::Integer(i as u32 as i64))
                }
            }
            (Value::Integer(i), Type::U64) => {
                if i < 0 {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u64: out of range",
                        i
                    )))
                } else {
                    Ok(Value::Integer(i as u64 as i64))
                }
            }
            // Integer to Float types
            (Value::Integer(i), Type::F32) => Ok(Value::Float(i as f64)),
            (Value::Integer(i), Type::F64) => Ok(Value::Float(i as f64)),

            // Float to Integer types
            (Value::Float(f), Type::I8) => Ok(Value::Integer(f as i8 as i64)),
            (Value::Float(f), Type::I16) => Ok(Value::Integer(f as i16 as i64)),
            (Value::Float(f), Type::I32) => Ok(Value::Integer(f as i32 as i64)),
            (Value::Float(f), Type::I64) => Ok(Value::Integer(f as i64)),

            // Float to Float types
            (Value::Float(f), Type::F32) => Ok(Value::Float(f as f32 as f64)),
            (Value::Float(f), Type::F64) => Ok(Value::Float(f)),

            // String conversions
            (Value::String(s), Type::I32) => match s.parse::<i32>() {
                Ok(i) => Ok(Value::Integer(i as i64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as i32",
                    s
                ))),
            },
            (Value::String(s), Type::F32) => match s.parse::<f32>() {
                Ok(f) => Ok(Value::Float(f as f64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as f32",
                    s
                ))),
            },
            (Value::String(s), Type::F64) => match s.parse::<f64>() {
                Ok(f) => Ok(Value::Float(f)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as f64",
                    s
                ))),
            },
            (Value::String(s), Type::I64) => match s.parse::<i64>() {
                Ok(i) => Ok(Value::Integer(i)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as i64",
                    s
                ))),
            },
            (Value::String(s), Type::U8) => match s.parse::<u8>() {
                Ok(i) => Ok(Value::Integer(i as i64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as u8",
                    s
                ))),
            },
            (Value::String(s), Type::U16) => match s.parse::<u16>() {
                Ok(i) => Ok(Value::Integer(i as i64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as u16",
                    s
                ))),
            },
            (Value::String(s), Type::U32) => match s.parse::<u32>() {
                Ok(i) => Ok(Value::Integer(i as i64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as u32",
                    s
                ))),
            },
            (Value::String(s), Type::U64) => match s.parse::<u64>() {
                Ok(i) => Ok(Value::Integer(i as i64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as u64",
                    s
                ))),
            },

            (Value::Integer(i), Type::String) => Ok(Value::String(i.to_string())),
            (Value::Float(f), Type::String) => Ok(Value::String(f.to_string())),

            // Invalid cast
            _ => Err(VeldError::RuntimeError(format!(
                "Invalid cast from {:?} to {:?}",
                value, target_type
            ))),
        }
    }

    fn initialize_array_methods(&mut self) {
        // Create array prototype with methods
        let mut array_methods = HashMap::new();

        // Get the last element of the array
        array_methods.insert(
            "last".to_string(),
            Value::Function {
                params: vec![(
                    "self".to_string(),
                    TypeAnnotation::Basic("Array".to_string()),
                )],
                body: vec![], // Built-in method with custom implementation
                return_type: TypeAnnotation::Basic("any".to_string()),
            },
        );

        // Get the first element of the array
        array_methods.insert(
            "first".to_string(),
            Value::Function {
                params: vec![(
                    "self".to_string(),
                    TypeAnnotation::Basic("Array".to_string()),
                )],
                body: vec![],
                return_type: TypeAnnotation::Basic("any".to_string()),
            },
        );

        // Return new array without the last element
        array_methods.insert(
            "init".to_string(),
            Value::Function {
                params: vec![(
                    "self".to_string(),
                    TypeAnnotation::Basic("Array".to_string()),
                )],
                body: vec![],
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // Return new array without the first element
        array_methods.insert(
            "tail".to_string(),
            Value::Function {
                params: vec![(
                    "self".to_string(),
                    TypeAnnotation::Basic("Array".to_string()),
                )],
                body: vec![],
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // Return new array with value added at the end
        array_methods.insert(
            "with".to_string(),
            Value::Function {
                params: vec![
                    (
                        "self".to_string(),
                        TypeAnnotation::Basic("Array".to_string()),
                    ),
                    (
                        "value".to_string(),
                        TypeAnnotation::Basic("any".to_string()),
                    ),
                ],
                body: vec![],
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // Take the first n elements of the array
        array_methods.insert(
            "take".to_string(),
            Value::Function {
                params: vec![
                    (
                        "self".to_string(),
                        TypeAnnotation::Basic("Array".to_string()),
                    ),
                    ("n".to_string(), TypeAnnotation::Basic("i32".to_string())),
                ],
                body: vec![],
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // Drop the first n elements of the array
        array_methods.insert(
            "drop".to_string(),
            Value::Function {
                params: vec![
                    (
                        "self".to_string(),
                        TypeAnnotation::Basic("Array".to_string()),
                    ),
                    ("n".to_string(), TypeAnnotation::Basic("i32".to_string())),
                ],
                body: vec![],
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // length method
        array_methods.insert(
            "len".to_string(),
            Value::Function {
                params: vec![(
                    "self".to_string(),
                    TypeAnnotation::Basic("Array".to_string()),
                )],
                body: vec![], // Built-in method with custom implementation
                return_type: TypeAnnotation::Basic("i32".to_string()),
            },
        );

        // map method
        array_methods.insert(
            "map".to_string(),
            Value::Function {
                params: vec![
                    (
                        "self".to_string(),
                        TypeAnnotation::Basic("Array".to_string()),
                    ),
                    ("fn".to_string(), TypeAnnotation::Basic("any".to_string())),
                ],
                body: vec![], // Built-in method with custom implementation
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // filter method
        array_methods.insert(
            "filter".to_string(),
            Value::Function {
                params: vec![
                    (
                        "self".to_string(),
                        TypeAnnotation::Basic("Array".to_string()),
                    ),
                    ("fn".to_string(), TypeAnnotation::Basic("any".to_string())),
                ],
                body: vec![], // Built-in method with custom implementation
                return_type: TypeAnnotation::Basic("Array".to_string()),
            },
        );

        // Store methods in struct_methods HashMap with a special key
        self.struct_methods
            .insert("Array".to_string(), array_methods);
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
                    let param_name = params[i].0.clone();
                    let arg_value = arg.clone();
                    self.current_scope_mut().set(param_name, arg_value);
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
            Value::Array(elements) => {
                // Handle array properties
                match property {
                    "len" => Ok(Value::Integer(elements.len() as i64)),
                    "last" | "first" | "init" | "tail" | "with" | "take" | "drop" | "map"
                    | "filter" => {
                        // Return a special function that, when called, will invoke the corresponding array method
                        Ok(Value::Function {
                            params: vec![
                                (
                                    "self".to_string(),
                                    TypeAnnotation::Basic("Array".to_string()),
                                ),
                                ("arg".to_string(), TypeAnnotation::Unit),
                            ],
                            body: vec![], // Empty body for built-in methods
                            return_type: TypeAnnotation::Basic("any".to_string()),
                        })
                    }
                    _ => Err(VeldError::RuntimeError(format!(
                        "Property '{}' not found on array",
                        property
                    ))),
                }
            }
            _ => Err(VeldError::RuntimeError(
                "Cannot access property on non-struct value".to_string(),
            )),
        }
    }

    fn call_function_with_single_argument(&mut self, func: &Value, arg: Value) -> Result<Value> {
        match func {
            Value::Function { params, body, .. } => {
                // Direct invocation of function value
                self.push_scope();

                // Bind the argument to the first parameter
                if params.is_empty() {
                    return Err(VeldError::RuntimeError(
                        "Function must take at least one parameter".to_string(),
                    ));
                }

                self.current_scope_mut().set(params[0].0.clone(), arg);

                // Execute function body
                let mut result = Value::Unit;
                for stmt in body {
                    result = self.execute_statement(stmt.clone())?;
                    if matches!(result, Value::Return(_)) {
                        break;
                    }
                }

                self.pop_scope();
                Ok(result.unwrap_return())
            }
            _ => Err(VeldError::RuntimeError(
                "Cannot call non-function value".to_string(),
            )),
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
                "last" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "last() takes no arguments".to_string(),
                        ));
                    }

                    if elements.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "Cannot get last element of empty array".to_string(),
                        ));
                    }

                    return Ok(elements.last().unwrap().clone());
                }

                "first" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "first() takes no arguments".to_string(),
                        ));
                    }

                    if elements.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "Cannot get first element of empty array".to_string(),
                        ));
                    }

                    return Ok(elements.first().unwrap().clone());
                }
                "init" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "init() takes no arguments".to_string(),
                        ));
                    }

                    if elements.is_empty() {
                        return Ok(Value::Array(vec![]));
                    }

                    let mut new_elements = elements.clone();
                    new_elements.pop();
                    return Ok(Value::Array(new_elements));
                }

                "tail" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "tail() takes no arguments".to_string(),
                        ));
                    }

                    if elements.is_empty() {
                        return Ok(Value::Array(vec![]));
                    }

                    let new_elements = elements.iter().skip(1).cloned().collect();
                    return Ok(Value::Array(new_elements));
                }
                "with" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "with() takes exactly one argument".to_string(),
                        ));
                    }

                    let mut new_elements = elements.clone();
                    new_elements.push(args[0].clone());
                    return Ok(Value::Array(new_elements));
                }

                "take" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "take() takes exactly one argument".to_string(),
                        ));
                    }

                    if let Value::Integer(n) = &args[0] {
                        if *n < 0 {
                            return Err(VeldError::RuntimeError(
                                "take() argument must be non-negative".to_string(),
                            ));
                        }

                        let n = *n as usize;
                        let new_elements = elements.iter().take(n).cloned().collect();
                        return Ok(Value::Array(new_elements));
                    } else {
                        return Err(VeldError::RuntimeError(
                            "take() argument must be an integer".to_string(),
                        ));
                    }
                }

                "drop" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "drop() takes exactly one argument".to_string(),
                        ));
                    }

                    if let Value::Integer(n) = &args[0] {
                        if *n < 0 {
                            return Err(VeldError::RuntimeError(
                                "drop() argument must be non-negative".to_string(),
                            ));
                        }

                        let n = *n as usize;
                        let new_elements = elements.iter().skip(n).cloned().collect();
                        return Ok(Value::Array(new_elements));
                    } else {
                        return Err(VeldError::RuntimeError(
                            "drop() argument must be an integer".to_string(),
                        ));
                    }
                }
                "len" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "len() takes no arguments".to_string(),
                        ));
                    }

                    return Ok(Value::Integer(elements.len() as i64));
                }
                "map" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "map() takes exactly one function argument".to_string(),
                        ));
                    }

                    let func = &args[0];
                    let mut result = Vec::new();

                    for element in elements {
                        let mapped =
                            self.call_function_with_single_argument(func, element.clone())?;
                        result.push(mapped);
                    }

                    return Ok(Value::Array(result));
                }
                "filter" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "filter() takes exactly one function argument".to_string(),
                        ));
                    }

                    let func = &args[0];
                    let mut result = Vec::new();

                    for element in elements {
                        let pred_result =
                            self.call_function_with_single_argument(func, element.clone())?;
                        if self.is_truthy(pred_result) {
                            result.push(element.clone());
                        }
                    }

                    return Ok(Value::Array(result));
                }
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
        if let Some(result) = self.try_call_operator_method(&left, &operator, &right)? {
            return Ok(result);
        }

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
            (Value::Integer(a), BinaryOperator::Modulo, Value::Integer(b)) => {
                if b == 0 {
                    Err(VeldError::RuntimeError("Modulo by zero".to_string()))
                } else {
                    Ok(Value::Integer(a % b))
                }
            }

            // Float modulo (remainder operation)
            (Value::Float(a), BinaryOperator::Modulo, Value::Float(b)) => {
                if b == 0.0 {
                    Err(VeldError::RuntimeError("Modulo by zero".to_string()))
                } else {
                    Ok(Value::Float(a % b))
                }
            }

            // Mixed types - convert integer to float
            (Value::Integer(a), BinaryOperator::Modulo, Value::Float(b)) => {
                if b == 0.0 {
                    Err(VeldError::RuntimeError("Modulo by zero".to_string()))
                } else {
                    Ok(Value::Float((a as f64) % b))
                }
            }

            (Value::Float(a), BinaryOperator::Modulo, Value::Integer(b)) => {
                if b == 0 {
                    Err(VeldError::RuntimeError("Modulo by zero".to_string()))
                } else {
                    Ok(Value::Float(a % (b as f64)))
                }
            }
            // String operations
            (Value::String(a), BinaryOperator::Add, Value::String(b)) => Ok(Value::String(a + &b)),
            (Value::String(a), BinaryOperator::EqualEqual, Value::String(b)) => {
                Ok(Value::Boolean(a == b))
            }
            (Value::String(a), BinaryOperator::NotEqual, Value::String(b)) => {
                Ok(Value::Boolean(a != b))
            }
            (Value::String(a), BinaryOperator::Less, Value::String(b)) => Ok(Value::Boolean(a < b)),
            (Value::String(a), BinaryOperator::Greater, Value::String(b)) => {
                Ok(Value::Boolean(a > b))
            }
            (Value::String(a), BinaryOperator::LessEq, Value::String(b)) => {
                Ok(Value::Boolean(a <= b))
            }
            (Value::String(a), BinaryOperator::GreaterEq, Value::String(b)) => {
                Ok(Value::Boolean(a >= b))
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
                )));
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

    fn execute_module_declaration(
        &mut self,
        name: String,
        body: Option<Vec<Statement>>,
        is_public: bool,
    ) -> Result<Value> {
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

    fn execute_import_declaration(
        &mut self,
        path: Vec<String>,
        items: Vec<ImportItem>,
        alias: Option<String>,
        is_public: bool,
    ) -> Result<Value> {
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
                let module = self
                    .module_manager
                    .get_module(&module_path_str)
                    .ok_or_else(|| VeldError::RuntimeError("Module not found".to_string()))?;

                match export_item {
                    ExportedItem::Function(idx) => {
                        if let Statement::FunctionDeclaration {
                            name: fn_name,
                            params,
                            return_type,
                            body,
                            ..
                        } = &module.statements[idx]
                        {
                            let function = Value::Function {
                                params: params.clone(),
                                body: body.clone(),
                                return_type: return_type.clone(),
                            };
                            self.current_scope_mut().set(name.clone(), function);
                        }
                    }
                    ExportedItem::Struct(idx) => {
                        // Handle struct imports - for now just remember the name
                        if let Statement::StructDeclaration {
                            name: struct_name,
                            fields,
                            ..
                        } = &module.statements[idx]
                        {
                            // Register the struct type in this scope
                            self.structs.insert(name, fields.clone());
                        }
                    }
                    // TODO - Handle other items here
                    _ => {}
                }
            }
        }
        Ok(Value::Unit)
    }

    fn initialize_operator_kinds(&mut self) -> Result<()> {
        let ordering_variants = vec![
            EnumVariant {
                name: "Less".to_string(),
                fields: None,
            },
            EnumVariant {
                name: "Equal".to_string(),
                fields: None,
            },
            EnumVariant {
                name: "Greater".to_string(),
                fields: None,
            },
        ];

        self.enums.insert("Ordering".to_string(), ordering_variants);

        let mut add_kind_methods = HashMap::new();
        use crate::types::Type;

        let add_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };

        add_kind_methods.insert("add".to_string(), add_method_type);

        self.type_checker.env.add_kind(
            "Add",
            add_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        let mut sub_kind_methods = HashMap::new();
        let sub_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };
        sub_kind_methods.insert("sub".to_string(), sub_method_type);
        self.type_checker.env.add_kind(
            "Sub",
            sub_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        let mut mul_kind_methods = HashMap::new();
        let mul_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };
        mul_kind_methods.insert("mul".to_string(), mul_method_type);
        self.type_checker.env.add_kind(
            "Mul",
            mul_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        let mut div_kind_methods = HashMap::new();
        let div_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };
        div_kind_methods.insert("div".to_string(), div_method_type);
        self.type_checker.env.add_kind(
            "Div",
            div_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        Ok(())
    }

    fn try_call_operator_method(
        &mut self,
        left: &Value,
        op: &BinaryOperator,
        right: &Value,
    ) -> Result<Option<Value>> {
        let method_name = match op {
            BinaryOperator::Add => "add",
            BinaryOperator::Subtract => "sub",
            BinaryOperator::Multiply => "mul",
            BinaryOperator::Divide => "div",
            BinaryOperator::Modulo => "rem",
            _ => return Ok(None), // Not an operator we can handle
        };

        if let Value::Struct {
            name: struct_name, ..
        } = left
        {
            if let Some(methods) = self.struct_methods.get(struct_name) {
                if let Some(_) = methods.get(method_name) {
                    let args = vec![right.clone()];
                    let result =
                        self.call_method_value(left.clone(), method_name.to_string(), args)?;
                    return Ok(Some(result));
                }
            }
        }
        Ok(None) // No operator method found
    }
}
