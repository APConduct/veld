use crate::ast::{Expr, Statement, StructMethod, TypeAnnotation};
use crate::error::{Result, VeldError};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    I32,
    F64,
    Bool,
    String,
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },
    // Other types to be added
}

impl Type {
    pub fn from_annotation(annotation: &TypeAnnotation) -> Result<Self> {
        match annotation {
            TypeAnnotation::Unit => Ok(Type::Unit),
            TypeAnnotation::Basic(name) => match name.as_str() {
                "i32" => Ok(Type::I32),
                "f64" => Ok(Type::F64),
                "bool" => Ok(Type::Bool),
                "str" => Ok(Type::String),
                _ => Err(VeldError::TypeError(format!("Unknown type: {}", name))),
            },
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                let param_types = params
                    .iter()
                    .map(|p| Type::from_annotation(p))
                    .collect::<Result<Vec<_>>>()?;
                let return_type = Type::from_annotation(return_type)?;

                Ok(Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                })
            }
            // We'll handle generics and other complex types later
            _ => Err(VeldError::TypeError("Unsupported type annotation".into())),
        }
    }
}

pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, Type>>,
    structs: HashMap<String, HashMap<String, Type>>, // struct name -> (field name -> type)
    struct_methods: HashMap<String, HashMap<String, Type>>, // struct name -> (method name -> type)
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, name: &str, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    pub fn get(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    pub fn add_struct(&mut self, name: &str, fields: HashMap<String, Type>) {
        self.structs.insert(name.to_string(), fields);
    }

    pub fn add_struct_method(&mut self, struct_name: &str, method_name: &str, method_type: Type) {
        let methods = self
            .struct_methods
            .entry(struct_name.to_string())
            .or_insert_with(HashMap::new);

        methods.insert(method_name.to_string(), method_type);
    }

    pub fn get_struct_field_type(&self, struct_name: &str, field_name: &str) -> Option<Type> {
        self.structs
            .get(struct_name)
            .and_then(|fields| fields.get(field_name).cloned())
    }

    pub fn get_struct_method_type(&self, struct_name: &str, method_name: &str) -> Option<Type> {
        self.struct_methods
            .get(struct_name)
            .and_then(|methods| methods.get(method_name).cloned())
    }
}

pub struct TypeChecker {
    env: TypeEnvironment,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
        }
    }

    pub fn check_program(&mut self, statements: &[Statement]) -> Result<()> {
        // First pass: register all struct types
        for stmt in statements {
            if let Statement::StructDeclaration { name, fields, .. } = stmt {
                let mut field_types = HashMap::new();

                for (field_name, field_type) in fields {
                    let ty = Type::from_annotation(field_type)?;
                    field_types.insert(field_name.clone(), ty);
                }

                self.env.add_struct(name, field_types);
            }
        }

        // Second pass: check all statements
        for stmt in statements {
            self.check_statement(stmt)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                ..
            } => {
                let return_type = Type::from_annotation(return_type)?;

                let param_types: Vec<Type> = params
                    .iter()
                    .map(|(_, ty)| Type::from_annotation(ty))
                    .collect::<Result<Vec<_>>>()?;

                let func_type = Type::Function {
                    params: param_types.clone(),
                    return_type: Box::new(return_type.clone()),
                };

                self.env.define(name, func_type);

                // Check function body
                self.env.push_scope();

                for (i, (param_name, _)) in params.iter().enumerate() {
                    self.env.define(param_name, param_types[i].clone());
                }

                for body_stmt in body {
                    self.check_statement(body_stmt)?;
                }

                self.env.pop_scope();
            }
            Statement::ProcDeclaration { name, params, body } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|(_, ty)| Type::from_annotation(ty))
                    .collect::<Result<Vec<_>>>()?;

                let func_type = Type::Function {
                    params: param_types.clone(),
                    return_type: Box::new(Type::Unit),
                };

                self.env.define(name, func_type);

                // Check function body
                self.env.push_scope();

                for (i, (param_name, _)) in params.iter().enumerate() {
                    self.env.define(param_name, param_types[i].clone());
                }

                for body_stmt in body {
                    self.check_statement(body_stmt)?;
                }

                self.env.pop_scope();
            }
            Statement::StructDeclaration {
                name,
                fields: _,
                methods,
            } => {
                // Register methods for this struct
                for method in methods {
                    self.check_struct_method(name, method)?;
                }
            }
            Statement::Implementation {
                type_name,
                kind_name: _,
                methods,
            } => {
                // Check that the struct type exists
                if !self.env.structs.contains_key(type_name) {
                    return Err(VeldError::TypeError(format!(
                        "Cannot implement for undefined type '{}'",
                        type_name
                    )));
                }

                // Check all methods
                for method in methods {
                    let param_types: Vec<Type> = method
                        .params
                        .iter()
                        .map(|(_, ty)| Type::from_annotation(ty))
                        .collect::<Result<Vec<_>>>()?;

                    let return_type = Type::from_annotation(&method.return_type)?;

                    let method_type = Type::Function {
                        params: param_types.clone(),
                        return_type: Box::new(return_type.clone()),
                    };

                    self.env
                        .add_struct_method(type_name, &method.name, method_type);

                    // Check method body
                    self.env.push_scope();

                    // Add self and other parameters
                    for (i, (param_name, _)) in method.params.iter().enumerate() {
                        self.env.define(param_name, param_types[i].clone());
                    }

                    for body_stmt in &method.body {
                        self.check_statement(body_stmt)?;
                    }

                    self.env.pop_scope();
                }
            }
            Statement::VariableDeclaration {
                name,
                type_annotation,
                value,
            } => {
                let value_type = self.check_expression(value)?;

                let var_type = if let Some(anno) = type_annotation {
                    let specified_type = Type::from_annotation(anno)?;

                    // Check that the value matches the specified type
                    if !self.types_compatible(&specified_type, &value_type) {
                        return Err(VeldError::TypeError(format!(
                            "Type mismatch: expected {:?}, got {:?}",
                            specified_type, value_type
                        )));
                    }

                    specified_type
                } else {
                    value_type
                };

                self.env.define(name, var_type);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(VeldError::TypeError(format!(
                        "Condition must be boolean, got {:?}",
                        cond_type
                    )));
                }

                self.env.push_scope();
                for stmt in then_branch {
                    self.check_statement(stmt)?;
                }
                self.env.pop_scope();

                if let Some(else_stmts) = else_branch {
                    self.env.push_scope();
                    for stmt in else_stmts {
                        self.check_statement(stmt)?;
                    }
                    self.env.pop_scope();
                }
            }
            Statement::While { condition, body } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(VeldError::TypeError(format!(
                        "Condition must be boolean, got {:?}",
                        cond_type
                    )));
                }

                self.env.push_scope();
                for stmt in body {
                    self.check_statement(stmt)?;
                }
                self.env.pop_scope();
            }
            Statement::For {
                iterator,
                iterable: _,
                body,
            } => {
                // For now, just typechecking the body
                self.env.push_scope();
                // We would need proper iterable typechecking here
                self.env.define(iterator, Type::Unit); // Placeholder

                for stmt in body {
                    self.check_statement(stmt)?;
                }
                self.env.pop_scope();
            }
            Statement::Return(expr_opt) => {
                // We would need to check against the enclosing function's return type
                if let Some(expr) = expr_opt {
                    self.check_expression(expr)?;
                }
            }
            Statement::ExprStatement(expr) => {
                self.check_expression(expr)?;
            }
            _ => (), // Other statement types
        }

        Ok(())
    }

    fn check_struct_method(&mut self, struct_name: &str, method: &StructMethod) -> Result<()> {
        let param_types: Vec<Type> = method
            .params
            .iter()
            .map(|(_, ty)| Type::from_annotation(ty))
            .collect::<Result<Vec<_>>>()?;

        let return_type = Type::from_annotation(&method.return_type)?;

        let method_type = Type::Function {
            params: param_types.clone(),
            return_type: Box::new(return_type.clone()),
        };

        self.env
            .add_struct_method(struct_name, &method.name, method_type);

        // Check method body
        self.env.push_scope();

        // Add self and other parameters
        for (i, (param_name, _)) in method.params.iter().enumerate() {
            self.env.define(param_name, param_types[i].clone());
        }

        for body_stmt in &method.body {
            self.check_statement(body_stmt)?;
        }

        self.env.pop_scope();

        Ok(())
    }

    fn check_expression(&mut self, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Literal(lit) => self.check_literal(lit),
            Expr::UnitLiteral => Ok(Type::Unit),
            Expr::Identifier(name) => self
                .env
                .get(name)
                .ok_or_else(|| VeldError::TypeError(format!("Undefined variable: {}", name))),
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                self.check_binary_op(&left_type, operator, &right_type)
            }
            Expr::FunctionCall { name, arguments } => {
                if name.contains('.') {
                    // Method call
                    let parts: Vec<&str> = name.split('.').collect();
                    if parts.len() == 2 {
                        return self.check_method_call(parts[0], parts[1], arguments);
                    }
                }

                let func_type = self
                    .env
                    .get(name)
                    .ok_or_else(|| VeldError::TypeError(format!("Undefined function: {}", name)))?;

                match func_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        if params.len() != arguments.len() {
                            return Err(VeldError::TypeError(format!(
                                "Function {} expects {} arguments, got {}",
                                name,
                                params.len(),
                                arguments.len()
                            )));
                        }

                        for (i, arg) in arguments.iter().enumerate() {
                            let arg_type = self.check_expression(arg)?;
                            if !self.types_compatible(&params[i], &arg_type) {
                                return Err(VeldError::TypeError(format!(
                                    "Type mismatch in argument {}: expected {:?}, got {:?}",
                                    i, params[i], arg_type
                                )));
                            }
                        }

                        Ok(*return_type)
                    }
                    _ => Err(VeldError::TypeError(format!("{} is not a function", name))),
                }
            }
            Expr::Lambda { .. } => {
                // We'll implement this later
                Err(VeldError::TypeError(
                    "Lambda expressions not yet supported".into(),
                ))
            }
        }
    }

    fn check_method_call(
        &mut self,
        struct_name: &str,
        method_name: &str,
        arguments: &[Expr],
    ) -> Result<Type> {
        let struct_type = self
            .env
            .get(struct_name)
            .ok_or_else(|| VeldError::TypeError(format!("Undefined variable: {}", struct_name)))?;

        let struct_name = match &struct_type {
            Type::Struct { name, .. } => name.clone(),
            _ => {
                return Err(VeldError::TypeError(format!(
                    "{} is not a struct",
                    struct_name
                )))
            }
        };

        let method_type = self
            .env
            .get_struct_method_type(&struct_name, method_name)
            .ok_or_else(|| {
                VeldError::TypeError(format!(
                    "Method {} not found on struct {}",
                    method_name, struct_name
                ))
            })?;

        match method_type {
            Type::Function {
                params,
                return_type,
            } => {
                if params.len() - 1 != arguments.len() {
                    // -1 for self
                    return Err(VeldError::TypeError(format!(
                        "Method {} expects {} arguments, got {}",
                        method_name,
                        params.len() - 1,
                        arguments.len()
                    )));
                }

                for (i, arg) in arguments.iter().enumerate() {
                    let arg_type = self.check_expression(arg)?;
                    // Skip the first parameter (self)
                    if !self.types_compatible(&params[i + 1], &arg_type) {
                        return Err(VeldError::TypeError(format!(
                            "Type mismatch in argument {}: expected {:?}, got {:?}",
                            i,
                            params[i + 1],
                            arg_type
                        )));
                    }
                }

                Ok(*return_type)
            }
            _ => Err(VeldError::TypeError(format!(
                "{}.{} is not a method",
                struct_name, method_name
            ))),
        }
    }

    fn check_literal(&self, lit: &crate::ast::Literal) -> Result<Type> {
        match lit {
            crate::ast::Literal::Integer(_) => Ok(Type::I32),
            crate::ast::Literal::Float(_) => Ok(Type::F64),
            crate::ast::Literal::String(_) => Ok(Type::String),
            crate::ast::Literal::Boolean(_) => Ok(Type::Bool),
            crate::ast::Literal::Unit => Ok(Type::Unit),
        }
    }

    fn check_binary_op(
        &self,
        left: &Type,
        op: &crate::ast::BinaryOperator,
        right: &Type,
    ) -> Result<Type> {
        match (left, op, right) {
            // Numeric operations
            (Type::I32, crate::ast::BinaryOperator::Add, Type::I32) => Ok(Type::I32),
            (Type::I32, crate::ast::BinaryOperator::Subtract, Type::I32) => Ok(Type::I32),
            (Type::I32, crate::ast::BinaryOperator::Multiply, Type::I32) => Ok(Type::I32),
            (Type::I32, crate::ast::BinaryOperator::Divide, Type::I32) => Ok(Type::I32),

            (Type::F64, crate::ast::BinaryOperator::Add, Type::F64) => Ok(Type::F64),
            (Type::F64, crate::ast::BinaryOperator::Subtract, Type::F64) => Ok(Type::F64),
            (Type::F64, crate::ast::BinaryOperator::Multiply, Type::F64) => Ok(Type::F64),
            (Type::F64, crate::ast::BinaryOperator::Divide, Type::F64) => Ok(Type::F64),

            // Comparison operations
            (Type::I32, crate::ast::BinaryOperator::LessEq, Type::I32) => Ok(Type::Bool),
            (Type::I32, crate::ast::BinaryOperator::GreaterEq, Type::I32) => Ok(Type::Bool),
            (Type::I32, crate::ast::BinaryOperator::Less, Type::I32) => Ok(Type::Bool),
            (Type::I32, crate::ast::BinaryOperator::Greater, Type::I32) => Ok(Type::Bool),
            (Type::I32, crate::ast::BinaryOperator::EqualEqual, Type::I32) => Ok(Type::Bool),
            (Type::I32, crate::ast::BinaryOperator::NotEqual, Type::I32) => Ok(Type::Bool),

            // Boolean operations
            (Type::Bool, crate::ast::BinaryOperator::And, Type::Bool) => Ok(Type::Bool),
            (Type::Bool, crate::ast::BinaryOperator::Or, Type::Bool) => Ok(Type::Bool),

            // Add more operation type rules
            _ => Err(VeldError::TypeError(format!(
                "Invalid operation: {:?} {:?} {:?}",
                left, op, right
            ))),
        }
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        if expected == actual {
            return true;
        }

        // Add more complex type compatibility rules as needed
        false
    }
}
