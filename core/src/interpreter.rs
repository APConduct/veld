use crate::ast::{
    Argument, BinaryOperator, EnumVariant, Expr, GenericArgument, ImportItem, Literal,
    MacroExpansion, MacroPattern, MatchPattern, Statement, StructField, TypeAnnotation, VarKind,
};
use crate::error::{Result, VeldError};
use crate::module::{ExportedItem, ModuleManager};
use crate::types::{FloatValue, IntegerValue, NumericValue, Type, TypeChecker};
use std::collections::HashMap;
use std::collections::HashSet;
use std::env::consts::OS;
use std::path::Path;

enum PatternToken {
    Literal(String),
    Variable(String),
    Repetition {
        variable: String,
        separator: Option<String>,
        min: usize, // 0 for * (zero or more), 1 for + (one or more)
    },
}

#[derive(Debug, Clone, PartialEq)]
struct VariableInfo {
    value: Value,
    var_kind: VarKind,
    is_initialized: bool,
}

impl VariableInfo {
    fn new(value: Value, var_kind: VarKind) -> Self {
        Self {
            value,
            var_kind,
            is_initialized: true,
        }
    }

    fn can_mutate(&self) -> bool {
        matches!(self.var_kind, VarKind::Var | VarKind::LetMut)
    }

    fn can_shadow(&self) -> bool {
        matches!(self.var_kind, VarKind::Let)
    }
}

#[derive(Debug, Clone)]
struct StructInfo {
    fields: Vec<(String, TypeAnnotation)>,
    generic_params: Vec<GenericArgument>,
}

impl StructInfo {
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            generic_params: Vec::new(),
        }
    }
    pub fn with_fields(fields: Vec<(String, TypeAnnotation)>) -> Self {
        Self {
            fields,
            generic_params: Vec::new(),
        }
    }
    pub fn with_generic_params(
        fields: Vec<(String, TypeAnnotation)>,
        generic_params: Vec<GenericArgument>,
    ) -> Self {
        Self {
            fields,
            generic_params,
        }
    }
    pub fn from(
        fields: Vec<(String, TypeAnnotation)>,
        generic_params: Vec<GenericArgument>,
    ) -> Self {
        Self {
            fields,
            generic_params,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Numeric(NumericValue),
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
        captured_vars: HashMap<String, Value>, // Captured variables for closures
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

    pub fn type_of(&self) -> Type {
        match self {
            Value::Numeric(numeric) => numeric.clone().type_of(),
            Value::Integer(_) => Type::I64,
            Value::Float(_) => Type::F64,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Bool,
            Value::Char(_) => Type::Char,
            Value::Return(_) => Type::Unit, // Return values are not a type
            Value::Unit => Type::Unit,
            Value::Array(_) => Type::Array(Box::new(Type::Any)), // Default to Any for arrays
            Value::Break | Value::Continue => Type::Unit, // Break and Continue are control flow, not values
            _ => todo!("Handle other Value types"),
        }
    }

    pub fn perform_binary_op(&self, op: &BinaryOperator, rhs: &Value) -> Result<Value> {
        match (self, rhs) {
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Numeric(a.perform_operation(op, b)?))
            }
            // TODO: Handle other numeric types
            _ => Err(VeldError::RuntimeError(format!(
                "Invalid operation {:?} between {:?} and {:?}",
                op, self, rhs
            ))),
        }
    }
}

#[derive(Debug, Default)]
struct Scope {
    values: HashMap<String, Value>,
    var_kinds: HashMap<String, VarKind>,
    scope_level: usize,
}

impl Scope {
    fn new(level: usize) -> Self {
        Self {
            values: HashMap::new(),
            var_kinds: HashMap::new(),
            scope_level: level,
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    fn set(&mut self, name: String, value: Value) {
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

    fn declare(&mut self, name: String, value: Value, kind: VarKind) -> Result<()> {
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

    fn assign(&mut self, name: &str, value: Value) -> Result<()> {
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

pub struct Interpreter {
    scopes: Vec<Scope>,
    structs: HashMap<String, Vec<StructField>>, // struct name -> fields
    struct_methods: HashMap<String, HashMap<String, Value>>, // struct name -> (method name -> method)
    generic_structs: HashMap<String, StructInfo>,
    module_manager: ModuleManager,
    current_module: String,
    imported_modules: HashMap<String, String>, // alias -> module name
    enums: HashMap<String, Vec<EnumVariant>>,
    pub type_checker: TypeChecker,
}

impl Interpreter {
    pub fn new<P: AsRef<Path>>(root_dir: P) -> Self {
        let mut interpreter = Self {
            scopes: vec![Scope::new(0)],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
            generic_structs: HashMap::new(),
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

        // First pass: register all function declarations
        for stmt in &statements {
            if let Statement::FunctionDeclaration { .. } = stmt {
                self.execute_statement(stmt.clone())?;
            }
        }

        // Second pass: execute everything else
        let mut last_value = Value::Unit;
        for stmt in statements {
            if !matches!(stmt, Statement::FunctionDeclaration { .. }) {
                last_value = self.execute_statement(stmt)?;
            }
        }

        println!("Interpretation complete");
        Ok(last_value)
    }

    fn initialize_std_modules(&mut self) {
        todo!("Load standard library modules");
    }

    fn collect_free_variables_expr(
        &self,
        expr: &Expr,
        bound_vars: &HashSet<String>,
        free_vars: &mut HashSet<String>,
    ) {
        match expr {
            Expr::Identifier(name) => {
                if !bound_vars.contains(name) {
                    free_vars.insert(name.clone());
                }
            }
            Expr::BinaryOp {
                left,
                operator: _,
                right,
            } => {
                self.collect_free_variables_expr(left, bound_vars, free_vars);
                self.collect_free_variables_expr(right, bound_vars, free_vars);
            }
            Expr::FunctionCall { name: _, arguments } => {
                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.collect_free_variables_expr(expr, bound_vars, free_vars);
                        }
                        Argument::Named { name: _, value } => {
                            self.collect_free_variables_expr(value, bound_vars, free_vars);
                        }
                    }
                }
            }
            Expr::Lambda {
                params,
                body,
                return_type: _,
            } => {
                let mut lambda_bound = bound_vars.clone();
                for (param_name, _) in params {
                    lambda_bound.insert(param_name.clone());
                }
                self.collect_free_variables_expr(body, &lambda_bound, free_vars);
            }
            Expr::BlockLambda {
                params,
                body,
                return_type: _,
            } => {
                let mut lambda_bound = bound_vars.clone();
                for (param_name, _) in params {
                    lambda_bound.insert(param_name.clone());
                }
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, &lambda_bound, free_vars);
                }
            }
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_free_variables_expr(condition, bound_vars, free_vars);
                self.collect_free_variables_expr(then_expr, bound_vars, free_vars);
                if let Some(else_expr) = else_expr {
                    self.collect_free_variables_expr(else_expr, bound_vars, free_vars);
                }
            }
            Expr::BlockExpression {
                statements,
                final_expr,
            } => {
                let mut block_bound = bound_vars.clone();
                for stmt in statements {
                    // Add any variables declared in this block to bound_vars
                    if let Statement::VariableDeclaration { name, .. } = stmt {
                        block_bound.insert(name.clone());
                    }
                    self.collect_free_variables_stmt(stmt, &block_bound, free_vars);
                }
                if let Some(final_expr) = final_expr {
                    self.collect_free_variables_expr(final_expr, &block_bound, free_vars);
                }
            }
            Expr::MethodCall {
                object,
                method: _,
                arguments,
            } => {
                self.collect_free_variables_expr(object, bound_vars, free_vars);
                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.collect_free_variables_expr(expr, bound_vars, free_vars);
                        }
                        Argument::Named { name: _, value } => {
                            self.collect_free_variables_expr(value, bound_vars, free_vars);
                        }
                    }
                }
            }
            Expr::PropertyAccess {
                object,
                property: _,
            } => {
                self.collect_free_variables_expr(object, bound_vars, free_vars);
            }
            Expr::StructCreate {
                struct_name: _,
                fields,
            } => {
                for (_, expr) in fields {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::ArrayLiteral(exprs) => {
                for expr in exprs {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::IndexAccess { object, index } => {
                self.collect_free_variables_expr(object, bound_vars, free_vars);
                self.collect_free_variables_expr(index, bound_vars, free_vars);
            }
            Expr::EnumVariant {
                enum_name: _,
                variant_name: _,
                fields,
            } => {
                for expr in fields {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::TupleLiteral(exprs) => {
                for expr in exprs {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::TupleAccess { tuple, index: _ } => {
                self.collect_free_variables_expr(tuple, bound_vars, free_vars);
            }
            Expr::MacroExpr { name: _, arguments } => {
                for expr in arguments {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::TypeCast {
                expr,
                target_type: _,
            } => {
                self.collect_free_variables_expr(expr, bound_vars, free_vars);
            }
            // Literals don't reference variables
            Expr::Literal(_) | Expr::UnitLiteral => {}
        }
    }

    fn get_current_module(&self) -> &str {
        &self.current_module
    }

    fn check_visibility(&self, is_public: bool, target_module: &str) -> Result<()> {
        if !is_public && self.current_module != target_module {
            return Err(VeldError::RuntimeError(format!(
                "Cannot access private item from module '{}'",
                target_module
            )));
        }
        Ok(())
    }

    fn check_module_exists(&self, module_name: &str) -> Result<()> {
        if !self.module_manager.is_module_loaded(module_name) {
            return Err(VeldError::RuntimeError(format!(
                "Module '{}' not found",
                module_name
            )));
        }
        Ok(())
    }

    fn check_module_access(&self, module_name: &str) -> Result<()> {
        let module = self.module_manager.get_module(module_name).ok_or_else(|| {
            VeldError::RuntimeError(format!("Module '{}' not found", module_name))
        })?;

        // TODO: Implement access control logic here

        Ok(())
    }

    /// Collect free variables in a statement
    fn collect_free_variables_stmt(
        &self,
        stmt: &Statement,
        bound_vars: &HashSet<String>,
        free_vars: &mut HashSet<String>,
    ) {
        match stmt {
            Statement::ExprStatement(expr) => {
                self.collect_free_variables_expr(expr, bound_vars, free_vars);
            }
            Statement::VariableDeclaration {
                name: _,
                var_kind: _,
                type_annotation: _,
                value,
                ..
            } => {
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::Assignment { name: _, value } => {
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::CompoundAssignment {
                name: _,
                operator: _,
                value,
            } => {
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_free_variables_expr(condition, bound_vars, free_vars);
                for stmt in then_branch {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                    }
                }
            }
            Statement::While { condition, body } => {
                self.collect_free_variables_expr(condition, bound_vars, free_vars);
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
            }
            Statement::For {
                iterator: _,
                iterable,
                body,
            } => {
                self.collect_free_variables_expr(iterable, bound_vars, free_vars);
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
            }
            Statement::Return(Some(expr)) => {
                self.collect_free_variables_expr(expr, bound_vars, free_vars);
            }
            Statement::BlockScope { body } => {
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
            }
            // Other statements don't reference variables in ways that matter for closures
            _ => {}
        }
    }

    /// Capture the current values of free variables
    fn capture_variables(&self, free_vars: &HashSet<String>) -> HashMap<String, Value> {
        let mut captured = HashMap::new();
        for var_name in free_vars {
            if let Some(value) = self.get_variable(var_name) {
                captured.insert(var_name.clone(), value);
            }
        }
        captured
    }

    /// Create a lambda with captured variables
    fn create_lambda(
        &self,
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Box<Expr>,
        return_type: Option<TypeAnnotation>,
    ) -> Value {
        // Collect parameter names as bound variables
        let mut bound_vars = HashSet::new();
        for (param_name, _) in &params {
            bound_vars.insert(param_name.clone());
        }

        // Find free variables in the lambda body
        let mut free_vars = HashSet::new();
        self.collect_free_variables_expr(&body, &bound_vars, &mut free_vars);

        // Capture the current values of free variables
        let captured_vars = self.capture_variables(&free_vars);

        println!(
            "Lambda created with captured variables: {:?}",
            captured_vars.keys().collect::<Vec<_>>()
        );

        let actual_return_type = match return_type {
            Some(type_anno) => type_anno,
            None => {
                // Try to infer return type from body
                match body.as_ref() {
                    Expr::Literal(Literal::String(_)) => TypeAnnotation::Basic("str".to_string()),
                    Expr::Literal(Literal::Integer(_)) => TypeAnnotation::Basic("i32".to_string()),
                    Expr::Literal(Literal::Float(_)) => TypeAnnotation::Basic("f64".to_string()),
                    Expr::Literal(Literal::Boolean(_)) => TypeAnnotation::Basic("bool".to_string()),
                    Expr::Literal(Literal::Char(_)) => TypeAnnotation::Basic("char".to_string()),
                    _ => TypeAnnotation::Basic("infer".to_string()),
                }
            }
        };

        Value::Function {
            params: params
                .into_iter()
                .map(|(name, type_anno)| (name, type_anno.unwrap_or(TypeAnnotation::Unit)))
                .collect(),
            body: vec![Statement::Return(Some(*body))],
            return_type: actual_return_type,
            captured_vars,
        }
    }

    /// Create a block lambda with captured variables
    fn create_block_lambda(
        &self,
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Vec<Statement>,
        return_type: Option<TypeAnnotation>,
    ) -> Value {
        // Collect parameter names as bound variables
        let mut bound_vars = HashSet::new();
        for (param_name, _) in &params {
            bound_vars.insert(param_name.clone());
        }

        // Find free variables in the lambda body
        let mut free_vars = HashSet::new();
        for stmt in &body {
            self.collect_free_variables_stmt(stmt, &bound_vars, &mut free_vars);
        }

        // Capture the current values of free variables
        let captured_vars = self.capture_variables(&free_vars);

        println!(
            "Block lambda created with captured variables: {:?}",
            captured_vars.keys().collect::<Vec<_>>()
        );

        let actual_return_type = return_type.unwrap_or(TypeAnnotation::Unit);

        Value::Function {
            params: params
                .into_iter()
                .map(|(name, type_anno)| (name, type_anno.unwrap_or(TypeAnnotation::Unit)))
                .collect(),
            body,
            return_type: actual_return_type,
            captured_vars,
        }
    }

    fn register_generic_struct(&mut self, name: &str, info: StructInfo) -> Result<()> {
        self.generic_structs.insert(name.to_string(), info.clone());
        let struct_fields: Vec<StructField> = info
            .fields
            .iter()
            .map(|(name, type_annotation)| {
                StructField {
                    name: name.clone(),
                    type_annotation: type_annotation.clone(),
                    is_public: true, // Default to public for now
                }
            })
            .collect();
        self.structs.insert(name.to_string(), struct_fields);
        Ok(())
    }

    fn value_to_expr(&self, value: Value) -> Result<Expr> {
        match value {
            Value::Numeric(NumericValue::Integer(IntegerValue::I64(n))) => {
                Ok(Expr::Literal(Literal::Integer(n)))
            }
            Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => {
                Ok(Expr::Literal(Literal::Float(f)))
            }
            Value::Integer(n) => Ok(Expr::Literal(Literal::Integer(n))),
            Value::Float(f) => Ok(Expr::Literal(Literal::Float(f))),
            Value::String(s) => Ok(Expr::Literal(Literal::String(s))),
            Value::Boolean(b) => Ok(Expr::Literal(Literal::Boolean(b))),
            Value::Char(c) => Ok(Expr::Literal(Literal::Char(c))),
            Value::Unit => Ok(Expr::Literal(Literal::Unit)),
            _ => Err(VeldError::RuntimeError(
                "Cannot convert value to expression".into(),
            )),
        }
    }

    fn execute_statement(&mut self, statement: Statement) -> Result<Value> {
        println!("Executing statement: {:?}", statement);

        match statement {
            Statement::VariableDeclaration {
                name,
                var_kind,
                type_annotation,
                value,
                ..
            } => self.execute_variable_declaration(name, var_kind, type_annotation, value),
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                ..
            } => {
                let processed_body = if !body.is_empty() {
                    // If we don't have any explicit returns and the last statement is an expression,
                    // convert it to a return
                    if !body.iter().any(|s| matches!(s, Statement::Return(_))) {
                        let mut new_body = body.clone();
                        if let Some(Statement::ExprStatement(expr)) = new_body.clone().last() {
                            new_body.pop();
                            new_body.push(Statement::Return(Some(expr.clone())));
                        }
                        new_body
                    } else {
                        body.clone()
                    }
                } else {
                    body.clone()
                };

                let actual_return_type = match &return_type {
                    TypeAnnotation::Basic(name) if name == "infer" => {
                        // Try to infer from the body
                        if let Some(Statement::Return(Some(expr))) = processed_body.last() {
                            match expr {
                                Expr::Literal(Literal::String(_)) => {
                                    TypeAnnotation::Basic("str".to_string())
                                }
                                Expr::Literal(Literal::Integer(_)) => {
                                    TypeAnnotation::Basic("i32".to_string())
                                }
                                Expr::Literal(Literal::Float(_)) => {
                                    TypeAnnotation::Basic("f64".to_string())
                                }
                                Expr::Literal(Literal::Boolean(_)) => {
                                    TypeAnnotation::Basic("bool".to_string())
                                }
                                Expr::Literal(Literal::Char(_)) => {
                                    TypeAnnotation::Basic("char".to_string())
                                }
                                _ => return_type.clone(),
                            }
                        } else {
                            TypeAnnotation::Unit
                        }
                    }
                    _ => return_type.clone(),
                };

                let function = Value::Function {
                    params: params.clone(),
                    body: processed_body,
                    return_type: actual_return_type,
                    captured_vars: HashMap::new(), // No captured vars for top-level functions
                };

                // Register function in current scope
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
            Statement::BlockScope { body } => {
                println!("Executing block scope with {} statements", body.len());

                // Create new scope for the block
                self.push_scope();

                let mut last_value = Value::Unit;
                for stmt in body {
                    let result = self.execute_statement(stmt)?;

                    // Handle control flow - early returns should bubble up
                    match result {
                        Value::Return(_) => {
                            self.pop_scope();
                            return Ok(result);
                        }
                        Value::Break | Value::Continue => {
                            self.pop_scope();
                            return Ok(result);
                        }
                        _ => last_value = result,
                    }
                }

                self.pop_scope();
                Ok(last_value)
            }
            Statement::ProcDeclaration {
                name,
                params,
                body,
                is_public,
            } => {
                println!("Executing proc declaration: {}", name);

                // Convert to a function with Unit return type
                let function = Value::Function {
                    params: params
                        .into_iter()
                        .map(|(name, type_anno)| (name, type_anno))
                        .collect(),
                    body,
                    return_type: TypeAnnotation::Unit,
                    captured_vars: HashMap::new(),
                };

                self.current_scope_mut().set(name, function);
                Ok(Value::Unit)
            }

            Statement::StructDeclaration {
                name,
                fields,
                methods,
                generic_params,
                is_public: _,
            } => {
                // Register the struct type
                if generic_params.is_empty() {
                    self.structs.insert(name.clone(), fields);
                } else {
                    let struct_info = StructInfo {
                        fields: fields
                            .into_iter()
                            .map(|f| (f.name, f.type_annotation))
                            .collect(),
                        generic_params: generic_params.clone(),
                    };
                    self.generic_structs.insert(name.clone(), struct_info);
                    self.structs.insert(name.clone(), vec![]);
                };

                // Register methods if any
                if !methods.is_empty() {
                    let mut method_map = HashMap::new();

                    for method in methods {
                        let method_value = Value::Function {
                            params: method.params,
                            body: method.body,
                            return_type: method.return_type,
                            captured_vars: HashMap::new(), // No captured vars for struct methods
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
                        captured_vars: HashMap::new(),
                    };

                    method_map.insert(method.name, method_value);
                }

                Ok(Value::Unit)
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

                // Convert the result back to an expression for assignment
                let result_expr = self.value_to_expr(result)?;
                self.execute_assignment(name, Box::new(result_expr))
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
                    if let Some(bindings) = self.pattern_matches(&arm.pat, &match_value)? {
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

            Statement::Assignment { name, value } => self.execute_assignment(name, value),
            Statement::MacroInvocation { name, arguments } => {
                // Evaluate all arguments
                let mut evaluated_args = Vec::new();
                for arg in arguments {
                    let arg_value = self.evaluate_expression(arg)?;
                    evaluated_args.push(arg_value);
                }

                let current_mod = self.current_module.clone();
                let macro_def = self.find_macro(&current_mod, &name)?;

                match macro_def {
                    Statement::MacroDeclaration {
                        patterns,
                        body: None,
                        ..
                    } => self.expand_pattern_macro(name.as_str(), &patterns, &evaluated_args),
                    // Procedural macro
                    Statement::MacroDeclaration {
                        patterns: _,
                        body: Some(body),
                        ..
                    } => self.execute_procedural_macro(name.as_str(), &body, &evaluated_args),
                    _ => Err(VeldError::RuntimeError(format!(
                        "Invalid macro definition for '{}'",
                        name
                    ))),
                }
            }
            _ => Ok(Value::Unit),
        }
    }

    fn execute_assignment(&mut self, name: String, value: Box<Expr>) -> Result<Value> {
        let new_value = self.evaluate_expression(*value)?.unwrap_return();

        // Find the variable in scopes (starting from innermost) and check mutability
        for scope in self.scopes.iter_mut().rev() {
            if scope.values.contains_key(&name) {
                return match scope.assign(&name, new_value) {
                    Ok(_) => Ok(Value::Unit),
                    Err(e) => Err(e),
                };
            }
        }

        // If we get here, the variable wasn't found in any scope
        Err(VeldError::RuntimeError(format!(
            "Cannot assign to undefined variable '{}'",
            name
        )))
    }

    fn execute_variable_declaration(
        &mut self,
        name: String,
        var_kind: VarKind,
        type_annotation: Option<TypeAnnotation>,
        value: Box<Expr>,
    ) -> Result<Value> {
        let evaluated_value = self.evaluate_expression(*value)?.unwrap_return();

        // For const declarations, ensure the value is compile-time evaluable
        if matches!(var_kind, VarKind::Const) {
            if !self.is_compile_time_constant(&evaluated_value) {
                return Err(VeldError::RuntimeError(
                    "Const declarations must have compile-time constant values".to_string(),
                ));
            }

            // Constants can only be declared at module level
            if self.scopes.len() > 1 {
                return Err(VeldError::RuntimeError(
                    "Constants can only be declared at module level".to_string(),
                ));
            }
        }

        // Type checking with annotations
        if let Some(type_anno) = type_annotation {
            let expected_type = self.type_checker.env.from_annotation(&type_anno, None)?;
            self.validate_value_type(&evaluated_value, &expected_type)?;
        }

        // Store variable with kind information
        self.current_scope_mut()
            .declare(name.clone(), evaluated_value.clone(), var_kind)?;

        Ok(evaluated_value)
    }

    fn value_to_literal(&self, value: Value) -> Result<Literal> {
        match value {
            Value::Numeric(NumericValue::Integer(IntegerValue::I64(n))) => Ok(Literal::Integer(n)),
            Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => Ok(Literal::Float(f)),
            Value::Integer(n) => Ok(Literal::Integer(n)),
            Value::Float(f) => Ok(Literal::Float(f)),
            Value::String(s) => Ok(Literal::String(s)),
            Value::Boolean(b) => Ok(Literal::Boolean(b)),
            Value::Char(c) => Ok(Literal::Char(c)),
            Value::Unit => Ok(Literal::Unit),
            _ => Err(VeldError::RuntimeError(
                "Cannot convert value to literal".into(),
            )),
        }
    }

    fn is_compile_time_constant(&self, value: &Value) -> bool {
        match value {
            Value::Numeric(_)
            | Value::Integer(_)
            | Value::Float(_)
            | Value::String(_)
            | Value::Boolean(_)
            | Value::Char(_)
            | Value::Unit => true,
            Value::Array(elements) => elements.iter().all(|e| self.is_compile_time_constant(e)),
            Value::Tuple(elements) => elements.iter().all(|e| self.is_compile_time_constant(e)),
            _ => false,
        }
    }

    fn validate_value_type(&self, value: &Value, expected_type: &Type) -> Result<()> {
        let value_type = self.get_value_type(value);

        if !self.types_compatible(&value_type, expected_type) {
            return Err(VeldError::RuntimeError(format!(
                "Type mismatch: expected {}, got {}",
                expected_type, value_type
            )));
        }

        Ok(())
    }

    fn get_value_type(&self, value: &Value) -> Type {
        match value {
            Value::Numeric(nv) => nv.clone().type_of(),
            Value::Integer(_) => Type::I64,
            Value::Float(_) => Type::F64,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Bool,
            Value::Char(_) => Type::Char,
            Value::Unit => Type::Unit,
            Value::Array(elements) => {
                if elements.is_empty() {
                    Type::Array(Box::new(Type::Any))
                } else {
                    let elem_type = self.get_value_type(&elements[0]);
                    Type::Array(Box::new(elem_type))
                }
            }
            Value::Tuple(elements) => {
                let elem_types = elements.iter().map(|e| self.get_value_type(e)).collect();
                Type::Tuple(elem_types)
            }
            Value::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|(_, type_anno)| {
                        // Convert TypeAnnotation to Type - simplified for now
                        match type_anno {
                            TypeAnnotation::Basic(name) => match name.as_str() {
                                "i32" => Type::I32,
                                "f64" => Type::F64,
                                "str" => Type::String,
                                "bool" => Type::Bool,
                                _ => Type::Any,
                            },
                            TypeAnnotation::Unit => Type::Unit,
                            _ => Type::Any,
                        }
                    })
                    .collect();

                let ret_type = match return_type {
                    TypeAnnotation::Basic(name) => match name.as_str() {
                        "i32" => Type::I32,
                        "f64" => Type::F64,
                        "str" => Type::String,
                        "bool" => Type::Bool,
                        _ => Type::Any,
                    },
                    TypeAnnotation::Unit => Type::Unit,
                    _ => Type::Any,
                };

                Type::Function {
                    params: param_types,
                    return_type: Box::new(ret_type),
                }
            }
            _ => Type::Any,
        }
    }

    fn types_compatible(&self, actual: &Type, expected: &Type) -> bool {
        // Implement your type compatibility rules
        actual == expected
            || matches!(expected, Type::Any)
            || (self.is_numeric_type_compat(actual) && self.is_numeric_type_compat(expected))
    }

    fn is_numeric_type_compat(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::F32
                | Type::F64
        )
    }

    fn pattern_matches(
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
                                    self.pattern_matches(&**pattern, field_value)?
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

    fn cast_val_to_num(&self, value: Value, target_type: &TypeAnnotation) -> Result<Value> {
        let numeric_value = match value {
            Value::Numeric(nv) => nv,
            Value::Integer(i) => NumericValue::Integer(IntegerValue::I64(i)),
            Value::Float(f) => NumericValue::Float(FloatValue::F64(f)),
            _ => {
                return Err(VeldError::RuntimeError(format!(
                    "Cannot cast value {:?} to numeric type {:?}",
                    value, target_type
                )));
            }
        };

        let result = match target_type {
            TypeAnnotation::Basic(type_name) => match type_name.as_str() {
                "i8" => numeric_value.to_i8()?,
                "i16" => numeric_value.to_i16()?,
                "i32" => numeric_value.to_i32()?,
                "i64" => numeric_value.to_i64()?,
                "u8" => numeric_value.to_u8()?,
                "u16" => numeric_value.to_u16()?,
                "u32" => numeric_value.to_u32()?,
                "u64" => numeric_value.to_u64()?,
                "f32" => numeric_value.to_f32()?,
                "f64" => numeric_value.to_f64()?,
                _ => {
                    return Err(VeldError::RuntimeError(format!(
                        "Invalid cast to target type '{}'",
                        type_name
                    )));
                }
            },
            _ => {
                return Err(VeldError::RuntimeError(format!(
                    "Cannot cast to target type {:?}",
                    target_type
                )));
            }
        };
        Ok(Value::Numeric(result))
    }

    fn evaluate_expression(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_value = self.evaluate_expression(*condition)?.unwrap_return();

                if self.is_truthy(cond_value) {
                    self.evaluate_expression(*then_expr)
                } else if let Some(else_expr) = else_expr {
                    self.evaluate_expression(*else_expr)
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::BlockLambda {
                params,
                body,
                return_type,
            } => Ok(self.create_block_lambda(params, body, return_type)),
            Expr::Literal(lit) => Ok(match lit {
                Literal::Integer(n) => {
                    // Default to i32, but this could be context-sensitive in the future
                    let int_val = if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                        IntegerValue::I32(n as i32)
                    } else {
                        IntegerValue::I64(n)
                    };
                    Value::Numeric(NumericValue::Integer(int_val))
                }
                Literal::Float(n) => {
                    // Default to f64, but could be context-sensitive in the future
                    Value::Float(n)
                }
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
            } => Ok(self.create_lambda(params, body, return_type)),

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
            Expr::BlockExpression {
                statements,
                final_expr,
            } => {
                println!(
                    "Evaluating block expression with {} statements",
                    statements.len()
                );

                // Create new scope for the block
                self.push_scope();

                // Execute all statements
                for stmt in statements {
                    let result = self.execute_statement(stmt)?;

                    // Handle early returns
                    match result {
                        Value::Return(_) | Value::Break | Value::Continue => {
                            self.pop_scope();
                            return Ok(result);
                        }
                        _ => {} // Continue with next statement
                    }
                }

                // Evaluate final expression or return unit
                let result = if let Some(expr) = final_expr {
                    self.evaluate_expression(*expr)?
                } else {
                    Value::Unit
                };

                self.pop_scope();
                Ok(result)
            }
            Expr::MacroExpr { name, arguments } => {
                // Evaluate all arguments
                let mut evaluated_args = Vec::new();
                for arg in arguments {
                    let arg_value = self.evaluate_expression(arg)?;
                    evaluated_args.push(arg_value);
                }

                // Find macro definition
                let current_mod = self.current_module.clone();
                let macro_def = self.find_macro(&current_mod, &name)?;

                match macro_def {
                    // Pattern-based macro
                    Statement::MacroDeclaration {
                        patterns,
                        body: None,
                        ..
                    } => self.expand_pattern_macro(&name, &patterns, &evaluated_args),
                    // Procedure macro
                    Statement::MacroDeclaration {
                        patterns: _,
                        body: Some(body),
                        ..
                    } => self.execute_procedural_macro(&name, &body, &evaluated_args),
                    _ => Err(VeldError::RuntimeError(format!(
                        "Invalid macro definition for '{name}'"
                    ))),
                }
            }
        }
    }

    fn find_macro(&self, module_name: &str, macro_name: &str) -> Result<Statement> {
        // Check current module first
        if let Some(module) = self.module_manager.get_module(module_name) {
            for stmt in &module.statements {
                if let Statement::MacroDeclaration { name, .. } = stmt {
                    if name == macro_name {
                        return Ok(stmt.clone());
                    }
                }
            }
        }

        // Check imported modules
        for (alias, imported_module_name) in &self.imported_modules {
            if let Some(module) = self.module_manager.get_module(imported_module_name) {
                for stmt in &module.statements {
                    if let Statement::MacroDeclaration { name, .. } = stmt {
                        if name == macro_name {
                            return Ok(stmt.clone());
                        }
                    }
                }
            }
        }

        Err(VeldError::RuntimeError(format!(
            "Macro '{macro_name}' not found'"
        )))
    }

    fn expand_pattern_macro(
        &mut self,
        macro_name: &str,
        patterns: &[(MacroPattern, MacroExpansion)],
        args: &[Value],
    ) -> Result<Value> {
        let args_str = args
            .iter()
            .map(|v| self.value_to_string(v))
            .collect::<Result<Vec<_>>>()?
            .join(" ");

        for (pattern, expansion) in patterns {
            // Try to match the pattern and extract bindings
            if let Some(bindings) = self.extract_pattern_bindings(&pattern.0, &args_str, args) {
                // Execute expansion with the extracted bindings
                return self.execute_macro_expansion_with_bindings(expansion, &bindings);
            }
        }

        Err(VeldError::RuntimeError(format!(
            "No matching pattern found for macro '{}' with arguments: {}",
            macro_name, args_str
        )))
    }

    fn extract_pattern_bindings(
        &self,
        pattern: &str,
        arg_str: &str,
        original_args: &[Value],
    ) -> Option<HashMap<String, Value>> {
        let pattern_tokens = self.tokenize_pattern(pattern);
        let args_tokens = self.tokenize_args(arg_str);

        let mut bindings = HashMap::new();
        let mut p_idx = 0;
        let mut a_idx = 0;

        while p_idx < pattern_tokens.len() && a_idx < args_tokens.len() {
            match &pattern_tokens[p_idx] {
                PatternToken::Literal(lit) => {
                    if &args_tokens[a_idx] != lit {
                        return None; // Pattern does not match
                    }
                    a_idx += 1;
                }
                PatternToken::Variable(var_name) => {
                    if a_idx < original_args.len() {
                        bindings.insert(var_name.clone(), original_args[a_idx].clone());
                    }
                    a_idx += 1;
                }
                PatternToken::Repetition {
                    variable,
                    separator,
                    ..
                } => {
                    // Handle repetition and collect all matches
                    let mut collected_values = Vec::new();
                    let mut matched = 0;

                    while a_idx < args_tokens.len() {
                        // Check for separator if not first match
                        if matched > 0 && separator.is_some() {
                            if a_idx >= args_tokens.len()
                                || &args_tokens[a_idx] != separator.as_ref().unwrap()
                            {
                                break;
                            }
                            a_idx += 1; // Skip separator
                        }

                        // Check if we hit the next pattern token
                        if p_idx + 1 < pattern_tokens.len() {
                            if let PatternToken::Literal(next_lit) = &pattern_tokens[p_idx + 1] {
                                if &args_tokens[a_idx] == next_lit {
                                    break;
                                }
                            }
                        }

                        if a_idx < original_args.len() {
                            collected_values.push(original_args[a_idx].clone());
                        }

                        matched += 1;
                        a_idx += 1;
                    }

                    // Store the collected values as an array
                    bindings.insert(variable.clone(), Value::Array(collected_values));
                }
            }
            p_idx += 1;
        }
        // Only return bindings if the entire pattern was matched
        if p_idx >= pattern_tokens.len() && a_idx >= args_tokens.len() {
            Some(bindings)
        } else {
            None
        }
    }

    fn execute_macro_expansion_with_bindings(
        &mut self,
        expansion: &MacroExpansion,
        bindings: &HashMap<String, Value>,
    ) -> Result<Value> {
        self.push_scope();

        // Bind extracted variables to the scope
        for (var_name, value) in bindings {
            self.current_scope_mut()
                .declare(var_name.clone(), value.clone(), VarKind::Let)?;
        }

        // Execute expansion statements
        let mut result = Value::Unit;
        for stmt in &expansion.0 {
            // Replace variables in statement before execution
            let expanded_stmt = self.substitute_variables_in_statement(stmt, bindings)?;
            match self.execute_statement(expanded_stmt) {
                Ok(value) => {
                    result = value;
                }
                Err(e) => {
                    self.pop_scope();
                    return Err(e);
                }
            }
        }
        self.pop_scope();
        Ok(result)
    }

    fn substitute_variables_in_statement(
        &self,
        stmt: &Statement,
        bindings: &HashMap<String, Value>,
    ) -> Result<Statement> {
        // Handle only the most common cases for now
        match stmt {
            Statement::ExprStatement(expr) => {
                let new_expr = self.substitute_variables_in_expression(expr, bindings)?;
                Ok(Statement::ExprStatement(new_expr))
            }
            Statement::Return(Some(expr)) => {
                let new_expr = self.substitute_variables_in_expression(expr, bindings)?;
                Ok(Statement::Return(Some(new_expr)))
            }
            // For other statements, just return the original statements for now
            _ => Ok(stmt.clone()),
        }
    }

    fn substitute_variables_in_expression(
        &self,
        expr: &Expr,
        bindings: &HashMap<String, Value>,
    ) -> Result<Expr> {
        match expr {
            Expr::Identifier(name) => {
                if let Some(value) = bindings.get(name) {
                    // Convert the value back to an expression
                    Ok(self.value_to_expr(value.clone())?)
                } else {
                    // If bindings not found, keep the original identifier
                    Ok(expr.clone())
                }
            }
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                let new_left = self.substitute_variables_in_expression(left, bindings)?;
                let new_right = self.substitute_variables_in_expression(expr, bindings)?;
                Ok(Expr::BinaryOp {
                    left: Box::new(new_left),
                    operator: operator.clone(),
                    right: Box::new(new_right),
                })
            }
            // For other expressions, just return the original for now
            // TODO: Handle other expression types
            _ => Ok(expr.clone()),
        }
    }

    fn tokenize_pattern(&self, pattern: &str) -> Vec<PatternToken> {
        let mut tokens = Vec::new();
        let mut chars = pattern.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '$' => {
                    // Check for repetition pattern: $($name)+, $($name)*, $($name),+, $($name),*
                    if chars.peek() == Some(&'(') {
                        chars.next(); // Consume '('
                        if chars.peek() == Some(&'$') {
                            chars.next(); // Consume '$'

                            // Read the variable name
                            let mut var_name = String::new();
                            while let Some(&next_c) = chars.peek() {
                                if next_c.is_alphanumeric() || next_c == '_' || next_c == ':' {
                                    var_name.push(chars.next().unwrap());
                                } else {
                                    break;
                                }
                            }

                            // Consume closing ')'
                            if chars.peek() == Some(&')') {
                                chars.peek();
                            } else {
                                // Syntax error, handle as literal
                                tokens.push(PatternToken::Literal(format!("$({})", var_name)));
                                continue;
                            }

                            // Check for repetition symbols
                            let mut seperator = None;
                            let min = match chars.peek() {
                                Some(&'+') => {
                                    chars.next();
                                    1 // One or more
                                }
                                Some(&'*') => {
                                    chars.next();
                                    0 // Zero or more
                                }
                                Some(&',') => {
                                    chars.next();
                                    seperator = Some(','.to_string());

                                    match chars.peek() {
                                        Some(&'+') => {
                                            chars.next();
                                            1 // One or more with separator
                                        }
                                        Some(&'*') => {
                                            chars.next();
                                            0 // Zero or more with separator
                                        }
                                        _ => {
                                            // Just a comma, no repetition
                                            tokens.push(PatternToken::Variable(var_name));
                                            tokens.push(PatternToken::Literal(",".to_string()));
                                            continue;
                                        }
                                    }
                                }
                                _ => {
                                    // Not a repetition, just a grouped variable
                                    tokens.push(PatternToken::Variable(var_name));
                                    continue;
                                }
                            };
                            tokens.push(PatternToken::Repetition {
                                variable: var_name,
                                separator: seperator,
                                min,
                            });
                        } else {
                            // Just $(something) without repetition
                            // Handle as literal for now
                            tokens.push(PatternToken::Literal("$(".to_string()));
                            // TODO: Handle the rest of the pattern branch
                        }
                    } else {
                        // Simple variable $name
                        let mut var_name = String::new();
                        while let Some(&next_c) = chars.peek() {
                            if next_c.is_alphanumeric() || next_c == '_' || next_c == ':' {
                                var_name.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        if !var_name.is_empty() {
                            tokens.push(PatternToken::Variable(var_name));
                        } else {
                            tokens.push(PatternToken::Literal("$".to_string()));
                        }
                    }
                }
                // TODO: Handle other special characters like `*`, `+`, etc.
                _ => {
                    let mut literal = String::new();
                    literal.push(c);

                    while let Some(&next_c) = chars.peek() {
                        if next_c != '$' && !next_c.is_whitespace() {
                            literal.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(PatternToken::Literal(literal));
                }
            }
        }
        tokens
    }

    fn tokenize_args(&self, args: &str) -> Vec<String> {
        // Simple tokenization by whitespace
        args.split_whitespace().map(|s| s.to_string()).collect()
    }

    fn match_pattern_tokens(&self, pattern: &[PatternToken], args: &[String]) -> bool {
        let mut p_idx = 0;
        let mut a_idx = 0;

        while p_idx < pattern.len() {
            if a_idx >= args.len() && p_idx < pattern.len() {
                // Check if remaining patterns are optional
                return self.remaining_patterns_optional(&pattern[p_idx..]);
            }

            match &pattern[p_idx] {
                PatternToken::Literal(lit) => {
                    if a_idx >= args.len() || &args[a_idx] != lit {
                        return false; // Literal mismatch
                    }
                    a_idx += 1; // Move to next argument
                }
                PatternToken::Variable(_) => {
                    // Variables match anything
                    if a_idx >= args.len() {
                        return false;
                    }
                    a_idx += 1; // Move to next argument
                }
                PatternToken::Repetition { min, separator, .. } => {
                    // Handle repetition
                    let mut matched = 0;

                    while a_idx < args.len() {
                        // Check for seperator if not first match
                        if matched > 0 && separator.is_some() {
                            if a_idx >= args.len() || &args[a_idx] != separator.as_ref().unwrap() {
                                break;
                            }
                            a_idx += 1; // Skip separator
                        }

                        // Check if next pattern is hit
                        if p_idx + 1 < pattern.len() {
                            if let PatternToken::Literal(next_lit) = &pattern[p_idx + 1] {
                                if &args[a_idx] == next_lit {
                                    break;
                                }
                            }
                        }

                        matched += 1;
                        a_idx += 1;
                    }

                    // Check if minimum matches
                    if matched < *min {
                        return false;
                    }
                }
            }
            p_idx += 1;
        }
        // Check if we consumed all arguments
        a_idx >= args.len()
    }

    fn remaining_patterns_optional(&self, patterns: &[PatternToken]) -> bool {
        patterns.iter().all(|token| match token {
            PatternToken::Repetition { min, .. } => *min == 0,
            _ => false,
        })
    }

    fn pattern_matches_args(&self, pattern: &str, args_str: &str) -> bool {
        // Parse the pattern into tokens
        let pattern_tokens = self.tokenize_pattern(pattern);
        let arg_tokens = self.tokenize_args(args_str);

        // Match with more advanced features
        self.match_pattern_tokens(&pattern_tokens, &arg_tokens)
    }

    fn execute_macro_expansion(
        &mut self,
        expansion: &MacroExpansion,
        args: &[Value],
    ) -> Result<Value> {
        self.push_scope();

        let mut result = Value::Unit;
        for stmt in expansion.0.clone() {
            match self.execute_statement(stmt) {
                Ok(value) => {
                    result = value;
                }

                Err(e) => {
                    self.pop_scope();
                    return Err(e);
                }
            }
        }

        self.pop_scope();
        Ok(result)
    }

    fn execute_procedural_macro(
        &mut self,
        macro_name: &str,
        body: &[Statement],
        args: &[Value],
    ) -> Result<Value> {
        self.push_scope();

        // TODO: Change to properly bind named parameters
        for (i, arg) in args.iter().enumerate() {
            let param_name = format!("${}", i + 1);
            self.current_scope_mut()
                .declare(param_name.clone(), arg.clone(), VarKind::Let)?;
        }

        // Execute the macro body
        let mut result = Value::Unit;
        for stmt in body {
            match self.execute_statement(stmt.clone()) {
                Ok(value) => {
                    result = value;
                }

                Err(e) => {
                    self.pop_scope();
                    return Err(e);
                }
            }
        }

        self.pop_scope();
        Ok(result)
    }

    fn value_to_string(&self, value: &Value) -> Result<String> {
        match value {
            Value::Integer(i) => Ok(i.to_string()),
            Value::Float(f) => Ok(f.to_string()),
            Value::String(s) => Ok(s.clone()),
            Value::Boolean(b) => Ok(b.to_string()),
            Value::Char(c) => Ok(c.to_string()),
            Value::Struct { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.value_to_string(v).unwrap_or_default()))
                    .collect::<Vec<_>>()
                    .join(", ");
                Ok(format!("{}({})", name, fields_str))
            }
            _ => Ok(format!("{:?}", value)),
        }
    }

    fn cast_value(&self, value: Value, target_type: &Type) -> Result<Value> {
        match (value.clone(), target_type) {
            // Integer to integer casts
            (Value::Integer(i), Type::I8) => {
                if i >= i8::MIN as i64 && i <= i8::MAX as i64 {
                    Ok(Value::Integer(i as i8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i8: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::I16) => {
                if i >= i16::MIN as i64 && i <= i16::MAX as i64 {
                    Ok(Value::Integer(i as i16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i16: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::I32) => {
                if i >= i32::MIN as i64 && i <= i32::MAX as i64 {
                    Ok(Value::Integer(i as i32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i32: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::I64) => Ok(Value::Integer(i)),

            // Integer to unsigned casts
            (Value::Integer(i), Type::U8) => {
                if i >= 0 && i <= u8::MAX as i64 {
                    Ok(Value::Integer(i as u8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u8: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::U16) => {
                if i >= 0 && i <= u16::MAX as i64 {
                    Ok(Value::Integer(i as u16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u16: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::U32) => {
                if i >= 0 && i <= u32::MAX as i64 {
                    Ok(Value::Integer(i as u32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u32: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::U64) => {
                if i >= 0 {
                    Ok(Value::Integer(i as u64 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u64: negative value",
                        i
                    )))
                }
            }

            // Integer to float casts
            (Value::Integer(i), Type::F32) => Ok(Value::Float(i as f32 as f64)),
            (Value::Integer(i), Type::F64) => Ok(Value::Float(i as f64)),

            // Float to integer casts (with truncation)
            (Value::Float(f), Type::I8) => {
                if f >= i8::MIN as f64 && f <= i8::MAX as f64 {
                    Ok(Value::Integer(f as i8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i8: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::I16) => {
                if f >= i16::MIN as f64 && f <= i16::MAX as f64 {
                    Ok(Value::Integer(f as i16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i16: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::I32) => {
                if f >= i32::MIN as f64 && f <= i32::MAX as f64 {
                    Ok(Value::Integer(f as i32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i32: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::I64) => {
                if f >= i64::MIN as f64 && f <= i64::MAX as f64 {
                    Ok(Value::Integer(f as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i64: out of range",
                        f
                    )))
                }
            }

            // Float to unsigned casts
            (Value::Float(f), Type::U8) => {
                if f >= 0.0 && f <= u8::MAX as f64 {
                    Ok(Value::Integer(f as u8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u8: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::U16) => {
                if f >= 0.0 && f <= u16::MAX as f64 {
                    Ok(Value::Integer(f as u16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u16: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::U32) => {
                if f >= 0.0 && f <= u32::MAX as f64 {
                    Ok(Value::Integer(f as u32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u32: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::U64) => {
                if f >= 0.0 && f <= u64::MAX as f64 {
                    Ok(Value::Integer(f as u64 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u64: out of range",
                        f
                    )))
                }
            }

            // Float to float casts
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
            (Value::String(s), Type::F64) => match s.parse::<f64>() {
                Ok(f) => Ok(Value::Float(f)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as f64",
                    s
                ))),
            },

            // To string conversions
            (Value::Integer(i), Type::String) => Ok(Value::String(i.to_string())),
            (Value::Float(f), Type::String) => Ok(Value::String(f.to_string())),

            // Same type conversion (no-op)
            (v, t) if v.type_of() == *t => Ok(v),

            // Invalid casts
            _ => Err(VeldError::RuntimeError(format!(
                "Invalid cast from {:?} to {:?}",
                value.type_of(),
                target_type
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
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
                captured_vars: HashMap::new(),
            },
        );

        // Store methods in struct_methods HashMap with a special key
        self.struct_methods
            .insert("Array".to_string(), array_methods);
    }

    // Helper method to call functions with pre-evaluated arguments
    fn call_function_with_values(&mut self, name: String, arg_values: Vec<Value>) -> Result<Value> {
        // Check if this might be a method call on a struct
        if name.contains('.') {
            let parts: Vec<&str> = name.split('.').collect();
            if parts.len() == 2 {
                let object = self.get_variable(parts[0]).ok_or_else(|| {
                    VeldError::RuntimeError(format!("Undefined variable '{}'", parts[0]))
                })?;
                return self.call_method_value(object, parts[1].to_string(), arg_values);
            }
        }

        let function = self
            .get_variable(&name)
            .ok_or_else(|| VeldError::RuntimeError(format!("Undefined function '{}'", name)))?;

        match function {
            Value::Function {
                params,
                body,
                captured_vars,
                ..
            } => {
                // Create new scope for function
                self.push_scope();

                // First, set up captured variables in the new scope
                for (var_name, var_value) in captured_vars {
                    self.current_scope_mut().set(var_name, var_value);
                }

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

                // For functions without explicit returns, if the last statement produced a value,
                // consider it the return value
                if !matches!(result, Value::Return(_)) && !matches!(result, Value::Unit) {
                    result = Value::Return(Box::new(result));
                }

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
            Value::Struct { name, fields } => {
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
                            captured_vars: HashMap::new(),
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

    fn to_numeric_value(&self, value: Value) -> Result<Option<NumericValue>> {
        match value {
            Value::Numeric(nv) => Ok(Some(nv)),
            Value::Integer(i) => Ok(Some(NumericValue::Integer(IntegerValue::I64(i)))),
            Value::Float(f) => Ok(Some(NumericValue::Float(FloatValue::F64(f)))),
            _ => Ok(None),
        }
    }

    fn from_numeric_option(&self, numeric_opt: Option<NumericValue>) -> Value {
        match numeric_opt {
            Some(nv) => Value::Numeric(nv),
            None => Value::Unit, // This should not happen in normal execution
        }
    }

    fn compare_numeric_values(
        &self,
        a: &NumericValue,
        b: &NumericValue,
        op: &BinaryOperator,
    ) -> Result<bool> {
        let a_f64 = a.clone().as_f64();
        let b_f64 = b.clone().as_f64();

        Ok(match op {
            BinaryOperator::Less => a_f64 < b_f64,
            BinaryOperator::Greater => a_f64 > b_f64,
            BinaryOperator::LessEq => a_f64 <= b_f64,
            BinaryOperator::GreaterEq => a_f64 >= b_f64,
            BinaryOperator::EqualEqual => a_f64 == b_f64,
            BinaryOperator::NotEqual => a_f64 != b_f64,
            _ => {
                return Err(VeldError::RuntimeError(
                    "Invalid comparison operator".into(),
                ));
            }
        })
    }

    fn numeric_values_equal(&self, a: &NumericValue, b: &NumericValue) -> bool {
        a.clone().as_f64() == b.clone().as_f64()
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            _ => false,
        }
    }

    fn evaluate_non_numeric_binary_op(
        &self,
        left: Option<NumericValue>,
        right: Option<NumericValue>,
        operator: BinaryOperator,
    ) -> Result<Value> {
        // Handle string concatenation and other non-numeric operations
        let left_val = self.from_numeric_option(left);
        let right_val = self.from_numeric_option(right);

        match (left_val, right_val) {
            (Value::String(a), Value::String(b)) => {
                let result = match operator {
                    BinaryOperator::Less => a < b,
                    BinaryOperator::LessEq => a <= b,
                    BinaryOperator::Greater => a > b,
                    BinaryOperator::GreaterEq => a >= b,
                    BinaryOperator::EqualEqual => a == b,
                    BinaryOperator::NotEqual => a != b,
                    _ => return Err(VeldError::RuntimeError("Invalid string comparison".into())),
                };
                Ok(Value::Boolean(result))
            }
            _ => Err(VeldError::RuntimeError(
                "Invalid non-numeric binary operation".into(),
            )),
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

        let left_numeric = self.to_numeric_value(left)?;
        let right_numeric = self.to_numeric_value(right)?;

        match operator {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulo
            | BinaryOperator::Exponent => match (&left_numeric, &right_numeric) {
                (Some(a), Some(b)) => {
                    let result = a.perform_operation(&operator, b)?;
                    Ok(Value::Numeric(result))
                }
                _ => self.evaluate_non_numeric_binary_op(left_numeric, right_numeric, operator),
            },

            // Comparison operators
            BinaryOperator::LessEq
            | BinaryOperator::GreaterEq
            | BinaryOperator::Less
            | BinaryOperator::Greater => match (&left_numeric, &right_numeric) {
                (Some(a), Some(b)) => {
                    let result = self.compare_numeric_values(a, b, &operator)?;
                    Ok(Value::Boolean(result))
                }
                _ => self.evaluate_non_numeric_binary_op(left_numeric, right_numeric, operator),
            },

            // Equality operators
            BinaryOperator::EqualEqual | BinaryOperator::NotEqual => {
                let is_equal = match (&left_numeric, &right_numeric) {
                    (Some(a), Some(b)) => self.numeric_values_equal(a, b),
                    _ => self.values_equal(
                        &self.from_numeric_option(left_numeric),
                        &self.from_numeric_option(right_numeric),
                    ),
                };

                let result = match operator {
                    BinaryOperator::EqualEqual => is_equal,
                    BinaryOperator::NotEqual => !is_equal,
                    _ => unreachable!(),
                };
                Ok(Value::Boolean(result))
            }

            // Logical operators
            BinaryOperator::And | BinaryOperator::Or => {
                let left_val = self.from_numeric_option(left_numeric);
                let right_val = self.from_numeric_option(right_numeric);

                match operator {
                    BinaryOperator::And => Ok(Value::Boolean(
                        self.is_truthy(left_val) && self.is_truthy(right_val),
                    )),
                    BinaryOperator::Or => Ok(Value::Boolean(
                        self.is_truthy(left_val) || self.is_truthy(right_val),
                    )),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn call_function(&mut self, name: String, arguments: Vec<Argument>) -> Result<Value> {
        // Evaluate all arguments first
        let mut arg_values = Vec::new();
        for arg in arguments {
            let expr = match arg {
                Argument::Positional(expr) => expr,
                Argument::Named { name: _, value } => value,
            };
            let value = self.evaluate_expression(expr)?;
            arg_values.push(value.unwrap_return());
        }

        // Call the function with evaluated arguments
        self.call_function_with_values(name, arg_values)
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
    fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[0]
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        if self.scopes.is_empty() {
            let new_level = self.scopes.len();
            self.scopes.push(Scope::new(new_level));
        }
        self.scopes.last_mut().unwrap()
    }

    fn push_scope(&mut self) {
        let new_level = self.scopes.len();
        self.scopes.push(Scope::new(new_level));
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
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
            self.imported_modules
                .insert(alias_name, module_path_str.clone());
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
                                captured_vars: HashMap::new(), // No captured vars for imports
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

        if is_public {
            let current_module = self.get_current_module().to_string();
            let exports = self.module_manager.get_exports(&module_path_str, &items)?;

            let module = self
                .module_manager
                .get_module_mut(&current_module)
                .ok_or_else(|| VeldError::RuntimeError("Current module not found".to_string()))?;

            for (name, export_item) in exports {
                module.exports.insert(name, export_item);
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
