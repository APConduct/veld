use tracing::Level;

use super::super::ast::{
    Argument, BinaryOperator, Expr, GenericArgument, Literal, MethodImpl, Statement, StructMethod,
    TypeAnnotation, UnaryOperator, VarKind,
};
use super::super::types::VarInfo;
use super::super::types::{EnumVariant, ImplementationInfo, Type, Type::TypeVar, TypeEnvironment};
use super::Value;
use std::collections::HashMap;
use veld_error::{Result, VeldError};

pub struct TypeChecker {
    env: TypeEnvironment,
    current_function_return_type: Option<Type>,
    var_info: HashMap<String, VarInfo>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let _span = tracing::span!(tracing::Level::INFO, "Initializing TypeChecker");
        let _guard = _span.enter();

        let mut env = TypeEnvironment::new();

        // Add std as a module identifier so property access like std.option.some works
        env.define("std", crate::types::Type::Module("std".to_string()));

        Self {
            env,
            current_function_return_type: None,
            var_info: HashMap::new(),
        }
    }

    pub fn env(&mut self) -> &mut TypeEnvironment {
        &mut self.env
    }

    pub fn with_env<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut TypeEnvironment) -> R,
    {
        f(&mut self.env)
    }

    pub fn find_operator_implementation(
        &mut self,
        op: &BinaryOperator,
        left: &Type,
        right: &Type,
    ) -> Result<Option<Type>> {
        let _span = tracing::span!(tracing::Level::INFO, "Finding operator implementation");
        let _guard = _span.enter();

        // Check if the operator is defined for the given types
        if let Some(impls) = self.env.get_implementations_for_type(&left.to_string()) {
            for impl_info in impls {
                if let Some(method_type) = impl_info.get_method(&op.to_string()) {
                    if self.env.types_match(method_type, right) {
                        return Ok(Some(method_type.clone()));
                    }
                }
            }
        }
        Ok(None)
    }

    // In TypeChecker implementation
    fn type_satisfies_constraint(&mut self, type_: &Type, constraint: &Type) -> bool {
        let _span = tracing::span!(tracing::Level::INFO, "Checking type satisfaction");
        let _guard = _span.enter();

        match constraint {
            Type::Generic { base, type_args } => {
                // For constraints like Neg<Output = T>
                let kind_name = base;

                // Check if type_ structurally implements the kind
                let implements_kind = self.env.type_structurally_implements_kind(type_, kind_name);
                if !implements_kind {
                    return false;
                }

                // If there are type arguments, verify them
                if !type_args.is_empty() {
                    // Find the implementation for the type
                    let type_name = match type_ {
                        Type::Struct { name, .. } => Some(name),
                        Type::Enum { name, .. } => Some(name),
                        // Add other cases that have names
                        _ => None,
                    };

                    if let Some(type_name) = type_name {
                        if let Some(impl_info) =
                            self.env.clone().find_implementation(type_name, kind_name)
                        {
                            // Check each type argument against the implementation
                            // This is a simplified version - you may need more complex logic
                            for (i, type_arg) in type_args.iter().enumerate() {
                                if let Some(impl_arg) = impl_info.generic_args().get(i) {
                                    // Convert the implementation's type annotation to a Type
                                    let impl_type = match self
                                        .env
                                        .from_annotation(&impl_arg.type_annotation, None)
                                    {
                                        Ok(t) => t,
                                        Err(_) => return false,
                                    };

                                    // Check if the types are compatible
                                    if !self.types_compatible(type_arg, &impl_type) {
                                        return false;
                                    }
                                } else {
                                    return false; // Not enough generic arguments
                                }
                            }
                        }
                    }
                }

                true
            }
            Type::TypeParam(param_name) => {
                // For simple type parameter constraints like T: SomeTrait
                match type_ {
                    Type::TypeParam(type_param) => type_param == param_name,
                    _ => self
                        .env
                        .type_structurally_implements_kind(type_, param_name),
                }
            }
            _ => false, // Other constraint types not supported
        }
    }

    pub fn check_generic_constraints(
        &mut self,
        type_params: &[GenericArgument],
        type_args: &[Type],
    ) -> bool {
        let _span = tracing::span!(tracing::Level::INFO, "Checking generic constraints");
        let _guard = _span.enter();

        if type_params.len() != type_args.len() {
            return false;
        }

        for (i, param) in type_params.iter().enumerate() {
            if !param.constraints.is_empty() {
                let arg_type = &type_args[i];

                for constraint_annotation in &param.constraints {
                    match self.env.from_annotation(constraint_annotation, None) {
                        Ok(constraint_type) => {
                            if !self.type_satisfies_constraint(arg_type, &constraint_type) {
                                return false;
                            }
                        }
                        Err(_) => return false, // Invalid constraint
                    }
                }
            }
        }

        true
    }

    pub fn check_numeric_operation(
        &mut self,
        op: &BinaryOperator,
        left: &Type,
        right: &Type,
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "Checking numeric operation");
        let _guard = _span.enter();

        // First check for kind implementations
        if let Some(impl_type) = self.find_operator_implementation(op, left, right)? {
            return Ok(impl_type);
        }

        // Default numeric operation rules
        match op {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide => {
                match (left, right) {
                    // Same type operations
                    (t1, t2) if t1 == t2 && t1.is_numeric() => Ok(t1.clone()),

                    // Mixed integer operations promote to larger type
                    (Type::I32, Type::I64) | (Type::I64, Type::I32) => Ok(Type::I64),

                    // Integer and float operations promote to float
                    (Type::I32, Type::F64) | (Type::F64, Type::I32) => Ok(Type::F64),

                    // Literal type inference
                    (Type::IntegerLiteral(_), t) | (t, Type::IntegerLiteral(_))
                        if t.is_numeric() =>
                    {
                        Ok(t.clone())
                    }

                    _ => Err(VeldError::TypeError(format!(
                        "Invalid numeric operation between {} and {}",
                        left, right
                    ))),
                }
            }
            _ => Err(VeldError::TypeError("Unsupported operator".into())),
        }
    }

    fn method_to_type(&mut self, method: &MethodImpl) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "Checking method type");
        let _guard = _span.enter();

        let mut param_types = Vec::new();

        for (_, type_annotation) in &method.params {
            let param_type = self.env.from_annotation(type_annotation, None)?;
            param_types.push(param_type);
        }

        let return_type = self.env.from_annotation(&method.return_type, None)?;

        Ok(Type::Function {
            params: param_types,
            return_type: Box::new(return_type),
        })
    }

    pub fn struct_method_to_type(&mut self, method: &StructMethod) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "Checking struct method type");
        let _guard = _span.enter();

        let mut param_types = Vec::new();

        for (_, type_annotation) in &method.params {
            let param_type = self.env.from_annotation(type_annotation, None)?;
            param_types.push(param_type);
        }
        let return_type = self.env.from_annotation(&method.return_type, None)?;
        Ok(Type::Function {
            params: param_types,
            return_type: Box::new(return_type),
        })
    }

    pub fn process_implementation(
        &mut self,
        type_name: &str,
        kind_name: Option<&str>,
        methods: &[MethodImpl],
        generic_args: &[GenericArgument],
    ) -> Result<()> {
        let _span = tracing::span!(tracing::Level::INFO, "Processing implementation");
        let _guard = _span.enter();

        if kind_name.is_none() {
            return Ok(());
        }
        let kind = kind_name.unwrap();

        let mut method_types = HashMap::new();
        for method in methods {
            let method_type = self.method_to_type(method)?;
            method_types.insert(method.name.clone(), method_type);
        }
        self.env
            .add_implementation(type_name, kind, generic_args.to_vec(), method_types);
        Ok(())
    }

    pub fn check_program(&mut self, statements: &[Statement]) -> Result<()> {
        let _span = tracing::span!(tracing::Level::INFO, "Checking program");
        let _guard = _span.enter();

        // First pass: Register struct declarations and function signatures
        for stmt in statements {
            match stmt {
                Statement::StructDeclaration {
                    name,
                    fields,
                    generic_params,
                    ..
                } => {
                    if !generic_params.is_empty() {
                        self.env.generic_struct_names.insert(name.clone());
                    } else {
                        let mut field_types = HashMap::new();
                        for field in fields {
                            let field_type =
                                self.env.from_annotation(&field.type_annotation, None)?;
                            field_types.insert(field.name.clone(), field_type);
                        }
                        self.env.add_struct(name, field_types);
                    }
                }
                Statement::FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    generic_params,
                    ..
                } => {
                    // Skip generic functions for now - they need special handling
                    if generic_params.is_empty() {
                        let param_types = params
                            .iter()
                            .map(|(_, type_anno)| self.env.from_annotation(type_anno, None))
                            .collect::<Result<Vec<_>>>()?;

                        let function_return_type = self.env.from_annotation(return_type, None)?;

                        let function_type = Type::Function {
                            params: param_types,
                            return_type: Box::new(function_return_type),
                        };

                        self.env.define(name, function_type);
                    }
                }
                _ => {} // TODO: Handle enum declarations
            }
        }
        // Second pass: Type check all statements (function bodies will be checked here)
        for stmt in statements {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }
        Ok(())
    }

    pub fn check_visibility_access(
        &self,
        accessing_module: &str,
        target_module: &str,
        is_public: bool,
    ) -> std::result::Result<(), String> {
        let _span = tracing::span!(tracing::Level::TRACE, "Checking visibility access");
        let _guard = _span.enter();

        if !is_public && accessing_module != target_module {
            return Err(format!(
                "Cannot access private item from module '{}' in module '{}'",
                target_module, accessing_module
            ));
        }
        Ok(())
    }

    pub fn check_field_access(
        &self,
        field_name: &str,
        struct_name: &str,
        _accessing_module: &str,
        is_public: bool,
    ) -> std::result::Result<(), String> {
        let _span = tracing::span!(tracing::Level::TRACE, "Checking field access");
        let _guard = _span.enter();

        if !is_public {
            return Err(format!(
                "Field '{}' of struct '{}' is private",
                field_name, struct_name
            ));
        }
        Ok(())
    }

    pub fn check_method_access(
        &self,
        method_name: &str,
        type_name: &str,
        _accessing_module: &str,
        is_public: bool,
    ) -> std::result::Result<(), String> {
        let _span = tracing::span!(tracing::Level::TRACE, "Checking method access");
        let _guard = _span.enter();

        if !is_public {
            return Err(format!(
                "Method '{}' of type '{}' is private",
                method_name, type_name
            ));
        }
        Ok(())
    }

    pub fn type_check_statement(&mut self, stmt: &mut Statement) -> Result<()> {
        let _span = tracing::span!(tracing::Level::INFO, "Type checking statement");
        let _guard = _span.enter();

        match stmt {
            Statement::KindDeclaration { .. } => self.type_check_kind_declaration(stmt),
            Statement::Implementation { .. } => self.type_check_implementation(stmt),
            Statement::PlexDeclaration {
                name,
                type_annotation,
                generic_params,
                ..
            } => {
                // Handle generic plex types
                if !generic_params.is_empty() {
                    self.env.push_type_param_scope();
                    for param in generic_params.clone() {
                        let param_name = match &param.name {
                            Some(name) => name.clone(),
                            None => {
                                if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                                    base_name.clone()
                                } else {
                                    "T".to_string()
                                }
                            }
                        };
                        self.env.add_type_param(&param_name);
                    }

                    // Convert the type annotation to a Type and add as generic alias
                    let aliased_type = self.env.from_annotation(type_annotation, None)?;
                    let type_param_names: Vec<String> = generic_params
                        .iter()
                        .map(|param| match &param.name {
                            Some(name) => name.clone(),
                            None => {
                                if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                                    base_name.clone()
                                } else {
                                    "T".to_string()
                                }
                            }
                        })
                        .collect();
                    self.env
                        .add_generic_type_alias(name, aliased_type, type_param_names);

                    self.env.pop_type_param_scope();
                } else {
                    // Regular plex type alias
                    let aliased_type = self.env.from_annotation(type_annotation, None)?;
                    self.env.add_type_alias(name, aliased_type);
                }
                Ok(())
            }
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                is_proc: _,
                is_public: _,
                generic_params,
            } => {
                if !generic_params.is_empty() {
                    // This is a generic function
                    self.type_check_generic_function(
                        name,
                        params,
                        return_type,
                        body,
                        generic_params,
                    )?;
                } else {
                    // Regular function, use existing logic
                    self.type_check_function(name, params, return_type, body)?;
                }
                Ok(())
            }
            Statement::VariableDeclaration {
                name,
                var_kind,
                type_annotation,
                value,
                ..
            } => self.type_check_variable_declaration(
                name,
                var_kind,
                type_annotation.as_ref(),
                &mut **value,
            ),

            Statement::ExprStatement(expr) => {
                self.infer_expression_type(expr)?;
                Ok(())
            }
            Statement::Return(expr_opt) => {
                if let Some(return_type) = &self.current_function_return_type.clone() {
                    if let Some(expr) = expr_opt {
                        let expr_type = self.infer_expression_type(expr)?;
                        self.env.add_constraint(expr_type, return_type.clone());
                    } else {
                        self.env.add_constraint(Type::Unit, return_type.clone());
                    }
                    self.env.solve_constraints()?;
                } else {
                    return Err(VeldError::TypeError(
                        "Cannot return from top-level function".into(),
                    ));
                }
                Ok(())
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => self.type_check_if(condition, then_branch, else_branch),
            Statement::While { condition, body } => self.type_check_while(condition, body),
            Statement::For {
                iterator,
                iterable,
                body,
            } => self.type_check_for(iterator, iterable, body),
            Statement::StructDeclaration {
                name,
                fields,
                generic_params,
                ..
            } => {
                if !generic_params.is_empty() {
                    self.env.push_type_param_scope();
                    for param in generic_params.clone() {
                        let param_name = match &param.name {
                            Some(name) => name.clone(),
                            None => {
                                if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                                    base_name.clone()
                                } else {
                                    "T".to_string()
                                }
                            }
                        };
                        self.env.add_type_param(&param_name);
                    }
                    let mut field_types = HashMap::new();
                    for field in fields {
                        let field_type = self.env.from_annotation(&field.type_annotation, None)?;
                        field_types.insert(field.name.clone(), field_type);
                    }
                    self.env
                        .add_generic_struct(name, field_types, generic_params.clone());
                    self.env.pop_type_param_scope();
                }
                Ok(())
            }
            Statement::BlockScope { body } => {
                tracing::debug!("Type checking block scope");

                self.env.push_scope();

                for stmt in body {
                    self.type_check_statement(stmt)?;
                }

                self.env.pop_scope();
                Ok(())
            }
            Statement::ProcDeclaration {
                name, params, body, ..
            } => {
                tracing::debug!("Type checking proc declaration: {}", name);

                // Type check as a function returning unit
                self.type_check_function(name, params, &TypeAnnotation::Unit, body)
            }
            _ => Ok(()),
        }
    }

    fn type_check_generic_function(
        &mut self,
        name: &str,
        params: &[(String, TypeAnnotation)],
        return_type: &TypeAnnotation,
        body: &[Statement],
        generic_params: &[GenericArgument],
    ) -> Result<()> {
        let _span = tracing::span!(
            tracing::Level::DEBUG,
            "Type checking generic function: {}",
            name
        )
        .entered();

        // Save the current type environment
        let saved_env = self.env.clone();

        // Add type parameters to the environment
        self.env.push_type_param_scope();
        for param in generic_params {
            match &param.type_annotation {
                TypeAnnotation::Basic(param_name) => {
                    self.env.add_type_param(param_name.as_str());
                }
                _ => return Err(VeldError::TypeError("Invalid type parameter".into())),
            }
        }

        // Push a new scope for function parameters
        self.env.push_scope();

        // Add function parameters to the environment
        for (param_name, param_type) in params {
            let param_type_resolved = self.env.from_annotation(param_type, None)?;
            self.env.define(param_name, param_type_resolved);
        }

        // Type check the function body with generic parameters
        let old_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(self.env.from_annotation(return_type, None)?);

        // Type check each statement in the function body
        for stmt in body {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }

        // Resolve function types BEFORE restoring the environment (while type parameters are still in scope)
        let param_types = params
            .iter()
            .map(|(_, type_anno)| self.env.from_annotation(type_anno, None))
            .collect::<Result<Vec<_>>>()?;
        let return_type_resolved = self.env.from_annotation(return_type, None)?;

        // Restore the original environment
        self.current_function_return_type = old_return_type;
        self.env.pop_scope();
        self.env.pop_type_param_scope();
        self.env = saved_env;

        // Register the generic function in the environment
        // For now, we'll create a simplified function type
        // In the future, this could be enhanced to handle proper generic function types
        let function_type = Type::Function {
            params: param_types,
            return_type: Box::new(return_type_resolved),
        };

        self.env.define(name, function_type);

        Ok(())
    }

    pub fn infer_expression_type_with_env(
        &self,
        expr: &Expr,
        env: &mut TypeEnvironment,
    ) -> Result<Type> {
        let _span = tracing::span!(Level::TRACE, "Infer expression type");
        let _enter = _span.enter();

        let mut type_checker = TypeChecker {
            env: env.clone(),
            current_function_return_type: self.current_function_return_type.clone(),
            var_info: self.var_info.clone(),
        };
        type_checker.infer_expression_type(expr)
    }

    pub fn type_check_function(
        &mut self,
        name: &str,
        params: &[(String, TypeAnnotation)],
        return_type: &TypeAnnotation,
        body: &[Statement],
    ) -> Result<()> {
        let _span = tracing::span!(Level::INFO, "Type check function");
        let _enter = _span.enter();

        let param_types = params
            .iter()
            .map(|(_, type_anno)| self.env.from_annotation(type_anno, None))
            .collect::<Result<Vec<_>>>()?;

        // Set up function context for return type checking
        let old_return_type = self.current_function_return_type.clone();
        let function_return_type = self.env.from_annotation(return_type, None)?;
        self.current_function_return_type = Some(function_return_type.clone());

        // Add function to environment before type-checking body (for recursive functions)
        let preliminary_function_type = Type::Function {
            params: param_types.clone(),
            return_type: Box::new(function_return_type.clone()),
        };
        self.env.define(name, preliminary_function_type);

        self.env.push_scope();

        for (i, (param_name, _)) in params.iter().enumerate() {
            self.env.define(param_name, param_types[i].clone());
        }

        // Type check all statements in the function body
        let inferred_return_type = if !body.is_empty() {
            let mut found_type = None;
            for stmt in body {
                // Type check every statement

                // Type check each statement by cloning it to make it mutable
                let mut stmt_clone = stmt.clone();
                self.type_check_statement(&mut stmt_clone)?;

                // Also check for return type inference
                match stmt {
                    Statement::Return(Some(expr)) => {
                        let expr_type = self.infer_expression_type(expr)?;
                        let resolved_type = self.env.apply_substitutions(&expr_type);
                        found_type = Some(resolved_type);
                        break;
                    }
                    Statement::ExprStatement(expr) if stmt == body.last().unwrap() => {
                        let expr_type = self.infer_expression_type(expr)?;
                        let resolved_type = self.env.apply_substitutions(&expr_type);
                        found_type = Some(resolved_type);
                    }
                    _ => {}
                }
            }
            found_type.unwrap_or(Type::Unit)
        } else {
            Type::Unit
        };

        let actual_return_type = match return_type {
            TypeAnnotation::Basic(name) if name == "infer" => inferred_return_type.clone(),
            _ => {
                let specified_type = self.env.from_annotation(return_type, None)?;
                self.env
                    .add_constraint(inferred_return_type.clone(), specified_type.clone());
                specified_type
            }
        };
        self.env.solve_constraints()?;
        self.env.pop_scope();

        // Restore the original return type context
        self.current_function_return_type = old_return_type;

        let function_type = Type::Function {
            params: param_types.clone(),
            return_type: Box::new(actual_return_type),
        };

        // Update function in environment with final type (may be different from preliminary)
        self.env.define(name, function_type);
        Ok(())
    }

    pub fn check_assignment(&self, name: &str, _value: &Expr) -> Result<()> {
        let _span = tracing::span!(Level::INFO, "Type check assignment");
        let _enter = _span.enter();

        if let Some(var_info) = self.var_info.get(name) {
            match var_info.var_kind() {
                VarKind::Const => {
                    return Err(VeldError::TypeError(format!(
                        "Cannot assign to constant '{}'",
                        name
                    )));
                }
                VarKind::Let => {
                    return Err(VeldError::TypeError(format!(
                        "Cannot assign immutable variable '{}'",
                        name
                    )));
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn evaluate_const_expr(&self, expr: &Expr) -> Result<Value> {
        let _span = tracing::span!(Level::INFO, "Evaluate constant expression");
        let _enter = _span.enter();

        match expr {
            Expr::Literal(lit) => Ok(match lit {
                Literal::Integer(n) => Value::Integer(*n),
                Literal::Float(n) => Value::Float(*n),
                Literal::String(s) => Value::String(s.clone()),
                Literal::Boolean(b) => Value::Boolean(*b),
                Literal::Char(c) => Value::Char(*c),
                Literal::Unit => Value::Unit,
            }),
            Expr::Identifier(name) => {
                if let Some(var_info) = self.var_info.get(name) {
                    if let Some(value) = var_info.value() {
                        Ok(value.clone())
                    } else {
                        Err(VeldError::TypeError(format!(
                            "Cannot use non-constant '{}' in expression",
                            name
                        )))
                    }
                } else {
                    Err(VeldError::TypeError(format!(
                        "Undefined constant '{}'",
                        name
                    )))
                }
            }
            Expr::BinaryOp {
                left,
                operator: _,
                right,
            } => {
                let _left_val = self.evaluate_const_expr(left)?;
                let _right_val = self.evaluate_const_expr(right)?;
                todo!("implement const binary operations")
            }
            _ => Err(VeldError::TypeError(format!(
                "Cannot evaluate constant expression for: {:?}",
                expr
            ))),
        }
    }

    fn type_check_variable_declaration(
        &mut self,
        name: &str,
        var_kind: &VarKind,
        type_annotation: Option<&TypeAnnotation>,
        value: &mut Expr,
    ) -> Result<()> {
        let _span = tracing::span!(Level::INFO, "Type check variable declaration");
        let _enter = _span.enter();

        // Patch: Fill in missing type_args for EnumVariant from annotation
        if let Expr::EnumVariant {
            enum_name,
            type_args,
            ..
        } = value
        {
            if type_args.is_none() {
                if let Some(TypeAnnotation::Generic {
                    base,
                    type_args: anno_args,
                }) = type_annotation
                {
                    if base == enum_name {
                        *type_args = Some(anno_args.clone());
                    }
                }
            }
        }

        // Check if this is a potentially recursive lambda assignment
        let is_recursive_lambda = matches!(value, Expr::Lambda { .. });
        tracing::debug!(
            "Variable '{}' is_recursive_lambda: {}",
            name,
            is_recursive_lambda
        );

        let value_type = if is_recursive_lambda && self.lambda_references_name(value, name) {
            tracing::debug!(
                "Detected recursive lambda for type checking variable: {}",
                name
            );
            tracing::debug!(
                "Current scope depth before pre-binding: {}",
                self.env.scope_depth()
            );

            // For recursive lambdas, pre-bind the variable with a temporary function type
            let temp_type = self.env.fresh_type_var();
            self.env.define(name, temp_type.clone());
            tracing::debug!(
                "Pre-bound '{}' with temp type: {:?} at scope depth {}",
                name,
                temp_type,
                self.env.scope_depth()
            );

            // Verify the variable is accessible
            if let Some(found_type) = self.env.get(name) {
                tracing::debug!(
                    "Verified '{}' is accessible with type: {:?}",
                    name,
                    found_type
                );
            } else {
                tracing::error!("Failed to find '{}' after pre-binding!", name);
            }

            let lambda_type = self.infer_expression_type(value)?;
            tracing::debug!("Inferred lambda type: {:?}", lambda_type);

            // Unify the temporary type with the actual lambda type
            self.env.add_constraint(temp_type, lambda_type.clone());
            tracing::debug!("Added constraint for recursive lambda '{}'", name);

            lambda_type
        } else {
            tracing::debug!(
                "Non-recursive variable declaration for type checking: {}",
                name
            );
            self.infer_expression_type(value)?
        };
        tracing::debug!("Variable '{}' value type: {:?}", name, value_type);

        let const_value = if matches!(var_kind, VarKind::Const) {
            Some(self.evaluate_const_expr(value)?)
        } else {
            None
        };

        let var_type = if let Some(anno) = type_annotation {
            let specified_type = self.env.from_annotation(anno, None)?;
            tracing::debug!("Variable '{}' specified type: {:?}", name, specified_type);

            // Special handling for integer literals that fit in target type
            if let Some(literal_value) = self.extract_integer_literal_value(value) {
                if self.literal_fits_in_type(literal_value, &specified_type) {
                    // Allow the literal to be coerced to the target type
                    self.env
                        .add_constraint(specified_type.clone(), specified_type.clone());
                    specified_type
                } else {
                    return Err(VeldError::TypeError(format!(
                        "Integer literal {} does not fit in type {}",
                        literal_value, specified_type
                    )));
                }
            } else if let (Type::Array(expected_elem), Type::Array(actual_elem)) =
                (&specified_type, &value_type)
            {
                // For arrays, be extra strict about type checking
                if !self.types_compatible(actual_elem, expected_elem) {
                    return Err(VeldError::TypeError(format!(
                        "Type mismatch for variable '{}': expected array of {}, got array of {}",
                        name, expected_elem, actual_elem
                    )));
                }
                self.env
                    .add_constraint(value_type.clone(), specified_type.clone());
                specified_type
            } else if let Some(coerced_type) = self.try_coerce_type(&value_type, &specified_type) {
                // Use the coerced type for safe widening conversions
                self.env
                    .add_constraint(coerced_type.clone(), specified_type.clone());
                specified_type
            } else if !self.types_compatible(&value_type, &specified_type) {
                return Err(VeldError::TypeError(format!(
                    "Type mismatch for variable '{}': expected {}, got {}",
                    name, specified_type, value_type
                )));
            } else {
                self.env
                    .add_constraint(value_type.clone(), specified_type.clone());
                specified_type
            }
        } else {
            value_type
        };

        self.env.solve_constraints()?;
        let final_type = self.env.apply_substitutions(&var_type);
        tracing::debug!("Variable '{}' final type: {:?}", name, final_type);

        self.var_info.insert(
            name.to_string(),
            VarInfo::new(final_type.clone(), var_kind.clone(), const_value),
        );

        // Only define if not already defined (for recursive lambdas)
        if !self.env.is_defined(name) {
            self.env.define(name, final_type);
        } else {
            // Update the existing definition for recursive lambdas
            self.env.update_definition(name, final_type);
        }
        Ok(())
    }

    // Helper function to check if a lambda expression references a given variable name
    fn lambda_references_name(&self, expr: &Expr, name: &str) -> bool {
        match expr {
            Expr::Lambda { body, .. } => self.expr_references_name(body, name),
            _ => false,
        }
    }

    // Helper function to check if an expression references a given variable name
    fn expr_references_name(&self, expr: &Expr, name: &str) -> bool {
        match expr {
            Expr::Identifier(id) => id == name,
            Expr::Call { callee, arguments } => {
                self.expr_references_name(callee, name)
                    || arguments.iter().any(|arg| match arg {
                        Argument::Positional(e) => self.expr_references_name(e, name),
                        Argument::Named { name: _, value: e } => self.expr_references_name(e, name),
                    })
            }
            Expr::BinaryOp { left, right, .. } => {
                self.expr_references_name(left, name) || self.expr_references_name(right, name)
            }
            Expr::UnaryOp { operand, .. } => self.expr_references_name(operand, name),
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                self.expr_references_name(condition, name)
                    || self.expr_references_name(then_expr, name)
                    || else_expr
                        .as_ref()
                        .map_or(false, |e| self.expr_references_name(e, name))
            }
            Expr::Lambda { body, .. } => self.expr_references_name(body, name),
            Expr::BlockExpression { statements, .. } => statements
                .iter()
                .any(|stmt| self.stmt_references_name(stmt, name)),
            // Add other expression types as needed
            _ => false,
        }
    }

    // Helper function to check if a statement references a given variable name
    fn stmt_references_name(&self, stmt: &Statement, name: &str) -> bool {
        match stmt {
            Statement::ExprStatement(expr) => self.expr_references_name(expr, name),
            Statement::VariableDeclaration { value, .. } => self.expr_references_name(value, name),
            Statement::Assignment { value, .. } => self.expr_references_name(value, name),
            // Add other statement types as needed
            _ => false,
        }
    }

    fn type_check_if(
        &mut self,
        condition: &Expr,
        then_branch: &[Statement],
        else_branch: &Option<Vec<Statement>>,
    ) -> Result<()> {
        let _span = tracing::span!(Level::INFO, "Type check if statement");
        let _enter = _span.enter();

        let cond_type = self.infer_expression_type(condition)?;
        self.env.add_constraint(cond_type, Type::Bool);

        self.env.push_scope();
        for stmt in then_branch {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }
        self.env.pop_scope();

        if let Some(else_stmts) = else_branch {
            self.env.push_scope();
            for stmt in else_stmts {
                let mut stmt_mut = stmt.clone();
                self.type_check_statement(&mut stmt_mut)?;
            }
            self.env.pop_scope();
        }
        self.env.solve_constraints()?;
        Ok(())
    }

    fn type_check_while(&mut self, condition: &Expr, body: &[Statement]) -> Result<()> {
        let _span = tracing::span!(Level::DEBUG, "Type check while statement");
        let _enter = _span.enter();

        let cond_type = self.infer_expression_type(condition)?;
        self.env.add_constraint(cond_type, Type::Bool);
        self.env.push_scope();
        for stmt in body {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }
        self.env.pop_scope();

        self.env.solve_constraints()?;
        Ok(())
    }

    fn type_check_for(
        &mut self,
        iterator: &str,
        iterable: &Expr,
        body: &[Statement],
    ) -> Result<()> {
        let _span = tracing::span!(Level::DEBUG, "Type check for statement");
        let _enter = _span.enter();

        let iterable_type = self.infer_expression_type(iterable)?;

        let iterator_type = match &iterable_type {
            Type::Array(elem_type) => *elem_type.clone(),
            Type::Generic { base, type_args } if base == "Array" && type_args.len() == 1 => {
                type_args[0].clone()
            }
            _ => Type::Any,
        };

        self.env.push_scope();
        self.env.define(iterator, iterator_type);

        for stmt in body {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }

        self.env.pop_scope();
        self.env.solve_constraints()?;
        Ok(())
    }

    pub fn type_check_struct_declaration(&mut self, stmt: &Statement) -> Result<()> {
        let _span = tracing::span!(Level::INFO, "Type check struct declaration");
        let _enter = _span.enter();

        if let Statement::StructDeclaration {
            name,
            fields,
            generic_params,
            ..
        } = stmt
        {
            self.env.push_type_param_scope();

            for param in generic_params {
                let param_name = match &param.name {
                    Some(name) => name.clone(),
                    None => {
                        if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                            base_name.clone()
                        } else {
                            "T".to_string()
                        }
                    }
                };
                self.env.add_type_param(&param_name);
            }

            let mut field_types = HashMap::new();
            for field in fields {
                let field_type = self.env.from_annotation(&field.type_annotation, None)?;
                field_types.insert(field.name.clone(), field_type);
            }
            self.env.add_struct(name, field_types);

            self.env.pop_type_param_scope();
        }
        Ok(())
    }

    fn infer_block_return_type(&mut self, body: &[Statement]) -> Result<Type> {
        let _span = tracing::span!(Level::DEBUG, "Infer block return type");
        let _enter = _span.enter();

        if body.is_empty() {
            return Ok(Type::Unit);
        }

        // Look for explicit returns first
        for stmt in body {
            if let Statement::Return(expr_opt) = stmt {
                return if let Some(expr) = expr_opt {
                    self.infer_expression_type(expr)
                } else {
                    Ok(Type::Unit)
                };
            }
        }

        // If no explicit return, check if last statement is an expression
        if let Some(Statement::ExprStatement(expr)) = body.last() {
            self.infer_expression_type(expr)
        } else {
            Ok(Type::Unit)
        }
    }

    fn infer_block_lambda_type(
        &mut self,
        params: &[(String, Option<TypeAnnotation>)],
        body: &[Statement],
        return_type_anno: Option<&TypeAnnotation>,
    ) -> Result<Type> {
        let _span = tracing::span!(Level::DEBUG, "Infer block lambda type");
        let _enter = _span.enter();

        self.env.push_scope();

        let mut param_types = Vec::new();
        for (name, type_anno) in params {
            let param_type = if let Some(anno) = type_anno {
                self.env.from_annotation(anno, None)?
            } else {
                self.env.fresh_type_var()
            };
            param_types.push(param_type.clone());
            self.env.define(name, param_type);
        }

        // Type check all statements in the body first
        for stmt in body {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }

        // Infer return type from body
        let body_type = if body.is_empty() {
            Type::Unit
        } else {
            self.infer_block_return_type(body)?
        };

        let return_type = if let Some(anno) = return_type_anno {
            let rt = self.env.from_annotation(anno, None)?;
            self.env.add_constraint(body_type, rt.clone());
            rt
        } else {
            body_type
        };

        self.env.pop_scope();
        self.env.solve_constraints()?;

        let final_return_type = self.env.apply_substitutions(&return_type);
        let final_param_types = param_types
            .iter()
            .map(|t| self.env.apply_substitutions(t))
            .collect();

        Ok(Type::Function {
            params: final_param_types,
            return_type: Box::new(final_return_type),
        })
    }

    pub fn infer_expression_type(&mut self, expr: &Expr) -> Result<Type> {
        let _span = tracing::span!(Level::INFO, "Infer expression type");
        let _enter = _span.enter();

        let result = match expr {
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_type = self.infer_expression_type(condition)?;
                self.env.add_constraint(cond_type, Type::Bool);

                let then_type = self.infer_expression_type(then_expr)?;

                if let Some(else_expr) = else_expr {
                    let else_type = self.infer_expression_type(else_expr)?;
                    self.env.add_constraint(then_type.clone(), else_type);
                    self.env.solve_constraints()?;
                    Ok(self.env.apply_substitutions(&then_type))
                } else {
                    // No else branch means it returns Unit when condition is false
                    self.env.add_constraint(then_type.clone(), Type::Unit);
                    self.env.solve_constraints()?;
                    Ok(Type::Unit)
                }
            }
            Expr::BlockLambda {
                params,
                body,
                return_type,
            } => self.infer_block_lambda_type(params, body, return_type.as_ref()),
            Expr::Literal(lit) => {
                let ty = self.infer_literal_type(lit)?;
                tracing::debug!("Inferred literal type: {:?} for {:?}", ty, lit);
                Ok(ty)
            }
            Expr::UnitLiteral => Ok(Type::Unit),
            Expr::Identifier(name) => self
                .env
                .get(name)
                .ok_or_else(|| VeldError::TypeError(format!("Undefined identifier: {}", name))),
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => self.infer_binary_op_type(left, operator, right),
            Expr::FunctionCall { name, arguments } => {
                self.infer_function_call_type(name, arguments)
            }
            Expr::Lambda {
                params,
                body,
                return_type,
            } => {
                tracing::debug!(
                    "Inferring lambda type with return_type hint: {:?}",
                    return_type
                );
                let inferred = self.infer_lambda_type(params, body, return_type.as_ref())?;
                tracing::debug!("Inferred lambda type: {:?}", inferred);
                Ok(inferred)
            }
            Expr::MethodCall {
                object,
                method,
                arguments,
            } => self.infer_method_call_type(object, method, arguments),
            Expr::PropertyAccess { object, property } => {
                self.infer_property_access_type(object, property)
            }
            Expr::StructCreate {
                struct_name,
                fields,
            } => self.infer_struct_create_type(struct_name, fields),
            Expr::ArrayLiteral(elements) => self.infer_array_literal_type(elements),
            Expr::IndexAccess { object, index } => self.infer_index_access_type(object, index),
            Expr::TypeCast { expr, target_type } => self.type_check_cast(expr, target_type),
            Expr::BlockExpression {
                statements,
                final_expr,
            } => {
                self.env.push_scope();

                // Type check all statements
                for stmt in statements {
                    let mut stmt_mut = stmt.clone();
                    self.type_check_statement(&mut stmt_mut)?;
                }

                // Infer type from final expression or return unit
                let result_type = if let Some(expr) = final_expr {
                    self.infer_expression_type(expr)?
                } else {
                    Type::Unit
                };

                self.env.pop_scope();
                Ok(result_type)
            }
            Expr::UnaryOp { operator, operand } => self.infer_unary_op_type(operator, operand),
            Expr::EnumVariant {
                enum_name,
                variant_name,
                fields,
                type_args,
            } => self.infer_enum_variant_type(enum_name, variant_name, fields, type_args.as_ref()),
            Expr::SelfReference { .. } => todo!("Self reference in expression"),
            Expr::TupleLiteral(_) => todo!("Tuple literal type inference"),
            Expr::TupleAccess { .. } => todo!("Tuple access type inference"),
            Expr::MacroExpr { .. } => todo!("Macro expression type inference"),
            Expr::MacroVar(_) => todo!("Macro variable type inference"),
            Expr::Call { callee, arguments } => {
                let callee_type = self.infer_expression_type(callee)?;

                match callee_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        if params.len() != arguments.len() {
                            return Err(VeldError::TypeError(format!(
                                "Function call expects {} arguments, but {} were provided",
                                params.len(),
                                arguments.len()
                            )));
                        }

                        for (i, arg) in arguments.iter().enumerate() {
                            let arg_expr = match arg {
                                Argument::Positional(expr) => expr,
                                Argument::Named { name: _, value } => value,
                            };

                            let arg_type = self.infer_expression_type(arg_expr)?;
                            self.env.add_constraint(arg_type, params[i].clone());
                        }

                        self.env.solve_constraints()?;
                        Ok(*return_type)
                    }
                    _ => {
                        // Try to treat as function call by identifier name
                        if let Expr::Identifier(name) = &**callee {
                            self.infer_function_call_type(name, arguments)
                        } else {
                            Err(VeldError::TypeError(format!(
                                "Cannot call non-function value of type {}",
                                callee_type
                            )))
                        }
                    }
                }
            }
            Expr::Range {
                start,
                end,
                inclusive,
            } => self.infer_range_type(
                start.as_ref().map(|v| &**v),
                end.as_ref().map(|v| &**v),
                *inclusive,
            ),
            Expr::Record { fields: f } => {
                // Fix: convert Vec<(String, Expr)> to HashMap<String, Type>
                let mut field_types = std::collections::HashMap::new();
                for (field_name, field_expr) in f {
                    let field_type = self.infer_expression_type(field_expr)?;
                    field_types.insert(field_name.clone(), field_type);
                }
                Ok(Type::Record {
                    fields: field_types,
                })
            }
        };
        if let Ok(ref t) = result {
            tracing::debug!("Final inferred type for expression: {:?} -> {:?}", expr, t);
        }
        result
    }

    fn is_valid_cast(&self, from: &Type, to: &Type) -> bool {
        let _span = tracing::span!(Level::TRACE, "Check valid cast");
        let _enter = _span.enter();

        // Numeric casts
        match (from, to) {
            // Numeric casts between integers
            (
                Type::I8 | Type::I16 | Type::I32 | Type::I64,
                Type::I8 | Type::I16 | Type::I32 | Type::I64,
            ) => true,
            (
                Type::U8 | Type::U16 | Type::U32 | Type::U64,
                Type::U8 | Type::U16 | Type::U32 | Type::U64,
            ) => true,

            // Mixed signed/unsigned casts
            (
                Type::I8 | Type::I16 | Type::I32 | Type::I64,
                Type::U8 | Type::U16 | Type::U32 | Type::U64,
            ) => true,
            (
                Type::U8 | Type::U16 | Type::U32 | Type::U64,
                Type::I8 | Type::I16 | Type::I32 | Type::I64,
            ) => true,

            // Integer to float casts
            (
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64,
                Type::F32 | Type::F64,
            ) => true,

            // Float to integer casts
            (
                Type::F32 | Type::F64,
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64,
            ) => true,

            // Float to float casts
            (Type::F32 | Type::F64, Type::F32 | Type::F64) => true,

            // String casts to numeric types if it can be parsed
            (Type::String, Type::I32 | Type::I64 | Type::F32 | Type::F64) => true,

            // String conversions (from numeric types)
            (
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::F32
                | Type::F64,
                Type::String,
            ) => true,

            // Char casts to string
            (Type::Char, Type::String) => true,

            // Any type to any
            (Type::Any, _) | (_, Type::Any) => true,

            // Structs and enums can be cast to their base type
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,

            // Record type casts - allow if structurally compatible
            (
                Type::Record {
                    fields: from_fields,
                },
                Type::Record { fields: to_fields },
            ) => {
                // Check if all fields in target type exist in source type with compatible types
                to_fields.iter().all(|(field_name, to_type)| {
                    if let Some(from_type) = from_fields.get(field_name) {
                        // Recursively check if the field types are compatible (same type, safe widening, or valid cast)
                        from_type == to_type
                            || self.is_safe_widening(from_type, to_type)
                            || self.is_valid_cast(from_type, to_type)
                    } else {
                        false
                    }
                })
            }

            // Same type is valid
            _ if from == to => true,

            // All other casts are invalid for now
            _ => false,
        }
    }

    fn type_check_cast(&mut self, expr: &Expr, target_type: &TypeAnnotation) -> Result<Type> {
        let _span = tracing::span!(Level::TRACE, "Type check cast");
        let _enter = _span.enter();

        let expr_type = self.infer_expression_type(expr)?;
        let target = self.env.from_annotation(target_type, None)?;

        if self.is_valid_cast(&expr_type, &target) {
            Ok(target)
        } else {
            Err(VeldError::TypeError(format!(
                "Invalid cast from {} to {}",
                expr_type, target
            )))
        }
    }

    fn infer_unary_op_type(&mut self, operator: &UnaryOperator, operand: &Expr) -> Result<Type> {
        let _span = tracing::span!(Level::DEBUG, "Infer unary op type");
        let _enter = _span.enter();

        let operand_type = self.infer_expression_type(operand)?;

        match operator {
            UnaryOperator::Negate => {
                if operand_type.is_numeric() {
                    Ok(operand_type)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply unary negation to type {}",
                        operand_type
                    )))
                }
            }
            UnaryOperator::Not => {
                if operand_type == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply logical negation to type {}",
                        operand_type
                    )))
                }
            }
        }
    }

    fn infer_literal_type(&mut self, lit: &Literal) -> Result<Type> {
        let _span = tracing::span!(Level::DEBUG, "Infer literal type");
        let _enter = _span.enter();

        match lit {
            Literal::Integer(value) => {
                // Default to i32 for better compatibility, only use larger types when necessary
                let inferred_type = if *value >= i32::MIN as i64 && *value <= i32::MAX as i64 {
                    Type::I32
                } else {
                    Type::I64
                };

                tracing::debug!("Inferred integer literal {} as {:?}", value, inferred_type);
                Ok(inferred_type)
            }
            Literal::Float(value) => {
                // For now, default to f64, but could be context-sensitive
                let inferred_type = if *value >= f32::MIN as f64 && *value <= f32::MAX as f64 {
                    Type::F32
                } else {
                    Type::F64
                };

                tracing::debug!("Inferred float literal {} as {:?}", value, inferred_type);
                Ok(inferred_type)
            }
            Literal::String(_) => {
                tracing::debug!("Inferred string literal as str");
                Ok(Type::String)
            }
            Literal::Boolean(_) => {
                tracing::debug!("Inferred boolean literal as bool");
                Ok(Type::Bool)
            }
            Literal::Unit => {
                tracing::debug!("Inferred unit literal as unit");
                Ok(Type::Unit)
            }
            Literal::Char(_) => {
                tracing::debug!("Inferred char literal as char");
                Ok(Type::Char)
            }
        }
    }

    fn is_64bit_type(&self, ty: &Type) -> bool {
        let _span = tracing::span!(tracing::Level::TRACE, "is_64bit_type", ty = ?ty);
        let _enter = _span.enter();
        matches!(ty, Type::I64 | Type::U64 | Type::F64)
    }

    fn is_integer_type(&self, ty: &Type) -> bool {
        let _span = tracing::span!(tracing::Level::TRACE, "is_integer_type", ty = ?ty);
        let _enter = _span.enter();
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
        )
    }

    fn promote_numeric_types(&self, left: &Type, right: &Type) -> Type {
        let _span = tracing::span!(tracing::Level::DEBUG, "promote_numeric_types", left = ?left, right = ?right);
        let _enter = _span.enter();

        match (left, right) {
            // Float promotion rules
            (Type::F64, _) | (_, Type::F64) => Type::F64,
            (Type::F32, _) | (_, Type::F32) => {
                // If either operand would overflow f32, promote to f64
                if self.is_64bit_type(left) || self.is_64bit_type(right) {
                    Type::F64
                } else {
                    Type::F32
                }
            }

            // Integer promotion rules
            (Type::I64, _) | (_, Type::I64) => Type::I64,
            (Type::U64, _) | (_, Type::U64) => Type::U64,
            (Type::I32, _) | (_, Type::I32) => Type::I32,
            (Type::U32, _) | (_, Type::U32) => Type::U32,
            (Type::I16, _) | (_, Type::I16) => Type::I16,
            (Type::U16, _) | (_, Type::U16) => Type::U16,
            (Type::I8, _) | (_, Type::I8) => Type::I8,
            (Type::U8, _) | (_, Type::U8) => Type::U8,

            // Default fallback
            _ => Type::I32,
        }
    }

    fn infer_binary_op_type(
        &mut self,
        left: &Expr,
        op: &BinaryOperator,
        right: &Expr,
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "infer_binary_op_type", left = ?left, op = ?op, right = ?right);
        let _enter = _span.enter();

        // Handle pipe operator specially to avoid premature type checking of the right side
        if matches!(op, BinaryOperator::Pipe) {
            return self.infer_pipe_type(left, right);
        }

        let left_type = self.infer_expression_type(left)?;
        let right_type = self.infer_expression_type(right)?;

        if let Some(result_type) = self.try_resolve_operator(&left_type, op, &right_type)? {
            return Ok(result_type);
        }

        match op {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Ok(self.promote_numeric_types(&left_type, &right_type))
                } else if matches!(op, BinaryOperator::Add) {
                    // String concatenation
                    match (&left_type, &right_type) {
                        (Type::String, Type::String) => Ok(Type::String),
                        _ => Err(VeldError::TypeError(format!(
                            "Cannot apply {} to {} and {}",
                            op, left_type, right_type
                        ))),
                    }
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply {} to {} and {}",
                        op, left_type, right_type
                    )))
                }
            }

            BinaryOperator::Modulo => {
                if self.is_integer_type(&left_type) && self.is_integer_type(&right_type) {
                    Ok(self.promote_numeric_types(&left_type, &right_type))
                } else {
                    Err(VeldError::TypeError(format!(
                        "Modulo operation requires integer types, got {} and {}",
                        left_type, right_type
                    )))
                }
            }

            BinaryOperator::Exponent => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    // Exponentiation always promotes to float
                    Ok(Type::F64)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Exponentiation requires numeric types, got {} and {}",
                        left_type, right_type
                    )))
                }
            }

            BinaryOperator::LessEq
            | BinaryOperator::GreaterEq
            | BinaryOperator::Less
            | BinaryOperator::Greater => {
                if (self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type))
                    || (left_type == Type::String && right_type == Type::String)
                {
                    Ok(Type::Bool)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot compare {} and {}",
                        left_type, right_type
                    )))
                }
            }

            BinaryOperator::EqualEqual | BinaryOperator::NotEqual => Ok(Type::Bool),

            BinaryOperator::And | BinaryOperator::Or => {
                self.env.add_constraint(left_type, Type::Bool);
                self.env.add_constraint(right_type, Type::Bool);
                self.env.solve_constraints()?;
                Ok(Type::Bool)
            }
            BinaryOperator::Pipe => {
                // This should never be reached since pipe is handled earlier
                unreachable!("Pipe operator should be handled by infer_pipe_type")
            }
        }
    }

    fn is_numeric_type(&self, ty: &Type) -> bool {
        let _span = tracing::span!(tracing::Level::DEBUG, "is_numeric_type", ty = ?ty);
        let _enter = _span.enter();

        matches!(
            ty,
            Type::I32
                | Type::F64
                | Type::I64
                | Type::F32
                | Type::U32
                | Type::U64
                | Type::U8
                | Type::U16
                | Type::I8
                | Type::I16
                | TypeVar(_)
        ) || self.env.is_type_var_numeric(ty)
    }

    fn types_compatible(&self, t1: &Type, t2: &Type) -> bool {
        let _span = tracing::span!(tracing::Level::INFO, "types_compatible", t1 = ?t1, t2 = ?t2);
        let _enter = _span.enter();

        let t1 = self.env.apply_substitutions(t1);
        let t2 = self.env.apply_substitutions(t2);

        match (&t1, &t2) {
            _ if t1 == t2 => true,

            (Type::Any, _) | (_, Type::Any) => true,

            (TypeVar(_), _) | (_, TypeVar(_)) => true,

            // For arrays, check element types recursively
            (Type::Array(elem1), Type::Array(elem2)) => self.types_compatible(elem1, elem2),

            // For generic types, check base and all type arguments
            (
                Type::Generic {
                    base: base1,
                    type_args: args1,
                },
                Type::Generic {
                    base: base2,
                    type_args: args2,
                },
            ) => {
                // Same base name is required
                if base1 != base2 {
                    return false;
                }

                // If either has empty type args, consider them compatible (lenient mode)
                // This handles cases like Result<> being compatible with Result<i32, str>
                if args1.is_empty() || args2.is_empty() {
                    return true;
                }

                // Otherwise, check type args compatibility
                args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(a1, a2)| self.types_compatible(a1, a2))
            }

            // For structs, check names match
            (Type::Struct { name: name1, .. }, Type::Struct { name: name2, .. }) => name1 == name2,

            // For enums, check names match
            (Type::Enum { name: name1, .. }, Type::Enum { name: name2, .. }) => name1 == name2,

            // For records, check that all fields match with compatible types (including safe coercions)
            (Type::Record { fields: fields1 }, Type::Record { fields: fields2 }) => {
                fields1.len() == fields2.len()
                    && fields1.iter().all(|(name, type1)| {
                        fields2.get(name).map_or(false, |type2| {
                            self.types_compatible(type1, type2)
                                || self.is_safe_widening(type1, type2)
                        })
                    })
            }

            // Allow safe widening conversions
            (actual, target) if self.is_safe_widening(actual, target) => true,

            _ if self.is_numeric_type(&t1) && self.is_numeric_type(&t2) => true,
            _ => false,
        }
    }

    /// Check if a type conversion is a safe widening conversion (no data loss)
    fn is_safe_widening(&self, from_type: &Type, to_type: &Type) -> bool {
        let _span = tracing::span!(tracing::Level::INFO, "is_safe_widening", from_type = ?from_type, to_type = ?to_type);
        let _enter = _span.enter();

        match (from_type, to_type) {
            // Same type is always safe
            (a, b) if a == b => true,

            // Integer to larger integer is safe
            (Type::I8, Type::I16 | Type::I32 | Type::I64) => true,
            (Type::I16, Type::I32 | Type::I64) => true,
            (Type::I32, Type::I64) => true,

            // Unsigned to larger unsigned is safe
            (Type::U8, Type::U16 | Type::U32 | Type::U64) => true,
            (Type::U16, Type::U32 | Type::U64) => true,
            (Type::U32, Type::U64) => true,

            // Small integers to floats is safe (within precision limits)
            (Type::I8 | Type::I16 | Type::I32, Type::F32 | Type::F64) => true,
            (Type::U8 | Type::U16 | Type::U32, Type::F32 | Type::F64) => true,

            // I64/U64 to F64 is safe (F64 has 53 bits of precision)
            (Type::I64 | Type::U64, Type::F64) => true,

            // F32 to F64 is safe
            (Type::F32, Type::F64) => true,

            _ => false,
        }
    }

    /// Attempt to coerce any type to match target type through safe widening
    fn try_coerce_type(&self, actual_type: &Type, target_type: &Type) -> Option<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "try_coerce_type", actual_type = ?actual_type, target_type = ?target_type);
        let _enter = _span.enter();

        match (actual_type, target_type) {
            // Direct safe widening for primitive types
            (actual, target) if self.is_safe_widening(actual, target) => Some(target.clone()),

            // Record type coercion
            (
                Type::Record {
                    fields: actual_fields,
                },
                Type::Record {
                    fields: target_fields,
                },
            ) => {
                // Check if we can coerce all fields
                if actual_fields.len() != target_fields.len() {
                    return None;
                }

                let mut coerced_fields = std::collections::HashMap::new();

                for (field_name, target_field_type) in target_fields {
                    if let Some(actual_field_type) = actual_fields.get(field_name) {
                        if self.is_safe_widening(actual_field_type, target_field_type) {
                            coerced_fields.insert(field_name.clone(), target_field_type.clone());
                        } else if actual_field_type == target_field_type {
                            coerced_fields.insert(field_name.clone(), target_field_type.clone());
                        } else {
                            // If any field can't be safely coerced, fail
                            return None;
                        }
                    } else {
                        // Missing field
                        return None;
                    }
                }

                Some(Type::Record {
                    fields: coerced_fields,
                })
            }
            _ => None,
        }
    }

    /// Extract the integer value from an expression if it's an integer literal or negated integer literal
    fn extract_integer_literal_value(&self, expr: &Expr) -> Option<i64> {
        match expr {
            // Direct integer literal
            Expr::Literal(Literal::Integer(value)) => Some(*value),
            // Negated integer literal
            Expr::UnaryOp {
                operator: UnaryOperator::Negate,
                operand,
            } => {
                if let Expr::Literal(Literal::Integer(value)) = &**operand {
                    Some(-value)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Check if an integer literal value fits within a target type's range
    fn literal_fits_in_type(&self, literal_value: i64, target_type: &Type) -> bool {
        match target_type {
            Type::I8 => literal_value >= i8::MIN as i64 && literal_value <= i8::MAX as i64,
            Type::I16 => literal_value >= i16::MIN as i64 && literal_value <= i16::MAX as i64,
            Type::I32 => literal_value >= i32::MIN as i64 && literal_value <= i32::MAX as i64,
            Type::I64 => true, // i64 can hold any literal value we parse
            Type::U8 => literal_value >= 0 && literal_value <= u8::MAX as i64,
            Type::U16 => literal_value >= 0 && literal_value <= u16::MAX as i64,
            Type::U32 => literal_value >= 0 && literal_value <= u32::MAX as i64,
            Type::U64 => literal_value >= 0, // Assuming our literals are positive or within i64 range
            Type::F32 | Type::F64 => true, // Floats can represent most integer values (with potential precision loss)
            _ => false,                    // Non-numeric types can't hold integer literals
        }
    }

    pub fn validate_value(&self, value: &Value, expected_type: &Type) -> Result<()> {
        let _span = tracing::span!(tracing::Level::INFO, "validate_value", value = ?value, expected_type = ?expected_type);
        let _enter = _span.enter();

        match (value, expected_type) {
            (Value::Integer(_), Type::I32) | (Value::Integer(_), Type::I64) => Ok(()),

            (Value::Float(_), Type::F32) | (Value::Float(_), Type::F64) => Ok(()),

            (Value::String(_), Type::String) => Ok(()),

            (Value::Boolean(_), Type::Bool) => Ok(()),

            (Value::Char(_), Type::Char) => Ok(()),

            (Value::Unit, Type::Unit) => Ok(()),

            (Value::Array(elements), Type::Array(elem_type)) => {
                for elem in elements {
                    self.validate_value(elem, elem_type)?;
                }
                Ok(())
            }

            (
                Value::Struct {
                    name: struct_name,
                    fields,
                },
                Type::Struct {
                    name: type_name,
                    fields: type_fields,
                },
            ) => {
                if struct_name != type_name {
                    return Err(VeldError::TypeError(format!(
                        "Expected struct of type {}, got {}",
                        type_name, struct_name
                    )));
                }

                for (field_name, field_type) in type_fields {
                    if let Some(field_value) = fields.get(field_name) {
                        self.validate_value(field_value, field_type)?;
                    } else {
                        return Err(VeldError::TypeError(format!(
                            "Missing field {} in struct {}",
                            field_name, struct_name
                        )));
                    }
                }

                Ok(())
            }

            // Add other type validation cases as needed
            _ => Err(VeldError::TypeError(format!(
                "Type mismatch: value {:?} does not match type {}",
                value, expected_type
            ))),
        }
    }

    fn infer_function_call_type(&mut self, name: &str, args: &[Argument]) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::DEBUG, "infer_function_call_type", name = name, args = ?args);
        let _enter = _span.enter();

        let func_type = self
            .env
            .get(name)
            .ok_or_else(|| VeldError::TypeError(format!("Undefined function: {}", name)))?;
        match func_type {
            Type::Function {
                params,
                return_type,
            } => {
                if params.len() != args.len() {
                    return Err(VeldError::TypeError(format!(
                        "Function {} takes {} arguments, but {} were provided",
                        name,
                        params.len(),
                        args.len()
                    )));
                }

                for (i, arg) in args.iter().enumerate() {
                    let arg_expr = match arg {
                        Argument::Positional(expr) => expr,
                        Argument::Named { name: _, value } => value,
                    };

                    let arg_type = self.infer_expression_type(arg_expr)?;
                    self.env.add_constraint(arg_type, params[i].clone());
                }
                self.env.solve_constraints()?;
                Ok(*return_type)
            }
            Type::TypeVar(_) => {
                // Handle type variables that should be constrained to function types
                tracing::debug!(
                    "Function call on type variable: {} with type {:?}",
                    name,
                    func_type
                );

                // Create parameter types for the arguments
                let mut param_types = Vec::new();
                for arg in args {
                    let arg_expr = match arg {
                        Argument::Positional(expr) => expr,
                        Argument::Named { name: _, value } => value,
                    };
                    let arg_type = self.infer_expression_type(arg_expr)?;
                    param_types.push(arg_type);
                }

                // Create a fresh return type variable
                let return_type = self.env.fresh_type_var();

                // Constrain the function type variable to be a function type
                let expected_func_type = Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type.clone()),
                };

                tracing::debug!(
                    "Constraining {} to function type: {:?}",
                    name,
                    expected_func_type
                );
                self.env.add_constraint(func_type, expected_func_type);

                Ok(return_type)
            }
            _ => Err(VeldError::TypeError(format!("{} is not a function", name))),
        }
    }

    fn infer_lambda_type(
        &mut self,
        params: &[(String, Option<TypeAnnotation>)],
        body: &Expr,
        return_type_anno: Option<&TypeAnnotation>,
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::DEBUG, "infer_lambda_type", params = ?params, body = ?body, return_type_anno = ?return_type_anno);
        let _enter = _span.enter();

        tracing::debug!(
            "Lambda type inference starting at scope depth: {}",
            self.env.scope_depth()
        );
        self.env.push_scope();
        tracing::debug!(
            "Lambda type inference after push_scope at depth: {}",
            self.env.scope_depth()
        );
        let mut param_types = Vec::new();

        for (name, type_anno) in params {
            let param_type = if let Some(anno) = type_anno {
                self.env.from_annotation(anno, None)?
            } else {
                self.env.fresh_type_var()
            };
            tracing::debug!("Lambda parameter {} type: {:?}", name, param_type);
            param_types.push(param_type.clone());
            self.env.define(name, param_type);
        }

        tracing::debug!(
            "About to type-check lambda body at scope depth: {}",
            self.env.scope_depth()
        );
        let body_type = self.infer_expression_type(body)?;
        tracing::debug!("Lambda body type before constraints: {:?}", body_type);

        let body_type = self.env.apply_substitutions(&body_type);
        tracing::debug!("Lambda body type after substitution: {:?}", body_type);

        let return_type = if let Some(anno) = return_type_anno {
            let rt = self.env.from_annotation(anno, None)?;
            tracing::debug!("Using explicit return type: {:?}", rt);
            self.env.add_constraint(body_type.clone(), rt.clone());
            rt
        } else {
            tracing::debug!("Using inferred return type: {:?}", body_type);
            body_type
        };

        self.env.pop_scope();
        self.env.solve_constraints()?;

        let final_return_type = self.env.apply_substitutions(&return_type);
        let final_param_types = param_types
            .iter()
            .map(|t| self.env.apply_substitutions(t))
            .collect::<Vec<_>>();

        tracing::debug!(
            "Final lambda type: fn({:?}) -> {:?}",
            final_param_types,
            final_return_type
        );

        Ok(Type::Function {
            params: final_param_types,
            return_type: Box::new(final_return_type),
        })
    }

    fn infer_method_call_type(
        &mut self,
        object: &Expr,
        method: &str,
        args: &[Argument],
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "infer_method_call_type", object = ?object, method = ?method, args = ?args);
        let _enter = _span.enter();

        let obj_type = self.infer_expression_type(object)?;

        // Special handling for built-in types
        match &obj_type {
            Type::Array(elem_type) => self.infer_array_method_call_type(elem_type, method, args),

            Type::Struct { name, .. } => {
                let name = name.clone();

                // Extract method type first to avoid borrow conflicts
                let method_type = {
                    if let Some(methods) = self.env.struct_methods().get(&name) {
                        methods.get(method).cloned()
                    } else {
                        None
                    }
                };

                match method_type {
                    Some(method_type) => {
                        match method_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                // Clone params and return_type to avoid borrow issues
                                let params = params.clone();
                                let return_type = *return_type;

                                // First param is self
                                if params.len() - 1 != args.len() {
                                    return Err(VeldError::TypeError(format!(
                                        "Method {} expects {} arguments, got {}",
                                        method,
                                        params.len() - 1,
                                        args.len()
                                    )));
                                }

                                // Check argument types (skip first param which is self)
                                for (i, arg) in args.iter().enumerate() {
                                    let arg_expr = match arg {
                                        Argument::Positional(expr) => expr,
                                        Argument::Named { name: _, value } => value,
                                    };

                                    let arg_type = self.infer_expression_type(arg_expr)?;
                                    self.env.add_constraint(arg_type, params[i + 1].clone());
                                }

                                self.env.solve_constraints()?;
                                Ok(return_type)
                            }
                            _ => Err(VeldError::TypeError(format!(
                                "{}.{} is not a method",
                                name, method
                            ))),
                        }
                    }
                    None => Err(VeldError::TypeError(format!(
                        "Method {} not found on {}",
                        method, name
                    ))),
                }
            }

            Type::Enum { name, .. } => {
                let name = name.clone();

                // Extract method type first to avoid borrow conflicts
                let method_type = {
                    if let Some(methods) = self.env.get_enum_methods(&name) {
                        methods.get(method).cloned()
                    } else {
                        None
                    }
                };

                match method_type {
                    Some(method_type) => {
                        match method_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                // Clone params and return_type to avoid borrow issues
                                let params = params.clone();
                                let return_type = *return_type;

                                // First param is self
                                if params.len() - 1 != args.len() {
                                    return Err(VeldError::TypeError(format!(
                                        "Method {} expects {} arguments, got {}",
                                        method,
                                        params.len() - 1,
                                        args.len()
                                    )));
                                }

                                // Check argument types (skip first param which is self)
                                for (i, arg) in args.iter().enumerate() {
                                    let arg_expr = match arg {
                                        Argument::Positional(expr) => expr,
                                        Argument::Named { name: _, value } => value,
                                    };

                                    let arg_type = self.infer_expression_type(arg_expr)?;
                                    self.env.add_constraint(arg_type, params[i + 1].clone());
                                }

                                self.env.solve_constraints()?;
                                Ok(return_type)
                            }
                            _ => Err(VeldError::TypeError(format!(
                                "{}.{} is not a method",
                                name, method
                            ))),
                        }
                    }
                    None => Err(VeldError::TypeError(format!(
                        "Method {} not found on {}",
                        method, name
                    ))),
                }
            }

            Type::Generic { base, type_args } => {
                // Handle method calls on generic types (like Vec<T>, Result<T, E>)
                let base_name = base.clone();
                let concrete_type_args = type_args.clone();

                // First try struct methods, then enum methods
                let method_type = {
                    if let Some(methods) = self.env.struct_methods().get(&base_name) {
                        methods.get(method).cloned()
                    } else if let Some(methods) = self.env.get_enum_methods(&base_name) {
                        methods.get(method).cloned()
                    } else {
                        None
                    }
                };

                match method_type {
                    Some(method_type) => {
                        match method_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                // Create type parameter substitution map
                                // For Result<T, E>, map T to concrete_type_args[0], E to concrete_type_args[1]
                                let mut substitutions = std::collections::HashMap::new();
                                if base_name == "Result" && concrete_type_args.len() >= 2 {
                                    substitutions
                                        .insert("T".to_string(), concrete_type_args[0].clone());
                                    substitutions
                                        .insert("E".to_string(), concrete_type_args[1].clone());
                                } else if base_name == "Option" && concrete_type_args.len() >= 1 {
                                    substitutions
                                        .insert("T".to_string(), concrete_type_args[0].clone());
                                }

                                // First param is self
                                if params.len() - 1 != args.len() {
                                    return Err(VeldError::TypeError(format!(
                                        "Method {} expects {} arguments, got {}",
                                        method,
                                        params.len() - 1,
                                        args.len()
                                    )));
                                }

                                // Infer method-specific type parameters by analyzing arguments
                                for (i, arg) in args.iter().enumerate() {
                                    let arg_expr = match arg {
                                        Argument::Positional(expr) => expr,
                                        Argument::Named { name: _, value } => value,
                                    };

                                    let expected_param = &params[i + 1];

                                    // For lambda expressions, provide type context to improve inference
                                    let arg_type = if let Expr::Lambda {
                                        params: lambda_params,
                                        body,
                                        ..
                                    } = arg_expr
                                    {
                                        // If we expect a function type, use it to provide parameter types for the lambda
                                        if let Type::Function {
                                            params: expected_fn_params,
                                            return_type: expected_fn_return,
                                        } = expected_param
                                        {
                                            // Substitute known type parameters in the expected function type
                                            let substituted_expected_params: Vec<Type> =
                                                expected_fn_params
                                                    .iter()
                                                    .map(|p| {
                                                        self.env.substitute_type_params(
                                                            p,
                                                            &substitutions,
                                                        )
                                                    })
                                                    .collect();
                                            let substituted_expected_return =
                                                self.env.substitute_type_params(
                                                    expected_fn_return,
                                                    &substitutions,
                                                );

                                            // Push a new scope for lambda type checking with known parameter types
                                            self.env.push_scope();
                                            let mut lambda_param_types = Vec::new();

                                            for (j, (param_name, _)) in
                                                lambda_params.iter().enumerate()
                                            {
                                                let param_type =
                                                    if j < substituted_expected_params.len() {
                                                        substituted_expected_params[j].clone()
                                                    } else {
                                                        self.env.fresh_type_var()
                                                    };
                                                lambda_param_types.push(param_type.clone());
                                                self.env.define(param_name, param_type);
                                            }

                                            // Infer the body type with proper parameter types
                                            let body_type = self.infer_expression_type(body)?;

                                            // If we expect a specific return type, try to unify
                                            if let Type::TypeParam(param_name) =
                                                &substituted_expected_return
                                            {
                                                // This is a method-level generic like U in map<U>
                                                substitutions
                                                    .insert(param_name.clone(), body_type.clone());
                                            }

                                            self.env.pop_scope();

                                            Type::Function {
                                                params: lambda_param_types,
                                                return_type: Box::new(body_type),
                                            }
                                        } else {
                                            // Fall back to regular lambda inference
                                            self.infer_expression_type(arg_expr)?
                                        }
                                    } else {
                                        self.infer_expression_type(arg_expr)?
                                    };

                                    // Substitute known type parameters in expected type and unify
                                    let substituted_expected = self
                                        .env
                                        .substitute_type_params(expected_param, &substitutions);

                                    // For function types, we already handled the unification above
                                    if !matches!(arg_expr, Expr::Lambda { .. }) {
                                        self.env.unify(arg_type, substituted_expected)?;
                                    }
                                }

                                // Substitute type parameters in return type
                                let substituted_return_type = self
                                    .env
                                    .substitute_type_params(&return_type, &substitutions);

                                Ok(substituted_return_type)
                            }
                            _ => Err(VeldError::TypeError(format!(
                                "{}.{} is not a method",
                                base_name, method
                            ))),
                        }
                    }
                    None => Err(VeldError::TypeError(format!(
                        "Method {} not found on {}",
                        method, base_name
                    ))),
                }
            }

            Type::String => self.infer_string_method_call_type(method, args),

            Type::Module(module_path) => {
                // Handle module function calls like std.option.some(69)
                // This is parsed as a method call on the module, but we treat it as a function call
                let function_path = format!("{}.{}", module_path, method);

                // For now, we'll assume it's a function and try to infer its type
                // In a more complete implementation, we'd look up the function in the module
                // For stdlib functions, we can hardcode some known types
                match function_path.as_str() {
                    "std.option.some" => {
                        // some<T>(value: T) -> Option<T>
                        if args.len() != 1 {
                            return Err(VeldError::TypeError(format!(
                                "Function some expects 1 argument, got {}",
                                args.len()
                            )));
                        }

                        let arg_expr = match &args[0] {
                            Argument::Positional(expr) => expr,
                            Argument::Named { name: _, value } => value,
                        };

                        let arg_type = self.infer_expression_type(arg_expr)?;

                        // Return Option<T> where T is the argument type
                        Ok(Type::Generic {
                            base: "Option".to_string(),
                            type_args: vec![arg_type],
                        })
                    }
                    "std.option.none" => {
                        // none<T>() -> Option<T>
                        if !args.is_empty() {
                            return Err(VeldError::TypeError(format!(
                                "Function none expects 0 arguments, got {}",
                                args.len()
                            )));
                        }

                        // Return Option<Any> since we can't infer the type parameter
                        Ok(Type::Generic {
                            base: "Option".to_string(),
                            type_args: vec![Type::Any],
                        })
                    }
                    "std.result.ok" => {
                        // ok<T, E>(value: T) -> Result<T, E>
                        if args.len() != 1 {
                            return Err(VeldError::TypeError(format!(
                                "Function ok expects 1 argument, got {}",
                                args.len()
                            )));
                        }

                        let arg_expr = match &args[0] {
                            Argument::Positional(expr) => expr,
                            Argument::Named { name: _, value } => value,
                        };

                        let arg_type = self.infer_expression_type(arg_expr)?;

                        // Return Result<T, Any> where T is the argument type
                        Ok(Type::Generic {
                            base: "Result".to_string(),
                            type_args: vec![arg_type, Type::Any],
                        })
                    }
                    "std.result.err" => {
                        // err<T, E>(error: E) -> Result<T, E>
                        if args.len() != 1 {
                            return Err(VeldError::TypeError(format!(
                                "Function err expects 1 argument, got {}",
                                args.len()
                            )));
                        }

                        let arg_expr = match &args[0] {
                            Argument::Positional(expr) => expr,
                            Argument::Named { name: _, value } => value,
                        };

                        let arg_type = self.infer_expression_type(arg_expr)?;

                        // Return Result<Any, E> where E is the argument type
                        Ok(Type::Generic {
                            base: "Result".to_string(),
                            type_args: vec![Type::Any, arg_type],
                        })
                    }
                    _ => {
                        // For unknown module functions, return Any for now
                        // In a complete implementation, we'd look up the function signature
                        Ok(Type::Any)
                    }
                }
            }

            // Handle primitive types with methods
            Type::Bool
            | Type::I32
            | Type::I64
            | Type::F32
            | Type::F64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::Char => {
                let type_name = match &obj_type {
                    Type::Bool => "bool",
                    Type::I32 => "i32",
                    Type::I64 => "i64",
                    Type::F32 => "f32",
                    Type::F64 => "f64",
                    Type::U8 => "u8",
                    Type::U16 => "u16",
                    Type::U32 => "u32",
                    Type::U64 => "u64",
                    Type::I8 => "i8",
                    Type::I16 => "i16",
                    Type::Char => "char",
                    _ => unreachable!(),
                };

                // Look up method in struct_methods (where primitive type methods are registered)
                let method_type = {
                    if let Some(methods) = self.env.struct_methods().get(type_name) {
                        methods.get(method).cloned()
                    } else {
                        None
                    }
                };

                match method_type {
                    Some(method_type) => {
                        match method_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                let params = params.clone();
                                let return_type = *return_type;

                                // First param is self
                                if params.len() - 1 != args.len() {
                                    return Err(VeldError::TypeError(format!(
                                        "Method {} expects {} arguments, got {}",
                                        method,
                                        params.len() - 1,
                                        args.len()
                                    )));
                                }

                                // Check argument types (skip first param which is self)
                                for (i, arg) in args.iter().enumerate() {
                                    let arg_expr = match arg {
                                        Argument::Positional(expr) => expr,
                                        Argument::Named { name: _, value } => value,
                                    };

                                    let arg_type = self.infer_expression_type(arg_expr)?;
                                    self.env.add_constraint(arg_type, params[i + 1].clone());
                                }

                                self.env.solve_constraints()?;
                                Ok(return_type)
                            }
                            _ => Err(VeldError::TypeError(format!(
                                "{}.{} is not a method",
                                type_name, method
                            ))),
                        }
                    }
                    None => Err(VeldError::TypeError(format!(
                        "Method {} not found on {}",
                        method, type_name
                    ))),
                }
            }

            _ => Err(VeldError::TypeError(format!(
                "Type {} does not have methods",
                obj_type
            ))),
        }
    }

    fn infer_array_method_call_type(
        &mut self,
        elem_type: &Type,
        method: &str,
        args: &[Argument],
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "infer_array_method_call_type", elem_type = ?elem_type, method = ?method, args = ?args);
        let _enter = _span.enter();

        match method {
            "len" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError("len() takes no arguments".into()));
                }
                Ok(Type::I32)
            }

            "is_empty" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError("is_empty() takes no arguments".into()));
                }
                Ok(Type::Bool)
            }

            "get" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "get() takes exactly one argument".into(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, Type::I32);
                self.env.solve_constraints()?;

                // get() returns Option<T>
                Ok(Type::Generic {
                    base: "Option".to_string(),
                    type_args: vec![elem_type.clone()],
                })
            }

            "set" => {
                if args.len() != 2 {
                    return Err(VeldError::TypeError(
                        "set() takes exactly two arguments".into(),
                    ));
                }

                let index_arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let value_arg = match &args[1] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let index_type = self.infer_expression_type(index_arg)?;
                let value_type = self.infer_expression_type(value_arg)?;

                self.env.add_constraint(index_type, Type::I32);
                self.env.add_constraint(value_type, elem_type.clone());
                self.env.solve_constraints()?;

                // set() returns bool
                Ok(Type::Bool)
            }

            "first" | "last" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError(
                        format!("{}() takes no arguments", method).into(),
                    ));
                }
                Ok(elem_type.clone())
            }

            "init" | "tail" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError(
                        format!("{}() takes no arguments", method).into(),
                    ));
                }
                Ok(Type::Array(Box::new(elem_type.clone())))
            }

            "with" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "with() takes exactly one argument".into(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, elem_type.clone());
                self.env.solve_constraints()?;

                Ok(Type::Array(Box::new(elem_type.clone())))
            }

            "take" | "drop" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        format!("{}() takes exactly one argument", method).into(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, Type::I32);
                self.env.solve_constraints()?;

                Ok(Type::Array(Box::new(elem_type.clone())))
            }

            "map" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "map() takes exactly one function argument".into(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let func_type = self.infer_expression_type(arg)?;

                match func_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        if params.len() != 1 {
                            return Err(VeldError::TypeError(
                                "map() function must take exactly one argument".into(),
                            ));
                        }

                        self.env
                            .add_constraint(params[0].clone(), elem_type.clone());
                        self.env.solve_constraints()?;

                        Ok(Type::Array(return_type.clone()))
                    }
                    _ => Err(VeldError::TypeError(
                        "map() requires a function argument".into(),
                    )),
                }
            }

            "filter" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "filter() takes exactly one function argument".into(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let func_type = self.infer_expression_type(arg)?;

                match func_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        if params.len() != 1 {
                            return Err(VeldError::TypeError(
                                "filter() function must take exactly one argument".into(),
                            ));
                        }

                        self.env
                            .add_constraint(params[0].clone(), elem_type.clone());
                        self.env.add_constraint(*return_type, Type::Bool);
                        self.env.solve_constraints()?;

                        Ok(Type::Array(Box::new(elem_type.clone())))
                    }
                    _ => Err(VeldError::TypeError(
                        "filter() requires a function argument".into(),
                    )),
                }
            }

            _ => Err(VeldError::TypeError(format!(
                "Unknown method {} on array",
                method
            ))),
        }
    }

    fn infer_string_method_call_type(&mut self, method: &str, args: &[Argument]) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "infer_string_method_call_type", method = ?method, args = ?args);
        let _enter = _span.enter();

        match method {
            "to_upper" | "to_lower" | "trim" | "trim_start" | "trim_end" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError(format!(
                        "{}() takes no arguments",
                        method
                    )));
                }
                Ok(Type::String)
            }

            "contains" | "starts_with" | "ends_with" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(format!(
                        "{}() takes exactly one argument",
                        method
                    )));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, Type::String);
                self.env.solve_constraints()?;

                Ok(Type::Bool)
            }

            "index_of" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "index_of() takes exactly one argument".to_string(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, Type::String);
                self.env.solve_constraints()?;

                Ok(Type::I32)
            }

            "substring" => {
                if args.len() != 2 {
                    return Err(VeldError::TypeError(
                        "substring() takes exactly two arguments".to_string(),
                    ));
                }

                let start_arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let end_arg = match &args[1] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let start_type = self.infer_expression_type(start_arg)?;
                let end_type = self.infer_expression_type(end_arg)?;

                self.env.add_constraint(start_type, Type::I32);
                self.env.add_constraint(end_type, Type::I32);
                self.env.solve_constraints()?;

                Ok(Type::String)
            }

            "replace" => {
                if args.len() != 2 {
                    return Err(VeldError::TypeError(
                        "replace() takes exactly two arguments".to_string(),
                    ));
                }

                let from_arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let to_arg = match &args[1] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let from_type = self.infer_expression_type(from_arg)?;
                let to_type = self.infer_expression_type(to_arg)?;

                self.env.add_constraint(from_type, Type::String);
                self.env.add_constraint(to_type, Type::String);
                self.env.solve_constraints()?;

                Ok(Type::String)
            }

            "split" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "split() takes exactly one argument".to_string(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, Type::String);
                self.env.solve_constraints()?;

                Ok(Type::Array(Box::new(Type::String)))
            }

            "pad_start" | "pad_end" => {
                if args.len() != 2 {
                    return Err(VeldError::TypeError(format!(
                        "{}() takes exactly two arguments",
                        method
                    )));
                }

                let length_arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let pad_arg = match &args[1] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let length_type = self.infer_expression_type(length_arg)?;
                let pad_type = self.infer_expression_type(pad_arg)?;

                self.env.add_constraint(length_type, Type::I32);
                self.env.add_constraint(pad_type, Type::String);
                self.env.solve_constraints()?;

                Ok(Type::String)
            }

            "repeat" => {
                if args.len() != 1 {
                    return Err(VeldError::TypeError(
                        "repeat() takes exactly one argument".to_string(),
                    ));
                }

                let arg = match &args[0] {
                    Argument::Positional(expr) => expr,
                    Argument::Named { name: _, value } => value,
                };

                let arg_type = self.infer_expression_type(arg)?;
                self.env.add_constraint(arg_type, Type::I32);
                self.env.solve_constraints()?;

                Ok(Type::String)
            }

            "to_int" | "to_float" | "to_bool" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError(format!(
                        "{}() takes no arguments",
                        method
                    )));
                }

                // Parse methods return Option<T>
                let inner_type = match method {
                    "to_int" => Type::I32,
                    "to_float" => Type::F64,
                    "to_bool" => Type::Bool,
                    _ => unreachable!(),
                };

                Ok(Type::Generic {
                    base: "Option".to_string(),
                    type_args: vec![inner_type],
                })
            }

            _ => Err(VeldError::TypeError(format!(
                "Unknown method {} on string",
                method
            ))),
        }
    }

    fn infer_property_access_type(&mut self, object: &Expr, property: &str) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "infer_property_access_type", object = ?object, property = ?property);
        let _enter = _span.enter();

        let object_type = self.infer_expression_type(object)?;
        match &object_type {
            Type::Struct { name, fields } => {
                if let Some(field_type) = fields.get(property) {
                    Ok(field_type.clone())
                } else {
                    Err(VeldError::TypeError(format!(
                        "Struct {} has no field {}",
                        name, property
                    )))
                }
            }
            Type::Record { fields } => {
                if let Some(field_type) = fields.get(property) {
                    Ok(field_type.clone())
                } else {
                    Err(VeldError::TypeError(format!(
                        "Record has no field {}",
                        property
                    )))
                }
            }
            Type::Generic { base, type_args: _ } => {
                // First check if it's a struct
                if let Some(struct_fields) = self.env.structs().get(base) {
                    if let Some(field_type) = struct_fields.get(property) {
                        Ok(field_type.clone())
                    } else {
                        Err(VeldError::TypeError(format!(
                            "Struct {} has no field {}",
                            base, property
                        )))
                    }
                }
                // Then check if it's an enum
                else if let Some(enum_variants) = self.env.enums.get(base) {
                    if let Some(variant) = enum_variants.get(property) {
                        match variant {
                            EnumVariant::Simple => {
                                // Return the generic enum type with the specific variant
                                Ok(Type::Generic {
                                    base: base.clone(),
                                    type_args: vec![],
                                })
                            }
                            EnumVariant::Tuple(types) => {
                                // For tuple variants, we need to construct the variant type
                                Ok(Type::Generic {
                                    base: base.clone(),
                                    type_args: types.clone(),
                                })
                            }
                            EnumVariant::Struct(fields) => Ok(Type::Struct {
                                name: property.to_string(),
                                fields: fields.clone(),
                            }),
                        }
                    } else {
                        Err(VeldError::TypeError(format!(
                            "Enum {} has no variant {}",
                            base, property
                        )))
                    }
                } else {
                    Err(VeldError::TypeError(format!("Unknown type: {}", base)))
                }
            }
            Type::Enum { name, variants } => {
                if let Some(variant) = variants.get(property) {
                    match variant {
                        EnumVariant::Simple => Ok(Type::Unit),
                        EnumVariant::Tuple(types) => Ok(Type::Tuple(types.clone())),
                        EnumVariant::Struct(fields) => Ok(Type::Struct {
                            name: property.to_string(),
                            fields: fields.clone(),
                        }),
                    }
                } else {
                    Err(VeldError::TypeError(format!(
                        "Enum {} has no variant {}",
                        name, property
                    )))
                }
            }
            Type::Array(_) => match property {
                "len" => Ok(Type::I32),
                _ => Err(VeldError::TypeError(format!(
                    "Cannot access property {} of array",
                    property
                ))),
            },
            Type::Module(module_name) => {
                // For module access like std.option, return another module type
                // This allows chaining like std.option.some
                Ok(Type::Module(format!("{}.{}", module_name, property)))
            }
            _ => Err(VeldError::TypeError(format!(
                "Cannot access property {} of non-struct type {}",
                property, object_type
            ))),
        }
    }

    fn infer_struct_create_type(
        &mut self,
        struct_name: &str,
        fields: &[(String, Expr)],
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::DEBUG, "infer_struct_create_type", struct_name = ?struct_name, fields = ?fields);
        let _enter = _span.enter();

        // First collect struct fields information to avoid borrow checker issues
        let struct_fields_data = if let Some(struct_fields) = self.env.structs().get(struct_name) {
            // Clone the fields map to avoid keeping the borrow
            struct_fields.clone()
        } else {
            return Err(VeldError::TypeError(format!(
                "Undefined struct: {}",
                struct_name
            )));
        };

        // Check if all required fields are provided
        for (name, _) in &struct_fields_data {
            if !fields.iter().any(|(field_name, _)| field_name == name) {
                return Err(VeldError::TypeError(format!(
                    "Missing field {} in struct {}",
                    name, struct_name
                )));
            }
        }

        // Check field types
        for (name, expr) in fields {
            if let Some(field_type) = struct_fields_data.get(name) {
                let expr_type = self.infer_expression_type(expr)?;
                self.env.add_constraint(expr_type, field_type.clone());
            } else {
                return Err(VeldError::TypeError(format!(
                    "Unknown field {} in struct {}",
                    name, struct_name
                )));
            }
        }

        self.env.solve_constraints()?;

        Ok(Type::Struct {
            name: struct_name.to_string(),
            fields: struct_fields_data,
        })
    }

    fn infer_array_literal_type(&mut self, elements: &[Expr]) -> Result<Type> {
        let _span =
            tracing::span!(tracing::Level::DEBUG, "infer_array_literal_type", elements = ?elements);
        let _enter = _span.enter();

        if elements.is_empty() {
            // Empty array - element type is a type variable to be inferred later
            let elem_type = self.env.fresh_type_var();
            return Ok(Type::Array(Box::new(elem_type)));
        }

        // Infer type of first element
        let first_type = self.infer_expression_type(&elements[0])?;
        tracing::debug!("Array first element type: {:?}", first_type);

        // Check all elements have the same type - be more strict
        for (i, elem) in elements.iter().skip(1).enumerate() {
            let elem_type = self.infer_expression_type(elem)?;
            tracing::debug!("Array element {} type: {:?}", i + 1, elem_type);

            // Add strict constraint that elements must match exactly
            if !self.types_compatible(&elem_type, &first_type) {
                return Err(VeldError::TypeError(format!(
                    "Array elements must have the same type. First element has type {}, but element {} has type {}",
                    first_type,
                    i + 1,
                    elem_type
                )));
            }

            self.env.add_constraint(elem_type, first_type.clone());
        }

        self.env.solve_constraints()?;

        let elem_type = self.env.apply_substitutions(&first_type);
        tracing::debug!("Final array element type: {:?}", elem_type);
        Ok(Type::Array(Box::new(elem_type)))
    }

    fn infer_enum_variant_type(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        fields: &[Expr],
        type_args: Option<&Vec<TypeAnnotation>>,
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::INFO, "infer_enum_variant_type", enum_name = enum_name, variant_name = variant_name, fields = ?fields);
        let _enter = _span.enter();

        // Check if the enum exists
        if !self.env.enums.contains_key(enum_name) {
            // Fallback: Check if this is actually a struct method call
            if self.env.structs().contains_key(enum_name) {
                // This is a struct method call, not an enum variant
                // The variant_name is actually the method name
                // For struct method calls like Vec.new(), we need to infer the return type
                // For now, assume it returns an instance of the struct type
                return Ok(Type::Generic {
                    base: enum_name.to_string(),
                    type_args: vec![], // TODO: Handle generic type args properly
                });
            }
            return Err(VeldError::TypeError(format!("Unknown enum: {}", enum_name)));
        }

        // For now, if type arguments are provided, create a generic type
        if let Some(args) = type_args {
            let type_arg_types = args
                .iter()
                .map(|arg| self.env.from_annotation(arg, None))
                .collect::<Result<Vec<_>>>()?;

            return Ok(Type::Generic {
                base: enum_name.to_string(),
                type_args: type_arg_types,
            });
        }

        // Try to infer type arguments from field values for generic enums
        // Clone the variant data to avoid borrow checker issues
        let variant_data = if let Some(enum_variants) = self.env.enums.get(enum_name) {
            enum_variants.get(variant_name).cloned()
        } else {
            None
        };

        if let Some(variant) = variant_data {
            match variant {
                EnumVariant::Tuple(expected_types) => {
                    if fields.len() != expected_types.len() {
                        return Err(VeldError::TypeError(format!(
                            "Variant {} expects {} fields, got {}",
                            variant_name,
                            expected_types.len(),
                            fields.len()
                        )));
                    }

                    // Infer field types and create substitution map for type parameters
                    let mut type_args = Vec::new();
                    let mut substitutions = std::collections::HashMap::new();

                    for (i, field) in fields.iter().enumerate() {
                        let field_type = self.infer_expression_type(field)?;
                        let expected_type = &expected_types[i];

                        // If expected type is a type parameter, map it to the inferred type
                        if let Type::TypeParam(param_name) = expected_type {
                            substitutions.insert(param_name.clone(), field_type.clone());
                        }
                    }

                    // For common generic enums like Result<T, E> and Option<T>,
                    // create appropriate type arguments
                    match enum_name {
                        "Result" => {
                            // Result<T, E> where Ok(T) and Err(E)
                            match variant_name {
                                "Ok" => {
                                    // Ok variant contains T, so T = field type, E = fresh type var
                                    if !fields.is_empty() {
                                        type_args.push(self.infer_expression_type(&fields[0])?);
                                    } else {
                                        type_args.push(self.env.fresh_type_var());
                                    }
                                    type_args.push(self.env.fresh_type_var()); // E is unbound
                                }
                                "Err" => {
                                    // Err variant contains E, so T = fresh type var, E = field type
                                    type_args.push(self.env.fresh_type_var()); // T is unbound
                                    if !fields.is_empty() {
                                        type_args.push(self.infer_expression_type(&fields[0])?);
                                    } else {
                                        type_args.push(self.env.fresh_type_var());
                                    }
                                }
                                _ => {
                                    // Unknown variant, use fresh type vars for both
                                    type_args.push(self.env.fresh_type_var());
                                    type_args.push(self.env.fresh_type_var());
                                }
                            }
                        }
                        "Option" => {
                            // Option<T> - type arg is from the field
                            if let Some(t_type) = substitutions.get("T") {
                                type_args.push(t_type.clone());
                            } else if !fields.is_empty() {
                                type_args.push(self.infer_expression_type(&fields[0])?);
                            } else {
                                type_args.push(self.env.fresh_type_var());
                            }
                        }
                        _ => {
                            // For other generic enums, just use the inferred field types
                            for field in fields {
                                type_args.push(self.infer_expression_type(field)?);
                            }
                        }
                    }

                    return Ok(Type::Generic {
                        base: enum_name.to_string(),
                        type_args,
                    });
                }
                EnumVariant::Simple => {
                    // Simple variant with no fields - return generic type with fresh type vars
                    match enum_name {
                        "Result" => Ok(Type::Generic {
                            base: enum_name.to_string(),
                            type_args: vec![self.env.fresh_type_var(), self.env.fresh_type_var()],
                        }),
                        "Option" => Ok(Type::Generic {
                            base: enum_name.to_string(),
                            type_args: vec![self.env.fresh_type_var()],
                        }),
                        _ => {
                            let enum_variants = self.env.enums.get(enum_name).unwrap().clone();
                            Ok(Type::Enum {
                                name: enum_name.to_string(),
                                variants: enum_variants,
                            })
                        }
                    }
                }
                EnumVariant::Struct(_) => {
                    // Struct variant - for now return generic type
                    Ok(Type::Generic {
                        base: enum_name.to_string(),
                        type_args: vec![],
                    })
                }
            }
        } else {
            Err(VeldError::TypeError(format!(
                "Variant {} not found in enum {}",
                variant_name, enum_name
            )))
        }
    }

    fn check_generic_arg_compatability(
        &mut self,
        generic_args: &[GenericArgument],
        param_name: &str,
        actual_type: &Type,
    ) -> bool {
        let _span = tracing::span!(tracing::Level::TRACE, "check_generic_arg_compatability", param_name = param_name, actual_type = ?actual_type);
        let _enter = _span.enter();

        for arg in generic_args {
            if let Some(name) = &arg.name {
                if name == param_name {
                    // Named parameter
                    let arg_type = self
                        .env
                        .from_annotation(&arg.type_annotation, None)
                        .unwrap();
                    return self.types_compatible(&arg_type, actual_type);
                }
            } else {
                // Positional parameter
                let arg_type = self
                    .env
                    .from_annotation(&arg.type_annotation, None)
                    .unwrap();
                return self.types_compatible(&arg_type, actual_type);
            }
        }
        false
    }

    fn infer_pipe_type(&mut self, left: &Expr, right: &Expr) -> Result<Type> {
        let left_type = self.infer_expression_type(left)?;

        match right {
            // Case 1: Function call - left |> func(args)
            Expr::FunctionCall { name, arguments } => {
                // For function calls, we create a new function call with left as first arg
                let left_expr = left.clone();
                let mut new_args = vec![Argument::Positional(left_expr)];
                for arg in arguments {
                    new_args.push(arg.clone());
                }

                // Infer the type of the function
                let func_type = self
                    .env
                    .get(name)
                    .ok_or_else(|| VeldError::TypeError(format!("Undefined function: {}", name)))?;

                match func_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        // Check that the function takes enough arguments
                        if params.len() < new_args.len() {
                            return Err(VeldError::TypeError(format!(
                                "Function {} takes {} arguments, but {} were provided",
                                name,
                                params.len(),
                                new_args.len()
                            )));
                        }

                        // Check argument types
                        for (i, arg) in new_args.iter().enumerate() {
                            let arg_expr = match arg {
                                Argument::Positional(expr) => expr,
                                Argument::Named { name: _, value } => value,
                            };

                            let arg_type = self.infer_expression_type(arg_expr)?;
                            self.env.add_constraint(arg_type, params[i].clone());
                        }

                        self.env.solve_constraints()?;
                        Ok(*return_type.clone())
                    }
                    _ => Err(VeldError::TypeError(format!("{} is not a function", name))),
                }
            }

            // Case 1b: General call - left |> func(args) where func is an expression
            Expr::Call { callee, arguments } => {
                // For Call expressions, we need to handle the case where callee is an Identifier
                match &**callee {
                    Expr::Identifier(name) => {
                        // This is equivalent to FunctionCall case
                        let left_expr = left.clone();
                        let mut new_args = vec![Argument::Positional(left_expr)];
                        for arg in arguments {
                            new_args.push(arg.clone());
                        }

                        // Infer the type of the function
                        let func_type = self.env.get(name).ok_or_else(|| {
                            VeldError::TypeError(format!("Undefined function: {}", name))
                        })?;

                        match func_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                // Check that the function takes enough arguments
                                if params.len() < new_args.len() {
                                    return Err(VeldError::TypeError(format!(
                                        "Function {} takes {} arguments, but {} were provided",
                                        name,
                                        params.len(),
                                        new_args.len()
                                    )));
                                }

                                // Check argument types
                                for (i, arg) in new_args.iter().enumerate() {
                                    let arg_expr = match arg {
                                        Argument::Positional(expr) => expr,
                                        Argument::Named { name: _, value } => value,
                                    };

                                    let arg_type = self.infer_expression_type(arg_expr)?;
                                    self.env.add_constraint(arg_type, params[i].clone());
                                }

                                self.env.solve_constraints()?;
                                Ok(*return_type.clone())
                            }
                            _ => Err(VeldError::TypeError(format!("{} is not a function", name))),
                        }
                    }
                    _ => {
                        // For more complex callees, fall back to the general case
                        let callee_type = self.infer_expression_type(callee)?;
                        match &callee_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                // Check that we have the right number of arguments (including the piped value)
                                let total_args = arguments.len() + 1; // +1 for the piped value
                                if params.len() != total_args {
                                    return Err(VeldError::TypeError(format!(
                                        "Function takes {} arguments, but {} were provided (including piped value)",
                                        params.len(),
                                        total_args
                                    )));
                                }

                                // Check that the left type is compatible with the first parameter
                                let first_param_type = &params[0];
                                self.env
                                    .add_constraint(left_type.clone(), first_param_type.clone());

                                // Check the remaining argument types
                                for (i, arg) in arguments.iter().enumerate() {
                                    let arg_expr = match arg {
                                        Argument::Positional(expr) => expr,
                                        Argument::Named { name: _, value } => value,
                                    };

                                    let arg_type = self.infer_expression_type(arg_expr)?;
                                    self.env.add_constraint(arg_type, params[i + 1].clone());
                                }

                                self.env.solve_constraints()?;
                                Ok(*return_type.clone())
                            }
                            _ => Err(VeldError::TypeError(
                                "Cannot pipe into non-function expression".to_string(),
                            )),
                        }
                    }
                }
            }

            // Case 2: Method call - left |> obj.method(args)
            Expr::MethodCall {
                object,
                method,
                arguments,
            } => {
                // For method calls, we need to create a new arguments list
                let left_expr = left.clone();
                let mut new_args = vec![Argument::Positional(left_expr)];
                for arg in arguments {
                    new_args.push(arg.clone());
                }

                // Use infer_method_call_type directly with the modified arguments
                self.infer_method_call_type(object, method, &new_args)
            }

            // Case 3: Simple identifier - left |> func
            Expr::Identifier(name) => {
                // For a simple function name, check that it can accept the left type
                let func_type = self
                    .env
                    .get(name)
                    .ok_or_else(|| VeldError::TypeError(format!("Undefined function: {}", name)))?;

                match func_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        // Check that the function takes at least one argument
                        if params.is_empty() {
                            return Err(VeldError::TypeError(format!(
                                "Function {} takes no arguments",
                                name
                            )));
                        }

                        // Check that the left type is compatible with the first parameter
                        let first_param_type = &params[0];
                        self.env
                            .add_constraint(left_type.clone(), first_param_type.clone());
                        self.env.solve_constraints()?;

                        Ok(*return_type.clone())
                    }
                    _ => Err(VeldError::TypeError(format!("{} is not a function", name))),
                }
            }

            // Case 4: Other expressions
            _ => {
                // For other expressions, check that right_type is a function that can accept left_type
                let right_type = self.infer_expression_type(right)?;
                match &right_type {
                    Type::Function {
                        params,
                        return_type,
                    } => {
                        if params.is_empty() {
                            return Err(VeldError::TypeError(
                                "Cannot pipe into a function that takes no arguments".to_string(),
                            ));
                        }

                        let first_param_type = &params[0];
                        self.env
                            .add_constraint(left_type.clone(), first_param_type.clone());
                        self.env.solve_constraints()?;

                        Ok(*return_type.clone())
                    }
                    _ => Err(VeldError::TypeError(format!(
                        "Cannot pipe into non-function value of type {}",
                        right_type
                    ))),
                }
            }
        }
    }

    fn try_resolve_operator(
        &mut self,
        left: &Type,
        op: &BinaryOperator,
        right: &Type,
    ) -> Result<Option<Type>> {
        let _span = tracing::span!(tracing::Level::TRACE, "try_resolve_operator", left = ?left, op = ?op, right = ?right);
        let _enter = _span.enter();

        let trait_name = match op {
            BinaryOperator::Add => "Add",
            BinaryOperator::Subtract => "Sub",
            BinaryOperator::Multiply => "Mul",
            BinaryOperator::Divide => "Div",
            BinaryOperator::Modulo => "Rem",
            BinaryOperator::Exponent => "Pow",
            // TODO: Add more operators
            _ => return Ok(None),
        };

        if let Type::Struct {
            name: left_name, ..
        } = left
        {
            if let Some(impls) = self.env.get_implementations_for_type(left_name) {
                for impl_info in impls.clone() {
                    if impl_info.kind_name() == trait_name {
                        if self.check_generic_arg_compatability(
                            &impl_info.generic_args(),
                            "Rhs",
                            right,
                        ) {
                            return Ok(Some(
                                self.resolve_output_type(left, &impl_info.generic_args())?,
                            ));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    fn infer_index_access_type(&mut self, object: &Expr, index: &Expr) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::TRACE, "infer_index_access_type", object = ?object, index = ?index);
        let _enter = _span.enter();

        let obj_type = self.infer_expression_type(object)?;
        let idx_type = self.infer_expression_type(index)?;

        // Index should be integer
        self.env.add_constraint(idx_type, Type::I32);

        match &obj_type {
            Type::Array(elem_type) => Ok(*elem_type.clone()),

            Type::String => {
                // Indexing into string gives single character string
                Ok(Type::String)
            }

            _ => Err(VeldError::TypeError(format!(
                "Cannot index into type: {}",
                obj_type
            ))),
        }
    }

    pub fn get_implementations_for_type(
        &self,
        type_name: &str,
    ) -> Option<&Vec<ImplementationInfo>> {
        self.env.get_implementations_for_type(type_name)
    }

    pub fn find_implementation(
        &self,
        type_name: &str,
        kind_name: &str,
    ) -> Option<&ImplementationInfo> {
        self.env.find_implementation(type_name, kind_name)
    }

    pub fn find_implementation_method(
        &self,
        type_name: &str,
        kind_name: &str,
        method_name: &str,
    ) -> Option<&Type> {
        self.env
            .find_implementation_method(type_name, kind_name, method_name)
    }

    fn resolve_output_type(
        &mut self,
        impl_type: &Type,
        generic_args: &[GenericArgument],
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::TRACE, "resolve_output_type", impl_type = ?impl_type, generic_args = ?generic_args);
        let _enter = _span.enter();

        for arg in generic_args {
            if let Some(name) = &arg.name {
                if name == "Output" {
                    let output_type = self.env.from_annotation(&arg.type_annotation, None)?;

                    if let Type::TypeParam(param) = &output_type {
                        if param == "Self" {
                            return Ok(impl_type.clone());
                        }
                    }
                    return Ok(output_type);
                }
            }
        }
        Ok(impl_type.clone())
    }

    fn type_check_kind_declaration(&mut self, stmt: &Statement) -> Result<()> {
        let _span =
            tracing::span!(tracing::Level::DEBUG, "type_check_kind_declaration", stmt = ?stmt);
        let _enter = _span.enter();

        if let Statement::KindDeclaration {
            name,
            methods,
            generic_params,
            ..
        } = stmt
        {
            self.env.push_type_param_scope();

            let mut generic_param_names = Vec::new();
            for param in generic_params {
                let param_name = match &param.name {
                    Some(name) => name.clone(),
                    None => {
                        if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                            base_name.clone()
                        } else {
                            // Default to placeholder type if type annotation isnt a basic indentifier
                            "T".to_string()
                        }
                    }
                };

                generic_param_names.push(param_name.clone());
                self.env.add_type_param(&param_name);
            }

            let mut method_types = HashMap::new();
            let mut default_impls = HashMap::new();

            for method in methods {
                let mut param_types = Vec::new();

                for (_, type_anno) in &method.params {
                    let param_type = self.env.from_annotation(type_anno, Some(name))?;
                    param_types.push(param_type);
                }

                let return_type = self.env.from_annotation(&method.return_type, Some(name))?;

                let method_type = Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                };

                method_types.insert(method.name.clone(), method_type);

                if let Some(default_body) = &method.default_impl {
                    default_impls.insert(method.name.clone(), default_body.clone());
                }
            }
            self.env
                .add_kind(name, method_types, default_impls, generic_param_names);

            self.env.pop_type_param_scope();
        }
        Ok(())
    }

    fn type_check_implementation(&mut self, stmt: &Statement) -> Result<()> {
        let _span =
            tracing::span!(tracing::Level::DEBUG, "type_check_implementation", stmt = ?stmt);
        let _enter = _span.enter();

        if let Statement::Implementation {
            type_name,
            kind_name,
            methods,
            generic_args,
        } = stmt
        {
            if let Some(kind_name) = kind_name {
                let kind = match self.env.kinds().get(kind_name) {
                    Some(k) => k.clone(),
                    None => {
                        return Err(VeldError::TypeError(format!(
                            "Cannot implement unknown kind: {}",
                            kind_name
                        )));
                    }
                };

                let mut impl_method_types = HashMap::new();
                for method in methods {
                    let method_type = self.method_to_type(method)?;
                    impl_method_types.insert(method.name.clone(), method_type);
                }

                for (method_name, required_type) in &kind.methods {
                    if let Some(impl_type) = impl_method_types.get(method_name) {
                        self.env
                            .add_constraint(impl_type.clone(), required_type.clone());

                        todo!("Implement handling to substitute 'Self' with actual type");
                    } else if !kind.default_impls.contains_key(method_name) {
                        return Err(VeldError::TypeError(format!(
                            "Kind '{}' requires method '{}' which is not implemented",
                            kind_name, method_name
                        )));
                    }
                }

                self.env.solve_constraints()?;

                let processed_methods = methods
                    .iter()
                    .map(|m| (m.name.clone(), self.method_to_type(m).unwrap()))
                    .collect();

                self.env.add_implementation(
                    type_name,
                    kind_name,
                    generic_args.clone(),
                    processed_methods,
                );
            } else {
                for method in methods {
                    let method_type = self.method_to_type(method)?;
                    let struct_methods = self
                        .env
                        .struct_methods_mut()
                        .entry(type_name.clone())
                        .or_insert_with(HashMap::new);

                    struct_methods.insert(method.name.clone(), method_type);
                }
            }
        }
        Ok(())
    }

    fn infer_range_type(
        &mut self,
        start: Option<&Expr>,
        end: Option<&Expr>,
        inclusive: bool,
    ) -> Result<Type> {
        let _span = tracing::span!(tracing::Level::DEBUG, "infer_range_type", start = ?start, end = ?end, inclusive = ?inclusive);
        let _enter = _span.enter();

        // Infer the element type from start or end expression
        let element_type = if let Some(start_expr) = start {
            self.infer_expression_type(start_expr)?
        } else if let Some(end_expr) = end {
            self.infer_expression_type(end_expr)?
        } else {
            // Full range (..) has no type constraints
            return Ok(Type::Struct {
                name: "RangeFull".to_string(),
                fields: std::collections::HashMap::new(),
            });
        };

        // If both start and end are present, ensure they have the same type
        if let (Some(start_expr), Some(end_expr)) = (start, end) {
            let start_type = self.infer_expression_type(start_expr)?;
            let end_type = self.infer_expression_type(end_expr)?;
            self.env.add_constraint(start_type, end_type);
        }

        // Return the appropriate range type based on the pattern
        match (start.is_some(), end.is_some(), inclusive) {
            // start..end
            (true, true, false) => Ok(Type::Generic {
                base: "Range".to_string(),
                type_args: vec![element_type],
            }),
            // start..=end
            (true, true, true) => Ok(Type::Generic {
                base: "RangeInclusive".to_string(),
                type_args: vec![element_type],
            }),
            // start..
            (true, false, false) => Ok(Type::Generic {
                base: "RangeFrom".to_string(),
                type_args: vec![element_type],
            }),
            // ..end
            (false, true, false) => Ok(Type::Generic {
                base: "RangeTo".to_string(),
                type_args: vec![element_type],
            }),
            // ..=end
            (false, true, true) => Ok(Type::Generic {
                base: "RangeToInclusive".to_string(),
                type_args: vec![element_type],
            }),
            // .. (full range)
            (false, false, false) => Ok(Type::Struct {
                name: "RangeFull".to_string(),
                fields: std::collections::HashMap::new(),
            }),
            // start..= (invalid)
            (true, false, true) => Err(VeldError::TypeError(
                "Invalid range: start..= requires an end value".to_string(),
            )),
            // ..= (invalid)
            (false, false, true) => Err(VeldError::TypeError(
                "Invalid range: ..= requires an end value".to_string(),
            )),
        }
    }
}
