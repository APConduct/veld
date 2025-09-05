use super::super::ast::{
    Argument, BinaryOperator, Expr, GenericArgument, Literal, MethodImpl, Statement, StructMethod,
    TypeAnnotation, UnaryOperator, VarKind,
};
use super::super::error::{Result, VeldError};
use super::super::types::{
    ImplementationInfo, Type, Type::TypeVar, TypeEnvironment, base::EnumVariant,
};
use super::base::VarInfo;
use crate::interpreter::Value;
use std::collections::HashMap;

pub struct TypeChecker {
    env: TypeEnvironment,
    current_function_return_type: Option<Type>,
    var_info: HashMap<String, VarInfo>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
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
                _ => {} // TODO: Handle enum declarations
            }
        }
        // Fix: type_check_statement expects &mut Statement, so we need to create mutable clones
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
        if !is_public {
            return Err(format!(
                "Method '{}' of type '{}' is private",
                method_name, type_name
            ));
        }
        Ok(())
    }

    pub fn type_check_statement(&mut self, stmt: &mut Statement) -> Result<()> {
        match stmt {
            Statement::KindDeclaration { .. } => self.type_check_kind_declaration(stmt),
            Statement::Implementation { .. } => self.type_check_implementation(stmt),
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
        _name: &str,
        _params: &[(String, TypeAnnotation)],
        return_type: &TypeAnnotation,
        body: &[Statement],
        generic_params: &[GenericArgument],
    ) -> Result<()> {
        // Save the current type environment
        let saved_env = self.env.clone();

        // Add type parameters to the environment
        self.env.push_type_param_scope();
        for param in generic_params {
            match &param.type_annotation {
                TypeAnnotation::Basic(name) => {
                    self.env.add_type_param(name.as_str());
                }
                _ => return Err(VeldError::TypeError("Invalid type parameter".into())),
            }
        }

        // Type check the function body with generic parameters
        let old_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(self.env.from_annotation(return_type, None)?);

        // Type check each statement in the function body
        for stmt in body {
            let mut stmt_mut = stmt.clone();
            self.type_check_statement(&mut stmt_mut)?;
        }

        // Restore the original environment
        self.current_function_return_type = old_return_type;
        self.env.pop_type_param_scope();
        self.env = saved_env;

        Ok(())
    }

    pub fn infer_expression_type_with_env(
        &self,
        expr: &Expr,
        env: &mut TypeEnvironment,
    ) -> Result<Type> {
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
        let param_types = params
            .iter()
            .map(|(_, type_anno)| self.env.from_annotation(type_anno, None))
            .collect::<Result<Vec<_>>>()?;

        self.env.push_scope();

        for (i, (param_name, _)) in params.iter().enumerate() {
            self.env.define(param_name, param_types[i].clone());
        }

        let inferred_return_type = if !body.is_empty() {
            let mut found_type = None;
            for stmt in body {
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

        let function_type = Type::Function {
            params: param_types.clone(),
            return_type: Box::new(actual_return_type),
        };
        self.env.define(name, function_type);
        self.env.pop_scope();
        Ok(())
    }

    pub fn check_assignment(&self, name: &str, _value: &Expr) -> Result<()> {
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

        let value_type = self.infer_expression_type(value)?;
        tracing::debug!("Variable '{}' value type: {:?}", name, value_type);

        let const_value = if matches!(var_kind, VarKind::Const) {
            Some(self.evaluate_const_expr(value)?)
        } else {
            None
        };

        let var_type = if let Some(anno) = type_annotation {
            let specified_type = self.env.from_annotation(anno, None)?;
            tracing::debug!("Variable '{}' specified type: {:?}", name, specified_type);

            // For arrays, be extra strict about type checking
            if let (Type::Array(expected_elem), Type::Array(actual_elem)) =
                (&specified_type, &value_type)
            {
                if !self.types_compatible(actual_elem, expected_elem) {
                    return Err(VeldError::TypeError(format!(
                        "Type mismatch for variable '{}': expected array of {}, got array of {}",
                        name, expected_elem, actual_elem
                    )));
                }
            } else if !self.types_compatible(&value_type, &specified_type) {
                return Err(VeldError::TypeError(format!(
                    "Type mismatch for variable '{}': expected {}, got {}",
                    name, specified_type, value_type
                )));
            }

            self.env
                .add_constraint(value_type.clone(), specified_type.clone());
            specified_type
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

        self.env.define(name, final_type);
        Ok(())
    }

    fn type_check_if(
        &mut self,
        condition: &Expr,
        then_branch: &[Statement],
        else_branch: &Option<Vec<Statement>>,
    ) -> Result<()> {
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
            Expr::Record { fields: _ } => todo!("Record type inference"),
        };
        if let Ok(ref t) = result {
            tracing::debug!("Final inferred type for expression: {:?} -> {:?}", expr, t);
        }
        result
    }

    fn is_valid_cast(&self, from: &Type, to: &Type) -> bool {
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

            // Same type is valid
            _ if from == to => true,

            // All other casts are invalid for now
            _ => false,
        }
    }

    fn type_check_cast(&mut self, expr: &Expr, target_type: &TypeAnnotation) -> Result<Type> {
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
        match lit {
            Literal::Integer(value) => {
                // Infer the smallest type that can hold a value
                let inferred_type = if *value >= i8::MIN as i64 && *value <= i8::MAX as i64 {
                    Type::I8
                } else if *value >= i16::MIN as i64 && *value <= i16::MAX as i64 {
                    Type::I16
                } else if *value >= i32::MIN as i64 && *value <= i32::MAX as i64 {
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
        matches!(ty, Type::I64 | Type::U64 | Type::F64)
    }

    fn is_integer_type(&self, ty: &Type) -> bool {
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
                // For the pipeline operator: left |> right

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
                        let func_type = self.env.get(name).ok_or_else(|| {
                            VeldError::TypeError(format!("Undefined function: {}", name))
                        })?;

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
                        match &right_type {
                            Type::Function {
                                params,
                                return_type,
                            } => {
                                if params.is_empty() {
                                    return Err(VeldError::TypeError(
                                        "Cannot pipe into a function that takes no arguments"
                                            .to_string(),
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
        }
    }

    fn is_numeric_type(&self, ty: &Type) -> bool {
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
        )
    }

    fn types_compatible(&self, t1: &Type, t2: &Type) -> bool {
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
                base1 == base2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(a1, a2)| self.types_compatible(a1, a2))
            }

            // For structs, check names match
            (Type::Struct { name: name1, .. }, Type::Struct { name: name2, .. }) => name1 == name2,

            // For enums, check names match
            (Type::Enum { name: name1, .. }, Type::Enum { name: name2, .. }) => name1 == name2,

            _ if self.is_numeric_type(&t1) && self.is_numeric_type(&t2) => true,
            _ => false,
        }
    }

    pub fn validate_value(&self, value: &Value, expected_type: &Type) -> Result<()> {
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
            _ => Err(VeldError::TypeError(format!("{} is not a function", name))),
        }
    }

    fn infer_lambda_type(
        &mut self,
        params: &[(String, Option<TypeAnnotation>)],
        body: &Expr,
        return_type_anno: Option<&TypeAnnotation>,
    ) -> Result<Type> {
        self.env.push_scope();
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

            Type::Generic { base, .. } => {
                // Handle method calls on generic types (like Result<T, E>)
                let base_name = base.clone();

                // Extract method type first to avoid borrow conflicts
                let method_type = {
                    if let Some(methods) = self.env.get_enum_methods(&base_name) {
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
        match method {
            "len" => {
                if !args.is_empty() {
                    return Err(VeldError::TypeError("len() takes no arguments".into()));
                }
                Ok(Type::I32)
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

    fn infer_property_access_type(&mut self, object: &Expr, property: &str) -> Result<Type> {
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
            Type::Generic { base, type_args: _ } => {
                if let Some(struct_fields) = self.env.structs().get(base) {
                    if let Some(field_type) = struct_fields.get(property) {
                        Ok(field_type.clone())
                    } else {
                        Err(VeldError::TypeError(format!(
                            "Struct {} has no field {}",
                            base, property
                        )))
                    }
                } else {
                    Err(VeldError::TypeError(format!("Unknown struct: {}", base)))
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
        // Check if the enum exists
        if !self.env.enums.contains_key(enum_name) {
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

        // For simple enums without type arguments, return the enum type
        Ok(Type::Enum {
            name: enum_name.to_string(),
            variants: self.env.enums.get(enum_name).unwrap().clone(),
        })
    }

    fn check_generic_arg_compatability(
        &mut self,
        generic_args: &[GenericArgument],
        param_name: &str,
        actual_type: &Type,
    ) -> bool {
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

    fn try_resolve_operator(
        &mut self,
        left: &Type,
        op: &BinaryOperator,
        right: &Type,
    ) -> Result<Option<Type>> {
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
}
