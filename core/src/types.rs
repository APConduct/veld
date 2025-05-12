use crate::ast::{Argument, BinaryOperator, Expr, Literal, Statement, TypeAnnotation};
use crate::error::{Result, VeldError};
use crate::types::Type::TypeVar;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;

pub const U8_MAX: u8 = 255;
pub const U16_MAX: u16 = 65535;
pub const U32_MAX: u32 = 4294967295;
pub const U64_MAX: u64 = 18446744073709551615;
pub const I8_MAX: i8 = 127;
pub const I16_MAX: i16 = 32767;
pub const I32_MAX: i32 = 2147483647;
pub const I64_MAX: i64 = 9223372036854775807;
pub const F32_MAX: f32 = 3.40282347e+38;
pub const F64_MAX: f64 = 1.7976931348623157e+308;
pub const I8_MIN: i8 = -128;
pub const I16_MIN: i16 = -32768;
pub const I32_MIN: i32 = -2147483648;
pub const I64_MIN: i64 = -9223372036854775808;
pub const F32_MIN: f32 = -3.40282347e+38;
pub const F64_MIN: f64 = -1.7976931348623157e+308;
pub const U8_MIN: u8 = 0;
pub const U16_MIN: u16 = 0;
pub const U32_MIN: u32 = 0;
pub const U64_MIN: u64 = 0;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    I32,
    F64,
    Bool,
    String,

    Char,

    I64,
    F32,

    U32,
    U64,
    U8,
    U16,

    I8,
    I16,

    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },

    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },

    Generic {
        base: String,
        type_args: Vec<Type>,
    },

    TypeParam(String),

    Enum {
        name: String,
        variants: HashMap<String, EnumVariant>,
    },

    Tuple(Vec<Type>),

    Array(Box<Type>),

    Any,

    TypeVar(usize),
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unit, Type::Unit) => true,
            (Type::I32, Type::I32) => true,
            (Type::F64, Type::F64) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Char, Type::Char) => true,
            (Type::I64, Type::I64) => true,
            (Type::F32, Type::F32) => true,
            (Type::U32, Type::U32) => true,
            (Type::U64, Type::U64) => true,
            (Type::U8, Type::U8) => true,
            (Type::U16, Type::U16) => true,
            (Type::I8, Type::I8) => true,
            (Type::I16, Type::I16) => true,

            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Simple,
    Tuple(Vec<Type>),
    Struct(HashMap<String, Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::I32 => write!(f, "i32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "str"),
            Type::Function {
                params,
                return_type,
            } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Generic { base, type_args } => {
                write!(f, "{}<", base)?;
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            Type::TypeParam(name) => write!(f, "{}", name),
            Type::Enum { name, .. } => write!(f, "{}", name),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Array(inner) => write!(f, "[{}]", inner),
            Type::Any => write!(f, "any"),
            Type::TypeVar(id) => write!(f, "T{}", id),
            Type::Char => write!(f, "char"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
        }
    }
}

pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, Type>>,

    structs: HashMap<String, HashMap<String, Type>>,

    struct_methods: HashMap<String, HashMap<String, Type>>,

    enums: HashMap<String, HashMap<String, EnumVariant>>,

    kinds: HashMap<String, KindDefenition>,

    type_params: Vec<HashSet<String>>,

    next_type_var: usize,

    constraints: Vec<(Type, Type)>,

    substitutions: HashMap<usize, Type>,
}

#[derive(Debug, Clone)]
pub struct KindDefenition {
    pub methods: HashMap<String, Type>,
    pub default_impls: HashMap<String, Vec<Statement>>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
            enums: HashMap::new(),
            kinds: HashMap::new(),
            type_params: vec![HashSet::new()],
            next_type_var: 0,
            constraints: Vec::new(),
            substitutions: HashMap::new(),
        }
    }

    pub fn fresh_type_var(&mut self) -> Type {
        let var_id = self.next_type_var;
        self.next_type_var += 1;
        Type::TypeVar(var_id)
    }

    pub fn add_constraint(&mut self, t1: Type, t2: Type) {
        self.constraints.push((t1, t2));
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Cannot pop the global scope");
        }
    }

    pub fn define(&mut self, name: &str, t: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), t);
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

    pub fn add_enum(&mut self, name: &str, variants: HashMap<String, EnumVariant>) {
        self.enums.insert(name.to_string(), variants);
    }

    pub fn push_type_param_scope(&mut self) {
        self.type_params.push(HashSet::new());
    }

    pub fn pop_type_param_scope(&mut self) {
        if !self.type_params.is_empty() {
            self.type_params.pop();
        }
    }

    pub fn add_type_param(&mut self, name: &str) {
        if let Some(scope) = self.type_params.last_mut() {
            scope.insert(name.to_string());
        }
    }

    pub fn is_type_param_in_scope(&self, name: &str) -> bool {
        for scope in self.type_params.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }
        false
    }

    pub fn from_annotation(&mut self, annotation: &TypeAnnotation) -> Result<Type> {
        match annotation {
            TypeAnnotation::Unit => Ok(Type::Unit),
            TypeAnnotation::Basic(name) => {
                if self.is_type_param_in_scope(name) {
                    return Ok(Type::TypeParam(name.clone()));
                }
                match name.as_str() {
                    "i32" => Ok(Type::I32),
                    "f64" => Ok(Type::F64),
                    "bool" => Ok(Type::Bool),
                    "str" => Ok(Type::String),
                    "char" => Ok(Type::Char),
                    "i64" => Ok(Type::I64),
                    "f32" => Ok(Type::F32),
                    "u32" => Ok(Type::U32),
                    "u64" => Ok(Type::U64),
                    "u8" => Ok(Type::U8),
                    "u16" => Ok(Type::U16),
                    "i8" => Ok(Type::I8),
                    "i16" => Ok(Type::I16),
                    "any" => Ok(Type::Any),
                    name if self.structs.contains_key(name) => Ok(Type::Struct {
                        name: name.to_string(),
                        fields: self.structs.get(name).unwrap().clone(),
                    }),
                    name if self.enums.contains_key(name) => Ok(Type::Enum {
                        name: name.to_string(),
                        variants: self.enums.get(name).unwrap().clone(),
                    }),
                    _ => Err(VeldError::TypeError(format!("Unknown type: {}", name))),
                }
            }
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                let param_types = params
                    .iter()
                    .map(|param| self.from_annotation(param))
                    .collect::<Result<Vec<_>>>()?;
                let return_type = self.from_annotation(return_type)?;
                Ok(Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                })
            }
            TypeAnnotation::Generic { base, type_args } => {
                let type_args = type_args
                    .iter()
                    .map(|arg| self.from_annotation(arg))
                    .collect::<Result<Vec<_>>>()?;

                if base == "Array" {
                    if type_args.len() != 1 {
                        return Err(VeldError::TypeError(
                            "Array must have exactly one type argument".into(),
                        ));
                    }
                    return Ok(Type::Array(Box::new(type_args[0].clone())));
                } else if self.structs.contains_key(base) {
                    Ok(Type::Generic {
                        base: base.clone(),
                        type_args,
                    })
                } else {
                    Err(VeldError::TypeError(format!(
                        "Unknown generic type: {}",
                        base
                    )))
                }
            }
            TypeAnnotation::Array(elem_type) => {
                let elem_type = self.from_annotation(elem_type)?;
                Ok(Type::Array(Box::new(elem_type)))
            }
            TypeAnnotation::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|t| self.from_annotation(t))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Type::Tuple(types))
            }
        }
    }

    fn apply_substitutions(&self, ty: &Type) -> Type {
        match ty {
            Type::TypeVar(id) => {
                if let Some(subst) = self.substitutions.get(&id) {
                    self.apply_substitutions(subst)
                } else {
                    ty.clone()
                }
            }
            Type::Function {
                params,
                return_type,
            } => {
                let new_params: Vec<Type> = params
                    .into_iter()
                    .map(|p| self.apply_substitutions(p))
                    .collect();

                let new_return = self.apply_substitutions(return_type);

                Type::Function {
                    params: new_params,
                    return_type: Box::new(new_return),
                }
            }
            Type::Generic { base, type_args } => {
                let new_args = type_args
                    .iter()
                    .map(|arg| self.apply_substitutions(arg))
                    .collect();
                Type::Generic {
                    base: base.clone(),
                    type_args: new_args,
                }
            }
            Type::Tuple(types) => {
                let new_types = types.iter().map(|t| self.apply_substitutions(t)).collect();
                Type::Tuple(new_types)
            }
            Type::Array(elem) => {
                let new_elem = self.apply_substitutions(elem);
                Type::Array(Box::new(new_elem))
            }
            _ => ty.clone(),
        }
    }

    pub fn solve_constraints(&mut self) -> Result<()> {
        while !self.constraints.is_empty() {
            let (t1, t2) = self.constraints.remove(0);
            self.unify(t1, t2)?;
        }
        Ok(())
    }

    pub fn unify(&mut self, t1: Type, t2: Type) -> Result<()> {
        let t1 = self.apply_substitutions(&t1);
        let t2 = self.apply_substitutions(&t2);

        match (t1, t2) {
            (Type::Unit, Type::Unit)
            | (Type::I32, Type::I32)
            | (Type::F64, Type::F64)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::I64, Type::I64)
            | (Type::F32, Type::F32)
            | (Type::U32, Type::U32)
            | (Type::U64, Type::U64)
            | (Type::U8, Type::U8)
            | (Type::U16, Type::U16)
            | (Type::I8, Type::I8)
            | (Type::I16, Type::I16) => Ok(()),

            (Type::Any, _) | (_, Type::Any) => Ok(()),

            (Type::TypeVar(id), t) | (t, Type::TypeVar(id)) => {
                if self.occurs_check(id, &t) {
                    return Err(VeldError::TypeError(format!("Infinite type: {}", t)));
                }
                self.substitutions.insert(id, t);
                Ok(())
            }
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                if p1.len() != p2.len() {
                    return Err(VeldError::TypeError(
                        "Cannot unify functions with different number of parameters".into(),
                    ));
                }
                for (param1, param2) in p1.into_iter().zip(p2.into_iter()) {
                    self.unify(param1, param2)?;
                }
                self.unify(*r1, *r2)
            }
            (Type::Struct { name: name1, .. }, Type::Struct { name: name2, .. }) => {
                if name1 != name2 {
                    Err(VeldError::TypeError(format!(
                        "Cannot unify struct types {} and {}",
                        name1, name2
                    )))
                } else {
                    Ok(())
                }
            }
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
                if base1 != base2 || args1.len() != args2.len() {
                    return Err(VeldError::TypeError(format!(
                        "Cannot unify generic types {} and {}",
                        base1, base2
                    )));
                }
                for (a1, a2) in args1.into_iter().zip(args2.into_iter()) {
                    self.unify(a1, a2)?;
                }
                Ok(())
            }

            (Type::Array(elem1), Type::Array(elem2)) => self.unify(*elem1, *elem2),

            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.iter().len() {
                    return Err(VeldError::TypeError(
                        "Cannot unify tupes with different lengths".into(),
                    ));
                }

                for (t1, t2) in types1.into_iter().zip(types2.into_iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }
            (t1, t2) => Err(VeldError::TypeError(format!(
                "Cannot unify {} with {}",
                t1, t2
            ))),
        }
    }

    fn occurs_check(&self, id: usize, ty: &Type) -> bool {
        match ty {
            Type::TypeVar(other_id) => *other_id == id,
            Type::Function {
                params,
                return_type,
            } => {
                params.iter().any(|p| self.occurs_check(id, p))
                    || self.occurs_check(id, return_type)
            }
            Type::Generic { type_args, .. } => {
                type_args.iter().any(|arg| self.occurs_check(id, arg))
            }
            Type::Tuple(types) => types.iter().any(|t| self.occurs_check(id, t)),
            Type::Array(elem) => self.occurs_check(id, elem),
            _ => false,
        }
    }
}

pub struct TypeChecker {
    env: TypeEnvironment,
    current_function_return_type: Option<Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
            current_function_return_type: None,
        }
    }

    pub fn check_program(&mut self, statements: &[Statement]) -> Result<()> {
        for stmt in statements {
            match stmt {
                Statement::StructDeclaration { name, fields, .. } => {
                    let mut field_types = HashMap::new();
                    for (field_name, field_type) in fields {
                        let field_type = self.env.from_annotation(field_type)?;
                        field_types.insert(field_name.clone(), field_type);
                    }
                    self.env.add_struct(name, field_types);
                }
                _ => {} // TODO: Handle enum declarations
            }
        }
        for stmt in statements {
            self.type_check_statement(stmt)?;
        }
        Ok(())
    }

    pub fn type_check_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                ..
            } => self.type_check_function(name, params, return_type, body),

            Statement::VariableDeclaration {
                name,
                type_annotation,
                value,
            } => self.type_check_variable_declaration(name, type_annotation.as_ref(), value),

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
            _ => Ok(()),
        }
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
            .map(|(_, type_anno)| self.env.from_annotation(type_anno))
            .collect::<Result<Vec<_>>>()?;

        let ret_type = self.env.from_annotation(return_type)?;

        let function_type = Type::Function {
            params: param_types.clone(),
            return_type: Box::new(ret_type.clone()),
        };

        self.env.define(name, function_type);

        let old_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(ret_type);

        self.env.push_scope();

        for (i, (param_name, _)) in params.iter().enumerate() {
            self.env.define(param_name, param_types[i].clone());
        }

        for stmt in body {
            self.type_check_statement(stmt)?;
        }

        self.current_function_return_type = old_return_type;
        self.env.pop_scope();
        Ok(())
    }

    fn type_check_variable_declaration(
        &mut self,
        name: &str,
        type_annotation: Option<&TypeAnnotation>,
        value: &Expr,
    ) -> Result<()> {
        let value_type = self.infer_expression_type(value)?;

        let var_type = if let Some(anno) = type_annotation {
            let specified_type = self.env.from_annotation(anno)?;
            self.env
                .add_constraint(value_type.clone(), specified_type.clone());
            specified_type
        } else {
            value_type
        };

        let final_type = self.env.apply_substitutions(&var_type);
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
            self.type_check_statement(stmt)?;
        }
        self.env.pop_scope();

        if let Some(else_stmts) = else_branch {
            self.env.push_scope();
            for stmt in else_stmts {
                self.type_check_statement(stmt)?;
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
            self.type_check_statement(stmt)?;
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
            self.type_check_statement(stmt)?;
        }

        self.env.pop_scope();
        self.env.solve_constraints()?;
        Ok(())
    }

    pub fn infer_expression_type(&mut self, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Literal(lit) => self.infer_literal_type(lit),
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
            } => self.infer_lambda_type(params, body, return_type.as_ref()),
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
            EnumVariant => todo!(),
        }
    }

    fn infer_literal_type(&mut self, lit: &Literal) -> Result<Type> {
        match lit {
            Literal::Integer(_) => Ok(Type::I32),
            // TODO: Handle other integer types
            Literal::Float(_) => Ok(Type::F64),
            // TODO: Handle other float types
            Literal::String(_) => Ok(Type::String),
            Literal::Boolean(_) => Ok(Type::Bool),
            Literal::Unit => Ok(Type::Unit),
            Literal::Char(_) => Ok(Type::Char),
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

        match op {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    if left_type == Type::F64 || right_type == Type::F64 {
                        Ok(Type::F64)
                    } else {
                        Ok(Type::I32)
                    }
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply {} to {} and {}",
                        op, left_type, right_type
                    )))
                }
            }
            BinaryOperator::Exponent => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Ok(Type::F64)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply {} to {} and {}",
                        op, left_type, right_type
                    )))
                }
            }
            BinaryOperator::LessEq
            | BinaryOperator::GreaterEq
            | BinaryOperator::Less
            | BinaryOperator::Greater => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Ok(Type::Bool)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply {} to {} and {}",
                        op, left_type, right_type
                    )))
                }
            }
            BinaryOperator::EqualEqual | BinaryOperator::NotEqual => Ok(Type::Bool),
            BinaryOperator::And | BinaryOperator::Or => {
                self.env.add_constraint(left_type, Type::Bool.clone());
                self.env.add_constraint(right_type, Type::Bool.clone());
                self.env.solve_constraints()?;
                Ok(Type::Bool)
            }
            BinaryOperator::Modulo => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Ok(Type::I32)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Cannot apply {} to {} and {}",
                        op, left_type, right_type
                    )))
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
            
            _ if self.is_numeric_type(&t1) && self.is_numeric_type(&t2) => true,
            _ => false,
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
                self.env.from_annotation(anno)?
            } else {
                self.env.fresh_type_var()
            };
            param_types.push(param_type.clone());
            self.env.define(name, param_type);
        }
        let body_type = self.infer_expression_type(body)?;
        let return_type = if let Some(anno) = return_type_anno {
            let rt = self.env.from_annotation(anno)?;
            self.env.add_constraint(body_type, rt.clone());
            rt
        } else {
            body_type
        };
        self.env.pop_scope();
        self.env.solve_constraints()?;
        Ok(Type::Function {
            params: param_types
                .iter()
                .map(|t| self.env.apply_substitutions(t))
                .collect(),
            return_type: Box::new(self.env.apply_substitutions(&return_type)),
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
                    if let Some(methods) = self.env.struct_methods.get(&name) {
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
            Type::Generic { base, type_args } => {
                if let Some(struct_fields) = self.env.structs.get(base) {
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
        let struct_fields_data = if let Some(struct_fields) = self.env.structs.get(struct_name) {
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

        // Check all elements have the same type
        for elem in elements.iter().skip(1) {
            let elem_type = self.infer_expression_type(elem)?;
            self.env.add_constraint(elem_type, first_type.clone());
        }

        self.env.solve_constraints()?;

        let elem_type = self.env.apply_substitutions(&first_type);
        Ok(Type::Array(Box::new(elem_type)))
    }

    fn infer_index_access_type(&mut self, object: &Expr, index: &Expr) -> Result<Type> {
        let obj_type = self.infer_expression_type(object)?;
        let idx_type = self.infer_expression_type(index)?;

        // Index should be integer
        self.env.add_constraint(idx_type, Type::I32);

        match &obj_type {
            Type::Array(elem_type) => {
                Ok(*elem_type.clone())
            },

            Type::String => {
                // Indexing into string gives single character string
                Ok(Type::String)
            },

            _ => Err(VeldError::TypeError(format!(
                "Cannot index into type: {}", obj_type
            ))),
        }
    }
}
