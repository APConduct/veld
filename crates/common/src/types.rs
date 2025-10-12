use super::ast::{GenericArgument, Statement, TypeAnnotation, VarKind};
use super::value::Value;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;
use veld_error::{Result, VeldError};

pub mod checker;

// pub const U8_MAX: u8 = 255;
// pub const U16_MAX: u16 = 65535;
// pub const U32_MAX: u32 = 4294967295;
// pub const U64_MAX: u64 = 18446744073709551615;
// pub const I8_MAX: i8 = 127;
// pub const I16_MAX: i16 = 32767;
// pub const I32_MAX: i32 = 2147483647;
// pub const I64_MAX: i64 = 9223372036854775807;
// pub const F32_MAX: f32 = 3.40282347e+38;
// pub const F64_MAX: f64 = 1.7976931348623157e+308;
// pub const I8_MIN: i8 = -128;
// pub const I16_MIN: i16 = -32768;
// pub const I32_MIN: i32 = -2147483648;
// pub const I64_MIN: i64 = -9223372036854775808;
// pub const F32_MIN: f32 = -3.40282347e+38;
// pub const F64_MIN: f64 = -1.7976931348623157e+308;
// pub const U8_MIN: u8 = 0;
// pub const U16_MIN: u16 = 0;
// pub const U32_MIN: u32 = 0;
// pub const U64_MIN: u64 = 0;

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

    IntegerLiteral(i64),
    FloatLiteral(f64),

    Number,

    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },

    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },

    // Anonymous struct(table), like this: { field1: Type, field2: Type }
    Record {
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

    KindSelf(String),
    Module(String),
    StructType(String), // Represents a struct type that can have static methods called on it
    EnumType(String),   // Represents an enum type that can have constructor methods called on it
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::F32
            | Type::F64 => true,
            _ => false,
        }
    }

    pub fn from_annotation(
        annotation: &TypeAnnotation,
        current_kind: Option<&str>,
    ) -> Result<Type> {
        match annotation {
            TypeAnnotation::Unit => Ok(Type::Unit),
            TypeAnnotation::Basic(name) => {
                if let Some(kind_name) = current_kind {
                    if name == kind_name {
                        return Ok(Type::KindSelf(name.clone()));
                    }
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
                    // "any" => Ok(Type::Any),
                    _ => Err(VeldError::TypeError(format!("Unknown type: {}", name))),
                }
            }
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                let param_types = params
                    .iter()
                    .map(|param| Type::from_annotation(param, None))
                    .collect::<Result<Vec<_>>>()?;
                let return_type = Type::from_annotation(return_type, None)?;
                Ok(Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                })
            }
            TypeAnnotation::Generic { base, type_args } => {
                let type_args = type_args
                    .iter()
                    .map(|arg| Type::from_annotation(arg, None))
                    .collect::<Result<Vec<_>>>()?;

                if base == "Array" {
                    if type_args.len() != 1 {
                        return Err(VeldError::TypeError(
                            "Array must have exactly one type argument".into(),
                        ));
                    }
                    return Ok(Type::Array(Box::new(type_args[0].clone())));
                } else {
                    Ok(Type::Generic {
                        base: base.clone(),
                        type_args,
                    })
                }
            }
            TypeAnnotation::Array(elem_type) => {
                let elem_type = Type::from_annotation(elem_type, None)?;
                Ok(Type::Array(Box::new(elem_type)))
            }
            TypeAnnotation::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|t| Type::from_annotation(t, None))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Type::Tuple(types))
            }
            TypeAnnotation::Record { fields } => {
                let mut field_types = std::collections::HashMap::new();
                for (field_name, field_type) in fields {
                    let ty = Type::from_annotation(field_type, None)?;
                    field_types.insert(field_name.clone(), ty);
                }
                Ok(Type::Record {
                    fields: field_types,
                })
            }
            TypeAnnotation::Constrained {
                base_type: _,
                constraints: _,
            } => {
                todo!("Handle constrained types")
            }
        }
    }
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
            // Anonymous struct(Record), looks like this: { field1: type1, field2: type2 }
            Type::Record { fields } => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.0, field.1)?;
                }
                write!(f, "}}")
            }
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
            Type::Module(name) => write!(f, "module: {}", name),
            Type::IntegerLiteral(_value) => write!(f, "int"),
            Type::FloatLiteral(_value) => write!(f, "float"),
            Type::Number => write!(f, "number"),

            Type::KindSelf(name) => write!(f, "{}", name),
            Type::StructType(name) => write!(f, "struct type: {}", name),
            Type::EnumType(name) => write!(f, "enum type: {}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Simple,
    Tuple(Vec<Type>),
    Struct(HashMap<String, Type>),
}

#[derive(Debug, Clone)]
pub struct ImplementationInfo {
    type_name: String,
    kind_name: String,
    generic_args: Vec<GenericArgument>,
    methods: HashMap<String, Type>,
    // TODO: Add where clause constraints to track conditional implementations
    // where_clause: Option<WhereClause>,
}

impl ImplementationInfo {
    pub fn from(
        type_name: String,
        kind_name: String,
        generic_args: Vec<GenericArgument>,
        methods: HashMap<String, Type>,
    ) -> Self {
        Self {
            type_name,
            kind_name,
            generic_args,
            methods,
        }
    }
    pub fn new() -> Self {
        Self {
            type_name: String::new(),
            kind_name: String::new(),
            generic_args: Vec::new(),
            methods: HashMap::new(),
        }
    }
    pub fn add_generic_arg(&mut self, arg: GenericArgument) {
        self.generic_args.push(arg);
    }
    pub fn generic_args(&self) -> Vec<GenericArgument> {
        self.generic_args.clone()
    }
    pub fn type_name(&self) -> String {
        self.type_name.clone()
    }
    pub fn add_method(&mut self, name: String, ty: Type) {
        self.methods.insert(name, ty);
    }
    pub fn get_method(&self, name: &str) -> Option<&Type> {
        self.methods.get(name)
    }
    pub fn kind_name(&self) -> String {
        self.kind_name.clone()
    }
    pub fn methods(&self) -> HashMap<String, Type> {
        self.methods.clone()
    }
    pub fn get_generic_arg(&self, name: &str) -> Option<&GenericArgument> {
        self.generic_args
            .iter()
            .find(|arg| arg.name == Some(name.to_string()))
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, Type>>,

    structs: HashMap<String, HashMap<String, Type>>,

    struct_methods: HashMap<String, HashMap<String, Type>>,

    pub enums: HashMap<String, HashMap<String, EnumVariant>>,

    enum_methods: HashMap<String, HashMap<String, Type>>,

    kinds: HashMap<String, KindDefenition>,

    type_params: Vec<HashSet<String>>,

    next_type_var: usize,

    constraints: Vec<(Type, Type)>,

    substitutions: HashMap<usize, Type>,

    implementations: HashMap<String, Vec<ImplementationInfo>>,
    generic_structs: HashMap<String, (HashMap<String, Type>, Vec<GenericArgument>)>,
    pub generic_struct_names: HashSet<String>,
    type_aliases: HashMap<String, Type>,
    generic_type_aliases: HashMap<String, (Type, Vec<String>)>, // name -> (definition, type_params)
}

#[derive(Debug, Clone)]
pub struct KindDefenition {
    pub methods: HashMap<String, Type>,
    pub default_impls: HashMap<String, Vec<Statement>>,
    pub generic_params: Vec<String>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
            enums: HashMap::new(),
            enum_methods: HashMap::new(),
            kinds: HashMap::new(),
            type_params: vec![HashSet::new()],
            next_type_var: 0,
            constraints: Vec::new(),
            substitutions: HashMap::new(),
            implementations: HashMap::new(),
            generic_structs: HashMap::new(),
            generic_struct_names: HashSet::new(),
            type_aliases: HashMap::new(),
            generic_type_aliases: HashMap::new(),
        }
    }

    pub fn struct_methods(&self) -> &HashMap<String, HashMap<String, Type>> {
        &self.struct_methods
    }

    pub fn struct_methods_mut(&mut self) -> &mut HashMap<String, HashMap<String, Type>> {
        &mut self.struct_methods
    }

    pub fn structs(&self) -> &HashMap<String, HashMap<String, Type>> {
        &self.structs
    }

    pub fn kinds(&self) -> &HashMap<String, KindDefenition> {
        &self.kinds
    }

    pub fn add_generic_struct(
        &mut self,
        name: &str,
        fields: HashMap<String, Type>,
        generic_params: Vec<GenericArgument>,
    ) {
        self.generic_structs
            .insert(name.to_string(), (fields, generic_params));
        self.generic_struct_names.insert(name.to_string());
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

    pub fn is_defined(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return true;
            }
        }
        false
    }

    pub fn update_definition(&mut self, name: &str, new_type: Type) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), new_type);
                return;
            }
        }
        // If not found, define it in the current scope
        self.define(name, new_type);
    }

    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
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

    pub fn add_type_alias(&mut self, name: &str, ty: Type) {
        self.type_aliases.insert(name.to_string(), ty);
    }

    pub fn add_generic_type_alias(&mut self, name: &str, ty: Type, type_params: Vec<String>) {
        self.generic_type_aliases
            .insert(name.to_string(), (ty, type_params));
    }

    pub fn get_type_alias(&self, name: &str) -> Option<&Type> {
        self.type_aliases.get(name)
    }

    pub fn resolve_type_alias(&self, name: &str) -> Option<Type> {
        if let Some(ty) = self.type_aliases.get(name) {
            Some(ty.clone())
        } else {
            None
        }
    }

    pub fn add_kind(
        &mut self,
        name: &str,
        methods: HashMap<String, Type>,
        default_impls: HashMap<String, Vec<Statement>>,
        generic_params: Vec<String>,
    ) {
        self.kinds.insert(
            name.to_string(),
            KindDefenition {
                methods,
                default_impls,
                generic_params,
            },
        );
    }

    pub fn add_enum(&mut self, name: &str, variants: HashMap<String, EnumVariant>) {
        self.enums.insert(name.to_string(), variants);
    }

    pub fn add_enum_method(&mut self, enum_name: &str, method_name: &str, method_type: Type) {
        let methods = self
            .enum_methods
            .entry(enum_name.to_string())
            .or_insert_with(HashMap::new);
        methods.insert(method_name.to_string(), method_type);
    }

    pub fn get_enum_methods(&self, enum_name: &str) -> Option<&HashMap<String, Type>> {
        self.enum_methods.get(enum_name)
    }

    pub fn push_type_param_scope(&mut self) {
        self.type_params.push(HashSet::new());
    }

    pub fn pop_type_param_scope(&mut self) {
        if !self.type_params.is_empty() {
            self.type_params.pop();
        }
    }

    pub fn has_type_param_scopes(&self) -> bool {
        !self.type_params.is_empty()
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

    /// Check if a type conversion is a safe widening conversion (no data loss)
    pub fn is_safe_widening(&self, from_type: &Type, to_type: &Type) -> bool {
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

    pub fn from_annotation(
        &mut self,
        annotation: &TypeAnnotation,
        current_kind: Option<&str>,
    ) -> Result<Type> {
        match annotation {
            TypeAnnotation::Unit => Ok(Type::Unit),
            TypeAnnotation::Basic(name) => {
                if self.is_type_param_in_scope(name) {
                    return Ok(Type::TypeParam(name.clone()));
                }
                if let Some(kind_name) = current_kind {
                    if name == kind_name {
                        return Ok(Type::KindSelf(name.clone()));
                    }
                }

                // Check for type aliases first
                if let Some(alias_type) = self.resolve_type_alias(name) {
                    return Ok(alias_type);
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
                    "infer" => Ok(self.fresh_type_var()),
                    name if self.structs.contains_key(name)
                        || self.generic_struct_names.contains(name) =>
                    {
                        // For generic structs, just use the name
                        if self.generic_struct_names.contains(name) {
                            return Ok(Type::Struct {
                                name: name.to_string(),
                                fields: HashMap::new(),
                            });
                        }
                        // For regular structs, use fields
                        Ok(Type::Struct {
                            name: name.to_string(),
                            fields: self.structs.get(name).unwrap().clone(),
                        })
                    }
                    name if self.enums.contains_key(name) => Ok(Type::Enum {
                        name: name.to_string(),
                        variants: self.enums.get(name).unwrap().clone(),
                    }),
                    name if self.kinds.contains_key(name) => Ok(Type::KindSelf(name.to_string())),
                    _ => Err(VeldError::TypeError(format!("Unknown type: {}", name))),
                }
            }
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                let param_types = params
                    .iter()
                    .map(|param| self.from_annotation(param, None))
                    .collect::<Result<Vec<_>>>()?;
                let return_type = self.from_annotation(return_type, None)?;
                Ok(Type::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                })
            }
            TypeAnnotation::Generic { base, type_args } => {
                let type_args = type_args
                    .iter()
                    .map(|arg| self.from_annotation(arg, None))
                    .collect::<Result<Vec<_>>>()?;

                if base == "Array" {
                    if type_args.len() != 1 {
                        return Err(VeldError::TypeError(
                            "Array must have exactly one type argument".into(),
                        ));
                    }
                    return Ok(Type::Array(Box::new(type_args[0].clone())));
                } else if self.structs.contains_key(base) || self.is_type_param_in_scope(base) {
                    Ok(Type::Generic {
                        base: base.clone(),
                        type_args,
                    })
                } else if self.enums.contains_key(base) {
                    Ok(Type::Generic {
                        base: base.clone(),
                        type_args,
                    })
                } else if let Some((definition, type_params)) =
                    self.generic_type_aliases.get(base).cloned()
                {
                    // Handle generic type aliases (plex types)
                    if type_args.len() != type_params.len() {
                        return Err(VeldError::TypeError(format!(
                            "Generic type '{}' expects {} type arguments, got {}",
                            base,
                            type_params.len(),
                            type_args.len()
                        )));
                    }

                    // Create substitution map
                    let mut substitutions = HashMap::new();
                    for (param, arg) in type_params.iter().zip(type_args.iter()) {
                        substitutions.insert(param.clone(), arg.clone());
                    }

                    // Substitute type parameters with concrete types
                    let substituted_type = self.substitute_type_params(&definition, &substitutions);
                    Ok(substituted_type)
                } else {
                    Err(VeldError::TypeError(format!(
                        "Unknown generic type: {}",
                        base
                    )))
                }
            }
            TypeAnnotation::Array(elem_type) => {
                let elem_type = self.from_annotation(elem_type, None)?;
                Ok(Type::Array(Box::new(elem_type)))
            }
            TypeAnnotation::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|t| self.from_annotation(t, None))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Type::Tuple(types))
            }
            TypeAnnotation::Record { fields } => {
                let mut field_types = std::collections::HashMap::new();
                for (field_name, field_type) in fields {
                    let ty = self.from_annotation(field_type, None)?;
                    field_types.insert(field_name.clone(), ty);
                }
                Ok(Type::Record {
                    fields: field_types,
                })
            }
            TypeAnnotation::Constrained {
                base_type: _,
                constraints: _,
            } => {
                todo!("Handle constrained types")
            }
        }
    }

    pub fn apply_substitutions(&self, ty: &Type) -> Type {
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

        match (t1.clone(), t2.clone()) {
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

            (Type::TypeParam(_), _) | (_, Type::TypeParam(_)) => {
                // Type parameters should be handled through substitution during method resolution
                // For now, consider them compatible with any type
                Ok(())
            }

            (Type::TypeVar(id), t) | (t, Type::TypeVar(id)) => {
                if self.occurs_check(id, &t) {
                    return Err(VeldError::TypeError(format!("Infinite type: {}", t)));
                }
                self.substitutions.insert(id, t);
                Ok(())
            }

            // Allow safe widening conversions in unification
            (actual, target) if self.is_safe_widening(&actual, &target) => Ok(()),
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
                if base1 != base2 {
                    return Err(VeldError::TypeError(format!(
                        "Cannot unify generic types {} and {}",
                        base1, base2
                    )));
                }

                // If either has empty type args, consider them unifiable (lenient mode)
                // This handles cases like Result<> being unifiable with Result<i32, str>
                if args1.is_empty() || args2.is_empty() {
                    return Ok(());
                }

                // Otherwise, check type args compatibility
                if args1.len() != args2.len() {
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
            (Type::Record { fields: fields1 }, Type::Record { fields: fields2 }) => {
                // Records are structurally typed - they unify if they have the same fields with unifiable types
                if fields1.len() != fields2.len() {
                    return Err(VeldError::TypeError(format!(
                        "Cannot unify record types with different number of fields: {} vs {}",
                        fields1.len(),
                        fields2.len()
                    )));
                }

                // Check that every field in fields1 has a corresponding field in fields2 with unifiable type
                for (field_name, field_type1) in fields1 {
                    if let Some(field_type2) = fields2.get(&field_name) {
                        self.unify(field_type1, field_type2.clone())?;
                    } else {
                        return Err(VeldError::TypeError(format!(
                            "Cannot unify record types: field '{}' missing in target type",
                            field_name
                        )));
                    }
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
    pub fn add_implementation(
        &mut self,
        type_name: &str,
        kind_name: &str,
        generic_args: Vec<GenericArgument>,
        methods: HashMap<String, Type>,
    ) {
        let impl_info = ImplementationInfo {
            type_name: type_name.to_string(),
            kind_name: kind_name.to_string(),
            generic_args,
            methods,
            // TODO: Pass and store where clause when adding implementations
        };
        self.implementations
            .entry(type_name.to_string())
            .or_insert_with(Vec::new)
            .push(impl_info);
    }

    pub fn get_implementations_for_type(
        &self,
        type_name: &str,
    ) -> Option<&Vec<ImplementationInfo>> {
        self.implementations.get(type_name)
    }

    pub fn find_implementation(
        &self,
        type_name: &str,
        kind_name: &str,
    ) -> Option<&ImplementationInfo> {
        if let Some(impls) = self.get_implementations_for_type(type_name) {
            return impls
                .iter()
                .find(|impl_info| impl_info.kind_name == kind_name);
            // TODO: When where clauses are implemented, also check that
            // the concrete types satisfy the where clause constraints
        }
        None
    }

    pub fn find_implementation_method(
        &self,
        type_name: &str,
        kind_name: &str,
        method_name: &str,
    ) -> Option<&Type> {
        if let Some(impl_info) = self.find_implementation(type_name, kind_name) {
            // TODO: Before returning method, validate that where clause constraints
            // are satisfied for the specific type arguments being used
            if let Some(method) = impl_info.methods.get(method_name) {
                return Some(method);
            }
        }
        None
    }

    pub fn types_match(&self, t1: &Type, t2: &Type) -> bool {
        let t1 = self.apply_substitutions(t1);
        let t2 = self.apply_substitutions(t2);

        match (&t1, &t2) {
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
                    return false;
                }
                for (params1, param2) in p1.iter().zip(p2.iter()) {
                    if !self.types_match(params1, param2) {
                        return false;
                    }
                }
                self.types_match(r1, r2)
            }
            (Type::Generic { base: b1, .. }, Type::Generic { base: b2, .. }) => b1 == b2,
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    return false;
                }
                for (t1, t2) in t1.iter().zip(t2.iter()) {
                    if !self.types_match(t1, t2) {
                        return false;
                    }
                }
                true
            }
            (Type::Array(t1), Type::Array(t2)) => self.types_match(t1, t2),
            // TODO: Handle other types
            _ => t1 == t2,
        }
    }

    // Check if a type structurally implements a kind
    pub fn type_structurally_implements_kind(&self, ty: &Type, kind_name: &str) -> bool {
        // Special cases for built-in types with native implementations
        match (ty, kind_name) {
            // Core capabilities
            (_, "std.core.Value") => return true, // All types implement Value
            (Type::String, "std.core.ToString") => return true,
            (Type::I32, "std.core.ToString") => return true,
            (Type::F64, "std.core.ToString") => return true,
            (Type::Bool, "std.core.ToString") => return true,
            (Type::String, "std.core.Sized") => return true,
            (Type::Array(_), "std.core.Sized") => return true,

            // String capabilities
            (Type::String, "std.string.Transformable") => return true,
            (Type::String, "std.string.Searchable") => return true,
            (Type::String, "std.string.Manipulatable") => return true,
            (Type::String, "std.string.Parsable") => return true,

            // Sequence capabilities
            (Type::Array(_), "std.collections.sequence.Sequence") => return true,

            // Vec capabilities
            (Type::Generic { base, .. }, "std.collections.sequence.Sequence")
                if base.to_string() == "Vec" =>
            {
                return true;
            }
            (Type::Generic { base, .. }, "std.collections.sequence.GrowableSequence")
                if base.to_string() == "Vec" =>
            {
                return true;
            }

            // Numeric capabilities
            (Type::I32, "std.numeric.Numeric") => return true,
            (Type::F64, "std.numeric.Numeric") => return true,
            (Type::I32, "std.numeric.Integer") => return true,
            (Type::F64, "std.numeric.Float") => return true,

            _ => {}
        }

        let kind = match self.kinds.get(kind_name) {
            Some(kind) => kind,
            None => return false,
        };

        match ty {
            Type::Struct { name, .. } => {
                for (method_name, required_type) in &kind.methods {
                    if let Some(methods) = self.struct_methods.get(name) {
                        if let Some(method_type) = methods.get(method_name) {
                            if !self.types_match(method_type, required_type) {
                                return false;
                            }
                        } else {
                            return false; // Method not found
                        }
                    } else {
                        return false; // No methods for this struct
                    }
                }
                true
            }
            Type::Enum { .. } => todo!("Handle enum implementations"),
            // TODO: Handle other types
            _ => false,
        }
    }

    pub fn get_struct_implementations(&self, struct_name: &str) -> Option<Vec<ImplementationInfo>> {
        let _ = struct_name; // Placeholder for future use
        todo!()
    }

    /// Substitute type parameters in a type with concrete types
    pub fn substitute_type_params(&self, ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
        match ty {
            Type::TypeParam(param_name) => substitutions
                .get(param_name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Type::Generic { base, type_args } => {
                let substituted_args = type_args
                    .iter()
                    .map(|arg| self.substitute_type_params(arg, substitutions))
                    .collect();
                Type::Generic {
                    base: base.clone(),
                    type_args: substituted_args,
                }
            }
            Type::Function {
                params,
                return_type,
            } => {
                let substituted_params = params
                    .iter()
                    .map(|param| self.substitute_type_params(param, substitutions))
                    .collect();
                let substituted_return =
                    Box::new(self.substitute_type_params(return_type, substitutions));
                Type::Function {
                    params: substituted_params,
                    return_type: substituted_return,
                }
            }
            Type::Array(elem_type) => Type::Array(Box::new(
                self.substitute_type_params(elem_type, substitutions),
            )),
            Type::Tuple(types) => {
                let substituted_types = types
                    .iter()
                    .map(|t| self.substitute_type_params(t, substitutions))
                    .collect();
                Type::Tuple(substituted_types)
            }
            Type::Record { fields } => {
                let substituted_fields = fields
                    .iter()
                    .map(|(name, field_type)| {
                        (
                            name.clone(),
                            self.substitute_type_params(field_type, substitutions),
                        )
                    })
                    .collect();
                Type::Record {
                    fields: substituted_fields,
                }
            }
            _ => ty.clone(),
        }
    }

    /// Check if a type variable has been constrained to be numeric
    pub fn is_type_var_numeric(&self, ty: &Type) -> bool {
        match ty {
            Type::TypeVar(id) => {
                // Check if this type variable has been unified with a numeric type
                if let Some(unified_type) = self.substitutions.get(id) {
                    matches!(
                        unified_type,
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
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    ty: Type,
    var_kind: VarKind,
    value: Option<Value>,
}

impl VarInfo {
    pub fn new(ty: Type, var_kind: VarKind, value: Option<Value>) -> Self {
        Self {
            ty,
            var_kind,
            value,
        }
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn var_kind(&self) -> &VarKind {
        &self.var_kind
    }

    pub fn value(&self) -> Option<&Value> {
        self.value.as_ref()
    }
}
