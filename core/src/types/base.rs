use crate::ast::{GenericArgument, Statement, TypeAnnotation, VarKind};
use crate::error::{Result, VeldError};
use crate::interpreter::Value;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;

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

    enums: HashMap<String, HashMap<String, EnumVariant>>,

    kinds: HashMap<String, KindDefenition>,

    type_params: Vec<HashSet<String>>,

    next_type_var: usize,

    constraints: Vec<(Type, Type)>,

    substitutions: HashMap<usize, Type>,

    implementations: HashMap<String, Vec<ImplementationInfo>>,
    generic_structs: HashMap<String, (HashMap<String, Type>, Vec<GenericArgument>)>,
    pub generic_struct_names: HashSet<String>,
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
            kinds: HashMap::new(),
            type_params: vec![HashSet::new()],
            next_type_var: 0,
            constraints: Vec::new(),
            substitutions: HashMap::new(),
            implementations: HashMap::new(),
            generic_structs: HashMap::new(),
            generic_struct_names: HashSet::new(),
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
