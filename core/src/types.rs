use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;
use crate::ast::{Statement, TypeAnnotation};
use crate::error::{Result, VeldError};
use crate::types::Type::TypeParam;

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
            Type::Function {params, return_type} => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            },
            Type::Struct {name, ..} => write!(f, "{}", name),
            Type::Generic {base, type_args} => {
                write!(f, "{}<", base)?;
                for (i, arg) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            },
            Type::TypeParam(name) => write!(f, "{}", name),
            Type::Enum {name, ..} => write!(f, "{}", name),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            },
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
        Self{
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
        let methods = self.struct_methods.entry(struct_name.to_string()).or_insert_with(HashMap::new);
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
    
    pub fn from_annotation(&mut self, annotation: &TypeAnnotation)->Result<Type>{
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
                    name if self.structs.contains_key(name) => {
                        Ok(Type::Struct {
                            name: name.to_string(),
                            fields: self.structs.get(name).unwrap().clone(),
                        })
                    }
                    name if self.enums.contains_key(name) => {
                        Ok(Type::Enum {
                            name: name.to_string(),
                            variants: self.enums.get(name).unwrap().clone(),
                        })
                    },
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
            TypeAnnotation::Generic {
                base,
                type_args,
            } => {
                let type_args = type_args
                    .iter()
                    .map(|arg| self.from_annotation(arg))
                    .collect::<Result<Vec<_>>>()?;
                
                if base == "Array" {
                    if type_args.len() != 1 {
                        return Err(VeldError::TypeError("Array must have exactly one type argument".into()));
                    }
                    return Ok(Type::Array(Box::new(type_args[0].clone())));
                } else if self.structs.contains_key(base) { 
                    Ok(Type::Generic {
                        base: base.clone(),
                        type_args
                    })
                } else { 
                    Err(VeldError::TypeError(format!("Unknown generic type: {}", base)))
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
            },
            Type::Function {params, return_type} => {
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
            Type::Generic {base, type_args} => {
                let new_args = type_args.iter().map(|arg| self.apply_substitutions(arg)).collect();
                Type::Generic {
                    base: base.clone(),
                    type_args: new_args,
                }
            }
            Type::Tuple(types) => {
                let new_types = types.iter().map(|t| self.apply_substitutions(t)).collect();
                Type::Tuple(new_types)
            },
            Type::Array(elem) => {
                let new_elem = self.apply_substitutions(elem);
                Type::Array(Box::new(new_elem))
            },
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
            (Type::Unit, Type::Unit) |
            (Type::I32, Type::I32) |
            (Type::F64, Type::F64) |
            (Type::Bool, Type::Bool) |
            (Type::String, Type::String) |
            
            (Type::Char, Type::Char) |
            (Type::I64, Type::I64) |
            (Type::F32, Type::F32) |
            (Type::U32, Type::U32) |
            (Type::U64, Type::U64) |
            (Type::U8, Type::U8) |
            (Type::U16, Type::U16) |
            (Type::I8, Type::I8) |
            (Type::I16, Type::I16) => Ok(()),

            (Type::Any, _) | (_, Type::Any) => Ok(()),
            
            (Type::TypeVar(id), t) | (t, Type::TypeVar(id)) => {
                if self.occurs_check(id, &t) {
                    return Err(VeldError::TypeError(format!("Infinite type: {}", t)));
                }
                self.substitutions.insert(id, t);
                Ok(())
            },
            (
                Type::Function {params: p1, return_type: r1},
                Type::Function {params: p2, return_type: r2}
            ) => {
                if p1.len() != p2.len() {
                    return Err(VeldError::TypeError("Cannot unify functions with different number of parameters".into()));
                }
                for (param1, param2) in p1.into_iter().zip(p2.into_iter()) {
                    self.unify(param1, param2)?;
                }
                self.unify(*r1, *r2)
            },
            (
                Type::Struct {name: name1, ..},
                Type::Struct {name: name2, ..}
            ) => {
                if name1 != name2 {
                    Err(VeldError::TypeError(format!("Cannot unify struct types {} and {}", name1, name2)))
                } else {
                    Ok(())
                }
            },
            (
                Type::Generic {base: base1, type_args: args1},
                Type::Generic {base: base2, type_args: args2}
            ) => {
                if base1!= base2 || args1.len() != args2.len() {
                    return Err(VeldError::TypeError(format!("Cannot unify generic types {} and {}", base1, base2)));
                }
                for (a1, a2) in args1.into_iter().zip(args2.into_iter()) {
                    self.unify(a1, a2)?;
                }
                Ok(())
            },

            (Type::Array(elem1), Type::Array(elem2)) => {
                self.unify(*elem1, *elem2)
            },

            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.iter().len() {
                    return Err(VeldError::TypeError("Cannot unify tupes with different lengths".into()));
                }

                for (t1, t2) in types1.into_iter().zip(types2.into_iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            },
            (t1, t2) => Err(VeldError::TypeError(format!("Cannot unify {} with {}", t1, t2)))
        }
    }

    fn occurs_check(&self, id: usize, ty: &Type) -> bool {
        match ty {
            Type::TypeVar(other_id) => *other_id == id,
            Type::Function {params, return_type} => {
                params.iter().any(|p| self.occurs_check(id, p)) || self.occurs_check(id, return_type)
            },
            Type::Generic {type_args, ..} => {
                type_args.iter().any(|arg| self.occurs_check(id, arg))
            },
            Type::Tuple(types) => {
                types.iter().any(|t| self.occurs_check(id, t))
            },
            Type::Array(elem) => {
                self.occurs_check(id, elem)
            },
            _ => {
                false
            }
        }
    }
}