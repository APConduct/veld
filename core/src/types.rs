use std::collections::HashMap;
use std::fmt::{write, Formatter};

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