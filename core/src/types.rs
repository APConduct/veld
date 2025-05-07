use std::collections::HashMap;

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