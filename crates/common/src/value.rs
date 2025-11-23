use super::ast::{Statement, TypeAnnotation};
use super::types::Type;
use std::collections::HashMap;
use std::fmt::Display;
use veld_error::{Result, VeldError};
pub mod module;
pub mod numeric;
pub mod numeric_ops;
use module::Module;
use numeric::NumericValue;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
// Anonymous struct like: { field1: Type, field2: Type }
pub struct Record {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Record(HashMap<String, Value>),
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
    GcRef(crate::gc::handle::GcHandle),
    Enum {
        enum_name: String, // This is the name of the enum type, not the instance or variant
        variant_name: String,
        fields: Vec<Value>,
        // methods: Option<HashMap<String, Value>>, // Methods are accessible via the EnumType
    },
    EnumType {
        name: String,
        methods: Option<HashMap<String, Value>>, // Methods are only stored on the EnumType
    },
    StructType {
        name: String,
        methods: Option<HashMap<String, Value>>, // Methods are only stored on the StructType
    },
    Tuple(Vec<Value>),

    Break,
    Continue,

    CompiledFunction {
        chunk: super::bytecode::Chunk,
        arity: u8,
        name: Option<String>,
    },

    Module(Module),
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Record(fields) => {
                state.write_u8(0);
                for (key, value) in fields {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Numeric(numeric) => {
                state.write_u8(1);
                numeric.hash(state);
            }
            Value::Integer(i) => {
                state.write_u8(2);
                i.hash(state);
            }
            Value::Float(f) => {
                state.write_u8(3);
                state.write(&f.to_le_bytes());
            }
            Value::String(s) => {
                state.write_u8(4);
                s.hash(state);
            }
            Value::Boolean(b) => {
                state.write_u8(5);
                b.hash(state);
            }
            Value::Char(c) => {
                state.write_u8(6);
                c.hash(state);
            }
            Value::Return(val) => {
                state.write_u8(7);
                val.hash(state);
            }
            Value::Unit => {
                state.write_u8(8);
            }
            Value::Function {
                params,
                body,
                return_type,
                captured_vars,
            } => {
                state.write_u8(9);
                for (param, ty) in params {
                    param.hash(state);
                    ty.hash(state);
                }
                for stmt in body {
                    stmt.hash(state);
                }
                return_type.hash(state);
                for (key, value) in captured_vars {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Struct { name, fields } => {
                state.write_u8(10);
                name.hash(state);
                for (key, value) in fields {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Array(arr) => {
                state.write_u8(11);
                for value in arr {
                    value.hash(state);
                }
            }
            Value::GcRef(gc_handle) => {
                state.write_u8(12);
                gc_handle.hash(state);
            }
            Value::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                state.write_u8(13);
                enum_name.hash(state);
                variant_name.hash(state);
                for field in fields {
                    field.hash(state);
                }
            }
            Value::EnumType { name, methods } => {
                state.write_u8(14);
                name.hash(state);
                if let Some(methods) = methods {
                    for (key, value) in methods {
                        key.hash(state);
                        value.hash(state);
                    }
                }
            }
            Value::StructType { name, methods } => {
                state.write_u8(15);
                name.hash(state);
                if let Some(methods) = methods {
                    for (key, value) in methods {
                        key.hash(state);
                        value.hash(state);
                    }
                }
            }
            Value::Tuple(values) => {
                state.write_u8(16);
                for value in values {
                    value.hash(state);
                }
            }
            Value::Break => {
                state.write_u8(17);
            }
            Value::Continue => {
                state.write_u8(18);
            }
            Value::CompiledFunction { chunk, arity, name } => {
                state.write_u8(19);
                chunk.hash(state);
                arity.hash(state);
                name.hash(state);
            }
            Value::Module(module) => {
                state.write_u8(20);
                module.hash(state);
            }
        }
    }
}

impl Value {
    // Helper method to unwrap return values
    pub fn unwrap_return(self) -> Value {
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
            Value::Module(module) => Type::Module(module.name.clone()),
            Value::Function {
                params,
                return_type,
                ..
            } => Type::Function {
                params: params
                    .iter()
                    .map(|(_, ty)| Type::from_annotation(ty, None).unwrap_or(Type::Any))
                    .collect::<Vec<_>>(),
                return_type: Box::new(
                    Type::from_annotation(return_type, None).unwrap_or(Type::Any),
                ),
            },
            Value::Struct { name, fields } => Type::Struct {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            },
            Value::Enum { enum_name, .. } => Type::Enum {
                name: enum_name.clone(),
                variants: HashMap::new(),
            },
            Value::EnumType { name, .. } => Type::Enum {
                name: name.clone(),
                variants: HashMap::new(),
            },
            Value::StructType { name, .. } => Type::Generic {
                base: name.clone(),
                type_args: vec![],
            },
            Value::Tuple(values) => Type::Tuple(values.iter().map(|v| v.type_of()).collect()),
            Value::Record(fields) => Type::Record {
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            },
            Value::CompiledFunction {
                chunk: _,
                arity: _,
                name: _,
            } => {
                todo!("Implement type inference for compiled functions")
            }
            Value::GcRef(value) => Type::GcRef(Box::new(value.type_of())),
        }
    }

    pub fn perform_binary_op(&self, op: &super::ast::BinaryOperator, rhs: &Value) -> Result<Value> {
        let span = tracing::debug_span!("binary_op", op = ?op, lhs = ?self, rhs = ?rhs);
        let _enter = span.enter();

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

    // Remove the generic parameter T, as it is not used and causes diagnostics.
    // If you want to downcast, you should use Any, but here we just provide a reference to self for supported variants.
    pub fn downcast_ref<T: ?Sized>(&self) -> Option<&Value> {
        match self {
            Value::Numeric(_) => Some(self),
            Value::String(_) => Some(self),
            Value::Boolean(_) => Some(self),
            Value::Unit => Some(self),
            Value::Struct { .. } => Some(self),
            Value::Enum { .. } => Some(self),
            Value::Array(_) => Some(self),
            Value::CompiledFunction { .. } => Some(self),
            Value::GcRef(_) => Some(self),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Numeric(n) => write!(f, "{:?}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
            Value::Struct { name, fields } => {
                write!(f, "{} {{", name)?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Enum {
                enum_name,
                variant_name,
                ..
            } => {
                write!(f, "{}::{}", enum_name, variant_name)
            }
            Value::EnumType { name, .. } => {
                write!(f, "{}", name)
            }
            Value::StructType { name, .. } => {
                write!(f, "{}", name)
            }
            Value::Tuple(values) => {
                write!(f, "(")?;
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Record(fields) => {
                write!(f, "{{")?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Char(c) => write!(f, "'{}'", c),
            Value::Return(val) => write!(f, "return {}", val),
            Value::Function { .. } => write!(f, "<function>"),
            Value::Break => write!(f, "break"),
            Value::Continue => write!(f, "continue"),
            Value::CompiledFunction { name, .. } => match name {
                Some(n) => write!(f, "<compiled function {}>", n),
                None => write!(f, "<compiled function>"),
            },
            Value::Module(module) => write!(f, "<module {}>", module.name),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::GcRef(_) => {
                write!(f, "<gc ref>")
            }
        }
    }
}
