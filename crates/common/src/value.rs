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
            Value::CompiledFunction { chunk, arity, name } => {
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
