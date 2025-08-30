use super::super::error::{Result, VeldError};
use super::super::interpreter::{BinaryOperator, Statement, TypeAnnotation};
use super::super::types::{NumericValue, Type};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct AnonymousStruct {
    name: String,
    fields: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    AnonymousStruct(AnonymousStruct),
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
    Tuple(Vec<Value>),

    Break,
    Continue,

    Module(crate::module::Module),
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
            Value::Tuple(values) => Type::Tuple(values.iter().map(|v| v.type_of()).collect()),
            Value::AnonymousStruct(anon) => Type::Struct {
                name: anon.name.clone(),
                fields: anon
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            },
        }
    }

    pub fn perform_binary_op(&self, op: &BinaryOperator, rhs: &Value) -> Result<Value> {
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
