use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use veld_common::value::Value as InterpreterValue;

/// Runtime values used by the bytecode virtual machine
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BytecodeValue {
    // Primitive types
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Unit,

    // Collection types
    Array(Vec<BytecodeValue>),
    Tuple(Vec<BytecodeValue>),

    // Structured types
    Struct {
        type_name: String,
        fields: HashMap<String, BytecodeValue>,
    },
    Enum {
        type_name: String,
        variant: String,
        fields: Vec<BytecodeValue>,
    },

    // Function types
    Function {
        chunk_index: usize,
        arity: u8,
        upvalue_count: u8,
        name: Option<String>,
    },
    NativeFunction {
        name: String,
        arity: u8,
        #[serde(skip, default = "get_default_native_function")]
        function: fn(&[BytecodeValue]) -> Result<BytecodeValue, RuntimeError>,
    },
    Closure {
        function: Box<BytecodeValue>,
        upvalues: Vec<UpvalueRef>,
    },

    // Reference types
    Reference(Box<BytecodeValue>),
    MutableReference(Box<BytecodeValue>),

    // Iterator type
    Iterator {
        values: Vec<BytecodeValue>,
        position: usize,
    },

    // Module type
    Module {
        name: String,
        exports: HashMap<String, BytecodeValue>,
    },

    // Type information
    Type(TypeInfo),

    // Special control flow values
    Return(Box<BytecodeValue>),
    Break,
    Continue,
    Exception(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UpvalueRef {
    pub index: usize,
    pub is_local: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeInfo {
    pub name: String,
    pub size: usize,
    pub alignment: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    TypeError { expected: String, actual: String },
    IndexOutOfBounds { index: i64, length: usize },
    FieldNotFound { field: String, type_name: String },
    VariantMismatch { expected: String, actual: String },
    DivisionByZero,
    StackOverflow,
    StackUnderflow,
    InvalidOperation { op: String, types: Vec<String> },
    UndefinedVariable(String),
    UndefinedFunction(String),
    ArityMismatch { expected: usize, actual: usize },
    InvalidCast { from: String, to: String },
    NullPointerAccess,
    MemoryError(String),
    ModuleNotFound(String),
    ImportError(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            RuntimeError::TypeError { expected, actual } => {
                write!(f, "Type error: expected {}, got {}", expected, actual)
            }
            RuntimeError::IndexOutOfBounds { index, length } => {
                write!(f, "Index {} out of bounds for length {}", index, length)
            }
            RuntimeError::FieldNotFound { field, type_name } => {
                write!(f, "Field '{}' not found in type '{}'", field, type_name)
            }
            RuntimeError::VariantMismatch { expected, actual } => {
                write!(f, "Variant mismatch: expected {}, got {}", expected, actual)
            }
            RuntimeError::DivisionByZero => write!(f, "Division by zero"),
            RuntimeError::StackOverflow => write!(f, "Stack overflow"),
            RuntimeError::StackUnderflow => write!(f, "Stack underflow"),
            RuntimeError::InvalidOperation { op, types } => {
                write!(
                    f,
                    "Invalid operation '{}' for types: {}",
                    op,
                    types.join(", ")
                )
            }
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            RuntimeError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            RuntimeError::ArityMismatch { expected, actual } => {
                write!(
                    f,
                    "Arity mismatch: expected {} arguments, got {}",
                    expected, actual
                )
            }
            RuntimeError::InvalidCast { from, to } => {
                write!(f, "Cannot cast from {} to {}", from, to)
            }
            RuntimeError::NullPointerAccess => write!(f, "Null pointer access"),
            RuntimeError::MemoryError(msg) => write!(f, "Memory error: {}", msg),
            RuntimeError::ModuleNotFound(name) => write!(f, "Module not found: {}", name),
            RuntimeError::ImportError(msg) => write!(f, "Import error: {}", msg),
        }
    }
}

impl std::error::Error for RuntimeError {}

impl Display for BytecodeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            BytecodeValue::Integer(i) => write!(f, "{}", i),
            BytecodeValue::Float(fl) => write!(f, "{}", fl),
            BytecodeValue::Boolean(b) => write!(f, "{}", b),
            BytecodeValue::Char(c) => write!(f, "'{}'", c),
            BytecodeValue::String(s) => write!(f, "\"{}\"", s),
            BytecodeValue::Unit => write!(f, "()"),
            BytecodeValue::Array(arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            BytecodeValue::Tuple(tuple) => {
                write!(f, "(")?;
                for (i, val) in tuple.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }
            BytecodeValue::Struct { type_name, fields } => {
                write!(f, "{} {{ ", type_name)?;
                for (i, (key, val)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, val)?;
                }
                write!(f, " }}")
            }
            BytecodeValue::Enum {
                type_name,
                variant,
                fields,
            } => {
                write!(f, "{}::{}", type_name, variant)?;
                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", field)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            BytecodeValue::Function { name, arity, .. } => match name {
                Some(n) => write!(f, "<fn {} arity={}>", n, arity),
                None => write!(f, "<anonymous fn arity={}>", arity),
            },
            BytecodeValue::NativeFunction { name, arity, .. } => {
                write!(f, "<native fn {} arity={}>", name, arity)
            }
            BytecodeValue::Closure { function, upvalues } => {
                write!(f, "<closure upvals={} {}>", upvalues.len(), function)
            }
            BytecodeValue::Reference(val) => write!(f, "&{}", val),
            BytecodeValue::MutableReference(val) => write!(f, "&mut {}", val),
            BytecodeValue::Iterator { position, values } => {
                write!(f, "<iterator {}/{}>", position, values.len())
            }
            BytecodeValue::Module { name, .. } => write!(f, "<module {}>", name),
            BytecodeValue::Type(type_info) => write!(f, "<type {}>", type_info.name),
            BytecodeValue::Return(val) => write!(f, "return {}", val),
            BytecodeValue::Break => write!(f, "break"),
            BytecodeValue::Continue => write!(f, "continue"),
            BytecodeValue::Exception(msg) => write!(f, "exception: {}", msg),
        }
    }
}

impl BytecodeValue {
    /// Get the type name of this value
    pub fn type_name(&self) -> &str {
        match self {
            BytecodeValue::Integer(_) => "i64",
            BytecodeValue::Float(_) => "f64",
            BytecodeValue::Boolean(_) => "bool",
            BytecodeValue::Char(_) => "char",
            BytecodeValue::String(_) => "String",
            BytecodeValue::Unit => "()",
            BytecodeValue::Array(_) => "Array",
            BytecodeValue::Tuple(_) => "Tuple",
            BytecodeValue::Struct { type_name, .. } => type_name,
            BytecodeValue::Enum { type_name, .. } => type_name,
            BytecodeValue::Function { .. } => "Function",
            BytecodeValue::NativeFunction { .. } => "NativeFunction",
            BytecodeValue::Closure { .. } => "Closure",
            BytecodeValue::Reference(_) => "Reference",
            BytecodeValue::MutableReference(_) => "MutableReference",
            BytecodeValue::Iterator { .. } => "Iterator",
            BytecodeValue::Module { .. } => "Module",
            BytecodeValue::Type(_) => "Type",
            BytecodeValue::Return(_) => "Return",
            BytecodeValue::Break => "Break",
            BytecodeValue::Continue => "Continue",
            BytecodeValue::Exception(_) => "Exception",
        }
    }

    /// Check if this value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            BytecodeValue::Boolean(b) => *b,
            BytecodeValue::Integer(i) => *i != 0,
            BytecodeValue::Float(f) => *f != 0.0,
            BytecodeValue::String(s) => !s.is_empty(),
            BytecodeValue::Array(arr) => !arr.is_empty(),
            BytecodeValue::Unit => false,
            _ => true,
        }
    }

    /// Check if this value is falsy
    pub fn is_falsy(&self) -> bool {
        !self.is_truthy()
    }

    /// Convert to a boolean value
    pub fn to_bool(&self) -> BytecodeValue {
        BytecodeValue::Boolean(self.is_truthy())
    }

    /// Check if two values are equal
    pub fn equals(&self, other: &BytecodeValue) -> bool {
        match (self, other) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => a == b,
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => a == b,
            (BytecodeValue::Boolean(a), BytecodeValue::Boolean(b)) => a == b,
            (BytecodeValue::Char(a), BytecodeValue::Char(b)) => a == b,
            (BytecodeValue::String(a), BytecodeValue::String(b)) => a == b,
            (BytecodeValue::Unit, BytecodeValue::Unit) => true,
            (BytecodeValue::Array(a), BytecodeValue::Array(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.equals(y))
            }
            (BytecodeValue::Tuple(a), BytecodeValue::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.equals(y))
            }
            _ => false,
        }
    }

    /// Try to convert interpreter value to bytecode value
    pub fn from_interpreter_value(value: &InterpreterValue) -> Result<BytecodeValue, RuntimeError> {
        match value {
            InterpreterValue::Integer(i) => Ok(BytecodeValue::Integer(*i)),
            InterpreterValue::Float(f) => Ok(BytecodeValue::Float(*f)),
            InterpreterValue::String(s) => Ok(BytecodeValue::String(s.clone())),
            InterpreterValue::Boolean(b) => Ok(BytecodeValue::Boolean(*b)),
            InterpreterValue::Char(c) => Ok(BytecodeValue::Char(*c)),
            InterpreterValue::Unit => Ok(BytecodeValue::Unit),
            InterpreterValue::Array(arr) => {
                let mut result = Vec::new();
                for val in arr {
                    result.push(Self::from_interpreter_value(val)?);
                }
                Ok(BytecodeValue::Array(result))
            }
            InterpreterValue::Tuple(tuple) => {
                let mut result = Vec::new();
                for val in tuple {
                    result.push(Self::from_interpreter_value(val)?);
                }
                Ok(BytecodeValue::Tuple(result))
            }
            InterpreterValue::Struct { name, fields } => {
                let mut bytecode_fields = HashMap::new();
                for (key, val) in fields {
                    bytecode_fields.insert(key.clone(), Self::from_interpreter_value(val)?);
                }
                Ok(BytecodeValue::Struct {
                    type_name: name.clone(),
                    fields: bytecode_fields,
                })
            }
            _ => Err(RuntimeError::InvalidCast {
                from: "InterpreterValue".to_string(),
                to: "BytecodeValue".to_string(),
            }),
        }
    }

    /// Try to convert bytecode value to interpreter value
    pub fn to_interpreter_value(&self) -> Result<InterpreterValue, RuntimeError> {
        match self {
            BytecodeValue::Integer(i) => Ok(InterpreterValue::Integer(*i)),
            BytecodeValue::Float(f) => Ok(InterpreterValue::Float(*f)),
            BytecodeValue::String(s) => Ok(InterpreterValue::String(s.clone())),
            BytecodeValue::Boolean(b) => Ok(InterpreterValue::Boolean(*b)),
            BytecodeValue::Char(c) => Ok(InterpreterValue::Char(*c)),
            BytecodeValue::Unit => Ok(InterpreterValue::Unit),
            BytecodeValue::Array(arr) => {
                let mut result = Vec::new();
                for val in arr {
                    result.push(val.to_interpreter_value()?);
                }
                Ok(InterpreterValue::Array(result))
            }
            BytecodeValue::Tuple(tuple) => {
                let mut result = Vec::new();
                for val in tuple {
                    result.push(val.to_interpreter_value()?);
                }
                Ok(InterpreterValue::Tuple(result))
            }
            BytecodeValue::Struct { type_name, fields } => {
                let mut interpreter_fields = HashMap::new();
                for (key, val) in fields {
                    interpreter_fields.insert(key.clone(), val.to_interpreter_value()?);
                }
                Ok(InterpreterValue::Struct {
                    name: type_name.clone(),
                    fields: interpreter_fields,
                })
            }
            _ => Err(RuntimeError::InvalidCast {
                from: "BytecodeValue".to_string(),
                to: "InterpreterValue".to_string(),
            }),
        }
    }
}

/// Default native function for deserialization
fn get_default_native_function() -> fn(&[BytecodeValue]) -> Result<BytecodeValue, RuntimeError> {
    |_args: &[BytecodeValue]| -> Result<BytecodeValue, RuntimeError> {
        Err(RuntimeError::UndefinedFunction(
            "default native function".to_string(),
        ))
    }
}
