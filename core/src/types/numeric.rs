use crate::ast::BinaryOperator;
use crate::error::{Result, VeldError};
use crate::interpreter::Value;
use crate::types::Type;
use crate::types::TypeEnvironment;
use std::collections::HashMap;
use std::fmt;

pub struct NumericKinds;

impl NumericKinds {
    pub fn register(env: &mut TypeEnvironment) -> Result<()> {
        // Register the Number kind
        let mut number_methods = HashMap::new();
        number_methods.insert(
            "as_i64".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::I64),
            },
        );
        number_methods.insert(
            "as_f64".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::F64),
            },
        );

        env.add_kind(
            "Number",
            number_methods,
            HashMap::new(),
            vec!["Self".to_string()],
        );

        // Register numeric operator kinds
        Self::register_operator_kinds(env)?;

        // Auto-implement for all numeric types
        for ty in [
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::F32,
            Type::F64,
        ] {
            Self::register_numeric_impl(env, ty)?;
        }

        Ok(())
    }

    fn register_operator_kinds(env: &mut TypeEnvironment) -> Result<()> {
        // Add operator kinds
        let mut add_methods = HashMap::new();
        add_methods.insert(
            "add".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                    Type::TypeParam("Rhs".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Add",
            add_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        // TODO: Add similar registrations for Sub, Mul, Div, etc.

        Ok(())
    }

    fn register_numeric_impl(env: &mut TypeEnvironment, ty: Type) -> Result<()> {
        // Implement Number kind
        let mut methods = HashMap::new();
        methods.insert(
            "as_i64".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::I64),
            },
        );
        methods.insert(
            "as_f64".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::F64),
            },
        );

        env.add_implementation(&ty.to_string(), "Number", vec![], methods);

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntegerValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl IntegerValue {
    pub fn as_i64(self) -> Option<i64> {
        match self {
            IntegerValue::I8(n) => Some(n as i64),
            IntegerValue::I16(n) => Some(n as i64),
            IntegerValue::I32(n) => Some(n as i64),
            IntegerValue::I64(n) => Some(n),
            IntegerValue::U8(n) => Some(n as i64),
            IntegerValue::U16(n) => Some(n as i64),
            IntegerValue::U32(n) => Some(n as i64),
            IntegerValue::U64(n) => Some(n as i64),
        }
    }

    pub fn as_f64(self) -> f64 {
        match self {
            IntegerValue::I8(n) => n as f64,
            IntegerValue::I16(n) => n as f64,
            IntegerValue::I32(n) => n as f64,
            IntegerValue::I64(n) => n as f64,
            IntegerValue::U8(n) => n as f64,
            IntegerValue::U16(n) => n as f64,
            IntegerValue::U32(n) => n as f64,
            IntegerValue::U64(n) => n as f64,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatValue {
    F32(f32),
    F64(f64),
}

impl FloatValue {
    pub fn as_f64(self) -> f64 {
        match self {
            FloatValue::F32(f) => f as f64,
            FloatValue::F64(f) => f,
        }
    }
    pub fn as_i64(self) -> Option<i64> {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumericValue {
    Integer(IntegerValue),
    Float(FloatValue),
}

impl NumericValue {
    pub fn type_of(self) -> Type {
        match self {
            NumericValue::Integer(iv) => match iv {
                IntegerValue::I8(_) => Type::I8,
                IntegerValue::I16(_) => Type::I16,
                IntegerValue::I32(_) => Type::I32,
                IntegerValue::I64(_) => Type::I64,
                IntegerValue::U8(_) => Type::U8,
                IntegerValue::U16(_) => Type::U16,
                IntegerValue::U32(_) => Type::U32,
                IntegerValue::U64(_) => Type::U64,
            },
            NumericValue::Float(fv) => match fv {
                FloatValue::F32(_) => Type::F32,
                FloatValue::F64(_) => Type::F64,
            },
        }
    }

    pub fn as_i64(self) -> Option<i64> {
        match self {
            NumericValue::Integer(iv) => iv.as_i64(),
            NumericValue::Float(fv) => fv.as_i64(),
        }
    }

    pub fn as_f64(self) -> f64 {
        match self {
            NumericValue::Integer(iv) => iv.as_f64(),
            NumericValue::Float(fv) => fv.as_f64(),
        }
    }

    pub fn is_zero(self) -> bool {
        match self {
            NumericValue::Integer(iv) => iv.as_i64() == Some(0),
            NumericValue::Float(fv) => fv.as_f64() == 0.0,
        }
    }
}
