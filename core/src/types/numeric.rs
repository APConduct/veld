use crate::error::Result;
use crate::types::Type;
use crate::types::TypeEnvironment;
use std::collections::HashMap;

pub struct NumericKinds;

impl NumericKinds {
    pub fn register(env: &mut TypeEnvironment) -> Result<()> {
        // Register the Number kind with comprehensive conversion methods
        let mut number_methods = HashMap::new();
        
        // Basic type conversion methods
        number_methods.insert(
            "as_i8".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::I8),
            },
        );
        number_methods.insert(
            "as_i16".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::I16),
            },
        );
        number_methods.insert(
            "as_i32".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::I32),
            },
        );
        number_methods.insert(
            "as_i64".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::I64),
            },
        );
        number_methods.insert(
            "as_u8".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::U8),
            },
        );
        number_methods.insert(
            "as_u16".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::U16),
            },
        );
        number_methods.insert(
            "as_u32".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::U32),
            },
        );
        number_methods.insert(
            "as_u64".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::U64),
            },
        );
        number_methods.insert(
            "as_f32".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::F32),
            },
        );
        number_methods.insert(
            "as_f64".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::F64),
            },
        );
        
        // Utility methods
        number_methods.insert(
            "is_integer".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::Bool),
            },
        );
        number_methods.insert(
            "is_float".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::Bool),
            },
        );
        number_methods.insert(
            "is_signed".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::Bool),
            },
        );
        number_methods.insert(
            "is_unsigned".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::Bool),
            },
        );
        number_methods.insert(
            "type_name".to_string(),
            Type::Function {
                params: vec![Type::KindSelf("Number".to_string())],
                return_type: Box::new(Type::String),
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

        // Subtraction
        let mut sub_methods = HashMap::new();
        sub_methods.insert(
            "sub".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                    Type::TypeParam("Rhs".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Sub",
            sub_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );
        
        // Multiplication
        let mut mul_methods = HashMap::new();
        mul_methods.insert(
            "mul".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                    Type::TypeParam("Rhs".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Mul",
            mul_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );
        
        // Division
        let mut div_methods = HashMap::new();
        div_methods.insert(
            "div".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                    Type::TypeParam("Rhs".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Div",
            div_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );
        
        // Remainder/Modulo
        let mut rem_methods = HashMap::new();
        rem_methods.insert(
            "rem".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                    Type::TypeParam("Rhs".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Rem",
            rem_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );
        
        // Exponent
        let mut pow_methods = HashMap::new();
        pow_methods.insert(
            "pow".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                    Type::TypeParam("Rhs".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Pow",
            pow_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );
        
        // Negation (unary minus)
        let mut neg_methods = HashMap::new();
        neg_methods.insert(
            "neg".to_string(),
            Type::Function {
                params: vec![
                    Type::KindSelf("Self".to_string()),
                ],
                return_type: Box::new(Type::TypeParam("Output".to_string())),
            },
        );
        env.add_kind(
            "Neg",
            neg_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Output".to_string()],
        );

        Ok(())
    }

    fn register_numeric_impl(env: &mut TypeEnvironment, ty: Type) -> Result<()> {
        // Implement Number kind with all conversion methods
        let mut methods = HashMap::new();
        
        // Basic type conversion methods
        methods.insert(
            "as_i8".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::I8),
            },
        );
        methods.insert(
            "as_i16".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::I16),
            },
        );
        methods.insert(
            "as_i32".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::I32),
            },
        );
        methods.insert(
            "as_i64".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::I64),
            },
        );
        methods.insert(
            "as_u8".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::U8),
            },
        );
        methods.insert(
            "as_u16".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::U16),
            },
        );
        methods.insert(
            "as_u32".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::U32),
            },
        );
        methods.insert(
            "as_u64".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::U64),
            },
        );
        methods.insert(
            "as_f32".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::F32),
            },
        );
        methods.insert(
            "as_f64".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::F64),
            },
        );
        
        // Utility methods
        methods.insert(
            "is_integer".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::Bool),
            },
        );
        methods.insert(
            "is_float".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::Bool),
            },
        );
        methods.insert(
            "is_signed".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::Bool),
            },
        );
        methods.insert(
            "is_unsigned".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::Bool),
            },
        );
        methods.insert(
            "type_name".to_string(),
            Type::Function {
                params: vec![ty.clone()],
                return_type: Box::new(Type::String),
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
    
    pub fn is_integer(&self) -> bool {
        matches!(self, NumericValue::Integer(_))
    }
    
    pub fn is_float(&self) -> bool {
        matches!(self, NumericValue::Float(_))
    }
    
    pub fn is_signed(&self) -> bool {
        match self {
            NumericValue::Integer(iv) => matches!(
                iv,
                IntegerValue::I8(_) | IntegerValue::I16(_) | IntegerValue::I32(_) | IntegerValue::I64(_)
            ),
            NumericValue::Float(_) => true, // Floats are always signed
        }
    }
    
    pub fn is_unsigned(&self) -> bool {
        match self {
            NumericValue::Integer(iv) => matches!(
                iv,
                IntegerValue::U8(_) | IntegerValue::U16(_) | IntegerValue::U32(_) | IntegerValue::U64(_)
            ),
            NumericValue::Float(_) => false, // Floats are never unsigned
        }
    }
    
    pub fn type_name(&self) -> String {
        match self {
            NumericValue::Integer(iv) => match iv {
                IntegerValue::I8(_) => "i8".to_string(),
                IntegerValue::I16(_) => "i16".to_string(),
                IntegerValue::I32(_) => "i32".to_string(),
                IntegerValue::I64(_) => "i64".to_string(),
                IntegerValue::U8(_) => "u8".to_string(),
                IntegerValue::U16(_) => "u16".to_string(),
                IntegerValue::U32(_) => "u32".to_string(),
                IntegerValue::U64(_) => "u64".to_string(),
            },
            NumericValue::Float(fv) => match fv {
                FloatValue::F32(_) => "f32".to_string(),
                FloatValue::F64(_) => "f64".to_string(),
            },
        }
    }
}
