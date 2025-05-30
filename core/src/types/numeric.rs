use crate::ast::BinaryOperator;
use crate::error::{Result, VeldError};
use crate::interpreter::Value;
use crate::types::Type;
use std::fmt;

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

#[derive(Debug, Clone, PartialEq)]
pub enum FloatValue {
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumericValue {
    Integer(IntegerValue),
    Float(FloatValue),
}
