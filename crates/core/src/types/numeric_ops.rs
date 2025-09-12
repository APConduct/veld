use crate::types::numeric::{FloatValue, IntegerValue, NumericValue};
use veld_ast::BinaryOperator;
use veld_error::{Result, VeldError};

impl NumericValue {
    /// Performs the specified binary operation with the given right-hand side numeric value.
    ///
    /// # Arguments
    /// * `op` - The binary operator to perform
    /// * `rhs` - The right-hand side numeric value to operate with
    ///
    /// # Returns
    /// The result of the operation as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if the operation is unsupported or results in an error (like overflow)
    pub fn perform_operation(
        &self,
        op: &BinaryOperator,
        rhs: &NumericValue,
    ) -> Result<NumericValue> {
        match op {
            BinaryOperator::Add => self.add(rhs),
            BinaryOperator::Subtract => self.sub(rhs),
            BinaryOperator::Multiply => self.mul(rhs),
            BinaryOperator::Divide => self.div(rhs),
            BinaryOperator::Modulo => self.rem(rhs),
            BinaryOperator::Exponent => self.pow(rhs),
            _ => Err(VeldError::RuntimeError(format!(
                "Unsupported numeric operation: {:?}",
                op
            ))),
        }
    }

    /// Adds a numeric value to this numeric value.
    ///
    /// # Arguments
    /// * `rhs` - The right-hand side numeric value to add
    ///
    /// # Returns
    /// The result of the addition as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if the addition results in overflow or if the types are incompatible
    fn add(&self, rhs: &NumericValue) -> Result<NumericValue> {
        let (left, right) = Self::coerce_numeric_pair(self, rhs)?;
        match (left, right) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => match (a.clone(), b.clone()) {
                (IntegerValue::I8(x), IntegerValue::I8(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::I16(x), IntegerValue::I16(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::I32(x), IntegerValue::I32(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::I64(x), IntegerValue::I64(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U8(x), IntegerValue::U8(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U16(x), IntegerValue::U16(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U32(x), IntegerValue::U32(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U64(x), IntegerValue::U64(y)) => x
                    .checked_add(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                // TODO: Handle mixed integer types
                // This will promote smaller types to the larger type of the two
                _ => Ok(NumericValue::Integer(IntegerValue::I64(
                    a.clone().as_i64().unwrap() + b.clone().as_i64().unwrap(),
                ))),
            },
            (NumericValue::Float(a), NumericValue::Float(b)) => match (a, b) {
                (FloatValue::F32(x), FloatValue::F32(y)) => {
                    Ok(NumericValue::Float(FloatValue::F32(x + y)))
                }
                (FloatValue::F64(x), FloatValue::F64(y)) => {
                    Ok(NumericValue::Float(FloatValue::F64(x + y)))
                }
                _ => unreachable!("Coercion should have handled this case"),
            },
            _ => unreachable!("Coercion should have handled this case"),
        }
    }

    /// Subtract a new numeric operation to the NumericValue (basic addition)
    ///
    /// # Arguments
    /// * `rhs` - The right-hand side numeric value to subtract from this numeric value.
    ///
    /// # Returns
    /// The result of the subtraction as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if the subtraction results in underflow or if the types are incompatible
    fn sub(&self, rhs: &NumericValue) -> Result<NumericValue> {
        let (left, right) = Self::coerce_numeric_pair(self, rhs)?;
        let result = match (left, right) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => match (a.clone(), b.clone()) {
                (IntegerValue::I8(x), IntegerValue::I8(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::I16(x), IntegerValue::I16(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::I32(x), IntegerValue::I32(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::I64(x), IntegerValue::I64(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::U8(x), IntegerValue::U8(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::U16(x), IntegerValue::U16(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::U32(x), IntegerValue::U32(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                (IntegerValue::U64(x), IntegerValue::U64(y)) => x
                    .checked_sub(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer underflow".into())),
                _ => Ok(NumericValue::Integer(IntegerValue::I64(
                    a.clone().as_i64().unwrap() - b.clone().as_i64().unwrap(),
                ))),
            },
            (NumericValue::Float(a), NumericValue::Float(b)) => match (a, b) {
                (FloatValue::F32(x), FloatValue::F32(y)) => {
                    Ok(NumericValue::Float(FloatValue::F32(x - y)))
                }
                (FloatValue::F64(x), FloatValue::F64(y)) => {
                    Ok(NumericValue::Float(FloatValue::F64(x - y)))
                }
                _ => unreachable!("Coercion should have handled this case"),
            },
            _ => unreachable!("Coercion should have handled this case"),
        };
        tracing::debug!("NumericValue::sub result = {:?}", result);
        result
    }

    /// Multiplies this numeric value by another numeric value.
    ///
    /// # Arguments
    /// * `rhs` - The right-hand side numeric value to multiply with
    ///
    /// # Returns
    /// The result of the multiplication as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if the multiplication results in overflow or if the types are incompatible
    fn mul(&self, rhs: &NumericValue) -> Result<NumericValue> {
        if rhs.clone().is_zero() {
            return Ok(NumericValue::Integer(IntegerValue::I64(0)));
        }
        if self.clone().is_zero() {
            return Ok(NumericValue::Integer(IntegerValue::I64(0)));
        }
        let (left, right) = Self::coerce_numeric_pair(self, rhs)?;
        match (left, right) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => match (a.clone(), b.clone()) {
                (IntegerValue::I8(x), IntegerValue::I8(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::I16(x), IntegerValue::I16(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::I32(x), IntegerValue::I32(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::I64(x), IntegerValue::I64(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U8(x), IntegerValue::U8(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U16(x), IntegerValue::U16(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U32(x), IntegerValue::U32(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                (IntegerValue::U64(x), IntegerValue::U64(y)) => x
                    .checked_mul(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                _ => Ok(NumericValue::Integer(IntegerValue::I64(
                    a.clone().as_i64().unwrap() * b.clone().as_i64().unwrap(),
                ))),
            },
            (NumericValue::Float(a), NumericValue::Float(b)) => match (a, b) {
                (FloatValue::F32(x), FloatValue::F32(y)) => {
                    Ok(NumericValue::Float(FloatValue::F32(x * y)))
                }
                (FloatValue::F64(x), FloatValue::F64(y)) => {
                    Ok(NumericValue::Float(FloatValue::F64(x * y)))
                }
                _ => unreachable!("Coercion should have handled this case"),
            },
            (a, b) => Ok(NumericValue::Float(FloatValue::F64(
                a.clone().as_f64() * b.clone().as_f64(),
            ))),
        }
    }

    /// Divides this numeric value by another numeric value.
    ///
    /// # Arguments
    /// * `rhs` - The right-hand side numeric value to divide by
    ///
    /// # Returns
    /// The result of the division as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if division by zero occurs or if the operation results in an error
    fn div(&self, rhs: &NumericValue) -> Result<NumericValue> {
        if rhs.clone().is_zero() {
            return Err(VeldError::RuntimeError("Division by zero".into()));
        }
        if self.clone().is_zero() {
            return Ok(NumericValue::Integer(IntegerValue::I64(0)));
        }
        let (left, right) = Self::coerce_numeric_pair(self, rhs)?;
        match (left, right) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => match (a.clone(), b.clone()) {
                (IntegerValue::I8(x), IntegerValue::I8(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::I16(x), IntegerValue::I16(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::I32(x), IntegerValue::I32(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::I64(x), IntegerValue::I64(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::U8(x), IntegerValue::U8(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::U16(x), IntegerValue::U16(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::U32(x), IntegerValue::U32(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                (IntegerValue::U64(x), IntegerValue::U64(y)) => x
                    .checked_div(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer division error".into())),
                _ => Ok(NumericValue::Integer(IntegerValue::I64(
                    a.clone().as_i64().unwrap() / b.clone().as_i64().unwrap(),
                ))),
            },
            (NumericValue::Float(a), NumericValue::Float(b)) => match (a, b) {
                (FloatValue::F32(x), FloatValue::F32(y)) => {
                    Ok(NumericValue::Float(FloatValue::F32(x / y)))
                }
                (FloatValue::F64(x), FloatValue::F64(y)) => {
                    Ok(NumericValue::Float(FloatValue::F64(x / y)))
                }
                _ => unreachable!("Coercion should have handled this case"),
            },
            (a, b) => Ok(NumericValue::Float(FloatValue::F64(
                a.clone().as_f64() / b.clone().as_f64(),
            ))),
        }
    }

    /// Calculates the remainder when dividing this numeric value by another (different from formal euclidean modulo).
    ///
    /// # Arguments
    /// * `rhs` - The right-hand side numeric value to operate with
    ///
    /// # Returns
    /// The result of the remainder operation as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if division by zero occurs or if the operation results in an error
    fn rem(&self, rhs: &NumericValue) -> Result<NumericValue> {
        if rhs.clone().is_zero() {
            return Err(VeldError::RuntimeError("Division by zero".into()));
        }
        if self.clone().is_zero() {
            return Ok(NumericValue::Integer(IntegerValue::I64(0)));
        }
        let (left, right) = Self::coerce_numeric_pair(self, rhs)?;
        match (left, right) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => match (a.clone(), b.clone()) {
                (IntegerValue::I8(x), IntegerValue::I8(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::I16(x), IntegerValue::I16(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::I32(x), IntegerValue::I32(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::I64(x), IntegerValue::I64(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::I64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::U8(x), IntegerValue::U8(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U8(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::U16(x), IntegerValue::U16(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U16(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::U32(x), IntegerValue::U32(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U32(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                (IntegerValue::U64(x), IntegerValue::U64(y)) => x
                    .checked_rem(y)
                    .map(|n| NumericValue::Integer(IntegerValue::U64(n)))
                    .ok_or_else(|| VeldError::RuntimeError("Integer modulo error".into())),
                _ => Ok(NumericValue::Integer(IntegerValue::I64(
                    a.clone().as_i64().unwrap() % b.clone().as_i64().unwrap(),
                ))),
            },
            (NumericValue::Float(a), NumericValue::Float(b)) => match (a, b) {
                (FloatValue::F32(x), FloatValue::F32(y)) => {
                    Ok(NumericValue::Float(FloatValue::F32(x % y)))
                }
                (FloatValue::F64(x), FloatValue::F64(y)) => {
                    Ok(NumericValue::Float(FloatValue::F64(x % y)))
                }
                _ => unreachable!("Coercion should have handled this case"),
            },
            (a, b) => Ok(NumericValue::Float(FloatValue::F64(
                a.clone().as_f64() % b.clone().as_f64(),
            ))),
        }
    }

    /// Raises this numeric value to the power of another numeric value.
    ///
    /// # Arguments
    /// * `rhs` - The right-hand side numeric value to use as the exponent
    ///
    /// # Returns
    /// The result of the exponentiation as a `NumericValue`
    ///
    /// # Errors
    /// Returns an error if the operation results in an error or if the types are incompatible
    fn pow(&self, rhs: &NumericValue) -> Result<NumericValue> {
        if rhs.clone().is_zero() {
            return Ok(NumericValue::Integer(IntegerValue::I64(1)));
        }
        if self.clone().is_zero() {
            return Ok(NumericValue::Integer(IntegerValue::I64(0)));
        }
        let (left, right) = Self::coerce_numeric_pair(self, rhs)?;
        match (left, right) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => {
                let base = a.clone().as_f64();
                let exp = b.clone().as_f64();
                Ok(NumericValue::Float(FloatValue::F64(base.powf(exp))))
            }
            (NumericValue::Float(a), NumericValue::Float(b)) => Ok(NumericValue::Float(
                FloatValue::F64(a.clone().as_f64().powf(b.clone().as_f64())),
            )),
            (a, b) => Ok(NumericValue::Float(FloatValue::F64(
                a.clone().as_f64().powf(b.clone().as_f64()),
            ))),
        }
    }

    /// Convert a NumericValue to an 8-bit signed integer (i8)
    ///
    /// # Returns
    /// Result containing the NumericValue as i8 or an error if conversion fails.
    pub fn to_i8(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= i8::MIN as i64 && n <= i8::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::I8(n as i8)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for i8".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= i8::MIN as f64 && val <= i8::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::I8(val as i8)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for i8".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to an 8-bit unsigned integer (u8)
    ///
    /// # Returns
    /// Result containing the NumericValue as u8 or an error if conversion fails.
    pub fn to_u8(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= 0 && n <= u8::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::U8(n as u8)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for u8".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= 0.0 && val <= u8::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::U8(val as u8)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for u8".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 16-bit signed integer (i16)
    ///
    /// # Returns
    /// Result containing the NumericValue as i16 or an error if conversion fails.
    pub fn to_i16(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= i16::MIN as i64 && n <= i16::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::I16(n as i16)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for i16".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= i16::MIN as f64 && val <= i16::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::I16(val as i16)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for i16".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 16-bit unsigned integer (u16)
    ///
    /// # Returns
    /// Result containing the NumericValue as u16 or an error if conversion fails.
    pub fn to_u16(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= 0 && n <= u16::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::U16(n as u16)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for u16".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= 0.0 && val <= u16::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::U16(val as u16)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for u16".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 32-bit signed integer (i32)
    ///
    /// # Returns
    /// Result containing the NumericValue as i32 or an error if conversion fails.
    pub fn to_i32(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= i32::MIN as i64 && n <= i32::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::I32(n as i32)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for i32".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= i32::MIN as f64 && val <= i32::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::I32(val as i32)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for i32".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 32-bit unsigned integer (u32)
    ///
    /// # Returns
    /// Result containing the NumericValue as u32 or an error if conversion fails.
    pub fn to_u32(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= 0 && n <= u32::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::U32(n as u32)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for u32".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= 0.0 && val <= u32::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::U32(val as u32)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for u32".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 64-bit signed integer (i64)
    ///
    /// # Returns
    /// Result containing the NumericValue as i64 or an error if conversion fails.
    pub fn to_i64(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => Ok(NumericValue::Integer(IntegerValue::I64(
                i.clone().as_i64().unwrap_or(0),
            ))),
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= i64::MIN as f64 && val <= i64::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::I64(val as i64)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for i64".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 64-bit unsigned integer (u64)
    ///
    /// # Returns
    /// Result containing the NumericValue as u64 or an error if conversion fails.
    pub fn to_u64(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => match i.clone().as_i64().unwrap() {
                n if n >= 0 && n <= u64::MAX as i64 => {
                    Ok(NumericValue::Integer(IntegerValue::U64(n as u64)))
                }
                _ => Err(VeldError::RuntimeError("Value out of range for u64".into())),
            },
            NumericValue::Float(f) => {
                let val = f.clone().as_f64();
                if val.is_finite() && val >= 0.0 && val <= u64::MAX as f64 {
                    Ok(NumericValue::Integer(IntegerValue::U64(val as u64)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for u64".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 32-bit floating point number (f32)
    ///
    /// # Returns
    /// Result containing the NumericValue as f32 or an error if conversion fails.
    pub fn to_f32(&self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => {
                let val = i.clone().as_i64().unwrap() as f32;
                Ok(NumericValue::Float(FloatValue::F32(val)))
            }
            NumericValue::Float(f) => {
                let val = f.clone().as_f64() as f32;
                if val.is_finite() {
                    Ok(NumericValue::Float(FloatValue::F32(val)))
                } else {
                    Err(VeldError::RuntimeError("Value out of range for f32".into()))
                }
            }
        }
    }

    /// Convert a NumericValue to a 64-bit floating point number (f64)
    ///
    /// # Returns
    /// Result containing the NumericValue as f64 or an error if conversion fails.
    pub fn to_f64(self) -> Result<NumericValue> {
        match self {
            NumericValue::Integer(i) => Ok(NumericValue::Float(FloatValue::F64(
                i.as_i64().unwrap() as f64,
            ))),
            NumericValue::Float(f) => Ok(NumericValue::Float(FloatValue::F64(f.as_f64()))),
        }
    }

    /// Coerce a pair of NumericValues to a pair of NumericValues of the same type
    ///
    /// # Arguments
    /// * `left` - The first NumericValue
    /// * `right` - The second NumericValue
    ///
    /// # Returns
    /// Result containing the coerced pair of NumericValues
    fn coerce_numeric_pair(
        left: &NumericValue,
        right: &NumericValue,
    ) -> Result<(NumericValue, NumericValue)> {
        match (left, right) {
            // Same type, no coercion needed
            (NumericValue::Integer(a), NumericValue::Integer(b))
                if std::mem::discriminant(a) == std::mem::discriminant(b) =>
            {
                Ok((left.clone(), right.clone()))
            }
            (NumericValue::Float(a), NumericValue::Float(b))
                if std::mem::discriminant(a) == std::mem::discriminant(b) =>
            {
                Ok((left.clone(), right.clone()))
            }

            // Mixed float types - promote to f64
            (NumericValue::Float(_), NumericValue::Float(_)) => Ok((
                NumericValue::Float(FloatValue::F64(left.clone().as_f64())),
                NumericValue::Float(FloatValue::F64(right.clone().as_f64())),
            )),

            // Mixed integer types - promote larger type
            (NumericValue::Integer(a), NumericValue::Integer(b)) => {
                match (a, b) {
                    // If either is signed, coerce to signed
                    (IntegerValue::I8(_), IntegerValue::U8(_))
                    | (IntegerValue::U8(_), IntegerValue::I8(_)) => Ok((
                        NumericValue::Integer(IntegerValue::I16(
                            left.clone().as_i64().unwrap() as i16
                        )),
                        NumericValue::Integer(IntegerValue::I16(
                            right.clone().as_i64().unwrap() as i16
                        )),
                    )),
                    (IntegerValue::I16(_), IntegerValue::U16(_))
                    | (IntegerValue::U16(_), IntegerValue::I16(_)) => Ok((
                        NumericValue::Integer(IntegerValue::I32(
                            left.clone().as_i64().unwrap() as i32
                        )),
                        NumericValue::Integer(IntegerValue::I32(
                            right.clone().as_i64().unwrap() as i32
                        )),
                    )),
                    (IntegerValue::I32(_), IntegerValue::U32(_))
                    | (IntegerValue::U32(_), IntegerValue::I32(_)) => Ok((
                        NumericValue::Integer(IntegerValue::I64(left.clone().as_i64().unwrap())),
                        NumericValue::Integer(IntegerValue::I64(right.clone().as_i64().unwrap())),
                    )),

                    // Unsigned types coerce to the larger unsigned type
                    (IntegerValue::U8(_), IntegerValue::U16(_))
                    | (IntegerValue::U16(_), IntegerValue::U8(_)) => Ok((
                        NumericValue::Integer(IntegerValue::U16(
                            left.clone().as_i64().unwrap() as u16
                        )),
                        NumericValue::Integer(IntegerValue::U16(
                            right.clone().as_i64().unwrap() as u16
                        )),
                    )),
                    (IntegerValue::U16(_), IntegerValue::U32(_))
                    | (IntegerValue::U32(_), IntegerValue::U16(_)) => Ok((
                        NumericValue::Integer(IntegerValue::U32(
                            left.clone().as_i64().unwrap() as u32
                        )),
                        NumericValue::Integer(IntegerValue::U32(
                            right.clone().as_i64().unwrap() as u32
                        )),
                    )),
                    (IntegerValue::U32(_), IntegerValue::U64(_))
                    | (IntegerValue::U64(_), IntegerValue::U32(_)) => Ok((
                        NumericValue::Integer(IntegerValue::U64(
                            left.clone().as_i64().unwrap() as u64
                        )),
                        NumericValue::Integer(IntegerValue::U64(
                            right.clone().as_i64().unwrap() as u64
                        )),
                    )),

                    // Signed types coerce to the larger signed type
                    (IntegerValue::I8(_), IntegerValue::I16(_))
                    | (IntegerValue::I16(_), IntegerValue::I8(_)) => Ok((
                        NumericValue::Integer(IntegerValue::I16(
                            left.clone().as_i64().unwrap() as i16
                        )),
                        NumericValue::Integer(IntegerValue::I16(
                            right.clone().as_i64().unwrap() as i16
                        )),
                    )),
                    (IntegerValue::I16(_), IntegerValue::I32(_))
                    | (IntegerValue::I32(_), IntegerValue::I16(_)) => Ok((
                        NumericValue::Integer(IntegerValue::I32(
                            left.clone().as_i64().unwrap() as i32
                        )),
                        NumericValue::Integer(IntegerValue::I32(
                            right.clone().as_i64().unwrap() as i32
                        )),
                    )),
                    (IntegerValue::I32(_), IntegerValue::I64(_))
                    | (IntegerValue::I64(_), IntegerValue::I32(_)) => Ok((
                        NumericValue::Integer(IntegerValue::I64(left.clone().as_i64().unwrap())),
                        NumericValue::Integer(IntegerValue::I64(right.clone().as_i64().unwrap())),
                    )),

                    // Default to i64 for mixed types
                    _ => Ok((
                        NumericValue::Integer(IntegerValue::I64(left.clone().as_i64().unwrap())),
                        NumericValue::Integer(IntegerValue::I64(right.clone().as_i64().unwrap())),
                    )),
                }
            }

            // Integer and Float - coerce to float
            (NumericValue::Integer(_), NumericValue::Float(FloatValue::F32(_)))
            | (NumericValue::Float(FloatValue::F32(_)), NumericValue::Integer(_)) => Ok((
                NumericValue::Float(FloatValue::F32(left.clone().as_f64() as f32)),
                NumericValue::Float(FloatValue::F32(right.clone().as_f64() as f32)),
            )),
            (NumericValue::Integer(_), NumericValue::Float(_))
            | (NumericValue::Float(_), NumericValue::Integer(_)) => Ok((
                NumericValue::Float(FloatValue::F64(left.clone().as_f64())),
                NumericValue::Float(FloatValue::F64(right.clone().as_f64())),
            )),
        }
    }
}
