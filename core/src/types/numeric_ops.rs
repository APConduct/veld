use crate::ast::BinaryOperator;
use crate::error::{Result, VeldError};
use crate::interpreter::Value;
use crate::types::numeric::{FloatValue, IntegerValue, NumericValue};

impl NumericValue {
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
            _ => Err(VeldError::RuntimeError(format!(
                "Unsupported numeric operation: {:?}",
                op
            ))),
        }
    }

    fn add(&self, rhs: &NumericValue) -> Result<NumericValue> {
        match (self, rhs) {
            (NumericValue::Integer(a), NumericValue::Integer(b)) => {
                match (a, b) {
                    (IntegerValue::I32(x), IntegerValue::I32(y)) => x
                        .checked_add(*y)
                        .map(|n| NumericValue::Integer(IntegerValue::I32(n)))
                        .ok_or_else(|| VeldError::RuntimeError("Integer overflow".into())),
                    // TODO: Add other integer combinations
                    _ => todo!("Implement other integer addition combinations"),
                }
            }
            (NumericValue::Float(a), NumericValue::Float(b)) => {
                match (a, b) {
                    (FloatValue::F64(x), FloatValue::F64(y)) => {
                        Ok(NumericValue::Float(FloatValue::F64(x + y)))
                    }
                    // TODO: Add other float combinations
                    _ => todo!("Implement other float addition combinations"),
                }
            }
            // Mixed operations promote to float
            (a, b) => Ok(NumericValue::Float(FloatValue::F64(
                a.clone().as_f64() + b.clone().as_f64(),
            ))),
        }
    }

    // TODO: Implement other operations (sub, mul, div, rem)
    fn sub(&self, rhs: &NumericValue) -> Result<NumericValue> {
        todo!("Implement subtraction")
    }

    fn mul(&self, rhs: &NumericValue) -> Result<NumericValue> {
        todo!("Implement multiplication")
    }

    fn div(&self, rhs: &NumericValue) -> Result<NumericValue> {
        if rhs.clone().is_zero() {
            return Err(VeldError::RuntimeError("Division by zero".into()));
        }
        todo!("Implement division")
    }

    fn rem(&self, rhs: &NumericValue) -> Result<NumericValue> {
        if rhs.clone().is_zero() {
            return Err(VeldError::RuntimeError("Division by zero".into()));
        }
        todo!("Implement remainder")
    }
}
