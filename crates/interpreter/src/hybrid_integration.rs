//! Integration layer for hybrid value system
//!
//! This module provides seamless integration between the new hybrid value system
//! and the existing interpreter, enabling gradual migration while maintaining
//! compatibility with existing code.

use std::collections::HashMap;
use std::convert::TryFrom;

use veld_common::ast::*;
use veld_common::value::Value as LegacyValue;
use veld_error::{Result, VeldError};

use crate::gc::{GarbageCollector, SafeGc};
use crate::hybrid_value::{
    HeapArray, HeapString, HeapStruct, HeapTuple, HybridValue, HybridValueBuilder,
};
use crate::interpreter::Interpreter;

/// Hybrid interpreter that can work with both legacy and hybrid values
pub struct HybridInterpreter {
    /// The underlying interpreter
    pub interpreter: Interpreter,

    /// Garbage collector for hybrid values
    pub gc: SafeGc,

    /// Value builder for creating optimized hybrid values
    pub builder: HybridValueBuilder,

    /// Migration mode - when true, automatically converts legacy values
    pub migration_mode: bool,
}

impl HybridInterpreter {
    /// Create a new hybrid interpreter
    pub fn new(working_dir: &str) -> Result<Self> {
        let interpreter = Interpreter::new(working_dir);
        let gc_inner = GarbageCollector::new();
        let gc = SafeGc::new(gc_inner);
        let builder = HybridValueBuilder::new(gc.clone());

        Ok(Self {
            interpreter,
            gc,
            builder,
            migration_mode: true,
        })
    }

    /// Enable or disable automatic migration from legacy values
    pub fn set_migration_mode(&mut self, enabled: bool) {
        self.migration_mode = enabled;
    }

    /// Execute a statement and return a hybrid value
    pub fn execute_hybrid(&mut self, statement: &Statement) -> Result<HybridValue> {
        // First try to execute using the hybrid system
        match self.execute_statement_hybrid(statement) {
            Ok(value) => Ok(value),
            Err(_) if self.migration_mode => {
                // For now, return Unit since we can't access private methods
                // TODO: Implement proper fallback when interpreter methods are made public
                Ok(HybridValue::Unit)
            }
            Err(e) => Err(e),
        }
    }

    /// Execute a statement using pure hybrid value semantics
    fn execute_statement_hybrid(&mut self, statement: &Statement) -> Result<HybridValue> {
        match statement {
            Statement::ExprStatement(expr) => self.evaluate_expression_hybrid(expr),

            Statement::VariableDeclaration { name, value, .. } => {
                let hybrid_value = self.evaluate_expression_hybrid(value)?;
                // TODO: Store in interpreter's variable scope when methods are public
                // For now, just return the value
                Ok(hybrid_value)
            }

            // Assignment is not a separate statement type in the actual AST
            // It would be handled as an ExprStatement with an assignment expression
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.evaluate_expression_hybrid(condition)?;
                let condition_bool = match cond_value {
                    HybridValue::Boolean(b) => b,
                    _ => {
                        return Err(VeldError::RuntimeError(
                            "If condition must be a boolean".to_string(),
                        ));
                    }
                };

                if condition_bool {
                    self.execute_block_hybrid(then_branch)
                } else if let Some(else_stmts) = else_branch {
                    self.execute_block_hybrid(else_stmts)
                } else {
                    Ok(HybridValue::Unit)
                }
            }

            Statement::BlockScope { body } => self.execute_block_hybrid(body),

            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.evaluate_expression_hybrid(expr)
                } else {
                    Ok(HybridValue::Unit)
                }
            }

            _ => Err(VeldError::RuntimeError(format!(
                "Statement type {:?} not yet implemented in hybrid mode",
                statement
            ))),
        }
    }

    /// Execute a block of statements
    fn execute_block_hybrid(&mut self, statements: &[Statement]) -> Result<HybridValue> {
        let mut last_value = HybridValue::Unit;

        for statement in statements {
            last_value = self.execute_statement_hybrid(statement)?;

            // Handle control flow
            match last_value {
                HybridValue::Break | HybridValue::Continue => return Ok(last_value),
                _ => {}
            }
        }

        Ok(last_value)
    }

    /// Evaluate an expression and return a hybrid value
    fn evaluate_expression_hybrid(&mut self, expr: &Expr) -> Result<HybridValue> {
        match expr {
            Expr::Literal(lit) => self.evaluate_literal_hybrid(lit),

            Expr::Identifier(name) => {
                // TODO: Get from interpreter's variables when methods are public
                // For now, return a placeholder
                Err(VeldError::RuntimeError(format!(
                    "Variable access not yet implemented in hybrid mode: '{}'",
                    name
                )))
            }

            Expr::BinaryOperation {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expression_hybrid(left)?;
                let right_val = self.evaluate_expression_hybrid(right)?;
                self.evaluate_binary_operation_hybrid(&left_val, operator, &right_val)
            }

            Expr::UnaryOperation { operator, operand } => {
                let operand_val = self.evaluate_expression_hybrid(operand)?;
                self.evaluate_unary_operation_hybrid(operator, &operand_val)
            }

            Expr::Array(elements) => {
                let mut hybrid_elements = Vec::new();
                for element in elements {
                    hybrid_elements.push(self.evaluate_expression_hybrid(element)?);
                }
                self.builder.array(hybrid_elements)
            }

            Expr::Tuple(elements) => {
                let mut hybrid_elements = Vec::new();
                for element in elements {
                    hybrid_elements.push(self.evaluate_expression_hybrid(element)?);
                }
                Ok(HybridValue::new_tuple(hybrid_elements))
            }

            Expr::Struct { name, fields } => {
                let mut hybrid_fields = HashMap::new();
                for (field_name, field_expr) in fields {
                    let field_value = self.evaluate_expression_hybrid(field_expr)?;
                    hybrid_fields.insert(field_name.clone(), field_value);
                }
                self.builder.struct_value(name.clone(), hybrid_fields)
            }

            Expr::FunctionCall { name, arguements } => {
                // TODO: Implement hybrid function calls
                Err(VeldError::RuntimeError(
                    "Function calls not yet implemented in hybrid mode".to_string(),
                ))
            }

            _ => {
                // TODO: Implement remaining expression types
                Err(VeldError::RuntimeError(
                    "Expression type not yet implemented in hybrid mode".to_string(),
                ))
            }
        }
    }

    /// Evaluate a literal as a hybrid value
    fn evaluate_literal_hybrid(&self, literal: &Literal) -> Result<HybridValue> {
        match literal {
            Literal::Integer(i) => Ok(HybridValue::Integer(*i)),
            Literal::Float(f) => Ok(HybridValue::Float(*f)),
            Literal::Boolean(b) => Ok(HybridValue::Boolean(*b)),
            Literal::Char(c) => Ok(HybridValue::Character(*c)),
            Literal::String(s) => Ok(HybridValue::new_string(s.clone())),
            Literal::Unit => Ok(HybridValue::Unit),
            // Note: Null is not in the actual Literal enum
        }
    }

    /// Evaluate binary operations on hybrid values
    fn evaluate_binary_operation_hybrid(
        &self,
        left: &HybridValue,
        operator: &BinaryOperator,
        right: &HybridValue,
    ) -> Result<HybridValue> {
        match (left, operator, right) {
            // Arithmetic operations
            (HybridValue::Integer(a), BinaryOperator::Add, HybridValue::Integer(b)) => {
                Ok(HybridValue::Integer(a + b))
            }
            (HybridValue::Integer(a), BinaryOperator::Subtract, HybridValue::Integer(b)) => {
                Ok(HybridValue::Integer(a - b))
            }
            (HybridValue::Integer(a), BinaryOperator::Multiply, HybridValue::Integer(b)) => {
                Ok(HybridValue::Integer(a * b))
            }
            (HybridValue::Integer(a), BinaryOperator::Divide, HybridValue::Integer(b)) => {
                if *b == 0 {
                    Err(VeldError::RuntimeError("Division by zero".to_string()))
                } else {
                    Ok(HybridValue::Integer(a / b))
                }
            }

            // Float arithmetic
            (HybridValue::Float(a), BinaryOperator::Add, HybridValue::Float(b)) => {
                Ok(HybridValue::Float(a + b))
            }
            (HybridValue::Float(a), BinaryOperator::Subtract, HybridValue::Float(b)) => {
                Ok(HybridValue::Float(a - b))
            }
            (HybridValue::Float(a), BinaryOperator::Multiply, HybridValue::Float(b)) => {
                Ok(HybridValue::Float(a * b))
            }
            (HybridValue::Float(a), BinaryOperator::Divide, HybridValue::Float(b)) => {
                Ok(HybridValue::Float(a / b))
            }

            // Mixed int/float arithmetic
            (HybridValue::Integer(a), BinaryOperator::Add, HybridValue::Float(b)) => {
                Ok(HybridValue::Float(*a as f64 + b))
            }
            (HybridValue::Float(a), BinaryOperator::Add, HybridValue::Integer(b)) => {
                Ok(HybridValue::Float(a + *b as f64))
            }

            // Comparison operations
            (HybridValue::Integer(a), BinaryOperator::EqualEqual, HybridValue::Integer(b)) => {
                Ok(HybridValue::Boolean(a == b))
            }
            (HybridValue::Integer(a), BinaryOperator::Less, HybridValue::Integer(b)) => {
                Ok(HybridValue::Boolean(a < b))
            }
            (HybridValue::Integer(a), BinaryOperator::Greater, HybridValue::Integer(b)) => {
                Ok(HybridValue::Boolean(a > b))
            }

            // String operations
            (left_str, BinaryOperator::Add, right_str) => {
                if let (Some(s1), Some(s2)) =
                    (left_str.as_string(&self.gc), right_str.as_string(&self.gc))
                {
                    Ok(HybridValue::new_string(s1 + &s2))
                } else {
                    Err(VeldError::RuntimeError(
                        "Cannot concatenate non-strings".to_string(),
                    ))
                }
            }

            // Logical operations
            (HybridValue::Boolean(a), BinaryOperator::And, HybridValue::Boolean(b)) => {
                Ok(HybridValue::Boolean(*a && *b))
            }
            (HybridValue::Boolean(a), BinaryOperator::Or, HybridValue::Boolean(b)) => {
                Ok(HybridValue::Boolean(*a || *b))
            }

            // Equality with GC dereferencing
            (left, BinaryOperator::EqualEqual, right) => {
                Ok(HybridValue::Boolean(left.equals(right, &self.gc)))
            }

            _ => Err(VeldError::RuntimeError(format!(
                "Unsupported binary operation: {:?} {:?} {:?}",
                left, operator, right
            ))),
        }
    }

    /// Evaluate unary operations on hybrid values
    fn evaluate_unary_operation_hybrid(
        &self,
        operator: &UnaryOperator,
        operand: &HybridValue,
    ) -> Result<HybridValue> {
        match (operator, operand) {
            (UnaryOperator::Negate, HybridValue::Integer(i)) => Ok(HybridValue::Integer(-i)),
            (UnaryOperator::Negate, HybridValue::Float(f)) => Ok(HybridValue::Float(-f)),
            (UnaryOperator::Not, HybridValue::Boolean(b)) => Ok(HybridValue::Boolean(!b)),
            _ => Err(VeldError::RuntimeError(format!(
                "Unsupported unary operation: {:?} {:?}",
                operator, operand
            ))),
        }
    }

    /// Convert a legacy Value to a HybridValue
    pub fn convert_legacy_to_hybrid(&self, value: LegacyValue) -> Result<HybridValue> {
        match value {
            LegacyValue::Integer(i) => Ok(HybridValue::Integer(i)),
            LegacyValue::Float(f) => Ok(HybridValue::Float(f)),
            LegacyValue::Boolean(b) => Ok(HybridValue::Boolean(b)),
            // Note: Character is not in the actual Value enum
            LegacyValue::String(s) => Ok(HybridValue::new_string(s)),
            LegacyValue::Unit => Ok(HybridValue::Unit),
            LegacyValue::Null => Ok(HybridValue::Null),
            LegacyValue::Break => Ok(HybridValue::Break),
            LegacyValue::Continue => Ok(HybridValue::Continue),

            LegacyValue::Array(elements) => {
                let mut hybrid_elements = Vec::new();
                for element in elements {
                    hybrid_elements.push(self.convert_legacy_to_hybrid(element)?);
                }
                self.builder.array(hybrid_elements)
            }

            LegacyValue::Tuple(elements) => {
                let mut hybrid_elements = Vec::new();
                for element in elements {
                    hybrid_elements.push(self.convert_legacy_to_hybrid(element)?);
                }
                Ok(HybridValue::new_tuple(hybrid_elements))
            }

            LegacyValue::Struct { name, fields } => {
                let mut hybrid_fields = HashMap::new();
                for (field_name, field_value) in fields {
                    hybrid_fields.insert(field_name, self.convert_legacy_to_hybrid(field_value)?);
                }
                self.builder.struct_value(name, hybrid_fields)
            }

            LegacyValue::Closure {
                params,
                body,
                captured_vars,
                ..
            } => {
                let mut hybrid_captures = HashMap::new();
                for (var_name, var_value) in captured_vars {
                    hybrid_captures.insert(var_name, self.convert_legacy_to_hybrid(var_value)?);
                }
                self.builder.closure(params, body, hybrid_captures)
            }

            // For complex types, create simplified representations for now
            _ => {
                // Log a warning that some features may be lost in conversion
                eprintln!(
                    "Warning: Converting complex legacy value type to Null (full conversion not implemented)"
                );
                Ok(HybridValue::Null)
            }
        }
    }

    /// Convert a HybridValue to a legacy Value
    pub fn convert_hybrid_to_legacy(&self, value: &HybridValue) -> Result<LegacyValue> {
        match value {
            HybridValue::Integer(i) => Ok(LegacyValue::Integer(*i)),
            HybridValue::Float(f) => Ok(LegacyValue::Float(*f)),
            HybridValue::Boolean(b) => Ok(LegacyValue::Boolean(*b)),
            HybridValue::Character(c) => Ok(LegacyValue::Character(*c)),
            HybridValue::Unit => Ok(LegacyValue::Unit),
            HybridValue::Null => Ok(LegacyValue::Null),
            HybridValue::Break => Ok(LegacyValue::Break),
            HybridValue::Continue => Ok(LegacyValue::Continue),

            // String conversion (inline or heap)
            HybridValue::SmallString { data, len } => {
                let bytes = &data[..*len as usize];
                let string = String::from_utf8(bytes.to_vec())
                    .map_err(|_| VeldError::RuntimeError("Invalid UTF-8 in string".to_string()))?;
                Ok(LegacyValue::String(string))
            }
            HybridValue::String(handle) => {
                if let Some(heap_string) = self.gc.deref::<HeapString>(handle) {
                    Ok(LegacyValue::String(heap_string.data.clone()))
                } else {
                    Err(VeldError::RuntimeError(
                        "Invalid string reference".to_string(),
                    ))
                }
            }

            // Tuple conversion (inline or heap)
            HybridValue::SmallTuple { elements, .. } => {
                let mut legacy_elements = Vec::new();
                for element in elements {
                    legacy_elements.push(self.convert_hybrid_to_legacy(element)?);
                }
                Ok(LegacyValue::Tuple(legacy_elements))
            }
            HybridValue::Tuple(handle) => {
                if let Some(heap_tuple) = self.gc.deref::<HeapTuple>(handle) {
                    let mut legacy_elements = Vec::new();
                    for element in &heap_tuple.elements {
                        legacy_elements.push(self.convert_hybrid_to_legacy(element)?);
                    }
                    Ok(LegacyValue::Tuple(legacy_elements))
                } else {
                    Err(VeldError::RuntimeError(
                        "Invalid tuple reference".to_string(),
                    ))
                }
            }

            // Array conversion
            HybridValue::Array(handle) => {
                if let Some(heap_array) = self.gc.deref::<HeapArray>(handle) {
                    let mut legacy_elements = Vec::new();
                    for element in &heap_array.elements {
                        legacy_elements.push(self.convert_hybrid_to_legacy(element)?);
                    }
                    Ok(LegacyValue::Array(legacy_elements))
                } else {
                    Err(VeldError::RuntimeError(
                        "Invalid array reference".to_string(),
                    ))
                }
            }

            // Struct conversion
            HybridValue::Struct(handle) => {
                if let Some(heap_struct) = self.gc.deref::<HeapStruct>(handle) {
                    let mut legacy_fields = HashMap::new();
                    for (field_name, field_value) in &heap_struct.fields {
                        legacy_fields.insert(
                            field_name.clone(),
                            self.convert_hybrid_to_legacy(field_value)?,
                        );
                    }
                    Ok(LegacyValue::Struct {
                        name: heap_struct.name.clone(),
                        fields: legacy_fields,
                    })
                } else {
                    Err(VeldError::RuntimeError(
                        "Invalid struct reference".to_string(),
                    ))
                }
            }

            // For now, convert other complex types to null with a warning
            _ => {
                eprintln!(
                    "Warning: Converting complex hybrid value type to Null (full conversion not implemented)"
                );
                Ok(LegacyValue::Null)
            }
        }
    }

    /// Get GC statistics
    pub fn gc_stats(&self) -> crate::gc::GcStatistics {
        self.gc.statistics()
    }

    /// Force garbage collection
    pub fn collect_garbage(&self) -> Result<(usize, usize)> {
        self.gc.collect()
    }

    /// Get current memory usage
    pub fn memory_usage(&self) -> usize {
        self.gc.memory_usage()
    }
}

/// Helper trait for automatic value conversion
pub trait IntoHybrid {
    fn into_hybrid(self, gc: &SafeGc) -> Result<HybridValue>;
}

impl IntoHybrid for LegacyValue {
    fn into_hybrid(self, _gc: &SafeGc) -> Result<HybridValue> {
        Ok(HybridValue::from_legacy(self))
    }
}

impl IntoHybrid for i64 {
    fn into_hybrid(self, _gc: &SafeGc) -> Result<HybridValue> {
        Ok(HybridValue::Integer(self))
    }
}

impl IntoHybrid for f64 {
    fn into_hybrid(self, _gc: &SafeGc) -> Result<HybridValue> {
        Ok(HybridValue::Float(self))
    }
}

impl IntoHybrid for bool {
    fn into_hybrid(self, _gc: &SafeGc) -> Result<HybridValue> {
        Ok(HybridValue::Boolean(self))
    }
}

impl IntoHybrid for String {
    fn into_hybrid(self, _gc: &SafeGc) -> Result<HybridValue> {
        Ok(HybridValue::new_string(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hybrid_interpreter_creation() -> Result<()> {
        let interpreter = HybridInterpreter::new(".")?;
        assert!(interpreter.migration_mode);
        Ok(())
    }

    #[test]
    fn test_literal_evaluation() -> Result<()> {
        let mut interpreter = HybridInterpreter::new(".")?;

        let int_lit = Literal::Integer(42);
        let result = interpreter.evaluate_literal_hybrid(&int_lit)?;
        assert_eq!(result, HybridValue::Integer(42));

        let str_lit = Literal::String("hello".to_string());
        let result = interpreter.evaluate_literal_hybrid(&str_lit)?;
        assert!(result.is_immediate()); // Should be stored inline

        Ok(())
    }

    #[test]
    fn test_binary_operations() -> Result<()> {
        let interpreter = HybridInterpreter::new(".")?;

        let left = HybridValue::Integer(10);
        let right = HybridValue::Integer(5);

        let result =
            interpreter.evaluate_binary_operation_hybrid(&left, &BinaryOperator::Add, &right)?;
        assert_eq!(result, HybridValue::Integer(15));

        let result = interpreter.evaluate_binary_operation_hybrid(
            &left,
            &BinaryOperator::Subtract,
            &right,
        )?;
        assert_eq!(result, HybridValue::Integer(5));

        Ok(())
    }

    #[test]
    fn test_value_conversion() -> Result<()> {
        let interpreter = HybridInterpreter::new(".")?;

        // Legacy to hybrid
        let legacy = LegacyValue::Integer(42);
        let hybrid = interpreter.convert_legacy_to_hybrid(legacy)?;
        assert_eq!(hybrid, HybridValue::Integer(42));

        // Hybrid to legacy
        let hybrid = HybridValue::Float(3.14);
        let legacy = interpreter.convert_hybrid_to_legacy(&hybrid)?;
        assert_eq!(legacy, LegacyValue::Float(3.14));

        Ok(())
    }

    #[test]
    fn test_string_optimization() -> Result<()> {
        let interpreter = HybridInterpreter::new(".")?;

        // Small string should be immediate
        let small = HybridValue::new_string("small".to_string());
        assert!(small.is_immediate());

        // Convert back to legacy
        let legacy = interpreter.convert_hybrid_to_legacy(&small)?;
        if let LegacyValue::String(s) = legacy {
            assert_eq!(s, "small");
        } else {
            panic!("Expected string value");
        }

        Ok(())
    }
}
