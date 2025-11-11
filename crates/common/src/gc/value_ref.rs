//! GC-Aware Value References
//!
//! This module provides GC-aware wrappers for Veld values that automatically
//! participate in garbage collection. Values can be stored either directly
//! or as GC-managed references.

use std::fmt;
use std::ops::{Deref, DerefMut};

use super::super::value::Value;
use super::GarbageCollector;
use super::handle::GcHandle;
use veld_error::{Result, VeldError};
/// A reference to a value that may be garbage collected
#[derive(Debug, Clone)]
pub enum GcValueRef {
    /// Direct value (not GC managed)
    Direct(Value),
    /// GC-managed reference
    Managed(GcHandle),
}

impl GcValueRef {
    /// Create a new direct value reference
    pub fn direct(value: Value) -> Self {
        Self::Direct(value)
    }

    /// Create a new GC-managed reference
    pub fn managed(handle: GcHandle) -> Self {
        Self::Managed(handle)
    }

    /// Check if this is a direct value
    pub fn is_direct(&self) -> bool {
        matches!(self, Self::Direct(_))
    }

    /// Check if this is a GC-managed reference
    pub fn is_managed(&self) -> bool {
        matches!(self, Self::Managed(_))
    }

    /// Get the value, dereferencing GC handles if necessary
    pub fn get_value(&self, gc: &GarbageCollector) -> Option<Value> {
        match self {
            Self::Direct(value) => Some(value.clone()),
            Self::Managed(handle) => gc.deref(handle),
        }
    }

    /// Get a reference to the value if it's direct
    pub fn as_direct(&self) -> Option<&Value> {
        match self {
            Self::Direct(value) => Some(value),
            Self::Managed(_) => None,
        }
    }

    /// Get the GC handle if this is managed
    pub fn as_handle(&self) -> Option<&GcHandle> {
        match self {
            Self::Direct(_) => None,
            Self::Managed(handle) => Some(handle),
        }
    }

    /// Convert to a direct value, dereferencing if necessary
    pub fn into_direct(self, gc: &GarbageCollector) -> Option<Value> {
        match self {
            Self::Direct(value) => Some(value),
            Self::Managed(handle) => gc.deref(&handle),
        }
    }

    /// Try to update the value (only works for managed references)
    pub fn update(&self, gc: &GarbageCollector, new_value: Value) -> Result<()> {
        match self {
            Self::Direct(_) => Err(VeldError::RuntimeError(
                "Cannot update direct value reference".to_string(),
            )),
            Self::Managed(handle) => gc.update(handle, new_value),
        }
    }

    /// Check if the referenced value is still alive (for GC references)
    pub fn is_alive(&self, gc: &GarbageCollector) -> bool {
        match self {
            Self::Direct(_) => true, // Direct values are always "alive"
            Self::Managed(handle) => gc.is_alive(handle),
        }
    }

    /// Convert a direct value to a GC-managed value
    pub fn make_managed(&mut self, gc: &GarbageCollector) -> Result<()> {
        match self {
            Self::Direct(value) => {
                let handle = gc.allocate(value.clone())?;
                *self = Self::Managed(handle);
                Ok(())
            }
            Self::Managed(_) => Ok(()), // Already managed
        }
    }

    /// Convert a GC-managed value to a direct value
    pub fn make_direct(&mut self, gc: &GarbageCollector) -> Result<()> {
        match self {
            Self::Direct(_) => Ok(()), // Already direct
            Self::Managed(handle) => {
                if let Some(value) = gc.deref(handle) {
                    *self = Self::Direct(value);
                    Ok(())
                } else {
                    Err(VeldError::RuntimeError(
                        "Cannot dereference dead GC handle".to_string(),
                    ))
                }
            }
        }
    }

    /// Get the type name of the referenced value
    pub fn type_name(&self, gc: &GarbageCollector) -> Option<String> {
        self.get_value(gc).map(|v| v.type_of().to_string())
    }
}

impl fmt::Display for GcValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Direct(value) => write!(f, "Direct({:?})", value),
            Self::Managed(handle) => write!(f, "Managed({})", handle),
        }
    }
}

impl PartialEq for GcValueRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Direct(a), Self::Direct(b)) => a == b,
            (Self::Managed(a), Self::Managed(b)) => a == b,
            _ => false, // Different types are not equal
        }
    }
}

impl Eq for GcValueRef {}

/// A mutable reference to a GC-managed value
pub struct GcValueMut {
    handle: GcHandle,
    value: Value,
    gc: *const GarbageCollector, // Raw pointer to avoid lifetime issues
}

impl GcValueMut {
    /// Create a new mutable GC value reference
    ///
    /// # Safety
    /// The caller must ensure that the GC reference remains valid for the lifetime
    /// of this GcValueMut instance.
    pub unsafe fn new(handle: GcHandle, value: Value, gc: &GarbageCollector) -> Self {
        Self {
            handle,
            value,
            gc: gc as *const GarbageCollector,
        }
    }

    /// Get the GC handle
    pub fn handle(&self) -> &GcHandle {
        &self.handle
    }

    /// Commit changes back to the GC
    pub fn commit(self) -> Result<()> {
        unsafe { (*self.gc).update(&self.handle, self.value.clone()) }
    }
}

impl Deref for GcValueMut {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for GcValueMut {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl Drop for GcValueMut {
    fn drop(&mut self) {
        // Automatically commit changes when dropped
        let _ = unsafe { (*self.gc).update(&self.handle, self.value.clone()) };
    }
}

/// Utility functions for working with GC values
pub mod gc_value_utils {
    use super::super::super::value::Value;
    use super::*;

    /// Convert a slice of values to GC-managed references
    pub fn values_to_gc_refs(values: &[Value], gc: &GarbageCollector) -> Result<Vec<GcValueRef>> {
        values
            .iter()
            .map(|v| {
                let handle = gc.allocate(v.clone())?;
                Ok(GcValueRef::Managed(handle))
            })
            .collect()
    }

    /// Convert GC references back to values
    pub fn gc_refs_to_values(refs: &[GcValueRef], gc: &GarbageCollector) -> Vec<Option<Value>> {
        refs.iter().map(|r| r.get_value(gc)).collect()
    }

    /// Filter out dead GC references
    pub fn filter_live_refs(refs: Vec<GcValueRef>, gc: &GarbageCollector) -> Vec<GcValueRef> {
        refs.into_iter().filter(|r| r.is_alive(gc)).collect()
    }

    /// Deep clone a value, converting any GC references to new handles
    pub fn deep_clone_with_gc(value: &Value, gc: &GarbageCollector) -> Result<GcValueRef> {
        match value {
            // Simple values can be cloned directly
            Value::Integer(_)
            | Value::Float(_)
            | Value::Boolean(_)
            | Value::Char(_)
            | Value::String(_)
            | Value::Unit
            | Value::Break
            | Value::Continue => Ok(GcValueRef::direct(value.clone())),

            // Complex values need GC allocation
            Value::Array(elements) => {
                let mut new_elements = Vec::new();
                for element in elements {
                    let cloned_ref = deep_clone_with_gc(element, gc)?;
                    if let Some(cloned_value) = cloned_ref.get_value(gc) {
                        new_elements.push(cloned_value);
                    }
                }
                let new_array = Value::Array(new_elements);
                let handle = gc.allocate(new_array)?;
                Ok(GcValueRef::managed(handle))
            }

            Value::Tuple(elements) => {
                let mut new_elements = Vec::new();
                for element in elements {
                    let cloned_ref = deep_clone_with_gc(element, gc)?;
                    if let Some(cloned_value) = cloned_ref.get_value(gc) {
                        new_elements.push(cloned_value);
                    }
                }
                let new_tuple = Value::Tuple(new_elements);
                let handle = gc.allocate(new_tuple)?;
                Ok(GcValueRef::managed(handle))
            }

            Value::Struct { name, fields } => {
                let mut new_fields = std::collections::HashMap::new();
                for (key, field_value) in fields {
                    let cloned_ref = deep_clone_with_gc(field_value, gc)?;
                    if let Some(cloned_value) = cloned_ref.get_value(gc) {
                        new_fields.insert(key.clone(), cloned_value);
                    }
                }
                let new_struct = Value::Struct {
                    name: name.clone(),
                    fields: new_fields,
                };
                let handle = gc.allocate(new_struct)?;
                Ok(GcValueRef::managed(handle))
            }

            // For other complex types, allocate a clone in GC
            _ => {
                let handle = gc.allocate(value.clone())?;
                Ok(GcValueRef::managed(handle))
            }
        }
    }

    /// Check if a value contains any GC-managed references
    pub fn contains_gc_refs(value: &Value) -> bool {
        match value {
            Value::Array(elements) => elements.iter().any(contains_gc_refs),
            Value::Tuple(elements) => elements.iter().any(contains_gc_refs),
            Value::Struct { fields, .. } => fields.values().any(contains_gc_refs),
            Value::Enum { fields, .. } => fields.iter().any(contains_gc_refs),
            Value::Function { captured_vars, .. } => captured_vars.values().any(contains_gc_refs),
            Value::Return(inner) => contains_gc_refs(inner),
            Value::Record(fields) => fields.values().any(contains_gc_refs),
            _ => false, // Primitive values don't contain GC refs in current implementation
        }
    }

    /// Estimate the total size of a value tree including GC references
    pub fn estimate_total_size(value: &Value, gc: &GarbageCollector) -> usize {
        match value {
            Value::Integer(_) => std::mem::size_of::<i64>(),
            Value::Float(_) => std::mem::size_of::<f64>(),
            Value::Boolean(_) => std::mem::size_of::<bool>(),
            Value::Char(_) => std::mem::size_of::<char>(),
            Value::String(s) => s.len() + std::mem::size_of::<String>(),
            Value::Unit => 0,

            Value::Array(elements) => {
                elements
                    .iter()
                    .map(|e| estimate_total_size(e, gc))
                    .sum::<usize>()
                    + std::mem::size_of::<Vec<Value>>()
            }

            Value::Tuple(elements) => {
                elements
                    .iter()
                    .map(|e| estimate_total_size(e, gc))
                    .sum::<usize>()
                    + std::mem::size_of::<Vec<Value>>()
            }

            Value::Struct { fields, .. } => {
                fields
                    .values()
                    .map(|v| estimate_total_size(v, gc))
                    .sum::<usize>()
                    + fields.keys().map(|k| k.len()).sum::<usize>()
                    + std::mem::size_of::<std::collections::HashMap<String, Value>>()
            }

            _ => std::mem::size_of::<Value>(), // Conservative estimate for other types
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::GarbageCollector;

    #[test]
    fn test_direct_value_ref() {
        let value = Value::Integer(42);
        let value_ref = GcValueRef::direct(value.clone());

        assert!(value_ref.is_direct());
        assert!(!value_ref.is_managed());
        assert_eq!(value_ref.as_direct(), Some(&value));
        assert_eq!(value_ref.as_handle(), None);
    }

    #[test]
    fn test_managed_value_ref() {
        let gc = GarbageCollector::new();
        let value = Value::String("test".to_string());
        let handle = gc.allocate(value.clone()).unwrap();
        let value_ref = GcValueRef::managed(handle.clone());

        assert!(!value_ref.is_direct());
        assert!(value_ref.is_managed());
        assert_eq!(value_ref.as_direct(), None);
        assert_eq!(value_ref.as_handle(), Some(&handle));
        assert_eq!(value_ref.get_value(&gc), Some(value));
    }

    #[test]
    fn test_make_managed() {
        let gc = GarbageCollector::new();
        let value = Value::Integer(100);
        let mut value_ref = GcValueRef::direct(value.clone());

        assert!(value_ref.is_direct());

        value_ref.make_managed(&gc).unwrap();

        assert!(value_ref.is_managed());
        assert_eq!(value_ref.get_value(&gc), Some(value));
    }

    #[test]
    fn test_make_direct() {
        let gc = GarbageCollector::new();
        let value = Value::Boolean(true);
        let handle = gc.allocate(value.clone()).unwrap();
        let mut value_ref = GcValueRef::managed(handle);

        assert!(value_ref.is_managed());

        value_ref.make_direct(&gc).unwrap();

        assert!(value_ref.is_direct());
        assert_eq!(value_ref.as_direct(), Some(&value));
    }

    #[test]
    fn test_value_equality() {
        let value1 = Value::Integer(42);
        let value2 = Value::Integer(42);
        let value3 = Value::Integer(43);

        let ref1 = GcValueRef::direct(value1);
        let ref2 = GcValueRef::direct(value2);
        let ref3 = GcValueRef::direct(value3);

        assert_eq!(ref1, ref2);
        assert_ne!(ref1, ref3);
    }

    #[test]
    fn test_gc_value_utils() {
        let gc = GarbageCollector::new();
        let values = vec![
            Value::Integer(1),
            Value::String("test".to_string()),
            Value::Boolean(true),
        ];

        let refs = gc_value_utils::values_to_gc_refs(&values, &gc).unwrap();
        assert_eq!(refs.len(), 3);
        assert!(refs.iter().all(|r| r.is_managed()));

        let recovered = gc_value_utils::gc_refs_to_values(&refs, &gc);
        assert_eq!(recovered.len(), 3);
        assert!(recovered.iter().all(|v| v.is_some()));
    }

    #[test]
    fn test_deep_clone() {
        let gc = GarbageCollector::new();

        let original = Value::Array(vec![Value::Integer(1), Value::String("nested".to_string())]);

        let cloned_ref = gc_value_utils::deep_clone_with_gc(&original, &gc).unwrap();
        assert!(cloned_ref.is_managed());

        let cloned_value = cloned_ref.get_value(&gc).unwrap();
        assert_eq!(cloned_value, original);
    }

    #[test]
    fn test_contains_gc_refs() {
        let simple_value = Value::Integer(42);
        assert!(!gc_value_utils::contains_gc_refs(&simple_value));

        let complex_value =
            Value::Array(vec![Value::Integer(1), Value::String("test".to_string())]);
        // Current implementation returns false since we don't embed GC refs directly
        assert!(!gc_value_utils::contains_gc_refs(&complex_value));
    }

    #[test]
    fn test_size_estimation() {
        let gc = GarbageCollector::new();

        let int_value = Value::Integer(42);
        let int_size = gc_value_utils::estimate_total_size(&int_value, &gc);
        assert_eq!(int_size, std::mem::size_of::<i64>());

        let string_value = Value::String("hello".to_string());
        let string_size = gc_value_utils::estimate_total_size(&string_value, &gc);
        assert!(string_size >= 5); // At least the string length
    }
}
