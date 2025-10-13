//! Hybrid Value System for Veld
//!
//! This module implements a hybrid memory model inspired by OCaml and C#:
//! - Immediate values (stored directly, no GC overhead)
//! - Reference values (heap-allocated with GC management)
//! - Automatic optimization based on size and type
//! - Opt-in manual control for advanced users

use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

use veld_common::ast::{Statement, TypeAnnotation};
use veld_common::types::Type;
use veld_common::value::Value as LegacyValue;
use veld_error::Result;

use crate::gc::{GcHandle, SafeGc};

/// Maximum size for inline string storage (22 bytes + length byte)
const MAX_INLINE_STRING_SIZE: usize = 22;

/// Maximum size for inline tuple storage (in bytes)
const MAX_INLINE_TUPLE_SIZE: usize = 32;

/// Hybrid value representation supporting both immediate and reference semantics
#[derive(Debug, Clone)]
pub enum HybridValue {
    // === Immediate Values (No GC overhead) ===
    /// 64-bit signed integer
    Integer(i64),

    /// 64-bit floating point
    Float(f64),

    /// Boolean value
    Boolean(bool),

    /// Unicode character (32-bit)
    Character(char),

    /// Small string stored inline (≤ 22 bytes)
    SmallString {
        data: [u8; MAX_INLINE_STRING_SIZE],
        len: u8,
    },

    /// Small tuple stored inline (≤ 32 bytes total)
    SmallTuple {
        elements: Vec<HybridValue>,
        size_bytes: u16,
    },

    /// Unit/void value
    Unit,

    // === Reference Values (GC Managed) ===
    /// Large string (heap allocated)
    String(GcHandle),

    /// Array of values
    Array(GcHandle),

    /// Tuple (when too large for inline storage)
    Tuple(GcHandle),

    /// Struct instance
    Struct(GcHandle),

    /// Enum variant
    Enum(GcHandle),

    /// Function closure
    Closure(GcHandle),

    /// Module reference
    Module(GcHandle),

    /// Optional value
    Option(Box<Option<HybridValue>>),

    /// Result value
    Result(Box<std::result::Result<HybridValue, HybridValue>>),

    // === Special Values ===
    /// Null/None value
    Null,

    /// Break statement marker
    Break,

    /// Continue statement marker
    Continue,
}

/// Heap-allocated string data
#[derive(Debug, Clone)]
pub struct HeapString {
    pub data: String,
}

/// Heap-allocated array data
#[derive(Debug, Clone)]
pub struct HeapArray {
    pub elements: Vec<HybridValue>,
    pub element_type: Option<Type>,
}

/// Heap-allocated tuple data
#[derive(Debug, Clone)]
pub struct HeapTuple {
    pub elements: Vec<HybridValue>,
}

/// Heap-allocated struct data
#[derive(Debug, Clone)]
pub struct HeapStruct {
    pub name: String,
    pub fields: HashMap<String, HybridValue>,
    pub struct_type: Option<Type>,
}

/// Heap-allocated enum variant
#[derive(Debug, Clone)]
pub struct HeapEnum {
    pub name: String,
    pub variant: String,
    pub data: Option<Box<HybridValue>>,
    pub enum_type: Option<Type>,
}

/// Heap-allocated closure
#[derive(Debug, Clone)]
pub struct HeapClosure {
    pub params: Vec<(String, TypeAnnotation)>,
    pub body: Vec<Statement>,
    pub captured_vars: HashMap<String, HybridValue>,
    pub function_type: Option<Type>,
}

/// Heap-allocated module
#[derive(Debug, Clone)]
pub struct HeapModule {
    pub name: String,
    pub exports: HashMap<String, HybridValue>,
    pub path: Option<std::path::PathBuf>,
}

impl HybridValue {
    /// Check if this value is stored immediately (no GC overhead)
    pub fn is_immediate(&self) -> bool {
        matches!(
            self,
            HybridValue::Integer(_)
                | HybridValue::Float(_)
                | HybridValue::Boolean(_)
                | HybridValue::Character(_)
                | HybridValue::SmallString { .. }
                | HybridValue::SmallTuple { .. }
                | HybridValue::Unit
                | HybridValue::Null
                | HybridValue::Break
                | HybridValue::Continue
        )
    }

    /// Check if this value needs GC tracking
    pub fn needs_gc_tracking(&self) -> bool {
        !self.is_immediate()
    }

    /// Get the estimated size in bytes
    pub fn size_bytes(&self) -> usize {
        match self {
            HybridValue::Integer(_) => 8,
            HybridValue::Float(_) => 8,
            HybridValue::Boolean(_) => 1,
            HybridValue::Character(_) => 4,
            HybridValue::SmallString { len, .. } => *len as usize,
            HybridValue::SmallTuple { size_bytes, .. } => *size_bytes as usize,
            HybridValue::Unit | HybridValue::Null | HybridValue::Break | HybridValue::Continue => 0,

            // Reference types - estimated heap object size
            HybridValue::String(_) => 24, // Estimate for string header + data
            HybridValue::Array(_) => 32,  // Estimate for array header
            HybridValue::Tuple(_) => 24,  // Estimate for tuple header
            HybridValue::Struct(_) => 48, // Estimate for struct header + fields
            HybridValue::Enum(_) => 32,   // Estimate for enum header
            HybridValue::Closure(_) => 64, // Estimate for closure + captures
            HybridValue::Module(_) => 128, // Estimate for module header
            HybridValue::Option(_) => 16, // Box overhead
            HybridValue::Result(_) => 16, // Box overhead
        }
    }

    /// Create a string value, choosing inline vs heap storage automatically
    pub fn new_string(s: String) -> Self {
        if s.len() <= MAX_INLINE_STRING_SIZE {
            // Store inline
            let mut data = [0u8; MAX_INLINE_STRING_SIZE];
            let bytes = s.as_bytes();
            data[..bytes.len()].copy_from_slice(bytes);

            HybridValue::SmallString {
                data,
                len: bytes.len() as u8,
            }
        } else {
            // Store on heap (will need GC handle)
            // For now, we'll create a placeholder - this needs GC integration
            HybridValue::String(GcHandle::null()) // TODO: Proper GC allocation
        }
    }

    /// Create a tuple value, choosing inline vs heap storage automatically
    pub fn new_tuple(elements: Vec<HybridValue>) -> Self {
        let total_size: usize = elements.iter().map(|e| e.size_bytes()).sum();

        if total_size <= MAX_INLINE_TUPLE_SIZE && elements.len() <= 4 {
            // Store inline
            HybridValue::SmallTuple {
                elements,
                size_bytes: total_size as u16,
            }
        } else {
            // Store on heap
            HybridValue::Tuple(GcHandle::null()) // TODO: Proper GC allocation
        }
    }

    /// Extract string data regardless of storage location
    pub fn as_string(&self, gc: &SafeGc) -> Option<String> {
        match self {
            HybridValue::SmallString { data, len } => {
                let bytes = &data[..*len as usize];
                String::from_utf8(bytes.to_vec()).ok()
            }
            HybridValue::String(handle) => gc
                .deref::<HeapString>(handle)
                .map(|heap_str| heap_str.data.clone()),
            _ => None,
        }
    }

    /// Extract array elements regardless of storage location
    pub fn as_array(&self, gc: &SafeGc) -> Option<Vec<HybridValue>> {
        match self {
            HybridValue::Array(handle) => gc
                .deref::<HeapArray>(handle)
                .map(|heap_arr| heap_arr.elements.clone()),
            _ => None,
        }
    }

    /// Extract tuple elements regardless of storage location
    pub fn as_tuple(&self, gc: &SafeGc) -> Option<Vec<HybridValue>> {
        match self {
            HybridValue::SmallTuple { elements, .. } => Some(elements.clone()),
            HybridValue::Tuple(handle) => gc
                .deref::<HeapTuple>(handle)
                .map(|heap_tuple| heap_tuple.elements.clone()),
            _ => None,
        }
    }

    /// Check if two values are equal (with GC dereferencing)
    pub fn equals(&self, other: &HybridValue, gc: &SafeGc) -> bool {
        match (self, other) {
            // Immediate value comparisons
            (HybridValue::Integer(a), HybridValue::Integer(b)) => a == b,
            (HybridValue::Float(a), HybridValue::Float(b)) => a == b,
            (HybridValue::Boolean(a), HybridValue::Boolean(b)) => a == b,
            (HybridValue::Character(a), HybridValue::Character(b)) => a == b,
            (HybridValue::Unit, HybridValue::Unit) => true,
            (HybridValue::Null, HybridValue::Null) => true,

            // String comparisons (inline vs heap)
            (
                HybridValue::SmallString { data: d1, len: l1 },
                HybridValue::SmallString { data: d2, len: l2 },
            ) => l1 == l2 && d1[..*l1 as usize] == d2[..*l2 as usize],

            // String vs heap string
            (s1, s2) => {
                if let (Some(str1), Some(str2)) = (s1.as_string(gc), s2.as_string(gc)) {
                    str1 == str2
                } else {
                    false
                }
            }
        }
    }

    /// Get the type of this value
    pub fn get_type(&self) -> Type {
        match self {
            HybridValue::Integer(_) => Type::Integer,
            HybridValue::Float(_) => Type::Float,
            HybridValue::Boolean(_) => Type::Boolean,
            HybridValue::Character(_) => Type::Character,
            HybridValue::SmallString { .. } | HybridValue::String(_) => Type::String,
            HybridValue::Unit => Type::Unit,
            HybridValue::Null => Type::Nullable(Box::new(Type::Unit)),

            // Complex types - these would need more sophisticated type tracking
            HybridValue::SmallTuple { elements, .. } => {
                // For now, return a generic tuple type
                Type::Tuple(vec![Type::Any; elements.len().max(1)]) // Simplified
            }
            HybridValue::Tuple(_) => {
                // For heap tuples, we'd need to dereference to get element count
                Type::Tuple(vec![Type::Any; 2]) // Simplified
            }
            HybridValue::Array(_) => Type::Array(Box::new(Type::Any)),
            HybridValue::Struct(_) => Type::Struct {
                name: "Unknown".to_string(),
                fields: HashMap::new(),
            },
            HybridValue::Enum(_) => Type::Enum {
                name: "Unknown".to_string(),
                variants: HashMap::new(),
            },
            HybridValue::Closure(_) => Type::Function {
                params: vec![],
                return_type: Box::new(Type::Any),
            },
            HybridValue::Module(_) => Type::Module,
            HybridValue::Option(_) => Type::Optional(Box::new(Type::Any)),
            HybridValue::Result(_) => Type::Result {
                ok_type: Box::new(Type::Any),
                err_type: Box::new(Type::Any),
            },
            HybridValue::Break | HybridValue::Continue => Type::Never,
        }
    }

    /// Convert from legacy Value type (for migration)
    pub fn from_legacy(value: LegacyValue) -> Self {
        match value {
            LegacyValue::Integer(i) => HybridValue::Integer(i),
            LegacyValue::Float(f) => HybridValue::Float(f),
            LegacyValue::Boolean(b) => HybridValue::Boolean(b),
            LegacyValue::Character(c) => HybridValue::Character(c),
            LegacyValue::String(s) => HybridValue::new_string(s),
            LegacyValue::Unit => HybridValue::Unit,
            LegacyValue::Null => HybridValue::Null,
            LegacyValue::Break => HybridValue::Break,
            LegacyValue::Continue => HybridValue::Continue,

            // Complex types need GC integration
            LegacyValue::Array(elements) => {
                let hybrid_elements: Vec<HybridValue> =
                    elements.into_iter().map(HybridValue::from_legacy).collect();
                HybridValue::new_tuple(hybrid_elements) // Temporary conversion
            }

            // Other complex types - simplified conversion for now
            _ => HybridValue::Null, // TODO: Implement full conversion
        }
    }
}

impl fmt::Display for HybridValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HybridValue::Integer(i) => write!(f, "{}", i),
            HybridValue::Float(fl) => write!(f, "{}", fl),
            HybridValue::Boolean(b) => write!(f, "{}", b),
            HybridValue::Character(c) => write!(f, "'{}'", c),
            HybridValue::SmallString { data, len } => {
                let bytes = &data[..*len as usize];
                if let Ok(s) = String::from_utf8(bytes.to_vec()) {
                    write!(f, "\"{}\"", s)
                } else {
                    write!(f, "<invalid string>")
                }
            }
            HybridValue::SmallTuple { elements, .. } => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            HybridValue::Unit => write!(f, "()"),
            HybridValue::Null => write!(f, "null"),
            HybridValue::Break => write!(f, "<break>"),
            HybridValue::Continue => write!(f, "<continue>"),

            // Reference types - simplified display (would need GC dereferencing)
            HybridValue::String(_) => write!(f, "<heap string>"),
            HybridValue::Array(_) => write!(f, "<heap array>"),
            HybridValue::Tuple(_) => write!(f, "<heap tuple>"),
            HybridValue::Struct(_) => write!(f, "<heap struct>"),
            HybridValue::Enum(_) => write!(f, "<heap enum>"),
            HybridValue::Closure(_) => write!(f, "<closure>"),
            HybridValue::Module(_) => write!(f, "<module>"),
            HybridValue::Option(opt) => match opt.as_ref() {
                Some(val) => write!(f, "Some({})", val),
                None => write!(f, "None"),
            },
            HybridValue::Result(res) => match res.as_ref() {
                Ok(val) => write!(f, "Ok({})", val),
                Err(err) => write!(f, "Err({})", err),
            },
        }
    }
}

impl PartialEq for HybridValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HybridValue::Integer(a), HybridValue::Integer(b)) => a == b,
            (HybridValue::Float(a), HybridValue::Float(b)) => a == b,
            (HybridValue::Boolean(a), HybridValue::Boolean(b)) => a == b,
            (HybridValue::Character(a), HybridValue::Character(b)) => a == b,
            (HybridValue::Unit, HybridValue::Unit) => true,
            (HybridValue::Null, HybridValue::Null) => true,
            (HybridValue::Break, HybridValue::Break) => true,
            (HybridValue::Continue, HybridValue::Continue) => true,

            (
                HybridValue::SmallString { data: d1, len: l1 },
                HybridValue::SmallString { data: d2, len: l2 },
            ) => l1 == l2 && d1[..*l1 as usize] == d2[..*l2 as usize],

            (
                HybridValue::SmallTuple { elements: e1, .. },
                HybridValue::SmallTuple { elements: e2, .. },
            ) => e1 == e2,

            (HybridValue::Option(o1), HybridValue::Option(o2)) => o1 == o2,
            (HybridValue::Result(r1), HybridValue::Result(r2)) => r1 == r2,

            // Reference types would need GC dereferencing for full equality
            _ => false,
        }
    }
}

impl Hash for HybridValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            HybridValue::Integer(i) => i.hash(state),
            HybridValue::Float(f) => f.to_bits().hash(state),
            HybridValue::Boolean(b) => b.hash(state),
            HybridValue::Character(c) => c.hash(state),
            HybridValue::SmallString { data, len } => {
                len.hash(state);
                data[..*len as usize].hash(state);
            }
            HybridValue::SmallTuple { elements, .. } => {
                elements.hash(state);
            }
            HybridValue::Option(opt) => opt.hash(state),
            HybridValue::Result(res) => res.hash(state),

            // Reference types - hash the handle ID
            HybridValue::String(handle)
            | HybridValue::Array(handle)
            | HybridValue::Tuple(handle)
            | HybridValue::Struct(handle)
            | HybridValue::Enum(handle)
            | HybridValue::Closure(handle)
            | HybridValue::Module(handle) => {
                handle.id().hash(state);
            }

            // Unit types
            HybridValue::Unit | HybridValue::Null | HybridValue::Break | HybridValue::Continue => {}
        }
    }
}

/// Helper trait for values that can be stored immediately
pub trait Immediate: Sized {
    fn is_immediate_compatible() -> bool;
    fn size_hint() -> usize;
}

impl Immediate for i64 {
    fn is_immediate_compatible() -> bool {
        true
    }
    fn size_hint() -> usize {
        8
    }
}

impl Immediate for f64 {
    fn is_immediate_compatible() -> bool {
        true
    }
    fn size_hint() -> usize {
        8
    }
}

impl Immediate for bool {
    fn is_immediate_compatible() -> bool {
        true
    }
    fn size_hint() -> usize {
        1
    }
}

impl Immediate for char {
    fn is_immediate_compatible() -> bool {
        true
    }
    fn size_hint() -> usize {
        4
    }
}

/// Builder for creating hybrid values with automatic optimization
pub struct HybridValueBuilder {
    gc: SafeGc,
}

impl HybridValueBuilder {
    pub fn new(gc: SafeGc) -> Self {
        Self { gc }
    }

    /// Create a string with automatic storage optimization
    pub fn string(&self, s: String) -> Result<HybridValue> {
        if s.len() <= MAX_INLINE_STRING_SIZE {
            Ok(HybridValue::new_string(s))
        } else {
            // Allocate on GC heap
            let heap_string = HeapString { data: s };
            let handle = self.gc.allocate(heap_string)?;
            Ok(HybridValue::String(handle))
        }
    }

    /// Create an array with GC allocation
    pub fn array(&self, elements: Vec<HybridValue>) -> Result<HybridValue> {
        let heap_array = HeapArray {
            elements,
            element_type: None,
        };
        let handle = self.gc.allocate(heap_array)?;
        Ok(HybridValue::Array(handle))
    }

    /// Create a struct with GC allocation
    pub fn struct_value(
        &self,
        name: String,
        fields: HashMap<String, HybridValue>,
    ) -> Result<HybridValue> {
        let heap_struct = HeapStruct {
            name,
            fields,
            struct_type: None,
        };
        let handle = self.gc.allocate(heap_struct)?;
        Ok(HybridValue::Struct(handle))
    }

    /// Create a closure with GC allocation
    pub fn closure(
        &self,
        params: Vec<(String, TypeAnnotation)>,
        body: Vec<Statement>,
        captured_vars: HashMap<String, HybridValue>,
    ) -> Result<HybridValue> {
        let heap_closure = HeapClosure {
            params,
            body,
            captured_vars,
            function_type: None,
        };
        let handle = self.gc.allocate(heap_closure)?;
        Ok(HybridValue::Closure(handle))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_immediate_values() {
        let int_val = HybridValue::Integer(42);
        let float_val = HybridValue::Float(3.14);
        let bool_val = HybridValue::Boolean(true);
        let char_val = HybridValue::Character('A');

        assert!(int_val.is_immediate());
        assert!(float_val.is_immediate());
        assert!(bool_val.is_immediate());
        assert!(char_val.is_immediate());

        assert!(!int_val.needs_gc_tracking());
        assert!(!float_val.needs_gc_tracking());
        assert!(!bool_val.needs_gc_tracking());
        assert!(!char_val.needs_gc_tracking());
    }

    #[test]
    fn test_small_string() {
        let small_str = HybridValue::new_string("hello".to_string());

        assert!(small_str.is_immediate());
        assert!(!small_str.needs_gc_tracking());

        if let HybridValue::SmallString { data, len } = small_str {
            assert_eq!(len, 5);
            assert_eq!(&data[..5], b"hello");
        } else {
            panic!("Expected SmallString variant");
        }
    }

    #[test]
    fn test_small_tuple() {
        let elements = vec![HybridValue::Integer(1), HybridValue::Integer(2)];
        let tuple = HybridValue::new_tuple(elements.clone());

        assert!(tuple.is_immediate());

        if let HybridValue::SmallTuple {
            elements: stored, ..
        } = tuple
        {
            assert_eq!(stored, elements);
        } else {
            panic!("Expected SmallTuple variant");
        }
    }

    #[test]
    fn test_value_equality() {
        let val1 = HybridValue::Integer(42);
        let val2 = HybridValue::Integer(42);
        let val3 = HybridValue::Integer(43);

        assert_eq!(val1, val2);
        assert_ne!(val1, val3);
    }

    #[test]
    fn test_size_estimation() {
        assert_eq!(HybridValue::Integer(42).size_bytes(), 8);
        assert_eq!(HybridValue::Float(3.14).size_bytes(), 8);
        assert_eq!(HybridValue::Boolean(true).size_bytes(), 1);
        assert_eq!(HybridValue::Character('A').size_bytes(), 4);

        let small_str = HybridValue::new_string("hello".to_string());
        assert_eq!(small_str.size_bytes(), 5);
    }
}
