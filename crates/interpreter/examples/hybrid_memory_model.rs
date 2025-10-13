//! Hybrid Memory Model Proof of Concept
//!
//! This example demonstrates Veld's OCaml/C#-inspired memory model:
//! - Immediate values (stored directly, no GC overhead)
//! - Reference values (heap-allocated with GC management)
//! - Automatic optimization based on size and type
//! - Clear performance characteristics

use std::collections::HashMap;
use std::fmt;
use std::time::Instant;

/// Maximum size for inline string storage (22 bytes + length byte)
const MAX_INLINE_STRING_SIZE: usize = 22;

/// Maximum size for inline tuple storage (in bytes)
const MAX_INLINE_TUPLE_SIZE: usize = 32;

/// Hybrid value representation supporting both immediate and reference semantics
#[derive(Debug, Clone, PartialEq)]
pub enum HybridValue {
    // === Immediate Values (No GC overhead) ===
    /// 64-bit signed integer - stored directly on stack
    Integer(i64),

    /// 64-bit floating point - stored directly on stack
    Float(f64),

    /// Boolean value - stored directly (1 byte)
    Boolean(bool),

    /// Unicode character - stored directly (4 bytes)
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

    // === Reference Values (Simulated GC) ===
    /// Large string (heap allocated) - contains ID for lookup
    String(usize), // Reference ID

    /// Array of values - contains ID for lookup
    Array(usize), // Reference ID

    /// Tuple (when too large for inline storage) - contains ID for lookup
    Tuple(usize), // Reference ID

    /// Struct instance - contains ID for lookup
    Struct(usize), // Reference ID
}

/// Heap-allocated string data
#[derive(Debug, Clone)]
pub struct HeapString {
    pub data: String,
    pub refs: usize, // Reference count for demonstration
}

/// Heap-allocated array data
#[derive(Debug, Clone)]
pub struct HeapArray {
    pub elements: Vec<HybridValue>,
    pub refs: usize,
}

/// Heap-allocated tuple data
#[derive(Debug, Clone)]
pub struct HeapTuple {
    pub elements: Vec<HybridValue>,
    pub refs: usize,
}

/// Heap-allocated struct data
#[derive(Debug, Clone)]
pub struct HeapStruct {
    pub name: String,
    pub fields: HashMap<String, HybridValue>,
    pub refs: usize,
}

/// Simulated garbage collector and heap manager
#[derive(Debug)]
pub struct HybridHeap {
    strings: HashMap<usize, HeapString>,
    arrays: HashMap<usize, HeapArray>,
    tuples: HashMap<usize, HeapTuple>,
    structs: HashMap<usize, HeapStruct>,
    next_id: usize,

    // Statistics
    pub total_allocations: usize,
    pub immediate_values_created: usize,
    pub heap_objects_created: usize,
    pub bytes_allocated: usize,
}

impl HybridHeap {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            arrays: HashMap::new(),
            tuples: HashMap::new(),
            structs: HashMap::new(),
            next_id: 1,
            total_allocations: 0,
            immediate_values_created: 0,
            heap_objects_created: 0,
            bytes_allocated: 0,
        }
    }

    /// Create a string value, choosing inline vs heap storage automatically
    pub fn create_string(&mut self, s: String) -> HybridValue {
        self.total_allocations += 1;

        if s.len() <= MAX_INLINE_STRING_SIZE {
            // Store inline - no GC overhead
            self.immediate_values_created += 1;
            let mut data = [0u8; MAX_INLINE_STRING_SIZE];
            let bytes = s.as_bytes();
            data[..bytes.len()].copy_from_slice(bytes);

            HybridValue::SmallString {
                data,
                len: bytes.len() as u8,
            }
        } else {
            // Store on heap
            self.heap_objects_created += 1;
            self.bytes_allocated += s.len() + std::mem::size_of::<HeapString>();

            let id = self.next_id;
            self.next_id += 1;

            self.strings.insert(id, HeapString { data: s, refs: 1 });

            HybridValue::String(id)
        }
    }

    /// Create a tuple value, choosing inline vs heap storage automatically
    pub fn create_tuple(&mut self, elements: Vec<HybridValue>) -> HybridValue {
        self.total_allocations += 1;

        let total_size: usize = elements.iter().map(|e| e.estimated_size()).sum();

        if total_size <= MAX_INLINE_TUPLE_SIZE && elements.len() <= 4 {
            // Store inline
            self.immediate_values_created += 1;
            HybridValue::SmallTuple {
                elements,
                size_bytes: total_size as u16,
            }
        } else {
            // Store on heap
            self.heap_objects_created += 1;
            self.bytes_allocated += total_size + std::mem::size_of::<HeapTuple>();

            let id = self.next_id;
            self.next_id += 1;

            self.tuples.insert(id, HeapTuple { elements, refs: 1 });

            HybridValue::Tuple(id)
        }
    }

    /// Create an array value (always heap-allocated for simplicity)
    pub fn create_array(&mut self, elements: Vec<HybridValue>) -> HybridValue {
        self.total_allocations += 1;
        self.heap_objects_created += 1;

        let size = elements.len() * 8 + std::mem::size_of::<HeapArray>(); // Rough estimate
        self.bytes_allocated += size;

        let id = self.next_id;
        self.next_id += 1;

        self.arrays.insert(id, HeapArray { elements, refs: 1 });

        HybridValue::Array(id)
    }

    /// Create a struct value (always heap-allocated)
    pub fn create_struct(
        &mut self,
        name: String,
        fields: HashMap<String, HybridValue>,
    ) -> HybridValue {
        self.total_allocations += 1;
        self.heap_objects_created += 1;

        let size = name.len() + fields.len() * 16 + std::mem::size_of::<HeapStruct>(); // Rough estimate
        self.bytes_allocated += size;

        let id = self.next_id;
        self.next_id += 1;

        self.structs.insert(
            id,
            HeapStruct {
                name,
                fields,
                refs: 1,
            },
        );

        HybridValue::Struct(id)
    }

    /// Get string data (handles both inline and heap strings)
    pub fn get_string(&self, value: &HybridValue) -> Option<String> {
        match value {
            HybridValue::SmallString { data, len } => {
                let bytes = &data[..*len as usize];
                String::from_utf8(bytes.to_vec()).ok()
            }
            HybridValue::String(id) => self.strings.get(id).map(|s| s.data.clone()),
            _ => None,
        }
    }

    /// Get array elements
    pub fn get_array(&self, value: &HybridValue) -> Option<&Vec<HybridValue>> {
        match value {
            HybridValue::Array(id) => self.arrays.get(id).map(|arr| &arr.elements),
            _ => None,
        }
    }

    /// Get tuple elements (handles both inline and heap tuples)
    pub fn get_tuple<'a>(&'a self, value: &'a HybridValue) -> Option<&'a Vec<HybridValue>> {
        match value {
            HybridValue::SmallTuple { elements, .. } => Some(elements),
            HybridValue::Tuple(id) => self.tuples.get(id).map(|tuple| &tuple.elements),
            _ => None,
        }
    }

    /// Get struct fields
    pub fn get_struct(
        &self,
        value: &HybridValue,
    ) -> Option<(&String, &HashMap<String, HybridValue>)> {
        match value {
            HybridValue::Struct(id) => self.structs.get(id).map(|s| (&s.name, &s.fields)),
            _ => None,
        }
    }

    /// Perform garbage collection (simplified mark-and-sweep)
    pub fn collect_garbage(&mut self) -> (usize, usize) {
        let initial_objects =
            self.strings.len() + self.arrays.len() + self.tuples.len() + self.structs.len();
        let initial_bytes = self.bytes_allocated;

        // In a real implementation, we'd mark reachable objects from roots
        // For this demo, we'll just clean up objects with zero references
        self.strings.retain(|_, obj| obj.refs > 0);
        self.arrays.retain(|_, obj| obj.refs > 0);
        self.tuples.retain(|_, obj| obj.refs > 0);
        self.structs.retain(|_, obj| obj.refs > 0);

        let final_objects =
            self.strings.len() + self.arrays.len() + self.tuples.len() + self.structs.len();
        let objects_collected = initial_objects - final_objects;

        // Recalculate bytes (simplified)
        self.bytes_allocated = self
            .strings
            .values()
            .map(|s| s.data.len() + std::mem::size_of::<HeapString>())
            .sum::<usize>()
            + self
                .arrays
                .values()
                .map(|a| a.elements.len() * 8 + std::mem::size_of::<HeapArray>())
                .sum::<usize>()
            + self
                .tuples
                .values()
                .map(|t| t.elements.len() * 8 + std::mem::size_of::<HeapTuple>())
                .sum::<usize>()
            + self
                .structs
                .values()
                .map(|s| s.name.len() + s.fields.len() * 16 + std::mem::size_of::<HeapStruct>())
                .sum::<usize>();

        let bytes_freed = initial_bytes - self.bytes_allocated;

        (objects_collected, bytes_freed)
    }

    pub fn print_stats(&self) {
        println!("\n=== Hybrid Memory Model Statistics ===");
        println!("Total allocations: {}", self.total_allocations);
        println!(
            "Immediate values created: {} ({:.1}%)",
            self.immediate_values_created,
            100.0 * self.immediate_values_created as f64 / self.total_allocations as f64
        );
        println!(
            "Heap objects created: {} ({:.1}%)",
            self.heap_objects_created,
            100.0 * self.heap_objects_created as f64 / self.total_allocations as f64
        );
        println!(
            "Current heap objects: {}",
            self.strings.len() + self.arrays.len() + self.tuples.len() + self.structs.len()
        );
        println!("Bytes allocated: {} bytes", self.bytes_allocated);
        println!("Heap breakdown:");
        println!("  - Strings: {}", self.strings.len());
        println!("  - Arrays: {}", self.arrays.len());
        println!("  - Tuples: {}", self.tuples.len());
        println!("  - Structs: {}", self.structs.len());
    }
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
        )
    }

    /// Get the estimated size in bytes
    pub fn estimated_size(&self) -> usize {
        match self {
            HybridValue::Integer(_) => 8,
            HybridValue::Float(_) => 8,
            HybridValue::Boolean(_) => 1,
            HybridValue::Character(_) => 4,
            HybridValue::SmallString { len, .. } => *len as usize,
            HybridValue::SmallTuple { size_bytes, .. } => *size_bytes as usize,
            HybridValue::Unit => 0,
            // Reference types - just the reference size
            HybridValue::String(_)
            | HybridValue::Array(_)
            | HybridValue::Tuple(_)
            | HybridValue::Struct(_) => 8,
        }
    }

    /// Get a display representation with heap lookup
    pub fn display_with_heap(&self, heap: &HybridHeap) -> String {
        match self {
            HybridValue::Integer(i) => i.to_string(),
            HybridValue::Float(f) => f.to_string(),
            HybridValue::Boolean(b) => b.to_string(),
            HybridValue::Character(c) => format!("'{}'", c),
            HybridValue::Unit => "()".to_string(),

            HybridValue::SmallString { data, len } => {
                let bytes = &data[..*len as usize];
                if let Ok(s) = String::from_utf8(bytes.to_vec()) {
                    format!("\"{}\"", s)
                } else {
                    "<invalid string>".to_string()
                }
            }

            HybridValue::SmallTuple { elements, .. } => {
                let element_strs: Vec<String> =
                    elements.iter().map(|e| e.display_with_heap(heap)).collect();
                format!("({})", element_strs.join(", "))
            }

            HybridValue::String(id) => {
                if let Some(s) = heap.strings.get(id) {
                    format!("\"{}\"", s.data)
                } else {
                    format!("<invalid string ref {}>", id)
                }
            }

            HybridValue::Array(id) => {
                if let Some(arr) = heap.arrays.get(id) {
                    let element_strs: Vec<String> = arr
                        .elements
                        .iter()
                        .map(|e| e.display_with_heap(heap))
                        .collect();
                    format!("[{}]", element_strs.join(", "))
                } else {
                    format!("<invalid array ref {}>", id)
                }
            }

            HybridValue::Tuple(id) => {
                if let Some(tuple) = heap.tuples.get(id) {
                    let element_strs: Vec<String> = tuple
                        .elements
                        .iter()
                        .map(|e| e.display_with_heap(heap))
                        .collect();
                    format!("({})", element_strs.join(", "))
                } else {
                    format!("<invalid tuple ref {}>", id)
                }
            }

            HybridValue::Struct(id) => {
                if let Some(s) = heap.structs.get(id) {
                    let field_strs: Vec<String> = s
                        .fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v.display_with_heap(heap)))
                        .collect();
                    format!("{} {{ {} }}", s.name, field_strs.join(", "))
                } else {
                    format!("<invalid struct ref {}>", id)
                }
            }
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
            HybridValue::Unit => write!(f, "()"),

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

            // Reference types show their ID
            HybridValue::String(id) => write!(f, "<string#{}>", id),
            HybridValue::Array(id) => write!(f, "<array#{}>", id),
            HybridValue::Tuple(id) => write!(f, "<tuple#{}>", id),
            HybridValue::Struct(id) => write!(f, "<struct#{}>", id),
        }
    }
}

// Performance and correctness tests
fn main() {
    println!("=== Veld Hybrid Memory Model Demo ===\n");

    let mut heap = HybridHeap::new();

    // Demonstrate immediate values (no heap allocation)
    println!("1. Creating immediate values (stack-allocated):");
    let int_val = HybridValue::Integer(42);
    let float_val = HybridValue::Float(3.14159);
    let bool_val = HybridValue::Boolean(true);
    let char_val = HybridValue::Character('🦀');
    let unit_val = HybridValue::Unit;

    println!(
        "   Integer: {} (immediate: {})",
        int_val,
        int_val.is_immediate()
    );
    println!(
        "   Float: {} (immediate: {})",
        float_val,
        float_val.is_immediate()
    );
    println!(
        "   Boolean: {} (immediate: {})",
        bool_val,
        bool_val.is_immediate()
    );
    println!(
        "   Character: {} (immediate: {})",
        char_val,
        char_val.is_immediate()
    );
    println!(
        "   Unit: {} (immediate: {})",
        unit_val,
        unit_val.is_immediate()
    );

    // Demonstrate string storage optimization
    println!("\n2. String storage optimization:");
    let small_str = heap.create_string("Hello!".to_string()); // Should be inline
    let large_str = heap.create_string(
        "This is a very long string that exceeds the inline storage limit".to_string(),
    ); // Should be heap

    println!(
        "   Small string: {} (immediate: {})",
        small_str.display_with_heap(&heap),
        small_str.is_immediate()
    );
    println!(
        "   Large string: {} (immediate: {})",
        large_str.display_with_heap(&heap),
        large_str.is_immediate()
    );

    // Demonstrate tuple storage optimization
    println!("\n3. Tuple storage optimization:");
    let small_tuple = heap.create_tuple(vec![HybridValue::Integer(1), HybridValue::Integer(2)]); // Should be inline

    let large_tuple = heap.create_tuple(vec![
        HybridValue::Integer(1),
        HybridValue::Integer(2),
        HybridValue::Integer(3),
        HybridValue::Integer(4),
        HybridValue::Integer(5),
        HybridValue::Integer(6),
    ]); // Should be heap

    println!(
        "   Small tuple: {} (immediate: {})",
        small_tuple.display_with_heap(&heap),
        small_tuple.is_immediate()
    );
    println!(
        "   Large tuple: {} (immediate: {})",
        large_tuple.display_with_heap(&heap),
        large_tuple.is_immediate()
    );

    // Demonstrate complex data structures
    println!("\n4. Complex data structures (always heap-allocated):");
    let array = heap.create_array(vec![
        HybridValue::Integer(10),
        HybridValue::Integer(20),
        HybridValue::Integer(30),
    ]);

    let mut struct_fields = HashMap::new();
    struct_fields.insert("name".to_string(), heap.create_string("Alice".to_string()));
    struct_fields.insert("age".to_string(), HybridValue::Integer(30));
    struct_fields.insert("active".to_string(), HybridValue::Boolean(true));

    let person_struct = heap.create_struct("Person".to_string(), struct_fields);

    println!(
        "   Array: {} (immediate: {})",
        array.display_with_heap(&heap),
        array.is_immediate()
    );
    println!(
        "   Struct: {} (immediate: {})",
        person_struct.display_with_heap(&heap),
        person_struct.is_immediate()
    );

    // Performance comparison
    println!("\n5. Performance comparison:");

    // Test immediate value performance
    let start = Instant::now();
    let mut _immediate_sum = 0i64;
    for i in 0..1_000_000 {
        let val = HybridValue::Integer(i);
        if let HybridValue::Integer(n) = val {
            _immediate_sum += n;
        }
    }
    let immediate_time = start.elapsed();

    // Test reference value performance (simulated)
    let start = Instant::now();
    let mut _reference_sum = 0i64;
    for i in 0..1_000_000 {
        let val = HybridValue::String(i as usize); // Simulate reference
        if let HybridValue::String(id) = val {
            _reference_sum += id as i64;
        }
    }
    let reference_time = start.elapsed();

    println!("   Immediate values (1M operations): {:?}", immediate_time);
    println!("   Reference values (1M operations): {:?}", reference_time);
    println!(
        "   Performance ratio: {:.2}x faster for immediate values",
        reference_time.as_nanos() as f64 / immediate_time.as_nanos() as f64
    );

    // Memory usage and GC demonstration
    println!("\n6. Memory usage and garbage collection:");
    heap.print_stats();

    // Create many temporary objects
    println!("\nCreating 1000 temporary strings...");
    let mut temp_values = Vec::new();
    for i in 0..1000 {
        let temp_str = heap.create_string(format!("temp_string_{}", i));
        temp_values.push(temp_str);
    }

    heap.print_stats();

    // Drop references (simulate going out of scope)
    temp_values.clear();

    println!("\nAfter dropping references and collecting garbage:");
    let (objects_collected, bytes_freed) = heap.collect_garbage();
    println!(
        "Collected {} objects, freed {} bytes",
        objects_collected, bytes_freed
    );
    heap.print_stats();

    // Demonstrate type safety and memory safety
    println!("\n7. Type safety and memory safety:");
    println!("   ✓ Immediate values are always valid (stored by value)");
    println!("   ✓ Reference values are checked at access time");
    println!("   ✓ No manual memory management required");
    println!("   ✓ Automatic storage optimization based on size");

    println!("\n=== Summary ===");
    println!("This hybrid memory model provides:");
    println!("• OCaml/C#-style automatic memory management");
    println!("• Performance optimization through immediate values");
    println!("• Transparent storage decisions (inline vs heap)");
    println!("• Garbage collection for complex objects");
    println!("• Type safety and memory safety by default");
    println!("• Optional manual control for advanced users (not shown)");

    heap.print_stats();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_immediate_values() {
        let int_val = HybridValue::Integer(42);
        let float_val = HybridValue::Float(3.14);
        let bool_val = HybridValue::Boolean(true);

        assert!(int_val.is_immediate());
        assert!(float_val.is_immediate());
        assert!(bool_val.is_immediate());

        assert_eq!(int_val.estimated_size(), 8);
        assert_eq!(float_val.estimated_size(), 8);
        assert_eq!(bool_val.estimated_size(), 1);
    }

    #[test]
    fn test_string_optimization() {
        let mut heap = HybridHeap::new();

        // Small string should be immediate
        let small = heap.create_string("hello".to_string());
        assert!(small.is_immediate());

        // Large string should be heap-allocated
        let large = heap
            .create_string("this is a very long string that exceeds the inline limit".to_string());
        assert!(!large.is_immediate());

        // Should be able to retrieve both
        assert_eq!(heap.get_string(&small), Some("hello".to_string()));
        assert!(heap.get_string(&large).is_some());
    }

    #[test]
    fn test_tuple_optimization() {
        let mut heap = HybridHeap::new();

        // Small tuple should be immediate
        let small = heap.create_tuple(vec![HybridValue::Integer(1), HybridValue::Integer(2)]);
        assert!(small.is_immediate());

        // Large tuple should be heap-allocated
        let large = heap.create_tuple(vec![HybridValue::Integer(1); 10]);
        assert!(!large.is_immediate());

        // Should be able to retrieve elements
        assert!(heap.get_tuple(&small).is_some());
        assert!(heap.get_tuple(&large).is_some());
    }

    #[test]
    fn test_complex_structures() {
        let mut heap = HybridHeap::new();

        let array = heap.create_array(vec![HybridValue::Integer(1), HybridValue::Integer(2)]);
        assert!(!array.is_immediate()); // Arrays are always heap-allocated

        let mut fields = HashMap::new();
        fields.insert("x".to_string(), HybridValue::Integer(10));
        let struct_val = heap.create_struct("Point".to_string(), fields);
        assert!(!struct_val.is_immediate()); // Structs are always heap-allocated

        assert!(heap.get_array(&array).is_some());
        assert!(heap.get_struct(&struct_val).is_some());
    }

    #[test]
    fn test_memory_stats() {
        let mut heap = HybridHeap::new();

        let initial_allocations = heap.total_allocations;

        // Create immediate value
        let _immediate = HybridValue::Integer(42);
        // Note: immediate values don't go through heap, so stats unchanged
        assert_eq!(heap.total_allocations, initial_allocations);

        // Create heap value (string must be > 22 bytes to trigger heap allocation)
        let _heap_val = heap.create_string(
            "this is a very long string that exceeds the inline storage limit".to_string(),
        );
        assert_eq!(heap.total_allocations, initial_allocations + 1);

        assert!(heap.bytes_allocated > 0);
    }
}
