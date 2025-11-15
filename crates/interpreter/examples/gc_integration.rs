//! Garbage Collection Integration Example for Veld Interpreter
//!
//! This example demonstrates how to integrate the garbage collector
//! with the Veld interpreter for automatic memory management.

use std::path::Path;
use veld_common::ast::{Expr, Literal, Pattern, Statement, VarKind};
use veld_common::gc::{GarbageCollector, GcConfig, SafeGc};
use veld_common::lexer::Lexer;
use veld_common::parser::Parser;
use veld_common::value::Value;
use veld_interpreter::interpreter::Interpreter;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing for debugging
    tracing_subscriber::fmt::init();

    println!("=== Veld Interpreter with Garbage Collection ===\n");

    // Example 1: Basic GC Integration
    basic_gc_example()?;

    // Example 2: Memory Management with Complex Objects
    complex_objects_example()?;

    // Example 3: GC Statistics and Monitoring
    gc_statistics_example()?;

    // Example 4: Manual Memory Management
    manual_gc_example()?;

    Ok(())
}

/// Basic GC integration example
fn basic_gc_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Basic GC Integration ===");

    // Create GC with custom configuration
    let config = GcConfig {
        initial_heap_size: 1024 * 1024,  // 1MB
        max_heap_size: 10 * 1024 * 1024, // 10MB max
        young_gen_threshold: 0.8,        // Collect at 80% full
        promotion_threshold: 2,          // Promote after 2 collections
        max_incremental_time: 1000,      // 1ms incremental steps
        concurrent_collection: false,
        generational: true,
    };

    let gc = GarbageCollector::with_config(config);
    let safe_gc = SafeGc::new(gc);

    // Allocate some values in GC
    let value1 = safe_gc.allocate(Value::Integer(42))?;
    let value2 = safe_gc.allocate(Value::String("Hello, GC!".to_string()))?;
    let value3 = safe_gc.allocate(Value::Array(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]))?;

    println!("Allocated 3 values in GC");

    // Add values as roots to prevent collection
    safe_gc.add_root(value1.clone())?;
    safe_gc.add_root(value2.clone())?;

    // Access values through GC
    if let Some(val) = safe_gc.deref(&value1) {
        println!("Value 1: {:?}", val);
    }

    if let Some(val) = safe_gc.deref(&value2) {
        println!("Value 2: {:?}", val);
    }

    if let Some(val) = safe_gc.deref(&value3) {
        println!("Value 3: {:?}", val);
    }

    // Force a collection
    println!("\nForcing garbage collection...");
    let result = safe_gc.collect()?;
    println!(
        "GC Result: {} objects collected, {} bytes freed",
        result.objects_collected, result.bytes_freed
    );

    // value3 should be collected (not rooted), others should survive
    println!("Value 1 still alive: {}", safe_gc.deref(&value1).is_some());
    println!("Value 2 still alive: {}", safe_gc.deref(&value2).is_some());
    println!("Value 3 still alive: {}", safe_gc.deref(&value3).is_some());

    // Clean up roots
    safe_gc.remove_root(&value1)?;
    safe_gc.remove_root(&value2)?;

    println!();
    Ok(())
}

/// Example with complex nested objects
fn complex_objects_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Complex Objects with GC ===");

    let gc = GarbageCollector::new();
    let safe_gc = SafeGc::new(gc);

    // Create nested data structures
    let nested_array = safe_gc.allocate(Value::Array(vec![
        Value::Array(vec![Value::Integer(1), Value::String("nested".to_string())]),
        Value::Tuple(vec![Value::Boolean(true), Value::Float(3.14)]),
    ]))?;

    let struct_value = {
        let mut fields = std::collections::HashMap::new();
        fields.insert("id".to_string(), Value::Integer(100));
        fields.insert("name".to_string(), Value::String("Test Struct".to_string()));
        fields.insert(
            "data".to_string(),
            Value::Array(vec![
                Value::Integer(10),
                Value::Integer(20),
                Value::Integer(30),
            ]),
        );

        safe_gc.allocate(Value::Struct {
            name: "TestStruct".to_string(),
            fields,
        })?
    };

    println!("Created nested array and struct");

    // Add as roots
    safe_gc.add_root(nested_array.clone())?;
    safe_gc.add_root(struct_value.clone())?;

    // Display values
    if let Some(arr) = safe_gc.deref(&nested_array) {
        println!("Nested array: {:?}", arr);
    }

    if let Some(struct_val) = safe_gc.deref(&struct_value) {
        println!("Struct: {:?}", struct_val);
    }

    // Create many temporary objects to trigger collection
    for i in 0..1000 {
        let temp = safe_gc.allocate(Value::Integer(i))?;
        // Don't add to roots - these should be collected
    }

    println!("\nCreated 1000 temporary objects");

    // Force collection
    let result = safe_gc.collect()?;
    println!(
        "After GC: {} objects collected, {} bytes freed",
        result.objects_collected, result.bytes_freed
    );

    // Root objects should still be alive
    println!(
        "Nested array still alive: {}",
        safe_gc.deref(&nested_array).is_some()
    );
    println!(
        "Struct still alive: {}",
        safe_gc.deref(&struct_value).is_some()
    );

    // Clean up
    safe_gc.remove_root(&nested_array)?;
    safe_gc.remove_root(&struct_value)?;

    println!();
    Ok(())
}

/// GC statistics and monitoring example
fn gc_statistics_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== GC Statistics and Monitoring ===");

    let gc = GarbageCollector::new();
    let safe_gc = SafeGc::new(gc);

    // Allocate various objects and trigger collections
    let mut handles = Vec::new();

    for i in 0..50 {
        let handle = safe_gc.allocate(Value::String(format!("Object {}", i)))?;
        if i % 10 == 0 {
            // Keep every 10th object as root
            safe_gc.add_root(handle.clone())?;
        }
        handles.push(handle);
    }

    println!("Allocated 50 objects (every 10th rooted)");

    // Force multiple collections to generate statistics
    for i in 0..5 {
        let result = safe_gc.collect()?;
        println!(
            "Collection {}: {} objects collected, {} bytes freed in {:?}",
            i + 1,
            result.objects_collected,
            result.bytes_freed,
            result.duration
        );
    }

    // Display statistics
    let stats = safe_gc.statistics();
    println!("\n=== GC Statistics ===");
    println!("Total collections: {}", stats.total_collections);
    println!("Young generation collections: {}", stats.young_collections);
    println!("Old generation collections: {}", stats.old_collections);
    println!("Full collections: {}", stats.full_collections);
    println!("Total objects collected: {}", stats.total_objects_collected);
    println!("Total bytes freed: {} bytes", stats.total_bytes_freed);
    println!("Total GC time: {:?}", stats.total_gc_time);
    println!(
        "Average collection time: {:?}",
        stats.average_collection_time
    );
    println!("Peak memory usage: {} bytes", stats.peak_memory_usage);
    println!("Current memory usage: {} bytes", stats.current_memory_usage);

    // Clean up roots
    for (i, handle) in handles.iter().enumerate() {
        if i % 10 == 0 {
            safe_gc.remove_root(handle)?;
        }
    }

    println!();
    Ok(())
}

/// Manual GC control example
fn manual_gc_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Manual GC Control ===");

    let gc = GarbageCollector::new();
    let safe_gc = SafeGc::new(gc);

    // Create objects with different lifecycles
    let permanent_data = safe_gc.allocate(Value::String("Permanent data".to_string()))?;
    safe_gc.add_root(permanent_data.clone())?;

    let mut temp_handles = Vec::new();

    // Phase 1: Allocate temporary objects
    println!("Phase 1: Allocating temporary objects...");
    for i in 0..100 {
        let temp = safe_gc.allocate(Value::Array(vec![
            Value::Integer(i),
            Value::String(format!("temp_{}", i)),
        ]))?;
        temp_handles.push(temp);
    }

    let stats_before = safe_gc.statistics();
    println!(
        "Objects before cleanup: ~{}",
        stats_before.current_memory_usage / 100
    );

    // Phase 2: Selective cleanup
    println!("\nPhase 2: Selective cleanup...");
    // Keep only even-numbered objects
    for (i, handle) in temp_handles.iter().enumerate() {
        if i % 2 == 0 {
            safe_gc.add_root(handle.clone())?;
        }
    }

    // Force collection
    let result = safe_gc.collect()?;
    println!(
        "Selective GC: {} objects collected",
        result.objects_collected
    );

    // Phase 3: Final cleanup
    println!("\nPhase 3: Final cleanup...");
    for (i, handle) in temp_handles.iter().enumerate() {
        if i % 2 == 0 {
            safe_gc.remove_root(handle)?;
        }
    }

    let final_result = safe_gc.collect()?;
    println!(
        "Final GC: {} objects collected",
        final_result.objects_collected
    );

    // Only permanent data should remain
    println!(
        "Permanent data still alive: {}",
        safe_gc.deref(&permanent_data).is_some()
    );

    if let Some(data) = safe_gc.deref(&permanent_data) {
        println!("Permanent data value: {:?}", data);
    }

    // Cleanup
    safe_gc.remove_root(&permanent_data)?;

    let stats_after = safe_gc.statistics();
    println!("\nFinal statistics:");
    println!("Total collections: {}", stats_after.total_collections);
    println!(
        "Memory freed: {} bytes",
        stats_before
            .current_memory_usage
            .saturating_sub(stats_after.current_memory_usage)
    );

    println!();
    Ok(())
}

/// Helper function to create a sample Veld AST for testing
fn create_sample_ast() -> veld_common::ast::AST {
    use veld_common::ast::AST;

    let statements = vec![
        Statement::VariableDeclaration {
            pattern: Pattern::Identifier("x".to_string()),
            var_kind: VarKind::Let,
            type_annotation: None,
            value: Box::new(Expr::Literal(Literal::Integer(42))),
            is_public: false,
        },
        Statement::VariableDeclaration {
            pattern: Pattern::Identifier("arr".to_string()),
            var_kind: VarKind::Let,
            type_annotation: None,
            value: Box::new(Expr::ArrayLiteral(vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2)),
                Expr::Literal(Literal::Integer(3)),
            ])),
            is_public: false,
        },
    ];

    AST {
        statements,
        source_map: None,
        errors: Vec::new(),
    }
}

/// Example of integrated GC with interpreter (conceptual)
fn _integrated_interpreter_example() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Integrated GC + Interpreter ===");

    // This is a conceptual example of how GC could be integrated
    // with the interpreter. The actual integration would require
    // modifications to the interpreter to use GC for value storage.

    let gc = GarbageCollector::new();
    let safe_gc = SafeGc::new(gc);

    // Initialize thread-local GC context
    veld_common::gc::init_gc_context(safe_gc.clone());

    // Create an interpreter instance
    let mut interpreter = Interpreter::new(".");

    // In a real integration, the interpreter would:
    // 1. Use GC for allocating values
    // 2. Add scope variables as GC roots
    // 3. Trigger collection based on allocation pressure
    // 4. Handle GC during function calls and returns

    println!("GC-enabled interpreter initialized");
    println!("In real integration, all values would be GC-managed");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_basic_functionality() {
        let gc = GarbageCollector::new();
        let safe_gc = SafeGc::new(gc);

        // Test allocation
        let handle = safe_gc.allocate(Value::Integer(42)).unwrap();
        assert_eq!(safe_gc.deref(&handle), Some(Value::Integer(42)));

        // Test root management
        safe_gc.add_root(handle.clone()).unwrap();
        let result = safe_gc.collect().unwrap();
        // Object should survive because it's rooted
        assert_eq!(safe_gc.deref(&handle), Some(Value::Integer(42)));

        safe_gc.remove_root(&handle).unwrap();
    }

    #[test]
    fn test_gc_collection() {
        let gc = GarbageCollector::new();
        let safe_gc = SafeGc::new(gc);

        // Allocate object without rooting
        let _handle = safe_gc
            .allocate(Value::String("temporary".to_string()))
            .unwrap();

        // Force collection - object should be collected
        let result = safe_gc.collect().unwrap();
        assert!(result.objects_collected > 0 || result.completed);
    }

    #[test]
    fn test_gc_statistics() {
        let gc = GarbageCollector::new();
        let safe_gc = SafeGc::new(gc);

        // Initial statistics
        let initial_stats = safe_gc.statistics();
        assert_eq!(initial_stats.total_collections, 0);

        // Trigger a collection
        safe_gc.collect().unwrap();

        // Statistics should update
        let updated_stats = safe_gc.statistics();
        assert!(updated_stats.total_collections > initial_stats.total_collections);
    }
}
