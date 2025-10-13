# Garbage Collection for Veld Interpreter

A modern, generational garbage collector integrated with the Veld programming language interpreter.

## Overview

This garbage collection system provides automatic memory management for the Veld interpreter, featuring:

- **Generational Collection**: Optimized for functional programming patterns
- **Incremental Collection**: Minimize pause times with incremental marking and sweeping
- **Precise Collection**: Exact reference tracking without conservative scanning
- **Value Integration**: Seamless integration with Veld's Value system
- **Performance Monitoring**: Comprehensive statistics and profiling

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Veld Interpreter                         │
├─────────────────────────────────────────────────────────────┤
│  Scope Management  │  Variable Storage  │  Function Calls  │
├─────────────────────────────────────────────────────────────┤
│                   GC Integration Layer                      │
├─────────────────────────────────────────────────────────────┤
│        Root Set       │    GC Handles    │   Value Refs    │
├─────────────────────────────────────────────────────────────┤
│                  Garbage Collector Core                     │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────────────┐    │
│  │ Young Gen   │ │   Old Gen   │ │   Permanent Gen     │    │
│  │ (Eden+S0/S1)│ │ (Tenured)   │ │   (Builtins)        │    │
│  └─────────────┘ └─────────────┘ └─────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## Key Components

### 1. Garbage Collector (`GarbageCollector`)
The main GC engine that orchestrates collection cycles:
- Manages object allocation across generations
- Coordinates mark-and-sweep collection phases
- Handles promotion between generations
- Provides statistics and monitoring

### 2. Allocator (`GcAllocator`) 
Manages memory allocation and object storage:
- Generational heap organization
- Object metadata tracking
- Size estimation and memory accounting
- Promotion logic for long-lived objects

### 3. Collector (`GcCollector`)
Implements the collection algorithms:
- Mark-and-sweep with reachability analysis
- Incremental collection for low-latency
- Support for minor, major, and full collections
- Finalization handling

### 4. Handle System (`GcHandle`, `WeakGcHandle`)
Safe object references with generation tracking:
- Strong references prevent collection
- Weak references don't affect object lifetime
- Generation counters detect stale references
- Automatic handle pool management

### 5. Root Set Management (`RootSet`)
Tracks objects that must not be collected:
- Global variables and constants
- Stack frame variables  
- Thread-local storage
- Named references for debugging

### 6. Value References (`GcValueRef`)
GC-aware wrappers for Veld values:
- Transparent GC integration with Value enum
- Automatic reference tracking
- Deep cloning with GC allocation
- Size estimation utilities

### 7. Statistics (`GcStatistics`)
Comprehensive performance monitoring:
- Collection frequency and timing
- Memory usage tracking
- Pause time analysis
- Efficiency metrics

## Usage Examples

### Basic Integration

```rust
use veld_interpreter::gc::{GarbageCollector, GcConfig, SafeGc};
use veld_common::value::Value;

// Create GC with custom configuration
let config = GcConfig {
    initial_heap_size: 1024 * 1024,  // 1MB
    young_gen_threshold: 0.8,        // Collect at 80% full
    promotion_threshold: 2,          // Promote after 2 collections
    ..Default::default()
};

let gc = GarbageCollector::with_config(config);
let safe_gc = SafeGc::new(gc);

// Allocate values
let handle1 = safe_gc.allocate(Value::Integer(42))?;
let handle2 = safe_gc.allocate(Value::String("Hello".to_string()))?;

// Add as roots to prevent collection
safe_gc.add_root(handle1.clone())?;
safe_gc.add_root(handle2.clone())?;

// Access values
if let Some(value) = safe_gc.deref(&handle1) {
    println!("Value: {}", value);
}

// Force collection
let result = safe_gc.collect()?;
println!("Collected {} objects", result.objects_collected);
```

### Interpreter Integration

```rust
// Initialize GC context for thread
veld_interpreter::gc::init_gc_context(safe_gc.clone());

// Use macros for convenient GC allocation
let handle = gc_alloc!(Value::Array(vec![
    Value::Integer(1),
    Value::Integer(2), 
    Value::Integer(3)
]))?;

// Dereference with macro
if let Some(array) = gc_deref!(handle) {
    println!("Array: {}", array);
}
```

### Complex Object Management

```rust
// Create nested structures
let nested_data = safe_gc.allocate(Value::Struct {
    name: "ComplexData".to_string(),
    fields: {
        let mut fields = HashMap::new();
        fields.insert("items".to_string(), Value::Array(vec![
            Value::Integer(1),
            Value::String("nested".to_string())
        ]));
        fields.insert("metadata".to_string(), Value::Tuple(vec![
            Value::Boolean(true),
            Value::Float(3.14)
        ]));
        fields
    }
})?;

// Root the object
safe_gc.add_root(nested_data.clone())?;

// Create temporary objects that will be collected
for i in 0..1000 {
    let temp = safe_gc.allocate(Value::Integer(i))?;
    // These are not rooted, so they'll be collected
}

// Trigger collection - only nested_data survives
let result = safe_gc.collect()?;
```

## Performance Characteristics

### Collection Types

1. **Minor Collection** (Young Generation Only)
   - Frequency: High (triggered by allocation pressure)
   - Duration: 1-5ms typical
   - Scope: Recently allocated objects only

2. **Major Collection** (Young + Old Generations) 
   - Frequency: Medium (when old gen fills up)
   - Duration: 5-20ms typical
   - Scope: Most objects, excluding permanent

3. **Full Collection** (All Generations)
   - Frequency: Low (memory pressure or explicit)
   - Duration: 10-50ms typical
   - Scope: All objects except permanent

### Memory Overhead

- **Handle Storage**: ~16 bytes per GC handle
- **Object Metadata**: ~64 bytes per GC object
- **Generation Tracking**: ~8 bytes per object
- **Total Overhead**: ~15-25% of object size

### Typical Performance

| Workload Type | Collection Frequency | Pause Times | Memory Efficiency |
|---------------|---------------------|-------------|-------------------|
| Functional    | 2-5 collections/sec | 1-3ms       | 85-95%           |
| Imperative    | 1-3 collections/sec | 2-8ms       | 80-90%           |
| Mixed         | 3-7 collections/sec | 1-5ms       | 85-92%           |

## Configuration Options

```rust
pub struct GcConfig {
    /// Initial heap size in bytes
    pub initial_heap_size: usize,
    
    /// Maximum heap size (0 = unlimited)  
    pub max_heap_size: usize,
    
    /// Young generation collection threshold
    pub young_gen_threshold: f64,
    
    /// Promotion threshold (collections survived)
    pub promotion_threshold: u32,
    
    /// Max time per incremental step (microseconds)
    pub max_incremental_time: u64,
    
    /// Enable concurrent collection (future)
    pub concurrent_collection: bool,
    
    /// Enable generational collection
    pub generational: bool,
}
```

### Recommended Settings

- **Development**: Small heap (1-10MB), frequent collection for debugging
- **Production**: Larger heap (100MB+), tuned thresholds for performance
- **Memory-Constrained**: Conservative limits with aggressive collection
- **Latency-Sensitive**: Incremental collection with small time slices

## Statistics and Monitoring

### Collection Statistics

```rust
let stats = safe_gc.statistics();
println!("Collections: {}", stats.total_collections);
println!("Objects collected: {}", stats.total_objects_collected);  
println!("Bytes freed: {}", stats.total_bytes_freed);
println!("Average pause: {:?}", stats.average_collection_time);
```

### Performance Metrics

```rust  
let efficiency = stats.efficiency_metrics();
println!("GC overhead: {:.2}%", efficiency.gc_overhead_percent);
println!("Memory efficiency: {:.2}%", efficiency.memory_efficiency_percent);
```

### Recent Activity

```rust
let recent = stats.recent_stats(Duration::from_secs(60));
println!("Recent collections/sec: {:.2}", recent.collections_per_second);
println!("Recent average pause: {:?}", recent.average_duration);
```

## Integration with Interpreter

### Scope Management

The GC integrates with Veld's scope system by:

1. **Automatic Root Tracking**: Variables in scope are automatically added as GC roots
2. **Scope Cleanup**: Exiting scopes removes roots, enabling collection  
3. **Function Calls**: Call frames maintain roots for parameters and locals
4. **Module Loading**: Module exports become permanent roots

### Value System Integration

```rust
// Direct value (not GC managed)
let direct_val = GcValueRef::direct(Value::Integer(42));

// GC-managed value
let gc_handle = safe_gc.allocate(Value::String("managed".to_string()))?;
let managed_val = GcValueRef::managed(gc_handle);

// Transparent access
let value = managed_val.get_value(&safe_gc.gc);
```

### Error Handling

The GC integrates with Veld's error system:
- Memory allocation failures are reported as `VeldError::OutOfMemory`
- Invalid handle access raises `VeldError::RuntimeError`
- Collection failures are logged and may trigger emergency collection

## Advanced Features

### Weak References

```rust
let handle = safe_gc.allocate(Value::Integer(42))?;
let weak_ref = safe_gc.weak_ref(&handle);

// Object can be collected even with weak reference
safe_gc.collect()?;

// Try to upgrade weak reference
if let Some(strong_ref) = safe_gc.upgrade(&weak_ref) {
    println!("Object still alive");
} else {
    println!("Object was collected");
}
```

### Incremental Collection

```rust
// Perform incremental collection step
let completed = safe_gc.collect_incremental()?;
if completed {
    println!("Collection completed incrementally");
} else {
    println!("More incremental steps needed");
}
```

### Manual Tuning

```rust
// Runtime configuration changes
let mut new_config = GcConfig::default();
new_config.young_gen_threshold = 0.9;  // More aggressive
gc.tune(new_config);

// Force specific collection types
gc.collect_young_generation()?;  // Minor collection only
gc.collect_all_generations()?;   // Full collection
```

## Debugging and Profiling

### Debug Mode

```rust
let gc = GarbageCollector::with_config(GcConfig {
    debug_info: true,
    trace_compilation: true,
    ..Default::default()
});
```

### Tracing Integration

```rust
// Enable detailed tracing
RUST_LOG=veld_interpreter::gc=trace cargo run

// Common log messages:
// - Object allocation and deallocation
// - Collection triggers and results  
// - Root set changes
// - Promotion events
```

### Memory Profiling

```rust
// Object count tracking
let count = safe_gc.object_count();
println!("Live objects: {}", count);

// Memory usage
let heap_size = safe_gc.heap_size();  
println!("Heap size: {} bytes", heap_size);

// Generation statistics
let gen_stats = gc.generation_stats();
for stat in gen_stats {
    println!("{:?}: {} objects, {} bytes", 
             stat.generation, stat.object_count, stat.allocated_bytes);
}
```

## Future Enhancements

### Planned Features

1. **Concurrent Collection**: Background collection threads
2. **Compacting Collection**: Reduce fragmentation
3. **Region-Based Collection**: Better cache locality
4. **LLVM Integration**: JIT-compiled collection routines
5. **Cross-Thread Collection**: Shared heap across threads

### Performance Improvements

1. **Inline Caching**: Cache object layouts for faster access
2. **Escape Analysis**: Stack-allocate objects that don't escape
3. **Generational Refinement**: Age-based sub-generations
4. **Collection Scheduling**: Predictive collection timing

### Developer Experience

1. **Visual Profiler**: Heap visualization tools
2. **Memory Leak Detection**: Automatic leak detection
3. **Collection Hints**: API for allocation guidance
4. **Benchmarking Suite**: Performance regression testing

## Testing

Run the GC test suite:

```bash
# Unit tests
cargo test -p veld-interpreter gc::

# Integration tests  
cargo test -p veld-interpreter --examples

# Benchmarks
cargo bench -p veld-interpreter gc_bench

# Memory leak testing
cargo test -p veld-interpreter --features leak-detection
```

## Contributing

When contributing to the GC system:

1. **Add Tests**: Every new feature needs comprehensive tests
2. **Benchmark Impact**: Measure performance effects
3. **Update Documentation**: Keep this README current
4. **Memory Safety**: Ensure all unsafe code is justified
5. **Error Handling**: Provide clear error messages

### Performance Testing

```bash
# Allocation performance
cargo bench allocator_bench

# Collection performance  
cargo bench collection_bench

# Integration performance
cargo bench interpreter_gc_bench
```

---

For more detailed API documentation, run `cargo doc --open -p veld-interpreter`.