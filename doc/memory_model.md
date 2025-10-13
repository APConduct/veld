# Veld Memory Model Design

## Overview

Veld's memory model is inspired by OCaml and C#, providing powerful garbage collection and automatic memory management by default, while allowing users to opt into deeper control when needed. The system uses a hybrid approach that distinguishes between value types (stored directly) and reference types (heap-allocated with GC).

## Core Principles

### 1. Safe by Default
- Automatic garbage collection handles memory management
- No manual memory allocation/deallocation required
- Memory safety guaranteed in safe code
- Strong type system prevents common memory errors

### 2. Performance Through Value Types
- Small, primitive values stored directly (no heap allocation)
- Immediate values: integers, floats, booleans, small strings, chars
- Copy semantics for value types (no GC overhead)
- Stack allocation when possible

### 3. Flexible Reference Types
- Complex objects allocated on GC heap
- Reference types: arrays, large strings, structs, enums, closures, modules
- Automatic lifetime management through GC
- Shared references with safety guarantees

### 4. Opt-in Control
- `unsafe` blocks for manual memory management
- Explicit allocation controls (`stack`, `heap`, `pin`)
- Custom allocators for specialized use cases
- Fine-grained GC control (generations, collection timing)

## Value Categories

### Immediate Values (No GC)
These are stored directly and copied by value:

```veld
// Value types - stored directly, no GC
let x: i32 = 42;           // 4 bytes on stack
let y: f64 = 3.14;         // 8 bytes on stack  
let flag: bool = true;     // 1 byte on stack
let ch: char = 'A';        // 4 bytes (UTF-32)
let small_str = "short";   // ≤ 22 bytes stored inline
```

### Reference Values (GC Managed)
These are heap-allocated and managed by the garbage collector:

```veld
// Reference types - heap allocated with GC
let arr = [1, 2, 3, 4, 5];              // Array on heap
let large_str = "very long string...";   // String > 22 bytes
let person = Person { name: "Alice", age: 30 };  // Struct
let closure = |x| x + 1;                 // Closure capturing environment
let module_ref = import("math");         // Module reference
```

### Hybrid Objects
Some types can be either immediate or reference depending on size:

```veld
// Small tuple - stored as immediate value
let point: (i32, i32) = (10, 20);       // 8 bytes inline

// Large tuple - heap allocated
let big_tuple = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);  // Heap allocated

// Option<T> - immediate if T is immediate and fits
let opt1: Option<i32> = Some(42);        // Immediate (8 bytes)
let opt2: Option<LargeStruct> = Some(s); // Heap allocated
```

## Memory Layout

### Immediate Value Layout
```
Stack Frame:
┌─────────────────┐
│ local_var1: i32 │  4 bytes
│ local_var2: f64 │  8 bytes  
│ flag: bool      │  1 byte + padding
│ point: (i32,i32)│  8 bytes
└─────────────────┘
```

### GC Heap Layout
```
GC Heap:
┌─────────────────────────────────────┐
│ Young Generation (Eden + Survivor)  │
├─────────────────────────────────────┤
│ Old Generation                      │
├─────────────────────────────────────┤
│ Large Object Heap                   │
└─────────────────────────────────────┘

Object Header (16 bytes):
┌──────────┬──────────┬──────────┬─────────┐
│ Type ID  │ Size     │ GC Flags │ Vtable  │
│ 4 bytes  │ 4 bytes  │ 4 bytes  │ 4 bytes │
└──────────┴──────────┴──────────┴─────────┘
```

## Type System Integration

### Value vs Reference Semantics
The type system distinguishes between value and reference types:

```veld
// Value types (Copy trait)
trait Copy {}
impl Copy for i32 {}
impl Copy for f64 {}
impl Copy for bool {}
impl Copy for char {}

// Reference types (managed by GC)
struct Array<T> { /* GC managed */ }
struct String { /* GC managed */ }
struct Closure { /* GC managed */ }

// Generic over value/reference
fn process<T>(x: T) -> T {
    // Behavior depends on whether T: Copy
    x  // Move or copy based on T
}
```

### Automatic vs Manual Layout Control

```veld
// Automatic layout (default)
struct Person {
    name: String,    // Reference type
    age: i32,        // Value type
    active: bool,    // Value type
}

// Manual layout control
#[repr(C)]           // C-compatible layout
struct CStruct {
    x: i32,
    y: f64,
}

#[inline_always]     // Force inlining
struct Point {
    x: f32,
    y: f32,
}

// Stack allocation hint
fn create_points() {
    let points: [Point; 1000] = stack_array!(); // Stack if possible
    // ...
}
```

## Garbage Collection Strategy

### Generational Collection
- **Young Generation**: Short-lived objects, frequent collection
- **Old Generation**: Long-lived objects, infrequent collection  
- **Large Object Heap**: Objects > 85KB, collected separately

### Collection Triggers
```veld
// Automatic (default)
let data = vec![1, 2, 3, 4, 5];  // GC manages automatically

// Manual control
gc::collect();                    // Force immediate collection
gc::collect_young();              // Collect only young generation
gc::set_threshold(1024 * 1024);   // Set collection threshold

// Fine-grained control
gc::with_config(GcConfig {
    young_threshold: 512 * 1024,
    old_threshold: 16 * 1024 * 1024,
    concurrent: true,
}) {
    // Code block with custom GC settings
    heavy_computation();
}
```

### Root Set Management
The GC automatically tracks roots from:
- Stack variables
- Global variables  
- Thread-local storage
- Module exports
- Active closures

## Opt-in Manual Control

### Unsafe Memory Operations
```veld
unsafe {
    // Raw pointer operations
    let ptr: *mut i32 = malloc(4);
    *ptr = 42;
    
    // Manual memory management
    let data = heap_allocate::<[i32; 1000]>();
    // ... use data ...
    heap_deallocate(data);
    
    // Pin objects to prevent GC movement
    let pinned = gc::pin(large_array);
    let raw_ptr = pinned.as_ptr();
    // ... use raw_ptr for FFI ...
    drop(pinned);  // Unpin
}
```

### Custom Allocators
```veld
// Arena allocator for temporary objects
arena::with_arena(|arena| {
    let temp1 = arena.alloc(SomeStruct::new());
    let temp2 = arena.alloc(AnotherStruct::new());
    // All objects freed when arena is dropped
});

// Pool allocator for frequent allocations
let pool = Pool::<Node>::new(1000);
let node = pool.alloc(Node::new());
pool.dealloc(node);  // Return to pool
```

### Lifetime Control
```veld
// Explicit lifetime management
fn process_data<'a>(data: &'a mut [i32]) -> &'a [i32] {
    // Borrow checker ensures memory safety
    data.sort();
    data
}

// RAII with automatic cleanup
struct FileHandle {
    fd: i32
}

impl Drop for FileHandle {
    fn drop(&mut self) {
        close(self.fd);  // Automatic cleanup
    }
}
```

## Performance Characteristics

### Value Types
- **Allocation**: Stack allocation, no GC overhead
- **Access**: Direct memory access, CPU cache friendly
- **Copy**: Cheap bitwise copy
- **Cleanup**: Automatic when scope ends

### Reference Types  
- **Allocation**: GC heap allocation
- **Access**: Single indirection through handle
- **Copy**: Reference copy (shared ownership)
- **Cleanup**: Automatic through GC collection

### Hybrid Approach Benefits
- **Small objects**: Fast value semantics
- **Large objects**: Efficient sharing through references
- **Memory usage**: Optimal layout based on object size
- **GC pressure**: Reduced by keeping small objects off heap

## Integration with Existing Systems

### Bytecode VM
```rust
// Bytecode values can be immediate or managed
pub enum BytecodeValue {
    Immediate(ImmediateValue),  // No GC tracking needed
    Managed(GcHandle),          // GC managed reference
}

pub enum ImmediateValue {
    Integer(i64),
    Float(f64), 
    Boolean(bool),
    Character(char),
    SmallString([u8; 22]),      // Inline string storage
}
```

### Interpreter Integration
```rust
// Hybrid value representation
pub enum Value {
    // Immediate values (no GC)
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Character(char),
    
    // GC managed values  
    String(GcHandle),
    Array(GcHandle),
    Struct(GcHandle),
    Closure(GcHandle),
    Module(GcHandle),
}

impl Value {
    pub fn is_immediate(&self) -> bool {
        matches!(self, 
            Value::Integer(_) | 
            Value::Float(_) | 
            Value::Boolean(_) | 
            Value::Character(_)
        )
    }
    
    pub fn needs_gc_tracking(&self) -> bool {
        !self.is_immediate()
    }
}
```

## Future Extensions

### Concurrent GC
- Background collection threads
- Read/write barriers for concurrent access
- Lock-free allocation fastpaths

### Compacting Collection
- Moving GC for defragmentation
- Handle-based indirection enables movement
- Generational promotion through copying

### JIT Integration  
- Escape analysis to stack-allocate heap objects
- Inline small objects in generated code
- Specialized GC write barriers in hot code

### Memory Profiling
- Allocation tracking and reporting
- GC pause analysis
- Memory leak detection
- Performance recommendations

## Migration Path

### Phase 1: Basic Hybrid (Current)
- ✅ Immediate vs managed value distinction
- ✅ Basic GC for reference types
- ✅ Simple rooting mechanism

### Phase 2: Type System Integration
- 🔄 Value/Reference trait system
- 🔄 Automatic layout optimization
- ⏳ Copy vs Move semantics

### Phase 3: Advanced Control
- ⏳ Unsafe memory operations
- ⏳ Custom allocators
- ⏳ Manual GC control

### Phase 4: Optimization
- ⏳ Concurrent collection
- ⏳ JIT integration
- ⏳ Advanced profiling

This memory model provides the foundation for a high-performance, safe language that gives users the control they need while maintaining safety and ease of use by default.