# HashMap Implementation Status

## Overview
HashMap is a key-value data structure implementation for Veld, providing O(1) average-case lookup, insertion, and deletion operations.

## Current Status: **Partial Implementation**

### ✅ Completed
- HashMap structure definition in `hash_map.veld`
- Map kind interface in `map.veld`
- Native method registration in Rust
- Constructor (`hash_map.new()`)
- Read-only operations:
  - `get(key)` - Returns `Option<V>`
  - `has(key)` - Returns `bool`
  - `len()` - Returns `i32`
  - `is_empty()` - Returns `bool`
  - `keys()` - Returns `[K]`
  - `values()` - Returns `[V]`
  - `entries()` - Returns `[(K, V)]`

### ⚠️ Known Limitations

#### 1. Mutation Issue
**Problem**: Mutating methods (`set`, `remove`, `clear`) don't persist changes to the HashMap.

**Root Cause**: When native methods are called in Veld, the object is cloned before being passed to the method handler. This means mutations happen on the clone, not the original variable.

**Affected Methods**:
- `set(key, value)` - Sets a value but changes don't persist
- `remove(key)` - Removes a key but changes don't persist
- `clear()` - Clears the map but changes don't persist

**Code Location**: `veld/crates/interpreter/src/interpreter.rs:6586`
```rust
let mut method_args = vec![object.clone()];  // <-- Clone here means mutations don't persist
```

**Impact**: HashMap can be created and queried, but cannot be modified after creation.

#### 2. Key Type Limitation
**Current**: Keys are converted to strings using `format!("{:?}", key)` for storage.

**Limitation**: This means keys are compared as strings, not by their actual value/type.

**Example Issue**:
```veld
let map = hash_map.new()
map.set(1, "one")      # Key stored as "1"
map.set("1", "one")    # Key stored as "\"1\"" (different from integer 1)
```

## Potential Solutions

### For Mutation Issue

#### Option 1: Return Modified HashMap (Functional Style)
Make mutating methods return the modified HashMap:
```veld
# Current (doesn't work):
let map = hash_map.new()
map.set("key", "value")  # Changes lost

# Proposed:
let map = hash_map.new()
let map = map.set("key", "value")  # Reassign to persist changes
```

**Pros**: Works with current architecture, functional programming style
**Cons**: Less intuitive for users expecting mutation

#### Option 2: Variable Tracking
Extend `call_method_value_with_mutation` to track the variable name and update it after native method calls that return `()`.

**Pros**: Maintains mutable semantics
**Cons**: Requires architectural changes, needs to handle all scopes correctly

#### Option 3: Interior Mutability
Use Rust's `RefCell` or similar for the internal HashMap storage.

**Pros**: True mutation support
**Cons**: Complex, adds runtime overhead, need to handle borrowing errors

### For Key Type Limitation

Store actual key values alongside their string representations:
```rust
// Instead of: HashMap<String, Value>
// Use: HashMap<String, (Value, Value)>  // (key_representation, actual_value)
```

## Next Steps

1. **Decide on mutation strategy** - Choose from Option 1, 2, or 3 above
2. **Implement chosen solution** - Requires changes to interpreter and/or stdlib
3. **Fix key type handling** - Store and return actual key types
4. **Add comprehensive tests** - Test all operations including mutation
5. **Document usage patterns** - Show users how to work with HashMap effectively

## Testing

### Current Tests
- `tests/hashmap_simple_test.veld` - Basic creation and read operations ✅
- `tests/hashmap_test.veld` - Full test suite (mutation tests fail) ⚠️

### Needed Tests
- Mutation operations (after mutation support is implemented)
- Type preservation for keys and values
- Generic type parameter handling
- Integration with Vec and other collections
- Performance benchmarks

## Related Files
- `stdlib/collections/hash_map.veld` - HashMap struct and impl
- `stdlib/collections/map.veld` - Map kind interface
- `stdlib/collections/mod.veld` - Module exports
- `crates/interpreter/src/interpreter.rs` - Native method registration (line ~885)
- `tests/hashmap_simple_test.veld` - Working test
- `tests/hashmap_test.veld` - Full test suite

## Similar Issues
This mutation issue also affects:
- `Vec<T>` - GrowableSequence with `push`, `pop`, `insert`, `remove`
- Any future mutable data structures

Solving this for HashMap will benefit all mutable types in Veld's standard library.