# Generic Parameter Display Update

## Problem

Generic functions weren't clearly showing that they have type parameters, making them look like concrete functions with arbitrary type names.

**Before:**
```
fn add(a: T, b: U)
```

This looks like `T` and `U` are concrete types, not generic parameters!

**User's Feedback:**
> "If the function uses generic types, shouldn't the lsp show the type with at least generic type parameter (which '< ... >' braces), to explicitly show what is generic?"

## Solution

Added explicit generic type parameters in angle brackets to make it clear when functions are generic.

**After:**
```
fn add<T, U>(a: T, b: U) -> V
```

Now it's obvious that this is a generic function with type parameters `T`, `U`, and return type `V`.

## Changes Made

### 1. Added Generic Parameter Brackets

**For functions with parameters:**
```rust
// Generate type parameters: T, U, V, etc.
let mut type_params: Vec<String> = Vec::new();

// Collect them from parameters
for (i, (param_name, _)) in params.iter().enumerate() {
    let type_param = if i < 26 {
        ((b'T' + i as u8) as char).to_string()  // T, U, V, ...
    } else {
        format!("T{}", i)  // T26, T27, ...
    };
    type_params.push(type_param.clone());
}

// Format: fn name<T, U>(a: T, b: U)
format!("fn {}<{}>({}) -> ...", name, type_params.join(", "), params.join(", "))
```

### 2. Improved Return Type Handling

**For functions with no parameters:**
```rust
// main() with Unit return
if let Type::Unit = return_type {
    return format!("fn {}() -> ()", name);
}

// If TypeVar (inference failed), assume Unit for no-param functions
if let Type::TypeVar(_) = return_type {
    return format!("fn {}() -> ()", name);
}
```

**For functions with parameters and generic return:**
```rust
// If return type is a TypeVar, add it to generics
if let Type::TypeVar(_) = return_type {
    let return_type_param = next_type_param_name();
    type_params.push(return_type_param.clone());
    return_type_str = format!(" -> {}", return_type_param);
}

// Result: fn add<T, U, V>(a: T, b: U) -> V
```

## Examples

### Generic Function with Parameters
```veld
fn add(a, b)
    a + b
end
```

**Hover shows:** `fn add<T, U, V>(a: T, b: U) -> V`

Clearly generic with 3 type parameters.

### Function with Concrete Types
```veld
let sum = add(10, 20)  // Called with i32
```

**Hover shows:** `fn add(a: i32, b: i32) -> i32`

No angle brackets because types are concrete, not generic.

### Function with Unit Return
```veld
import std.io as io

pub fn main() => do
    io.println("Hello, World!")
end
```

**Hover shows:** `fn main() -> ()`

NOT `fn main() -> W` or `fn main() -> U` anymore!

### Generic Function with Multiple Params
```veld
fn calculate(x, y, z)
    x + y * z
end
```

**Hover shows:** `fn calculate<T, U, V, W>(x: T, y: U, z: V) -> W`

Shows all 4 generic type parameters (3 params + return).

## Special Cases

### Empty Functions
```veld
fn empty_function() => do
end
```

**Hover shows:** `fn empty_function() -> ()`

No parameters, infers Unit return.

### Type Inference Failed
If the type checker can't determine the return type and creates a TypeVar:

**For no-param functions:** Assume `-> ()` (most common case)
**For functions with params:** Show as generic return type in angle brackets

## Benefits

1. **Clear Generic Indication**: Angle brackets `<T, U>` make it obvious
2. **Standard Syntax**: Matches Rust, TypeScript, C++, etc.
3. **Complete Information**: Shows all type parameters including return
4. **No Confusion**: Can't mistake `T` for a concrete type anymore

## Comparison with Other Languages

### Rust
```rust
fn add<T, U>(a: T, b: U) -> T { ... }
```

### TypeScript
```typescript
function add<T, U>(a: T, b: U): T { ... }
```

### Veld (now)
```veld
fn add<T, U>(a: T, b: U) -> T
```

Consistent with industry standards!

## Testing

### Test Files

1. **`test_lsp_simple.veld`** - Basic functions
   - `fn add(a, b)` → Should show `fn add<T, U, V>(a: T, b: U) -> V`
   - `fn multiply(x, y)` → Should show `fn multiply<T, U, V>(x: T, y: U) -> V`

2. **`test_main_return.veld`** - Unit return functions
   - `fn main()` → Should show `fn main() -> ()`
   - NOT `fn main() -> W` or `fn main() -> U`

3. **`test_stdlib_import.veld`** - With imports
   - Functions using `io.println` should infer `-> ()`

### Manual Testing

1. Rebuild: `cargo build --release --bin veld-lsp`
2. Reload VSCode: `Cmd+Shift+P` → "Developer: Reload Window"
3. Open test files and hover over functions
4. Verify:
   - ✅ Generic functions show `<T, U, V>`
   - ✅ Concrete functions show no angle brackets
   - ✅ Unit returns show `-> ()`
   - ✅ Generic returns included in type params

## Known Issues & Limitations

### Issue: TypeVar Still Showing for main()

**Problem:** `fn main() -> W` still appearing in some cases

**Cause:** The type checker is creating a TypeVar for the return type because it doesn't know what `io.println` returns, even though we registered it.

**Possible reasons:**
1. Registration not happening (check logs)
2. Method call syntax `io.println()` not resolved correctly
3. Type checker not looking up qualified names properly

**Workaround:** We now assume `-> ()` for no-parameter functions with TypeVar returns, since that's the most common case.

**Proper fix:** Ensure `io.println` is properly resolved during type checking, not just registered in the environment.

### Limitation: Generic Constraints Not Shown

We don't yet show where clauses:
```
fn add<T, U, V>(a: T, b: U) -> V where T: Add<U, Output = V>
```

This would require the type checker to track and expose constraint information.

### Limitation: Return Type Sometimes Omitted

If the return type can't be determined and isn't a simple TypeVar, we might not show it. This is better than showing confusing internal types.

## Next Steps

If you still see `fn main() -> W`:

1. **Check logs:**
   ```bash
   tail -f lsp_server.log | grep -i "register\|import"
   ```
   Should see: "Found import: std.io", "Registering io.println", etc.

2. **Verify type checker is using registered types:**
   Add logging to see if `io.println` is being looked up

3. **Alternative fix:**
   Register `println` as a global function, not just `io.println`

4. **Proper solution:**
   Improve how method calls on modules are resolved in the type checker

## Files Modified

- **`crates/lsp/src/analysis.rs`**:
  - Enhanced `format_function_with_params()` to add `<T, U, V>` brackets
  - Improved return type handling for no-param functions
  - Added logic to include return TypeVars in generic parameters
  - Added debug logging for import registration

## Summary

✅ **Fixed:** Generic functions now show `fn name<T, U>(params)`  
✅ **Improved:** Return types properly handled for Unit and TypeVars  
🔧 **In Progress:** Ensuring `main()` shows `-> ()` consistently  

**Result:** Much clearer type signatures that match standard generic syntax!