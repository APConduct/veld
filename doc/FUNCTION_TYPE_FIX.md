# Function Type Display Improvements

## Problem

When hovering over functions in the LSP, the type information was showing internal type variables instead of user-friendly signatures:

**Before:**
```
add: fn(T9, T10) -> i32
```

This was confusing because:
- `T9`, `T10` are internal type variable IDs from the type inference system
- No parameter names shown
- Not clear what the function actually does

**Desired:**
```
fn add(a: T, b: U)
```

or with concrete types after inference:
```
fn add(a: i32, b: i32) -> i32
```

## Root Cause

The type checker creates internal `TypeVar(usize)` during type inference. These were being displayed directly to users via the `format_type()` function, showing implementation details like `T9`, `T10` instead of:
1. The original parameter names from the source code
2. User-friendly generic type names (T, U, V)
3. Concrete inferred types when available

## Solution

### 1. Enhanced `get_hover_info()` to Prioritize AST

Changed the lookup order to check the AST first for function declarations:

```rust
// NEW: Check AST for function declarations to get parameter names
for stmt in ast {
    if let Statement::FunctionDeclaration { name, params, .. } = stmt {
        if name == &identifier {
            // Get type info from type checker
            let type_from_checker = tc.env().get(&identifier);
            
            // Format with both AST params and inferred types
            return format_function_with_params(name, params, type_from_checker);
        }
    }
}

// FALLBACK: For variables and other identifiers
if let Some(type_info) = type_from_checker {
    format!("{}: {}", identifier, format_type(&type_info))
}
```

### 2. New Function: `format_function_with_params()`

Creates user-friendly function signatures combining:
- **Parameter names** from the AST
- **Inferred types** from the type checker

```rust
fn format_function_with_params(
    name: &str,
    params: &[(String, TypeAnnotation)],  // From AST
    inferred_type: Option<&Type>,          // From type checker
) -> String
```

**Logic:**

**Case 1: Concrete inferred types available**
```rust
// If type checker has concrete types (no TypeVars)
fn add(a: i32, b: i32) -> i32
```

**Case 2: Generic/Unknown types**
```rust
// Use generic names T, U, V instead of T9, T10
fn add(a: T, b: U)
fn multiply(x: T, y: U)
```

The generic names are generated:
- First 26 parameters: `T`, `U`, `V`, `W`, ..., `Z`
- Beyond 26: `T26`, `T27`, etc.

### 3. Improved TypeVar Formatting

Updated `format_type()` to show better names for type variables:

```rust
Type::TypeVar(id) => {
    // OLD: format!("T{}", id)  // Shows "T9", "T10"
    
    // NEW: Use letters for readability
    if *id < 26 {
        ((b'T' + *id as u8) as char).to_string()  // "T", "U", "V", ...
    } else {
        format!("T{}", id)  // "T26", "T27", ...
    }
}
```

## Examples

### Simple Function (No Type Annotations)

**Source:**
```veld
fn add(a, b)
    a + b
end
```

**Hover Info:**
- If not yet inferred: `fn add(a: T, b: U)`
- After inference with concrete types: `fn add(a: i32, b: i32) -> i32`

### Function with Multiple Parameters

**Source:**
```veld
fn calculate(x, y, z)
    x + y * z
end
```

**Hover Info:** `fn calculate(x: T, y: U, z: V)`

### Used Function with Inferred Types

**Source:**
```veld
fn multiply(x, y)
    x * y
end

let product = multiply(5, 6)  // Called with i32
```

**Hover Info:** `fn multiply(x: i32, y: i32) -> i32`

## Testing

### Test File: `test_lsp_simple.veld`

Open this file and test hover on:

1. **Line 3: `fn add(a, b)`**
   - Expected: `fn add(a: T, b: U)` or `fn add(a: i32, b: i32) -> i32`
   - Should NOT show: `add: fn(T9, T10) -> i32`

2. **Line 7: `fn multiply(x, y)`**
   - Expected: `fn multiply(x: T, y: U)` or with concrete types
   - Should show different type vars than `add` if not inferred

3. **Line 15: `fn calculate(a, b)`**
   - Expected: Shows parameter names `a` and `b`

### Testing Steps

1. Rebuild LSP:
   ```bash
   cargo build --release --bin veld-lsp
   ```

2. Reload VSCode: `Cmd+Shift+P` → "Developer: Reload Window"

3. Open `test_lsp_simple.veld`

4. Hover over function names and check:
   - ✅ Parameter names appear (a, b, x, y, etc.)
   - ✅ Generic types shown as T, U, V (not T9, T10)
   - ✅ Or concrete types like i32 when inferred
   - ❌ Should NOT see internal type variable IDs

## Benefits

1. **More Readable**: `fn add(a: T, b: U)` vs `add: fn(T9, T10) -> i32`
2. **Informative**: Shows actual parameter names from source
3. **Professional**: Looks like proper documentation
4. **Flexible**: Shows generic types when unknown, concrete types when inferred

## Known Limitations

1. **Generic Constraints Not Shown**: 
   - We don't display `where T: Add<U>` constraints yet
   - Type checker would need to expose constraint information

2. **Return Type Sometimes Missing**:
   - When return type isn't explicitly annotated and inference incomplete
   - Shows just `fn name(params)` without `-> RetType`

3. **Complex Generic Types**:
   - Generic structs like `Vec<T>` are shown but might not be fully resolved

## Future Improvements

1. **Add Generic Constraints**: Show `where` clauses
   ```
   fn add(a: T, b: U) -> V where T: Add<U, Output = V>
   ```

2. **Documentation Comments**: Extract and show doc comments
   ```
   /// Adds two numbers
   fn add(a: T, b: U) -> V
   ```

3. **Examples in Hover**: Show usage examples
   ```
   fn add(a: T, b: U) -> V
   
   Example:
     let sum = add(1, 2)  // => 3
   ```

4. **Type Inference Trace**: Show how types were inferred
   ```
   fn add(a: i32, b: i32) -> i32
   
   Inferred from:
     - Call at line 20: add(10, 20)
   ```

## Files Modified

- `crates/lsp/src/analysis.rs`:
  - Enhanced `get_hover_info()` to check AST first
  - Added `format_function_with_params()` helper
  - Improved `format_type()` for TypeVar display

## See Also

- `HOVER_FIX_SUMMARY.md` - Position mapping fix
- `test_lsp_simple.veld` - Simple test file
- `test_hover_fix.md` - Testing instructions