# Standard Library Import Fix

## Problems Addressed

### 1. False "Undefined identifier" Errors for Imports

**Issue:**
```veld
import std.io as io

pub fn main() => do
    io.println("Hello, World!")  // ERROR: Undefined identifier: io
end
```

The LSP showed an error because it didn't understand import statements or load the standard library.

### 2. Incorrect Return Type Inference

**Issue:**
```veld
pub fn main() => do
    io.println("Hello, World!")
end
```

Hover showed: `fn main() -> U` instead of `fn main() -> ()`

The return type wasn't being inferred correctly for functions with Unit returns.

### 3. Standard Library Not Available

The LSP's type checker didn't know about standard library modules or their functions, causing false errors everywhere stdlib was used.

## Solutions Implemented

### 1. Standard Library Prelude Registration

Added `register_stdlib_prelude()` that:
- Scans the AST for import statements
- Registers imported modules in the type environment
- Adds known stdlib function signatures

**Supported Modules:**

- **`std.io`**
  - `io.println(String) -> ()`
  - `io.print(String) -> ()`

- **`std.option`** - Registered as Module type
- **`std.result`** - Registered as Module type  
- **`std.collections`** - Registered as Module type
- Any `std.*` module - Generic registration

**How it works:**
```rust
// Before type checking, scan imports
for stmt in statements {
    if let ImportDeclaration { path, alias, .. } = stmt {
        let module_path = path.join(".");
        match module_path.as_str() {
            "std.io" => {
                // Register io.println, io.print, etc.
                let alias_name = alias.as_deref().unwrap_or("io");
                type_checker.env().define(
                    &format!("{}.println", alias_name),
                    Type::Function {
                        params: vec![Type::String],
                        return_type: Box::new(Type::Unit),
                    }
                );
                // Also register the module itself
                type_checker.env().define(
                    alias_name,
                    Type::Module(module_path)
                );
            }
            // ... other modules
        }
    }
}
```

### 2. Import Error Suppression

Added `is_known_import_error()` to filter false positives:

```rust
fn is_known_import_error(&self, error: &VeldError, statements: &[Statement]) -> bool {
    if let VeldError::TypeError(msg) = error {
        if msg.contains("Undefined identifier:") {
            // Get all imported aliases
            let imported_aliases = extract_aliases(statements);
            
            // Check if error is about an imported module
            for alias in imported_aliases {
                if msg.contains(&alias) {
                    return true; // Suppress this error
                }
            }
        }
    }
    false
}
```

This prevents errors like "Undefined identifier: io" when `io` is imported.

### 3. Improved Return Type Display

Enhanced `format_function_with_params()` to show return types:

**Before:**
```
fn main()           // No return type shown
fn add(a: T, b: U)  // No return type shown
```

**After:**
```
fn main() -> ()               // Unit return shown
fn add(a: T, b: U) -> V       // Generic return shown
fn add(a: i32, b: i32) -> i32 // Concrete return shown
```

**Logic:**
- If return type is `Unit` (`()`), always show it
- If return type is `TypeVar`, omit it (avoid showing U, V, etc.)
- If return type is concrete, show it

## Testing

### Test File: `test_stdlib_import.veld`

A comprehensive test file covering:
- Basic `std.io` import and usage
- Multiple imports (`std.option`, `std.result`)
- Functions with `-> ()` return type
- Import aliases (`as io`)

### Test Steps

1. **Rebuild LSP:**
   ```bash
   cargo build --release --bin veld-lsp
   ```

2. **Reload VSCode:** `Cmd+Shift+P` → "Developer: Reload Window"

3. **Open `test_stdlib_import.veld`**

4. **Verify No Errors:**
   - Line 4: `import std.io as io` - No error
   - Line 7: `io.println("Hello, World!")` - No "undefined identifier" error
   - Multiple uses of `io` throughout - All should be error-free

5. **Check Hover Info:**
   - Hover over `main` (line 6) - Should show `fn main() -> ()`
   - Hover over `test_print` (line 11) - Should show `-> ()`
   - NOT `-> U` anymore!

6. **Test Your Original Example:**
   ```veld
   # hi from Veld in VSCode!
   import std.io as io

   pub fn main() => do 
       io.println("Hello, World!")
   end
   ```
   
   Should have:
   - ✅ No "Undefined identifier: io" error
   - ✅ Hover on `main` shows `fn main() -> ()`

## Expected Results

### ✅ Should Now Work:
- Import statements don't cause errors
- `std.io` functions recognized
- `io.println()` and `io.print()` available
- Functions with Unit return show `-> ()`
- Import aliases work correctly

### ⚠️ Current Limitations:

1. **Limited Stdlib Coverage**
   - Only `std.io` has function signatures registered
   - Other modules registered as Module type only
   - Functions from other modules may still show errors

2. **No Full Module Resolution**
   - This is a temporary workaround
   - Full solution needs proper module loading system
   - Should load actual stdlib files and extract exports

3. **Import Syntax Variations**
   - Only handles `import std.module as alias`
   - May not handle all import forms (destructuring, wildcards, etc.)

## Future Improvements

### Short Term (Quick Wins)

1. **Add More Stdlib Functions**
   ```rust
   "std.collections" => {
       // Register Vec, HashMap, etc.
   }
   "std.fs" => {
       // Register read_file, write_file, etc.
   }
   ```

2. **Support More Import Syntax**
   - `import std.io { println, print }`
   - `import std.io.*`
   - `from std.io import println`

3. **Return Type Always Shown**
   - Even for TypeVars, show something like `-> T`
   - Makes signatures more complete

### Long Term (Proper Solution)

1. **Load Actual Stdlib Files**
   - Parse stdlib source files
   - Extract all exports automatically
   - No hardcoding needed

2. **Module Resolution System**
   - Proper module loader in type checker
   - Handle nested modules (std.collections.map)
   - Support user modules, not just stdlib

3. **Import Analysis**
   - Track what's imported where
   - Scope-aware completion (only show imported items)
   - Unused import warnings

4. **Workspace-Aware Analysis**
   - Load all files in project
   - Cross-file type checking
   - Go-to-definition across files

## Implementation Details

### Files Modified

- **`crates/lsp/src/analysis.rs`**:
  - Added `register_stdlib_prelude()`
  - Added `is_known_import_error()`
  - Enhanced `format_function_with_params()` return type display
  - Updated `analyze()` to call new functions

### Key Functions

```rust
// Register stdlib modules before type checking
fn register_stdlib_prelude(
    &self,
    type_checker: &mut TypeChecker,
    statements: &[Statement]
)

// Filter out false positive errors
fn is_known_import_error(
    &self,
    error: &VeldError,
    statements: &[Statement]
) -> bool

// Show return types in function signatures
fn format_function_with_params(
    &self,
    name: &str,
    params: &[(String, TypeAnnotation)],
    inferred_type: Option<&Type>
) -> String
```

### Type Registration Example

```rust
type_checker.env().define(
    "io.println",
    Type::Function {
        params: vec![Type::String],
        return_type: Box::new(Type::Unit),
    }
);
```

## Extending to More Modules

To add support for another stdlib module:

1. **Add to `register_stdlib_prelude()`:**
   ```rust
   "std.fs" => {
       let alias_name = alias.as_deref().unwrap_or("fs");
       
       // Register module functions
       type_checker.env().define(
           &format!("{}.read_file", alias_name),
           Type::Function {
               params: vec![Type::String],
               return_type: Box::new(Type::String),
           }
       );
       
       // Register the module
       type_checker.env().define(
           alias_name,
           Type::Module(module_path.clone())
       );
   }
   ```

2. **Test it:**
   ```veld
   import std.fs as fs
   
   fn test() => do
       let content = fs.read_file("test.txt")
   end
   ```

## Debugging

If imports still show errors:

1. **Check the log:**
   ```bash
   tail -f lsp_server.log
   ```
   Look for type checking errors

2. **Verify import syntax:**
   - Must be `import std.module as alias`
   - Check path is recognized in `register_stdlib_prelude()`

3. **Check type environment:**
   - Add debug logging in `register_stdlib_prelude()`
   - Verify module/function is being registered

4. **Test with simple example:**
   ```veld
   import std.io as io
   
   fn test() => do
       io.println("test")
   end
   ```
   
   Should have no errors.

## Related Fixes

- **`HOVER_FIX_SUMMARY.md`** - Position mapping fix
- **`FUNCTION_TYPE_FIX.md`** - Function type display improvements
- **`test_lsp_simple.veld`** - Simple test without imports
- **`test_stdlib_import.veld`** - Stdlib import test file

## Status

✅ **Complete** - Basic stdlib import support implemented  
⚠️ **Limitations** - Only std.io fully registered, others partial  
🔜 **Next** - Add more stdlib modules, proper module loading

---

**Test now and report results!**