# Bytecode Compilation Fixes and Remaining Work

**Date:** November 11, 2024  
**Status:** ✅ Partial fixes applied, ⚠️ Recursive functions need VM support

---

## Summary

I've made significant progress on fixing bytecode compilation limitations. Basic features now work correctly, and I've identified the root cause of recursive function issues.

---

## ✅ Fixes Applied

### 1. Fixed `Expr::Call` Capture Detection

**Problem:** The `analyze_captures` function wasn't detecting variable references in `Expr::Call` expressions, which prevented proper closure variable capture.

**Root Cause:** The `find_captured_vars_in_expr` function had cases for `Expr::FunctionCall` but not for `Expr::Call`, which is the modern AST variant used by the parser.

**Fix Applied:**
```rust
// Added to compiler_v2.rs in find_captured_vars_in_expr()
Expr::Call { callee, arguments } => {
    // Handle general call expression - callee can be any expression including Identifier
    self.find_captured_vars_in_expr(callee, parent_vars, captures);
    for arg in arguments {
        if let Argument::Positional(e) = arg {
            self.find_captured_vars_in_expr(e, parent_vars, captures);
        }
    }
}
```

**Impact:** This fix enables proper closure variable capture for nested functions.

**Location:** `crates/bytecode/src/compiler_v2.rs` around line 1640

---

### 2. Added Function Name to Scope Before Body Compilation

**Problem:** Recursive functions failed with "Undefined variable" errors because the function name wasn't available during body compilation.

**Root Cause:** The function was only registered in `self.variables` AFTER compiling the function body, so recursive calls couldn't find the function name.

**Fix Applied:**
```rust
// In compile_function_declaration(), BEFORE compiling body:
// Allocate register for function variable BEFORE compiling body
// This allows recursive calls to reference the function
let func_reg = self
    .allocator
    .allocate_variable(name.to_string(), false)
    .map_err(|e| VeldError::CompileError {
        message: e,
        line: Some(self.current_line as usize),
        column: None,
    })?;

// Track function variable before compiling body (for recursion)
self.variables.insert(
    name.to_string(),
    VarInfo {
        register: func_reg,
        is_mutable: false,
        depth: self.scope_depth,
        is_captured: false,
        is_upvalue: false,
        is_type: false,
    },
);
```

**Impact:** Recursive function compilation now succeeds at compile-time.

**Location:** `crates/bytecode/src/compiler_v2.rs` around line 1085

---

## ⚠️ Remaining Issues

### 1. Recursive Function Runtime Failure

**Status:** ⚠️ Compiles but fails at runtime

**Problem:** Recursive functions compile successfully but crash at runtime with:
```
Runtime error: InvalidOperation { op: "call", types: ["unit"] }
```

**Root Cause:** The recursive self-reference creates a circular dependency in closure/upvalue setup. The function tries to capture itself, but at runtime the upvalue points to `unit` instead of the actual function.

**Why This Happens:**
1. Function compilation creates an upvalue for self-reference
2. At runtime, when the closure is created, it tries to capture the function from the parent scope
3. But the function isn't fully initialized yet (chicken-and-egg problem)
4. The upvalue ends up pointing to `unit` instead of the function

**Possible Solutions:**

**Option A: Two-Phase Initialization (Recommended)**
- Emit bytecode to create the closure with a placeholder
- Then emit a special instruction to "fix up" the self-reference upvalue
- Similar to how Lua handles recursive functions
- Requires VM changes to support fixup instruction

**Option B: Use Global References for Top-Level Functions**
- Top-level functions (function_depth == 0) don't use upvalues for recursion
- Instead, emit LoadGlobal instruction to reference themselves
- Only works for top-level functions, not nested recursive functions

**Option C: Lazy Upvalue Resolution**
- Modify VM to delay upvalue resolution until first use
- When accessing an upvalue that's still uninitialized, resolve it from parent scope
- Requires VM changes to track unresolved upvalues

**Implementation Priority:** Option B is simplest and handles the common case (top-level recursive functions like factorial). Option A is more complete but requires more VM work.

---

### 2. Complex Closures Untested

**Status:** ⚠️ Not yet tested

**Examples:**
- Closures that capture mutable variables
- Multiple levels of nested closures
- Closures with multiple captured variables

**Action Needed:** Create comprehensive test suite for closure scenarios once recursion is fixed.

---

### 3. String Handling Issues

**Status:** ⚠️ Partial support

**Problem:** String literal compilation and display has formatting issues.

**Example:**
```veld
let greeting = "Hello"
greeting
```

Expected output: `Hello`  
Actual output: `\` (or malformed)

**Root Cause:** Likely an issue in:
- Constant string serialization to bytecode
- VM string value representation
- Output formatting for string values

**Action Needed:** Debug string constant handling in bytecode format.

---

## 🎯 Verification Tests

### ✅ Working Cases

```bash
# Simple functions
echo 'fn add(a,b) a+b end; add(5,10)' > test.veld
veld build test.veld && veld test.veldc
# Expected: 15 ✓

# Multiple functions
cat > test.veld << 'EOF'
fn double(x) x * 2 end
fn square(x) x * x end
square(double(5))
EOF
veld build test.veld && veld test.veldc
# Expected: 100 ✓

# Mutation
cat > test.veld << 'EOF'
let mut x = 10
x = x + 5
x = x * 2
x
EOF
veld build test.veld && veld test.veldc
# Expected: 30 ✓
```

### ⚠️ Failing Cases

```bash
# Recursive function
cat > test.veld << 'EOF'
fn factorial(n)
    if n <= 1 then 1 else n * factorial(n-1) end
end
factorial(5)
EOF
veld build test.veld && veld test.veldc
# Compiles: ✓
# Runtime: ✗ (InvalidOperation error)

# String literals
echo '"Hello, World!"' > test.veld
veld build test.veld && veld test.veldc
# Compiles: ✓
# Runtime: ⚠️ (output formatting issues)
```

---

## 📋 Action Items

### High Priority
1. **Fix recursive function runtime** (Option B: global references for top-level functions)
   - Modify `compile_identifier` to check function_depth
   - For top-level recursive references, emit LoadGlobal instead of GetUpvalue
   - Test with factorial, fibonacci, etc.

2. **Fix string handling**
   - Debug Constant::String serialization
   - Verify VM string value representation
   - Test string output formatting

### Medium Priority
3. **Test complex closures**
   - Multi-level nesting
   - Multiple captured variables
   - Mutable captures

4. **Add bytecode tests**
   - Automated test suite for bytecode features
   - Compare bytecode vs source output
   - Regression tests for fixed issues

### Low Priority
5. **Optimize recursive function handling**
   - Implement Option A (two-phase initialization) for better support
   - Support nested recursive functions
   - Tail call optimization

---

## 🔧 Technical Details

### Compiler Structure

```
RegisterCompiler
├── variables: HashMap<String, VarInfo>  // Current scope variables
├── upvalues: Vec<CompilerUpvalueInfo>   // Captured variables
├── function_depth: usize                // Nesting level (0 = top)
└── allocator: RegisterAllocator         // Register management
```

### Recursive Function Compilation Flow

1. **Before Fix:**
   ```
   Start compile_function_declaration("factorial")
   ├── Compile function body
   │   └── ERROR: "factorial" not in variables
   └── Register "factorial" in variables  ← Too late!
   ```

2. **After Fix:**
   ```
   Start compile_function_declaration("factorial")
   ├── Register "factorial" in variables   ← Now early!
   ├── Analyze captures
   │   └── Finds "factorial" as self-reference
   ├── Create nested compiler
   │   └── Add "factorial" as upvalue
   ├── Compile function body
   │   └── "factorial" found as upvalue ✓
   └── Emit closure with upvalues
   ```

3. **Runtime Issue:**
   ```
   Execute main chunk
   ├── CLOSURE R0 P0  ← Create factorial closure
   │   └── Capture upvalue 0 (factorial itself)
   │       └── But factorial isn't ready yet! ✗
   │       └── Upvalue points to unit/nil
   ├── STORE_GLOBAL "factorial"
   └── Call factorial(5)
       └── ERROR: Trying to call unit ✗
   ```

### Solution for Runtime Issue

**Approach: Use Global Reference for Top-Level Recursion**

```rust
fn compile_identifier(&mut self, name: &str) -> Result<ExprResult> {
    if let Some(var_info) = self.variables.get(name) {
        // Check if this is a recursive self-reference at top level
        if var_info.is_upvalue && self.function_depth == 1 {
            // This is a top-level function referencing itself
            // Use global lookup instead of upvalue
            let dest = self.allocate_temp()?;
            let name_const = self.chunk.add_constant(Constant::String(name.to_string()));
            self.chunk.load_global(dest, name_const);
            return Ok(ExprResult::temp(dest));
        }
        
        // ... rest of existing logic
    }
    // ...
}
```

This way:
1. Function is stored as global first
2. Recursive calls use LoadGlobal to fetch it
3. No circular upvalue dependency
4. Works reliably at runtime

---

## 📝 Files Modified

- `crates/bytecode/src/compiler_v2.rs`
  - Added `Expr::Call` handling in `find_captured_vars_in_expr`
  - Moved function variable registration before body compilation
  - Added `analyze_captures_for_function` to handle self-references

---

## 🧪 Testing Commands

```bash
# Build with latest fixes
cargo build --release --bin veld

# Test simple functions (should work)
./target/release/veld build examples/simple.veld
./target/release/veld examples/simple.veldc

# Test recursive functions (compiles, runtime fails)
./target/release/veld build examples/recursive.veld
./target/release/veld examples/recursive.veldc

# View disassembly
./target/release/veld disasm examples/recursive.veldc
```

---

## 💡 Conclusion

**Progress Made:**
- ✅ Fixed closure variable capture detection
- ✅ Fixed recursive function compilation
- ✅ Basic bytecode features working

**Still Needed:**
- ⚠️ Fix recursive function runtime (global reference approach)
- ⚠️ Fix string handling
- ⚠️ Test complex closures
- ⚠️ Add comprehensive test suite

**Next Step:** Implement global reference approach for top-level recursive functions. This is a small, focused change that will make recursive functions work correctly.