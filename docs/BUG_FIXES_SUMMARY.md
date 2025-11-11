# Bug Fixes Summary - Register VM Function Calls

## Overview

This document summarizes all bugs discovered and fixed during the debugging session after implementing Phase 5 (Iterators & For Loops) of the register-based VM migration.

**Initial State:** 115/124 tests passing (92.7%)  
**Final State:** 137/137 tests passing (100%) ✅

---

## Critical Bugs Fixed

### 1. Missing `register_count` in Nested Functions ⚠️ **FOUNDATIONAL**

**Severity:** Critical  
**Impact:** All nested function calls failed with "Register out of bounds" errors

**Problem:**
- When compiling nested functions (both regular functions and lambdas), the compiler wasn't setting `register_count` before calling `build()`
- This resulted in `FunctionProto` objects with `register_count: 0`
- The VM would allocate 0 registers for function call frames, causing immediate crashes

**Root Cause:**
The main `compile()` function properly set register_count:
```rust
let max_reg = self.allocator.max_register();
self.chunk.register_count(register_count);
```

But `compile_function_declaration()` and `compile_lambda()` never did this for nested functions.

**Fix:**
```rust
// In compile_function_declaration and compile_lambda:
let max_reg = func_compiler.allocator.max_register();
let register_count = if max_reg == 0 {
    params.len().max(1) as u8
} else {
    (max_reg + 1).max(params.len() as u8)
};
func_compiler.chunk.register_count(register_count);
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (lines ~1029-1039, ~933-943)

---

### 2. Implicit Return Always Returning `nil` ⚠️ **FOUNDATIONAL**

**Severity:** Critical  
**Impact:** All closure factories and functions returning values failed

**Problem:**
- Functions always returned `nil` regardless of their last expression
- The compiler unconditionally emitted: `return nil` at the end of every function
- This broke closure factories like:
```veld
fn make_adder(x)
    fn inner(y)
        x + y
    end
    inner  # This should return the inner closure, not nil
end
```

**Root Cause:**
```rust
// Old code - always returned nil:
let nil_const = func_compiler.chunk.add_constant(Constant::Nil);
let nil_reg = func_compiler.allocate_temp()?;
func_compiler.chunk.load_const(nil_reg, nil_const);
func_compiler.chunk.return_vals(nil_reg, 1);
```

**Fix:**
Check if the last statement is an expression and return its value:
```rust
// Track if the last statement is an expression that should be returned
let last_stmt_index = body.len().saturating_sub(1);
let mut last_expr_reg: Option<Reg> = None;

for (i, stmt) in body.iter().enumerate() {
    if i == last_stmt_index {
        if let Statement::ExprStatement(expr) = stmt {
            let result = func_compiler.compile_expr_to_reg(expr)?;
            last_expr_reg = Some(result.register);
            // Don't free the temp - we need it for return
        } else {
            func_compiler.compile_statement(stmt)?;
        }
    } else {
        func_compiler.compile_statement(stmt)?;
    }
}

// Return the last expression or nil
let return_reg = if let Some(reg) = last_expr_reg {
    reg
} else {
    // ... create nil and return it
};
func_compiler.chunk.return_vals(return_reg, 1);
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (lines ~1049-1078)

---

### 3. Upvalues Not Accessible in Nested Functions

**Severity:** High  
**Impact:** Multi-level closures failed (e.g., outer → middle → inner)

**Problem:**
- When compiling a nested function, captured variables from parent scope were removed from the variables map
- This made them inaccessible to deeper nested functions
- Example failure:
```veld
fn outer(x)
    fn middle(y)
        fn inner(z)
            x + y + z  # "Undefined variable: x"
        end
    end
end
```

**Root Cause:**
Initially, captured variables were added to the nested compiler's `variables` map with their parent's register numbers. This was wrong because:
1. They should be accessed via `GetUpvalue`, not as local variables
2. The register numbers from the parent scope are meaningless in the child scope

Then the fix was to NOT add them at all, which broke multi-level nesting because `inner` couldn't find `x` in `middle`'s scope.

**Fix:**
Added an `is_upvalue` flag to `VarInfo`:
```rust
struct VarInfo {
    register: Reg,
    is_mutable: bool,
    depth: usize,
    is_captured: bool,
    is_upvalue: bool,  // NEW: marks this as an upvalue, not local variable
}
```

Then in `compile_identifier`:
```rust
if let Some(var_info) = self.variables.get(name) {
    if var_info.is_upvalue {
        // This is an upvalue, use GetUpvalue instruction
        if let Some(upvalue_idx) = self.find_upvalue(name) {
            let dest = self.allocate_temp()?;
            self.chunk.get_upvalue(dest, upvalue_idx as u8);
            return Ok(ExprResult::temp(dest));
        }
    } else {
        // Regular local variable
        return Ok(ExprResult::var(var_info.register));
    }
}
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (VarInfo struct, compile_identifier, function setup)

---

### 4. Function Call Arguments in Wrong Registers

**Severity:** High  
**Impact:** All function calls with arguments had incorrect argument values

**Problem:**
- The VM expected arguments at `func_reg + 1, func_reg + 2, ...`
- But the compiler placed arguments in arbitrary registers
- Example: `make_adder(5)` would place 5 in register 2, but VM looked for it at register 1

**Root Cause:**
```rust
// Old compile_call code:
for arg in arguments {
    let arg_result = self.compile_expr_to_reg(arg_expr)?;
    arg_regs.push(arg_result);  // Just tracked, never moved to right place
}
self.chunk.call(func_result.register, arg_count, 1);
```

**Fix:**
Move arguments to consecutive registers after the function register:
```rust
for (i, arg) in arguments.iter().enumerate() {
    let arg_result = self.compile_expr_to_reg(arg_expr)?;
    
    // Move argument to the expected position (func_reg + 1 + i)
    let target_reg = func_result.register + 1 + i as u8;
    if arg_result.register != target_reg {
        self.chunk.move_reg(target_reg, arg_result.register);
    }
    
    // Free the temp if it's not the target register
    if arg_result.is_temp && arg_result.register != target_reg {
        self.free_temp(arg_result.register);
    }
}
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (compile_call function)

---

### 5. Function Register Overwritten by Return Value

**Severity:** High  
**Impact:** Function calls inside loops failed on second iteration

**Problem:**
- When calling a function, the return value was placed in `func_reg` (overwriting the function)
- In loops, the second iteration would try to call the return value instead of the original function
- Error: "call i64" when trying to call an integer that was the previous return value

**Debug Output:**
```
Iteration 1: Register 0 = Closure { ... }  ✓
  Call succeeds, returns Integer(2)
Iteration 2: Register 0 = Integer(2)      ✗
  Error: trying to call Integer(2)
```

**Root Cause:**
The VM's `call_function` set `return_address: func_reg`, so returns went back to the same register as the function, destroying it.

**Fix:**
When the function is a variable (not a temporary), move it to a temp register before calling:
```rust
// If the function is not a temp, move it to preserve the original
let call_func_reg = if func_result.is_temp {
    func_result.register
} else {
    let temp_reg = self.allocate_temp()?;
    self.chunk.move_reg(temp_reg, func_result.register);
    temp_reg
};

// Now call using the temp register
self.chunk.call(call_func_reg, arg_count, 1);

// Result is in call_func_reg, move to final result register
let result_reg = self.allocate_temp()?;
self.chunk.move_reg(result_reg, call_func_reg);
self.free_temp(call_func_reg);
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (compile_call function)

---

### 6. If/Else Expressions Not Returning Values

**Severity:** Medium  
**Impact:** Functions ending with if/else couldn't return values from branches

**Problem:**
- Veld treats if/else as expressions that can return values
- But the compiler only supported if/else as statements
- Example:
```veld
fn make_function(use_x)
    if use_x then
        use_first
    else
        use_second
    end
end
```
This should return either `use_first` or `use_second`, but returned `nil`.

**Root Cause:**
If statements were compiled without producing a value. The implicit return logic only handled `Statement::ExprStatement`.

**Fix:**
1. Added special handling in function compilation for if/else as last statement
2. Created `compile_if_expression()` that compiles both branches to place their result in a designated register:

```rust
// In function body compilation:
Statement::If {
    condition,
    then_branch,
    else_branch: Some(else_branch),
} => {
    // If/else as expression - compile it and capture the result
    let result_reg = func_compiler.allocate_temp()?;
    func_compiler.compile_if_expression(
        condition,
        then_branch,
        else_branch,
        result_reg,
    )?;
    last_expr_reg = Some(result_reg);
}
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (function compilation, new compile_if_expression method)

---

### 7. Main Program Always Returning `Unit` ⚠️ **FOUNDATIONAL**

**Severity:** Medium (affects REPL and test experience)  
**Impact:** Top-level expressions didn't return their values

**Problem:**
- The `Halt` instruction always returned `Unit`
- Expression statements at the top level were evaluated but their values were discarded
- Example: `double(5)` would compute 10 but return `Unit`

**Root Cause:**
```rust
// Old VM Halt handling:
Instruction::Halt => {
    self.state = VmState::Halted;
    return Ok(BytecodeValue::Unit);  // Always Unit!
}

// Old compiler:
for statement in &ast.statements {
    self.compile_statement(statement)?;  // Results discarded
}
self.chunk.halt();
```

**Fix:**
1. Compiler: Track last expression and place it in register 0 before Halt:
```rust
let mut last_expr_reg: Option<Reg> = None;

for (i, statement) in ast.statements.iter().enumerate() {
    if i == last_stmt_index {
        if let Statement::ExprStatement(expr) = statement {
            let result = self.compile_expr_to_reg(expr)?;
            last_expr_reg = Some(result.register);
        }
    } else {
        self.compile_statement(statement)?;
    }
}

// Move result to register 0 before Halt
if let Some(expr_reg) = last_expr_reg {
    if expr_reg != 0 {
        self.chunk.move_reg(0, expr_reg);
    }
}
self.chunk.halt();
```

2. VM: Return value from register 0:
```rust
Instruction::Halt => {
    self.state = VmState::Halted;
    let result = self.get_register(0).unwrap_or(&BytecodeValue::Unit).clone();
    return Ok(result);
}
```

**Files Changed:**
- `crates/bytecode/src/compiler_v2.rs` (compile method)
- `crates/bytecode/src/vm_v2.rs` (Halt instruction handling)

---

## Testing Results

### Before Fixes
- VM core: 59/59 ✅
- Compiler integration: 32/32 ✅
- Closures: 7/12 ❌ (5 failures)
- For loops: 11/13 ❌ (2 failures)
- Real Veld files: 6/8 ❌ (2 failures)
- **Total: 115/124 (92.7%)**

### After Fixes
- VM core: 59/59 ✅
- Compiler integration: 32/32 ✅
- Closures: 12/12 ✅
- For loops: 13/13 ✅
- Real Veld files: 8/8 ✅
- Simple function call tests: 6/6 ✅
- End-to-end tests: 7/7 ✅
- **Total: 137/137 (100%)** 🎉

---

## Key Learnings

### 1. Register Count is Critical
Every function needs an accurate `register_count` set before VM execution. This isn't just an optimization - it's required for correct frame allocation.

### 2. Implicit Returns Need Special Handling
Languages with implicit returns (like Ruby, Rust, Veld) require the compiler to track and preserve the last expression's value, not just discard it.

### 3. Upvalues Need Clear Semantics
There's a difference between:
- Local variables (accessed directly by register number)
- Upvalues (accessed via GetUpvalue instruction)
- Variables that ARE upvalues (marked with `is_upvalue` flag)

### 4. Function Calls Need Careful Register Management
The calling convention must be strictly enforced:
- Arguments at `func_reg + 1, func_reg + 2, ...`
- Function must be preserved if it's in a variable (not overwritten by return value)

### 5. Test-Driven Debugging Works
Each bug was isolated with a minimal test case, fixed, then verified with the full suite.

---

## Performance Impact

No performance regressions were introduced. All fixes were correctness improvements, not optimizations. The VM now correctly executes:
- Nested closures with multi-level captures
- Functions returning closures (closure factories)
- Function calls inside loops
- If/else expressions returning values
- Top-level expressions in REPL mode

---

## Future Considerations

### Potential Improvements
1. **Better error messages**: Track source locations through compilation for better runtime error reporting
2. **Register allocation optimization**: Current approach is conservative but could be more efficient
3. **If/else expressions everywhere**: Currently only supported as function-final expressions
4. **Match expressions**: Similar treatment will be needed for match arms returning values

### Known Limitations
1. If/else as expression only works when it's the last statement in a function
2. Main program return value convention (register 0) is implicit and undocumented
3. No optimization for tail call elimination yet (would help with loops in closures)

---

## Conclusion

All foundational bugs in the register-based VM's function calling and closure mechanisms have been identified and fixed. The system now correctly handles:
- ✅ Basic function calls
- ✅ Nested function definitions
- ✅ Closures with upvalue capture
- ✅ Multi-level closure nesting
- ✅ Closure factories
- ✅ Function calls in loops
- ✅ If/else as expressions
- ✅ Implicit returns
- ✅ Main program return values

The VM is now ready for **Phase 6: Standard Library & Advanced Features**.