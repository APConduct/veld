# Tuple Destructuring Design Document

## Date: November 15, 2024

## Executive Summary

This document outlines the complete design and implementation plan for tuple destructuring in Veld. Tuple destructuring allows unpacking tuple values into multiple variables in a single declaration, making tuple usage more ergonomic and natural.

---

## Table of Contents

1. [Feature Overview](#feature-overview)
2. [Syntax Examples](#syntax-examples)
3. [Current State](#current-state)
4. [Architecture Changes](#architecture-changes)
5. [Implementation Plan](#implementation-plan)
6. [File-by-File Changes](#file-by-file-changes)
7. [Testing Strategy](#testing-strategy)
8. [Edge Cases](#edge-cases)
9. [Future Enhancements](#future-enhancements)

---

## Feature Overview

### Goal
Enable tuple destructuring in variable declarations, allowing patterns like:

```veld
let (x, y) = get_coords()
let (a, b, c) = (1, 2, 3)
var (first, second) = compute_pair()
```

### Benefits
- **Ergonomic**: No need for `.0`, `.1` tuple access
- **Clear intent**: Shows that you're unpacking multiple values
- **Functional style**: Common in modern languages (Rust, Python, JavaScript)
- **Complements existing features**: Works with multi-value returns and let-in expressions

### Non-Goals (For Now)
- Nested tuple destructuring: `let ((a, b), c) = nested_tuple()`
- Destructuring in function parameters: `fn process((x, y): (i32, i32)) => ...`
- Destructuring in for loops: `for (key, value) in pairs do ... end`
- Struct destructuring: `let Point { x, y } = point`

These can be added later as enhancements.

---

## Syntax Examples

### Basic Destructuring

```veld
# Simple tuple destructuring
let (x, y) = (10, 20)
# x = 10, y = 20

# From function call
fn get_coords() => (100, 200)
let (x, y) = get_coords()
```

### With Type Annotations

```veld
# Type annotation on entire tuple
let (x, y): (i32, i32) = get_pair()

# Future: Per-element type annotations (not in initial implementation)
# let (x: i32, y: i32) = get_pair()
```

### With Wildcards

```veld
# Ignore some elements
let (x, _, z) = (1, 2, 3)
# x = 1, z = 3, middle element ignored

# Ignore all but one
let (_, y, _) = compute_triple()
```

### With Mutable Variables

```veld
# Mutable tuple destructuring
var (counter, total) = (0, 0)
counter = counter + 1
total = total + 10
```

### Empty Tuple (Unit)

```veld
# Empty tuple pattern (matches unit type)
let () = do_something_with_side_effects()
```

### Parenthesized Identifiers

```veld
# Single identifier in parens is NOT a tuple
let (x) = 42  # Same as: let x = 42

# Needs comma to be a tuple
let (x,) = (42,)  # Single-element tuple
```

---

## Current State

### What's Been Done

1. **AST Updated**: 
   - Changed `Statement::VariableDeclaration` from `name: String` to `pattern: Pattern`
   - `Pattern` enum already exists with `TuplePattern(Vec<Pattern>)` variant

2. **Parser Updated**:
   - Added `parse_binding_pattern()` - Parses tuple patterns or identifiers
   - Added `parse_single_binding_pattern()` - Parses individual pattern elements
   - Handles `(x, y)` syntax with proper comma detection

3. **Type Checker Updated**:
   - Added `type_check_pattern_variable_declaration()`
   - Type checks tuple destructuring
   - Validates pattern length matches value tuple length
   - Binds each pattern element to its corresponding type

4. **Module System Updated**:
   - Module registry handles pattern-based declarations
   - Module exports handle pattern-based declarations

### What Needs to Be Done

The following files still reference `name` field and need updates:

1. **Interpreter** (`crates/interpreter/src/interpreter.rs`)
2. **Bytecode Compiler** (`crates/bytecode/src/compiler_v2.rs`)
3. **Legacy Compiler** (`crates/bytecode/src/compiler.rs`)
4. **Expander** (`crates/expander/src/lib.rs`, `crates/expander/src/integration.rs`)

---

## Architecture Changes

### AST Changes

**Before:**
```rust
Statement::VariableDeclaration {
    name: String,
    var_kind: VarKind,
    type_annotation: Option<TypeAnnotation>,
    value: Box<Expr>,
    is_public: bool,
}
```

**After:**
```rust
Statement::VariableDeclaration {
    pattern: Pattern,  // Changed from name: String
    var_kind: VarKind,
    type_annotation: Option<TypeAnnotation>,
    value: Box<Expr>,
    is_public: bool,
}
```

### Pattern Types

The `Pattern` enum supports:

```rust
pub enum Pattern {
    Identifier(String),           // Simple variable: x
    Wildcard,                     // Ignore: _
    TuplePattern(Vec<Pattern>),   // Tuple: (x, y, z)
    Literal(Literal),             // Not used in declarations
    EnumPattern { ... },          // Not used in declarations yet
    StructPattern { ... },        // Not used in declarations yet
}
```

For the initial implementation, we only support:
- `Pattern::Identifier(name)` - Simple variable binding
- `Pattern::Wildcard` - Ignore value (no binding)
- `Pattern::TuplePattern(patterns)` - Tuple destructuring with nested identifiers/wildcards

---

## Implementation Plan

### Phase 1: Core Infrastructure ✅ (Complete)
- [x] Update AST structure
- [x] Update parser to parse patterns
- [x] Update type checker
- [x] Update module system

### Phase 2: Runtime Support (Current Phase)
- [ ] Update interpreter to evaluate pattern destructuring
- [ ] Update bytecode compiler to compile pattern destructuring
- [ ] Add runtime checks for tuple length mismatches

### Phase 3: Testing
- [ ] Create comprehensive test suite
- [ ] Test simple cases
- [ ] Test edge cases (wildcards, empty tuples, nested tuples)
- [ ] Test with let-in expressions
- [ ] Test with do blocks
- [ ] Test mutable variables

### Phase 4: Documentation
- [ ] Update language documentation
- [ ] Add examples to stdlib
- [ ] Create tutorial/guide

---

## File-by-File Changes

### 1. Interpreter (`crates/interpreter/src/interpreter.rs`)

**Current Code Pattern:**
```rust
Statement::VariableDeclaration { name, var_kind, value, .. } => {
    let value_result = self.evaluate_expression(*value)?;
    self.current_scope_mut().vals_mut().insert(name.clone(), value_result);
}
```

**Required Changes:**

1. **Add pattern evaluation helper:**
```rust
fn bind_pattern(&mut self, pattern: &Pattern, value: Value) -> Result<()> {
    match pattern {
        Pattern::Identifier(name) => {
            self.current_scope_mut().vals_mut().insert(name.clone(), value);
            Ok(())
        }
        Pattern::Wildcard => {
            // Don't bind anything
            Ok(())
        }
        Pattern::TuplePattern(patterns) => {
            match value {
                Value::Tuple(elements) => {
                    if patterns.len() != elements.len() {
                        return Err(VeldError::RuntimeError(format!(
                            "Tuple destructuring: expected {} elements, got {}",
                            patterns.len(),
                            elements.len()
                        )));
                    }
                    for (pat, elem) in patterns.iter().zip(elements.into_iter()) {
                        self.bind_pattern(pat, elem)?;
                    }
                    Ok(())
                }
                _ => Err(VeldError::RuntimeError(
                    "Cannot destructure non-tuple value".to_string()
                ))
            }
        }
        _ => Err(VeldError::RuntimeError(
            "Pattern not supported in variable declaration".to_string()
        ))
    }
}
```

2. **Update variable declaration handling:**
```rust
Statement::VariableDeclaration { pattern, var_kind, value, .. } => {
    let value_result = self.evaluate_expression(*value)?.unwrap_return();
    self.bind_pattern(pattern, value_result)?;
    Ok(Value::Unit)
}
```

**Locations to Update:**
- Line ~2951: `execute_variable_declaration()`
- Any helper functions that construct VariableDeclaration statements

---

### 2. Bytecode Compiler V2 (`crates/bytecode/src/compiler_v2.rs`)

**Current Code Pattern:**
```rust
Statement::VariableDeclaration { name, value, .. } => {
    let value_result = self.compile_expr_to_reg(value)?;
    let var_reg = value_result.register;
    self.variables.insert(name.clone(), VarInfo { register: var_reg, .. });
}
```

**Required Changes:**

1. **Add pattern compilation helper:**
```rust
fn compile_pattern_binding(
    &mut self,
    pattern: &Pattern,
    value_reg: Reg,
    var_kind: &VarKind,
) -> Result<()> {
    match pattern {
        Pattern::Identifier(name) => {
            // Simple binding - register variable
            self.variables.insert(
                name.clone(),
                VarInfo {
                    register: value_reg,
                    is_mutable: *var_kind == VarKind::Var || *var_kind == VarKind::LetMut,
                    depth: self.scope_depth,
                    is_captured: false,
                    is_upvalue: false,
                    is_type: false,
                    is_global_ref: false,
                },
            );
            Ok(())
        }
        Pattern::Wildcard => {
            // Don't bind anything, but register was still used
            // Could free the register here if needed
            Ok(())
        }
        Pattern::TuplePattern(patterns) => {
            // Emit tuple destructuring bytecode
            for (i, pat) in patterns.iter().enumerate() {
                if !matches!(pat, Pattern::Wildcard) {
                    // Get tuple element at index i
                    let elem_reg = self.allocate_temp()?;
                    self.chunk.tuple_get(elem_reg, value_reg, i as u8);
                    
                    // Recursively bind pattern
                    self.compile_pattern_binding(pat, elem_reg, var_kind)?;
                }
            }
            Ok(())
        }
        _ => Err(VeldError::CompileError {
            message: "Pattern not supported in variable declaration".to_string(),
            line: Some(self.current_line as usize),
            column: None,
        })
    }
}
```

2. **Add TupleGet instruction (if not exists):**
```rust
// In bytecode.rs or bytecode_v2.rs
pub enum Instruction {
    // ... existing instructions ...
    TupleGet {
        dest: Reg,
        tuple: Reg,
        index: u8,
    },
}
```

3. **Implement TupleGet in VM:**
```rust
// In vm_v2.rs
Instruction::TupleGet { dest, tuple, index } => {
    let tuple_val = self.get_register(*tuple)?;
    match tuple_val {
        BytecodeValue::Tuple(elements) => {
            if (*index as usize) < elements.len() {
                self.set_register(*dest, elements[*index as usize].clone())?;
            } else {
                return Err("Tuple index out of bounds".to_string());
            }
        }
        _ => return Err("TupleGet on non-tuple value".to_string()),
    }
}
```

4. **Update variable declaration compilation:**
```rust
Statement::VariableDeclaration { pattern, var_kind, value, .. } => {
    let value_result = self.compile_expr_to_reg(value)?;
    self.compile_pattern_binding(pattern, value_result.register, var_kind)?;
    
    // Free temporary if needed
    if value_result.is_temp && !matches!(pattern, Pattern::Identifier(_)) {
        self.free_temp(value_result.register);
    }
    
    Ok(())
}
```

**Locations to Update:**
- Line ~251: `compile_var_declaration()`
- Any other places that handle VariableDeclaration

**Note:** TupleGet instruction might already exist as TupleAccess or similar. Check existing bytecode instructions first.

---

### 3. Legacy Compiler (`crates/bytecode/src/compiler.rs`)

**Decision:** Mark as deprecated or add minimal support.

**Recommended Approach:**
```rust
Statement::VariableDeclaration { pattern, .. } => {
    // Only support simple identifier patterns in legacy compiler
    match pattern {
        Pattern::Identifier(name) => {
            // Existing simple binding code
            // ...
        }
        _ => {
            return Err(VeldError::CompileError {
                message: "Tuple destructuring not supported in legacy compiler. Use RegisterCompiler.".to_string(),
                line: None,
                column: None,
            })
        }
    }
}
```

**Locations to Update:**
- Line ~208: Variable declaration compilation

---

### 4. Expander (`crates/expander/src/lib.rs`)

**Current Usage:**
The expander likely constructs VariableDeclaration statements during macro expansion.

**Required Changes:**

Update any code that constructs `VariableDeclaration` to use `Pattern::Identifier(name)` instead of just `name`.

**Example:**
```rust
// Before:
Statement::VariableDeclaration {
    name: var_name.clone(),
    var_kind: VarKind::Let,
    type_annotation: None,
    value: Box::new(expanded_expr),
    is_public: false,
}

// After:
Statement::VariableDeclaration {
    pattern: Pattern::Identifier(var_name.clone()),
    var_kind: VarKind::Let,
    type_annotation: None,
    value: Box::new(expanded_expr),
    is_public: false,
}
```

**Locations to Update:**
- Line ~689: Macro expansion variable binding
- Any helper functions that create variable declarations

---

### 5. Expander Integration (`crates/expander/src/integration.rs`)

Similar to above, update any code that matches or constructs VariableDeclaration statements.

**Locations to Update:**
- Line ~205: Pattern matching on VariableDeclaration

---

## Testing Strategy

### Test Categories

#### 1. Basic Functionality Tests

```veld
# Test: Simple tuple destructuring
let (x, y) = (10, 20)
assert(x == 10)
assert(y == 20)

# Test: Three elements
let (a, b, c) = (1, 2, 3)
assert(a == 1 && b == 2 && c == 3)

# Test: From function
fn get_pair() => (100, 200)
let (x, y) = get_pair()
assert(x == 100 && y == 200)
```

#### 2. Type Checking Tests

```veld
# Test: Type annotation
let (x, y): (i32, i32) = (1, 2)

# Test: Type mismatch should fail
# let (x, y): (i32, i32) = ("hello", "world")  # Error

# Test: Length mismatch should fail
# let (x, y) = (1, 2, 3)  # Error
```

#### 3. Wildcard Tests

```veld
# Test: Ignore middle element
let (x, _, z) = (1, 2, 3)
assert(x == 1 && z == 3)

# Test: Ignore all but one
let (_, y, _) = (10, 20, 30)
assert(y == 20)

# Test: All wildcards (valid but unusual)
let (_, _, _) = (1, 2, 3)
```

#### 4. Mutable Variable Tests

```veld
# Test: Var destructuring
var (counter, total) = (0, 0)
counter = 10
total = 100
assert(counter == 10 && total == 100)

# Test: Modify destructured vars
var (x, y) = (5, 10)
x += 5
y *= 2
assert(x == 10 && y == 20)
```

#### 5. Scope Tests

```veld
# Test: Destructuring in do block
let result = do
    let (a, b) = (1, 2)
    a + b
end
assert(result == 3)

# Test: Destructuring with let-in
let result = let (x, y) = (10, 20) in x + y
assert(result == 30)
```

#### 6. Edge Cases

```veld
# Test: Empty tuple (unit)
let () = ()

# Test: Single element tuple needs comma
let (x,) = (42,)
assert(x == 42)

# Test: Parenthesized identifier (NOT a tuple)
let (x) = 42
assert(x == 42)

# Test: Nested tuples (should fail for now)
# let ((a, b), c) = ((1, 2), 3)  # Error: not supported yet
```

#### 7. Error Cases

```veld
# Test: Length mismatch
# let (x, y) = (1, 2, 3)  # Error: pattern has 2 elements, value has 3

# Test: Type mismatch
# let (x, y): (i32, i32) = (1, "hello")  # Error: type mismatch

# Test: Non-tuple value
# let (x, y) = 42  # Error: cannot destructure non-tuple
```

### Test File Structure

Create `test_tuple_destructuring.veld`:
- 20+ test cases
- Cover all basic functionality
- Cover edge cases
- Cover error cases (commented out with expected error)

---

## Edge Cases

### 1. Empty Tuple (Unit Type)
```veld
let () = ()  # Should work - matches unit type
let () = do_side_effect()  # Useful for functions that return ()
```

### 2. Single-Element Tuples
```veld
let (x,) = (42,)  # Single-element tuple (note the comma)
let (x) = 42      # NOT a tuple - just parenthesized identifier
```

### 3. Nested Tuples
```veld
# Not supported in initial version - should give clear error
let ((a, b), c) = ((1, 2), 3)  # Error: nested patterns not yet supported
```

### 4. Mixed Patterns
```veld
let (x, _, z) = (1, 2, 3)  # Mix of identifiers and wildcards - OK
let (_, _, _) = compute()   # All wildcards - valid but unusual
```

### 5. Type Annotations on Tuple
```veld
let (x, y): (i32, i32) = get_pair()  # Type annotation on whole tuple - OK
# Future: let (x: i32, y: i32) = get_pair()  # Per-element annotations
```

### 6. Mutability
```veld
var (x, y) = (1, 2)  # Both x and y are mutable
# Cannot mix: var (x, let y) = ...  # Not supported
```

### 7. Scope and Shadowing
```veld
let x = 10
let (x, y) = (20, 30)  # x shadows outer x
# x is now 20 in this scope
```

---

## Future Enhancements

### Phase 2 Features (Not in Initial Implementation)

#### 1. Function Parameter Destructuring
```veld
fn process((x, y): (i32, i32)) => x + y
fn distance((x1, y1): (f64, f64), (x2, y2): (f64, f64)) => ...
```

#### 2. For Loop Destructuring
```veld
for (key, value) in pairs do
    io.println(key + ": " + value.to_str())
end
```

#### 3. Nested Tuple Destructuring
```veld
let ((a, b), (c, d)) = ((1, 2), (3, 4))
```

#### 4. Struct Destructuring
```veld
struct Point { x: i32, y: i32 }
let Point { x, y } = point
let Point { x: px, y: py } = point  # With renaming
```

#### 5. Rest Patterns
```veld
let (first, ..rest) = (1, 2, 3, 4, 5)
# first = 1, rest = (2, 3, 4, 5)
```

#### 6. Per-Element Type Annotations
```veld
let (x: i32, y: f64) = get_mixed_tuple()
```

#### 7. Match Arm Patterns (Enhanced)
```veld
match value
    (0, 0) => "origin"
    (x, 0) => "on x-axis"
    (0, y) => "on y-axis"
    (x, y) => "point at " + x.to_str() + "," + y.to_str()
end
```

---

## Error Messages

### Parser Errors

```
Error: Expected identifier or wildcard in pattern, found Token::IntegerLiteral(42)
  --> example.veld:1:9
   |
1  | let (42, y) = pair
   |      ^^ literals not allowed in binding patterns
```

### Type Checker Errors

```
Error: Tuple destructuring pattern has 2 elements but value has 3
  --> example.veld:1:5
   |
1  | let (x, y) = (1, 2, 3)
   |     ^^^^^^ pattern expects 2 elements, but the tuple has 3 elements
```

```
Error: Cannot destructure non-tuple type: i32
  --> example.veld:1:5
   |
1  | let (x, y) = 42
   |     ^^^^^^ cannot destructure value of type i32
   |
   = help: tuple destructuring requires a tuple value on the right-hand side
```

### Runtime Errors

```
Runtime error: Tuple destructuring: expected 2 elements, got 3
  --> example.veld:5:9
   |
5  |     let (x, y) = compute_tuple()
   |         ^^^^^^ this pattern expects 2 elements
   |
   = note: the function returned a tuple with 3 elements
```

---

## Implementation Checklist

### Core Changes
- [x] Update AST structure (VariableDeclaration)
- [x] Update Parser (parse_binding_pattern, parse_single_binding_pattern)
- [x] Update Type Checker (type_check_pattern_variable_declaration)
- [x] Update Module Registry
- [x] Update Module Exports

### Interpreter
- [ ] Add bind_pattern() helper function
- [ ] Update execute_variable_declaration()
- [ ] Add runtime checks for tuple length
- [ ] Handle wildcards correctly
- [ ] Test with Value::Tuple

### Bytecode Compiler
- [ ] Add compile_pattern_binding() helper
- [ ] Add/verify TupleGet instruction exists
- [ ] Implement TupleGet in VM
- [ ] Update compile_var_declaration()
- [ ] Handle register allocation for destructured values
- [ ] Free temporary registers appropriately

### Legacy Compiler
- [ ] Add error for unsupported patterns
- [ ] Support simple identifiers only

### Expander
- [ ] Update macro expansion to use Pattern::Identifier
- [ ] Fix any pattern matching on VariableDeclaration

### Testing
- [ ] Create test_tuple_destructuring.veld
- [ ] Test basic destructuring (10 tests)
- [ ] Test wildcards (3 tests)
- [ ] Test edge cases (5 tests)
- [ ] Test error cases (5 tests)
- [ ] Test with let-in expressions
- [ ] Test with do blocks
- [ ] Test with mutable variables

### Documentation
- [ ] Update language guide
- [ ] Add examples to stdlib
- [ ] Document limitations
- [ ] Document error messages

---

## Success Criteria

The feature is complete when:

1. ✅ **Parser Works**: Can parse `let (x, y) = (1, 2)`
2. ✅ **Type Checker Works**: Validates tuple length and types
3. ⏳ **Interpreter Works**: Can execute tuple destructuring
4. ⏳ **Bytecode Works**: Can compile and run tuple destructuring
5. ⏳ **Tests Pass**: All 20+ test cases pass
6. ⏳ **No Regressions**: All existing tests still pass
7. ⏳ **Documentation**: Feature is documented with examples

---

## Timeline Estimate

- **Interpreter Implementation**: 2-3 hours
- **Bytecode Implementation**: 3-4 hours (including TupleGet instruction if needed)
- **Testing**: 2 hours
- **Bug Fixes & Polish**: 2 hours
- **Documentation**: 1 hour

**Total**: ~10-12 hours of focused work

---

## Conclusion

Tuple destructuring is a natural extension of Veld's tuple support and will significantly improve the ergonomics of working with multi-value returns. The design is straightforward, builds on existing infrastructure (Pattern enum), and has clear boundaries for the initial implementation.

The phased approach (simple patterns first, complex patterns later) allows us to deliver value quickly while keeping the door open for future enhancements.

**Next Step**: Begin Phase 2 implementation starting with the interpreter.