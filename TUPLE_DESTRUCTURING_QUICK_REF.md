# Tuple Destructuring Quick Reference

## What's Done ✅

### AST
```rust
// OLD: name: String
// NEW: pattern: Pattern
Statement::VariableDeclaration {
    pattern: Pattern,  // ← Changed
    var_kind: VarKind,
    type_annotation: Option<TypeAnnotation>,
    value: Box<Expr>,
    is_public: bool,
}
```

### Parser
```rust
fn parse_binding_pattern(&mut self) -> Result<Pattern>
fn parse_single_binding_pattern(&mut self) -> Result<Pattern>
```

### Type Checker
```rust
fn type_check_pattern_variable_declaration(
    &mut self,
    pattern: &Pattern,
    var_kind: &VarKind,
    type_annotation: Option<&TypeAnnotation>,
    value: &mut Expr,
) -> Result<()>
```

---

## What's Left ⏳

### 1. Interpreter (`crates/interpreter/src/interpreter.rs`)

**Add this function:**
```rust
fn bind_pattern(&mut self, pattern: &Pattern, value: Value) -> Result<()> {
    match pattern {
        Pattern::Identifier(name) => {
            self.current_scope_mut()
                .vals_mut()
                .insert(name.clone(), value);
            Ok(())
        }
        Pattern::Wildcard => Ok(()),
        Pattern::TuplePattern(patterns) => {
            match value {
                Value::Tuple(elements) => {
                    if patterns.len() != elements.len() {
                        return Err(VeldError::RuntimeError(format!(
                            "Tuple pattern has {} elements but value has {}",
                            patterns.len(), elements.len()
                        )));
                    }
                    for (pat, elem) in patterns.iter().zip(elements) {
                        self.bind_pattern(pat, elem)?;
                    }
                    Ok(())
                }
                _ => Err(VeldError::RuntimeError(
                    "Cannot destructure non-tuple".to_string()
                ))
            }
        }
        _ => Err(VeldError::RuntimeError(
            "Pattern not supported".to_string()
        ))
    }
}
```

**Update this line (~2951):**
```rust
// OLD:
Statement::VariableDeclaration { name, value, .. } => {
    let val = self.evaluate_expression(*value)?;
    self.current_scope_mut().vals_mut().insert(name.clone(), val);
}

// NEW:
Statement::VariableDeclaration { pattern, value, .. } => {
    let val = self.evaluate_expression(*value)?.unwrap_return();
    self.bind_pattern(pattern, val)?;
    Ok(Value::Unit)
}
```

---

### 2. Bytecode Compiler (`crates/bytecode/src/compiler_v2.rs`)

**Add this function:**
```rust
fn compile_pattern_binding(
    &mut self,
    pattern: &Pattern,
    value_reg: Reg,
    var_kind: &VarKind,
) -> Result<()> {
    match pattern {
        Pattern::Identifier(name) => {
            self.variables.insert(
                name.clone(),
                VarInfo {
                    register: value_reg,
                    is_mutable: *var_kind == VarKind::Var 
                        || *var_kind == VarKind::LetMut,
                    depth: self.scope_depth,
                    is_captured: false,
                    is_upvalue: false,
                    is_type: false,
                    is_global_ref: false,
                },
            );
            Ok(())
        }
        Pattern::Wildcard => Ok(()),
        Pattern::TuplePattern(patterns) => {
            for (i, pat) in patterns.iter().enumerate() {
                if !matches!(pat, Pattern::Wildcard) {
                    let elem_reg = self.allocate_temp()?;
                    self.chunk.tuple_get(elem_reg, value_reg, i as u8);
                    self.compile_pattern_binding(pat, elem_reg, var_kind)?;
                }
            }
            Ok(())
        }
        _ => Err(VeldError::CompileError {
            message: "Pattern not supported".to_string(),
            line: Some(self.current_line as usize),
            column: None,
        })
    }
}
```

**Update this line (~251):**
```rust
// OLD:
Statement::VariableDeclaration { name, value, .. } => {
    let val_result = self.compile_expr_to_reg(value)?;
    self.variables.insert(name.clone(), VarInfo { ... });
}

// NEW:
Statement::VariableDeclaration { pattern, var_kind, value, .. } => {
    let val_result = self.compile_expr_to_reg(value)?;
    self.compile_pattern_binding(pattern, val_result.register, var_kind)?;
    Ok(())
}
```

**Check if TupleGet instruction exists:**
```rust
// In bytecode_v2.rs, might already exist as:
TupleAccess { dest, tuple, index }
// OR need to add:
TupleGet { dest, tuple, index }
```

---

### 3. Expander (`crates/expander/src/lib.rs` + `integration.rs`)

**Update all VariableDeclaration construction (~689, ~205):**
```rust
// OLD:
Statement::VariableDeclaration {
    name: var_name.clone(),
    ...
}

// NEW:
Statement::VariableDeclaration {
    pattern: Pattern::Identifier(var_name.clone()),
    ...
}
```

---

### 4. Legacy Compiler (`crates/bytecode/src/compiler.rs`)

**Add error for unsupported patterns (~208):**
```rust
Statement::VariableDeclaration { pattern, .. } => {
    match pattern {
        Pattern::Identifier(name) => {
            // Existing code for simple binding
        }
        _ => {
            return Err(VeldError::CompileError {
                message: "Tuple destructuring not supported in legacy compiler".to_string(),
                line: None,
                column: None,
            })
        }
    }
}
```

---

## Test Examples

```veld
# Basic
let (x, y) = (10, 20)

# Wildcard
let (x, _, z) = (1, 2, 3)

# From function
fn get_pair() => (100, 200)
let (a, b) = get_pair()

# With type
let (x, y): (i32, i32) = get_coords()

# Mutable
var (counter, total) = (0, 0)
counter = 10

# In do block
let result = do
    let (a, b) = (1, 2)
    a + b
end

# With let-in
let sum = let (x, y) = (5, 10) in x + y
```

---

## Build & Test Commands

```bash
# Fix all compilation errors
cargo build --release 2>&1 | grep "error\[E"

# Run tests
cargo test -p veld-bytecode
cargo test -p veld-common

# Test file
./target/release/veld test_tuple_destructuring.veld
```

---

## Error Patterns to Look For

```bash
# Find all places using old 'name' field
grep -r "VariableDeclaration.*name" crates/

# Find all pattern matching on VariableDeclaration
grep -r "Statement::VariableDeclaration {" crates/
```

---

## Order of Implementation

1. ✅ AST (done)
2. ✅ Parser (done)
3. ✅ Type Checker (done)
4. ⏳ Fix Expander (simple - just wrap in Pattern::Identifier)
5. ⏳ Fix Legacy Compiler (add error message)
6. ⏳ Implement Interpreter (core feature)
7. ⏳ Implement Bytecode Compiler (core feature)
8. ⏳ Add Tests
9. ⏳ Verify all tests pass

---

## Time Estimates

- Expander fixes: 15 min
- Legacy compiler: 15 min
- Interpreter: 1-2 hours
- Bytecode compiler: 2-3 hours
- Testing: 1 hour
- Total: ~5-7 hours