# Do Block Parser Fix

## Date: November 15, 2024

## Summary

Fixed a critical parser bug that prevented `var` declarations and assignments from working inside `do` blocks. The parser was incorrectly treating assignments as potential final expressions, causing it to fail when encountering statements like `x = 15` in block contexts.

---

## Problem Description

### Symptom
Code like the following would fail with a parser error:

```veld
let result = do
    var counter = 0
    counter = counter + 10
    counter
end
```

**Error:**
```
Error: ParserError("Expected 'end' after block expression")
```

### Root Cause

The block expression parser had multiple issues:

1. **Missing statement recognition**: `check_statement_start()` didn't include `Token::Var` or `Token::Const`, so var/const declarations weren't recognized as valid statements in blocks.

2. **Assignment misclassification**: The `is_likely_final_expression()` function would see an identifier like `x` at the start of `x = 15` and incorrectly classify it as a final expression rather than an assignment statement.

3. **Premature expression parsing**: When `is_likely_final_expression()` returned true for assignments, the parser would call `expression()` which would parse just the identifier and stop at the `=`, leaving the block in an invalid state.

---

## Root Cause Analysis

### Parser Flow (Before Fix)

1. Parse `var counter = 0` as a statement ✓
2. Loop back to parse next statement
3. See token `counter` (identifier)
4. Call `is_likely_final_expression()` → returns **true** (incorrect)
5. Call `expression()` → parses just `counter`, stops at `=`
6. Break from loop
7. Try to consume `end` but find `=` token ✗
8. Parser error!

### Why `is_likely_final_expression()` Failed

The function checked if the current token could start an expression and if `end` followed. However, it didn't check if the token sequence was actually an **assignment statement** (identifier followed by `=` or compound assignment operator).

---

## Solution

### Fix 1: Add Var/Const to Statement Recognition

**File**: `crates/common/src/parser.rs`

**Change**: Updated `check_statement_start()` to include `Var` and `Const` tokens.

```rust
fn check_statement_start(&self) -> bool {
    self.check(&Token::If(ZTUP))
        || self.check(&Token::While(ZTUP))
        || self.check(&Token::For(ZTUP))
        || self.check(&Token::Return(ZTUP))
        || matches!(self.peek(), Token::Let(_))
        || matches!(self.peek(), Token::Var(_))      // Added
        || matches!(self.peek(), Token::Const(_))    // Added
        || self.check(&Token::Do(ZTUP))
}
```

### Fix 2: Detect Assignments in `is_likely_final_expression()`

**File**: `crates/common/src/parser.rs`

**Change**: Added check for assignment operators before treating an identifier as a final expression.

```rust
fn is_likely_final_expression(&self) -> bool {
    // ... existing checks ...
    
    _ => {
        // Check if this looks like an assignment statement
        if self.check_assignment() {
            return false;  // It's a statement, not a final expression
        }
        
        // Look ahead to see if 'end' comes after this expression
        let result = self.expression_followed_by_end();
        result
    }
}
```

This ensures that sequences like `x = 15` are recognized as assignment statements and not misinterpreted as expression `x` followed by unexpected tokens.

---

## Test Coverage

Created comprehensive test suite with **11 tests**, all passing:

### Test Cases

1. ✅ Simple var in do block
2. ✅ Var with reassignment
3. ✅ Multiple reassignments
4. ✅ Compound assignments (`+=`, `-=`, `*=`, `/=`)
5. ✅ Multiple vars in same block
6. ✅ Mix of let and var
7. ✅ Nested do blocks with var
8. ✅ Functions with var in do blocks
9. ✅ Var with conditional logic
10. ✅ Assignment as non-final statement
11. ✅ Complex computations with var

### Test File

`test_do_block_var_assignment.veld` - Comprehensive test suite for do blocks with var assignments

---

## Examples

### Before Fix (Failed)

```veld
let result = do
    var counter = 0
    counter = counter + 10
    counter = counter + 5
    counter
end
# Error: ParserError("Expected 'end' after block expression")
```

### After Fix (Works!)

```veld
let result = do
    var counter = 0
    counter = counter + 10
    counter = counter + 5
    counter
end
# result = 15 ✓
```

### Complex Example

```veld
fn accumulate(start: i32) => do
    var total = start
    total += 10
    total *= 2
    total -= 5
    let bonus = 100
    total + bonus
end

let final = accumulate(5)
# final = 125 ✓
```

---

## Impact

### What Now Works

✅ Var declarations in do blocks  
✅ Assignments to var in do blocks  
✅ Compound assignments in do blocks  
✅ Mix of let/var in same block  
✅ Nested do blocks with var  
✅ Functions using do blocks with var  

### Backward Compatibility

✅ All existing tests pass (326 tests)  
✅ No breaking changes  
✅ Let declarations continue to work as before  

---

## Related Issues

This fix resolves the edge case discovered while testing the let-in expression feature. The issue was that do blocks with mutable variables and assignments were not properly supported by the parser.

---

## Files Modified

- `crates/common/src/parser.rs`:
  - `check_statement_start()` - Added Var/Const recognition
  - `is_likely_final_expression()` - Added assignment detection
  - `parse_block_expression()` - Improved statement/expression distinction

---

## Testing

### Regression Tests
- ✅ All bytecode tests pass (93/93)
- ✅ All common crate tests pass (213/213)
- ✅ All parser tests pass (20/20)

### New Tests
- ✅ test_do_block_var_assignment.veld (11/11 tests)
- ✅ test_let_in.veld (15/15 tests - still passing)
- ✅ test_tuple_returns_comprehensive.veld (8/8 tests - still passing)

---

## Conclusion

The parser now correctly handles all forms of variable declarations and assignments within do blocks. This was a critical fix that enables proper use of mutable variables in Veld's expression-based block syntax.

**Status: COMPLETE ✅**