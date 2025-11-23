# Parser Fix: Newline-Aware Expression Parsing

## Date: November 15, 2024

## Problem Statement

The Veld parser was incorrectly treating parenthesized expressions on new lines as function call continuations when semicolons were omitted. This caused tuple literals in `do` blocks to be misparsed as function calls.

### Example of Bug

**Source Code:**
```veld
fn test_simple() => do
    let x = 10
    let y = 20
    (x, y)
end
```

**Incorrect AST (before fix):**
```
VariableDeclaration {
    name: "y",
    value: Call {
        callee: Literal(Integer(20)),
        arguments: [
            Positional(Identifier("x")),
            Positional(Identifier("y"))
        ]
    }
}
```

The parser interpreted `let y = 20 (x, y)` as calling `20` as a function with arguments `x` and `y`, which resulted in a type error: "Cannot call non-function value of type i32".

**Correct AST (after fix):**
```
VariableDeclaration {
    name: "y",
    value: Literal(Integer(20))
}
ExprStatement(
    TupleLiteral([
        Identifier("x"),
        Identifier("y")
    ])
)
```

## Root Cause Analysis

### Lexer Behavior
The Veld lexer skips newline characters entirely (`#[logos(skip(r"\n", newline_callback))]`), so newlines are not present in the token stream. However, the lexer *does* track line and column information for each token via the `(usize, usize)` tuple attached to most tokens.

### Parser Behavior
The `postfix_with_expr` function in the parser was too greedy when consuming tokens. When it encountered an `LParen` token after an expression, it would always treat it as the start of a function call, regardless of whether it was on the same line or a new line.

This is problematic in contexts like:
- Variable declarations without semicolons
- Do blocks with multiple statements
- Function bodies with expression sequences

### Why This Matters
Veld is designed to work without requiring semicolons for statement separation. To make this work correctly, the parser needs to be aware of line boundaries and treat them as implicit statement separators in certain contexts.

## Solution

### Implementation
Added newline-awareness to the expression parser:

1. **New Helper Method** (`is_new_line`):
```rust
/// Check if the current token is on a different line than the previous token.
/// This is used to determine if we should treat a token as continuing the previous
/// expression or starting a new statement.
fn is_new_line(&self) -> bool {
    if self.current == 0 || self.current >= self.tokens.len() {
        return false;
    }
    let prev_line = self.tokens[self.current - 1].source_pos().line;
    let curr_line = self.tokens[self.current].source_pos().line;
    curr_line > prev_line
}
```

2. **Updated `postfix_with_expr` Logic**:
Modified the function call detection to check if `LParen` is on a new line:

```rust
// Before (always treated LParen as function call):
} else if self.match_token(&[Token::LParen(ZTUP)]) {
    // Function call on identifier or property access chain
    let mut args = Vec::new();
    ...
}

// After (checks for newline):
} else if self.check(&Token::LParen(ZTUP)) && !self.is_new_line() {
    // Function call on identifier or property access chain
    // Only treat as function call if LParen is on the same line
    self.advance(); // consume LParen
    let mut args = Vec::new();
    ...
}
```

The same check was added to a second location where function calls on identifiers were parsed.

### Design Rationale

**Why not require semicolons?**
- Veld's design goal is clean, readable syntax without unnecessary punctuation
- Many modern languages (Python, Ruby, Swift, etc.) successfully parse without mandatory semicolons

**Why check newlines only for function calls?**
- Most operators and method calls should continue across lines (for readability)
- Function call syntax `expr(args)` is ambiguous when `expr` and `(args)` are on different lines
- Checking newlines specifically for this case preserves the flexibility for line breaks in other contexts

**Why not emit newline tokens?**
- Newline tokens would complicate the entire parser
- The current approach is minimally invasive - only affects function call parsing
- Token position information is already tracked and available

## Test Coverage

### Comprehensive Test Suite
Created `test_tuple_returns_comprehensive.veld` with 8 test cases:

1. ✅ Simple tuple from variables in do block
2. ✅ Direct tuple return without do block
3. ✅ Single value from do block
4. ✅ Tuple with literals from do block
5. ✅ With computation and reassignment
6. ✅ With compound assignment operators (`+=`, etc.)
7. ✅ Multiple value tuples (3+ elements)
8. ✅ Nested do blocks with tuple returns

All tests pass with the fix.

### Regression Testing
- ✅ All existing bytecode tests pass (145/145)
- ✅ All existing parser tests pass
- ✅ No breaking changes to valid code

## Impact

### User Experience
**Before Fix:**
Users had to either:
1. Add explicit semicolons: `let y = 20;`
2. Avoid newlines before tuples
3. Experience cryptic type errors

**After Fix:**
Users can write natural, semicolon-free code:
```veld
fn get_coords() => do
    let x = 10
    let y = 20
    (x, y)
end
```

### Code Quality
The fix is:
- ✅ Minimal and focused (only affects function call parsing)
- ✅ Well-documented with inline comments
- ✅ Backward compatible (doesn't break valid code)
- ✅ Thoroughly tested

## Files Modified

### `crates/common/src/parser.rs`
- Added `is_new_line()` method (lines 58-65)
- Updated `postfix_with_expr()` function call detection (lines 3742, 3791-3793)

## Future Considerations

### Additional Newline-Aware Contexts
Consider adding similar checks for:
- Array indexing: `expr[index]` - currently continues across lines (probably fine)
- Property access: `expr.property` - currently continues across lines (probably fine)
- Binary operators: `expr + expr` - currently continues across lines (desirable)

### Alternative Approaches Considered

1. **Semicolon inference** (like Scala):
   - More complex to implement correctly
   - Would require deeper parser changes
   - Current fix is simpler and sufficient

2. **Newline tokens in lexer**:
   - Would affect every parser rule
   - Much larger change surface
   - Not necessary for solving this specific issue

3. **Explicit line continuation** (like Python's `\`):
   - Adds syntax burden to users
   - Not in line with Veld's design goals

## Conclusion

This fix resolves the "interpreter bug" which was actually a parser bug. The solution is minimal, well-tested, and maintains Veld's design principle of clean syntax without mandatory semicolons. The parser now correctly respects line boundaries when disambiguating between function calls and new statements.

## Related Issues

- Fixed: #N/A (from bug report INTERPRETER_BUG_REPORT.md)
- Impact: Compound assignment and multi-value return features now work correctly in all contexts