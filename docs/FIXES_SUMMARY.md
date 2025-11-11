# Fixes Summary - println~ Macro Limitations

## Date: 2024

This document summarizes the fixes applied to resolve limitations in the `println~` macro and related functionality.

---

## ✅ Fix #1: Array Printing (GcRef Display Issue)

### Problem
Arrays and other GC-managed values were displaying as `GcRef(...)` instead of their actual contents when used in `println~` interpolation.

**Example of the issue:**
```veld
let numbers = [1, 2, 3, 4, 5]
println~("Numbers: {numbers}")
// Output: Numbers: GcRef(0x...)  ❌
```

### Root Cause
The `value_to_string` function in `interpreter.rs` had commented-out code for handling `Value::GcRef` cases. When arrays were allocated via GC, they were wrapped in `GcRef`, but the string conversion didn't dereference them.

### Solution
Implemented GcRef dereferencing in the `value_to_string` function:

```rust
Value::GcRef(handle) => {
    // Dereference GcRef and recursively convert the actual value to string
    let allocator = self.allocator.read().unwrap();
    if let Some(actual_value) = allocator.get_value(handle) {
        self.value_to_string(actual_value)
    } else {
        Ok(format!("GcRef({:?})", handle))
    }
}
```

This mirrors the pattern already used in other parts of the codebase (e.g., `io_print`, `io_println`, `get_property`).

### Result
Arrays now display their contents correctly:
```veld
let numbers = [1, 2, 3, 4, 5]
println~("Numbers: {numbers}")
// Output: Numbers: [1, 2, 3, 4, 5]  ✅
```

**Files Modified:**
- `crates/interpreter/src/interpreter.rs` (lines ~5012-5020)

---

## ✅ Fix #2: Logical Operator Precedence

### Problem
Expressions with `and`/`or` logical operators combined with comparisons would fail to parse or produce incorrect results in interpolation.

**Example of the issue:**
```veld
let num = 15
println~("In range: {num >= 10 and num <= 20}")
// Program would silently fail or parse incorrectly  ❌
```

### Root Cause
The expression parser in `parse_interpolation_expr` was checking for operators in the wrong order:
1. It checked comparison operators (>=, <=, ==, etc.) first
2. Then it checked logical operators (and, or)

This caused `num >= 10 and num <= 20` to be incorrectly parsed as:
- Left: `num >= 10` ❌ (but the right side included "and num <= 20")
- The parser would split on `>=` first, creating `num` and `10 and num <= 20`

### Solution
Reordered operator precedence checking to match standard precedence rules:

1. **Logical operators** (and, or) - **LOWEST precedence** - checked first
2. **Comparisons** (==, !=, <, >, <=, >=)
3. **Addition/Subtraction** (+, -)
4. **Multiplication/Division/Modulo** (*, /, %) - **HIGHEST precedence**

The parser now correctly handles expressions by finding the lowest-precedence operator first, which becomes the root of the expression tree.

### Result
Complex boolean expressions now work correctly:
```veld
let num = 15
println~("In range: {num >= 10 and num <= 20}")
// Output: In range: true  ✅
```

**Files Modified:**
- `crates/expander/src/integration.rs` (lines ~758-815)

**Key Change:**
Moved logical operator checks (and/or) to appear BEFORE comparison operator checks in the parsing order.

---

## Test Results

All tests in `tests/print_macro_test.veld` now pass successfully:

- ✅ Test 6: Array Interpolation - Arrays display correctly
- ✅ Test 15: Boolean Expressions - Complex `and`/`or` expressions work
- ✅ Test 16: String Operations - String concatenation in expressions
- ✅ Test 17: Comparison Results - All comparison operators
- ✅ All 20 tests complete without errors

---

## Remaining Limitations

These limitations still exist and may be addressed in future work:

### 1. Complex Nested Parentheses
**Status:** Not yet fixed

The simple recursive descent parser may fail on deeply nested or complex parenthesized expressions:
```veld
// Workaround: Use intermediate variables
let complex = (a + b) * (c - d)
println~("Result: {complex}")
```

### 2. Method Calls with Arguments
**Status:** Not yet fixed

Only zero-argument method calls are supported in interpolation:
```veld
// This works:
println~("Length: {str.len()}")

// This doesn't work:
println~("Substring: {str.substring(0, 5)}")
```

### 3. Format Specifiers
**Status:** Not yet implemented

Advanced formatting features are not available:
```veld
// Not yet supported:
println~("Debug: {value:?}")
println~("Hex: {num:x}")
println~("Precision: {pi:.2}")
```

### 4. Positional and Named Arguments
**Status:** Not yet implemented

```veld
// Not yet supported:
println~("{0} {1} {0}", x, y)
println~("{name}: {value}", name="x", value=10)
```

---

## Performance Notes

Both fixes maintain the compile-time expansion approach:
- No runtime parsing overhead
- All type checking happens at compile time
- Efficient string concatenation
- GC dereferencing adds minimal overhead (one read-lock and hash lookup)

---

## Migration Guide

If you previously had workarounds for these issues, you can now remove them:

### Before (Array Workaround)
```veld
let arr = [1, 2, 3]
io.print("Array: [")
for item in arr do
    print~("{item} ")
end
io.println("]")
```

### After (Direct Array Printing)
```veld
let arr = [1, 2, 3]
println~("Array: {arr}")
```

### Before (Complex Boolean Workaround)
```veld
let in_range = num >= 10
let also_under_max = num <= 20
let valid = in_range and also_under_max
println~("Valid: {valid}")
```

### After (Direct Expression)
```veld
println~("Valid: {num >= 10 and num <= 20}")
```

---

## Testing Recommendations

When adding new features or modifications to the interpolation system:

1. **Test GC-managed types** - Arrays, strings, tuples, structs
2. **Test operator precedence** - Mix different operator types in one expression
3. **Test edge cases** - Empty arrays, nested arrays, complex boolean logic
4. **Add regression tests** - Ensure fixes remain stable

---

## Related Documentation

- [PRINTLN_MACRO_GUIDE.md](./PRINTLN_MACRO_GUIDE.md) - Complete usage guide
- Conversation thread: "Option unwrap and HashMap aliasing"
- Test file: `tests/print_macro_test.veld`

---

## Credits

Fixes implemented based on the conversation in thread "Option unwrap and HashMap aliasing" which covered:
- HashMap implementation and re-export fixes
- Module import/aliasing improvements  
- Native method registration
- println~ macro interpolation development
- These specific bug fixes

---

## Future Work Priorities

Based on the remaining limitations, the recommended priority order for future work:

### High Priority
1. **Improve expression parser** - Use or integrate with the main Veld parser for full precedence support and nested parentheses
2. **Better error messages** - When interpolation expressions fail to parse, provide clear feedback

### Medium Priority
3. **Format specifiers** - Implement `:?` for debug, `:x` for hex, `:.N` for precision
4. **Method calls with arguments** - Parse and expand method call argument lists
5. **Positional arguments** - Support `{0}`, `{1}`, etc.

### Low Priority
6. **Named arguments** - Support `name=value` syntax
7. **Alignment and padding** - Implement width, fill, and alignment specifiers
8. **Performance optimization** - Reduce temporary string allocations during expansion

---

## Changelog

### v0.1.4 (Current)
- ✅ Fixed array printing (GcRef display issue)
- ✅ Fixed logical operator precedence in interpolation expressions
- Updated documentation to reflect fixes

### v0.1.3 (Previous)
- Initial println~ macro with basic interpolation support
- Support for arithmetic, comparisons, property access
- Known issues with arrays and operator precedence