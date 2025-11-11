# Changelog - println~ Macro Fixes

## [Unreleased] - 2024

### Fixed

#### 🎉 Array and GC-Managed Value Display in Interpolation
**Issue:** Arrays and other garbage-collected values displayed as `GcRef(0x...)` instead of their actual contents when used in `println~` interpolation.

**Example:**
```veld
let numbers = [1, 2, 3, 4, 5]
println~("Numbers: {numbers}")
// Before: Numbers: GcRef(0x7f...)
// After:  Numbers: [1, 2, 3, 4, 5]  ✅
```

**Technical Details:**
- Implemented proper GcRef dereferencing in the `value_to_string` function
- The function now reads the GC allocator and retrieves the actual value
- Recursively converts the dereferenced value to its string representation
- Mirrors the pattern used in other parts of the codebase (io_print, io_println, get_property)

**Files Changed:**
- `crates/interpreter/src/interpreter.rs` - Added GcRef handling in value_to_string (lines ~5012-5020)

**Impact:**
- All GC-managed types now display correctly: arrays, strings, tuples, structs
- No more confusing GcRef output in debug/print statements
- Consistent behavior across the entire println~ macro system

---

#### 🎉 Logical Operator Precedence in Interpolation Expressions
**Issue:** Expressions combining logical operators (`and`, `or`) with comparisons would fail silently or parse incorrectly.

**Example:**
```veld
let num = 15
println~("In range: {num >= 10 and num <= 20}")
// Before: Program fails silently
// After:  In range: true  ✅
```

**Technical Details:**
- Fixed operator precedence in the interpolation expression parser
- Parser now checks operators in the correct order (lowest to highest precedence):
  1. Logical operators (and, or) - LOWEST precedence
  2. Comparisons (==, !=, <, >, <=, >=)
  3. Addition/Subtraction (+, -)
  4. Multiplication/Division/Modulo (*, /, %) - HIGHEST precedence
- This ensures the expression tree is built correctly with proper associativity

**Root Cause:**
The parser was checking comparison operators before logical operators, causing expressions like `num >= 10 and num <= 20` to be incorrectly split on `>=` first, resulting in malformed parse trees.

**Files Changed:**
- `crates/expander/src/integration.rs` - Reordered operator precedence checks (lines ~758-815)

**Impact:**
- Complex boolean expressions now work correctly
- Enables natural expression of range checks and compound conditions
- Matches standard operator precedence rules from other languages

---

### Test Coverage

All tests now pass successfully:
- ✅ `tests/print_macro_test.veld` - All 20 comprehensive tests pass
- ✅ `tests/fixed_features_demo.veld` - Demonstrates both fixes with real-world examples
- ✅ `examples/println_demo.veld` - Basic demo still works
- ✅ `tests/hashmap_alias_test.veld` - Unaffected by changes

---

### Documentation Updates

Updated documentation to reflect fixes:
- [x] `docs/PRINTLN_MACRO_GUIDE.md` - Updated limitations section
- [x] `docs/FIXES_SUMMARY.md` - Comprehensive technical summary of both fixes
- [x] `docs/FIXES_QUICK_REF.md` - Quick reference guide for developers
- [x] `tests/fixed_features_demo.veld` - Demonstration file with examples

---

### Breaking Changes

None. These are pure bug fixes with no API changes.

---

### Migration Guide

No migration needed! If you had workarounds for these issues, you can now simplify your code:

**Array Printing Workaround (No Longer Needed):**
```veld
# Before:
let arr = [1, 2, 3]
io.print("Array: [")
for item in arr do
    print~("{item} ")
end
io.println("]")

# After (direct):
let arr = [1, 2, 3]
println~("Array: {arr}")
```

**Boolean Expression Workaround (No Longer Needed):**
```veld
# Before:
let in_range = num >= 10
let also_under_max = num <= 20
let valid = in_range and also_under_max
println~("Valid: {valid}")

# After (direct):
println~("Valid: {num >= 10 and num <= 20}")
```

---

### Known Remaining Limitations

These limitations still exist and are documented for future work:

1. **Complex Nested Parentheses** - Deeply nested expressions may not parse correctly
   - Workaround: Use intermediate variables
   
2. **Method Calls with Arguments** - Only zero-argument methods work in interpolation
   - Example: `{str.len()}` works, but `{str.substring(0, 5)}` doesn't
   - Workaround: Call method separately and store result in variable

3. **Format Specifiers** - Not yet implemented (`:?`, `:x`, `:.2`, etc.)

4. **Positional/Named Arguments** - Not yet implemented

See `docs/PRINTLN_MACRO_GUIDE.md` for full details on current capabilities and limitations.

---

### Performance Notes

Both fixes maintain zero runtime overhead:
- All macro expansion happens at compile time
- No runtime format string parsing
- GC dereferencing adds minimal overhead (one read-lock + hash lookup)
- Type checking still happens at compile time

---

### Acknowledgments

These fixes were implemented as part of ongoing improvements to the Veld language standard library and macro system. Special thanks to the conversation thread "Option unwrap and HashMap aliasing" which identified these issues during comprehensive testing.

---

### Related Issues

- Option/unwrap bug (previously fixed)
- HashMap implementation and re-export fixes (previously fixed)
- Module import/aliasing improvements (previously fixed)
- This release: println~ macro interpolation fixes

---

### Compatibility

- **Rust Version:** No change in MSRV
- **API Compatibility:** 100% backward compatible
- **Stdlib Version:** Compatible with all previous v0.1.x versions

---

### Contributors

- Core interpreter team
- Macro system development
- Standard library maintainers

---

## Version History

### v0.1.4 (Current)
- ✅ Fixed array printing (GcRef display issue)
- ✅ Fixed logical operator precedence
- Updated documentation

### v0.1.3
- Initial println~ macro with interpolation
- Known issues with arrays and operator precedence

### v0.1.2
- HashMap native implementation
- Re-export/aliasing fixes
- Module system improvements

### v0.1.1
- Option.unwrap fix
- Method lookup improvements

### v0.1.0
- Initial release