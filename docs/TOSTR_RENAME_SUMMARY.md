# ToString → ToStr Rename Summary

## Overview

This document tracks the renaming of the `ToString` kind and `to_string` method to `ToStr` and `to_str` respectively.

## Rationale

1. **Type Accuracy**: The current `str` type in Veld is immutable. `ToStr` better reflects conversion to this type.
2. **Future-Proofing**: Reserves `ToString` for a potential mutable `String` type.
3. **Consistency**: Method name (`to_str`) matches the type name (`str`).
4. **Common Pattern**: `to_X()` conventionally means "convert to type X".

## Changes Made

### ✅ Standard Library

- [x] `stdlib/mod.veld` - Renamed kind from `ToString` to `ToStr`
- [x] `stdlib/option.veld` - Updated `Option<T>` impl and documentation

### ✅ Rust Implementation

- [x] `crates/interpreter/src/interpreter.rs`
  - Renamed all native method registrations from `to_string` to `to_str`
  - Updated method names for: str, i8-i64, u8-u64, f32, f64, bool, char
  - Renamed helper functions: `register_integer_to_str`, `register_float_to_str`
  - Updated type checker method registrations

- [x] `crates/common/src/types/checker.rs`
  - Renamed `type_has_to_string_method()` → `type_has_to_str_method()`
  - Updated all method lookups to check for `"to_str"` instead of `"to_string"`
  - Updated error messages

### ✅ Test Files (Partially Complete)

- [x] `tests/conditional_impl_parsing_test.veld`
- [x] `tests/conditional_where_clause_demo.veld`
- [x] `tests/hashmap_test.veld`
- [x] `tests/minimal_arrow_do_test.veld`
- [x] `test_generic_import/main.veld`
- [x] `test_generic_import/success_test.veld`

### ⚠️ Remaining Test Files

The following test files still reference `.to_string()` and need updating:

- `tests/test_basic_macros.veld`
- `tests/test_complex_constraint.veld`
- `tests/test_debug_issue.veld`
- `tests/test_debug_struct_method.veld`
- `tests/test_macro_integration_more.veld`
- `tests/test_option_conditional_to_string.veld` *(also rename file)*
- `tests/test_option_simple.veld`
- `examples/macro_usage.veld`
- `tests/poss/piping.veld`
- `tests/poss/print_test.veld`
- Various other test files

### 📝 Documentation Updates Needed

- [ ] Update main README if it references `ToString`
- [ ] Update any API documentation
- [ ] Update tutorial/guide documents

## Breaking Changes

**Yes** - This is a breaking change. All code using:
- `ToString` kind → must change to `ToStr`
- `.to_string()` method → must change to `.to_str()`

## Migration Guide

### For Kind Implementations

**Before:**
```veld
impl MyType <- std.ToString
    pub fn to_string(self) -> str
        return "MyType"
    end
end
```

**After:**
```veld
impl MyType <- std.ToStr
    pub fn to_str(self) -> str
        return "MyType"
    end
end
```

### For Method Calls

**Before:**
```veld
let num = 42
let str = num.to_string()
```

**After:**
```veld
let num = 42
let str = num.to_str()
```

### For Generic Constraints

**Before:**
```veld
impl<T> Container<T> <- ToString where T: ToString
    pub fn to_string(self) -> str
        return self.value.to_string()
    end
end
```

**After:**
```veld
impl<T> Container<T> <- ToStr where T: ToStr
    pub fn to_str(self) -> str
        return self.value.to_str()
    end
end
```

## Automated Migration

To update your codebase, you can use the following replacements:

### In `.veld` files:

1. Kind declarations:
   - `ToString` → `ToStr`

2. Method declarations:
   - `fn to_string(` → `fn to_str(`

3. Method calls:
   - `.to_string()` → `.to_str()`

4. In documentation comments:
   - References to `to_string` → `to_str`
   - References to `ToString` → `ToStr`

### Example sed commands (for Unix-like systems):

```bash
# Update kind references
find . -name "*.veld" -type f -exec sed -i 's/std\.ToString/std.ToStr/g' {} \;
find . -name "*.veld" -type f -exec sed -i 's/<- ToString/<- ToStr/g' {} \;

# Update method declarations
find . -name "*.veld" -type f -exec sed -i 's/fn to_string(/fn to_str(/g' {} \;

# Update method calls
find . -name "*.veld" -type f -exec sed -i 's/\.to_string()/\.to_str()/g' {} \;

# Update documentation
find . -name "*.veld" -type f -exec sed -i 's/to_string method/to_str method/g' {} \;
```

## Testing Checklist

After completing the rename:

- [ ] Run `cargo build --release`
- [ ] Run all stdlib tests
- [ ] Run compiler/interpreter tests
- [ ] Test Option<T> conditional to_str
- [ ] Test generic constraint with ToStr
- [ ] Test all numeric types (i8-i64, u8-u64, f32, f64)
- [ ] Test bool and char types
- [ ] Verify type checker correctly validates ToStr availability

## Future Considerations

### When Adding `String` Type

When implementing a mutable `String` type, you can:

1. Keep `ToStr` for immutable `str` conversion
2. Add `ToString` for owned `String` conversion
3. Consider `AsStr` for cheap reference conversion (like Rust's `AsRef<str>`)

**Example future API:**
```veld
// Convert to immutable str (may copy/format)
kind ToStr
    fn to_str(self) -> str
end

// Convert to owned String (allocates)
kind ToString
    fn to_string(self) -> String
end

// Borrow as str (zero-cost when possible)
kind AsStr
    fn as_str(self) -> &str
end
```

## Compatibility Matrix

| Veld Version | Kind Name | Method Name | Status |
|--------------|-----------|-------------|--------|
| ≤ 0.1.3 | `ToString` | `to_string()` | ❌ Old API |
| ≥ 0.1.4 | `ToStr` | `to_str()` | ✅ Current API |
| Future | Both? | Both? | 🔮 When String added |

## Notes

- The native function `to_str()` (global) remains unchanged
- The `println~` macro uses `to_str()` internally (already correct)
- Error messages in type checker updated to reference `to_str`
- All primitive types (int, float, bool, char, str) have `to_str` method

## References

- Related to: println~ macro fixes (uses to_str conversion)
- See also: `docs/PRINTLN_MACRO_GUIDE.md`
- See also: `stdlib/option.veld` for conditional trait implementation example

---

**Status**: ⚠️ **In Progress** - Core changes complete, test files need bulk update

**Next Step**: Complete remaining test file updates and run full test suite