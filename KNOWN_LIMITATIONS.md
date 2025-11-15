# Known Limitations and Workarounds

**Last Updated:** December 2024  
**Veld Version:** 0.1.4+

This document describes known limitations in the Veld language and provides workarounds where available.

---

## Type Inference Limitations

### 1. `to_str()` in Map Lambdas

**Status:** ✅ FIXED  
**Severity:** N/A  
**Affects:** Veld 0.1.4 (fixed in current version)

#### Problem (Historical)
Calling `.to_str()` on numeric types inside `map()` lambdas would fail with type inference errors:

```veld
# ✅ Now works!
let numbers = [1, 2, 3]
let strings = numbers.map(n => n.to_str())
# Previously: Error: "Cannot unify str<T11> with i32"
```

#### Fix
The type checker now properly handles `TypeVar` resolution for primitive types. When `.to_str()` is called on a type variable, the type checker:
1. Solves constraints to try to resolve the type variable
2. If it resolves to a primitive type (i32, bool, etc.), returns `String` type
3. If still unresolved, assumes it will resolve to a type with `to_str()` method

#### Usage (All patterns now work)

```veld
# ✅ Direct in map lambda
let numbers = [1, 2, 3]
let strings = numbers.map(n => n.to_str())

# ✅ Helper function
fn int_to_str(n: i32) -> str
    n.to_str()
end
let strings = numbers.map(int_to_str)

# ✅ Direct usage
let x = 42
let s = x.to_str()

# ✅ String concatenation
std.io.println("Number: " + x.to_str())
```

All patterns for using `.to_str()` now work correctly, including inside lambda expressions.

---

## Pattern Matching Limitations

### 2. Pattern Exhaustiveness Checking

**Status:** Incomplete  
**Severity:** Low  
**Affects:** All versions

#### Problem
The compiler may not always warn about non-exhaustive pattern matches:

```veld
# May not warn if a case is missing
match value
    Option.Some(x) => std.io.println(x.to_str())
    # Missing Option.None case - should warn but might not
end
```

#### Workaround
Always include a catch-all pattern or explicitly handle all cases:

```veld
# ✅ Better
match value
    Option.Some(x) => std.io.println(x.to_str())
    Option.None => std.io.println("No value")
end
```

---

## Standard Library Limitations

### 3. `unique()` Performance

**Status:** ✅ OPTIMIZED  
**Severity:** N/A  
**Affects:** N/A (optimized in current version)

#### Optimization
The `unique()` method now uses O(n) HashSet-based duplicate detection:

```veld
let data = [1, 2, 2, 3, 1, 4, 3, 5]
let unique = data.unique()  # O(n) time complexity
```

#### Implementation
- Uses Rust's `HashSet` internally for duplicate detection
- Keeps first occurrence of each element
- Works efficiently with arrays of any size
- Both `unique()` and `dedup()` use the optimized implementation

#### Performance
- **Before:** O(n²) - nested loops comparing all elements
- **After:** O(n) - HashSet-based duplicate detection
- Significant performance improvement for large arrays

---

## Array Method Limitations

### 4. `first()` and `last()` with GcRef

**Status:** Minor Issue  
**Severity:** Very Low  
**Affects:** Veld 0.1.4+

#### Problem
In rare cases, `first()` may return `None` for non-empty arrays when dealing with GC-allocated values.

#### Workaround
Use array indexing or iteration:

```veld
# If first() seems wrong
let first = array.first()
match first
    Option.None => do
        # Use iteration instead
        for item in array.take(1) do
            # Process first item
        end
    end
    Option.Some(x) => # Process x
end
```

---

## For Loop Limitations

### 5. Multiple Iterator Variables

**Status:** Not Implemented  
**Severity:** Low  
**Affects:** All versions

#### Problem
Cannot iterate over multiple arrays simultaneously:

```veld
# ❌ Not supported
for a in array1, b in array2 do
    # ...
end
```

#### Workaround
Use `zip()`:

```veld
# ✅ Works
for (a, b) in array1.zip(array2) do
    # Process a and b together
end
```

---

## Type System Limitations

### 6. No Higher-Kinded Types

**Status:** Not Implemented  
**Severity:** Low  
**Affects:** All versions

#### Problem
Cannot write functions that are generic over type constructors:

```veld
# ❌ Not supported
fn map_container<F<_>>(container: F<i32>, f: i32 -> str) -> F<str>
```

#### Workaround
Write specific implementations for each container type.

---

## Module System Limitations

### 7. Circular Dependencies

**Status:** Not Fully Tested  
**Severity:** Medium  
**Affects:** All versions

#### Problem
Circular module imports may cause issues:

```veld
# module_a.veld
import module_b

# module_b.veld
import module_a  # May cause problems
```

#### Workaround
- Refactor to remove circular dependencies
- Extract shared code to a third module
- Use forward declarations (if available)

---

## Numeric Type Limitations

### 8. Integer Overflow

**Status:** By Design  
**Severity:** Low  
**Affects:** All versions

#### Problem
Integer overflow wraps around (no automatic promotion):

```veld
let max_i32 = 2147483647
let overflow = max_i32 + 1  # Wraps to -2147483648
```

#### Workaround
Use larger integer types when needed:

```veld
let big: i64 = 2147483647
let no_overflow = big + 1  # Works correctly
```

---

## String Limitations

### 9. Unicode Handling

**Status:** Partial Support  
**Severity:** Medium  
**Affects:** All versions

#### Problem
String indexing and length may not work correctly with multi-byte Unicode characters.

#### Workaround
Use string methods that are Unicode-aware:
- `split()` - generally safe
- `trim()` - safe
- Avoid character-by-character manipulation

---

## Concurrency Limitations

### 10. No Concurrency Primitives

**Status:** Not Implemented  
**Severity:** High (for concurrent applications)  
**Affects:** All versions

#### Problem
No built-in support for:
- Threads
- Async/await
- Channels
- Mutexes

#### Workaround
Current Veld is single-threaded. For concurrent applications:
- Use external programs/scripts
- Wait for future concurrency support
- Consider if Veld is the right tool for your use case

---

## Error Handling Limitations

### 11. Limited Stack Traces

**Status:** Partial Implementation  
**Severity:** Medium  
**Affects:** All versions

#### Problem
Error messages may not always include full stack traces or line numbers.

#### Workaround
- Use descriptive variable names
- Add debug print statements
- Keep functions small for easier debugging

---

## Performance Limitations

### 12. Interpreter Performance

**Status:** By Design  
**Severity:** Variable  
**Affects:** All versions

#### Problem
Interpreter mode is slower than compiled languages.

#### Notes
- JIT compilation is in development
- Bytecode compilation available for some cases
- Acceptable for scripting and non-performance-critical code

---

## Reporting New Limitations

If you discover a limitation not listed here:

1. **Check if it's a bug** - Some limitations are actually bugs that can be fixed
2. **Search existing issues** - May already be known
3. **Create an issue** - Document the limitation with:
   - Minimal reproduction case
   - Expected vs actual behavior
   - Workaround (if you found one)

---

## Future Improvements

Planned improvements to address these limitations:

### Short Term
- ✅ ~~Fix `to_str()` in map lambdas~~ (COMPLETED)
- ✅ ~~Optimize `unique()` with HashSet~~ (COMPLETED)
- Better error messages

### Medium Term
- Exhaustiveness checking
- Improved Unicode support
- Better GC handling

### Long Term
- Concurrency primitives
- JIT compilation
- Higher-kinded types (maybe)

---

## Limitations vs Bugs

**How to distinguish:**

- **Limitation:** By design, documented, has workaround, low priority
- **Bug:** Unintended behavior, should work, high priority to fix

**Examples:**

- "No threads" → Limitation (not implemented yet)
- "to_str() fails in map" → Bug (should work, but has edge case)
- "unique() is O(n²)" → Limitation (intentional, can be improved)
- "first() returns None for non-empty array" → Bug (should fix)

---

## Workaround Patterns

### General Strategies

1. **Extract to helper function** - Often solves type inference issues
2. **Use intermediate variables** - Helps type checker resolve constraints
3. **Add explicit type annotations** - When inference fails
4. **Restructure code** - Sometimes a different approach avoids the limitation
5. **Check this document** - Workaround may already be documented

---

## Version History

### v0.1.4+
- Documented to_str() in map issue
- Added all current known limitations
- Included workarounds for each

### Future Versions
- This document will be updated as limitations are addressed
- Check git history for changes

---

**Remember:** Most limitations have workarounds. If you're stuck, check this document or ask for help!