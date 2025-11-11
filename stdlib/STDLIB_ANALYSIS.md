# Veld Standard Library - Comprehensive Analysis & Roadmap

**Date:** November 2024  
**Status:** Post-Register VM Migration, Phase 6 Pattern Matching Complete

---

## Executive Summary

The Veld standard library is **partially implemented** with a solid foundation of core algebraic data types (Option, Result) and basic utilities. The current implementation has ~25-30% of a typical systems language stdlib complete, with significant gaps in collections, I/O, and string manipulation.

**Key Strengths:**
- ✅ Core ADTs (Option, Result) fully functional
- ✅ Pattern matching works excellently with stdlib types
- ✅ Math module has essential functions
- ✅ Basic I/O (print, println, file operations) works
- ✅ Type system supports generic types well

**Critical Gaps:**
- ❌ No working HashMap (mutation doesn't persist)
- ❌ Vec implementation incomplete/untested
- ❌ String methods defined but not implemented
- ❌ Iterator trait defined but not implemented anywhere
- ❌ No error handling types beyond Result
- ❌ Module-level constants not accessible

---

## Detailed Module Analysis

### 1. Core Types (`stdlib/core/mod.veld`)

**Status:** 🟡 Partially Defined

**What Exists:**
- `Value` kind (type introspection)
- `Sized` kind (collection sizing)
- `Iterable<T>` kind (iteration interface)
- `Iterator<T>` kind (iterator protocol)

**What Works:**
- Kind definitions parse and type-check correctly
- Panic function registered and works (`panic~()` macro)

**What's Missing:**
- No implementations of Iterator anywhere
- No actual introspection (`type_name()` not implemented)
- Sized is not implemented for any types
- No Debug/Display equivalents

**Recommendations:**
```veld
# Add these to core:
pub kind Debug
    fn debug(self) -> str
end

pub kind Clone<T>
    fn clone(self) -> T
end

pub kind Default<T>
    fn default() -> T
end
```

---

### 2. Option Type (`stdlib/option.veld`)

**Status:** 🟢 Fully Functional

**What Works:**
- ✅ `Option.Some(T)` and `Option.None`
- ✅ `is_some()`, `is_none()`
- ✅ `unwrap()`, `unwrap_or(default)`
- ✅ Pattern matching
- ✅ ToStr implementation
- ✅ Nested Option types work perfectly

**What's Commented Out:**
- ⚠️ `map<U>(self, f: (T) -> U) -> Option<U>` - Generic method-level type parameters not supported

**Missing:**
- `and_then()` / `flat_map()` (monadic bind)
- `or_else()`
- `filter()`
- `zip()`
- `transpose()` for `Option<Result<T, E>>`
- `flatten()` for `Option<Option<T>>`

**Test Coverage:** ✅ 100% of implemented features tested

**Priority Additions:**
```veld
impl<T> Option<T>
    # Most important missing method
    pub fn is_some_and(self, f: (T) -> bool) -> bool
        match self
            Option.Some(value) => f(value),
            Option.None => false,
        end
    end
    
    # For Result interop
    pub fn ok_or<E>(self, err: E) -> Result<T, E>
        match self
            Option.Some(value) => Result.Ok(value),
            Option.None => Result.Err(err),
        end
    end
end
```

---

### 3. Result Type (`stdlib/result.veld`)

**Status:** 🟢 Mostly Functional

**What Works:**
- ✅ `Result.Ok(T)` and `Result.Err(E)`
- ✅ `is_ok()`, `is_err()`
- ✅ `unwrap()`, `unwrap_or(default)`
- ✅ Pattern matching works perfectly
- ✅ Nested Result types work

**What's Commented Out:**
- ⚠️ `map<U>(self, f: (T) -> U) -> Result<U, E>` - Same generic limitation

**Missing:**
- `map_err<F>(self, f: (E) -> F) -> Result<T, F>`
- `and_then()` / `flat_map()` (monadic bind)
- `or_else()`
- `unwrap_expect(message: str)`
- `transpose()` for `Result<Option<T>, E>`
- Error context/tracing utilities

**Test Coverage:** ✅ 100% of implemented features tested

**Critical Addition Needed:**
```veld
# IO and FS functions should return this instead of panicking
pub enum IoError
    NotFound(str),
    PermissionDenied(str),
    AlreadyExists(str),
    Other(str),
end

pub type IoResult<T> = Result<T, IoError>
```

---

### 4. Math Module (`stdlib/math/mod.veld`)

**Status:** 🟢 Core Functions Work

**What Works:**
- ✅ `max(a, b)`, `min(a, b)`
- ✅ `clamp(x, min, max)`
- ✅ Native functions: `sqrt`, `pow`, `sin`, `cos`, `tan`
- ✅ Generic type constraints work (`T: Ord<T>`)

**What's Broken:**
- ❌ Constants (`pi`, `e`, `tau`) - Module-level variables not accessible from imports
  - Error: "Variable 'pi' exported from module 'std.math' is not directly accessible yet"

**Missing:**
- Logarithm functions (`log`, `log10`, `ln`)
- Hyperbolic functions (`sinh`, `cosh`, `tanh`)
- Rounding functions (`floor`, `ceil`, `round`, `trunc`)
- Absolute value for floats (only works for types with `Neg`)
- Conversion functions (`radians`, `degrees`)
- More constants (`sqrt(2)`, `ln(2)`, etc.)

**Test Coverage:** ✅ All working functions tested

**Critical Fix Needed:**
```rust
// In interpreter.rs, need to implement module variable resolution
// Currently only functions are accessible from imported modules
```

---

### 5. Collections Module (`stdlib/collections/`)

**Status:** 🔴 Major Issues

#### 5.1 HashMap (`collections/hash_map.veld`)

**Status:** 🔴 Non-Functional (Mutation Broken)

**What Exists:**
- Struct definition with native backing
- All methods declared (get, set, has, remove, clear, keys, values, entries)
- Native Rust implementation in interpreter

**What's Broken:**
- ❌ **Mutation does not persist!**
  - `set(key, value)` appears to work but changes are lost
  - `remove(key)` same issue
  - Root cause: Methods receive a clone of the object, mutations don't propagate back
  - See `stdlib/collections/HASHMAP_STATUS.md` for full details

**Workaround:**
- Could make methods return `Self` and require reassignment (functional style)
- Example: `let map = map.set("key", "value")`

**Test Coverage:** ⚠️ Tests exist but fail due to mutation issue

**Recommendation:** **HIGH PRIORITY** - This needs architectural fix in interpreter

#### 5.2 Vec (`stdlib/vec.veld`)

**Status:** 🟡 Implemented but Likely Has Same Mutation Issue

**What Exists:**
- Full implementation of `Sequence<T>` and `GrowableSequence<T>` traits
- Methods: `new()`, `with_capacity()`, `push()`, `pop()`, `insert()`, `remove()`, `get()`, `set()`, `len()`, `is_empty()`

**What's Untested:**
- ⚠️ Likely has the same mutation persistence issue as HashMap
- No comprehensive test suite exists

**Test Coverage:** 🔴 Minimal (only basic creation tested)

**Recommendation:** Test thoroughly, likely needs same fix as HashMap

#### 5.3 Sequence Traits (`collections/sequence.veld`)

**Status:** 🟢 Well Defined

- ✅ `Sequence<T>` - immutable sequence operations
- ✅ `GrowableSequence<T>` - mutable operations (inherits Sequence)
- Clean design, but no native array implementation

**Missing:**
- Native arrays don't implement Sequence<T> (should they?)
- No LinkedList, Deque, or other sequence types

#### 5.4 Map Trait (`collections/map.veld`)

**Status:** 🟢 Well Defined

Clean generic Map interface, but only HashMap exists (and is broken).

**Missing Collections:**
- `BTreeMap` (ordered map)
- `HashSet`, `BTreeSet`
- `LinkedList`
- `VecDeque`
- `BinaryHeap`

---

### 6. String Module (`stdlib/string/mod.veld`)

**Status:** 🔴 Defined But Not Implemented

**What Exists:**
- Excellent kind definitions for string operations:
  - `Transformable` (to_upper, to_lower, trim, etc.)
  - `Searchable` (contains, starts_with, ends_with, index_of)
  - `Manipulatable` (substring, replace, split, pad, repeat)
  - `Parsable` (to_int, to_float, to_bool)

**What's Implemented:**
- ❌ **NONE** - All methods are defined but not registered in native registry

**Native Methods That Do Exist:**
```rust
// In interpreter.rs native_method_registry:
- register_string_method("to_upper", ...)
- register_string_method("to_lower", ...)
// But these are NOT connected to the kind definitions
```

**Test Coverage:** 🔴 None

**Recommendation:** **HIGH PRIORITY** - Bridge the gap between kind definitions and native methods

**Implementation Plan:**
```rust
// In interpreter.rs initialize_string_capabilities():
impl str <- Transformable
    self.native_method_registry.register("str", "to_upper", |_, args| { ... });
    self.native_method_registry.register("str", "to_lower", |_, args| { ... });
    // ... etc
end
```

---

### 7. I/O Module (`stdlib/io/mod.veld`)

**Status:** 🟢 Basic Functionality Works

**What Works:**
- ✅ `print(text: str)`
- ✅ `println(text: str)`
- ✅ `read_file(path: str) -> str`
- ✅ `write_file(path: str, content: str) -> ()`
- ✅ `file_exists(path: str) -> bool`
- ✅ `read_line() -> str`

**What's Wrong:**
- ⚠️ All functions return `str` or `()` instead of `Result<T, E>`
- ⚠️ File operations panic on error instead of returning Result
- ⚠️ No buffered I/O
- ⚠️ No binary file support
- ⚠️ No stdin/stdout/stderr handles

**Missing:**
- Proper error handling (should return `Result<T, IoError>`)
- `BufReader`, `BufWriter`
- `read_to_string()`, `read_bytes()`
- `write_bytes()`
- Format string support (exists as `_format_string` but not exposed)

**Test Coverage:** ✅ Manually tested

**Critical Fix:**
```veld
# Change signatures from:
pub fn read_file(path: str) -> str

# To:
pub fn read_file(path: str) -> Result<str, IoError>
```

---

### 8. Filesystem Module (`stdlib/fs/mod.veld`)

**Status:** 🔴 Nearly Empty

**What Exists:**
- `test() -> i32` (placeholder function returning 42)
- Native `std.fs.write` and `std.fs.read` that return Result enums

**What's Missing:**
- Directory operations (create, remove, list, walk)
- Path manipulation (join, parent, extension, stem)
- File metadata (size, modified time, permissions)
- Symlink operations
- File type detection

**Test Coverage:** 🔴 None

**Recommendation:** Build out using Rust's `std::fs` as model

---

### 9. Range Module (`stdlib/range.veld`)

**Status:** 🟡 Defined But Untested

**What Exists:**
- `Range<T>` - exclusive range (start..stop)
- `RangeInclusive<T>` - inclusive range (start..=stop)
- `RangeFrom<T>` - unbounded start (start..)
- `RangeTo<T>` - unbounded end (..stop)
- `RangeToInclusive<T>` - (..=stop)
- `RangeFull` - (..)
- Iterator support for i32 and f64 ranges

**What's Unknown:**
- ⚠️ Unclear if syntax `0..10` actually creates Range types
- ⚠️ Iterator implementation may not be connected to for-loops

**Test Coverage:** 🔴 Minimal

**Needs:**
- Integration tests with for-loops
- Verify syntax parsing works
- Test step_by for floating point

---

### 10. Operators Module (`stdlib/ops/mod.veld`)

**Status:** 🟢 Well Defined

**What Exists:**
- Complete set of operator kinds:
  - Arithmetic: Add, Sub, Mul, Div, Rem, Neg
  - Bitwise: BitAnd, BitOr, BitXor
  - Comparison: Eq, NotEq, Ord, Lt, Gt, Le, Ge
  - Indexing: Index, IndexMut, SetIndex
  - Ordering enum

**What's Unclear:**
- ⚠️ Are these actually used by the compiler for operator overloading?
- ⚠️ Or are they just documentation?

**Test Coverage:** 🔴 None

**Needs:**
- Verify operators actually call these methods
- Test custom types implementing operators

---

### 11. Numeric Module (`stdlib/numeric/`)

**Status:** 🔴 Not Found

Directory exists but appears empty or not exported.

**Needed:**
- Numeric type hierarchy
- Conversion traits (Into, From, TryInto, TryFrom)
- Checked/wrapping/saturating arithmetic
- Min/max values for numeric types

---

### 12. Time Module (`stdlib/time/`)

**Status:** 🔴 Not Found

**Needed:**
- Duration type
- Instant (monotonic time)
- SystemTime (wall clock)
- Time measurement utilities
- Sleep function

---

### 13. Either Type (`stdlib/either.veld`)

**Status:** 🟡 Prototype (Not Exported)

**What Exists:**
- Full implementation of Either<Left, Right>
- Methods: is_left, is_right, map_left, map_right, left_or, right_or

**Why Not Exported:**
- Marked as "prototype" and "may not be included"
- Not in `stdlib/mod.veld` exports

**Recommendation:** Either export it or delete it

---

## Native Function/Method Registry Analysis

### What's Registered (in `interpreter.rs`)

**Math Functions:**
- `std.math.sqrt`, `std.math.pow`
- `std.math.sin`, `std.math.cos`, `std.math.tan`

**I/O Functions:**
- `std.io.print`, `std.io.println`
- `std.io.read_file`, `std.io.write_file`, `std.io.file_exists`
- `std.io.read_line`
- `std.io._format_string` (internal)

**Filesystem Functions:**
- `std.fs.write`, `std.fs.read` (return Result enums)

**Utility Functions:**
- `std.panic` (for panic~ macro)
- `to_str` (generic value to string)

**String Methods (Registered but disconnected from kinds):**
- Various string transformations exist but aren't linked to Veld code

**HashMap Methods (Declared as `@Compiler.Builtin`):**
- HashMap methods are declared but mutation doesn't work

### What's Missing from Native Registry

- String kind methods not connected
- No numeric conversion functions
- No time/duration functions
- No process/environment functions
- No random number generation
- No threading primitives

---

## Architectural Issues

### 1. 🔴 **CRITICAL: Mutation Doesn't Persist**

**Problem:**
```veld
let map = HashMap.new()
map.set("key", "value")  # Changes lost!
let val = map.get("key")  # Returns None
```

**Root Cause:**
```rust
// In interpreter.rs:6586
let mut method_args = vec![object.clone()];  // ← Clone means mutations lost
```

**Impact:**
- HashMap unusable
- Vec likely unusable
- Any mutable collection broken

**Solutions:**

**Option A: Functional Style (Quick Fix)**
```veld
# Make mutating methods return Self
pub fn set(self, key: K, value: V) -> Self

# Usage:
let map = map.set("key", "value")  # Reassign
```
✅ Works with current architecture  
❌ Less intuitive for users expecting mutation

**Option B: Interior Mutability (Proper Fix)**
```rust
// Use RefCell for internal storage
pub struct HashMap<K, V> {
    data: RefCell<rust_std::HashMap<K, V>>
}
```
✅ True mutation semantics  
❌ Requires architectural changes  
❌ Runtime borrow checking overhead

**Option C: Variable Tracking (Proper Fix)**
```rust
// Track the variable name in method calls
// Update the variable after native method returns ()
```
✅ Maintains mutable semantics  
❌ Complex to implement  
❌ Needs to handle all scopes

**Recommendation:** Start with **Option A** for immediate usability, plan **Option C** for proper fix.

---

### 2. 🟡 Generic Method-Level Type Parameters Not Supported

**Problem:**
```veld
impl<T> Option<T>
    pub fn map<U>(self, f: (T) -> U) -> Option<U>  # ← U not recognized
        # ...
    end
end
```

**Impact:**
- Can't implement `map`, `and_then`, `flat_map` on Option/Result
- Limits functional programming patterns
- Workarounds are verbose

**Current Workaround:**
- Comment out methods using method-level generics
- Users implement manually when needed

**Solution:**
Type checker needs to:
1. Push a new type parameter scope when entering a method with generics
2. Register method-level type parameters
3. Pop scope when exiting method

---

### 3. 🟡 Module-Level Constants Not Accessible

**Problem:**
```veld
# In math/mod.veld:
pub let pi: f64 = 3.1415926535

# In user code:
import std.math
let x = math.pi  # Error: "not directly accessible yet"
```

**Impact:**
- Math constants unusable
- Can't export configuration values
- Forces everything to be functions

**Solution:**
```rust
// In interpreter.rs module resolution:
// Handle ExportedItem::Variable in addition to ExportedItem::Function
```

---

### 4. 🟡 Array Indexing Issues

**Problem:**
```veld
let arr = [1, 2, 3]
let x = arr[0]  # Sometimes fails with "Cannot index into non-array value"
```

**Impact:**
- Array indexing unreliable in some contexts
- Workarounds needed

**Needs:** More investigation into when/why this fails

---

## Test Coverage Summary

| Module | Coverage | Status |
|--------|----------|--------|
| Option | 100% | ✅ Comprehensive |
| Result | 100% | ✅ Comprehensive |
| Math | 90% | ✅ Good (constants untested) |
| Arrays | 50% | 🟡 Basic tests only |
| Strings | 0% | 🔴 No tests |
| HashMap | 10% | 🔴 Tests fail (mutation) |
| Vec | 5% | 🔴 Minimal |
| Range | 5% | 🔴 Minimal |
| IO | 30% | 🟡 Manual only |
| FS | 0% | 🔴 No tests |

**Overall Test Coverage:** ~25%

---

## Recommended Priorities

### 🔴 Critical (Do First)

1. **Fix Mutation Persistence Issue**
   - Implement functional-style workaround (methods return Self)
   - Enables HashMap and Vec to work
   - Unblocks user code

2. **Implement String Methods**
   - Connect existing native implementations to kind definitions
   - High user value, relatively easy
   - Many methods already written in Rust

3. **Fix Module Constant Access**
   - Enable `math.pi`, `math.e` access
   - Small change, high impact

4. **Add Proper Error Handling to I/O**
   - Change I/O functions to return `Result<T, IoError>`
   - Critical for production code

### 🟡 High Priority (Do Soon)

5. **Complete String Module**
   - Add missing methods (split, replace, parse, etc.)
   - Add comprehensive tests

6. **Test and Fix Vec**
   - Add comprehensive test suite
   - Fix mutation if broken
   - Document usage patterns

7. **Build Out FS Module**
   - Directory operations
   - Path manipulation
   - File metadata

8. **Implement Iterator Protocol**
   - Make ranges work with for-loops
   - Add iterator adapters (map, filter, take, skip)
   - Implement for arrays, Vec

### 🟢 Medium Priority (Nice to Have)

9. **Add More Collection Types**
   - HashSet
   - BTreeMap, BTreeSet
   - LinkedList, VecDeque

10. **Expand Math Module**
    - Logarithms, exponentials
    - Rounding functions
    - More constants

11. **Support Generic Method Parameters**
    - Enable `map`, `and_then`, etc.
    - Requires type checker changes

12. **Add Time Module**
    - Duration, Instant, SystemTime
    - Sleep, timing utilities

### 🔵 Low Priority (Future)

13. **Numeric Module**
    - Type conversions
    - Checked arithmetic

14. **Concurrency Primitives**
    - Threads, async/await (if planned)
    - Mutexes, channels

15. **Networking**
    - TCP/UDP sockets
    - HTTP client

---

## Stdlib Maturity Roadmap

### Phase 1: Core Usability (Current → 1 month)
**Goal:** Make stdlib usable for basic programs

- ✅ Fix mutation issue (workaround)
- ✅ String methods working
- ✅ I/O returns Results
- ✅ Module constants accessible
- ✅ Vec tested and working
- ✅ HashMap tested (with workaround)

**Outcome:** Can write basic file processing, string manipulation, data structure programs

### Phase 2: Completeness (1-2 months)
**Goal:** Feature parity with typical scripting language stdlib

- Iterator protocol working
- Range for-loops
- Full filesystem operations
- More collection types (HashSet, etc.)
- Comprehensive string operations
- Time/duration support

**Outcome:** Can write complex applications

### Phase 3: Production Ready (2-4 months)
**Goal:** Suitable for production systems

- Fix mutation properly (no workarounds)
- Generic method parameters
- Comprehensive error handling
- Full test coverage (>80%)
- Performance benchmarks
- Concurrency primitives
- Documentation complete

**Outcome:** Production-ready stdlib

---

## Key Design Questions

### 1. Mutation Semantics
**Question:** Should Veld follow Rust's ownership model, or allow more liberal mutation?

**Options:**
- **Rust-like:** Explicit mut, borrowing rules → Complex but safe
- **Swift-like:** Value types copy, reference types mutate → Simpler, some overhead
- **Functional:** Immutable by default, functional updates → Clean but different

**Current State:** Unclear - mutation exists but doesn't work reliably

### 2. Error Handling Philosophy
**Question:** When to panic vs return Result?

**Recommendation:**
- **Panic:** Programmer errors, assertion failures, unreachable code
- **Result:** Expected failures (file not found, network timeout, parse error)

**Needs:** Document and enforce consistently

### 3. Iterator Model
**Question:** Should iterators be:
- **External:** User calls `next()` explicitly (like Rust)
- **Internal:** Iterator calls user function (like Ruby `each`)
- **Both:** Support both styles

**Current State:** External iterator trait defined but not used

### 4. Collection Ownership
**Question:** Do collections own their data or reference it?

**Current State:** Collections appear to own data (like Rust)

---

## Comparison to Other Languages

### Rust Stdlib Equivalents

| Rust | Veld | Status |
|------|------|--------|
| Option<T> | Option<T> | ✅ Complete |
| Result<T,E> | Result<T,E> | ✅ Complete |
| Vec<T> | Vec<T> | 🟡 Broken |
| HashMap<K,V> | HashMap<K,V> | 🔴 Broken |
| String | str | 🟡 Partial |
| Iterator | Iterator | 🔴 Not impl |
| Range | Range | 🟡 Unclear |
| std::fs | std.fs | 🔴 Minimal |
| std::io | std.io | 🟡 Basic |

### Python Stdlib Equivalents

| Python | Veld | Status |
|--------|------|--------|
| list | Vec / array | 🟡 Partial |
| dict | HashMap | 🔴 Broken |
| str methods | string kinds | 🟡 Defined not impl |
| open/read/write | io module | 🟡 Basic |
| os.path | (missing) | 🔴 None |
| math | math | ✅ Core funcs |
| datetime | (missing) | 🔴 None |

---

## Conclusion

The Veld standard library has a **strong foundation** with excellent design of core types (Option, Result) and pattern matching integration. However, it's currently at ~25-30% completeness and has **critical blockers** preventing practical use:

1. **Collections don't work** (mutation issue)
2. **String methods not implemented** (despite being designed)
3. **I/O error handling is broken** (panics instead of Results)

**To reach usability for basic programs:** Focus on the 4 critical priorities listed above. These are relatively small changes that unlock massive value.

**To reach production readiness:** Follow the 3-phase roadmap, with particular attention to fixing mutation semantics properly and achieving high test coverage.

The good news: The architecture is sound, the designs are clean, and most of the hard type system work is done. The remaining work is primarily:
- Connecting existing pieces
- Filling in missing implementations
- Testing thoroughly
- Fixing the mutation architecture

**Estimated effort to basic usability:** 1-2 weeks focused work  
**Estimated effort to production ready:** 2-4 months

---

## Appendix: Quick Reference - What Actually Works

```veld
# ✅ WORKS - Use freely
import std.option.{Option}
import std.result.{Result}
import std.io.{print, println}
import std.math.{max, min, clamp}

let opt = Option.Some(42)
let none = Option.None
opt.is_some()
opt.unwrap_or(0)

let res = Result.Ok(100)
res.is_ok()

println("Hello")
print("World")

let bigger = math.max(10, 20)

# Pattern matching with Option/Result
match opt
    Option.Some(x) => x * 2,
    Option.None => 0,
end

# ❌ BROKEN - Don't use
let map = HashMap.new()  # mutation doesn't persist
let vec = Vec.new()      # probably broken too

# 🟡 PARTIALLY WORKS - Use with caution
let arr = [1, 2, 3]
let len = arr.len()      # works
let x = arr[0]           # might fail in some contexts
let arr2 = arr.with(4)   # works

let s = "hello" + " world"  # works
# s.to_upper()             # defined but not implemented

# math.pi                  # defined but not accessible
```
