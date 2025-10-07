# Veld Standard Library Index

This document provides an overview of all available modules in the Veld standard library.

## Core Modules

### std.io
**Status:** ✅ Fully Implemented  
**Documentation:** [std-io.md](std-io.md)

Input/output operations including console output, file operations, and standard input.

**Key Functions:**
- `print(text: str)` - Output text without newline
- `println(text: str)` - Output text with newline  
- `read_file(path: str) -> str` - Read file contents
- `write_file(path: str, content: str) -> bool` - Write to file
- `file_exists(path: str) -> bool` - Check file existence
- `read_line() -> str` - Read from stdin

### std.math
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Mathematical functions and constants.

**Key Functions:**
- `sqrt(x: f64) -> f64` - Square root
- `pow(base: f64, exp: f64) -> f64` - Exponentiation
- `sin(x: f64) -> f64`, `cos(x: f64) -> f64`, `tan(x: f64) -> f64` - Trigonometric functions
- `abs(x: f64) -> f64` - Absolute value
- `min(a: f64, b: f64) -> f64`, `max(a: f64, b: f64) -> f64` - Min/max
- `clamp(value: f64, min: f64, max: f64) -> f64` - Clamp value

**Constants:**
- `pi` - π (3.14159...)
- `e` - Euler's number (2.71828...)
- `tau` - τ (2π)

### std.ops
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Operator traits and functionality for custom types.

**Key Traits:**
- `Add`, `Sub`, `Mul`, `Div` - Arithmetic operators
- `Eq`, `NotEq` - Equality comparison
- `Lt`, `Le`, `Gt`, `Ge` - Ordering comparison
- `Index`, `IndexMut` - Array/collection indexing
- `BitAnd`, `BitOr`, `BitXor` - Bitwise operations

## Type System Modules

### std.option
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Optional values that can be either `Some(T)` or `None`.

**Type:** `Option<T>`
- `Option.Some(value)` - Contains a value
- `Option.None` - No value

**Key Methods:**
- `is_some() -> bool` - Check if contains value
- `is_none() -> bool` - Check if empty
- `unwrap_or(default: T) -> T` - Get value or default
- `map<U>(f: (T) -> U) -> Option<U>` - Transform contained value

**Helper Functions:**
- `some<T>(value: T) -> Option<T>` - Create Some variant
- `none<T>() -> Option<T>` - Create None variant

### std.result
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Result type for operations that can succeed or fail.

**Type:** `Result<T, E>`
- `Result.Ok(value)` - Success case
- `Result.Err(error)` - Error case

### std.either
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Either type for values that can be one of two types.

**Type:** `Either<L, R>`
- `Either.Left(value)` - Left variant
- `Either.Right(value)` - Right variant

## Collection Modules

### std.vec
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Dynamic arrays with grow/shrink capabilities.

**Type:** `Vec<T>`

### std.collections
**Status:** 🚧 Partial Implementation  
**Documentation:** Coming Soon

Additional collection types.

**Available Types:**
- `Sequence<T>` - Basic sequence interface
- `GrowableSequence<T>` - Growable sequence interface

### std.range
**Status:** ✅ Implemented  
**Documentation:** Coming Soon

Range types for iteration and slicing.

**Type:** `Range`

## Utility Modules

### std.string
**Status:** 🚧 Partial Implementation  
**Documentation:** Coming Soon

String manipulation functions and utilities.

### std.time
**Status:** 🚧 Partial Implementation  
**Documentation:** Coming Soon

Time and date operations.

### std.numeric
**Status:** 🚧 Partial Implementation  
**Documentation:** Coming Soon

Numeric type utilities and conversions.

### std.core
**Status:** 🚧 Partial Implementation  
**Documentation:** Coming Soon

Core language utilities and fundamental types.

### std.builtin
**Status:** 🚧 Partial Implementation  
**Documentation:** Coming Soon

Built-in language constructs and primitive operations.

## Usage Examples

### Importing Modules

```veld
# Import entire module
import std.io

# Import specific functions
import std.io.{print, println}

# Import with alias
import std.math.{sqrt as square_root}

# Multiple imports
import std.io.{print, println}
import std.option.{Option, some, none}
```

### Using Standard Library

```veld
import std.io.{println}
import std.math.{sqrt, pi}
import std.option.{Option, some}

# Math operations
let radius = 5.0
let area = pi * radius * radius
println("Area: " + area.to_str())

# Optional values
let maybe_value = some(42)
if maybe_value.is_some()
    println("Value: " + maybe_value.unwrap_or(0).to_str())
end

# File operations
if std.io.file_exists("data.txt")
    let content = std.io.read_file("data.txt")
    println("File content: " + content)
end
```

## Status Legend

- ✅ **Fully Implemented** - Complete implementation with all planned features
- 🚧 **Partial Implementation** - Basic functionality available, more features planned
- ⏳ **Planned** - Not yet implemented, planned for future releases
- ❌ **Deprecated** - No longer supported

## Implementation Notes

### Native vs Veld Implementation

- **Native Functions**: Implemented in Rust for performance (e.g., `std.io`, `std.math`)
- **Veld Functions**: Implemented in Veld itself (e.g., most `Option` methods)

### Import System

The Veld import system supports:
- Direct module calls: `std.io.print("Hello")`
- Imported function calls: `print("Hello")` (after `import std.io.{print}`)
- Automatic redirection to native implementations when appropriate

### Generic Functions

Note: There is currently a limitation with importing generic functions. As a workaround, use direct module access:

```veld
# ❌ This may not work correctly:
import std.option.{some}
let opt = some("hello")

# ✅ This works correctly:
let opt = std.option.some("hello")
```

## Contributing

To add new standard library modules:

1. Create the module file in the appropriate `stdlib/` subdirectory
2. Implement native functions in `crates/interpreter/src/interpreter.rs`
3. Add exports to the module's `mod.veld` file
4. Update this index with the new module
5. Create detailed documentation following the `std-io.md` example