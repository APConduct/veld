# println~ Macro Findings and Test Results

## Current Status

The `println~` macro in Veld is currently a **simple wrapper** around `std.io.println()` and does **NOT** support string interpolation like Rust's `println!` macro.

## Test Results

### ✅ What Works

1. **Basic string printing**
   ```veld
   println~("Hello, World!")
   ```
   Output: `Hello, World!`

2. **Empty strings**
   ```veld
   println~("")
   ```
   Output: (empty line)

3. **Passing expressions directly**
   ```veld
   let x = 42
   println~(x)  # Would print the value representation
   ```

### ❌ What Does NOT Work (String Interpolation)

The following patterns are **not supported** - they print the literal string with braces:

1. **Variable interpolation**
   ```veld
   let name = "Alice"
   println~("Hello, {name}!")
   ```
   **Expected**: `Hello, Alice!`
   **Actual**: `Hello, {name}!`

2. **Expression interpolation**
   ```veld
   let x = 10
   let y = 20
   println~("Sum: {x + y}")
   ```
   **Expected**: `Sum: 30`
   **Actual**: `Sum: {x + y}`

3. **Multiple interpolations**
   ```veld
   let user = "Bob"
   let score = 95
   println~("Player {user} scored {score} points")
   ```
   **Expected**: `Player Bob scored 95 points`
   **Actual**: `Player {user} scored {score} points`

4. **Complex expressions**
   ```veld
   let a = 5
   println~("Double: {a * 2}")
   ```
   **Expected**: `Double: 10`
   **Actual**: `Double: {a * 2}`

## Comparison with Rust

Rust's `println!` macro provides:
- ✅ Format string interpolation with `{}`
- ✅ Named placeholders: `{variable}`
- ✅ Positional arguments: `println!("{} {}", x, y)`
- ✅ Format specifiers: `{:?}`, `{:#?}`, `{:x}`, etc.
- ✅ Named arguments: `println!("{name}", name="Alice")`
- ✅ Expression evaluation: `{x + y}`

Veld's `println~` macro currently provides:
- ✅ Simple string printing
- ❌ Format string interpolation
- ❌ Variable substitution
- ❌ Expression evaluation in format strings
- ❌ Format specifiers

## Current Implementation

The `println~` macro (in `crates/expander/src/integration.rs`) has this behavior:

```rust
fn expand_println_macro(args: &[Expr], _call_site: NodeId) -> Result<Vec<Statement>, ExpansionError> {
    if args.is_empty() {
        // println!() -> std.io.println("")
        Ok(vec![...])
    } else if args.len() == 1 {
        // println!(expr) -> std.io.println(expr)
        // Just passes the argument directly without processing
        Ok(vec![...])
    } else {
        // println!("format", args...) -> println(format("format", args...))
        // Calls format() function (which may not be implemented)
        Ok(vec![...])
    }
}
```

## What Needs to Be Implemented

To achieve Rust-like `println!` capabilities:

### 1. Format String Parser
Parse format strings to extract:
- Literal text segments
- Placeholder positions: `{name}`, `{expr}`, `{}`
- Format specifiers (optional): `{:?}`, `{:#x}`, etc.

Example:
```
"Player {user} scored {score} points"
  → ["Player ", <user>, " scored ", <score>, " points"]
```

### 2. Expression Evaluator in Placeholders
Parse and evaluate expressions within `{...}`:
- Simple identifiers: `{x}`
- Binary operations: `{x + y}`
- Method calls: `{obj.method()}`
- Property access: `{user.name}`
- Comparisons: `{x > y}`

### 3. Format Specifiers (Advanced)
Support Rust-like format specifiers:
- Debug: `{:?}` - debug representation
- Pretty: `{:#?}` - pretty-printed debug
- Hex: `{:x}`, `{:X}` - hexadecimal
- Binary: `{:b}` - binary
- Width: `{:10}` - minimum width
- Precision: `{:.2}` - decimal precision for floats

### 4. Positional Arguments (Advanced)
Support indexed placeholders:
```veld
println~("{0} {1} {0}", x, y)  # → "x y x"
```

### 5. Named Arguments (Advanced)
Support named parameters:
```veld
println~("Hello, {name}!", name=user)
```

## Implementation Strategy

### Phase 1: Basic Interpolation (Minimum Viable)
1. Parse format string to find `{identifier}` patterns
2. Extract variable names
3. Generate string concatenation code
4. Handle edge cases (escaped braces `{{`, `}}`)

Example transformation:
```veld
println~("Hello, {name}!")
  ↓
let _tmp = "Hello, " + name.to_string() + "!"
std.io.println(_tmp)
```

### Phase 2: Expression Support
1. Parse expressions within `{...}`
2. Evaluate expressions
3. Convert results to strings

Example:
```veld
println~("Sum: {x + y}")
  ↓
let _tmp = "Sum: " + (x + y).to_string()
std.io.println(_tmp)
```

### Phase 3: Format Specifiers
1. Parse format specifier syntax `{expr:spec}`
2. Implement formatting functions
3. Apply appropriate formatting

### Phase 4: Advanced Features
1. Positional arguments
2. Named arguments
3. Width and alignment
4. Precision control

## Recommended Approach

Start with **Phase 1** to get basic interpolation working. This would cover 90% of common use cases:

```veld
let name = "Alice"
let age = 30
println~("Hello, {name}! You are {age} years old.")
  → "Hello, Alice! You are 30 years old."
```

## Edge Cases to Handle

1. **Escaped braces**
   ```veld
   println~("Use {{braces}} for literal braces")
   → "Use {braces} for literal braces"
   ```

2. **Empty interpolation**
   ```veld
   println~("Value: {}")  # Error or use positional?
   ```

3. **Nested braces**
   ```veld
   println~("Array: {[1, 2, 3]}")  # How to handle?
   ```

4. **Whitespace in placeholders**
   ```veld
   println~("Value: { x }")  # Should this work?
   ```

5. **Type conversion**
   ```veld
   println~("Number: {42}")  # Already a string literal?
   println~("Array: {[1, 2, 3]}")  # How to format arrays?
   ```

## Testing Checklist

Once interpolation is implemented, verify:
- [ ] Simple variable substitution
- [ ] Multiple variables in one string
- [ ] Expressions in placeholders
- [ ] Arithmetic operations
- [ ] String concatenation in expressions
- [ ] Boolean expressions
- [ ] Comparison results
- [ ] Array/collection printing
- [ ] Nested expressions
- [ ] Escaped braces
- [ ] Empty strings
- [ ] Very long strings
- [ ] Unicode characters
- [ ] Special characters in variables
- [ ] Whitespace handling
- [ ] Repeated variable references

## Conclusion

The `println~` macro currently lacks the string interpolation feature that makes Rust's `println!` so convenient. Implementing basic interpolation (Phase 1) would be a high-value addition that would significantly improve the developer experience in Veld.

The current test file (`tests/print_macro_test.veld`) can serve as a comprehensive test suite once interpolation is implemented.