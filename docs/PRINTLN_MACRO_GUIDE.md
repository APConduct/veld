# println~ Macro Guide

## Quick Reference

### Basic Syntax
```veld
println~("format string with {placeholders}")
```

### Simple Variable
```veld
let name = "Alice"
println~("Hello, {name}!")
```

### Expressions
```veld
let x = 10
println~("Result: {x * 2}")
```

### Multiple Variables
```veld
let a = 5
let b = 3
println~("{a} + {b} = {a + b}")
```

## Supported in Placeholders

### ✅ Identifiers
```veld
{variable}
{my_var}
```

### ✅ Arithmetic
```veld
{x + y}
{a - b}
{n * 2}
{total / count}
{x % 2}
```

### ✅ Comparisons
```veld
{x < y}
{a > b}
{x == y}
{a != b}
{x <= 10}
{y >= 0}
```

### ✅ Logical
```veld
{x > 0 and x < 10}
{is_valid or is_admin}
```

### ✅ String Concatenation
```veld
{first + " " + last}
```

### ✅ Property Access
```veld
{obj.field}
```

### ✅ Literals
```veld
{42}
{3.14}
{"text"}
{true}
```

## Tips

1. **Complex expressions** - Use variables:
   ```veld
   # Instead of:
   println~("Result: {(a + b) * (c - d)}")
   
   # Do:
   let result = (a + b) * (c - d)
   println~("Result: {result}")
   ```

2. **Whitespace** - Allowed in placeholders:
   ```veld
   println~("Value: { x + y }")  # Works!
   ```

3. **Empty strings** - Valid:
   ```veld
   println~("")  # Prints empty line
   ```

4. **print~ vs println~**:
   ```veld
   print~("No newline")
   println~("With newline")
   ```

## Common Patterns

### Debug Output
```veld
let x = 10
let y = 20
println~("DEBUG: x={x}, y={y}, sum={x+y}")
```

### Error Messages
```veld
let expected = 5
let got = 3
println~("Error: expected {expected}, got {got}")
```

### Progress
```veld
let done = 50
let total = 100
println~("Progress: {done}/{total}")
```

### Conditionals
```veld
let score = 85
println~("Pass: {score >= 60}")
```

## Escape Sequences

### Literal Braces (not fully tested)
```veld
println~("Use {{braces}} for literal")
# Should output: Use {braces} for literal
```

## Limitations

1. **Arrays** - Currently show as GcRef
2. **Nested parens** - May not parse correctly
3. **Method args** - Only zero-argument methods work
4. **Format specs** - Not yet implemented (`:?`, etc.)

## Migration from Old Code

### Before
```veld
let name = "Alice"
let age = 30
io.println("Hello, " + name + "! You are " + age.to_string() + " years old.")
```

### After
```veld
let name = "Alice"
let age = 30
println~("Hello, {name}! You are {age} years old.")
```

Much cleaner! 🎉

## Examples

### Example 1: User Greeting
```veld
import std.io

let username = "Alice"
let login_count = 42
println~("Welcome back, {username}! This is your {login_count}th login.")
```

### Example 2: Math Operations
```veld
let x = 10
let y = 20
println~("Addition: {x} + {y} = {x + y}")
println~("Subtraction: {x} - {y} = {x - y}")
println~("Multiplication: {x} * {y} = {x * y}")
println~("Division: {x} / {y} = {x / y}")
```

### Example 3: Validation Messages
```veld
let age = 25
let min_age = 18
println~("Age valid: {age >= min_age}")
println~("Message: {age} is {age - min_age} years above minimum")
```

### Example 4: Status Display
```veld
let success = true
let error_count = 0
println~("Status: {success}")
println~("Errors: {error_count}")
println~("All clear: {success and error_count == 0}")
```

## Performance Notes

The `println~` macro expands at compile-time into string concatenation and `to_str()` calls. For example:

```veld
println~("Hello, {name}!")
```

Becomes approximately:

```veld
std.io.println("Hello, " + to_str(name) + "!")
```

This means:
- No runtime overhead for parsing format strings
- All type checking happens at compile time
- Efficient string concatenation

## Comparison with Rust

Veld's `println~` is inspired by Rust's `println!` but has some differences:

| Feature | Rust | Veld |
|---------|------|------|
| Basic interpolation | ✅ | ✅ |
| Expressions | ✅ | ✅ |
| Format specifiers | ✅ | ❌ (not yet) |
| Positional args | ✅ | ❌ (not yet) |
| Named args | ✅ | ❌ (not yet) |

## Future Enhancements

Planned features for future versions:

1. **Format Specifiers**
   ```veld
   println~("Debug: {value:?}")
   println~("Hex: {num:x}")
   println~("Precision: {pi:.2}")
   ```

2. **Positional Arguments**
   ```veld
   println~("{0} {1} {0}", x, y)
   ```

3. **Named Arguments**
   ```veld
   println~("{name}: {value}", name="x", value=10)
   ```

4. **Width and Alignment**
   ```veld
   println~("{:10}", x)  # Right align in 10 chars
   println~("{:<10}", x) # Left align
   ```

## Troubleshooting

### Problem: "Invalid expression in interpolation"
**Cause**: Complex expression not supported by parser  
**Solution**: Break into simpler expressions or use a variable

```veld
# Instead of:
println~("Result: {(a + b) * (c - d)}")

# Do:
let result = (a + b) * (c - d)
println~("Result: {result}")
```

### Problem: Array shows as GcRef
**Cause**: Array dereferencing not yet implemented  
**Solution**: Use workaround or wait for fix

```veld
# Workaround - iterate and print manually
let arr = [1, 2, 3]
for item in arr do
    print~("{item} ")
end
```

### Problem: "Unclosed brace"
**Cause**: Missing closing brace in placeholder  
**Solution**: Ensure all `{` have matching `}`

```veld
# Wrong:
println~("Value: {x")

# Right:
println~("Value: {x}")
```

## Best Practices

1. **Use descriptive variables in placeholders**
   ```veld
   # Good
   println~("User {username} has {login_count} logins")
   
   # Less clear
   println~("User {u} has {c} logins")
   ```

2. **Keep expressions simple**
   ```veld
   # Good
   let total = price * quantity
   println~("Total: {total}")
   
   # Harder to read
   println~("Total: {price * quantity * (1 + tax_rate)}")
   ```

3. **Use for debugging during development**
   ```veld
   println~("DEBUG: entering function with x={x}, y={y}")
   ```

4. **Combine with io.println for complex formatting**
   ```veld
   println~("Results:")
   for item in results do
       println~("  - {item}")
   end
   ```

## See Also

- `print~` - Print without newline
- `std.io` module - Other I/O functions
- String formatting documentation
- Macro system guide