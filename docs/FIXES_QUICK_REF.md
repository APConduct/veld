# Quick Reference: println~ Macro Fixes

## What Was Fixed? ✅

### 1. Array Printing
**Before:**
```veld
let arr = [1, 2, 3]
println~("Array: {arr}")
// Output: Array: GcRef(0x7f...)  ❌
```

**After:**
```veld
let arr = [1, 2, 3]
println~("Array: {arr}")
// Output: Array: [1, 2, 3]  ✅
```

### 2. Logical Operators with Comparisons
**Before:**
```veld
let num = 15
println~("Valid: {num >= 10 and num <= 20}")
// Program fails silently  ❌
```

**After:**
```veld
let num = 15
println~("Valid: {num >= 10 and num <= 20}")
// Output: Valid: true  ✅
```

---

## What Works Now? ✅

### All Value Types
```veld
let int = 42
let float = 3.14
let str = "hello"
let bool = true
let arr = [1, 2, 3]
let tuple = (1, "two", 3.0)

println~("Int: {int}")           // 42
println~("Float: {float}")       // 3.14
println~("String: {str}")        // hello
println~("Bool: {bool}")         // true
println~("Array: {arr}")         // [1, 2, 3]
println~("Tuple: {tuple}")       // (1, two, 3.0)
```

### Arithmetic Expressions
```veld
let x = 10
let y = 20

println~("Sum: {x + y}")         // Sum: 30
println~("Product: {x * y}")     // Product: 200
println~("Division: {y / x}")    // Division: 2
println~("Modulo: {x % 3}")      // Modulo: 1
```

### Comparison Expressions
```veld
let a = 5
let b = 10

println~("Less: {a < b}")        // Less: true
println~("Equal: {a == b}")      // Equal: false
println~("Greater: {a > b}")     // Greater: false
println~("LessEq: {a <= b}")     // LessEq: true
println~("GreaterEq: {a >= b}")  // GreaterEq: false
println~("NotEqual: {a != b}")   // NotEqual: true
```

### Logical Expressions
```veld
let x = 15

println~("Range: {x >= 10 and x <= 20}")    // Range: true
println~("Either: {x < 10 or x > 50}")      // Either: false
println~("Complex: {x > 0 and x < 100}")    // Complex: true
```

### String Operations
```veld
let first = "Hello"
let last = "World"

println~("Concat: {first + \" \" + last}")  // Concat: Hello World
```

### Property Access
```veld
let point = Point { x: 10, y: 20 }

println~("X: {point.x}")         // X: 10
println~("Y: {point.y}")         // Y: 20
```

### Multiple Interpolations
```veld
let name = "Alice"
let score = 95
let level = 10

println~("Player {name} scored {score} at level {level}")
// Player Alice scored 95 at level 10
```

---

## What Still Doesn't Work? ⚠️

### Complex Nested Parentheses
```veld
// May fail:
println~("Result: {((a + b) * (c - d)) / 2}")

// Workaround:
let result = ((a + b) * (c - d)) / 2
println~("Result: {result}")
```

### Method Calls with Arguments
```veld
// Works:
println~("Length: {str.len()}")

// Doesn't work yet:
println~("Sub: {str.substring(0, 5)}")

// Workaround:
let sub = str.substring(0, 5)
println~("Sub: {sub}")
```

### Format Specifiers
```veld
// Not yet supported:
println~("Debug: {value:?}")
println~("Hex: {num:x}")
println~("Precision: {pi:.2}")
```

### Positional/Named Arguments
```veld
// Not yet supported:
println~("{0} {1} {0}", x, y)
println~("{name}: {value}", name="x", value=10)
```

---

## Best Practices

### ✅ DO: Use simple expressions
```veld
println~("Sum: {a + b}")
println~("Valid: {x > 0 and x < 100}")
println~("Array: {numbers}")
```

### ✅ DO: Use intermediate variables for complex logic
```veld
let is_valid = check_conditions(x, y, z)
let formatted_name = format_user_name(user)
println~("User {formatted_name} valid: {is_valid}")
```

### ❌ DON'T: Use deeply nested expressions
```veld
// Avoid:
println~("Result: {((a + b) * (c - d)) / (e + f)}")

// Instead:
let numerator = (a + b) * (c - d)
let denominator = e + f
let result = numerator / denominator
println~("Result: {result}")
```

### ❌ DON'T: Mix too many operators without variables
```veld
// Harder to read and debug:
println~("Complex: {a + b * c - d / e and x > y or z < w}")

// Better:
let calc = a + b * c - d / e
let condition = x > y or z < w
let result = calc and condition
println~("Result: {result}")
```

---

## Testing

Run the comprehensive test suite:
```bash
./target/release/veld tests/print_macro_test.veld
```

All 20 tests should pass:
- ✅ Basic literals
- ✅ Variable interpolation
- ✅ Expressions
- ✅ Arrays *(newly fixed)*
- ✅ Logical operators *(newly fixed)*
- ✅ String concatenation
- ✅ Edge cases

---

## Quick Migration Checklist

- [x] Arrays print correctly (no more GcRef)
- [x] `and`/`or` operators work with comparisons
- [x] All basic value types display properly
- [ ] Complex nested expressions (use variables)
- [ ] Method calls with args (use variables)
- [ ] Format specifiers (not implemented yet)

---

## Need Help?

- Full guide: [PRINTLN_MACRO_GUIDE.md](./PRINTLN_MACRO_GUIDE.md)
- Detailed fixes: [FIXES_SUMMARY.md](./FIXES_SUMMARY.md)
- Test examples: `tests/print_macro_test.veld`
- Demo: `examples/println_demo.veld`
