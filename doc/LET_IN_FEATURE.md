# Let-In Expressions Feature

## Date: November 15, 2024

## Summary

Veld now supports OCaml-style `let ... in ...` expressions, allowing for elegant local bindings without needing `do` blocks. This feature provides a more functional programming style for creating scoped variable bindings within expressions.

---

## Syntax

```veld
let <var> = <expr> in <body>
```

### Variants
- `let <var> = <expr> in <body>` - Immutable binding
- `var <var> = <expr> in <body>` - Mutable binding
- `const <var> = <expr> in <body>` - Constant binding
- `let <var>: <type> = <expr> in <body>` - With type annotation

---

## Examples

### Basic Usage

```veld
# Simple let-in
let result = let x = 10 in x + 5
# result = 15

# Chained let-in
let result = let x = 10 in let y = 20 in x + y
# result = 30
```

### In Functions

```veld
fn calculate(x: i32) =>
    let doubled = x * 2 in
    let squared = doubled * doubled in
    squared + x
```

### With Shadowing

```veld
let result =
    let x = 10 in
    let x = x + 5 in
    let x = x * 2 in
    x
# result = 30
```

### Nested Let-In

```veld
let result =
    let x = 5 in
    let y = (let inner = 10 in inner * 2) in
    x + y
# result = 25
```

### With If Expressions

```veld
let result =
    let x = 15 in
    if x > 10 then
        let bonus = 100 in
        x + bonus
    else
        x
    end
# result = 115
```

### Returning Tuples

```veld
let pair =
    let a = 10 in
    let b = 20 in
    (a, b, a + b)
# pair = (10, 20, 30)
```

### With Type Annotations

```veld
let result =
    let x: i32 = 42 in
    let y: i32 = x * 2 in
    x + y
# result = 126
```

### Accessing Outer Scope

```veld
fn outer_function() => do
    let outer_var = 100
    let inner_result = let x = 10 in x + outer_var
    inner_result
end
# Returns 110
```

---

## Comparison with Do Blocks

### Let-In (Expression-based)

```veld
let result = let x = 10 in let y = 20 in x + y
```

**Advantages:**
- More concise for simple bindings
- Purely expression-based (functional style)
- Easy to chain multiple bindings
- Natural scoping without explicit blocks

### Do Blocks (Statement-based)

```veld
let result = do
    let x = 10
    let y = 20
    x + y
end
```

**Advantages:**
- Better for multiple statements
- Clearer for complex logic
- More familiar to imperative programmers
- Can mix statements and expressions

---

## Scoping Rules

### Local Scope
Each `let ... in` creates a new scope for the binding:

```veld
let x = 10
let result = let x = 20 in x + 5  # Inner x shadows outer x
# result = 25, outer x is still 10
```

### Chained Scope
Later bindings can reference earlier ones:

```veld
let result =
    let a = 1 in
    let b = a + 1 in    # b can reference a
    let c = a + b in    # c can reference both a and b
    c
# result = 3
```

### Nested Scope
Inner let-in expressions have access to outer bindings:

```veld
let result =
    let outer = 10 in
    let inner = (let x = 5 in x + outer) in
    inner
# result = 15
```

---

## Type Checking

### Type Inference
Types are inferred from the value expression:

```veld
let result = let x = 42 in x + 10
# x is inferred as i32
```

### Type Annotations
Explicit type annotations are supported:

```veld
let result = let x: i32 = 42 in x + 10
```

### Type Constraints
The binding type must match the annotation:

```veld
let result = let x: i32 = 42 in let y: i32 = x * 2 in x + y
# Type checked: x and y are both i32
```

---

## Implementation Details

### AST Node

```rust
Expr::LetIn {
    name: String,
    var_kind: VarKind,
    type_annotation: Option<TypeAnnotation>,
    value: Box<Expr>,
    body: Box<Expr>,
}
```

### Parser
- Recognized in `primary()` expression parser
- Checks for `let`, `var`, or `const` token
- Parses variable name, optional type annotation, value expression
- Expects `in` keyword
- Parses body expression
- Creates new `LetIn` expression node

### Type Checker
- Creates new scope for the binding
- Infers type of value expression
- Validates type annotation if present
- Binds variable in scope
- Infers type of body expression with binding available
- Pops scope after body
- Returns body's type as the let-in expression's type

### Interpreter
- Evaluates value expression
- Creates new scope
- Binds variable to evaluated value
- Evaluates body expression in new scope
- Pops scope
- Returns body result

### Bytecode Compiler
- Begins new scope
- Compiles value expression to register
- Registers variable in scope
- Compiles body expression with variable available
- Ends scope (frees local registers)
- Returns body result register

---

## Use Cases

### 1. Simple Calculations with Named Intermediates

```veld
let total =
    let subtotal = price * quantity in
    let tax = subtotal * tax_rate in
    subtotal + tax
```

### 2. Functional-Style Pipelines

```veld
let result =
    let data = load_data() in
    let filtered = filter(data) in
    let transformed = transform(filtered) in
    aggregate(transformed)
```

### 3. Avoiding Temporary Variables

**Before:**
```veld
fn calculate(x: i32) => do
    let temp1 = x * 2
    let temp2 = temp1 + 10
    temp2 * temp2
end
```

**After:**
```veld
fn calculate(x: i32) =>
    let temp1 = x * 2 in
    let temp2 = temp1 + 10 in
    temp2 * temp2
```

### 4. Local Constants in Expressions

```veld
let circle_area =
    let pi = 3.14159 in
    let radius = 5.0 in
    pi * radius * radius
```

---

## Testing

### Test Coverage
15 comprehensive tests covering:
1. ✅ Simple let-in expressions
2. ✅ Computed let-in with multiple bindings
3. ✅ Let-in in function bodies
4. ✅ Nested let-in expressions
5. ✅ Let-in with shadowing
6. ✅ Let-in returning tuples
7. ✅ Let-in with if expressions
8. ✅ Multiple chained let-in
9. ✅ Let-in with type annotations
10. ✅ Let-in as function body
11. ✅ Complex nesting
12. ✅ Let-in with arithmetic
13. ✅ Chained let-in in single line
14. ✅ Let-in with outer scope access
15. ✅ Let-in with function calls

### Test File
`test_let_in.veld` - Comprehensive test suite demonstrating all features

---

## Language Design

### Philosophy
The let-in expression brings functional programming elegance to Veld while maintaining compatibility with the existing imperative style. It provides developers with choices:

- **Functional style**: Use let-in for expression-based code
- **Imperative style**: Use do blocks for statement-based code
- **Hybrid**: Mix both styles as appropriate

### Inspiration
Borrowed from OCaml and other functional languages:

**OCaml:**
```ocaml
let x = 10 in
let y = 20 in
x + y
```

**Veld:**
```veld
let x = 10 in
let y = 20 in
x + y
```

---

## Benefits

1. **Expressiveness**: More concise syntax for simple bindings
2. **Functional Style**: Supports functional programming patterns
3. **Scoping**: Clear, lexical scoping without explicit blocks
4. **Composability**: Easy to nest and chain expressions
5. **Type Safety**: Full type checking and inference
6. **Performance**: Efficient compilation (no overhead vs do blocks)

---

## Future Enhancements

### Potential Features
1. **Pattern matching in let-in**: `let (x, y) = get_pair() in x + y`
2. **Multiple bindings**: `let x = 1, y = 2 in x + y`
3. **Let-in with guards**: `let x = value in if x > 0 then ... else ...`
4. **Recursive let**: `let rec fib = ... in fib(10)`

---

## Conclusion

The let-in expression feature successfully extends Veld's expression system with functional programming capabilities while maintaining backward compatibility. All tests pass, and the implementation is complete across the parser, type checker, interpreter, and bytecode compiler.

**Status: COMPLETE ✅**

---

## Files Modified

- `crates/common/src/ast.rs` - Added `Expr::LetIn` variant
- `crates/common/src/parser.rs` - Added `parse_let_in_expression()` function
- `crates/common/src/types/checker.rs` - Added let-in type checking
- `crates/interpreter/src/interpreter.rs` - Added let-in interpretation
- `crates/bytecode/src/compiler_v2.rs` - Added let-in bytecode compilation

## Tests Created

- `test_let_in.veld` - 15 comprehensive tests
- `test_let_in_simple.veld` - Basic functionality tests

## Documentation

- `LET_IN_FEATURE.md` - This file (feature documentation)