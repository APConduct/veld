# Next Steps for Type Display

## Current State ✅

After the improvements, hover info now shows:
- `fn add(a: T, b: U)` - Parameter names with generic types
- `fn multiply(x: i32, y: i32) -> i32` - Concrete inferred types

## Future Enhancements

### 1. Generic Constraints (Most Valuable)

Show trait bounds and constraints:

```veld
fn add(a: T, b: U) -> V where T: Add<U, Output = V>
```

**Why?** Users would know what operations are allowed on the parameters.

**Implementation:**
- Type checker needs to track constraints during inference
- Extract constraint information from checker
- Format with `where` clauses

### 2. Documentation Comments

Extract and display doc comments:

```veld
/// Adds two numbers together
/// Returns the sum
fn add(a: T, b: U) -> V
```

**Why?** Provides context and usage information.

**Implementation:**
- Parser needs to preserve comments
- Associate comments with AST nodes
- Include in hover response

### 3. Type Inference Context

Show why types were inferred:

```veld
fn add(a: i32, b: i32) -> i32

Inferred from:
  - Call at line 20: add(10, 20)
  - Variable assignment: sum: i32
```

**Why?** Helps understand type inference behavior.

**Implementation:**
- Track inference provenance in type checker
- Store origin information (call sites, assignments)
- Display in hover

### 4. Examples in Hover

Show usage examples:

```veld
fn add(a: T, b: U) -> V

Example:
  let sum = add(1, 2)      // => 3
  let concat = add("a", "b")  // => "ab"
```

**Why?** Quick reference without leaving the editor.

**Implementation:**
- Parse doc comment examples
- Or generate from test cases
- Format in hover response

### 5. Signature Overloads

If Veld supports overloading:

```veld
fn add
  (a: i32, b: i32) -> i32
  (a: f64, b: f64) -> f64  
  (a: String, b: String) -> String
```

**Why?** Shows all available versions.

### 6. Return Type Inference

Always show return type, even when inferred:

```veld
fn add(a: T, b: U) -> T  // Currently might just be "fn add(a: T, b: U)"
```

**Why?** Complete signature information.

**Implementation:**
- Always extract return type from type checker
- Display even if not annotated in source

## Priority Ranking

Based on value to users:

1. **🔥 Generic Constraints** - Most important for understanding functions
2. **📝 Documentation Comments** - Standard in modern IDEs
3. **↩️ Return Type Always Shown** - Basic completeness
4. **📚 Examples** - Very helpful for learning
5. **🔍 Type Inference Context** - Advanced debugging feature
6. **⚡ Signature Overloads** - Only if language supports it

## Quick Wins

Start with these simpler improvements:

### A. Show Return Type Even When Inferred
Currently: `fn add(a: T, b: U)`
Better: `fn add(a: T, b: U) -> V`

Just need to extract return type from checker and always display it.

### B. Basic Doc Comment Support
Just show the first line of any comment above the function:

```veld
# Adds two numbers
fn add(a, b)
```

Hover shows: 
```
Adds two numbers
fn add(a: T, b: U) -> V
```

### C. Fallback to "Any" for Unknown Types
Currently: `fn add(a: T, b: U)`
If truly unknown: `fn add(a: any, b: any)`

Makes it clear when types aren't constrained.

## Your Thoughts?

Which of these would be most valuable for Veld users?
What other information would be helpful in hover tooltips?
