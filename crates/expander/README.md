# Veld Macro Expander

The Veld Macro Expander is a comprehensive macro system for the Veld programming language, inspired by both Rust's declarative macros and Nim's template system. It provides powerful metaprogramming capabilities while maintaining safety, hygiene, and **backward compatibility** with existing macro code.

## Features

- **Backward Compatibility**: Existing simple macros continue to work unchanged
- **Evolutionary Design**: Gradual migration path from simple to advanced macros
- **Declarative Macros**: Pattern-matching macros similar to Rust's `macro_rules~`
- **Template Macros**: Nim-style template system for direct AST manipulation
- **Compile-time Procedures**: Execute code during compilation
- **Hygienic Expansion**: Automatic variable renaming to prevent name conflicts
- **Built-in Macros**: Common macros like `vec~`, `format~`, `println!`, etc.
- **Custom DSLs**: Support for domain-specific languages like HTML and SQL
- **Rich Metadata**: Description, examples, stability markers, and custom attributes

## Architecture

The expander consists of several key components:

### Core Components

- **`ExpansionContext`**: Manages macro definitions and expansion state
- **`PatternMatcher`**: Handles pattern matching for declarative macros
- **`TemplateExpander`**: Expands template macros with variable substitution
- **`MacroSystem`**: High-level integration point with the Veld interpreter/compiler

### Parser Components

- **`MacroParser`**: Parses macro definitions from source code
- **`MacroPatternParser`**: Specialized parser for macro patterns

## Macro Types

### 1. Simple Macros (Original Format)

Simple macros use the original structure and continue to work unchanged:

```veld
# Original macro format (still supported)
let my_macro = MacroDefinition::simple(
    "my_macro".to_string(),
    vec!["x".to_string(), "y".to_string()],
    vec![/* body statements */],
    node_id,
)
```

### 2. Enhanced Simple Macros

Add metadata without changing functionality:

```veld
let enhanced_macro = MacroDefinition::simple(
    "my_macro".to_string(),
    vec!["x".to_string()],
    vec![/* body */],
    node_id,
)
.with_description("My enhanced macro".to_string())
.with_example("my_macro~(42)".to_string())
.with_stability(MacroStability::Stable);
```

### 3. Declarative Macros

Declarative macros use pattern matching to transform input into output:

```rust
let vec_macro = MacroDefinition::declarative(
    "vec".to_string(),
    vec![
        MacroPattern("()".to_string()),
        MacroPattern("($($elem:expr),+ $(,)?)".to_string()),
    ],
    vec![
        MacroTemplate { /* empty vec template */ },
        MacroTemplate { /* elements template */ },
    ],
    node_id,
)
.with_description("Creates a vector from elements".to_string());
```

**Veld Syntax:**
```veld
macro~ vec
    () => Vec::new(),
    ($($elem:expr),+ $(,)?) => [$$elem]+
end
```

**Pattern Elements:**
- `$name:type` - Capture a fragment of the specified type
- `$($pattern),+` - Repetition with separator (one or more)
- `$($pattern),*` - Repetition with separator (zero or more)
- `$($pattern)?` - Optional pattern

**Fragment Types:**
- `expr` - Expression
- `stmt` - Statement
- `ident` - Identifier
- `literal` - Literal value
- `tt` - Token tree (any token sequence)
- `item` - Top-level item (function, struct, etc.)
- `block` - Block of statements

### 4. Template Macros

Template macros provide direct AST manipulation:

```rust
let debug_macro = MacroDefinition::template(
    "debug".to_string(),
    vec!["expr".to_string()],
    vec![/* template body */],
    node_id,
    false, // not typed
)
.with_description("Debug print an expression".to_string());
```

**Veld Syntax:**
```veld
template~ debug(expr)
    let value = expr
    println~("DEBUG: {} = {}", stringify!(expr), value)
    value
end
```

### 5. Compile-time Procedures

Execute code during compilation:

```rust
let proc_macro = MacroDefinition::compile_time_proc(
    "generate_getters".to_string(),
    vec!["struct_name".to_string(), "fields".to_string()],
    vec![/* procedure body */],
    node_id,
    Some(return_type),
)
.with_stability(MacroStability::Experimental);
```

### 6. Built-in Macros

The system includes several built-in macros:

- **`vec~()`** - Create vectors: `vec~(1, 2, 3)`
- **`format~()`** - String formatting: `format~("Hello {}", name)`
- **`println~()`** - Print to stdout: `println~("Hello, World!")`
- **`debug~()`** - Debug printing: `debug~(expression)`
- **`assert~()`** - Runtime assertions: `assert~(condition)`
- **`todo~()`** - Placeholder for unimplemented code

## Usage

### Migration-Friendly Usage

The system is designed for **zero-breaking-change migration**:

```rust
use veld_expander::{MacroDefinition, MacroSystem, MacroStability};
use veld_core::ast::*;

// 1. Existing code works unchanged
let old_macro = MacroDefinition::simple(
    "my_macro".to_string(),
    vec!["x".to_string()],
    vec![/* body */],
    NodeId::new(),
);

// 2. Enhance gradually with metadata
let enhanced = old_macro
    .with_description("My macro description".to_string())
    .with_stability(MacroStability::Stable);

// 3. Upgrade to advanced features when ready
let advanced = MacroDefinition::declarative(
    "advanced_macro".to_string(),
    patterns,
    templates,
    NodeId::new(),
);

// 4. Use with macro system
let mut macro_system = MacroSystem::new();
let expanded = macro_system.expand_macro_call("vec", &args, NodeId::new())?;
```

### Parsing and Registering Custom Macros

```rust
let macro_source = r#"
macro~ unless
    ($condition:expr => $body:block) => do
        if !($condition) then
            $body
        end
    end
end
"#;

let macro_name = macro_system.parse_and_register_macro(macro_source, NodeId::new())?;
```

### Preprocessing Statements

```rust
// Preprocess a statement to expand any macro calls
let stmt = Statement::MacroInvocation {
    name: "vec".to_string(),
    arguments: vec![
        Expr::Literal(Literal::Integer(1)),
        Expr::Literal(Literal::Integer(2)),
    ],
};

let expanded_stmts = macro_system.preprocess_statement(stmt)?;
```

## Advanced Features

### Migration Strategies

The system supports multiple migration strategies:

**Strategy 1: Keep Existing (No Changes)**
```rust
// Existing macros work as-is
let macro_def = MacroDefinition::simple(name, params, body, node_id);
```

**Strategy 2: Add Metadata Gradually**
```rust
// Add documentation and stability info
let enhanced = existing_macro
    .with_description("Description".to_string())
    .with_stability(MacroStability::Stable);
```

**Strategy 3: Upgrade to Declarative**
```rust
// Move to pattern-based macros when needed
let declarative = MacroDefinition::declarative(name, patterns, templates, node_id);
```

### Hygiene

The macro system automatically handles variable hygiene to prevent name conflicts:

```veld
macro~ swap
    ($a:ident, $b:ident) => do
        let temp = $a
        $a = $b
        $b = temp
    end
end

# The 'temp' variable is automatically renamed to avoid conflicts
```

### Repetition Patterns

Support for complex repetition patterns:

```veld
macro~ match_arms
    ($($pattern:pat => $expr:expr),*) => do
        match value
            $($pattern => $expr),*
        end
    end
end
```

### DSL Support

Create domain-specific languages:

```veld
# HTML DSL
let content = html~(
    <div class="container">
        <h1>Welcome</h1>
        <p>This is a paragraph</p>
    </div>
)

# SQL DSL
let query = sql~(
    SELECT name, email FROM users WHERE active = true
)
```

### Metadata and Stability

The system includes rich metadata support:

```rust
let macro_def = MacroDefinition::simple(name, params, body, node_id)
    .with_description("Detailed description".to_string())
    .with_example("example_usage!()".to_string())
    .with_stability(MacroStability::Stable)
    .with_attribute("category".to_string(), "utility".to_string());

// Check macro properties
assert!(macro_def.is_simple());
assert_eq!(macro_def.metadata.stability, MacroStability::Stable);
```

## Error Handling

The expander provides comprehensive error reporting:

```rust
pub enum ExpansionError {
    RecursionLimit { macro_name: String, call_site: NodeId },
    MacroNotFound { name: String, call_site: NodeId },
    ArgumentCountMismatch { macro_name: String, expected: usize, got: usize, call_site: NodeId },
    PatternMatchFailed { macro_name: String, expected_pattern: String, call_site: NodeId },
    UnboundVariable { name: String, macro_name: String, position: NodeId },
    // ... more error types
}
```

## Performance Considerations

- **Recursion Limits**: Prevents infinite macro expansion
- **Size Limits**: Prevents macros from generating excessively large code
- **Caching**: Pattern matching results are cached for performance
- **Lazy Evaluation**: Templates are only expanded when needed

## Testing

The crate includes comprehensive tests:

```bash
cargo test
```

Run specific test categories:

```bash
# Test pattern matching
cargo test pattern

# Test macro expansion
cargo test expansion

# Test built-in macros
cargo test builtin
```

## Examples

See the `examples/` directory for comprehensive usage examples:

- `macro_usage.veld` - Basic macro usage patterns
- `advanced_macros.veld` - Advanced macro techniques
- `dsl_examples.veld` - Domain-specific language examples

## Integration with Veld Core

The expander integrates seamlessly with the Veld core:

```rust
use veld_expander::MacroSystem;
use veld_core::interpreter::Interpreter;

let mut interpreter = Interpreter::new();
let mut macro_system = MacroSystem::new();

// Preprocess AST to expand macros
let processed_ast = macro_system.preprocess_statements(&ast.statements)?;
```

## Future Enhancements

- **Procedural Macros**: Function-like macros that operate on token streams
- **Attribute Macros**: Macros that can be attached to items
- **Derive Macros**: Automatic implementation generation
- **IDE Integration**: Language server support for macro expansion
- **Debugging Support**: Step-through debugging of macro expansions

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the same terms as the main Veld project.
