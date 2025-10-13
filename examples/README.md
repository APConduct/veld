# Veld Macro System Examples

This directory contains examples demonstrating the macro expansion system in Veld.

## Overview

The Veld macro system supports powerful compile-time code generation through several types of macros:

1. **Builtin Macros** - Pre-defined macros like `vec!`, `format!`, `println!`
2. **Custom Macros** - User-defined macros that can generate complex code patterns
3. **Declarative Macros** - Pattern-based macros with template expansion
4. **Procedural Macros** - Full compile-time code generation

## Example Macros

### `getters_setters!`

Automatically generates getter and setter methods for struct fields.

**Usage:**
```rust
impl MyStruct {
    getters_setters!(name: String, age: i32, active: bool);
}
```

**Expands to:**
```rust
impl MyStruct {
    pub fn get_name(&self) -> &String {
        &self.name
    }
    
    pub fn set_name(&mut self, value: String) {
        self.name = value;
    }
    
    pub fn get_age(&self) -> &i32 {
        &self.age
    }
    
    pub fn set_age(&mut self, value: i32) {
        self.age = value;
    }
    
    pub fn get_active(&self) -> &bool {
        &self.active
    }
    
    pub fn set_active(&mut self, value: bool) {
        self.active = value;
    }
}
```

### `with_fields!`

Generates builder pattern methods for fluent configuration.

**Usage:**
```rust
impl Config {
    with_fields!(host: String, port: i32, debug: bool);
}
```

**Expands to:**
```rust
impl Config {
    pub fn with_host(mut self, host: String) -> Self {
        self.host = host;
        self
    }
    
    pub fn with_port(mut self, port: i32) -> Self {
        self.port = port;
        self
    }
    
    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }
}
```

### `repeat!`

Repeats a statement or expression a specified number of times.

**Usage:**
```rust
repeat!(5, println!("Hello"));
```

**Expands to:**
```rust
println!("Hello");
println!("Hello");
println!("Hello");
println!("Hello");
println!("Hello");
```

## Files in this Directory

- `macro_usage.veld` - Complete examples showing macro usage in different contexts
- `example_macro.rs` - Implementation of the custom macros in Rust

## How Macros Work

The macro expansion system works in several phases:

1. **Parsing**: Macro calls are identified during AST construction
2. **Expansion**: Each macro call is replaced with generated AST nodes
3. **Validation**: Generated code is validated for correctness
4. **Integration**: Expanded code is integrated into the final AST

### Macro Implementation Structure

```rust
pub fn expand_my_macro(
    args: &[Expr],
    call_site: NodeId,
) -> Result<Vec<Statement>, ExpansionError> {
    // 1. Validate arguments
    if args.is_empty() {
        return Err(ExpansionError::ArgumentCountMismatch { ... });
    }
    
    // 2. Parse arguments
    let parsed_data = parse_arguments(args, call_site)?;
    
    // 3. Generate AST nodes
    let mut statements = Vec::new();
    for item in parsed_data {
        let stmt = generate_statement_for(item);
        statements.push(stmt);
    }
    
    // 4. Return generated code
    Ok(statements)
}
```

## Error Handling

The macro system provides comprehensive error handling:

- **ArgumentCountMismatch** - Wrong number of arguments
- **InvalidArgumentType** - Argument has wrong type
- **PatternMatchFailed** - Argument doesn't match expected pattern
- **UnboundVariable** - Variable used but not defined
- **ExpansionTooLarge** - Generated code exceeds limits

## Running the Examples

To run the macro examples:

```bash
# Compile and run the example
cargo run --example macro_usage

# Or if using the Veld interpreter
veld run examples/macro_usage.veld
```

## Best Practices

1. **Validate Early** - Check arguments at the beginning of macro functions
2. **Clear Error Messages** - Provide helpful error messages with context
3. **Hygiene** - Avoid symbol collisions in generated code
4. **Documentation** - Document macro behavior and expansion patterns
5. **Testing** - Write comprehensive tests for macro edge cases

## Advanced Features

### Pattern Matching

Macros can use sophisticated pattern matching:

```rust
match expr {
    Expr::BinaryOp { left, operator: BinaryOperator::Colon, right } => {
        // Handle field: type patterns
    }
    Expr::Identifier(name) => {
        // Handle simple identifiers
    }
    _ => return Err(/* pattern match failed */)
}
```

### Generic Support

Macros can handle generic types:

```rust
// getters_setters!(items: Vec<T>, map: HashMap<K, V>);
```

### Conditional Generation

Generate different code based on conditions:

```rust
if field_type.is_reference() {
    // Generate different getter for references
} else {
    // Standard getter
}
```

## Integration with Build System

The macro system integrates seamlessly with the Veld build process:

1. Macros are expanded during compilation
2. Generated code is included in type checking
3. Debug information tracks macro expansions
4. IDE support shows expanded code

This enables powerful metaprogramming while maintaining good developer experience.