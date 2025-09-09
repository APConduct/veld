# Type Conformance in Veld

This document explains how to ensure that types properly conform to kinds (interfaces) in the Veld programming language.

## Overview

In Veld, types can implement kinds in two ways:

1. **Explicit Implementation**: Using `impl TypeName: KindName` syntax
2. **Structural Implementation**: Automatic conformance when a type has all required methods

For built-in types like `str` and arrays, conformance is declared in the Rust implementation of the type system.

## Current Type Conformance

### String Types (`str`)

The `str` type automatically conforms to the following kinds:

- `std.string.Transformable` - String transformation operations
- `std.string.Searchable` - String search operations  
- `std.string.Manipulatable` - String manipulation operations
- `std.string.Parsable` - String parsing operations
- `std.core.ToString` - String representation
- `std.core.Sized` - Size/length operations

### Array Types (`[T]`)

Array types automatically conform to:

- `std.collections.sequence.Sequence<T>` - Basic sequence operations
- `std.core.Sized` - Size/length operations

### Numeric Types (`i32`, `f64`, etc.)

Numeric types conform to:

- `std.numeric.Numeric` - Basic numeric operations
- `std.numeric.Integer` (for integer types) - Integer-specific operations
- `std.numeric.Float` (for float types) - Float-specific operations
- `std.core.ToString` - String representation

## How Conformance Works

### Built-in Type Conformance

Built-in type conformance is declared in the Rust source file `core/src/types/base.rs` in the `type_structurally_implements_kind` method:

```rust
// String capabilities
(Type::String, "std.string.Transformable") => return true,
(Type::String, "std.string.Searchable") => return true,
(Type::String, "std.string.Manipulatable") => return true,
(Type::String, "std.string.Parsable") => return true,

// Sequence capabilities  
(Type::Array(_), "std.collections.sequence.Sequence") => return true,
```

### Custom Type Conformance

For custom types, you can declare conformance explicitly:

```veld
# Define a custom string-like type
struct MyString
    content: str
end

# Explicitly implement string kinds
impl MyString: Transformable
    fn to_upper(self) -> str
        self.content.to_upper()
    end
    
    fn to_lower(self) -> str
        self.content.to_lower()
    end
    
    fn trim(self) -> str
        self.content.trim()
    end
    
    fn trim_start(self) -> str
        self.content.trim_start()
    end
    
    fn trim_end(self) -> str
        self.content.trim_end()
    end
end
```

### Structural Conformance

Types can also conform structurally by simply having all required methods:

```veld
struct AutoString
    value: str
    
    # These methods automatically make AutoString conform to Transformable
    fn to_upper(self) -> str => self.value.to_upper()
    fn to_lower(self) -> str => self.value.to_lower()
    fn trim(self) -> str => self.value.trim()
    fn trim_start(self) -> str => self.value.trim_start()
    fn trim_end(self) -> str => self.value.trim_end()
end
```

## Testing Type Conformance

To test that types properly conform to kinds, you can:

1. **Use polymorphic functions** that accept types implementing specific kinds:

```veld
fn normalize<T: Transformable>(text: T) -> str
    text.to_lower().trim()
end

# This should work with str
let result = normalize("  HELLO  ")
```

2. **Test method availability** directly:

```veld
let s = "test string"
let upper = s.to_upper()  # Should work if str implements Transformable
let contains = s.contains("test")  # Should work if str implements Searchable
```

3. **Use type annotations** to verify conformance:

```veld
let s: str = "hello"
let transformable: Transformable = s  # Should work if str implements Transformable
```

## Adding New Kind Conformance

### For Built-in Types

To add conformance for built-in types to new kinds:

1. Add the conformance declaration in `core/src/types/base.rs`:

```rust
// In type_structurally_implements_kind method
(Type::String, "std.mynewmodule.MyNewKind") => return true,
```

2. Ensure the required methods are implemented in the interpreter's native method registry.

### For New Built-in Methods

If you need to add new methods to built-in types:

1. Register the method in `core/src/interpreter.rs` in the appropriate initialization function:

```rust
// For string methods
self.native_method_registry.register_string_method("new_method", |s| {
    // implementation
});

// For array methods in initialize_array_methods()
array_methods.insert("new_method".to_string(), Value::Function { /* ... */ });
```

## Best Practices

1. **Define comprehensive kinds** that group related functionality
2. **Use explicit implementation** for custom types to make intent clear
3. **Test conformance** with polymorphic functions and type annotations
4. **Document kind requirements** clearly in your kind definitions
5. **Leverage structural typing** for rapid prototyping, but prefer explicit implementations for production code

## Example: Complete Type Conformance

Here's a complete example showing how to define and test type conformance:

```veld
# Define a kind
kind Drawable
    fn draw(self) -> str
    fn get_color(self) -> str
end

# Custom type with explicit conformance
struct Circle
    radius: f64,
    color: str
end

impl Circle: Drawable
    fn draw(self) -> str
        format!("Drawing circle with radius {}", self.radius)
    end
    
    fn get_color(self) -> str
        self.color
    end
end

# Polymorphic function using the kind
fn render<T: Drawable>(shape: T) -> str
    let drawing = shape.draw()
    let color = shape.get_color()
    format!("{} in color {}", drawing, color)
end

# Test the conformance
let circle = Circle { radius: 5.0, color: "red" }
let rendered = render(circle)  # Should work because Circle implements Drawable
```

This system ensures type safety while providing flexibility for both built-in and custom types.