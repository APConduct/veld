# Debugging stdlib Registration

## The Problem

`empty_function()` shows `-> ()` correctly, but:
- `main()` shows `-> Y` (TypeVar)
- `another_test()` shows `-> ]` (now fixed to `-> T9` or similar)

Both `main` and `another_test` call `io.println()`, suggesting the type checker doesn't know its return type.

## Why empty_function Works

```veld
fn empty_function() => do
end
```

Empty block → type checker infers Unit directly. No function calls involved.

## Why main/another_test Don't Work

```veld
fn main() => do
    io.println("Hello, World!")
end
```

The type checker needs to:
1. Resolve `io` as a module
2. Resolve `io.println` as a function
3. Get its return type (Unit)
4. Infer `main`'s return type from that

## Current Registration

```rust
type_checker.env().define(
    "io.println",           // Registered as "io.println"
    Type::Function { ... }
);
```

## Possible Issues

### 1. Method Call vs Property Access

The expression `io.println("...")` might be parsed as:
- Method call: `io.println(...)`
- Property access + call: `(io.println)(...)`

If it's parsed as method call on module, type checker might look for it differently.

### 2. Qualified Name Resolution

Type checker might not be looking up "io.println" as a single identifier.
It might be:
1. Looking up "io" → gets Module type
2. Looking for "println" method on Module type
3. Not finding it because we registered "io.println" as a string key

### 3. Scope Issues

The registration happens in the type environment, but method calls on modules
might not check the same place.

## Solutions to Try

### Option A: Register as Method on Module Type

Instead of:
```rust
env.define("io.println", Function { ... })
```

Try something like:
```rust
// This might require type checker API changes
module_type.add_method("println", Function { ... })
```

### Option B: Register with Different Keys

Try multiple registration strategies:
```rust
// As qualified name
env.define("io.println", fn_type);

// As just the function name (if imported)
env.define("println", fn_type);

// In a module-specific namespace
// (might need type checker changes)
```

### Option C: Hardcode in Type Checker

Add special handling in the type checker's method resolution for stdlib:

```rust
// In type_checker method_call handling
if module_name == "io" && method == "println" {
    return Type::Unit;
}
```

## Next Steps

1. Add more logging to see how `io.println` calls are being resolved
2. Check what the type checker's method resolution looks like
3. Try Option B (multiple registration keys)
4. If that doesn't work, may need to modify type checker itself

## Workaround

For now, the LSP assumes `-> ()` for no-param functions with TypeVar returns.
This works for `main()` but isn't correct in general.
