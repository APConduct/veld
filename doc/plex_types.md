# Plex Types in Veld

Plex types provide structural typing capabilities in Veld, similar to TypeScript's interface system. Unlike Veld's nominal struct types, plex types are compared structurally - two plex types are compatible if they have the same field names with compatible types, regardless of the type alias name.

## Basic Syntax

### Plex Type Declaration

```veld
plex Point = { x: f64, y: f64 }
plex Person = { name: str, age: i32 }
```

### Using Plex Types

```veld
# Type annotation with plex type
let origin: Point = { x: 0.0, y: 0.0 }

# Direct record type annotation
let person: { name: str, age: i32 } = { name: "Alice", age: 25 }
```

## Automatic Type Coercion

Plex types support automatic safe widening conversions to improve ergonomics while maintaining type safety.

### Safe Widening Rules

The following conversions are considered "safe" and will be performed automatically:

#### Integer Widening
- `i8` → `i16`, `i32`, `i64`
- `i16` → `i32`, `i64`  
- `i32` → `i64`
- `u8` → `u16`, `u32`, `u64`
- `u16` → `u32`, `u64`
- `u32` → `u64`

#### Integer to Float (Safe Precision)
- `i8`, `i16`, `i32` → `f32`, `f64`
- `u8`, `u16`, `u32` → `f32`, `f64`
- `i64`, `u64` → `f64` (f64 has 53 bits of precision)

#### Float Widening
- `f32` → `f64`

### Automatic Coercion Examples

```veld
plex Point = { x: f64, y: f64 }

# These work automatically (safe widening)
let p1: Point = { x: 10, y: 20 }           # i32 → f64
let p2: Point = { x: 10.0, y: 20.0 }       # f32 → f64 (if f32 default)

plex MixedData = { count: i64, ratio: f64 }
let data: MixedData = { count: 42, ratio: 3.14 }  # i32 → i64, f32 → f64
```

### Nested Record Coercion

Coercion works recursively through nested records:

```veld
plex Location = { x: f64, y: f64 }
plex Entity = { name: str, position: Location }

let entity: Entity = {
    name: "Player",
    position: { x: 100, y: 200 }  # i32 → f64 in nested record
}
```

## Explicit Casting

When automatic coercion isn't sufficient, use explicit casting:

### Field-Level Casting

```veld
plex Point = { x: f64, y: f64 }

# Explicit casting for clarity or when types are ambiguous
let point: Point = { x: 10.0 as f64, y: 20.0 as f64 }
```

### Record-Level Casting

```veld
# Cast the entire record when needed
let point: Point = { x: 10.0, y: 20.0 } as Point

# Useful for type assertions
let some_record = get_record_from_somewhere()
let typed_record: Point = some_record as Point
```

## Limitations and Non-Coercible Conversions

The following conversions require explicit casting as they may lose data:

### Narrowing Conversions (Explicit Cast Required)
- `i64` → `i32`, `i16`, `i8`
- `f64` → `f32`
- `f64`, `f32` → any integer type
- Between signed and unsigned types of same size

### Examples Requiring Explicit Casting

```veld
plex Config = { width: i32, height: i32 }

# This would require explicit casting:
# let config: Config = { width: 1920i64, height: 1080i64 }  # Error!

# Instead, use explicit casting:
let config: Config = { 
    width: 1920i64 as i32, 
    height: 1080i64 as i32 
}
```

## Function Parameters and Return Types

Plex types work seamlessly with functions:

```veld
plex Point = { x: f64, y: f64 }

fn distance(p1: Point, p2: Point) -> f64
    let dx = p2.x - p1.x
    let dy = p2.y - p1.y
    dx * dx + dy * dy
end

# Automatic coercion in function calls
let dist = distance(
    { x: 0, y: 0 },     # i32 → f64 coercion
    { x: 3, y: 4 }      # i32 → f64 coercion
)

fn create_point(x: i32, y: i32) -> Point
    { x: x, y: y }  # i32 → f64 coercion in return
end
```

## Generics with Plex Types

Plex types support generic parameters:

```veld
plex Container<T> = { items: [T], count: i32 }
plex Result<T, E> = { ok: Option<T>, err: Option<E> }

let numbers: Container<i32> = {
    items: [1, 2, 3],
    count: 3
}

let result: Result<str, str> = {
    ok: Option.Some("success"),
    err: Option.None
}
```

## Structural vs Nominal Typing

### Plex Types (Structural)

```veld
plex Point2D = { x: f64, y: f64 }
plex Coordinate = { x: f64, y: f64 }

# These are compatible - same structure
let point: Point2D = { x: 1.0, y: 2.0 }
let coord: Coordinate = point  # Works! Structural compatibility
```

### Struct Types (Nominal)

```veld
struct Point2D
    x: f64,
    y: f64
end

struct Coordinate  
    x: f64,
    y: f64
end

# These are NOT compatible - different names
let point = Point2D { x: 1.0, y: 2.0 }
# let coord: Coordinate = point  # Error! Nominal typing
```

## Best Practices

1. **Use plex for data shapes**: When you care about the structure, not the type name
2. **Use struct for domain objects**: When you want distinct types with methods
3. **Leverage auto-coercion**: Let the compiler handle safe widening conversions
4. **Be explicit when needed**: Use `as` casting for clarity or when coercion fails
5. **Design for composition**: Plex types compose well due to structural typing

## Error Messages

When coercion fails, you'll see helpful error messages:

```veld
plex Point = { x: f32, y: f32 }

# This would fail:
# let point: Point = { x: 1.0f64, y: 2.0f64 }
# Error: Cannot safely coerce field 'x' from f64 to f32

# Solution - explicit cast:
let point: Point = { x: 1.0f64 as f32, y: 2.0f64 as f32 }
```

## Implementation Notes

- Automatic coercion happens at variable assignment time
- Type checking validates that coercion is safe before attempting it  
- Runtime coercion uses the existing `cast_value` infrastructure
- Field order in records doesn't matter for compatibility
- Empty records `{}` are valid plex types