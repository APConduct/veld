# Veld Language Reference and Examples

## Function Declarations

There are multiple equivalent ways to declare functions in Veld:

### Standard Function Declaration
```veld
fn add(a: i32, b: i32) -> i32
    a + b
end
```

### Function as a Value
```veld
let add = fn(a: i32, b: i32) -> i32
    a + b
end
```

### First-class Function with Type Annotation
```veld
let add: (i32, i32) -> i32 = fn(a, b)
    a + b
end
```

### Single-line Function (Demi-lambda)
```veld
fn add(a, b) a + b end -- Type inference available
```

### Lambda Function
```veld
let add = a, b => a + b     -- Type inference available, completes after expression
```

### Multi-line Lambda Function
```veld
let add = a, b => do    -- Starts a block to be the expression, finishes with end
    let result = a + b
    result
end
```

### Multi-line Function Example
```veld
fn complex_function(x: i32) -> i32
    let temp = something()
    temp + x        -- Implicit return
end
```

## Types

### Structs

#### Traditional Struct Declaration
```veld
struct Point
    x: f64,
    y: f64,
end
```

#### Single-line Struct (Tuple-style)
```veld
struct Point(x: f64, y: f64)
```

### Kinds (Traits/Interfaces)

#### Basic Kind Declaration
```veld
kind Shape
    fn area(self)  -- No default implementation
end
```

#### Multi-line Kind
```veld
kind Shape
    fn area(self) -> f64
    fn perimeter(self) -> f64
end
```

#### Single-line Kind
```veld
kind Printable = fn print(self) -> str;     -- if a type has a matching function, it implements the kind
```

### Implementations

#### Single-line Implementation
```veld
impl Point fn distance(self) -> f64 (self.x * self.x + self.y * self.y).sqrt() end
```

#### Multi-line Implementation
```veld
impl Circle
    fn area(self) -> f64
        3.14 * self.radius * self.radius
    end
end
```

### Tables (Hash Maps)
```veld
var foo = {
    name: "John",
    age: 30,
    email: "john@example.com"
}
```

### Enums

#### Multi-line Enum
```veld
enum Color
    Red,
    Green,
    Blue,
end
```

#### Single-line Enum
```veld
enum Direction(North, South, East, West)
```

## Macros

### Macro Declarations

#### Basic Declarative Macro
```veld
@~macro println(fmt, args...) =     -- EXPERIMENTAL/ NOT FINALIZED
    -- macro implementation
    format_and_print(fmt, args)
end
```

#### Pattern Matching Macro
```veld
@~macro vec                         -- EXPERIMENTAL/ NOT FINALIZED
    () => new_vec(),
    ($elem:expr) => {
        let mut temp = new_vec();
        temp.push($elem);
        temp
    },
    ($($elem:expr),+ $(,)?) => {
        let mut temp = new_vec();
        $(
            temp.push($elem);
        )+
        temp
    }
end
```

#### Procedural Macro
```veld
@comptime
fn derive(trait_name) =
    match trait_name
        "Debug" => generate_debug_impl(),
        "Clone" => generate_clone_impl(),
        _ => error~("Unknown trait: {}", trait_name)
    end
end
```

### Macro Usage

#### Basic Macro Calls
```veld
println~("Hello {}!", "World");
debug~(some_value);
format~("Value: {}", 42);
```

#### Chainable Macros
```veld
some_value
    ~debug                      -- prints debug info
    ~validate                   -- validates the value
    ~transform(x => x * 2);     -- transforms the value
```

## Generics

### Generic Functions

#### Basic Generic Function
```veld
fn map<T, U>(value: T, f: fn(T) -> U) -> U
    f(value)
end
```

#### Generic Function with Constraints
```veld
fn sum<T: Number>(list: List<T>) -> T
    list.reduce((a, b) => a + b)
end
```

#### Multiple Constraints
```veld
fn print_and_compare<T: Display + Eq>(a: T, b: T) -> bool
    println~("{} vs {}", a, b)
    a == b
end
```

### Generic Types

#### Generic Struct with Constraints
```veld
struct SortedList<T: Ord>
    items: List<T>
impl
    fn add(self, item: T)
        self.items.push(item);
        self.items.sort()
    end
end
```

#### Generic Kind
```veld
kind Container<T>
    fn get(self) -> T
    fn set(self, value: T)
end
```

#### Generic Kind with Associated Types
```veld
kind Iterator
    type Item
    fn next(self) -> Option<Self.Item>
    fn map<U>(self, f: fn(Self.Item) -> U) -> MapIterator<Self, U>
end
```
