## Core Types & Unit Type

```veld
-- Basic types
let i: i32 = 42;
let f: f64 = 3.14;
let s: str = "hello";
let b: bool = true;

-- Unit type (similar to void/nil but type-safe)
let unit_value: () = ();

-- Function returning unit (proc)
fn log_message(msg: str) -> () =
    println~("LOG: {}", msg)
end

-- Shorthand for procedures
proc greet(name: str) =
    println~("Hello, {}!", name)
end
```

## Functions and Lambdas

```veld
-- Standard function
fn add(a: i32, b: i32) -> i32 =
    a + b
end

-- Single-line function
fn multiply(a: i32, b: i32) -> i32 = a * b;

-- Anonymous function
let div = fn(a: i32, b: i32) -> i32 =
    if b == 0
        error~("Division by zero")
    else
        a / b
    end
end

-- Lambda expression
let square = x => x * x;

-- Function with generic parameters
fn map<T, U>(value: T, f: fn(T) -> U) -> U =
    f(value)
end

-- Function with where clause
fn sort<T>(list: List<T>) -> List<T> where T: Comparable =
    -- implementation
end
```

## Control Flow

```veld
-- If expression
let max = if a > b then a else b;

-- Multi-line if statement
if user.is_admin()
    grant_access()
    log_access()
else if user.has_permission("read")
    grant_read_only()
else
    deny_access()
end

-- Pattern matching with guards
match value
    Person{name, age} where age >= 18 => handle_adult(name),
    Person{name, age} => handle_minor(name, age),
    _ => handle_unknown()
end

-- While loop
while connection.is_active()
    process_next_message()
end

-- For loop (iteration)
for item in collection
    process(item)
end

-- For loop with index
for item, index in collection.with_index()
    println~("Item {} at index {}", item, index)
end

-- Infinite loop
for
    if should_exit()
        break
    end
    do_work()
end

-- For with range
for i in 0..10
    println~("{}", i)  -- Prints 0 to 9
end
```

## Data Structures

```veld
-- Struct declaration
struct Point
    x: f64,
    y: f64,
end

-- Single-line struct
struct Color(r: u8, g: u8, b: u8);

-- Struct with methods
struct Rectangle
    width: f64,
    height: f64,
impl
    fn area(self) -> f64 = self.width * self.height;
    fn scale(self, factor: f64) -> Rectangle =
        Rectangle{
            width: self.width * factor,
            height: self.height * factor,
        }
    end
end

-- Enum type
enum Result<T, E>
    Ok(T),
    Err(E),
end

-- Using enum
let result: Result<i32, str> = Ok(42);

-- Pattern matching on enum
match result
    Ok(value) => println~("Success: {}", value),
    Err(message) => println~("Error: {}", message),
end

-- Table (hash map)
let config = {
    host: "localhost",
    port: 8080,
    debug: true,
}
```

## Kinds and Implementations

```veld
-- Kind declaration (interface)
kind Serializable
    fn serialize(self) -> [u8]
    fn deserialize(data: [u8]) -> Self
end

-- Explicit implementation
struct User
    id: u64,
    name: str,
end

impl User: Serializable
    fn serialize(self) -> [u8] =
        -- implementation
    end

    fn deserialize(data: [u8]) -> User =
        -- implementation
    end
end

-- Implicit (structural) implementation
struct Message
    content: str,
    timestamp: u64,

    -- Methods defined inline satisfy kind requirements
    fn serialize(self) -> [u8] =
        -- implementation
    end

    fn deserialize(data: [u8]) -> Message =
        -- implementation
    end
end

-- Kind with default implementation
kind Container<T>
    fn add(self, item: T)
    fn contains(self, item: T) -> bool
    fn size(self) -> usize

    -- Default method using other required methods
    fn is_empty(self) -> bool = self.size() == 0
end
```

## Velvet Annotations

```veld
-- Function annotation
@deprecated("Use new_function instead")
fn old_function() -> i32 = 42;

-- Implementation optimization hints
@optimize(inline)
impl Vector: Numeric
    @simd
    fn dot_product(self, other: Vector) -> f64 =
        -- optimized implementation
    end
end

-- Attribute for exhaustive match checking
@exhaustive
match value
    Case1 => handle_case1(),
    Case2 => handle_case2(),
    -- Compiler error if any case is missing
end

-- Conditional compilation
@when(target = "wasm")
fn web_specific() =
    -- WASM-specific code
end

-- Derive attributes for auto-implementation
@derive(Debug, Clone, Serialize)
struct Config
    name: str,
    version: str,
end
```

## Macros

```veld
-- Declarative macro
@~macro vec
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

-- Using a macro
let numbers = vec~(1, 2, 3, 4);

-- Macro with infix notation
let html = html~(
    <div>
        <h1>{title}</h1>
        <p>{content}</p>
    </div>
);

-- Postfix macro for method-like calls
data
    ~trace               -- print debug info
    ~transform(x => x*2)  -- apply transformation
    ~validate            -- validate result
```

## Generics and Type Constraints

```veld
-- Generic struct
struct Pair<A, B>
    first: A,
    second: B,
end

-- Generic function with constraints
fn find<T, U>(haystack: T, needle: U) -> bool
where
    T: Container<U>,
    U: Equatable
=
    for item in haystack
        if item == needle
            return true
        end
    end
    false
end

-- Associated types
kind Iterator
    type Item
    fn next(self) -> Option<Self.Item>
end

-- Implementing kind with associated type
impl List<T>: Iterator
    type Item = T

    fn next(self) -> Option<T> =
        -- implementation
    end
end

-- Multiple constraints
fn print_all<T>(items: List<T>) where T: ToString + Display =
    for item in items
        println~("{}", item)
    end
end
```

## Error Handling

```veld
-- Using Result enum
fn divide(a: i32, b: i32) -> Result<i32, str> =
    if b == 0
        Err("Division by zero")
    else
        Ok(a / b)
    end
end

-- Error propagation with ? operator
fn calculate() -> Result<i32, str> =
    let x = divide(10, 2)?;  -- Returns error if divide fails
    let y = divide(20, x)?;  -- Same here
    Ok(y + 5)
end

-- Try-catch like pattern
fn safe_operation() =
    try
        risky_function()
    catch e: IOException
        handle_io_error(e)
    catch e: ValueError
        handle_value_error(e)
    finally
        cleanup()
    end
end

-- With statement for resource management
fn process_file(path: str) =
    with file = File.open(path) do
        -- file is automatically closed at end of block
        process(file.read_all())
    end
end
```

## Concurrency

```veld
-- Spawning a task
let handle = spawn fn() =
    perform_work()
end

-- Awaiting async functions
fn fetch_data() async -> Data =
    let response = http.get("https://api.example.com/data").await;
    parse_data(response)
end

-- Using channels
let (sender, receiver) = channel::<Message>();

spawn fn() =
    for i in 1..10
        sender.send(Message{id: i})
    end
end

for msg in receiver
    process(msg)
end
```

## Metaprogramming

```veld
-- Compile-time reflection
fn generate_fields<T>() -> [str] =
    @reflect(T).fields().map(f => f.name)
end

-- Runtime type checking
fn process(value: any) =
    if value is Serializable
        let bytes = value.serialize()
        -- process bytes
    else
        error~("Value must be Serializable")
    end
end
```
