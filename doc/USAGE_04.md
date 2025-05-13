Veld Language Reference

## Core Concepts

Veld is a modern, multi-paradigm programming language with eager typing, built for expressiveness and safety. This documentation covers the latest syntax and features.

## Table of Contents
1. [Basic Types](#basic-types)
2. [Variables](#variables)
3. [Functions](#functions)
4. [Control Flow](#control-flow)
5. [Structs and Methods](#structs-and-methods)
6. [Kinds (Interfaces)](#kinds-interfaces)
7. [Structural Typing](#structural-typing)
8. [Operator Overloading](#operator-overloading)
9. [Modules and Imports](#modules-and-imports)
10. [Enums and Pattern Matching](#enums-and-pattern-matching)
11. [Macros](#macros)

## Basic Types

```veld
-- Basic types
let i: i32 = 42
let f: f64 = 3.14
let s: str = "hello"
let b: bool = true
let c: char = 'a'

-- Unit type (similar to void/nil)
let unit_value: () = ()

-- Arrays
let numbers: [i32] = [1, 2, 3, 4, 5]
```

## Variables

```veld
-- Immutable variable (default)
let name = "Alice"

-- Mutable variable
var counter = 0
counter = counter + 1

-- Type annotations (optional when type can be inferred)
let age: i32 = 30
let height: f64 = 175.5
```

## Functions

Veld offers multiple ways to declare functions:

### Standard Function Declaration

```veld
-- Multi-line function with implicit return
fn add(a: i32, b: i32) -> i32
    a + b
end

-- Multi-line function with optional 'do' keyword
fn calculate(x: i32, y: i32) -> i32 do
    let temp = x * 2
    temp + y
end

-- Single-expression function with fat arrow
fn square(x: i32) -> i32 => x * x

-- Void function (returns unit)
fn log(message: str) -> ()
    println~("LOG: {}", message)
end

-- Procedure shorthand (implicit unit return)
proc greet(name: str)
    println~("Hello, {}!", name)
end

-- First-class functions with type annotations
let multiply: (i32, i32) -> i32 = fn(a, b) => a * b

-- Lambda expressions
let add_one = x => x + 1

-- Multi-line lambda
let process = data => do
    let result = transform(data)
    filter(result)
end

-- Anonymous block (proc lambda)
let setup = do
    initialize_resources()
    configure_settings()
    log("Setup complete")
end
```

## Control Flow

```veld
-- If expression
let max = if a > b then a else b end

-- Multi-line if statement
if user.is_admin() then
    grant_access()
    log_access()
else if user.has_permission("read") then
    grant_read_only()
else
    deny_access()
end

-- While loop
while connection.is_active() do
    process_next_message()
end

-- For loop (iteration)
for item in collection do
    process(item)
end

-- For loop with index
for item, index in collection.with_index() do
    println~("Item {} at index {}", item, index)
end

-- For with range
for i in 0..10 do
    println~("{}", i)  -- Prints 0 to 9
end

-- Pipeline operator for chaining operations
let result = data
    |> transform
    |> validate
    |> process
```

## Structs and Methods

```veld
-- Basic struct with fields
struct Point do
    x: f64,
    y: f64,
end

-- Single-line tuple-style struct
struct Color(r: u8, g: u8, b: u8)

-- Struct with methods
struct Rectangle
    width: f64,
    height: f64,

    -- Method with arrow syntax
    fn area(self) -> f64 => self.width * self.height

    -- Method with block syntax and optional 'do'
    fn scale(self, factor: f64) -> Rectangle do
        Rectangle(
            width: self.width * factor,
            height: self.height * factor
        )
    end
end

-- Creating and using structs
let p = Point(x: 10.0, y: 20.0)
let c = Color(255, 0, 0)  -- Positional arguments also work
let r = Rectangle(width: 5.0, height: 10.0)

let area = r.area()  -- 50.0
let scaled = r.scale(2.0)

-- Preventing structural type conformance with attributes
@no_conform
struct SpecialPoint
    x: f64,
    y: f64,

    fn distance(self) -> f64 => (self.x * self.x + self.y * self.y).sqrt()
end

-- Preventing specific kind conformance
@no_conform(Printable)
struct PrivateData
    content: str,

    fn to_string(self) -> str => "<redacted>"
end
```

## Kinds (Interfaces)

```veld
-- Basic kind definition
kind Shape
    fn area(self) -> f64
    fn perimeter(self) -> f64
end

-- Kind with default implementations
kind Comparable
    fn compare(self, other: Self) -> i32

    fn less_than(self, other: Self) -> bool do
        self.compare(other) < 0
    end

    fn greater_than(self, other: Self) -> bool do
        self.compare(other) > 0
    end
end

-- Implementing a kind with arrow syntax
struct Circle
    radius: f64,
end

impl Circle <- Shape
    fn area(self) -> f64 => 3.14 * self.radius * self.radius
    fn perimeter(self) -> f64 => 2.0 * 3.14 * self.radius
end

-- Implementing a kind with 'for' syntax
struct Square
    side: f64,
end

impl Shape for Square
    fn area(self) -> f64 => self.side * self.side
    fn perimeter(self) -> f64 => 4.0 * self.side
end

-- Using kinds as type constraints
fn print_shape_info(s: Shape)
    println~("Area: {}", s.area())
    println~("Perimeter: {}", s.perimeter())
end

-- Using the functions
let circle = Circle(radius: 5.0)
let square = Square(side: 4.0)

print_shape_info(circle)
print_shape_info(square)
```

## Structural Typing

```veld
-- A kind definition
kind Printable
    fn to_string(self) -> str
end

-- A struct that happens to have a compatible method
struct Person
    name: str,
    age: i32,

     fn to_string(self) -> str => format~("{} ({})", self.name, self.age)
end

-- Function taking the kind as parameter
fn display(item: Printable)
    println(item.to_string())
end

-- Usage - Person is automatically treated as Printable
let person = Person(name: "Alice", age: 30)
display(person)  -- Works without explicit implementation

-- Opting out of structural typing
@no_conform
struct PrivateData
    value: str,

    fn to_string(self) -> str => "<hidden>"
end

let data = PrivateData(value: "secret")
display(data)  -- Error: PrivateData does not implement Printable

-- Opting out of specific kind conformance
@no_conform(Comparable)
struct UncomparableItem
    id: i32,

    fn compare(self, other: Self) -> i32 => self.id - other.id
end
```

## Operator Overloading

```veld
-- Import operator kinds from standard library
import std.ops.{Add, Sub, Mul, Div, Neg}

-- Complex number implementation
struct Complex
    real: f64,
    imag: f64,
end

-- Addition with arrow syntax
impl Complex <- Add<Complex, Output = Complex>
    fn add(self, rhs: Complex) -> Complex => Complex(
        real: self.real + rhs.real,
        imag: self.imag + rhs.imag
    )
end

-- Multiplication with 'for' syntax
impl Mul<Complex, Output = Complex> for Complex
    fn mul(self, rhs: Complex) -> Complex do
        Complex(
            real: self.real * rhs.real - self.imag * rhs.imag,
            imag: self.real * rhs.imag + self.imag * rhs.real
        )
    end
end

-- Negation (unary minus)
impl Complex <- Neg<Output = Complex>
    fn neg(self) -> Complex => Complex(
        real: -self.real,
        imag: -self.imag
    )
end

-- Vector type with scalar multiplication
struct Vector2
    x: f64,
    y: f64,
end

-- Vector addition
impl Vector2 <- Add<Vector2, Output = Vector2>
    fn add(self, rhs: Vector2) -> Vector2 => Vector2(
        x: self.x + rhs.x,
        y: self.y + rhs.y
    )
end

-- Scalar multiplication
impl Mul<f64, Output = Vector2> for Vector2
    fn mul(self, scalar: f64) -> Vector2 => Vector2(
        x: self.x * scalar,
        y: self.y * scalar
    )
end

-- Usage of operators
let a = Complex(real: 1.0, imag: 2.0)
let b = Complex(real: 3.0, imag: 4.0)

let sum = a + b        -- Complex(real: 4.0, imag: 6.0)
let product = a * b    -- Complex(real: -5.0, imag: 10.0)
let negated = -a       -- Complex(real: -1.0, imag: -2.0)

let v = Vector2(x: 3.0, y: 4.0)
let scaled = v * 2.0   -- Vector2(x: 6.0, y: 8.0)
```

## Modules and Imports

```veld
-- Defining a module
mod math
    -- Public function
    pub fn add(a: i32, b: i32) -> i32 => a + b

    -- Private function (default)
    fn helper() -> i32 => 42

    -- Public struct
    pub struct Vector2
        x: f64,
        y: f64,

        pub fn length(self) -> f64 => (self.x * self.x + self.y * self.y).sqrt()
    end
end

-- Simple import
import math

let sum = math.add(1, 2)

-- Importing specific items
import math.{add, Vector2}

let result = add(3, 4)
let vec = Vector2(x: 1.0, y: 2.0)

-- Import with alias
import math as m

let sum2 = m.add(5, 6)

-- Importing all items
import math.*

-- Importing from nested modules
import std.collections.{List, Map}
import std.io.{read_file, write_file}

-- Re-exporting items
mod geometry
    import math.Vector2
    pub(Vector2)  -- Re-export Vector2

    pub struct Rectangle
        width: f64,
        height: f64,
    end
end
```

## Enums and Pattern Matching

```veld
-- Basic enum
enum Direction(North, South, East, West)

-- Enum with associated data
enum Shape
    Circle(f64),              -- radius
    Rectangle(f64, f64),      -- width, height
    Triangle(f64, f64, f64),  -- sides
end

-- Generic enum
enum Result<T, E>
    Ok(T),
    Err(E),
end

-- Pattern matching
let shape = Shape.Circle(5.0)

let area = match shape
    Shape.Circle(radius) => 3.14 * radius * radius,
    Shape.Rectangle(w, h) => w * h,
    Shape.Triangle(a, b, c) do
        let s = (a + b + c) / 2.0
        (s * (s - a) * (s - b) * (s - c)).sqrt()
    end,
end

-- Pattern matching with guards
let result: Result<i32, str> = Result.Ok(42)

match result
    Result.Ok(value) where value > 0 => println~("Positive: {}", value),
    Result.Ok(value) => println~("Non-positive: {}", value),
    Result.Err(msg) => println~("Error: {}", msg),
end

-- Using enums for state machines
enum ConnectionState
    Disconnected,
    Connecting,
    Connected(str),  -- IP address
    Failed(str),     -- Error message
end

let state = ConnectionState.Connected("192.168.1.1")

match state
    ConnectionState.Connected(ip) => println~("Connected to {}", ip),
    ConnectionState.Disconnected => connect(),
    ConnectionState.Connecting => wait(),
    ConnectionState.Failed(error) => report_error(error),
end
```

## Macros

```veld
-- Declarative macro
macro~ vec
    () => new_vec(),
    ($elem:expr) => do
        let mut temp = new_vec()
        temp.push($elem)
        temp
    end,
    ($($elem:expr),+ $(,)?) => do
        let mut temp = new_vec()
        $(
            temp.push($elem)
        )+
        temp
    end
end

-- Using a macro
let numbers = vec~(1, 2, 3, 4)

-- Format macro
let message = format~("Hello, {}!", name)

-- Chainable macros
let result = data
    ~debug         -- Print debug info
    ~validate      -- Validate data
    ~transform     -- Transform data

-- HTML-like DSL macro
let content = html~(
    <div class="container">
        <h1>{title}</h1>
        <p>{body}</p>
        <ul>
            {items.map(item => <li>{item}</li>)}
        </ul>
    </div>
)

-- SQL query macro
let users = sql~(
    SELECT name, email
    FROM users
    WHERE active = true
    ORDER BY name ASC
)
```

## Advanced Features

### Pipeline Operator

```veld
-- Processing data with pipeline operator
let result = data
    |> parse
    |> validate
    |> transform
    |> format

-- With arguments
let processed = data
    |> filter(x => x > 0)
    |> map(x => x * 2)
    |> reduce((acc, x) => acc + x)
```

### Concurrency

```veld
-- Spawning a task
let handle = spawn fn()
    perform_work()
end

-- Awaiting async functions
fn fetch_data() async -> Data
    let response = http.get("https://api.example.com/data").await
    parse_data(response)
end

-- Using channels
let (sender, receiver) = channel<Message>()

spawn fn()
    for i in 1..10 do
        sender.send(Message(id: i))
    end
end

for msg in receiver do
    process(msg)
end
```

### Type Inference and Generics

```veld
-- Generic function
fn map<T, U>(value: T, f: fn(T) -> U) -> U
    f(value)
end

-- Generic struct
struct Pair<A, B>
    first: A,
    second: B,
end

-- Generic function with constraints
fn sum<T: Number>(list: List<T>) -> T
    list.reduce((a, b) => a + b)
end

-- Multiple constraints
fn print_and_compare<T: Display + Eq>(a: T, b: T) -> bool
    println~("{} vs {}", a, b)
    a == b
end

-- Generics with where clause
fn merge<T, U, V>(a: T, b: U) -> V
where
    T: Convertible<V>,
    U: Convertible<V>
do
    a.convert() + b.convert()
end

-- Associated types
kind Iterator
    type Item
    fn next(self) -> Option<Self.Item>
end

impl List<T>: Iterator
    type Item = T

    fn next(self) -> Option<T>
        -- implementation
    end
end
```
