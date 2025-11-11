# Veld Standard Library - New Module Proposals

This document contains concrete, ready-to-implement proposals for new stdlib modules and APIs.

---

## Proposal 1: Path Manipulation Module (`stdlib/path/mod.veld`)

**Priority:** HIGH  
**Effort:** Medium (1-2 days)  
**Dependencies:** None

### Rationale
Every program that works with files needs path manipulation. Currently users must do string concatenation, which is error-prone and platform-dependent.

### API Design

```veld
#| Path manipulation utilities
#| Platform-independent path operations

pub struct Path
    inner: str
end

impl Path
    #| Create a new path from a string
    pub fn new(path: str) -> Path
        Path(inner: path)
    end

    #| Join this path with another component
    #| Example: Path.new("/home/user").join("file.txt") => "/home/user/file.txt"
    pub fn join(self, other: str) -> Path
        # Native implementation handles platform-specific separators
    end

    #| Get the parent directory
    #| Example: Path.new("/home/user/file.txt").parent() => Some("/home/user")
    pub fn parent(self) -> Option<Path>
    end

    #| Get the file name (last component)
    #| Example: Path.new("/home/user/file.txt").file_name() => Some("file.txt")
    pub fn file_name(self) -> Option<str>
    end

    #| Get the file extension
    #| Example: Path.new("file.txt").extension() => Some("txt")
    pub fn extension(self) -> Option<str>
    end

    #| Get the file stem (name without extension)
    #| Example: Path.new("file.txt").stem() => Some("file")
    pub fn stem(self) -> Option<str>
    end

    #| Check if path is absolute
    pub fn is_absolute(self) -> bool
    end

    #| Check if path is relative
    pub fn is_relative(self) -> bool
    end

    #| Check if path exists in filesystem
    pub fn exists(self) -> bool
    end

    #| Check if path is a file
    pub fn is_file(self) -> bool
    end

    #| Check if path is a directory
    pub fn is_dir(self) -> bool
    end

    #| Convert to string
    pub fn to_str(self) -> str
        self.inner
    end
end

#| Convenience function to create a path
pub fn path(p: str) -> Path
    Path.new(p)
end
```

### Native Implementation (Rust)

```rust
// In interpreter.rs
fn register_path_methods(&mut self) {
    use std::path::Path as StdPath;

    // Path.join
    self.native_method_registry.register("Path", "join", |_, args| {
        if let (Some(Value::Struct { fields, .. }), Some(Value::String(other))) = 
            (args.get(0), args.get(1)) 
        {
            let inner = fields.get("inner")
                .and_then(|v| if let Value::String(s) = v { Some(s) } else { None })
                .ok_or_else(|| VeldError::RuntimeError("Invalid Path".into()))?;
            
            let path = StdPath::new(inner).join(other);
            let path_str = path.to_string_lossy().to_string();
            
            Ok(Value::Struct {
                name: "Path".to_string(),
                fields: [("inner".to_string(), Value::String(path_str))].into(),
            })
        } else {
            Err(VeldError::RuntimeError("Path.join expects (Path, str)".into()))
        }
    });

    // Path.parent
    self.native_method_registry.register("Path", "parent", |_, args| {
        if let Some(Value::Struct { fields, .. }) = args.get(0) {
            let inner = fields.get("inner")
                .and_then(|v| if let Value::String(s) = v { Some(s) } else { None })
                .ok_or_else(|| VeldError::RuntimeError("Invalid Path".into()))?;
            
            let path = StdPath::new(inner);
            if let Some(parent) = path.parent() {
                let parent_str = parent.to_string_lossy().to_string();
                Ok(Value::Enum {
                    enum_name: "Option".to_string(),
                    variant_name: "Some".to_string(),
                    fields: vec![Value::Struct {
                        name: "Path".to_string(),
                        fields: [("inner".to_string(), Value::String(parent_str))].into(),
                    }],
                })
            } else {
                Ok(Value::Enum {
                    enum_name: "Option".to_string(),
                    variant_name: "None".to_string(),
                    fields: vec![],
                })
            }
        } else {
            Err(VeldError::RuntimeError("Path.parent expects Path".into()))
        }
    });

    // ... similar for other methods
}
```

### Example Usage

```veld
import std.path.{Path, path}
import std.io.{println}

pub fn main() -> ()
    let p = path("/home/user/documents")
    let file = p.join("report.pdf")
    
    println("Full path: " + file.to_str())
    
    match file.parent()
        Option.Some(parent) => println("Parent: " + parent.to_str()),
        Option.None => println("No parent"),
    end
    
    match file.extension()
        Option.Some(ext) => println("Extension: " + ext),
        Option.None => println("No extension"),
    end
end
```

---

## Proposal 2: Enhanced Error Types (`stdlib/error.veld`)

**Priority:** CRITICAL  
**Effort:** Small (1 day)  
**Dependencies:** Result type

### Rationale
Currently I/O and FS functions panic on errors. Production code needs proper error handling with context.

### API Design

```veld
#| Standard error types for Veld

#| I/O error kinds
pub enum IoErrorKind
    NotFound,
    PermissionDenied,
    ConnectionRefused,
    ConnectionReset,
    ConnectionAborted,
    NotConnected,
    AddrInUse,
    AddrNotAvailable,
    BrokenPipe,
    AlreadyExists,
    WouldBlock,
    InvalidInput,
    InvalidData,
    TimedOut,
    WriteZero,
    Interrupted,
    UnexpectedEof,
    Other,
end

#| I/O error with message and kind
pub struct IoError
    kind: IoErrorKind,
    message: str,
end

impl IoError
    pub fn new(kind: IoErrorKind, message: str) -> IoError
        IoError(kind: kind, message: message)
    end

    pub fn kind(self) -> IoErrorKind
        self.kind
    end

    pub fn message(self) -> str
        self.message
    end

    pub fn is_not_found(self) -> bool
        match self.kind
            IoErrorKind.NotFound => true,
            _ => false,
        end
    end

    pub fn is_permission_denied(self) -> bool
        match self.kind
            IoErrorKind.PermissionDenied => true,
            _ => false,
        end
    end
end

impl IoError <- ToStr
    pub fn to_str(self) -> str
        "IoError: " + self.message
    end
end

#| Result type alias for I/O operations
pub type IoResult<T> = Result<T, IoError>

#| Parse error for string conversions
pub struct ParseError
    message: str,
    input: str,
end

impl ParseError
    pub fn new(message: str, input: str) -> ParseError
        ParseError(message: message, input: input)
    end
end

impl ParseError <- ToStr
    pub fn to_str(self) -> str
        "ParseError: " + self.message + " (input: '" + self.input + "')"
    end
end

#| Result type for parsing operations
pub type ParseResult<T> = Result<T, ParseError>
```

### Updated I/O Module

```veld
#| stdlib/io/mod.veld - Updated to use proper error handling
import std.error.{IoError, IoErrorKind, IoResult}

#| Read a file's contents as a string
#| Returns an error if the file doesn't exist or can't be read
pub fn read_file(path: str) -> IoResult<str>
end

#| Write content to a file
#| Returns an error if the file can't be written
pub fn write_file(path: str, content: str) -> IoResult<()>
end

#| Check if a file exists
pub fn file_exists(path: str) -> bool
end

#| Read a line from standard input
#| Returns an error if stdin is closed or read fails
pub fn read_line() -> IoResult<str>
end
```

### Example Usage

```veld
import std.io.{read_file}
import std.error.{IoError}

pub fn main() -> ()
    match read_file("config.txt")
        Result.Ok(content) => {
            println("Config loaded: " + content)
        },
        Result.Err(err) => {
            if err.is_not_found() then
                println("Config file not found, using defaults")
            else if err.is_permission_denied() then
                println("Permission denied reading config")
            else
                println("Error: " + err.to_str())
            end
        },
    end
end
```

---

## Proposal 3: Iterator Adapters (`stdlib/iter/mod.veld`)

**Priority:** HIGH  
**Effort:** Medium (2-3 days)  
**Dependencies:** Iterator trait, functional closure support

### Rationale
Functional iteration patterns are essential for modern programming. Currently no way to map/filter/fold.

### API Design

```veld
#| Iterator utilities and adapters
import std.core.{Iterator}
import std.option.{Option}

#| Map adapter - transforms each element
pub struct MapIterator<T, U>
    inner: Iterator<T>,
    func: (T) -> U,
end

impl<T, U> MapIterator<T, U> <- Iterator<U>
    pub fn next(mut self) -> Option<U>
        match self.inner.next()
            Option.Some(value) => Option.Some(self.func(value)),
            Option.None => Option.None,
        end
    end
end

#| Filter adapter - keeps only elements matching predicate
pub struct FilterIterator<T>
    inner: Iterator<T>,
    predicate: (T) -> bool,
end

impl<T> FilterIterator<T> <- Iterator<T>
    pub fn next(mut self) -> Option<T>
        loop
            match self.inner.next()
                Option.Some(value) => {
                    if self.predicate(value) then
                        return Option.Some(value)
                    end
                    # Continue loop
                },
                Option.None => return Option.None,
            end
        end
    end
end

#| Take adapter - takes first n elements
pub struct TakeIterator<T>
    inner: Iterator<T>,
    remaining: i32,
end

impl<T> TakeIterator<T> <- Iterator<T>
    pub fn next(mut self) -> Option<T>
        if self.remaining > 0 then
            self.remaining = self.remaining - 1
            self.inner.next()
        else
            Option.None
        end
    end
end

#| Extension methods for Iterator (when method generics work)
#| For now, provide standalone functions

pub fn map<T, U>(iter: Iterator<T>, f: (T) -> U) -> MapIterator<T, U>
    MapIterator(inner: iter, func: f)
end

pub fn filter<T>(iter: Iterator<T>, pred: (T) -> bool) -> FilterIterator<T>
    FilterIterator(inner: iter, predicate: pred)
end

pub fn take<T>(iter: Iterator<T>, n: i32) -> TakeIterator<T>
    TakeIterator(inner: iter, remaining: n)
end

#| Collect all elements into an array
pub fn collect<T>(iter: Iterator<T>) -> [T]
    var result = []
    loop
        match iter.next()
            Option.Some(value) => result = result.with(value),
            Option.None => break,
        end
    end
    result
end

#| Fold (reduce) an iterator
pub fn fold<T, Acc>(iter: Iterator<T>, init: Acc, f: (Acc, T) -> Acc) -> Acc
    var acc = init
    loop
        match iter.next()
            Option.Some(value) => acc = f(acc, value),
            Option.None => break,
        end
    end
    acc
end

#| Count the number of elements
pub fn count<T>(iter: Iterator<T>) -> i32
    var cnt = 0
    loop
        match iter.next()
            Option.Some(_) => cnt = cnt + 1,
            Option.None => break,
        end
    end
    cnt
end

#| Check if any element matches predicate
pub fn any<T>(iter: Iterator<T>, pred: (T) -> bool) -> bool
    loop
        match iter.next()
            Option.Some(value) => {
                if pred(value) then
                    return true
                end
            },
            Option.None => return false,
        end
    end
end

#| Check if all elements match predicate
pub fn all<T>(iter: Iterator<T>, pred: (T) -> bool) -> bool
    loop
        match iter.next()
            Option.Some(value) => {
                if not pred(value) then
                    return false
                end
            },
            Option.None => return true,
        end
    end
end
```

### Example Usage

```veld
import std.range.{Range}
import std.iter.{map, filter, collect, fold}

pub fn main() -> ()
    # Create a range 0..10
    let range = Range.new(0, 10)
    let iter = range.iter()
    
    # Map: multiply by 2
    let doubled = map(iter, fn (x) -> x * 2 end)
    
    # Filter: keep only even numbers
    let evens = filter(doubled, fn (x) -> x % 2 == 0 end)
    
    # Collect to array
    let result = collect(evens)
    
    # Fold: sum all values
    let sum = fold(result.iter(), 0, fn (acc, x) -> acc + x end)
    
    println("Sum: " + sum.to_str())
end
```

---

## Proposal 4: Collections - HashSet (`stdlib/collections/hash_set.veld`)

**Priority:** MEDIUM  
**Effort:** Small (1 day)  
**Dependencies:** HashMap (or native Rust HashSet)

### Rationale
Sets are fundamental. Can be implemented on top of HashMap or as separate native type.

### API Design

```veld
#| Hash-based set implementation
pub struct HashSet<T>
    data: any  # Native Rust HashSet backing
end

impl<T> HashSet<T>
    #| Create a new empty set
    pub fn new() -> HashSet<T>
    end

    #| Insert an element into the set
    #| Returns true if the element was newly inserted
    pub fn insert(mut self, value: T) -> bool
    end

    #| Check if the set contains an element
    pub fn contains(self, value: T) -> bool
    end

    #| Remove an element from the set
    #| Returns true if the element was present
    pub fn remove(mut self, value: T) -> bool
    end

    #| Get the number of elements
    pub fn len(self) -> i32
    end

    #| Check if the set is empty
    pub fn is_empty(self) -> bool
    end

    #| Remove all elements
    pub fn clear(mut self) -> ()
    end

    #| Get all elements as an array
    pub fn to_array(self) -> [T]
    end

    #| Union with another set (elements in either set)
    pub fn union(self, other: HashSet<T>) -> HashSet<T>
    end

    #| Intersection with another set (elements in both sets)
    pub fn intersection(self, other: HashSet<T>) -> HashSet<T>
    end

    #| Difference with another set (elements in this but not other)
    pub fn difference(self, other: HashSet<T>) -> HashSet<T>
    end

    #| Check if this is a subset of another set
    pub fn is_subset(self, other: HashSet<T>) -> bool
    end

    #| Check if this is a superset of another set
    pub fn is_superset(self, other: HashSet<T>) -> bool
    end

    #| Check if sets are disjoint (no common elements)
    pub fn is_disjoint(self, other: HashSet<T>) -> bool
    end
end

#| Convenience function to create a set
pub fn hash_set<T>() -> HashSet<T>
    HashSet.new()
end
```

### Example Usage

```veld
import std.collections.{HashSet}

pub fn main() -> ()
    let mut set1 = HashSet.new()
    set1.insert(1)
    set1.insert(2)
    set1.insert(3)
    
    let mut set2 = HashSet.new()
    set2.insert(2)
    set2.insert(3)
    set2.insert(4)
    
    if set1.contains(2) then
        println("Set contains 2")
    end
    
    let union = set1.union(set2)  # {1, 2, 3, 4}
    let intersection = set1.intersection(set2)  # {2, 3}
    let difference = set1.difference(set2)  # {1}
    
    println("Union size: " + union.len().to_str())
end
```

---

## Proposal 5: String Enhancement (`stdlib/string/mod.veld` - Complete Implementation)

**Priority:** CRITICAL  
**Effort:** Medium (2 days)  
**Dependencies:** Native string methods already exist

### Implementation Strategy

The string kinds are already defined, just need to connect native implementations:

```rust
// In interpreter.rs initialize_string_capabilities()
fn initialize_string_capabilities(&mut self) {
    // Register each method from Transformable kind
    self.native_method_registry.register_string_method("to_upper", |s| s.to_uppercase());
    self.native_method_registry.register_string_method("to_lower", |s| s.to_lowercase());
    self.native_method_registry.register_string_method("trim", |s| s.trim().to_string());
    self.native_method_registry.register_string_method("trim_start", |s| s.trim_start().to_string());
    self.native_method_registry.register_string_method("trim_end", |s| s.trim_end().to_string());

    // Searchable methods
    self.native_method_registry.register_string_bool_method_with_string_param(
        "contains", 
        |s, substr| s.contains(substr)
    );
    self.native_method_registry.register_string_bool_method_with_string_param(
        "starts_with", 
        |s, prefix| s.starts_with(prefix)
    );
    self.native_method_registry.register_string_bool_method_with_string_param(
        "ends_with", 
        |s, suffix| s.ends_with(suffix)
    );

    // ... continue for all methods
}
```

### Add Missing Methods

```veld
#| stdlib/string/mod.veld additions

# Re-export existing kinds
pub import std.string.{Transformable, Searchable, Manipulatable, Parsable}

#| Additional utility functions

#| Join an array of strings with a separator
pub fn join(strings: [str], separator: str) -> str
end

#| Split a string and collect results
pub fn split_collect(s: str, delimiter: str) -> [str]
    s.split(delimiter)
end

#| Format a string with arguments (when format strings are ready)
pub fn format(template: str, args: [any]) -> str
end

#| Check if a string is empty
pub fn is_empty(s: str) -> bool
    s.len() == 0
end

#| Repeat a string n times
pub fn repeat(s: str, n: i32) -> str
end
```

---

## Proposal 6: Time Module (`stdlib/time/mod.veld`)

**Priority:** MEDIUM  
**Effort:** Medium (2-3 days)  
**Dependencies:** None

### API Design

```veld
#| Time and duration utilities

#| A duration of time
pub struct Duration
    seconds: i64,
    nanos: i32,
end

impl Duration
    #| Create a duration from seconds
    pub fn from_secs(secs: i64) -> Duration
    end

    #| Create a duration from milliseconds
    pub fn from_millis(millis: i64) -> Duration
    end

    #| Create a duration from microseconds
    pub fn from_micros(micros: i64) -> Duration
    end

    #| Create a duration from nanoseconds
    pub fn from_nanos(nanos: i64) -> Duration
    end

    #| Get total seconds
    pub fn as_secs(self) -> i64
    end

    #| Get total milliseconds
    pub fn as_millis(self) -> i64
    end

    #| Add two durations
    pub fn add(self, other: Duration) -> Duration
    end

    #| Subtract two durations
    pub fn sub(self, other: Duration) -> Duration
    end

    #| Multiply duration by a scalar
    pub fn mul(self, factor: i32) -> Duration
    end
end

#| Monotonic time measurement (doesn't go backwards)
pub struct Instant
    inner: i64  # Native representation
end

impl Instant
    #| Get the current instant
    pub fn now() -> Instant
    end

    #| Get elapsed time since this instant
    pub fn elapsed(self) -> Duration
    end

    #| Get duration between two instants
    pub fn duration_since(self, earlier: Instant) -> Duration
    end
end

#| System time (wall clock)
pub struct SystemTime
    inner: i64  # Native representation
end

impl SystemTime
    #| Get the current system time
    pub fn now() -> SystemTime
    end

    #| Get duration since Unix epoch
    pub fn duration_since_epoch(self) -> Duration
    end
end

#| Sleep for a duration
pub fn sleep(duration: Duration) -> ()
end
```

### Example Usage

```veld
import std.time.{Instant, Duration, sleep}
import std.io.{println}

pub fn benchmark(f: () -> ()) -> Duration
    let start = Instant.now()
    f()
    start.elapsed()
end

pub fn main() -> ()
    let duration = benchmark(fn () -> {
        # Some expensive operation
        var sum = 0
        var i = 0
        while i < 1000000 do
            sum = sum + i
            i = i + 1
        end
    end)
    
    println("Took: " + duration.as_millis().to_str() + "ms")
    
    # Sleep for 1 second
    sleep(Duration.from_secs(1))
    println("Done sleeping")
end
```

---

## Proposal 7: Process/Environment Module (`stdlib/env/mod.veld`)

**Priority:** LOW  
**Effort:** Small (1 day)  
**Dependencies:** None

### API Design

```veld
#| Environment and process utilities

#| Get an environment variable
pub fn var(name: str) -> Option<str>
end

#| Set an environment variable
pub fn set_var(name: str, value: str) -> ()
end

#| Get all environment variables
pub fn vars() -> [(str, str)]
end

#| Get command line arguments
pub fn args() -> [str]
end

#| Get current working directory
pub fn current_dir() -> Result<str, IoError>
end

#| Change current working directory
pub fn set_current_dir(path: str) -> Result<(), IoError>
end

#| Exit the program with a status code
pub fn exit(code: i32) -> !
end

#| Get the path to the current executable
pub fn current_exe() -> Result<str, IoError>
end
```

---

## Summary of Proposals

| Proposal | Priority | Effort | Impact |
|----------|----------|--------|--------|
| 1. Path Module | HIGH | Medium | Essential for file operations |
| 2. Error Types | CRITICAL | Small | Enables proper error handling |
| 3. Iterator Adapters | HIGH | Medium | Functional programming patterns |
| 4. HashSet | MEDIUM | Small | Common collection type |
| 5. String Enhancement | CRITICAL | Medium | Already designed, just connect |
| 6. Time Module | MEDIUM | Medium | Benchmarking, delays, timing |
| 7. Process/Env | LOW | Small | System interaction |

## Implementation Order

1. **Error Types** (1 day) - Unblocks I/O improvements
2. **String Enhancement** (2 days) - High value, mostly done
3. **Path Module** (2 days) - Essential for file work
4. **HashSet** (1 day) - Quick win
5. **Iterator Adapters** (3 days) - Enables functional style
6. **Time Module** (2 days) - Nice to have
7. **Process/Env** (1 day) - Low priority

**Total effort for all:** ~12 days focused work

---

## Next Steps

1. Review and approve proposals
2. Implement in priority order
3. Write comprehensive tests for each
4. Update documentation
5. Create example programs showcasing new features