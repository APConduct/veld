# std.io Module Documentation

The `std.io` module provides input/output operations for the Veld programming language. This module includes functions for console output, file operations, and standard input reading.

## Table of Contents

- [Overview](#overview)
- [Import Syntax](#import-syntax)
- [Console Output Functions](#console-output-functions)
- [File Operations](#file-operations)
- [Input Functions](#input-functions)
- [Examples](#examples)
- [Error Handling](#error-handling)
- [Implementation Notes](#implementation-notes)

## Overview

The `std.io` module is part of Veld's standard library and provides essential I/O capabilities. All functions in this module are implemented natively in Rust for optimal performance and system integration.

## Import Syntax

You can import the entire module or specific functions:

```veld
# Import the entire module
import std.io

# Import specific functions
import std.io.{print, println}

# Import with alias
import std.io.{println as putline}
```

## Console Output Functions

### `print(text: str) -> ()`

Outputs text to standard output without adding a newline character.

**Parameters:**
- `text`: A string to output

**Return Value:**
- `()` (Unit type)

**Example:**
```veld
std.io.print("Hello")
std.io.print(" World")
# Output: Hello World
```

### `println(text: str) -> ()`

Outputs text to standard output followed by a newline character.

**Parameters:**
- `text`: A string to output

**Return Value:**
- `()` (Unit type)

**Example:**
```veld
std.io.println("First line")
std.io.println("Second line")
# Output:
# First line
# Second line
```

## File Operations

### `read_file(path: str) -> str`

Reads the entire contents of a file as a string.

**Parameters:**
- `path`: Path to the file to read

**Return Value:**
- `str`: The contents of the file

**Errors:**
- Throws a runtime error if the file cannot be read or does not exist

**Example:**
```veld
let content = std.io.read_file("example.txt")
std.io.println(content)
```

### `write_file(path: str, content: str) -> bool`

Writes content to a file, creating it if it doesn't exist or overwriting if it does.

**Parameters:**
- `path`: Path where the file should be written
- `content`: String content to write to the file

**Return Value:**
- `bool`: `true` if successful

**Errors:**
- Throws a runtime error if the file cannot be written (e.g., permission issues)

**Example:**
```veld
let success = std.io.write_file("output.txt", "Hello, file!")
if success
    std.io.println("File written successfully")
end
```

### `file_exists(path: str) -> bool`

Checks whether a file exists at the specified path.

**Parameters:**
- `path`: Path to check

**Return Value:**
- `bool`: `true` if the file exists, `false` otherwise

**Example:**
```veld
if std.io.file_exists("config.json")
    let config = std.io.read_file("config.json")
    # Process config...
else
    std.io.println("Config file not found")
end
```

## Input Functions

### `read_line() -> str`

Reads a line of input from standard input (stdin).

**Parameters:**
- None

**Return Value:**
- `str`: The input line with trailing newline characters removed

**Errors:**
- Throws a runtime error if reading from stdin fails

**Example:**
```veld
std.io.print("Enter your name: ")
let name = std.io.read_line()
std.io.println("Hello, " + name + "!")
```

## Examples

### Basic Console I/O
```veld
import std.io.{print, println, read_line}

println("Welcome to Veld!")
print("What's your name? ")
let name = read_line()
println("Nice to meet you, " + name + "!")
```

### File Processing
```veld
import std.io

# Read a file, process it, and write the result
if std.io.file_exists("input.txt")
    let content = std.io.read_file("input.txt")
    let processed = content.to_uppercase()  # Hypothetical string method
    std.io.write_file("output.txt", processed)
    std.io.println("File processed successfully!")
else
    std.io.println("Input file not found")
end
```

### Interactive Program
```veld
import std.io.{print, println, read_line}

println("Simple Calculator")
print("Enter first number: ")
let a = read_line()

print("Enter second number: ")
let b = read_line()

println("Sum: " + (a.to_int() + b.to_int()).to_str())
```

## Error Handling

All I/O operations can potentially fail and will throw runtime errors in case of issues:

- **File operations**: May fail due to permission issues, disk space, or invalid paths
- **Standard input**: May fail if stdin is closed or redirected incorrectly

**Example error handling:**
```veld
# Error handling is currently done through runtime exceptions
# Future versions may include Result<T, E> types for better error handling
```

## Implementation Notes

### Native Implementation
All functions in `std.io` are implemented natively in Rust for:
- **Performance**: Direct system calls without interpretation overhead
- **Safety**: Rust's memory safety guarantees
- **Cross-platform compatibility**: Works on Windows, macOS, and Linux

### String Encoding
- All string operations use UTF-8 encoding
- File operations preserve the original encoding when possible
- Input/output handles various line ending formats (LF, CRLF)

### Import Redirection
The Veld interpreter automatically redirects both direct module calls and imported function calls to their native implementations:

```veld
import std.io.{print}

# Both of these call the same native implementation:
std.io.print("Hello")  # Direct module call
print("Hello")         # Imported function call
```

### Escape Sequences
String literals support standard escape sequences:
- `\n` - Newline
- `\t` - Tab
- `\r` - Carriage return
- `\\` - Backslash
- `\"` - Quote
- `\0` - Null character

```veld
std.io.print("Line 1\nLine 2\tTabbed")
# Output:
# Line 1
# Line 2    Tabbed
```

## Future Enhancements

Planned improvements for the `std.io` module include:
- Buffered I/O operations for large files
- Binary file operations
- Directory operations (listing, creating, etc.)
- Formatted output functions (similar to `printf`)
- Result<T, E> types for better error handling
- Streaming I/O for large datasets