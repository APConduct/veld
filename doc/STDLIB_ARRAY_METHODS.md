# Veld Array Methods - Quick Reference Guide

Complete reference for all array methods available in the Veld standard library.

## Table of Contents
- [Core Methods](#core-methods)
- [Higher-Order Functions](#higher-order-functions)
- [Conditional Iteration](#conditional-iteration)
- [Deduplication & Grouping](#deduplication--grouping)
- [Transformation](#transformation)
- [Aggregation](#aggregation)
- [Search & Test](#search--test)
- [Utilities](#utilities)

---

## Core Methods

### `len()`
Returns the number of elements in the array.
```veld
let arr = [1, 2, 3, 4, 5]
let size = arr.len()  # 5
```
- **Signature:** `Array<T> -> i32`
- **Time:** O(1)

### `is_empty()`
Returns true if the array has no elements.
```veld
let arr = []
let empty = arr.is_empty()  # true
```
- **Signature:** `Array<T> -> bool`
- **Time:** O(1)

### `first()`
Returns the first element wrapped in Option.
```veld
let arr = [1, 2, 3]
match arr.first()
    Option.Some(x) => std.io.println(x.to_str())
    Option.None => std.io.println("empty")
end
```
- **Signature:** `Array<T> -> Option<T>`
- **Time:** O(1)

### `last()`
Returns the last element wrapped in Option.
```veld
let arr = [1, 2, 3]
let last = arr.last()  # Option.Some(3)
```
- **Signature:** `Array<T> -> Option<T>`
- **Time:** O(1)

### `init()`
Returns all elements except the last.
```veld
let arr = [1, 2, 3, 4]
let without_last = arr.init()  # [1, 2, 3]
```
- **Signature:** `Array<T> -> Array<T>`
- **Time:** O(n)

### `tail()`
Returns all elements except the first.
```veld
let arr = [1, 2, 3, 4]
let without_first = arr.tail()  # [2, 3, 4]
```
- **Signature:** `Array<T> -> Array<T>`
- **Time:** O(n)

---

## Higher-Order Functions

### `map(function)`
Transforms each element using the provided function.
```veld
let numbers = [1, 2, 3, 4]
let doubled = numbers.map(x => x * 2)  # [2, 4, 6, 8]
```
- **Signature:** `Array<T> -> (T -> U) -> Array<U>`
- **Time:** O(n)

### `filter(predicate)`
Keeps only elements that satisfy the predicate.
```veld
let numbers = [1, 2, 3, 4, 5, 6]
let evens = numbers.filter(x => x % 2 == 0)  # [2, 4, 6]
```
- **Signature:** `Array<T> -> (T -> bool) -> Array<T>`
- **Time:** O(n)

### `reduce(initial, function)` / `fold(initial, function)`
Reduces the array to a single value by accumulating.
```veld
let numbers = [1, 2, 3, 4, 5]
let sum = numbers.reduce(0, (acc, x) => acc + x)  # 15
let product = numbers.reduce(1, (acc, x) => acc * x)  # 120
```
- **Signature:** `Array<T> -> U -> ((U, T) -> U) -> U`
- **Time:** O(n)
- **Note:** Function takes (accumulator, element)

### `flat_map(function)`
Maps each element to an array and flattens the results.
```veld
let numbers = [1, 2, 3]
let expanded = numbers.flat_map(x => [x, x * 10])
# Result: [1, 10, 2, 20, 3, 30]
```
- **Signature:** `Array<T> -> (T -> Array<U>) -> Array<U>`
- **Time:** O(n * m) where m is avg result array size

---

## Conditional Iteration

### `take_while(predicate)` ✨ NEW
Takes elements from the start while predicate is true.
```veld
let numbers = [1, 2, 3, 4, 1, 2, 3]
let result = numbers.take_while(x => x < 4)  # [1, 2, 3]
```
- **Signature:** `Array<T> -> (T -> bool) -> Array<T>`
- **Time:** O(k) where k is number of matching elements
- **Behavior:** Stops at first false, doesn't continue

### `drop_while(predicate)` ✨ NEW
Drops elements from the start while predicate is true.
```veld
let numbers = [1, 2, 3, 4, 5, 6]
let result = numbers.drop_while(x => x < 4)  # [4, 5, 6]
```
- **Signature:** `Array<T> -> (T -> bool) -> Array<T>`
- **Time:** O(n)
- **Behavior:** Keeps everything starting from first false

### `take(n)`
Returns the first n elements.
```veld
let arr = [1, 2, 3, 4, 5]
let first_three = arr.take(3)  # [1, 2, 3]
```
- **Signature:** `Array<T> -> i32 -> Array<T>`
- **Time:** O(n)

### `drop(n)`
Returns all elements after the first n.
```veld
let arr = [1, 2, 3, 4, 5]
let after_two = arr.drop(2)  # [3, 4, 5]
```
- **Signature:** `Array<T> -> i32 -> Array<T>`
- **Time:** O(n)

---

## Deduplication & Grouping

### `unique()` / `dedup()` ✨ NEW
Removes duplicate elements, keeping the first occurrence.
```veld
let numbers = [1, 2, 2, 3, 1, 4, 3, 5]
let unique = numbers.unique()  # [1, 2, 3, 4, 5]
```
- **Signature:** `Array<T> -> Array<T>`
- **Time:** O(n²) - could be optimized
- **Behavior:** Maintains order of first occurrence

### `partition(predicate)` ✨ NEW
Splits array into two arrays based on predicate.
```veld
let numbers = [1, 2, 3, 4, 5, 6]
let (evens, odds) = numbers.partition(x => x % 2 == 0)
# evens: [2, 4, 6], odds: [1, 3, 5]
```
- **Signature:** `Array<T> -> (T -> bool) -> (Array<T>, Array<T>)`
- **Time:** O(n)
- **Returns:** Tuple of (matching, non-matching)

---

## Transformation

### `enumerate()` ✨ NEW
Adds indices to each element.
```veld
let fruits = ["apple", "banana", "cherry"]
let indexed = fruits.enumerate()
# Result: [(0, "apple"), (1, "banana"), (2, "cherry")]
```
- **Signature:** `Array<T> -> Array<(i32, T)>`
- **Time:** O(n)
- **Use with:** map, filter, or destructuring in lambdas

### `zip(other_array)`
Combines two arrays into an array of tuples.
```veld
let names = ["Alice", "Bob", "Charlie"]
let ages = [25, 30, 35]
let people = names.zip(ages)
# Result: [("Alice", 25), ("Bob", 30), ("Charlie", 35)]
```
- **Signature:** `Array<T> -> Array<U> -> Array<(T, U)>`
- **Time:** O(min(n, m))
- **Length:** Uses minimum of both array lengths

### `reverse()`
Reverses the order of elements.
```veld
let numbers = [1, 2, 3, 4, 5]
let reversed = numbers.reverse()  # [5, 4, 3, 2, 1]
```
- **Signature:** `Array<T> -> Array<T>`
- **Time:** O(n)

### `with(element)`
Returns a new array with the element appended.
```veld
let arr = [1, 2, 3]
let new_arr = arr.with(4)  # [1, 2, 3, 4]
```
- **Signature:** `Array<T> -> T -> Array<T>`
- **Time:** O(n)
- **Note:** Immutable - returns new array

---

## Aggregation

### `join(separator)`
Joins array elements into a string.
```veld
let words = ["Hello", "World", "from", "Veld"]
let sentence = words.join(" ")  # "Hello World from Veld"

let numbers = [1, 2, 3]
let csv = numbers.join(", ")  # "1, 2, 3"
```
- **Signature:** `Array<T> -> String -> String`
- **Time:** O(n)
- **Works with:** Any type (calls to_str() if needed)

---

## Search & Test

### `find(predicate)`
Returns the first element matching the predicate.
```veld
let numbers = [1, 2, 3, 4, 5]
let result = numbers.find(x => x > 3)
match result
    Option.Some(n) => std.io.println(n.to_str())  # Prints "4"
    Option.None => std.io.println("Not found")
end
```
- **Signature:** `Array<T> -> (T -> bool) -> Option<T>`
- **Time:** O(n) worst case, O(k) average
- **Short-circuits:** Stops at first match

### `any(predicate)`
Returns true if any element satisfies the predicate.
```veld
let numbers = [1, 2, 3, 4, 5]
let has_even = numbers.any(x => x % 2 == 0)  # true
```
- **Signature:** `Array<T> -> (T -> bool) -> bool`
- **Time:** O(n) worst case
- **Short-circuits:** Stops at first true

### `all(predicate)`
Returns true if all elements satisfy the predicate.
```veld
let numbers = [2, 4, 6, 8]
let all_even = numbers.all(x => x % 2 == 0)  # true
```
- **Signature:** `Array<T> -> (T -> bool) -> bool`
- **Time:** O(n) worst case
- **Short-circuits:** Stops at first false
- **Empty array:** Returns true (vacuous truth)

---

## Common Patterns

### Pattern 1: Filter and Map
```veld
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let result = numbers
    .filter(x => x % 2 == 0)  # [2, 4, 6, 8, 10]
    .map(x => x * x)          # [4, 16, 36, 64, 100]
```

### Pattern 2: Partition and Process
```veld
let scores = [45, 67, 89, 34, 92, 56, 78, 23, 91]
let (passing, failing) = scores.partition(s => s >= 60)

let pass_avg = passing.reduce(0, (acc, x) => acc + x) / passing.len()
let fail_avg = failing.reduce(0, (acc, x) => acc + x) / failing.len()
```

### Pattern 3: Enumerate and Filter by Index
```veld
let items = ["a", "b", "c", "d", "e", "f"]
let even_indexed = items
    .enumerate()
    .map((pair) => do
        let (i, item) = pair
        if i % 2 == 0 then Option.Some(item) else Option.None
    end)
    .filter(opt => match opt
        Option.Some(_) => true
        Option.None => false
    end)
```

### Pattern 4: Take/Drop for Range Extraction
```veld
let sequence = [1, 2, 3, 10, 20, 30, 100, 200, 300]
let middle = sequence
    .drop_while(x => x < 10)   # [10, 20, 30, 100, 200, 300]
    .take_while(x => x < 100)  # [10, 20, 30]
```

### Pattern 5: Unique and Sort
```veld
let data = [5, 3, 8, 3, 1, 5, 9, 1, 8]
let unique_sorted = data.unique()  # [5, 3, 8, 1, 9]
# Note: sort() not yet implemented, coming soon!
```

### Pattern 6: Chaining Everything
```veld
let result = raw_data
    .unique()                  # Remove duplicates
    .filter(x => x > 10)      # Keep values > 10
    .take_while(x => x < 100) # Take until we hit 100
    .map(x => x * 2)          # Double each value
    .reduce(0, (acc, x) => acc + x)  # Sum them up
```

---

## Performance Tips

### ✅ DO: Chain methods for readability
```veld
let result = data.filter(x => x > 0).map(x => x * 2).reduce(0, (acc, x) => acc + x)
```

### ✅ DO: Use short-circuiting methods when possible
```veld
# Stops at first match
let has_negative = numbers.any(x => x < 0)

# Better than
let negatives = numbers.filter(x => x < 0)
let has_negative = negatives.len() > 0
```

### ✅ DO: Use partition for two-way splits
```veld
let (good, bad) = data.partition(x => x.is_valid())
```

### ⚠️ CAUTION: unique() is O(n²)
For large arrays with many unique elements, consider alternative approaches.

### ⚠️ CAUTION: Immutable operations create new arrays
```veld
# This creates n new arrays!
let result = arr
for i in 0..1000 do
    result = result.with(i)  # O(n) each time
end

# Better: build once or use Vec
```

---

## Method Chaining Examples

### Example 1: Data Cleaning Pipeline
```veld
let clean_data = raw_data
    .unique()                    # Remove duplicates
    .filter(x => x > 0)         # Remove invalid values
    .take_while(x => x < 1000)  # Take reasonable range
```

### Example 2: Statistical Analysis
```veld
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

let even_sum = numbers
    .filter(x => x % 2 == 0)
    .reduce(0, (acc, x) => acc + x)

let odd_product = numbers
    .filter(x => x % 2 == 1)
    .reduce(1, (acc, x) => acc * x)
```

### Example 3: Text Processing
```veld
let lines = ["INFO: Started", "DEBUG: Value=42", "ERROR: Failed", "INFO: Done"]

let errors = lines
    .filter(line => line.len() > 5)
    .filter(line => line.take(5).join("") == "ERROR")

let log_summary = lines
    .enumerate()
    .map((pair) => do
        let (i, line) = pair
        "[" + i.to_str() + "] " + line
    end)
    .join("\n")
```

---

## Gotchas and Known Issues

### Issue 1: Tuple Destructuring in Filter
Currently, tuple destructuring in filter predicates has type inference limitations.

**Workaround:**
```veld
# Instead of:
let result = indexed.filter((pair) => do
    let (i, val) = pair  # May fail with type error
    val > 10
end)

# Use map after filter:
let result = indexed
    .filter(pair => true)  # Filter without destructuring
    .map((pair) => do
        let (i, val) = pair  # Works here
        # process
    end)
```

### Issue 2: For-Loop Tuple Destructuring
For-loop destructuring of arrays returned by enumerate() currently has issues.

**Workaround:**
```veld
# Instead of:
for (i, elem) in array.enumerate() do
    # May fail
end

# Use:
let indexed = array.enumerate()
let result = indexed.map((pair) => do
    let (i, elem) = pair
    # process
end)
```

---

## Summary

### Total Array Methods: 21
- **Core:** 7 methods
- **Higher-Order:** 4 methods
- **Conditional:** 4 methods
- **Deduplication:** 2 methods
- **Transformation:** 4 methods
- **Search & Test:** 3 methods

### New in This Version: ✨
- `enumerate()` - Add indices
- `partition()` - Split by predicate  
- `take_while()` - Take until false
- `drop_while()` - Drop until false
- `unique()` / `dedup()` - Remove duplicates

### Coming Soon: 🚀
- `sort()` - Sort in ascending order
- `sort_by()` - Sort with custom comparator
- `scan()` - Cumulative reduce
- `fold_right()` - Right-to-left fold
- `chunk()` - Split into chunks
- `windows()` - Sliding windows
- `group_by()` - Group consecutive elements

---

## Resources

- **Documentation:** See `SESSION_SUMMARY_STDLIB_EXPANSION.md` for detailed implementation notes
- **Examples:** Check `test_*` files in the project root
- **Bug Reports:** Known issues documented in session summary

**Last Updated:** December 2024
**Veld Version:** 0.1.4