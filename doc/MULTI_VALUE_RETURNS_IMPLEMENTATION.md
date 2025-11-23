# Multi-Value Returns Implementation

## Overview
Successfully implemented multi-value returns for the Veld bytecode compiler. Functions can now return multiple values as tuples, which are automatically packed and unpacked.

## What Was Implemented

### Core Feature
Functions can return multiple values by returning a tuple:
```veld
fn get_coords() => (10, 20)
fn get_user_info() => ("Alice", 25, true)
fn divide_with_remainder(a, b) => (a / b, a % b)
```

The returned tuple can be assigned to a variable and used like any other tuple:
```veld
let coords = get_coords()
let user = get_user_info()
```

### How It Works

#### 1. VM Enhancement
**File Modified**: `crates/bytecode/src/vm_v2.rs`

The `Return` instruction already supported multiple return values with a `count` parameter. Updated the implementation to pack multiple values into a tuple:

```rust
Instruction::Return { first, count } => {
    let result = if count == 0 {
        BytecodeValue::Unit
    } else if count == 1 {
        self.get_register(first)?.clone()
    } else {
        // Multiple return values - pack into a tuple
        let mut values = Vec::new();
        for i in 0..count {
            values.push(self.get_register(first + i)?.clone());
        }
        BytecodeValue::Tuple(values)
    };
    // ... rest of return handling
}
```

**Behavior**:
- `count == 0`: Returns `Unit` (void)
- `count == 1`: Returns single value directly
- `count > 1`: Creates a tuple containing all values

#### 2. Tuple Compilation Fix
**File Modified**: `crates/bytecode/src/compiler_v2.rs`

Fixed the `compile_tuple` function to properly allocate consecutive registers for tuple elements, as required by the `NewTuple` instruction.

**Problem**: The `NewTuple` instruction expects tuple elements in consecutive registers starting at `dest+1`, but the compiler was placing elements in arbitrary registers.

**Solution**:
1. Use `allocate_range()` to reserve consecutive registers
2. Compile each element into its consecutive position
3. Move elements to the required positions (dest+1, dest+2, etc.)
4. Call `NewTuple` instruction

```rust
fn compile_tuple(&mut self, elements: &[Expr]) -> Result<ExprResult> {
    let dest = self.allocate_temp()?;
    
    // Allocate consecutive registers for elements
    let elem_base = self.allocator.allocate_range(elements.len() as u8)?;
    
    // Compile each element into consecutive registers
    for (i, elem) in elements.iter().enumerate() {
        let result = self.compile_expr_to_reg(elem)?;
        let target_reg = elem_base + i as u8;
        if result.register != target_reg {
            self.chunk.move_reg(target_reg, result.register);
        }
        // ...
    }
    
    // Ensure elements are at dest+1, dest+2, etc.
    if elem_base != dest + 1 {
        for i in 0..elements.len() {
            self.chunk.move_reg(dest + 1 + i as u8, elem_base + i as u8);
        }
    }
    
    self.chunk.new_tuple(dest, elements.len() as u8);
    Ok(ExprResult::temp(dest))
}
```

## Tests Added

### Integration Tests (`crates/bytecode/tests/compiler_integration.rs`)
Added 7 comprehensive tests:

1. `test_multi_value_return_simple` - Basic tuple return `(10, 20)`
2. `test_multi_value_return_different_types` - Mixed types `("Alice", 25, true)`
3. `test_multi_value_return_from_body` - Return tuple from do block
4. `test_multi_value_return_conditional` - Return tuple from if/else
5. `test_multi_value_return_three_values` - Three values `(255, 128, 64)`
6. `test_multi_value_return_computed` - Computed tuple values
7. `test_multi_value_return_empty_tuple` - Empty tuple `()`

### Test Results
```
✅ All 49 compiler integration tests pass (up from 42)
✅ All 145 total bytecode tests pass
✅ No regressions
```

## Example Usage

### Basic Multi-Value Return
```veld
fn get_coords() => (10, 20)

let point = get_coords()
# point is now a tuple: (10, 20)
```

### Different Types
```veld
fn get_user_info() => ("Alice", 25, true)

let user = get_user_info()
# user is tuple: ("Alice", 25, true)
```

### Computed Values
```veld
fn get_dimensions() => (100, 200)

let dims = get_dimensions()
```

### Conditional Returns
```veld
fn get_min_max(a, b) => do
    if a < b then
        (a, b)
    else
        (b, a)
    end
end

let result = get_min_max(10, 5)
# result is (5, 10)
```

### Empty Tuple (Unit)
```veld
fn nothing() => ()

let unit = nothing()
# unit is ()
```

## Technical Details

### Return Instruction Format
The `Return` instruction in bytecode_v2:
```rust
Return { 
    first: Reg,  // First register containing return value(s)
    count: u8    // Number of consecutive registers to return
}
```

- `first` = starting register
- `count` = number of values to return
- Values are read from registers `first`, `first+1`, `first+2`, etc.

### Tuple Creation
The `NewTuple` instruction:
```rust
NewTuple { 
    dest: Reg,   // Destination register for tuple
    size: u8     // Number of elements
}
```

- Reads elements from `dest+1`, `dest+2`, ..., `dest+size`
- Creates tuple in register `dest`

### Register Allocation
Uses `RegisterAllocator::allocate_range()` to ensure consecutive register allocation for tuple elements. This is critical because:
1. `NewTuple` expects consecutive registers
2. Random allocation would cause runtime errors
3. Moving values to consecutive positions ensures correctness

## Limitations & Future Work

### Current Limitations
1. **No tuple destructuring in let statements**: Cannot write `let (x, y) = get_coords()`
   - Workaround: Use tuple access: `let coords = get_coords()` then access `coords.0`, `coords.1`
   
2. **No unpacking in function parameters**: Cannot write `fn use_pair((x, y)) => ...`
   - Would require pattern matching in parameter lists

3. **Return tuple, not true multi-values**: Veld uses tuple-packing approach (like Python/Rust) rather than Lua-style multi-values
   - This is simpler and more predictable
   - Tuples are first-class values that can be stored and passed around

### Future Enhancements
1. **Tuple destructuring**: `let (x, y, z) = get_triple()`
2. **Pattern matching on return**: `match get_coords() { (x, y) => ... }`
3. **Optimized single-value case**: Skip tuple creation when only one value returned
4. **Variadic returns**: Support variable number of return values

## Comparison with Other Languages

### Python-style (what Veld does)
```python
def get_coords():
    return (10, 20)  # Returns a tuple object

x, y = get_coords()  # Unpacks tuple
```

### Lua-style (not implemented)
```lua
function get_coords()
    return 10, 20  # Returns multiple values on stack
end

local x, y = get_coords()  # Receives multiple values
```

### Go-style (different syntax, similar semantics)
```go
func getCoords() (int, int) {
    return 10, 20
}

x, y := getCoords()
```

Veld's approach is closest to Python/Rust - return values are packed into a tuple, which is a first-class value.

## Status: ✅ COMPLETE

Multi-value returns are now fully implemented and tested in the Veld bytecode compiler!

## Next Steps
Moving on to the next TODO item from the compiler improvement list:
- [x] Compound assignment operators (+=, -=, *=, /=)
- [x] Multi-value returns
- [ ] Runtime type checking
- [ ] Type casting
- [ ] Try/catch mechanism
- [ ] Module imports (in VM)
- [ ] Tail call optimization