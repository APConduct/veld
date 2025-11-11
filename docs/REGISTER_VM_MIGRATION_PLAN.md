# Register-Based VM Migration Plan

## Overview

This document outlines the step-by-step plan to migrate Veld's bytecode VM from stack-based to register-based architecture.

## Migration Strategy

### Phase 0: Preparation (Current)
- [x] Analysis complete
- [x] Decision made: Register-based VM
- [x] Documentation created

### Phase 1: Instruction Set Design ✅ NEXT
**Goal:** Design the new register-based instruction set

**Files to create:**
- `crates/common/src/bytecode_v2.rs` - New instruction set (keep old as v1)
- `docs/INSTRUCTION_SET_SPEC.md` - Detailed instruction documentation

**Key decisions:**
1. Register count per frame (256 max, use u8)
2. Instruction encoding (fixed vs variable width)
3. Operand types (register, constant, immediate)
4. Calling convention
5. Closure/upvalue representation

**Timeline:** 2-3 days

---

### Phase 2: VM Core Refactor
**Goal:** Rewrite VM execution engine for registers

**Files to modify:**
- `crates/bytecode/src/vm.rs` - Main VM implementation
- `crates/bytecode/src/value.rs` - May need updates

**Changes:**
1. Replace `stack: Vec<BytecodeValue>` with register files
2. Update `CallFrame` to track register base/count
3. Rewrite instruction execution loop
4. Update function call mechanism
5. Implement register allocation in frames

**Key structures:**
```rust
pub struct VirtualMachine {
    registers: Vec<BytecodeValue>,  // Flat register file
    frames: Vec<CallFrame>,
    globals: HashMap<String, BytecodeValue>,
    // ... rest
}

pub struct CallFrame {
    chunk: Chunk,
    ip: usize,
    register_base: usize,  // Start of this frame's registers
    register_count: usize, // Number of registers for this frame
    upvalues: Vec<UpvalueRef>,
}
```

**Timeline:** 1 week

---

### Phase 3: Compiler Refactor - Part 1 (Expressions)
**Goal:** Update compiler to emit register-based bytecode

**Files to modify:**
- `crates/bytecode/src/compiler.rs` - Main compiler

**Changes:**
1. Add register allocator
2. Track register usage per scope
3. Compile expressions to registers
4. Handle temporaries properly

**Register Allocator:**
```rust
struct RegisterAllocator {
    next_free: u8,
    in_use: HashSet<u8>,
    scope_registers: Vec<Vec<u8>>,
}

impl RegisterAllocator {
    fn allocate(&mut self) -> u8 { /* ... */ }
    fn allocate_range(&mut self, count: u8) -> u8 { /* ... */ }
    fn free(&mut self, reg: u8) { /* ... */ }
    fn mark_used(&mut self, reg: u8) { /* ... */ }
}
```

**Timeline:** 1 week

---

### Phase 4: Compiler Refactor - Part 2 (Statements & Control Flow)
**Goal:** Compile statements, control flow, and functions

**Changes:**
1. Compile variable declarations (assign to registers)
2. Compile assignments
3. Compile if/while/for statements
4. Compile function definitions
5. Handle return statements

**Timeline:** 1 week

---

### Phase 5: Closures and Upvalues
**Goal:** Implement closure support in register model

**Changes:**
1. Design upvalue representation
2. Implement closure creation
3. Implement upvalue access/capture
4. Update function calls to handle closures

**Upvalue mechanism (similar to Lua):**
```rust
pub enum Upvalue {
    Open { register: u8, frame: usize },  // Still on stack
    Closed { value: BytecodeValue },      // Captured
}
```

**Timeline:** 4-5 days

---

### Phase 6: Advanced Features
**Goal:** Implement remaining features

**Features:**
1. Pattern matching
2. Struct operations (NewStruct, GetField, SetField)
3. Array operations (NewArray, GetIndex, SetIndex)
4. Exception handling (Try/Catch)
5. Iterator protocol

**Timeline:** 1 week

---

### Phase 7: Testing & Validation
**Goal:** Ensure correctness and performance

**Tasks:**
1. Port existing bytecode tests
2. Add register-specific tests
3. Test edge cases (register spilling, deep call stacks)
4. Benchmark vs tree-walk interpreter
5. Benchmark vs old stack-based VM (if kept for comparison)

**Test categories:**
- Arithmetic expressions
- Function calls
- Closures
- Loops
- Pattern matching
- Error handling

**Timeline:** 1 week

---

### Phase 8: Optimization
**Goal:** Optimize the register VM

**Optimizations:**
1. Peephole optimization (combine instructions)
2. Dead register elimination
3. Register coalescing (reduce moves)
4. Constant folding
5. Inline small functions (optional)

**Timeline:** 4-5 days

---

### Phase 9: Integration & Cleanup
**Goal:** Integrate with rest of Veld system

**Tasks:**
1. Update REPL to use new bytecode
2. Update CLI to select interpreter vs bytecode
3. Add bytecode dumper/disassembler (debugging tool)
4. Update documentation
5. Remove old stack-based code (or mark deprecated)

**Timeline:** 3-4 days

---

## Detailed Phase 1: Instruction Set Design

### Register Encoding
```rust
// Register index (0-255)
type Reg = u8;

// Constant pool index
type ConstIdx = u16;

// Jump offset (signed)
type JumpOffset = i16;
```

### Instruction Categories

#### 1. Move/Load Instructions
```rust
Move(Reg, Reg),              // R(A) = R(B)
LoadConst(Reg, ConstIdx),    // R(A) = K(Bx)
LoadBool(Reg, bool),         // R(A) = bool
LoadNil(Reg),                // R(A) = nil/unit
```

#### 2. Arithmetic Instructions (3-address code)
```rust
Add(Reg, Reg, Reg),          // R(A) = R(B) + R(C)
Sub(Reg, Reg, Reg),          // R(A) = R(B) - R(C)
Mul(Reg, Reg, Reg),          // R(A) = R(B) * R(C)
Div(Reg, Reg, Reg),          // R(A) = R(B) / R(C)
Mod(Reg, Reg, Reg),          // R(A) = R(B) % R(C)
Pow(Reg, Reg, Reg),          // R(A) = R(B) ^ R(C)
Neg(Reg, Reg),               // R(A) = -R(B)

// Variants with immediate constants
AddK(Reg, Reg, ConstIdx),    // R(A) = R(B) + K(C)
MulK(Reg, Reg, ConstIdx),    // R(A) = R(B) * K(C)
```

#### 3. Comparison Instructions
```rust
Eq(Reg, Reg, Reg),           // R(A) = R(B) == R(C)
Neq(Reg, Reg, Reg),          // R(A) = R(B) != R(C)
Lt(Reg, Reg, Reg),           // R(A) = R(B) < R(C)
Le(Reg, Reg, Reg),           // R(A) = R(B) <= R(C)
Gt(Reg, Reg, Reg),           // R(A) = R(B) > R(C)
Ge(Reg, Reg, Reg),           // R(A) = R(B) >= R(C)
```

#### 4. Logical Instructions
```rust
And(Reg, Reg, Reg),          // R(A) = R(B) && R(C)
Or(Reg, Reg, Reg),           // R(A) = R(B) || R(C)
Not(Reg, Reg),               // R(A) = !R(B)
```

#### 5. Control Flow
```rust
Jump(JumpOffset),            // PC += offset
JumpIf(Reg, JumpOffset),     // if R(A) then PC += offset
JumpIfNot(Reg, JumpOffset),  // if !R(A) then PC += offset
```

#### 6. Function Calls
```rust
// Call function in R(A) with args R(A+1)..R(A+B)
// Results go to R(A)..R(A+C-1)
Call(Reg, u8, u8),           // Call(func_reg, arg_count, ret_count)

// Return values R(A)..R(A+B-1)
Return(Reg, u8),             // Return(first_reg, count)
```

#### 7. Closures and Upvalues
```rust
Closure(Reg, ConstIdx),      // R(A) = closure(PROTO[Bx])
GetUpvalue(Reg, u8),         // R(A) = Upvalue[B]
SetUpvalue(u8, Reg),         // Upvalue[A] = R(B)
CloseUpvalues(Reg),          // Close upvalues >= R(A)
```

#### 8. Data Structures
```rust
NewArray(Reg, u16),          // R(A) = array[B elements from R(A+1)..R(A+B)]
GetIndex(Reg, Reg, Reg),     // R(A) = R(B)[R(C)]
SetIndex(Reg, Reg, Reg),     // R(A)[R(B)] = R(C)

NewStruct(Reg, ConstIdx, u8),// R(A) = struct type K(B) with C fields
GetField(Reg, Reg, ConstIdx),// R(A) = R(B).K(C)
SetField(Reg, ConstIdx, Reg),// R(A).K(B) = R(C)

NewTuple(Reg, u8),           // R(A) = tuple of B elements from R(A+1)
NewEnum(Reg, ConstIdx, u8),  // R(A) = enum variant K(B) with C fields
```

#### 9. Pattern Matching
```rust
Match(Reg, u16),             // Match on R(A), jump table at Bx
MatchPattern(Reg, ConstIdx, JumpOffset), // Test R(A) against pattern K(B), jump if match
```

#### 10. Miscellaneous
```rust
Print(Reg),                  // Print R(A)
TypeCheck(Reg, ConstIdx),    // Check R(A) is type K(B)
Halt,                        // Stop execution
Nop,                         // No operation
```

### Instruction Encoding

Two options:

#### Option A: Fixed-Width (32-bit instructions)
```
┌────────┬────────┬────────┬────────┐
│ Opcode │   A    │   B    │   C    │
│ 8 bits │ 8 bits │ 8 bits │ 8 bits │
└────────┴────────┴────────┴────────┘

Pros: Simple, fast decode
Cons: Larger bytecode
```

#### Option B: Variable-Width (Lua-style)
```
Format 1 (ABC):
┌────────┬────────┬────────┬────────┐
│ Opcode │   A    │   B    │   C    │
│ 6 bits │ 8 bits │ 9 bits │ 9 bits │
└────────┴────────┴────────┴────────┘

Format 2 (ABx):
┌────────┬────────┬─────────────────┐
│ Opcode │   A    │       Bx        │
│ 6 bits │ 8 bits │     18 bits     │
└────────┴────────┴─────────────────┘

Pros: Compact
Cons: More complex encoding/decoding
```

**Recommendation:** Start with Option A (fixed-width) for simplicity, optimize later if needed.

---

## Register Allocation Strategy

### Basic Strategy
```rust
// Local variables get fixed registers
let a = 10;     // R0 = 10
let b = 20;     // R1 = 20
let c = 30;     // R2 = 30

// Temporaries use next available
let result = a + b * c;
// R3 = R1 * R2  (temp for b * c)
// R4 = R0 + R3  (result)
```

### Register Windows (Lua-style)
```
Function call sets up register window:

Caller:
  R0-R9: caller's registers

Call prepare:
  R10: function
  R11-R13: arguments

Callee frame:
  R0: function
  R1-R3: parameters (same as R11-R13 in caller)
  R4-R9: callee's locals/temps

After return:
  R10-R12: return values (overwrite function + args)
```

### Scope Management
```rust
struct Scope {
    depth: usize,
    locals: Vec<Local>,
    first_register: u8,
    register_count: u8,
}

struct Local {
    name: String,
    register: u8,
    depth: usize,
    is_captured: bool,
}
```

---

## Example Compilation

### Source Code
```veld
fn fibonacci(n: i32) -> i32
    if n <= 1 then
        return n
    end
    return fibonacci(n - 1) + fibonacci(n - 2)
end
```

### Register-Based Bytecode
```
fibonacci:  ; R0 = n (parameter)
  LoadConst R1, 1          ; R1 = 1
  Le R2, R0, R1            ; R2 = (n <= 1)
  JumpIfNot R2, +2         ; if not (n <= 1), skip return
  Return R0, 1             ; return n
  
  LoadConst R3, 1          ; R3 = 1
  Sub R4, R0, R3           ; R4 = n - 1
  LoadGlobal R5, "fibonacci"
  Move R6, R4              ; R6 = arg (n-1)
  Call R5, 1, 1            ; R5 = fibonacci(n-1)
  
  LoadConst R7, 2          ; R7 = 2
  Sub R8, R0, R7           ; R8 = n - 2
  LoadGlobal R9, "fibonacci"
  Move R10, R8             ; R10 = arg (n-2)
  Call R9, 1, 1            ; R9 = fibonacci(n-2)
  
  Add R11, R5, R9          ; R11 = fib(n-1) + fib(n-2)
  Return R11, 1            ; return result
```

---

## Testing Strategy

### Unit Tests
1. **Instruction execution**
   - Test each instruction independently
   - Verify register reads/writes
   - Test edge cases (overflow, nil values)

2. **Register allocation**
   - Test scope management
   - Test register reuse
   - Test spilling (if implemented)

3. **Function calls**
   - Test parameter passing
   - Test return values
   - Test nested calls
   - Test recursion

### Integration Tests
1. **Arithmetic expressions**
   ```veld
   let x = (a + b) * (c - d)
   ```

2. **Control flow**
   ```veld
   if condition then
       // ...
   else
       // ...
   end
   ```

3. **Functions**
   ```veld
   fn factorial(n: i32) -> i32
       if n <= 1 then return 1 end
       return n * factorial(n - 1)
   end
   ```

4. **Closures**
   ```veld
   fn make_counter() -> fn() -> i32
       let count = 0
       return fn() -> i32
           count = count + 1
           return count
       end
   end
   ```

### Benchmark Tests
Compare performance against:
1. Tree-walk interpreter
2. Old stack-based VM (if kept)

**Metrics:**
- Execution time
- Memory usage
- Bytecode size
- Compilation time

---

## Rollout Plan

### Step 1: Side-by-side implementation
- Keep old stack-based VM as `bytecode_v1`
- Build new register-based as `bytecode_v2`
- Allow choosing which to use via flag

### Step 2: Testing period
- Run both VMs on test suite
- Verify identical results
- Performance comparison

### Step 3: Default switch
- Make register-based the default
- Keep stack-based for comparison/fallback

### Step 4: Deprecation
- After stable period, deprecate stack-based
- Eventually remove old code

---

## File Organization

### New Files
```
crates/common/src/
  bytecode.rs          (rename to bytecode_v1.rs)
  bytecode_v2.rs       (new register-based instructions)
  
crates/bytecode/src/
  vm.rs                (modify for registers)
  vm_v1.rs             (backup of stack-based, optional)
  compiler.rs          (modify for register allocation)
  register_alloc.rs    (new: register allocator)
  
docs/
  INSTRUCTION_SET_SPEC.md      (new)
  REGISTER_VM_MIGRATION_PLAN.md (this file)
  BYTECODE_ARCHITECTURE_ANALYSIS.md (existing)
```

---

## Risk Mitigation

### Risk: Bugs in register allocation
**Mitigation:** 
- Extensive unit tests for allocator
- Compare with Lua's implementation
- Fuzz testing with random expressions

### Risk: Performance not as expected
**Mitigation:**
- Benchmark early and often
- Profile to find bottlenecks
- Iterate on hot paths

### Risk: Closures/upvalues complexity
**Mitigation:**
- Follow Lua's proven design closely
- Test closure edge cases thoroughly
- Document the mechanism clearly

### Risk: Integration issues
**Mitigation:**
- Keep old VM as fallback during transition
- Incremental integration
- Comprehensive integration tests

---

## Success Criteria

The migration is successful when:

1. ✅ All existing bytecode tests pass
2. ✅ Register VM is 25-35% faster than tree-walk interpreter
3. ✅ Bytecode size is <20% larger than stack-based
4. ✅ All language features work (closures, pattern matching, etc.)
5. ✅ Code is well-documented and maintainable
6. ✅ No regressions in functionality

---

## Timeline Summary

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| 1. Instruction Set Design | 2-3 days | None |
| 2. VM Core | 1 week | Phase 1 |
| 3. Compiler - Expressions | 1 week | Phase 2 |
| 4. Compiler - Statements | 1 week | Phase 3 |
| 5. Closures/Upvalues | 4-5 days | Phase 4 |
| 6. Advanced Features | 1 week | Phase 5 |
| 7. Testing | 1 week | Phase 6 |
| 8. Optimization | 4-5 days | Phase 7 |
| 9. Integration | 3-4 days | Phase 8 |

**Total: ~8-10 weeks**

---

## Next Steps

1. ✅ Review and approve this plan
2. → Start Phase 1: Design instruction set
3. → Create `bytecode_v2.rs` with new instruction enum
4. → Document instruction set in `INSTRUCTION_SET_SPEC.md`
5. → Begin VM refactor

---

**Status:** 📋 Plan Ready - Awaiting Approval
**Author:** Migration Planning Team
**Last Updated:** 2024