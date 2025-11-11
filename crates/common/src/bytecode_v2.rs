//! Register-based bytecode instruction set for Veld VM
//!
//! This module defines a register-based virtual machine instruction set,
//! inspired by Lua 5.x. Each instruction operates on virtual registers
//! rather than a stack, providing better performance through fewer
//! memory operations and more compact instruction sequences.
//!
//! # Register Model
//!
//! - Each call frame has up to 256 registers (indexed 0-255)
//! - Registers hold `Value` instances
//! - Local variables are assigned to specific registers
//! - Temporaries use dynamically allocated registers
//! - Function parameters start at register 0
//!
//! # Instruction Format
//!
//! Instructions use a fixed 32-bit encoding for simplicity:
//!
//! ```text
//! ┌────────┬────────┬────────┬────────┐
//! │ Opcode │   A    │   B    │   C    │
//! │ 8 bits │ 8 bits │ 8 bits │ 8 bits │
//! └────────┴────────┴────────┴────────┘
//! ```
//!
//! - Opcode: Operation to perform
//! - A, B, C: Operands (meaning depends on instruction)
//!
//! Most instructions follow 3-address code format:
//! `INSTRUCTION R(A), R(B), R(C)` means `R(A) = R(B) op R(C)`

use std::fmt;

/// Register index (0-255)
pub type Reg = u8;

/// Constant pool index
pub type ConstIdx = u16;

/// Jump offset (signed, relative to next instruction)
pub type JumpOffset = i16;

/// Register-based instruction set
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // ============================================================
    // MOVE AND LOAD INSTRUCTIONS
    // ============================================================
    /// Move value from one register to another
    /// `R(A) = R(B)`
    Move { dest: Reg, src: Reg },

    /// Load constant into register
    /// `R(A) = K(Bx)` where Bx is 16-bit constant index
    LoadConst { dest: Reg, const_idx: ConstIdx },

    /// Load boolean into register
    /// `R(A) = bool`
    LoadBool { dest: Reg, value: bool },

    /// Load nil/unit into register
    /// `R(A) = nil`
    LoadNil { dest: Reg },

    /// Load nil into range of registers
    /// `R(A) ... R(A+B) = nil`
    LoadNilRange { start: Reg, count: u8 },

    // ============================================================
    // ARITHMETIC INSTRUCTIONS (3-address code)
    // ============================================================
    /// Addition: `R(A) = R(B) + R(C)`
    Add { dest: Reg, lhs: Reg, rhs: Reg },

    /// Addition with constant: `R(A) = R(B) + K(C)`
    AddK { dest: Reg, lhs: Reg, const_idx: u8 },

    /// Subtraction: `R(A) = R(B) - R(C)`
    Sub { dest: Reg, lhs: Reg, rhs: Reg },

    /// Subtraction with constant: `R(A) = R(B) - K(C)`
    SubK { dest: Reg, lhs: Reg, const_idx: u8 },

    /// Multiplication: `R(A) = R(B) * R(C)`
    Mul { dest: Reg, lhs: Reg, rhs: Reg },

    /// Multiplication with constant: `R(A) = R(B) * K(C)`
    MulK { dest: Reg, lhs: Reg, const_idx: u8 },

    /// Division: `R(A) = R(B) / R(C)`
    Div { dest: Reg, lhs: Reg, rhs: Reg },

    /// Division with constant: `R(A) = R(B) / K(C)`
    DivK { dest: Reg, lhs: Reg, const_idx: u8 },

    /// Modulo: `R(A) = R(B) % R(C)`
    Mod { dest: Reg, lhs: Reg, rhs: Reg },

    /// Modulo with constant: `R(A) = R(B) % K(C)`
    ModK { dest: Reg, lhs: Reg, const_idx: u8 },

    /// Power: `R(A) = R(B) ^ R(C)`
    Pow { dest: Reg, lhs: Reg, rhs: Reg },

    /// Negation: `R(A) = -R(B)`
    Neg { dest: Reg, src: Reg },

    // ============================================================
    // COMPARISON INSTRUCTIONS
    // ============================================================
    /// Equal: `R(A) = R(B) == R(C)`
    Eq { dest: Reg, lhs: Reg, rhs: Reg },

    /// Not equal: `R(A) = R(B) != R(C)`
    Neq { dest: Reg, lhs: Reg, rhs: Reg },

    /// Less than: `R(A) = R(B) < R(C)`
    Lt { dest: Reg, lhs: Reg, rhs: Reg },

    /// Less than or equal: `R(A) = R(B) <= R(C)`
    Le { dest: Reg, lhs: Reg, rhs: Reg },

    /// Greater than: `R(A) = R(B) > R(C)`
    Gt { dest: Reg, lhs: Reg, rhs: Reg },

    /// Greater than or equal: `R(A) = R(B) >= R(C)`
    Ge { dest: Reg, lhs: Reg, rhs: Reg },

    // ============================================================
    // LOGICAL INSTRUCTIONS
    // ============================================================
    /// Logical AND: `R(A) = R(B) && R(C)`
    And { dest: Reg, lhs: Reg, rhs: Reg },

    /// Logical OR: `R(A) = R(B) || R(C)`
    Or { dest: Reg, lhs: Reg, rhs: Reg },

    /// Logical NOT: `R(A) = !R(B)`
    Not { dest: Reg, src: Reg },

    // ============================================================
    // BITWISE INSTRUCTIONS
    // ============================================================
    /// Bitwise AND: `R(A) = R(B) & R(C)`
    BitAnd { dest: Reg, lhs: Reg, rhs: Reg },

    /// Bitwise OR: `R(A) = R(B) | R(C)`
    BitOr { dest: Reg, lhs: Reg, rhs: Reg },

    /// Bitwise XOR: `R(A) = R(B) ^ R(C)`
    BitXor { dest: Reg, lhs: Reg, rhs: Reg },

    /// Bitwise NOT: `R(A) = ~R(B)`
    BitNot { dest: Reg, src: Reg },

    /// Left shift: `R(A) = R(B) << R(C)`
    Shl { dest: Reg, lhs: Reg, rhs: Reg },

    /// Right shift: `R(A) = R(B) >> R(C)`
    Shr { dest: Reg, lhs: Reg, rhs: Reg },

    // ============================================================
    // CONTROL FLOW INSTRUCTIONS
    // ============================================================
    /// Unconditional jump: `PC += offset`
    Jump { offset: JumpOffset },

    /// Jump if true: `if R(A) then PC += offset`
    JumpIf { condition: Reg, offset: JumpOffset },

    /// Jump if false: `if !R(A) then PC += offset`
    JumpIfNot { condition: Reg, offset: JumpOffset },

    /// Jump if equal: `if R(A) == R(B) then PC += offset`
    JumpIfEq {
        lhs: Reg,
        rhs: Reg,
        offset: JumpOffset,
    },

    /// Jump if not equal: `if R(A) != R(B) then PC += offset`
    JumpIfNeq {
        lhs: Reg,
        rhs: Reg,
        offset: JumpOffset,
    },

    // ============================================================
    // FUNCTION CALL INSTRUCTIONS
    // ============================================================
    /// Call function
    /// `R(A) ... R(A+C-1) = R(A)(R(A+1) ... R(A+B))`
    /// - R(A): function to call
    /// - R(A+1) ... R(A+B): arguments
    /// - Results returned to R(A) ... R(A+C-1)
    /// - If C == 0, use all available return values
    Call {
        func: Reg,
        arg_count: u8,
        ret_count: u8,
    },

    /// Tail call (optimized return + call)
    /// `return R(A)(R(A+1) ... R(A+B))`
    TailCall { func: Reg, arg_count: u8 },

    /// Return from function
    /// `return R(A) ... R(A+B-1)`
    /// - If B == 0, return no values
    /// - If B == 1, return R(A)
    /// - If B > 1, return multiple values
    Return { first: Reg, count: u8 },

    // ============================================================
    // CLOSURE AND UPVALUE INSTRUCTIONS
    // ============================================================
    /// Create closure
    /// `R(A) = closure(PROTO[Bx])`
    /// Prototype index Bx points to function definition in constant pool
    Closure { dest: Reg, proto_idx: ConstIdx },

    /// Get upvalue
    /// `R(A) = Upvalue[B]`
    GetUpvalue { dest: Reg, upvalue_idx: u8 },

    /// Set upvalue
    /// `Upvalue[A] = R(B)`
    SetUpvalue { upvalue_idx: u8, src: Reg },

    /// Close upvalues
    /// Close all open upvalues >= R(A)
    /// This captures values when they go out of scope
    CloseUpvalues { start: Reg },

    // ============================================================
    // GLOBAL VARIABLE INSTRUCTIONS
    // ============================================================
    /// Load global variable
    /// `R(A) = Globals[K(Bx)]`
    LoadGlobal { dest: Reg, name_idx: ConstIdx },

    /// Store global variable
    /// `Globals[K(Bx)] = R(A)`
    StoreGlobal { name_idx: ConstIdx, src: Reg },

    // ============================================================
    // DATA STRUCTURE INSTRUCTIONS
    // ============================================================
    /// Create new array
    /// `R(A) = [R(A+1), R(A+2), ..., R(A+B)]`
    /// Creates array with B elements starting from R(A+1)
    NewArray { dest: Reg, size: u8 },

    /// Get array/table element
    /// `R(A) = R(B)[R(C)]`
    GetIndex { dest: Reg, array: Reg, index: Reg },

    /// Set array/table element
    /// `R(A)[R(B)] = R(C)`
    SetIndex { array: Reg, index: Reg, value: Reg },

    /// Create new struct
    /// `R(A) = StructType[K(Bx)] { fields from R(A+1)..R(A+C) }`
    NewStruct {
        dest: Reg,
        type_idx: ConstIdx,
        field_count: u8,
    },

    /// Get struct field
    /// `R(A) = R(B).field[K(C)]`
    GetField {
        dest: Reg,
        object: Reg,
        field_idx: u8,
    },

    /// Set struct field
    /// `R(A).field[K(B)] = R(C)`
    SetField {
        object: Reg,
        field_idx: u8,
        value: Reg,
    },

    /// Create tuple
    /// `R(A) = (R(A+1), R(A+2), ..., R(A+B))`
    NewTuple { dest: Reg, size: u8 },

    /// Create enum variant
    /// `R(A) = EnumType[K(Bx)]::Variant(R(A+1)..R(A+C))`
    NewEnum {
        dest: Reg,
        variant_idx: ConstIdx,
        field_count: u8,
    },

    // ============================================================
    // PATTERN MATCHING INSTRUCTIONS
    // ============================================================
    /// Start pattern match
    /// Prepares R(A) for pattern matching
    MatchStart { value: Reg },

    /// Test pattern
    /// `if R(A) matches Pattern[K(B)] then PC += offset`
    /// Binds captured values if pattern matches
    MatchPattern {
        value: Reg,
        pattern_idx: ConstIdx,
        offset: JumpOffset,
    },

    /// End pattern match (no match found - error or default)
    MatchEnd,

    /// Extract enum variant field
    /// `R(A) = R(B).variant_field[C]`
    /// Used after successful pattern match
    ExtractField {
        dest: Reg,
        enum_value: Reg,
        field_idx: u8,
    },

    // ============================================================
    // ITERATOR INSTRUCTIONS
    // ============================================================
    /// Create iterator from iterable
    /// `R(A) = iter(R(B))`
    MakeIterator { dest: Reg, iterable: Reg },

    /// Get next iterator value
    /// `R(A) = R(B).next()`
    /// Returns Option<T>
    IteratorNext { dest: Reg, iterator: Reg },

    /// Check if iterator has next value
    /// `R(A) = R(B).has_next()`
    IteratorHasNext { dest: Reg, iterator: Reg },

    /// For-loop iterator setup
    /// Specialized instruction for for-in loops
    /// Sets up iterator and prepares loop
    ForIterator {
        iterator: Reg,
        loop_var: Reg,
        offset: JumpOffset,
    },

    // ============================================================
    // TYPE INSTRUCTIONS
    // ============================================================
    /// Type check
    /// `assert(R(A) is Type[K(B)])`
    /// Throws error if type check fails
    TypeCheck { value: Reg, type_idx: ConstIdx },

    /// Type cast
    /// `R(A) = R(B) as Type[K(C)]`
    TypeCast {
        dest: Reg,
        src: Reg,
        type_idx: ConstIdx,
    },

    /// Get type of value
    /// `R(A) = typeof(R(B))`
    TypeOf { dest: Reg, value: Reg },

    // ============================================================
    // EXCEPTION HANDLING INSTRUCTIONS
    // ============================================================
    /// Try block start
    /// Sets up exception handler at PC + offset
    TryStart { handler_offset: JumpOffset },

    /// Try block end
    /// Clears exception handler
    TryEnd,

    /// Throw exception
    /// `throw R(A)`
    Throw { value: Reg },

    /// Catch exception
    /// `R(A) = caught_exception`
    Catch { dest: Reg },

    // ============================================================
    // MISCELLANEOUS INSTRUCTIONS
    // ============================================================
    /// Print value (debugging)
    /// `print(R(A))`
    Print { value: Reg },

    /// Halt execution
    Halt,

    /// No operation
    Nop,

    /// Import module
    /// `R(A) = import(K(Bx))`
    Import { dest: Reg, module_idx: ConstIdx },

    /// Assert/debug breakpoint
    /// Used for debugging and assertions
    Assert {
        condition: Reg,
        message_idx: ConstIdx,
    },
}

impl Instruction {
    /// Get the size of the instruction in bytes
    pub fn size(&self) -> usize {
        4 // All instructions are 32-bit (4 bytes) in our fixed-width encoding
    }

    /// Check if instruction is a control flow instruction
    pub fn is_control_flow(&self) -> bool {
        matches!(
            self,
            Instruction::Jump { .. }
                | Instruction::JumpIf { .. }
                | Instruction::JumpIfNot { .. }
                | Instruction::JumpIfEq { .. }
                | Instruction::JumpIfNeq { .. }
                | Instruction::Call { .. }
                | Instruction::TailCall { .. }
                | Instruction::Return { .. }
                | Instruction::Throw { .. }
        )
    }

    /// Check if instruction can throw an exception
    pub fn can_throw(&self) -> bool {
        matches!(
            self,
            Instruction::Div { .. }
                | Instruction::DivK { .. }
                | Instruction::Mod { .. }
                | Instruction::ModK { .. }
                | Instruction::GetIndex { .. }
                | Instruction::SetIndex { .. }
                | Instruction::GetField { .. }
                | Instruction::SetField { .. }
                | Instruction::TypeCheck { .. }
                | Instruction::TypeCast { .. }
                | Instruction::Throw { .. }
                | Instruction::Assert { .. }
        )
    }

    /// Get the destination register, if any
    pub fn dest_register(&self) -> Option<Reg> {
        match self {
            Instruction::Move { dest, .. }
            | Instruction::LoadConst { dest, .. }
            | Instruction::LoadBool { dest, .. }
            | Instruction::LoadNil { dest }
            | Instruction::Add { dest, .. }
            | Instruction::AddK { dest, .. }
            | Instruction::Sub { dest, .. }
            | Instruction::SubK { dest, .. }
            | Instruction::Mul { dest, .. }
            | Instruction::MulK { dest, .. }
            | Instruction::Div { dest, .. }
            | Instruction::DivK { dest, .. }
            | Instruction::Mod { dest, .. }
            | Instruction::ModK { dest, .. }
            | Instruction::Pow { dest, .. }
            | Instruction::Neg { dest, .. }
            | Instruction::Eq { dest, .. }
            | Instruction::Neq { dest, .. }
            | Instruction::Lt { dest, .. }
            | Instruction::Le { dest, .. }
            | Instruction::Gt { dest, .. }
            | Instruction::Ge { dest, .. }
            | Instruction::And { dest, .. }
            | Instruction::Or { dest, .. }
            | Instruction::Not { dest, .. }
            | Instruction::BitAnd { dest, .. }
            | Instruction::BitOr { dest, .. }
            | Instruction::BitXor { dest, .. }
            | Instruction::BitNot { dest, .. }
            | Instruction::Shl { dest, .. }
            | Instruction::Shr { dest, .. }
            | Instruction::Closure { dest, .. }
            | Instruction::GetUpvalue { dest, .. }
            | Instruction::LoadGlobal { dest, .. }
            | Instruction::NewArray { dest, .. }
            | Instruction::GetIndex { dest, .. }
            | Instruction::NewStruct { dest, .. }
            | Instruction::GetField { dest, .. }
            | Instruction::NewTuple { dest, .. }
            | Instruction::NewEnum { dest, .. }
            | Instruction::ExtractField { dest, .. }
            | Instruction::MakeIterator { dest, .. }
            | Instruction::IteratorNext { dest, .. }
            | Instruction::IteratorHasNext { dest, .. }
            | Instruction::TypeCast { dest, .. }
            | Instruction::TypeOf { dest, .. }
            | Instruction::Catch { dest }
            | Instruction::Import { dest, .. } => Some(*dest),
            _ => None,
        }
    }

    /// Get all source registers used by this instruction
    pub fn source_registers(&self) -> Vec<Reg> {
        match self {
            Instruction::Move { src, .. } => vec![*src],
            Instruction::Add { lhs, rhs, .. }
            | Instruction::Sub { lhs, rhs, .. }
            | Instruction::Mul { lhs, rhs, .. }
            | Instruction::Div { lhs, rhs, .. }
            | Instruction::Mod { lhs, rhs, .. }
            | Instruction::Pow { lhs, rhs, .. }
            | Instruction::Eq { lhs, rhs, .. }
            | Instruction::Neq { lhs, rhs, .. }
            | Instruction::Lt { lhs, rhs, .. }
            | Instruction::Le { lhs, rhs, .. }
            | Instruction::Gt { lhs, rhs, .. }
            | Instruction::Ge { lhs, rhs, .. }
            | Instruction::And { lhs, rhs, .. }
            | Instruction::Or { lhs, rhs, .. }
            | Instruction::BitAnd { lhs, rhs, .. }
            | Instruction::BitOr { lhs, rhs, .. }
            | Instruction::BitXor { lhs, rhs, .. }
            | Instruction::Shl { lhs, rhs, .. }
            | Instruction::Shr { lhs, rhs, .. } => vec![*lhs, *rhs],
            Instruction::AddK { lhs, .. }
            | Instruction::SubK { lhs, .. }
            | Instruction::MulK { lhs, .. }
            | Instruction::DivK { lhs, .. }
            | Instruction::ModK { lhs, .. }
            | Instruction::Neg { src: lhs, .. }
            | Instruction::Not { src: lhs, .. }
            | Instruction::BitNot { src: lhs, .. } => vec![*lhs],
            Instruction::SetIndex {
                array,
                index,
                value,
            } => vec![*array, *index, *value],
            Instruction::SetField {
                object,
                field_idx: _,
                value,
            } => vec![*object, *value],
            Instruction::GetIndex { array, index, .. } => vec![*array, *index],
            Instruction::Return { first, count } => {
                let mut regs = Vec::new();
                for i in 0..*count {
                    regs.push(first + i);
                }
                regs
            }
            _ => Vec::new(),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Move { dest, src } => write!(f, "MOVE R{} R{}", dest, src),
            Instruction::LoadConst { dest, const_idx } => {
                write!(f, "LOADK R{} K{}", dest, const_idx)
            }
            Instruction::LoadBool { dest, value } => write!(f, "LOADBOOL R{} {}", dest, value),
            Instruction::LoadNil { dest } => write!(f, "LOADNIL R{}", dest),
            Instruction::LoadNilRange { start, count } => {
                write!(f, "LOADNIL R{}..R{}", start, start + count)
            }
            Instruction::Add { dest, lhs, rhs } => {
                write!(f, "ADD R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::AddK {
                dest,
                lhs,
                const_idx,
            } => {
                write!(f, "ADDK R{} R{} K{}", dest, lhs, const_idx)
            }
            Instruction::Sub { dest, lhs, rhs } => {
                write!(f, "SUB R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::SubK {
                dest,
                lhs,
                const_idx,
            } => {
                write!(f, "SUBK R{} R{} K{}", dest, lhs, const_idx)
            }
            Instruction::Mul { dest, lhs, rhs } => {
                write!(f, "MUL R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::MulK {
                dest,
                lhs,
                const_idx,
            } => {
                write!(f, "MULK R{} R{} K{}", dest, lhs, const_idx)
            }
            Instruction::Div { dest, lhs, rhs } => {
                write!(f, "DIV R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::DivK {
                dest,
                lhs,
                const_idx,
            } => {
                write!(f, "DIVK R{} R{} K{}", dest, lhs, const_idx)
            }
            Instruction::Mod { dest, lhs, rhs } => {
                write!(f, "MOD R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::ModK {
                dest,
                lhs,
                const_idx,
            } => {
                write!(f, "MODK R{} R{} K{}", dest, lhs, const_idx)
            }
            Instruction::Pow { dest, lhs, rhs } => {
                write!(f, "POW R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::Neg { dest, src } => write!(f, "NEG R{} R{}", dest, src),
            Instruction::Eq { dest, lhs, rhs } => write!(f, "EQ R{} R{} R{}", dest, lhs, rhs),
            Instruction::Neq { dest, lhs, rhs } => write!(f, "NEQ R{} R{} R{}", dest, lhs, rhs),
            Instruction::Lt { dest, lhs, rhs } => write!(f, "LT R{} R{} R{}", dest, lhs, rhs),
            Instruction::Le { dest, lhs, rhs } => write!(f, "LE R{} R{} R{}", dest, lhs, rhs),
            Instruction::Gt { dest, lhs, rhs } => write!(f, "GT R{} R{} R{}", dest, lhs, rhs),
            Instruction::Ge { dest, lhs, rhs } => write!(f, "GE R{} R{} R{}", dest, lhs, rhs),
            Instruction::And { dest, lhs, rhs } => write!(f, "AND R{} R{} R{}", dest, lhs, rhs),
            Instruction::Or { dest, lhs, rhs } => write!(f, "OR R{} R{} R{}", dest, lhs, rhs),
            Instruction::Not { dest, src } => write!(f, "NOT R{} R{}", dest, src),
            Instruction::BitAnd { dest, lhs, rhs } => {
                write!(f, "BITAND R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::BitOr { dest, lhs, rhs } => {
                write!(f, "BITOR R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::BitXor { dest, lhs, rhs } => {
                write!(f, "BITXOR R{} R{} R{}", dest, lhs, rhs)
            }
            Instruction::BitNot { dest, src } => write!(f, "BITNOT R{} R{}", dest, src),
            Instruction::Shl { dest, lhs, rhs } => write!(f, "SHL R{} R{} R{}", dest, lhs, rhs),
            Instruction::Shr { dest, lhs, rhs } => write!(f, "SHR R{} R{} R{}", dest, lhs, rhs),
            Instruction::Jump { offset } => write!(f, "JUMP {:+}", offset),
            Instruction::JumpIf { condition, offset } => {
                write!(f, "JUMPIF R{} {:+}", condition, offset)
            }
            Instruction::JumpIfNot { condition, offset } => {
                write!(f, "JUMPIFNOT R{} {:+}", condition, offset)
            }
            Instruction::JumpIfEq { lhs, rhs, offset } => {
                write!(f, "JUMPIFEQ R{} R{} {:+}", lhs, rhs, offset)
            }
            Instruction::JumpIfNeq { lhs, rhs, offset } => {
                write!(f, "JUMPIFNEQ R{} R{} {:+}", lhs, rhs, offset)
            }
            Instruction::Call {
                func,
                arg_count,
                ret_count,
            } => {
                write!(f, "CALL R{} {} {}", func, arg_count, ret_count)
            }
            Instruction::TailCall { func, arg_count } => {
                write!(f, "TAILCALL R{} {}", func, arg_count)
            }
            Instruction::Return { first, count } => write!(f, "RETURN R{} {}", first, count),
            Instruction::Closure { dest, proto_idx } => {
                write!(f, "CLOSURE R{} P{}", dest, proto_idx)
            }
            Instruction::GetUpvalue { dest, upvalue_idx } => {
                write!(f, "GETUPVAL R{} U{}", dest, upvalue_idx)
            }
            Instruction::SetUpvalue { upvalue_idx, src } => {
                write!(f, "SETUPVAL U{} R{}", upvalue_idx, src)
            }
            Instruction::CloseUpvalues { start } => write!(f, "CLOSE R{}", start),
            Instruction::LoadGlobal { dest, name_idx } => {
                write!(f, "GETGLOBAL R{} K{}", dest, name_idx)
            }
            Instruction::StoreGlobal { name_idx, src } => {
                write!(f, "SETGLOBAL K{} R{}", name_idx, src)
            }
            Instruction::NewArray { dest, size } => write!(f, "NEWARRAY R{} {}", dest, size),
            Instruction::GetIndex { dest, array, index } => {
                write!(f, "GETINDEX R{} R{} R{}", dest, array, index)
            }
            Instruction::SetIndex {
                array,
                index,
                value,
            } => {
                write!(f, "SETINDEX R{} R{} R{}", array, index, value)
            }
            Instruction::NewStruct {
                dest,
                type_idx,
                field_count,
            } => {
                write!(f, "NEWSTRUCT R{} K{} {}", dest, type_idx, field_count)
            }
            Instruction::GetField {
                dest,
                object,
                field_idx,
            } => {
                write!(f, "GETFIELD R{} R{} F{}", dest, object, field_idx)
            }
            Instruction::SetField {
                object,
                field_idx,
                value,
            } => {
                write!(f, "SETFIELD R{} F{} R{}", object, field_idx, value)
            }
            Instruction::NewTuple { dest, size } => write!(f, "NEWTUPLE R{} {}", dest, size),
            Instruction::NewEnum {
                dest,
                variant_idx,
                field_count,
            } => {
                write!(f, "NEWENUM R{} K{} {}", dest, variant_idx, field_count)
            }
            Instruction::MatchStart { value } => write!(f, "MATCHSTART R{}", value),
            Instruction::MatchPattern {
                value,
                pattern_idx,
                offset,
            } => {
                write!(f, "MATCHPAT R{} K{} {:+}", value, pattern_idx, offset)
            }
            Instruction::MatchEnd => write!(f, "MATCHEND"),
            Instruction::ExtractField {
                dest,
                enum_value,
                field_idx,
            } => {
                write!(f, "EXTRACT R{} R{} F{}", dest, enum_value, field_idx)
            }
            Instruction::MakeIterator { dest, iterable } => {
                write!(f, "MAKEITER R{} R{}", dest, iterable)
            }
            Instruction::IteratorNext { dest, iterator } => {
                write!(f, "ITERNEXT R{} R{}", dest, iterator)
            }
            Instruction::IteratorHasNext { dest, iterator } => {
                write!(f, "ITERHASNEXT R{} R{}", dest, iterator)
            }
            Instruction::ForIterator {
                iterator,
                loop_var,
                offset,
            } => {
                write!(f, "FORITER R{} R{} {:+}", iterator, loop_var, offset)
            }
            Instruction::TypeCheck { value, type_idx } => {
                write!(f, "TYPECHECK R{} K{}", value, type_idx)
            }
            Instruction::TypeCast {
                dest,
                src,
                type_idx,
            } => {
                write!(f, "TYPECAST R{} R{} K{}", dest, src, type_idx)
            }
            Instruction::TypeOf { dest, value } => write!(f, "TYPEOF R{} R{}", dest, value),
            Instruction::TryStart { handler_offset } => write!(f, "TRYSTART {:+}", handler_offset),
            Instruction::TryEnd => write!(f, "TRYEND"),
            Instruction::Throw { value } => write!(f, "THROW R{}", value),
            Instruction::Catch { dest } => write!(f, "CATCH R{}", dest),
            Instruction::Print { value } => write!(f, "PRINT R{}", value),
            Instruction::Halt => write!(f, "HALT"),
            Instruction::Nop => write!(f, "NOP"),
            Instruction::Import { dest, module_idx } => {
                write!(f, "IMPORT R{} K{}", dest, module_idx)
            }
            Instruction::Assert {
                condition,
                message_idx,
            } => {
                write!(f, "ASSERT R{} K{}", condition, message_idx)
            }
        }
    }
}

/// Function prototype - represents a function definition
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionProto {
    /// Function name (for debugging)
    pub name: String,

    /// Number of parameters
    pub param_count: u8,

    /// Number of registers needed
    pub register_count: u8,

    /// Instructions
    pub instructions: Vec<Instruction>,

    /// Constant pool
    pub constants: Vec<Constant>,

    /// Line number information (for debugging)
    pub line_info: Vec<u32>,

    /// Nested function prototypes (for closures)
    pub prototypes: Vec<FunctionProto>,

    /// Upvalue information
    pub upvalues: Vec<UpvalueInfo>,

    /// Is this a variadic function?
    pub is_variadic: bool,
}

impl FunctionProto {
    /// Create a new function prototype
    pub fn new(name: String, param_count: u8) -> Self {
        Self {
            name,
            param_count,
            register_count: param_count, // Start with params
            instructions: Vec::new(),
            constants: Vec::new(),
            line_info: Vec::new(),
            prototypes: Vec::new(),
            upvalues: Vec::new(),
            is_variadic: false,
        }
    }

    /// Add an instruction and return its index
    pub fn add_instruction(&mut self, instr: Instruction, line: u32) -> usize {
        let idx = self.instructions.len();
        self.instructions.push(instr);
        self.line_info.push(line);
        idx
    }

    /// Add a constant and return its index
    pub fn add_constant(&mut self, constant: Constant) -> ConstIdx {
        // Check if constant already exists
        for (i, c) in self.constants.iter().enumerate() {
            if c == &constant {
                return i as ConstIdx;
            }
        }

        let idx = self.constants.len();
        self.constants.push(constant);
        idx as ConstIdx
    }

    /// Add a nested function prototype
    pub fn add_prototype(&mut self, proto: FunctionProto) -> ConstIdx {
        let idx = self.prototypes.len();
        self.prototypes.push(proto);
        idx as ConstIdx
    }

    /// Get instruction at index
    pub fn get_instruction(&self, idx: usize) -> Option<&Instruction> {
        self.instructions.get(idx)
    }

    /// Get constant at index
    pub fn get_constant(&self, idx: ConstIdx) -> Option<&Constant> {
        self.constants.get(idx as usize)
    }

    /// Patch a jump instruction at the given index
    pub fn patch_jump(&mut self, idx: usize, offset: JumpOffset) -> Result<(), String> {
        if idx >= self.instructions.len() {
            return Err(format!("Invalid instruction index: {}", idx));
        }

        let instr = &mut self.instructions[idx];
        match instr {
            Instruction::Jump { offset: o } => *o = offset,
            Instruction::JumpIf { offset: o, .. } => *o = offset,
            Instruction::JumpIfNot { offset: o, .. } => *o = offset,
            Instruction::JumpIfEq { offset: o, .. } => *o = offset,
            Instruction::JumpIfNeq { offset: o, .. } => *o = offset,
            _ => return Err("Instruction is not a jump".to_string()),
        }

        Ok(())
    }

    /// Allocate registers (update register count if needed)
    pub fn allocate_registers(&mut self, count: u8) {
        let needed = self.param_count + count;
        if needed > self.register_count {
            self.register_count = needed;
        }
    }
}

impl Default for FunctionProto {
    fn default() -> Self {
        Self::new("<main>".to_string(), 0)
    }
}

/// Constant value in the constant pool
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Nil/Unit value
    Nil,

    /// Boolean value
    Boolean(bool),

    /// Integer value
    Integer(i64),

    /// Float value
    Float(f64),

    /// String value
    String(String),

    /// Function prototype
    Function(Box<FunctionProto>),

    /// Type name
    Type(String),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Nil => write!(f, "nil"),
            Constant::Boolean(b) => write!(f, "{}", b),
            Constant::Integer(i) => write!(f, "{}", i),
            Constant::Float(fl) => write!(f, "{}", fl),
            Constant::String(s) => write!(f, "{:?}", s),
            Constant::Function(proto) => write!(f, "<function {}>", proto.name),
            Constant::Type(t) => write!(f, "<type {}>", t),
        }
    }
}

/// Upvalue information
#[derive(Debug, Clone, PartialEq)]
pub struct UpvalueInfo {
    /// Register index (in parent frame)
    pub register: Reg,

    /// Is this upvalue in the parent frame (true) or parent's upvalue (false)?
    pub is_local: bool,

    /// Upvalue name (for debugging)
    pub name: String,
}

/// Bytecode chunk - top-level container
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    /// Main function prototype
    pub main: FunctionProto,

    /// Source file name
    pub source_file: String,

    /// Chunk metadata
    pub metadata: ChunkMetadata,
}

impl Chunk {
    /// Create a new chunk
    pub fn new(source_file: String) -> Self {
        Self {
            main: FunctionProto::default(),
            source_file,
            metadata: ChunkMetadata::default(),
        }
    }

    /// Create chunk with a specific main function name
    pub fn with_name(source_file: String, name: String) -> Self {
        Self {
            main: FunctionProto::new(name, 0),
            source_file,
            metadata: ChunkMetadata::default(),
        }
    }

    /// Disassemble the chunk for debugging
    pub fn disassemble(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("=== {} ===\n", self.source_file));
        output.push_str(&self.disassemble_function(&self.main, 0));
        output
    }

    fn disassemble_function(&self, func: &FunctionProto, indent: usize) -> String {
        let mut output = String::new();
        let indent_str = "  ".repeat(indent);

        output.push_str(&format!(
            "{}function {} ({} params, {} registers)\n",
            indent_str, func.name, func.param_count, func.register_count
        ));

        // Constants
        if !func.constants.is_empty() {
            output.push_str(&format!("{}  Constants:\n", indent_str));
            for (i, c) in func.constants.iter().enumerate() {
                output.push_str(&format!("{}    K{}: {}\n", indent_str, i, c));
            }
        }

        // Upvalues
        if !func.upvalues.is_empty() {
            output.push_str(&format!("{}  Upvalues:\n", indent_str));
            for (i, u) in func.upvalues.iter().enumerate() {
                output.push_str(&format!(
                    "{}    U{}: {} (R{}, local={})\n",
                    indent_str, i, u.name, u.register, u.is_local
                ));
            }
        }

        // Instructions
        output.push_str(&format!("{}  Code:\n", indent_str));
        for (i, instr) in func.instructions.iter().enumerate() {
            let line = func.line_info.get(i).copied().unwrap_or(0);
            output.push_str(&format!(
                "{}    {:04}  {:>4}  {}\n",
                indent_str, i, line, instr
            ));
        }

        // Nested functions
        for proto in &func.prototypes {
            output.push_str(&self.disassemble_function(proto, indent + 1));
        }

        output
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new("<unknown>".to_string())
    }
}

/// Chunk metadata
#[derive(Debug, Clone, PartialEq)]
pub struct ChunkMetadata {
    /// Veld version that compiled this chunk
    pub version: String,

    /// Compilation timestamp
    pub timestamp: u64,

    /// Optimization level
    pub optimization_level: u8,

    /// Debug information included?
    pub has_debug_info: bool,
}

impl Default for ChunkMetadata {
    fn default() -> Self {
        Self {
            version: env!("CARGO_PKG_VERSION").to_string(),
            timestamp: 0,
            optimization_level: 0,
            has_debug_info: true,
        }
    }
}

/// Builder for creating chunks
pub struct ChunkBuilder {
    chunk: Chunk,
    current_line: u32,
}

impl ChunkBuilder {
    /// Create a new chunk builder
    pub fn new() -> Self {
        Self {
            chunk: Chunk::default(),
            current_line: 1,
        }
    }

    /// Set source file
    pub fn source_file(mut self, source_file: String) -> Self {
        self.chunk.source_file = source_file;
        self
    }

    /// Set main function name
    pub fn main_name(mut self, name: String) -> Self {
        self.chunk.main.name = name;
        self
    }

    /// Set optimization level
    pub fn optimization_level(mut self, level: u8) -> Self {
        self.chunk.metadata.optimization_level = level;
        self
    }

    /// Enable/disable debug info
    pub fn debug_info(mut self, enabled: bool) -> Self {
        self.chunk.metadata.has_debug_info = enabled;
        self
    }

    /// Set number of registers needed
    pub fn register_count(&mut self, count: u8) -> &mut Self {
        self.chunk.main.register_count = count;
        self
    }

    /// Set current line number (for debug info)
    pub fn line(&mut self, line: u32) -> &mut Self {
        self.current_line = line;
        self
    }

    /// Add a constant to the constant pool and return its index
    pub fn add_constant(&mut self, constant: Constant) -> ConstIdx {
        // Check if constant already exists (deduplication)
        for (i, existing) in self.chunk.main.constants.iter().enumerate() {
            if existing == &constant {
                return i as ConstIdx;
            }
        }
        let idx = self.chunk.main.constants.len() as ConstIdx;
        self.chunk.main.constants.push(constant);
        idx
    }

    /// Add an instruction
    fn emit(&mut self, instr: Instruction) -> &mut Self {
        self.chunk.main.instructions.push(instr);
        self.chunk.main.line_info.push(self.current_line);
        self
    }

    // ============================================================
    // MOVE AND LOAD INSTRUCTIONS
    // ============================================================

    /// Emit Move instruction
    pub fn move_reg(&mut self, dest: Reg, src: Reg) -> &mut Self {
        self.emit(Instruction::Move { dest, src })
    }

    /// Emit LoadConst instruction
    pub fn load_const(&mut self, dest: Reg, const_idx: ConstIdx) -> &mut Self {
        self.emit(Instruction::LoadConst { dest, const_idx })
    }

    /// Emit LoadBool instruction
    pub fn load_bool(&mut self, dest: Reg, value: bool) -> &mut Self {
        self.emit(Instruction::LoadBool { dest, value })
    }

    /// Emit LoadNil instruction
    pub fn load_nil(&mut self, dest: Reg) -> &mut Self {
        self.emit(Instruction::LoadNil { dest })
    }

    /// Emit LoadNilRange instruction
    pub fn load_nil_range(&mut self, start: Reg, count: u8) -> &mut Self {
        self.emit(Instruction::LoadNilRange { start, count })
    }

    // ============================================================
    // ARITHMETIC INSTRUCTIONS
    // ============================================================

    /// Emit Add instruction
    pub fn add(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Add { dest, lhs, rhs })
    }

    /// Emit AddK instruction
    pub fn add_k(&mut self, dest: Reg, lhs: Reg, const_idx: u8) -> &mut Self {
        self.emit(Instruction::AddK {
            dest,
            lhs,
            const_idx,
        })
    }

    /// Emit Sub instruction
    pub fn sub(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Sub { dest, lhs, rhs })
    }

    /// Emit SubK instruction
    pub fn sub_k(&mut self, dest: Reg, lhs: Reg, const_idx: u8) -> &mut Self {
        self.emit(Instruction::SubK {
            dest,
            lhs,
            const_idx,
        })
    }

    /// Emit Mul instruction
    pub fn mul(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Mul { dest, lhs, rhs })
    }

    /// Emit MulK instruction
    pub fn mul_k(&mut self, dest: Reg, lhs: Reg, const_idx: u8) -> &mut Self {
        self.emit(Instruction::MulK {
            dest,
            lhs,
            const_idx,
        })
    }

    /// Emit Div instruction
    pub fn div(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Div { dest, lhs, rhs })
    }

    /// Emit DivK instruction
    pub fn div_k(&mut self, dest: Reg, lhs: Reg, const_idx: u8) -> &mut Self {
        self.emit(Instruction::DivK {
            dest,
            lhs,
            const_idx,
        })
    }

    /// Emit Mod instruction
    pub fn mod_op(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Mod { dest, lhs, rhs })
    }

    /// Emit ModK instruction
    pub fn mod_k(&mut self, dest: Reg, lhs: Reg, const_idx: u8) -> &mut Self {
        self.emit(Instruction::ModK {
            dest,
            lhs,
            const_idx,
        })
    }

    /// Emit Pow instruction
    pub fn pow(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Pow { dest, lhs, rhs })
    }

    /// Emit Neg instruction
    pub fn neg(&mut self, dest: Reg, src: Reg) -> &mut Self {
        self.emit(Instruction::Neg { dest, src })
    }

    // ============================================================
    // COMPARISON INSTRUCTIONS
    // ============================================================

    /// Emit Eq instruction
    pub fn eq(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Eq { dest, lhs, rhs })
    }

    /// Emit Neq instruction
    pub fn neq(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Neq { dest, lhs, rhs })
    }

    /// Emit Lt instruction
    pub fn lt(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Lt { dest, lhs, rhs })
    }

    /// Emit Le instruction
    pub fn le(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Le { dest, lhs, rhs })
    }

    /// Emit Gt instruction
    pub fn gt(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Gt { dest, lhs, rhs })
    }

    /// Emit Ge instruction
    pub fn ge(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Ge { dest, lhs, rhs })
    }

    // ============================================================
    // LOGICAL INSTRUCTIONS
    // ============================================================

    /// Emit And instruction
    pub fn and(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::And { dest, lhs, rhs })
    }

    /// Emit Or instruction
    pub fn or(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Or { dest, lhs, rhs })
    }

    /// Emit Not instruction
    pub fn not(&mut self, dest: Reg, src: Reg) -> &mut Self {
        self.emit(Instruction::Not { dest, src })
    }

    // ============================================================
    // BITWISE INSTRUCTIONS
    // ============================================================

    /// Emit BitAnd instruction
    pub fn bit_and(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::BitAnd { dest, lhs, rhs })
    }

    /// Emit BitOr instruction
    pub fn bit_or(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::BitOr { dest, lhs, rhs })
    }

    /// Emit BitXor instruction
    pub fn bit_xor(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::BitXor { dest, lhs, rhs })
    }

    /// Emit BitNot instruction
    pub fn bit_not(&mut self, dest: Reg, src: Reg) -> &mut Self {
        self.emit(Instruction::BitNot { dest, src })
    }

    /// Emit Shl instruction
    pub fn shl(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Shl { dest, lhs, rhs })
    }

    /// Emit Shr instruction
    pub fn shr(&mut self, dest: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.emit(Instruction::Shr { dest, lhs, rhs })
    }

    // ============================================================
    // CONTROL FLOW INSTRUCTIONS
    // ============================================================

    /// Emit Jump instruction and return instruction index for patching
    pub fn jump(&mut self, offset: JumpOffset) -> usize {
        let idx = self.chunk.main.instructions.len();
        self.emit(Instruction::Jump { offset });
        idx
    }

    /// Emit JumpIf instruction
    pub fn jump_if(&mut self, condition: Reg, offset: JumpOffset) -> usize {
        let idx = self.chunk.main.instructions.len();
        self.emit(Instruction::JumpIf { condition, offset });
        idx
    }

    /// Emit JumpIfNot instruction
    pub fn jump_if_not(&mut self, condition: Reg, offset: JumpOffset) -> usize {
        let idx = self.chunk.main.instructions.len();
        self.emit(Instruction::JumpIfNot { condition, offset });
        idx
    }

    /// Emit JumpIfEq instruction
    pub fn jump_if_eq(&mut self, lhs: Reg, rhs: Reg, offset: JumpOffset) -> usize {
        let idx = self.chunk.main.instructions.len();
        self.emit(Instruction::JumpIfEq { lhs, rhs, offset });
        idx
    }

    /// Emit JumpIfNeq instruction
    pub fn jump_if_neq(&mut self, lhs: Reg, rhs: Reg, offset: JumpOffset) -> usize {
        let idx = self.chunk.main.instructions.len();
        self.emit(Instruction::JumpIfNeq { lhs, rhs, offset });
        idx
    }

    /// Patch a jump instruction at the given index with the correct offset
    pub fn patch_jump(&mut self, jump_idx: usize) {
        let offset = (self.chunk.main.instructions.len() as i32 - jump_idx as i32 - 1) as i16;
        match &mut self.chunk.main.instructions[jump_idx] {
            Instruction::Jump { offset: o } => *o = offset,
            Instruction::JumpIf { offset: o, .. } => *o = offset,
            Instruction::JumpIfNot { offset: o, .. } => *o = offset,
            Instruction::JumpIfEq { offset: o, .. } => *o = offset,
            Instruction::JumpIfNeq { offset: o, .. } => *o = offset,
            _ => panic!("Attempted to patch non-jump instruction"),
        }
    }

    /// Get current instruction index (for loop jumps)
    pub fn current_index(&self) -> usize {
        self.chunk.main.instructions.len()
    }

    /// Emit a backward jump to a specific instruction index
    pub fn jump_back(&mut self, target_idx: usize) -> &mut Self {
        let offset = (target_idx as i32 - self.chunk.main.instructions.len() as i32 - 1) as i16;
        self.emit(Instruction::Jump { offset })
    }

    // ============================================================
    // FUNCTION CALL INSTRUCTIONS
    // ============================================================

    /// Emit Call instruction
    pub fn call(&mut self, func: Reg, arg_count: u8, ret_count: u8) -> &mut Self {
        self.emit(Instruction::Call {
            func,
            arg_count,
            ret_count,
        })
    }

    /// Emit TailCall instruction
    pub fn tail_call(&mut self, func: Reg, arg_count: u8) -> &mut Self {
        self.emit(Instruction::TailCall { func, arg_count })
    }

    /// Emit Return instruction
    pub fn return_vals(&mut self, first: Reg, count: u8) -> &mut Self {
        self.emit(Instruction::Return { first, count })
    }

    // ============================================================
    // GLOBAL VARIABLE INSTRUCTIONS
    // ============================================================

    /// Emit LoadGlobal instruction
    pub fn load_global(&mut self, dest: Reg, name_idx: ConstIdx) -> &mut Self {
        self.emit(Instruction::LoadGlobal { dest, name_idx })
    }

    /// Emit StoreGlobal instruction
    pub fn store_global(&mut self, name_idx: ConstIdx, src: Reg) -> &mut Self {
        self.emit(Instruction::StoreGlobal { name_idx, src })
    }

    // ============================================================
    // CLOSURE AND UPVALUE INSTRUCTIONS
    // ============================================================

    /// Emit Closure instruction
    pub fn closure(&mut self, dest: Reg, proto_idx: ConstIdx) -> &mut Self {
        self.emit(Instruction::Closure { dest, proto_idx })
    }

    /// Emit GetUpvalue instruction
    pub fn get_upvalue(&mut self, dest: Reg, upvalue_idx: u8) -> &mut Self {
        self.emit(Instruction::GetUpvalue { dest, upvalue_idx })
    }

    /// Emit SetUpvalue instruction
    pub fn set_upvalue(&mut self, upvalue_idx: u8, src: Reg) -> &mut Self {
        self.emit(Instruction::SetUpvalue { upvalue_idx, src })
    }

    /// Emit CloseUpvalues instruction
    pub fn close_upvalues(&mut self, start: Reg) -> &mut Self {
        self.emit(Instruction::CloseUpvalues { start })
    }

    // ============================================================
    // DATA STRUCTURE INSTRUCTIONS
    // ============================================================

    /// Emit NewArray instruction
    pub fn new_array(&mut self, dest: Reg, size: u8) -> &mut Self {
        self.emit(Instruction::NewArray { dest, size })
    }

    /// Emit GetIndex instruction
    pub fn get_index(&mut self, dest: Reg, array: Reg, index: Reg) -> &mut Self {
        self.emit(Instruction::GetIndex { dest, array, index })
    }

    /// Emit SetIndex instruction
    pub fn set_index(&mut self, array: Reg, index: Reg, value: Reg) -> &mut Self {
        self.emit(Instruction::SetIndex {
            array,
            index,
            value,
        })
    }

    /// Emit NewStruct instruction
    pub fn new_struct(&mut self, dest: Reg, type_idx: ConstIdx, field_count: u8) -> &mut Self {
        self.emit(Instruction::NewStruct {
            dest,
            type_idx,
            field_count,
        })
    }

    /// Emit GetField instruction
    pub fn get_field(&mut self, dest: Reg, object: Reg, field_idx: u8) -> &mut Self {
        self.emit(Instruction::GetField {
            dest,
            object,
            field_idx,
        })
    }

    /// Emit SetField instruction
    pub fn set_field(&mut self, object: Reg, field_idx: u8, value: Reg) -> &mut Self {
        self.emit(Instruction::SetField {
            object,
            field_idx,
            value,
        })
    }

    /// Emit NewTuple instruction
    pub fn new_tuple(&mut self, dest: Reg, size: u8) -> &mut Self {
        self.emit(Instruction::NewTuple { dest, size })
    }

    /// Emit NewEnum instruction
    pub fn new_enum(&mut self, dest: Reg, variant_idx: ConstIdx, field_count: u8) -> &mut Self {
        self.emit(Instruction::NewEnum {
            dest,
            variant_idx,
            field_count,
        })
    }

    // ============================================================
    // TYPE INSTRUCTIONS
    // ============================================================

    /// Emit TypeOf instruction
    pub fn type_of(&mut self, dest: Reg, value: Reg) -> &mut Self {
        self.emit(Instruction::TypeOf { dest, value })
    }

    /// Emit TypeCheck instruction
    pub fn type_check(&mut self, value: Reg, type_idx: ConstIdx) -> &mut Self {
        self.emit(Instruction::TypeCheck { value, type_idx })
    }

    /// Emit TypeCast instruction
    pub fn type_cast(&mut self, dest: Reg, src: Reg, type_idx: ConstIdx) -> &mut Self {
        self.emit(Instruction::TypeCast {
            dest,
            src,
            type_idx,
        })
    }

    // ============================================================
    // MISCELLANEOUS INSTRUCTIONS
    // ============================================================

    /// Emit Print instruction
    pub fn print(&mut self, value: Reg) -> &mut Self {
        self.emit(Instruction::Print { value })
    }

    /// Emit Halt instruction
    pub fn halt(&mut self) -> &mut Self {
        self.emit(Instruction::Halt)
    }

    /// Emit Nop instruction
    pub fn nop(&mut self) -> &mut Self {
        self.emit(Instruction::Nop)
    }

    /// Emit Assert instruction
    pub fn assert(&mut self, condition: Reg, message_idx: ConstIdx) -> &mut Self {
        self.emit(Instruction::Assert {
            condition,
            message_idx,
        })
    }

    /// Build the chunk
    pub fn build(self) -> Chunk {
        self.chunk
    }
}

impl Default for ChunkBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_size() {
        let instr = Instruction::Add {
            dest: 0,
            lhs: 1,
            rhs: 2,
        };
        assert_eq!(instr.size(), 4);
    }

    #[test]
    fn test_control_flow_detection() {
        assert!(Instruction::Jump { offset: 10 }.is_control_flow());
        assert!(
            Instruction::Call {
                func: 0,
                arg_count: 2,
                ret_count: 1
            }
            .is_control_flow()
        );
        assert!(
            !Instruction::Add {
                dest: 0,
                lhs: 1,
                rhs: 2
            }
            .is_control_flow()
        );
    }

    #[test]
    fn test_dest_register() {
        let instr = Instruction::Add {
            dest: 5,
            lhs: 1,
            rhs: 2,
        };
        assert_eq!(instr.dest_register(), Some(5));

        let jump = Instruction::Jump { offset: 10 };
        assert_eq!(jump.dest_register(), None);
    }

    #[test]
    fn test_source_registers() {
        let instr = Instruction::Add {
            dest: 0,
            lhs: 1,
            rhs: 2,
        };
        assert_eq!(instr.source_registers(), vec![1, 2]);

        let move_instr = Instruction::Move { dest: 0, src: 5 };
        assert_eq!(move_instr.source_registers(), vec![5]);
    }

    #[test]
    fn test_instruction_display() {
        let instr = Instruction::Add {
            dest: 0,
            lhs: 1,
            rhs: 2,
        };
        assert_eq!(format!("{}", instr), "ADD R0 R1 R2");

        let jump = Instruction::Jump { offset: -5 };
        assert_eq!(format!("{}", jump), "JUMP -5");
    }

    #[test]
    fn test_function_proto() {
        let mut proto = FunctionProto::new("test".to_string(), 2);

        assert_eq!(proto.param_count, 2);
        assert_eq!(proto.register_count, 2);

        // Add instruction
        let idx = proto.add_instruction(
            Instruction::Add {
                dest: 2,
                lhs: 0,
                rhs: 1,
            },
            1,
        );
        assert_eq!(idx, 0);
        assert_eq!(proto.instructions.len(), 1);

        // Add constant
        let const_idx = proto.add_constant(Constant::Integer(42));
        assert_eq!(const_idx, 0);

        // Adding same constant returns same index
        let const_idx2 = proto.add_constant(Constant::Integer(42));
        assert_eq!(const_idx2, 0);
        assert_eq!(proto.constants.len(), 1);
    }

    #[test]
    fn test_chunk_builder() {
        let chunk = ChunkBuilder::new()
            .source_file("test.veld".to_string())
            .main_name("main".to_string())
            .optimization_level(2)
            .build();

        assert_eq!(chunk.source_file, "test.veld");
        assert_eq!(chunk.main.name, "main");
        assert_eq!(chunk.metadata.optimization_level, 2);
    }

    #[test]
    fn test_constant_deduplication() {
        let mut proto = FunctionProto::new("test".to_string(), 0);

        let idx1 = proto.add_constant(Constant::String("hello".to_string()));
        let idx2 = proto.add_constant(Constant::Integer(42));
        let idx3 = proto.add_constant(Constant::String("hello".to_string()));

        assert_eq!(idx1, 0);
        assert_eq!(idx2, 1);
        assert_eq!(idx3, 0); // Deduplicated!
        assert_eq!(proto.constants.len(), 2);
    }
}
