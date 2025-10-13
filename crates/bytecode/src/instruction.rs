use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result as FmtResult};

/// Bytecode instructions for the Veld virtual machine
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    // Stack operations
    /// Push a constant value onto the stack
    LoadConstant(u16),
    /// Pop the top value from the stack
    Pop,
    /// Duplicate the top value on the stack
    Duplicate,
    /// Swap the top two values on the stack
    Swap,

    // Variable operations
    /// Load a local variable onto the stack
    LoadLocal(u16),
    /// Store the top stack value into a local variable
    StoreLocal(u16),
    /// Load a global variable onto the stack
    LoadGlobal(u16),
    /// Store the top stack value into a global variable
    StoreGlobal(u16),
    /// Load an upvalue (closure variable) onto the stack
    LoadUpvalue(u16),
    /// Store the top stack value into an upvalue
    StoreUpvalue(u16),

    // Arithmetic operations
    /// Add two numeric values
    Add,
    /// Subtract two numeric values
    Subtract,
    /// Multiply two numeric values
    Multiply,
    /// Divide two numeric values
    Divide,
    /// Modulo operation
    Modulo,
    /// Exponentiation
    Power,
    /// Negate a numeric value
    Negate,

    // Comparison operations
    /// Equal comparison
    Equal,
    /// Not equal comparison
    NotEqual,
    /// Greater than comparison
    Greater,
    /// Greater than or equal comparison
    GreaterEqual,
    /// Less than comparison
    Less,
    /// Less than or equal comparison
    LessEqual,

    // Logical operations
    /// Logical AND
    LogicalAnd,
    /// Logical OR
    LogicalOr,
    /// Logical NOT
    LogicalNot,

    // Bitwise operations
    /// Bitwise AND
    BitwiseAnd,
    /// Bitwise OR
    BitwiseOr,
    /// Bitwise XOR
    BitwiseXor,
    /// Bitwise NOT
    BitwiseNot,
    /// Left shift
    LeftShift,
    /// Right shift
    RightShift,

    // Control flow
    /// Unconditional jump to offset
    Jump(i16),
    /// Jump if top of stack is false
    JumpIfFalse(i16),
    /// Jump if top of stack is true
    JumpIfTrue(i16),
    /// Pop and jump if false
    PopJumpIfFalse(i16),
    /// Pop and jump if true
    PopJumpIfTrue(i16),
    /// Return from current function
    Return,

    // Function operations
    /// Call a function with n arguments
    Call(u8),
    /// Create a closure with n upvalues
    Closure(u16, u8),
    /// Close upvalues from stack position
    CloseUpvalues(u16),

    // Object operations
    /// Create a new struct instance
    NewStruct(u16),
    /// Get a field from a struct
    GetField(u16),
    /// Set a field in a struct
    SetField(u16),
    /// Create a new array with n elements
    NewArray(u16),
    /// Get an element from an array
    GetIndex,
    /// Set an element in an array
    SetIndex,
    /// Create a tuple with n elements
    NewTuple(u8),
    /// Create an enum variant
    NewEnum(u16, u8),

    // Pattern matching
    /// Start pattern matching
    MatchStart,
    /// Match against a pattern
    MatchPattern(u16),
    /// Jump to match arm
    MatchJump(i16),
    /// End pattern matching
    MatchEnd,

    // Type operations
    /// Check type of value on stack
    TypeCheck(u16),
    /// Cast value to specified type
    TypeCast(u16),

    // Module operations
    /// Import a module
    Import(u16),
    /// Export a value
    Export(u16),

    // Special operations
    /// Print top of stack (for debugging)
    Print,
    /// Halt the VM
    Halt,
    /// No operation
    Nop,

    // Iterator operations
    /// Create an iterator from a collection
    MakeIterator,
    /// Get next value from iterator
    IteratorNext,
    /// Check if iterator has more values
    IteratorHasNext,

    // Exception handling
    /// Throw an exception
    Throw,
    /// Try block start
    TryStart(i16),
    /// Try block end
    TryEnd,
    /// Catch block
    Catch(u16),

    // Memory operations
    /// Allocate memory on heap
    Allocate(u16),
    /// Deallocate memory
    Deallocate,
    /// Copy memory
    MemCopy(u16),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Instruction::LoadConstant(idx) => write!(f, "LOAD_CONST {}", idx),
            Instruction::Pop => write!(f, "POP"),
            Instruction::Duplicate => write!(f, "DUP"),
            Instruction::Swap => write!(f, "SWAP"),

            Instruction::LoadLocal(idx) => write!(f, "LOAD_LOCAL {}", idx),
            Instruction::StoreLocal(idx) => write!(f, "STORE_LOCAL {}", idx),
            Instruction::LoadGlobal(idx) => write!(f, "LOAD_GLOBAL {}", idx),
            Instruction::StoreGlobal(idx) => write!(f, "STORE_GLOBAL {}", idx),
            Instruction::LoadUpvalue(idx) => write!(f, "LOAD_UPVAL {}", idx),
            Instruction::StoreUpvalue(idx) => write!(f, "STORE_UPVAL {}", idx),

            Instruction::Add => write!(f, "ADD"),
            Instruction::Subtract => write!(f, "SUB"),
            Instruction::Multiply => write!(f, "MUL"),
            Instruction::Divide => write!(f, "DIV"),
            Instruction::Modulo => write!(f, "MOD"),
            Instruction::Power => write!(f, "POW"),
            Instruction::Negate => write!(f, "NEG"),

            Instruction::Equal => write!(f, "EQ"),
            Instruction::NotEqual => write!(f, "NE"),
            Instruction::Greater => write!(f, "GT"),
            Instruction::GreaterEqual => write!(f, "GE"),
            Instruction::Less => write!(f, "LT"),
            Instruction::LessEqual => write!(f, "LE"),

            Instruction::LogicalAnd => write!(f, "AND"),
            Instruction::LogicalOr => write!(f, "OR"),
            Instruction::LogicalNot => write!(f, "NOT"),

            Instruction::BitwiseAnd => write!(f, "BIT_AND"),
            Instruction::BitwiseOr => write!(f, "BIT_OR"),
            Instruction::BitwiseXor => write!(f, "BIT_XOR"),
            Instruction::BitwiseNot => write!(f, "BIT_NOT"),
            Instruction::LeftShift => write!(f, "LSHIFT"),
            Instruction::RightShift => write!(f, "RSHIFT"),

            Instruction::Jump(offset) => write!(f, "JUMP {}", offset),
            Instruction::JumpIfFalse(offset) => write!(f, "JUMP_IF_FALSE {}", offset),
            Instruction::JumpIfTrue(offset) => write!(f, "JUMP_IF_TRUE {}", offset),
            Instruction::PopJumpIfFalse(offset) => write!(f, "POP_JUMP_IF_FALSE {}", offset),
            Instruction::PopJumpIfTrue(offset) => write!(f, "POP_JUMP_IF_TRUE {}", offset),
            Instruction::Return => write!(f, "RETURN"),

            Instruction::Call(argc) => write!(f, "CALL {}", argc),
            Instruction::Closure(idx, upvals) => write!(f, "CLOSURE {} {}", idx, upvals),
            Instruction::CloseUpvalues(idx) => write!(f, "CLOSE_UPVALS {}", idx),

            Instruction::NewStruct(idx) => write!(f, "NEW_STRUCT {}", idx),
            Instruction::GetField(idx) => write!(f, "GET_FIELD {}", idx),
            Instruction::SetField(idx) => write!(f, "SET_FIELD {}", idx),
            Instruction::NewArray(size) => write!(f, "NEW_ARRAY {}", size),
            Instruction::GetIndex => write!(f, "GET_INDEX"),
            Instruction::SetIndex => write!(f, "SET_INDEX"),
            Instruction::NewTuple(size) => write!(f, "NEW_TUPLE {}", size),
            Instruction::NewEnum(variant, fields) => write!(f, "NEW_ENUM {} {}", variant, fields),

            Instruction::MatchStart => write!(f, "MATCH_START"),
            Instruction::MatchPattern(idx) => write!(f, "MATCH_PATTERN {}", idx),
            Instruction::MatchJump(offset) => write!(f, "MATCH_JUMP {}", offset),
            Instruction::MatchEnd => write!(f, "MATCH_END"),

            Instruction::TypeCheck(idx) => write!(f, "TYPE_CHECK {}", idx),
            Instruction::TypeCast(idx) => write!(f, "TYPE_CAST {}", idx),

            Instruction::Import(idx) => write!(f, "IMPORT {}", idx),
            Instruction::Export(idx) => write!(f, "EXPORT {}", idx),

            Instruction::Print => write!(f, "PRINT"),
            Instruction::Halt => write!(f, "HALT"),
            Instruction::Nop => write!(f, "NOP"),

            Instruction::MakeIterator => write!(f, "MAKE_ITER"),
            Instruction::IteratorNext => write!(f, "ITER_NEXT"),
            Instruction::IteratorHasNext => write!(f, "ITER_HAS_NEXT"),

            Instruction::Throw => write!(f, "THROW"),
            Instruction::TryStart(handler) => write!(f, "TRY_START {}", handler),
            Instruction::TryEnd => write!(f, "TRY_END"),
            Instruction::Catch(idx) => write!(f, "CATCH {}", idx),

            Instruction::Allocate(size) => write!(f, "ALLOC {}", size),
            Instruction::Deallocate => write!(f, "DEALLOC"),
            Instruction::MemCopy(size) => write!(f, "MEMCPY {}", size),
        }
    }
}

impl Instruction {
    /// Returns the size of the instruction in bytes
    pub fn size(&self) -> usize {
        match self {
            // Instructions with no operands
            Instruction::Pop
            | Instruction::Duplicate
            | Instruction::Swap
            | Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide
            | Instruction::Modulo
            | Instruction::Power
            | Instruction::Negate
            | Instruction::Equal
            | Instruction::NotEqual
            | Instruction::Greater
            | Instruction::GreaterEqual
            | Instruction::Less
            | Instruction::LessEqual
            | Instruction::LogicalAnd
            | Instruction::LogicalOr
            | Instruction::LogicalNot
            | Instruction::BitwiseAnd
            | Instruction::BitwiseOr
            | Instruction::BitwiseXor
            | Instruction::BitwiseNot
            | Instruction::LeftShift
            | Instruction::RightShift
            | Instruction::Return
            | Instruction::GetIndex
            | Instruction::SetIndex
            | Instruction::MatchStart
            | Instruction::MatchEnd
            | Instruction::Print
            | Instruction::Halt
            | Instruction::Nop
            | Instruction::MakeIterator
            | Instruction::IteratorNext
            | Instruction::IteratorHasNext
            | Instruction::Throw
            | Instruction::TryEnd
            | Instruction::Deallocate => 1,

            // Instructions with 1-byte operands
            Instruction::Call(_) | Instruction::NewTuple(_) => 2,

            // Instructions with 2-byte operands
            Instruction::LoadConstant(_)
            | Instruction::LoadLocal(_)
            | Instruction::StoreLocal(_)
            | Instruction::LoadGlobal(_)
            | Instruction::StoreGlobal(_)
            | Instruction::LoadUpvalue(_)
            | Instruction::StoreUpvalue(_)
            | Instruction::NewStruct(_)
            | Instruction::GetField(_)
            | Instruction::SetField(_)
            | Instruction::NewArray(_)
            | Instruction::MatchPattern(_)
            | Instruction::TypeCheck(_)
            | Instruction::TypeCast(_)
            | Instruction::Import(_)
            | Instruction::Export(_)
            | Instruction::CloseUpvalues(_)
            | Instruction::Catch(_)
            | Instruction::Allocate(_)
            | Instruction::MemCopy(_) => 3,

            // Instructions with 2-byte signed operands
            Instruction::Jump(_)
            | Instruction::JumpIfFalse(_)
            | Instruction::JumpIfTrue(_)
            | Instruction::PopJumpIfFalse(_)
            | Instruction::PopJumpIfTrue(_)
            | Instruction::MatchJump(_)
            | Instruction::TryStart(_) => 3,

            // Instructions with multiple operands
            Instruction::Closure(_, _) | Instruction::NewEnum(_, _) => 4,
        }
    }

    /// Returns true if this instruction can modify control flow
    pub fn is_control_flow(&self) -> bool {
        matches!(
            self,
            Instruction::Jump(_)
                | Instruction::JumpIfFalse(_)
                | Instruction::JumpIfTrue(_)
                | Instruction::PopJumpIfFalse(_)
                | Instruction::PopJumpIfTrue(_)
                | Instruction::Return
                | Instruction::Call(_)
                | Instruction::MatchJump(_)
                | Instruction::Throw
                | Instruction::TryStart(_)
                | Instruction::Halt
        )
    }

    /// Returns true if this instruction pops values from the stack
    pub fn pops_stack(&self) -> bool {
        matches!(
            self,
            Instruction::Pop
                | Instruction::StoreLocal(_)
                | Instruction::StoreGlobal(_)
                | Instruction::StoreUpvalue(_)
                | Instruction::Add
                | Instruction::Subtract
                | Instruction::Multiply
                | Instruction::Divide
                | Instruction::Modulo
                | Instruction::Power
                | Instruction::Equal
                | Instruction::NotEqual
                | Instruction::Greater
                | Instruction::GreaterEqual
                | Instruction::Less
                | Instruction::LessEqual
                | Instruction::LogicalAnd
                | Instruction::LogicalOr
                | Instruction::BitwiseAnd
                | Instruction::BitwiseOr
                | Instruction::BitwiseXor
                | Instruction::LeftShift
                | Instruction::RightShift
                | Instruction::PopJumpIfFalse(_)
                | Instruction::PopJumpIfTrue(_)
                | Instruction::Return
                | Instruction::Call(_)
                | Instruction::SetField(_)
                | Instruction::SetIndex
                | Instruction::Print
                | Instruction::Throw
        )
    }

    /// Returns true if this instruction pushes values onto the stack
    pub fn pushes_stack(&self) -> bool {
        matches!(
            self,
            Instruction::LoadConstant(_)
                | Instruction::Duplicate
                | Instruction::LoadLocal(_)
                | Instruction::LoadGlobal(_)
                | Instruction::LoadUpvalue(_)
                | Instruction::Add
                | Instruction::Subtract
                | Instruction::Multiply
                | Instruction::Divide
                | Instruction::Modulo
                | Instruction::Power
                | Instruction::Negate
                | Instruction::Equal
                | Instruction::NotEqual
                | Instruction::Greater
                | Instruction::GreaterEqual
                | Instruction::Less
                | Instruction::LessEqual
                | Instruction::LogicalAnd
                | Instruction::LogicalOr
                | Instruction::LogicalNot
                | Instruction::BitwiseAnd
                | Instruction::BitwiseOr
                | Instruction::BitwiseXor
                | Instruction::BitwiseNot
                | Instruction::LeftShift
                | Instruction::RightShift
                | Instruction::Call(_)
                | Instruction::Closure(_, _)
                | Instruction::NewStruct(_)
                | Instruction::GetField(_)
                | Instruction::NewArray(_)
                | Instruction::GetIndex
                | Instruction::NewTuple(_)
                | Instruction::NewEnum(_, _)
                | Instruction::TypeCheck(_)
                | Instruction::MakeIterator
                | Instruction::IteratorNext
                | Instruction::IteratorHasNext
        )
    }
}
