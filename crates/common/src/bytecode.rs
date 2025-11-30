use super::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

// =======================================================================
//  Instruction Definition (from former instruction.rs)
// =======================================================================

/// Bytecode instructions for the Veld virtual machine
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    LoadConstant(u16),
    Pop,
    Duplicate,
    Swap,
    LoadLocal(u16),
    StoreLocal(u16),
    LoadGlobal(u16),
    StoreGlobal(u16),
    LoadUpvalue(u16),
    StoreUpvalue(u16),
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Negate,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
    Jump(i16),
    JumpIfFalse(i16),
    JumpIfTrue(i16),
    PopJumpIfFalse(i16),
    PopJumpIfTrue(i16),
    Return,
    Call(u8),
    Closure(u16, u8),
    CloseUpvalues(u16),
    NewStruct(u16),
    GetField(u16),
    SetField(u16),
    NewArray(u16),
    GetIndex,
    SetIndex,
    NewTuple(u8),
    NewEnum(u16, u8),
    MatchStart,
    MatchPattern(u16),
    MatchJump(i16),
    MatchEnd,
    TypeCheck(u16),
    TypeCast(u16),
    Import(u16),
    Export(u16),
    Print,
    Halt,
    Nop,
    MakeIterator,
    IteratorNext,
    IteratorHasNext,
    Throw,
    TryStart(i16),
    TryEnd,
    Catch(u16),
    Allocate(u16),
    Deallocate,
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
    pub fn size(&self) -> usize {
        match self {
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
            Instruction::Call(_) | Instruction::NewTuple(_) => 2,
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
            Instruction::Jump(_)
            | Instruction::JumpIfFalse(_)
            | Instruction::JumpIfTrue(_)
            | Instruction::PopJumpIfFalse(_)
            | Instruction::PopJumpIfTrue(_)
            | Instruction::MatchJump(_)
            | Instruction::TryStart(_) => 3,
            Instruction::Closure(_, _) | Instruction::NewEnum(_, _) => 4,
        }
    }

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

// =======================================================================
//  Chunk Definition (from former chunk.rs)
// =======================================================================

/// A chunk of bytecode containing instructions, constants, and metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Chunk {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Value>,
    pub line_numbers: Vec<usize>,
    pub source_file: Option<String>,
    pub name: Option<String>,
    pub local_count: usize,
    pub parameter_count: usize,
    pub max_stack_depth: usize,
    pub upvalues: Vec<UpvalueInfo>,
    pub labels: HashMap<String, usize>,
    pub debug_symbols: Vec<DebugSymbol>,
}

/// Information about an upvalue (captured variable)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UpvalueInfo {
    pub index: usize,
    pub is_local: bool,
    pub name: Option<String>,
}

/// Debug information for variables
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DebugSymbol {
    pub name: String,
    pub index: usize,
    pub start_instruction: usize,
    pub end_instruction: usize,
    pub line: Option<usize>,
    pub column: Option<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            line_numbers: Vec::new(),
            source_file: None,
            name: None,
            local_count: 0,
            parameter_count: 0,
            max_stack_depth: 0,
            upvalues: Vec::new(),
            labels: HashMap::new(),
            debug_symbols: Vec::new(),
        }
    }

    pub fn with_name(name: String) -> Self {
        let mut chunk = Self::new();
        chunk.name = Some(name);
        chunk
    }

    pub fn add_instruction(&mut self, instruction: Instruction, line: usize) -> usize {
        let index = self.instructions.len();
        self.instructions.push(instruction);
        self.line_numbers.push(line);
        index
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        if let Some(index) = self
            .constants
            .iter()
            .position(|existing| existing == &value)
        {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(value);
        index
    }

    pub fn get_constant(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }

    pub fn get_instruction(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }

    pub fn get_line_number(&self, instruction_index: usize) -> Option<usize> {
        self.line_numbers.get(instruction_index).copied()
    }

    pub fn add_label(&mut self, label: String) {
        let position = self.instructions.len();
        self.labels.insert(label, position);
    }

    pub fn get_label_position(&self, label: &str) -> Option<usize> {
        self.labels.get(label).copied()
    }

    pub fn add_upvalue(&mut self, info: UpvalueInfo) -> usize {
        let index = self.upvalues.len();
        self.upvalues.push(info);
        index
    }

    pub fn add_debug_symbol(&mut self, symbol: DebugSymbol) {
        self.debug_symbols.push(symbol);
    }

    pub fn find_debug_symbol(&self, name: &str, instruction: usize) -> Option<&DebugSymbol> {
        self.debug_symbols.iter().find(|symbol| {
            symbol.name == name
                && instruction >= symbol.start_instruction
                && instruction <= symbol.end_instruction
        })
    }

    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn constant_count(&self) -> usize {
        self.constants.len()
    }

    pub fn patch_jump(&mut self, instruction_index: usize, target: usize) -> Result<(), String> {
        if instruction_index >= self.instructions.len() {
            return Err("Invalid instruction index".to_string());
        }
        let offset = target as i16 - instruction_index as i16 - 1;
        match &mut self.instructions[instruction_index] {
            Instruction::Jump(jump_offset) => *jump_offset = offset,
            Instruction::JumpIfFalse(jump_offset) => *jump_offset = offset,
            Instruction::JumpIfTrue(jump_offset) => *jump_offset = offset,
            Instruction::PopJumpIfFalse(jump_offset) => *jump_offset = offset,
            Instruction::PopJumpIfTrue(jump_offset) => *jump_offset = offset,
            Instruction::MatchJump(jump_offset) => *jump_offset = offset,
            Instruction::TryStart(handler_offset) => *handler_offset = offset,
            _ => return Err("Cannot patch non-jump instruction".to_string()),
        }
        Ok(())
    }

    pub fn calculate_max_stack_depth(&mut self) {
        let mut current_depth: usize = 0;
        let mut max_depth: usize = 0;
        for instruction in &self.instructions {
            if instruction.pops_stack() {
                match instruction {
                    Instruction::Pop => current_depth = current_depth.saturating_sub(1),
                    Instruction::Call(argc) => {
                        current_depth = current_depth.saturating_sub(*argc as usize + 1);
                        current_depth += 1;
                    }
                    Instruction::Return => current_depth = current_depth.saturating_sub(1),
                    Instruction::NewArray(size) => {
                        current_depth = current_depth.saturating_sub(*size as usize);
                        current_depth += 1;
                    }
                    Instruction::NewTuple(size) => {
                        current_depth = current_depth.saturating_sub(*size as usize);
                        current_depth += 1;
                    }
                    Instruction::NewStruct(_) => {
                        current_depth = current_depth.saturating_sub(1);
                        current_depth += 1;
                    }
                    _ => current_depth = current_depth.saturating_sub(1),
                }
            }
            if instruction.pushes_stack() {
                current_depth += 1;
            }
            max_depth = max_depth.max(current_depth);
        }
        self.max_stack_depth = max_depth;
    }

    pub fn optimize(&mut self) {
        let mut optimized_instructions = Vec::new();
        let mut i = 0;
        while i < self.instructions.len() {
            match &self.instructions[i] {
                Instruction::Pop => {
                    let mut pop_count = 1;
                    while i + pop_count < self.instructions.len() {
                        if matches!(self.instructions[i + pop_count], Instruction::Pop) {
                            pop_count += 1;
                        } else {
                            break;
                        }
                    }
                    for _ in 0..pop_count {
                        optimized_instructions.push(Instruction::Pop);
                    }
                    i += pop_count;
                }
                _ => {
                    optimized_instructions.push(self.instructions[i].clone());
                    i += 1;
                }
            }
        }
        self.instructions = optimized_instructions;
    }

    pub fn validate(&self) -> Result<(), String> {
        for instruction in &self.instructions {
            match instruction {
                Instruction::LoadConstant(index) => {
                    if *index as usize >= self.constants.len() {
                        return Err(format!("Invalid constant index: {}", index));
                    }
                }
                Instruction::LoadLocal(index) | Instruction::StoreLocal(index) => {
                    if *index as usize >= self.local_count {
                        return Err(format!("Invalid local variable index: {}", index));
                    }
                }
                _ => {}
            }
        }
        if self.line_numbers.len() != self.instructions.len() {
            return Err("Line numbers count doesn't match instructions count".to_string());
        }
        Ok(())
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        writeln!(f, "=== {} ===", self.name.as_deref().unwrap_or("Anonymous"))?;
        writeln!(f, "Constants: {}", self.constants.len())?;
        writeln!(f, "Locals: {}", self.local_count)?;
        writeln!(f, "Parameters: {}", self.parameter_count)?;
        writeln!(f, "Max stack depth: {}", self.max_stack_depth)?;
        writeln!(f, "Upvalues: {}", self.upvalues.len())?;
        writeln!(f)?;
        if !self.constants.is_empty() {
            writeln!(f, "Constants:")?;
            for (i, constant) in self.constants.iter().enumerate() {
                writeln!(f, "  {:04}: {}", i, constant)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "Instructions:")?;
        for (i, instruction) in self.instructions.iter().enumerate() {
            let line = self.line_numbers.get(i).copied().unwrap_or(0);
            writeln!(f, "  {:04} (line {:3}): {}", i, line, instruction)?;
        }
        Ok(())
    }
}

/// Builder for creating chunks with fluent interface
#[derive(Debug, Clone)]
pub struct ChunkBuilder {
    chunk: Chunk,
}

impl ChunkBuilder {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
        }
    }

    pub fn with_name(mut self, name: String) -> Self {
        self.chunk.name = Some(name);
        self
    }

    pub fn with_source_file(mut self, source_file: String) -> Self {
        self.chunk.source_file = Some(source_file);
        self
    }

    pub fn with_local_count(mut self, count: usize) -> Self {
        self.chunk.local_count = count;
        self
    }

    pub fn with_parameter_count(mut self, count: usize) -> Self {
        self.chunk.parameter_count = count;
        self
    }

    pub fn instruction(mut self, instruction: Instruction, line: usize) -> Self {
        self.chunk.add_instruction(instruction, line);
        self
    }

    pub fn constant(mut self, value: Value) -> Self {
        self.chunk.add_constant(value);
        self
    }

    pub fn build(mut self) -> Chunk {
        self.chunk.calculate_max_stack_depth();
        self.chunk
    }
}

impl Default for ChunkBuilder {
    fn default() -> Self {
        Self::new()
    }
}
