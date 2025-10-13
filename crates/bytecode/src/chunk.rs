use crate::instruction::Instruction;
use crate::value::BytecodeValue;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// A chunk of bytecode containing instructions, constants, and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Chunk {
    /// The bytecode instructions
    pub instructions: Vec<Instruction>,

    /// Constant pool for literal values
    pub constants: Vec<BytecodeValue>,

    /// Line number information for debugging
    pub line_numbers: Vec<usize>,

    /// Source file information
    pub source_file: Option<String>,

    /// Function name (if this chunk represents a function)
    pub name: Option<String>,

    /// Number of local variables this chunk uses
    pub local_count: usize,

    /// Number of parameters (for function chunks)
    pub parameter_count: usize,

    /// Maximum stack depth required
    pub max_stack_depth: usize,

    /// Upvalue information for closures
    pub upvalues: Vec<UpvalueInfo>,

    /// Jump target labels for control flow
    pub labels: HashMap<String, usize>,

    /// Debug symbols for variables
    pub debug_symbols: Vec<DebugSymbol>,
}

/// Information about an upvalue (captured variable)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UpvalueInfo {
    /// Index in the enclosing scope
    pub index: usize,
    /// Whether this upvalue is local to the immediately enclosing scope
    pub is_local: bool,
    /// Name of the captured variable (for debugging)
    pub name: Option<String>,
}

/// Debug information for variables
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DebugSymbol {
    /// Variable name
    pub name: String,
    /// Local variable index
    pub index: usize,
    /// Instruction range where this variable is valid
    pub start_instruction: usize,
    pub end_instruction: usize,
    /// Source location information
    pub line: Option<usize>,
    pub column: Option<usize>,
}

impl Chunk {
    /// Create a new empty chunk
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

    /// Create a new chunk with a name
    pub fn with_name(name: String) -> Self {
        let mut chunk = Self::new();
        chunk.name = Some(name);
        chunk
    }

    /// Add an instruction to the chunk
    pub fn add_instruction(&mut self, instruction: Instruction, line: usize) -> usize {
        let index = self.instructions.len();
        self.instructions.push(instruction);
        self.line_numbers.push(line);
        index
    }

    /// Add a constant to the constant pool and return its index
    pub fn add_constant(&mut self, value: BytecodeValue) -> usize {
        // Check if the constant already exists to avoid duplicates
        for (i, existing) in self.constants.iter().enumerate() {
            if existing.equals(&value) {
                return i;
            }
        }

        let index = self.constants.len();
        self.constants.push(value);
        index
    }

    /// Get a constant by index
    pub fn get_constant(&self, index: usize) -> Option<&BytecodeValue> {
        self.constants.get(index)
    }

    /// Get an instruction by index
    pub fn get_instruction(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }

    /// Get the line number for an instruction
    pub fn get_line_number(&self, instruction_index: usize) -> Option<usize> {
        self.line_numbers.get(instruction_index).copied()
    }

    /// Add a label at the current instruction position
    pub fn add_label(&mut self, label: String) {
        let position = self.instructions.len();
        self.labels.insert(label, position);
    }

    /// Get the instruction index for a label
    pub fn get_label_position(&self, label: &str) -> Option<usize> {
        self.labels.get(label).copied()
    }

    /// Add an upvalue to the chunk
    pub fn add_upvalue(&mut self, info: UpvalueInfo) -> usize {
        let index = self.upvalues.len();
        self.upvalues.push(info);
        index
    }

    /// Add debug symbol information for a variable
    pub fn add_debug_symbol(&mut self, symbol: DebugSymbol) {
        self.debug_symbols.push(symbol);
    }

    /// Find debug symbol for a variable at a given instruction
    pub fn find_debug_symbol(&self, name: &str, instruction: usize) -> Option<&DebugSymbol> {
        self.debug_symbols.iter().find(|symbol| {
            symbol.name == name
                && instruction >= symbol.start_instruction
                && instruction <= symbol.end_instruction
        })
    }

    /// Get the total number of instructions
    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    /// Get the total number of constants
    pub fn constant_count(&self) -> usize {
        self.constants.len()
    }

    /// Patch a jump instruction with the correct offset
    pub fn patch_jump(&mut self, instruction_index: usize, target: usize) -> Result<(), String> {
        if instruction_index >= self.instructions.len() {
            return Err("Invalid instruction index".to_string());
        }

        let offset = target as i16 - instruction_index as i16 - 1;

        match &mut self.instructions[instruction_index] {
            Instruction::Jump(jump_offset) => {
                *jump_offset = offset;
            }
            Instruction::JumpIfFalse(jump_offset) => {
                *jump_offset = offset;
            }
            Instruction::JumpIfTrue(jump_offset) => {
                *jump_offset = offset;
            }
            Instruction::PopJumpIfFalse(jump_offset) => {
                *jump_offset = offset;
            }
            Instruction::PopJumpIfTrue(jump_offset) => {
                *jump_offset = offset;
            }
            Instruction::MatchJump(jump_offset) => {
                *jump_offset = offset;
            }
            Instruction::TryStart(handler_offset) => {
                *handler_offset = offset;
            }
            _ => {
                return Err("Cannot patch non-jump instruction".to_string());
            }
        }

        Ok(())
    }

    /// Calculate and update the maximum stack depth required
    pub fn calculate_max_stack_depth(&mut self) {
        let mut current_depth: usize = 0;
        let mut max_depth: usize = 0;

        for instruction in &self.instructions {
            // Instructions that pop values
            if instruction.pops_stack() {
                match instruction {
                    Instruction::Pop => current_depth = current_depth.saturating_sub(1),
                    Instruction::Call(argc) => {
                        // Call pops function + arguments, pushes return value
                        current_depth = current_depth.saturating_sub(*argc as usize + 1);
                        current_depth += 1;
                    }
                    Instruction::Return => current_depth = current_depth.saturating_sub(1),
                    Instruction::NewArray(size) => {
                        // NewArray pops size elements, pushes 1 array
                        current_depth = current_depth.saturating_sub(*size as usize);
                        current_depth += 1;
                    }
                    Instruction::NewTuple(size) => {
                        // NewTuple pops size elements, pushes 1 tuple
                        current_depth = current_depth.saturating_sub(*size as usize);
                        current_depth += 1;
                    }
                    Instruction::NewStruct(_) => {
                        // This would need more context about field count
                        current_depth = current_depth.saturating_sub(1);
                        current_depth += 1;
                    }
                    _ => current_depth = current_depth.saturating_sub(1),
                }
            }

            // Instructions that push values
            if instruction.pushes_stack() {
                current_depth += 1;
            }

            max_depth = max_depth.max(current_depth);
        }

        self.max_stack_depth = max_depth;
    }

    /// Optimize the chunk by removing dead code and combining instructions
    pub fn optimize(&mut self) {
        // Remove consecutive POP instructions
        let mut optimized_instructions = Vec::new();
        let mut i = 0;

        while i < self.instructions.len() {
            match &self.instructions[i] {
                Instruction::Pop => {
                    // Count consecutive POPs
                    let mut pop_count = 1;
                    while i + pop_count < self.instructions.len() {
                        if matches!(self.instructions[i + pop_count], Instruction::Pop) {
                            pop_count += 1;
                        } else {
                            break;
                        }
                    }

                    // Add all the POPs (could be optimized further)
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

    /// Validate the chunk for correctness
    pub fn validate(&self) -> Result<(), String> {
        // Check that all constant references are valid
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

        // Check that line numbers match instruction count
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

        // Print constants
        if !self.constants.is_empty() {
            writeln!(f, "Constants:")?;
            for (i, constant) in self.constants.iter().enumerate() {
                writeln!(f, "  {:04}: {}", i, constant)?;
            }
            writeln!(f)?;
        }

        // Print instructions
        writeln!(f, "Instructions:")?;
        for (i, instruction) in self.instructions.iter().enumerate() {
            let line = self.line_numbers.get(i).copied().unwrap_or(0);
            writeln!(f, "  {:04} (line {:3}): {}", i, line, instruction)?;
        }

        Ok(())
    }
}

/// Builder for creating chunks with fluent interface
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

    pub fn constant(mut self, value: BytecodeValue) -> Self {
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
