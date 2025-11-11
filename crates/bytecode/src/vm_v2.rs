//! Register-based Virtual Machine for Veld
//!
//! This is the new register-based VM implementation that executes
//! `bytecode_v2::Instruction`s. It uses a register file architecture
//! inspired by Lua 5.x.
//!
//! # Architecture
//!
//! - Each call frame has its own register window (up to 256 registers)
//! - Registers are allocated from a flat vector with per-frame base offsets
//! - Function parameters are passed via registers at the start of a frame
//! - Return values are placed in specific registers
//!
//! # Key Differences from Stack VM
//!
//! - No operand stack (registers replace it)
//! - 3-address code style (dest = src1 op src2)
//! - Fewer instructions needed for most operations
//! - More efficient function calls (register windows)

use crate::value::{BytecodeValue, RuntimeError, TypeInfo, UpvalueRef};
use std::collections::HashMap;
use std::rc::Rc;
use tracing::{trace, warn};
use veld_common::bytecode_v2::{Chunk, ConstIdx, Constant, Instruction, JumpOffset, Reg};

/// The register-based virtual machine
pub struct VirtualMachine {
    /// Flat register file (all frames share this)
    /// Each frame gets a window into this array
    registers: Vec<BytecodeValue>,

    /// Call frame stack
    frames: Vec<CallFrame>,

    /// Global variables
    globals: HashMap<String, BytecodeValue>,

    /// Open upvalues (for closures)
    open_upvalues: Vec<Upvalue>,

    /// Native function registry
    native_functions: HashMap<String, fn(&[BytecodeValue]) -> Result<BytecodeValue, RuntimeError>>,

    /// Maximum register count to prevent overflow
    max_registers: usize,

    /// Current execution state
    state: VmState,

    /// Debugging flags
    debug_mode: bool,
    trace_execution: bool,
}

/// A call frame represents a function call context
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// The chunk being executed
    chunk: Chunk,

    /// Current instruction pointer
    ip: usize,

    /// Base index into the register file for this frame
    register_base: usize,

    /// Number of registers allocated for this frame
    register_count: usize,

    /// Closure upvalues (if this is a closure call)
    upvalues: Vec<UpvalueRef>,

    /// Function name (for debugging)
    function_name: Option<String>,
}

/// Represents an upvalue (captured variable)
#[derive(Debug, Clone)]
pub struct Upvalue {
    /// Index in the register file where the value is stored
    register_index: usize,

    /// The actual value (if closed over)
    closed_value: Option<BytecodeValue>,

    /// Whether this upvalue is closed
    is_closed: bool,
}

/// Current state of the virtual machine
#[derive(Debug, Clone, PartialEq)]
pub enum VmState {
    Ready,
    Running,
    Halted,
    Error(RuntimeError),
}

/// Result of interpreting bytecode
#[derive(Debug, Clone, PartialEq)]
pub enum InterpretResult {
    Ok(BytecodeValue),
    RuntimeError(RuntimeError),
    CompileError(String),
}

impl VirtualMachine {
    /// Create a new virtual machine
    pub fn new() -> Self {
        Self {
            registers: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: Vec::new(),
            native_functions: HashMap::new(),
            max_registers: 1024 * 64, // 64K registers max
            state: VmState::Ready,
            debug_mode: false,
            trace_execution: false,
        }
    }

    /// Create a new VM with debugging enabled
    pub fn with_debug() -> Self {
        let mut vm = Self::new();
        vm.debug_mode = true;
        vm.trace_execution = true;
        vm
    }

    /// Set the maximum register count
    pub fn set_max_registers(&mut self, count: usize) {
        self.max_registers = count;
    }

    /// Register a native function
    pub fn register_native_function(
        &mut self,
        name: String,
        function: fn(&[BytecodeValue]) -> Result<BytecodeValue, RuntimeError>,
    ) {
        self.native_functions.insert(name, function);
    }

    /// Interpret a chunk of bytecode
    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.state = VmState::Running;
        self.registers.clear();
        self.frames.clear();

        // Allocate initial register window
        let register_count = chunk.main.register_count as usize;
        self.registers.resize(register_count, BytecodeValue::Unit);

        // Create initial call frame
        let frame = CallFrame {
            chunk,
            ip: 0,
            register_base: 0,
            register_count,
            upvalues: Vec::new(),
            function_name: None,
        };

        self.frames.push(frame);

        match self.run() {
            Ok(value) => InterpretResult::Ok(value),
            Err(error) => {
                self.state = VmState::Error(error.clone());
                InterpretResult::RuntimeError(error)
            }
        }
    }

    /// Main execution loop
    fn run(&mut self) -> Result<BytecodeValue, RuntimeError> {
        loop {
            if self.trace_execution {
                self.trace_instruction();
            }

            let instruction = self.read_instruction()?;

            match instruction {
                // ============================================================
                // MOVE AND LOAD INSTRUCTIONS
                // ============================================================
                Instruction::Move { dest, src } => {
                    let value = self.get_register(src)?.clone();
                    self.set_register(dest, value)?;
                }

                Instruction::LoadConst { dest, const_idx } => {
                    let value = self.read_constant(const_idx)?;
                    self.set_register(dest, value)?;
                }

                Instruction::LoadBool { dest, value } => {
                    self.set_register(dest, BytecodeValue::Boolean(value))?;
                }

                Instruction::LoadNil { dest } => {
                    self.set_register(dest, BytecodeValue::Unit)?;
                }

                Instruction::LoadNilRange { start, count } => {
                    for i in 0..count {
                        let reg = start.wrapping_add(i);
                        self.set_register(reg, BytecodeValue::Unit)?;
                    }
                }

                // ============================================================
                // ARITHMETIC INSTRUCTIONS
                // ============================================================
                Instruction::Add { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.add_values(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::AddK {
                    dest,
                    lhs,
                    const_idx,
                } => {
                    let left = self.get_register(lhs)?;
                    let right = self.read_constant(const_idx as ConstIdx)?;
                    let result = self.add_values(left, &right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Sub { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.subtract_values(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::SubK {
                    dest,
                    lhs,
                    const_idx,
                } => {
                    let left = self.get_register(lhs)?;
                    let right = self.read_constant(const_idx as ConstIdx)?;
                    let result = self.subtract_values(left, &right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Mul { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.multiply_values(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::MulK {
                    dest,
                    lhs,
                    const_idx,
                } => {
                    let left = self.get_register(lhs)?;
                    let right = self.read_constant(const_idx as ConstIdx)?;
                    let result = self.multiply_values(left, &right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Div { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.divide_values(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::DivK {
                    dest,
                    lhs,
                    const_idx,
                } => {
                    let left = self.get_register(lhs)?;
                    let right = self.read_constant(const_idx as ConstIdx)?;
                    let result = self.divide_values(left, &right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Mod { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.modulo_values(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::ModK {
                    dest,
                    lhs,
                    const_idx,
                } => {
                    let left = self.get_register(lhs)?;
                    let right = self.read_constant(const_idx as ConstIdx)?;
                    let result = self.modulo_values(left, &right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Pow { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.power_values(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Neg { dest, src } => {
                    let value = self.get_register(src)?;
                    let result = self.negate_value(value)?;
                    self.set_register(dest, result)?;
                }

                // ============================================================
                // COMPARISON INSTRUCTIONS
                // ============================================================
                Instruction::Eq { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = BytecodeValue::Boolean(left == right);
                    self.set_register(dest, result)?;
                }

                Instruction::Neq { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = BytecodeValue::Boolean(left != right);
                    self.set_register(dest, result)?;
                }

                Instruction::Lt { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.compare_less_than(left, right)?;
                    self.set_register(dest, BytecodeValue::Boolean(result))?;
                }

                Instruction::Le { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.compare_less_equal(left, right)?;
                    self.set_register(dest, BytecodeValue::Boolean(result))?;
                }

                Instruction::Gt { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.compare_greater_than(left, right)?;
                    self.set_register(dest, BytecodeValue::Boolean(result))?;
                }

                Instruction::Ge { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.compare_greater_equal(left, right)?;
                    self.set_register(dest, BytecodeValue::Boolean(result))?;
                }

                // ============================================================
                // LOGICAL INSTRUCTIONS
                // ============================================================
                Instruction::And { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let result = if self.is_truthy(left) {
                        self.get_register(rhs)?.clone()
                    } else {
                        left.clone()
                    };
                    self.set_register(dest, result)?;
                }

                Instruction::Or { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let result = if self.is_truthy(left) {
                        left.clone()
                    } else {
                        self.get_register(rhs)?.clone()
                    };
                    self.set_register(dest, result)?;
                }

                Instruction::Not { dest, src } => {
                    let value = self.get_register(src)?;
                    let result = BytecodeValue::Boolean(!self.is_truthy(value));
                    self.set_register(dest, result)?;
                }

                // ============================================================
                // BITWISE INSTRUCTIONS
                // ============================================================
                Instruction::BitAnd { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.bitwise_and(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::BitOr { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.bitwise_or(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::BitXor { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.bitwise_xor(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::BitNot { dest, src } => {
                    let value = self.get_register(src)?;
                    let result = self.bitwise_not(value)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Shl { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.shift_left(left, right)?;
                    self.set_register(dest, result)?;
                }

                Instruction::Shr { dest, lhs, rhs } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    let result = self.shift_right(left, right)?;
                    self.set_register(dest, result)?;
                }

                // ============================================================
                // CONTROL FLOW INSTRUCTIONS
                // ============================================================
                Instruction::Jump { offset } => {
                    self.jump(offset)?;
                }

                Instruction::JumpIf { condition, offset } => {
                    let cond = self.get_register(condition)?;
                    if self.is_truthy(cond) {
                        self.jump(offset)?;
                    }
                }

                Instruction::JumpIfNot { condition, offset } => {
                    let cond = self.get_register(condition)?;
                    if !self.is_truthy(cond) {
                        self.jump(offset)?;
                    }
                }

                Instruction::JumpIfEq { lhs, rhs, offset } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    if left == right {
                        self.jump(offset)?;
                    }
                }

                Instruction::JumpIfNeq { lhs, rhs, offset } => {
                    let left = self.get_register(lhs)?;
                    let right = self.get_register(rhs)?;
                    if left != right {
                        self.jump(offset)?;
                    }
                }

                // ============================================================
                // FUNCTION CALL INSTRUCTIONS
                // ============================================================
                Instruction::Call {
                    func,
                    arg_count,
                    ret_count,
                } => {
                    self.call_function(func, arg_count, ret_count)?;
                }

                Instruction::TailCall { func, arg_count } => {
                    self.tail_call_function(func, arg_count)?;
                }

                Instruction::Return { first, count } => {
                    let result = if count == 0 {
                        BytecodeValue::Unit
                    } else if count == 1 {
                        self.get_register(first)?.clone()
                    } else {
                        // Multiple return values - for now just return first
                        // TODO: Implement proper multi-value returns
                        self.get_register(first)?.clone()
                    };

                    // Pop frame
                    self.frames.pop();

                    if self.frames.is_empty() {
                        // Top-level return
                        self.state = VmState::Halted;
                        return Ok(result);
                    } else {
                        // Return to caller - place result in appropriate register
                        // This will be handled by call_function logic
                        // For now, just continue execution
                        continue;
                    }
                }

                // ============================================================
                // GLOBAL VARIABLE INSTRUCTIONS
                // ============================================================
                Instruction::LoadGlobal { dest, name_idx } => {
                    let name = self.read_constant(name_idx)?;
                    let name_str = match &name {
                        BytecodeValue::String(s) => s.clone(),
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "String".to_string(),
                                actual: self.type_name(&name),
                            });
                        }
                    };

                    let value = self
                        .globals
                        .get(&name_str)
                        .cloned()
                        .unwrap_or(BytecodeValue::Unit);
                    self.set_register(dest, value)?;
                }

                Instruction::StoreGlobal { name_idx, src } => {
                    let name = self.read_constant(name_idx)?;
                    let name_str = match &name {
                        BytecodeValue::String(s) => s.clone(),
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "String".to_string(),
                                actual: self.type_name(&name),
                            });
                        }
                    };

                    let value = self.get_register(src)?.clone();
                    self.globals.insert(name_str, value);
                }

                // ============================================================
                // CLOSURE AND UPVALUE INSTRUCTIONS
                // ============================================================
                Instruction::Closure { dest, proto_idx } => {
                    // TODO: Implement closure creation
                    // For now, just load unit
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("Closure instruction not yet fully implemented");
                }

                Instruction::GetUpvalue { dest, upvalue_idx } => {
                    let frame = self.current_frame();
                    if let Some(_upvalue_ref) = frame.upvalues.get(upvalue_idx as usize) {
                        // TODO: Proper upvalue dereferencing
                        self.set_register(dest, BytecodeValue::Unit)?;
                    } else {
                        return Err(RuntimeError::MemoryError(format!(
                            "Upvalue index {} out of bounds",
                            upvalue_idx
                        )));
                    }
                }

                Instruction::SetUpvalue { upvalue_idx, src } => {
                    let _value = self.get_register(src)?.clone();
                    let frame = self.current_frame();
                    if let Some(_upvalue_ref) = frame.upvalues.get(upvalue_idx as usize) {
                        // TODO: Proper upvalue mutation
                        warn!("SetUpvalue not yet fully implemented");
                    } else {
                        return Err(RuntimeError::MemoryError(format!(
                            "Upvalue index {} out of bounds",
                            upvalue_idx
                        )));
                    }
                }

                Instruction::CloseUpvalues { start } => {
                    self.close_upvalues(start)?;
                }

                // ============================================================
                // DATA STRUCTURE INSTRUCTIONS
                // ============================================================
                Instruction::NewArray { dest, size } => {
                    // TODO: Implement array creation
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("NewArray instruction not yet fully implemented");
                }

                Instruction::GetIndex { dest, array, index } => {
                    let arr = self.get_register(array)?;
                    let idx = self.get_register(index)?;
                    let result = self.get_array_element(arr, idx)?;
                    self.set_register(dest, result)?;
                }

                Instruction::SetIndex {
                    array,
                    index,
                    value,
                } => {
                    let arr = self.get_register(array)?;
                    let idx = self.get_register(index)?;
                    let val = self.get_register(value)?;
                    self.set_array_element(arr, idx, val)?;
                }

                Instruction::GetField {
                    dest,
                    object,
                    field_idx,
                } => {
                    // TODO: Implement struct field access
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("GetField instruction not yet fully implemented");
                }

                Instruction::SetField {
                    object,
                    field_idx,
                    value,
                } => {
                    // TODO: Implement struct field assignment
                    warn!("SetField instruction not yet fully implemented");
                }

                Instruction::NewStruct {
                    dest,
                    type_idx,
                    field_count,
                } => {
                    // TODO: Implement struct creation
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("NewStruct instruction not yet fully implemented");
                }

                Instruction::NewTuple { dest, size } => {
                    // TODO: Implement tuple creation
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("NewTuple instruction not yet fully implemented");
                }

                Instruction::NewEnum {
                    dest,
                    variant_idx,
                    field_count,
                } => {
                    // TODO: Implement enum variant creation
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("NewEnum instruction not yet fully implemented");
                }

                // ============================================================
                // PATTERN MATCHING INSTRUCTIONS
                // ============================================================
                Instruction::MatchStart { value } => {
                    // TODO: Implement pattern matching
                    warn!("MatchStart instruction not yet fully implemented");
                }

                Instruction::MatchPattern {
                    value,
                    pattern_idx,
                    offset,
                } => {
                    // TODO: Implement pattern matching
                    warn!("MatchPattern instruction not yet fully implemented");
                }

                Instruction::MatchEnd => {
                    // TODO: Implement pattern matching
                    warn!("MatchEnd instruction not yet fully implemented");
                }

                Instruction::ExtractField {
                    dest,
                    enum_value,
                    field_idx,
                } => {
                    // TODO: Implement field extraction
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("ExtractField instruction not yet fully implemented");
                }

                // ============================================================
                // ITERATOR INSTRUCTIONS
                // ============================================================
                Instruction::MakeIterator { dest, iterable } => {
                    // TODO: Implement iterator creation
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("MakeIterator instruction not yet fully implemented");
                }

                Instruction::IteratorNext { dest, iterator } => {
                    // TODO: Implement iterator advancement
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("IteratorNext instruction not yet fully implemented");
                }

                Instruction::IteratorHasNext { dest, iterator } => {
                    // TODO: Implement iterator checking
                    self.set_register(dest, BytecodeValue::Boolean(false))?;
                    warn!("IteratorHasNext instruction not yet fully implemented");
                }

                Instruction::ForIterator {
                    iterator,
                    loop_var,
                    offset,
                } => {
                    // TODO: Implement for-loop iterator
                    warn!("ForIterator instruction not yet fully implemented");
                }

                // ============================================================
                // TYPE INSTRUCTIONS
                // ============================================================
                Instruction::TypeCheck { value, type_idx } => {
                    // TODO: Implement runtime type checking
                    warn!("TypeCheck instruction not yet fully implemented");
                }

                Instruction::TypeCast {
                    dest,
                    src,
                    type_idx,
                } => {
                    // TODO: Implement type casting
                    let val = self.get_register(src)?.clone();
                    self.set_register(dest, val)?;
                    warn!("TypeCast instruction not yet fully implemented");
                }

                Instruction::TypeOf { dest, value } => {
                    let val = self.get_register(value)?;
                    let type_name = self.type_name(val);
                    self.set_register(dest, BytecodeValue::String(type_name))?;
                }

                // ============================================================
                // EXCEPTION HANDLING INSTRUCTIONS
                // ============================================================
                Instruction::Throw { value } => {
                    let val = self.get_register(value)?;
                    return Err(RuntimeError::MemoryError(format!("Thrown: {:?}", val)));
                }

                Instruction::TryStart { handler_offset } => {
                    // TODO: Implement try/catch mechanism
                    warn!("TryStart instruction not yet fully implemented");
                }

                Instruction::TryEnd => {
                    // TODO: Implement try/catch cleanup
                    warn!("TryEnd instruction not yet fully implemented");
                }

                Instruction::Catch { dest } => {
                    // TODO: Implement catch handler
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("Catch instruction not yet fully implemented");
                }

                // ============================================================
                // MISCELLANEOUS INSTRUCTIONS
                // ============================================================
                Instruction::Print { value } => {
                    let val = self.get_register(value)?;
                    println!("{:?}", val);
                }

                Instruction::Halt => {
                    self.state = VmState::Halted;
                    return Ok(BytecodeValue::Unit);
                }

                Instruction::Nop => {
                    // Do nothing
                }

                Instruction::Import { dest, module_idx } => {
                    // TODO: Implement module import
                    self.set_register(dest, BytecodeValue::Unit)?;
                    warn!("Import instruction not yet fully implemented");
                }

                Instruction::Assert {
                    condition,
                    message_idx,
                } => {
                    let cond = self.get_register(condition)?;
                    if !self.is_truthy(cond) {
                        let msg = self.read_constant(message_idx)?;
                        return Err(RuntimeError::MemoryError(format!(
                            "Assertion failed: {:?}",
                            msg
                        )));
                    }
                }
            }
        }
    }

    // ============================================================
    // HELPER METHODS
    // ============================================================

    /// Read the next instruction
    fn read_instruction(&mut self) -> Result<Instruction, RuntimeError> {
        let frame = self.current_frame_mut();
        if frame.ip >= frame.chunk.main.instructions.len() {
            return Err(RuntimeError::MemoryError(
                "Instruction pointer out of bounds".to_string(),
            ));
        }

        let instruction = frame.chunk.main.instructions[frame.ip].clone();
        frame.ip += 1;
        Ok(instruction)
    }

    /// Read a constant from the current chunk's constant pool
    fn read_constant(&self, idx: ConstIdx) -> Result<BytecodeValue, RuntimeError> {
        let frame = self.current_frame();
        frame
            .chunk
            .main
            .constants
            .get(idx as usize)
            .map(|c| self.constant_to_bytecode_value(c))
            .ok_or_else(|| {
                RuntimeError::MemoryError(format!("Constant index {} out of bounds", idx))
            })
    }

    /// Convert a Constant to BytecodeValue
    fn constant_to_bytecode_value(&self, constant: &Constant) -> BytecodeValue {
        match constant {
            Constant::Nil => BytecodeValue::Unit,
            Constant::Boolean(b) => BytecodeValue::Boolean(*b),
            Constant::Integer(i) => BytecodeValue::Integer(*i),
            Constant::Float(f) => BytecodeValue::Float(*f),
            Constant::String(s) => BytecodeValue::String(s.clone()),
            Constant::Function(proto) => BytecodeValue::Function {
                chunk_index: 0, // TODO: proper chunk indexing
                arity: proto.param_count,
                upvalue_count: proto.upvalues.len() as u8,
                name: Some(proto.name.clone()),
            },
            Constant::Type(t) => BytecodeValue::Type(TypeInfo {
                name: t.clone(),
                size: 0,
                alignment: 0,
            }),
        }
    }

    /// Get a register value (with bounds checking)
    fn get_register(&self, reg: Reg) -> Result<&BytecodeValue, RuntimeError> {
        let frame = self.current_frame();
        let absolute_index = frame.register_base + reg as usize;

        if absolute_index >= frame.register_base + frame.register_count {
            return Err(RuntimeError::MemoryError(format!(
                "Register {} out of bounds (frame has {} registers)",
                reg, frame.register_count
            )));
        }

        self.registers.get(absolute_index).ok_or_else(|| {
            RuntimeError::MemoryError(format!("Register index {} out of bounds", absolute_index))
        })
    }

    /// Set a register value (with bounds checking)
    fn set_register(&mut self, reg: Reg, value: BytecodeValue) -> Result<(), RuntimeError> {
        let frame = self.current_frame();
        let absolute_index = frame.register_base + reg as usize;

        if absolute_index >= frame.register_base + frame.register_count {
            return Err(RuntimeError::MemoryError(format!(
                "Register {} out of bounds (frame has {} registers)",
                reg, frame.register_count
            )));
        }

        if absolute_index >= self.registers.len() {
            self.registers
                .resize(absolute_index + 1, BytecodeValue::Unit);
        }

        self.registers[absolute_index] = value;
        Ok(())
    }

    /// Get the current call frame
    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect("No call frame available")
    }

    /// Get the current call frame mutably
    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("No call frame available")
    }

    /// Perform a jump
    fn jump(&mut self, offset: JumpOffset) -> Result<(), RuntimeError> {
        let frame = self.current_frame_mut();
        let new_ip = (frame.ip as i32 + offset as i32) as usize;

        if new_ip > frame.chunk.main.instructions.len() {
            return Err(RuntimeError::MemoryError(format!(
                "Jump target {} out of bounds",
                new_ip
            )));
        }

        frame.ip = new_ip;
        Ok(())
    }

    /// Call a function
    fn call_function(
        &mut self,
        func_reg: Reg,
        arg_count: u8,
        result_count: u8,
    ) -> Result<(), RuntimeError> {
        // TODO: Implement full function calling
        // For now, just check for native functions
        let func = self.get_register(func_reg)?;

        match func {
            BytecodeValue::NativeFunction { name, .. } => {
                if let Some(native_fn) = self.native_functions.get(name).copied() {
                    // Collect arguments from registers
                    let mut args = Vec::new();
                    for i in 0..arg_count {
                        let arg_reg = func_reg + 1 + i;
                        args.push(self.get_register(arg_reg)?.clone());
                    }

                    // Call native function
                    let result = native_fn(&args)?;

                    // Place result in destination register
                    if result_count > 0 {
                        self.set_register(func_reg, result)?;
                    }

                    Ok(())
                } else {
                    Err(RuntimeError::UndefinedFunction(name.clone()))
                }
            }
            _ => {
                // TODO: Implement Veld function calls
                warn!("User-defined function calls not yet implemented");
                Ok(())
            }
        }
    }

    /// Perform a tail call
    fn tail_call_function(&mut self, func_reg: Reg, arg_count: u8) -> Result<(), RuntimeError> {
        // TODO: Implement tail call optimization
        self.call_function(func_reg, arg_count, 1)
    }

    /// Close upvalues at or above a register
    fn close_upvalues(&mut self, _start: Reg) -> Result<(), RuntimeError> {
        // TODO: Implement upvalue closing
        Ok(())
    }

    /// Check if a value is truthy
    fn is_truthy(&self, value: &BytecodeValue) -> bool {
        match value {
            BytecodeValue::Unit => false,
            BytecodeValue::Boolean(b) => *b,
            _ => true,
        }
    }

    /// Get the type name of a value
    fn type_name(&self, value: &BytecodeValue) -> String {
        match value {
            BytecodeValue::Unit => "unit".to_string(),
            BytecodeValue::Boolean(_) => "bool".to_string(),
            BytecodeValue::Integer(_) => "i64".to_string(),
            BytecodeValue::Float(_) => "f64".to_string(),
            BytecodeValue::String(_) => "str".to_string(),
            BytecodeValue::Char(_) => "char".to_string(),
            BytecodeValue::Function { .. } => "function".to_string(),
            BytecodeValue::NativeFunction { .. } => "native_function".to_string(),
            BytecodeValue::Array(_) => "array".to_string(),
            BytecodeValue::Struct { .. } => "struct".to_string(),
            BytecodeValue::Enum { .. } => "enum".to_string(),
            BytecodeValue::Tuple(_) => "tuple".to_string(),
            BytecodeValue::Closure { .. } => "closure".to_string(),
            BytecodeValue::Reference(_) => "reference".to_string(),
            BytecodeValue::MutableReference(_) => "mut_reference".to_string(),
            BytecodeValue::Iterator { .. } => "iterator".to_string(),
            BytecodeValue::Module { .. } => "module".to_string(),
            BytecodeValue::Type(_) => "type".to_string(),
            BytecodeValue::Return(_) => "return".to_string(),
            BytecodeValue::Break => "break".to_string(),
            BytecodeValue::Continue => "continue".to_string(),
            BytecodeValue::Exception(_) => "exception".to_string(),
        }
    }

    /// Trace current instruction (for debugging)
    fn trace_instruction(&self) {
        if let Some(frame) = self.frames.last() {
            if frame.ip < frame.chunk.main.instructions.len() {
                let instruction = &frame.chunk.main.instructions[frame.ip];
                trace!(
                    "[{:04}] {:?} (frame base: {}, regs: {})",
                    frame.ip, instruction, frame.register_base, frame.register_count
                );
            }
        }
    }

    // ============================================================
    // ARITHMETIC OPERATIONS
    // ============================================================

    fn add_values(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Integer(a + b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(BytecodeValue::Float(a + b)),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => {
                Ok(BytecodeValue::Float(*a as f64 + b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Float(a + *b as f64))
            }
            (BytecodeValue::String(a), BytecodeValue::String(b)) => {
                Ok(BytecodeValue::String(format!("{}{}", a, b)))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "add".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn subtract_values(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Integer(a - b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(BytecodeValue::Float(a - b)),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => {
                Ok(BytecodeValue::Float(*a as f64 - b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Float(a - *b as f64))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "subtract".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn multiply_values(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Integer(a * b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(BytecodeValue::Float(a * b)),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => {
                Ok(BytecodeValue::Float(*a as f64 * b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Float(a * *b as f64))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "multiply".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn divide_values(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                if *b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Ok(BytecodeValue::Integer(a / b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => {
                if *b == 0.0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Ok(BytecodeValue::Float(a / b))
            }
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => {
                if *b == 0.0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Ok(BytecodeValue::Float(*a as f64 / b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => {
                if *b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Ok(BytecodeValue::Float(a / *b as f64))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "divide".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn modulo_values(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                if *b == 0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Ok(BytecodeValue::Integer(a % b))
            }
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => {
                if *b == 0.0 {
                    return Err(RuntimeError::DivisionByZero);
                }
                Ok(BytecodeValue::Float(a % b))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "modulo".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn power_values(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                if *b < 0 {
                    Ok(BytecodeValue::Float((*a as f64).powf(*b as f64)))
                } else {
                    Ok(BytecodeValue::Integer(a.pow(*b as u32)))
                }
            }
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => {
                Ok(BytecodeValue::Float(a.powf(*b)))
            }
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => {
                Ok(BytecodeValue::Float((*a as f64).powf(*b)))
            }
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Float(a.powf(*b as f64)))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "power".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn negate_value(&self, value: &BytecodeValue) -> Result<BytecodeValue, RuntimeError> {
        match value {
            BytecodeValue::Integer(n) => Ok(BytecodeValue::Integer(-n)),
            BytecodeValue::Float(f) => Ok(BytecodeValue::Float(-f)),
            _ => Err(RuntimeError::InvalidOperation {
                op: "negate".to_string(),
                types: vec![self.type_name(value)],
            }),
        }
    }

    // ============================================================
    // COMPARISON OPERATIONS
    // ============================================================

    fn compare_less_than(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<bool, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => Ok(a < b),
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(a < b),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => Ok((*a as f64) < *b),
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => Ok(*a < (*b as f64)),
            (BytecodeValue::String(a), BytecodeValue::String(b)) => Ok(a < b),
            _ => Err(RuntimeError::InvalidOperation {
                op: "less_than".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn compare_less_equal(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<bool, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => Ok(a <= b),
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(a <= b),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => Ok((*a as f64) <= *b),
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => Ok(*a <= (*b as f64)),
            (BytecodeValue::String(a), BytecodeValue::String(b)) => Ok(a <= b),
            _ => Err(RuntimeError::InvalidOperation {
                op: "less_equal".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn compare_greater_than(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<bool, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => Ok(a > b),
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(a > b),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => Ok((*a as f64) > *b),
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => Ok(*a > (*b as f64)),
            (BytecodeValue::String(a), BytecodeValue::String(b)) => Ok(a > b),
            _ => Err(RuntimeError::InvalidOperation {
                op: "greater_than".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn compare_greater_equal(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<bool, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => Ok(a >= b),
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => Ok(a >= b),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => Ok((*a as f64) >= *b),
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => Ok(*a >= (*b as f64)),
            (BytecodeValue::String(a), BytecodeValue::String(b)) => Ok(a >= b),
            _ => Err(RuntimeError::InvalidOperation {
                op: "greater_equal".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    // ============================================================
    // BITWISE OPERATIONS
    // ============================================================

    fn bitwise_and(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Integer(a & b))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "bitwise_and".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn bitwise_or(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Integer(a | b))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "bitwise_or".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn bitwise_xor(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                Ok(BytecodeValue::Integer(a ^ b))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "bitwise_xor".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn bitwise_not(&self, value: &BytecodeValue) -> Result<BytecodeValue, RuntimeError> {
        match value {
            BytecodeValue::Integer(n) => Ok(BytecodeValue::Integer(!n)),
            _ => Err(RuntimeError::InvalidOperation {
                op: "bitwise_not".to_string(),
                types: vec![self.type_name(value)],
            }),
        }
    }

    fn shift_left(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                if *b < 0 {
                    return Err(RuntimeError::MemoryError(
                        "Shift amount cannot be negative".to_string(),
                    ));
                }
                Ok(BytecodeValue::Integer(a << b))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "shift_left".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    fn shift_right(
        &self,
        left: &BytecodeValue,
        right: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (left, right) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => {
                if *b < 0 {
                    return Err(RuntimeError::MemoryError(
                        "Shift amount cannot be negative".to_string(),
                    ));
                }
                Ok(BytecodeValue::Integer(a >> b))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "shift_right".to_string(),
                types: vec![self.type_name(left), self.type_name(right)],
            }),
        }
    }

    // ============================================================
    // ARRAY/COLLECTION OPERATIONS
    // ============================================================

    fn get_array_element(
        &self,
        array: &BytecodeValue,
        index: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (array, index) {
            (BytecodeValue::Array(arr), BytecodeValue::Integer(idx)) => {
                let idx_usize = if *idx < 0 {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *idx,
                        length: arr.len(),
                    });
                } else {
                    *idx as usize
                };

                arr.get(idx_usize)
                    .cloned()
                    .ok_or(RuntimeError::IndexOutOfBounds {
                        index: *idx,
                        length: arr.len(),
                    })
            }
            (BytecodeValue::Reference(inner), idx)
            | (BytecodeValue::MutableReference(inner), idx) => self.get_array_element(inner, idx),
            _ => Err(RuntimeError::InvalidOperation {
                op: "index".to_string(),
                types: vec![self.type_name(array), self.type_name(index)],
            }),
        }
    }

    fn set_array_element(
        &self,
        array: &BytecodeValue,
        index: &BytecodeValue,
        _value: &BytecodeValue,
    ) -> Result<(), RuntimeError> {
        match (array, index) {
            (BytecodeValue::MutableReference(_inner), _idx) => {
                // For now, we can't mutate through boxed references easily
                // This is a limitation we'll need to address with proper GC
                Err(RuntimeError::MemoryError(
                    "Cannot mutate through MutableReference yet".to_string(),
                ))
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "set_index".to_string(),
                types: vec![self.type_name(array), self.type_name(index)],
            }),
        }
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================
// TESTS
// ============================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vm_creation() {
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
        assert_eq!(vm.registers.len(), 0);
        assert_eq!(vm.frames.len(), 0);
    }

    #[test]
    fn test_load_const() {
        // TODO: Add builder helper methods to ChunkBuilder
        // For now just test VM creation
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
    }

    #[test]
    fn test_arithmetic_add() {
        // TODO: Implement once ChunkBuilder has helper methods
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
    }

    #[test]
    fn test_move_instruction() {
        // TODO: Implement once ChunkBuilder has helper methods
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
    }

    #[test]
    fn test_comparison_eq() {
        // TODO: Implement once ChunkBuilder has helper methods
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
    }

    #[test]
    fn test_logical_not() {
        // TODO: Implement once ChunkBuilder has helper methods
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
    }

    #[test]
    fn test_jump() {
        // TODO: Implement once ChunkBuilder has helper methods
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
    }
}
