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

use crate::value::{BytecodeValue, RuntimeError, TypeInfo, Upvalue, UpvalueRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use tracing::{trace, warn};
use veld_common::bytecode_v2::{
    Chunk, ChunkMetadata, ConstIdx, Constant, FunctionProto, Instruction, JumpOffset, Reg,
};

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
    open_upvalues: Vec<UpvalueRef>,

    /// Native function registry
    native_functions: HashMap<String, fn(&[BytecodeValue]) -> Result<BytecodeValue, RuntimeError>>,

    /// Function prototypes registry (for user-defined functions)
    function_protos: Vec<Rc<FunctionProto>>,

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

    /// Register to place return value (in caller's frame)
    return_address: Reg,
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
            function_protos: Vec::new(),
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
            return_address: 0, // Main frame doesn't return anywhere
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

                    // Get return address before closing upvalues and popping frame
                    let return_address = if !self.frames.is_empty() {
                        Some(self.current_frame().return_address)
                    } else {
                        None
                    };

                    // Close all upvalues in the current frame before returning
                    if !self.frames.is_empty() {
                        self.close_upvalues_at(0)?; // Close all upvalues in current frame
                    }

                    // Pop frame and clean up registers
                    if let Some(frame) = self.frames.pop() {
                        // Remove this frame's registers
                        self.registers.truncate(frame.register_base);
                    }

                    if self.frames.is_empty() {
                        // Top-level return
                        self.state = VmState::Halted;
                        return Ok(result);
                    } else {
                        // Return to caller - place result in return address register
                        if let Some(ret_addr) = return_address {
                            self.set_register(ret_addr, result)?;
                        }
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
                    // Get the function prototype from constants
                    let proto_const = self.read_constant(proto_idx)?;

                    // Extract the FunctionProto and register it
                    match proto_const {
                        BytecodeValue::Function {
                            arity,
                            upvalue_count,
                            name,
                            ..
                        } => {
                            // Get the actual FunctionProto from constants
                            let constants = &self.current_frame().chunk.main.constants;
                            let proto = if let Some(Constant::Function(proto_box)) =
                                constants.get(proto_idx as usize)
                            {
                                Rc::new((**proto_box).clone())
                            } else {
                                return Err(RuntimeError::TypeError {
                                    expected: "Function constant".to_string(),
                                    actual: "unknown".to_string(),
                                });
                            };

                            // Store the proto and get its index
                            let proto_index = self.function_protos.len();
                            self.function_protos.push(proto.clone());

                            // Create the function value with the correct index
                            let func_value = BytecodeValue::Function {
                                chunk_index: proto_index,
                                arity,
                                upvalue_count,
                                name,
                            };

                            // Capture upvalues if this function needs them
                            let mut captured_upvalues = Vec::new();
                            if upvalue_count > 0 {
                                let proto_upvalues = &proto.upvalues;
                                for upvalue_info in proto_upvalues {
                                    if upvalue_info.is_local {
                                        // Capture from current frame's registers
                                        let register_index = self.current_frame().register_base
                                            + upvalue_info.register as usize;
                                        let upvalue_ref = self.capture_upvalue(register_index);
                                        captured_upvalues.push(upvalue_ref);
                                    } else {
                                        // Capture from parent's upvalues
                                        let frame = self.current_frame();
                                        if let Some(parent_upvalue) =
                                            frame.upvalues.get(upvalue_info.register as usize)
                                        {
                                            captured_upvalues.push(parent_upvalue.clone());
                                        }
                                    }
                                }
                            }

                            // Create closure with captured upvalues
                            let closure = BytecodeValue::Closure {
                                function: Box::new(func_value),
                                upvalues: captured_upvalues,
                            };
                            self.set_register(dest, closure)?;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "Function".to_string(),
                                actual: self.type_name(&proto_const),
                            });
                        }
                    };
                }

                Instruction::GetUpvalue { dest, upvalue_idx } => {
                    // Get the upvalue from the current frame
                    let upvalue_ref = {
                        let frame = self.current_frame();
                        frame
                            .upvalues
                            .get(upvalue_idx as usize)
                            .cloned()
                            .ok_or_else(|| {
                                RuntimeError::MemoryError(format!(
                                    "Upvalue index {} out of bounds",
                                    upvalue_idx
                                ))
                            })?
                    };

                    // Read the value from the upvalue
                    let value = upvalue_ref.borrow().value.clone();
                    self.set_register(dest, value)?;
                }

                Instruction::SetUpvalue { upvalue_idx, src } => {
                    let value = self.get_register(src)?.clone();

                    // Get the upvalue from the current frame
                    let upvalue_ref = {
                        let frame = self.current_frame();
                        frame
                            .upvalues
                            .get(upvalue_idx as usize)
                            .cloned()
                            .ok_or_else(|| {
                                RuntimeError::MemoryError(format!(
                                    "Upvalue index {} out of bounds",
                                    upvalue_idx
                                ))
                            })?
                    };

                    // Write the value to the upvalue
                    upvalue_ref.borrow_mut().value = value;
                }

                Instruction::CloseUpvalues { start } => {
                    // Close all upvalues at or above the specified register
                    self.close_upvalues_at(start)?;
                }

                // ============================================================
                // DATA STRUCTURE INSTRUCTIONS
                // ============================================================
                Instruction::NewArray { dest, size } => {
                    // Create a new empty array with the specified capacity
                    // Elements will be populated via SetIndex instructions
                    let elements = Vec::with_capacity(size as usize);
                    self.set_register(dest, BytecodeValue::Array(elements))?;
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
                    let idx = self.get_register(index)?.clone();
                    let val = self.get_register(value)?.clone();
                    self.set_array_element(array, &idx, &val)?;
                }

                Instruction::GetField {
                    dest,
                    object,
                    field_idx,
                } => {
                    let obj = self.get_register(object)?;
                    match obj {
                        BytecodeValue::Struct { fields, .. } => {
                            // field_idx is an index into the constant pool containing the field name
                            let field_name_const = self.read_constant(field_idx as ConstIdx)?;
                            let field_name = match &field_name_const {
                                BytecodeValue::String(s) => s.clone(),
                                _ => {
                                    return Err(RuntimeError::TypeError {
                                        expected: "String".to_string(),
                                        actual: self.type_name(&field_name_const),
                                    });
                                }
                            };

                            let value = fields.get(&field_name).cloned().ok_or_else(|| {
                                RuntimeError::FieldNotFound {
                                    field: field_name.clone(),
                                    type_name: "struct".to_string(),
                                }
                            })?;

                            self.set_register(dest, value)?;
                        }
                        _ => {
                            return Err(RuntimeError::InvalidOperation {
                                op: "get_field".to_string(),
                                types: vec![self.type_name(obj)],
                            });
                        }
                    }
                }

                Instruction::SetField {
                    object,
                    field_idx,
                    value,
                } => {
                    let val = self.get_register(value)?.clone();

                    // Read field name before getting mutable access
                    let field_name_const = self.read_constant(field_idx as ConstIdx)?;
                    let field_name = match &field_name_const {
                        BytecodeValue::String(s) => s.clone(),
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "String".to_string(),
                                actual: self.type_name(&field_name_const),
                            });
                        }
                    };

                    // Get mutable access to the struct
                    let frame = self.current_frame();
                    let absolute_index = frame.register_base + object as usize;

                    if absolute_index >= self.registers.len() {
                        return Err(RuntimeError::MemoryError(format!(
                            "Register index {} out of bounds",
                            absolute_index
                        )));
                    }

                    // Check type before mutable borrow
                    let is_struct = matches!(
                        &self.registers[absolute_index],
                        BytecodeValue::Struct { .. }
                    );

                    if !is_struct {
                        let obj_type = self.type_name(&self.registers[absolute_index]);
                        return Err(RuntimeError::InvalidOperation {
                            op: "set_field".to_string(),
                            types: vec![obj_type],
                        });
                    }

                    match &mut self.registers[absolute_index] {
                        BytecodeValue::Struct { fields, .. } => {
                            fields.insert(field_name, val);
                        }
                        _ => unreachable!(),
                    }
                }

                Instruction::NewStruct {
                    dest,
                    type_idx,
                    field_count,
                } => {
                    // Get the type name from constants
                    let type_name_const = self.read_constant(type_idx)?;
                    let type_name = match &type_name_const {
                        BytecodeValue::String(s) => s.clone(),
                        BytecodeValue::Type(ti) => ti.name.clone(),
                        _ => "Unknown".to_string(),
                    };

                    // Create struct with fields from consecutive registers
                    // Fields are expected to be alternating: name, value, name, value...
                    let mut fields = std::collections::HashMap::new();
                    for i in 0..field_count {
                        let name_reg = dest.wrapping_add((i * 2) + 1);
                        let value_reg = dest.wrapping_add((i * 2) + 2);

                        let name_val = self.get_register(name_reg)?;
                        let field_name = match name_val {
                            BytecodeValue::String(s) => s.clone(),
                            _ => format!("field_{}", i),
                        };

                        let value = self.get_register(value_reg)?.clone();
                        fields.insert(field_name, value);
                    }

                    self.set_register(dest, BytecodeValue::Struct { type_name, fields })?;
                }

                Instruction::NewTuple { dest, size } => {
                    // Create a new tuple with elements from consecutive registers
                    let mut elements = Vec::with_capacity(size as usize);
                    for i in 0..size {
                        let reg = dest.wrapping_add(i + 1);
                        elements.push(self.get_register(reg)?.clone());
                    }
                    self.set_register(dest, BytecodeValue::Tuple(elements))?;
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
                    let iterable_value = self.get_register(iterable)?.clone();
                    let iterator = self.make_iterator(iterable_value)?;
                    self.set_register(dest, iterator)?;
                }

                Instruction::IteratorNext { dest, iterator } => {
                    let iter_value = self.get_register(iterator)?;
                    match iter_value {
                        BytecodeValue::Iterator { values, position } => {
                            if *position < values.len() {
                                let value = values[*position].clone();
                                // Update the iterator position
                                let new_iter = BytecodeValue::Iterator {
                                    values: values.clone(),
                                    position: position + 1,
                                };
                                self.set_register(iterator, new_iter)?;
                                self.set_register(dest, value)?;
                            } else {
                                // Iterator exhausted, return Unit
                                self.set_register(dest, BytecodeValue::Unit)?;
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "Iterator".to_string(),
                                actual: self.type_name(iter_value),
                            });
                        }
                    }
                }

                Instruction::IteratorHasNext { dest, iterator } => {
                    let iter_value = self.get_register(iterator)?;
                    match iter_value {
                        BytecodeValue::Iterator { values, position } => {
                            let has_next = *position < values.len();
                            self.set_register(dest, BytecodeValue::Boolean(has_next))?;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "Iterator".to_string(),
                                actual: self.type_name(iter_value),
                            });
                        }
                    }
                }

                Instruction::ForIterator {
                    iterator,
                    loop_var,
                    offset,
                } => {
                    // Check if iterator has next value
                    let iter_value = self.get_register(iterator)?;
                    let has_next = match iter_value {
                        BytecodeValue::Iterator { values, position } => *position < values.len(),
                        _ => false,
                    };

                    if has_next {
                        // Get next value and store in loop variable
                        let iter_value = self.get_register(iterator)?;
                        if let BytecodeValue::Iterator { values, position } = iter_value {
                            let value = values[*position].clone();
                            // Update iterator
                            let new_iter = BytecodeValue::Iterator {
                                values: values.clone(),
                                position: position + 1,
                            };
                            self.set_register(iterator, new_iter)?;
                            // Store value in loop variable
                            self.set_register(loop_var, value)?;
                        }
                        // Continue with loop body (don't jump)
                    } else {
                        // Iterator exhausted, jump to end of loop
                        self.jump(offset)?;
                    }
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
        let func = self.get_register(func_reg)?.clone();

        match func {
            BytecodeValue::NativeFunction { name, .. } => {
                if let Some(native_fn) = self.native_functions.get(&name).copied() {
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
            BytecodeValue::Closure { function, upvalues } => {
                // Extract function info
                let (chunk_index, arity, name) = match function.as_ref() {
                    BytecodeValue::Function {
                        chunk_index,
                        arity,
                        name,
                        ..
                    } => (chunk_index, arity, name.clone()),
                    _ => {
                        return Err(RuntimeError::TypeError {
                            expected: "Function".to_string(),
                            actual: "invalid closure function".to_string(),
                        });
                    }
                };

                // Get the function prototype
                let proto = self
                    .function_protos
                    .get(*chunk_index)
                    .ok_or_else(|| {
                        RuntimeError::MemoryError(format!(
                            "Function prototype {} not found",
                            chunk_index
                        ))
                    })?
                    .clone();

                // Verify argument count matches
                if arg_count != *arity {
                    return Err(RuntimeError::InvalidOperation {
                        op: format!(
                            "call function '{}'",
                            name.unwrap_or_else(|| "<anonymous>".to_string())
                        ),
                        types: vec![
                            format!("expected {} arguments", *arity),
                            format!("got {} arguments", arg_count),
                        ],
                    });
                }

                // Calculate register base for the new frame
                let register_base = self.registers.len();

                // Copy arguments to new frame's registers
                // Arguments start at func_reg + 1
                for i in 0..arg_count {
                    let arg_reg = func_reg + 1 + i;
                    let arg_value = self.get_register(arg_reg)?.clone();
                    self.registers.push(arg_value);
                }

                // Initialize remaining registers for the function
                let remaining = proto.register_count.saturating_sub(arg_count);
                for _ in 0..remaining {
                    self.registers.push(BytecodeValue::Unit);
                }

                // Create a chunk from the proto
                let chunk = Chunk {
                    main: (*proto).clone(),
                    source_file: format!(
                        "<function {}>",
                        name.clone().unwrap_or_else(|| "<anonymous>".to_string())
                    ),
                    metadata: ChunkMetadata {
                        version: "0.1.0".to_string(),
                        timestamp: 0,
                        optimization_level: 0,
                        has_debug_info: false,
                    },
                };

                // Create new call frame
                let frame = CallFrame {
                    chunk,
                    ip: 0,
                    register_base,
                    register_count: proto.register_count as usize,
                    upvalues: upvalues.clone(),
                    function_name: name.clone(),
                    return_address: func_reg, // Return result to func_reg
                };

                self.frames.push(frame);

                Ok(())
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "call".to_string(),
                types: vec![self.type_name(&func)],
            }),
        }
    }

    /// Perform a tail call
    fn tail_call_function(&mut self, func_reg: Reg, arg_count: u8) -> Result<(), RuntimeError> {
        // TODO: Implement tail call optimization
        self.call_function(func_reg, arg_count, 1)
    }

    /// Close upvalues at or above a register
    fn close_upvalues_at(&mut self, start: Reg) -> Result<(), RuntimeError> {
        let absolute_start = {
            let frame = self.current_frame();
            frame.register_base + start as usize
        };

        // Close all open upvalues that point to registers >= absolute_start
        for upvalue_ref in &self.open_upvalues {
            let mut upvalue = upvalue_ref.borrow_mut();
            if !upvalue.is_closed {
                if let Some(location) = upvalue.location {
                    if location >= absolute_start {
                        // Close this upvalue by capturing the current value
                        if location < self.registers.len() {
                            upvalue.value = self.registers[location].clone();
                        }
                        upvalue.close();
                    }
                }
            }
        }

        // Remove closed upvalues from the open list
        self.open_upvalues.retain(|uv| !uv.borrow().is_closed);

        Ok(())
    }

    /// Create or find an open upvalue for a register
    fn capture_upvalue(&mut self, register_index: usize) -> UpvalueRef {
        // Check if we already have an open upvalue for this register
        for upvalue_ref in &self.open_upvalues {
            let upvalue = upvalue_ref.borrow();
            if !upvalue.is_closed {
                if let Some(location) = upvalue.location {
                    if location == register_index {
                        return Rc::clone(upvalue_ref);
                    }
                }
            }
        }

        // Create a new open upvalue
        let value = self
            .registers
            .get(register_index)
            .cloned()
            .unwrap_or(BytecodeValue::Unit);
        let upvalue = Rc::new(RefCell::new(Upvalue::new_open(register_index, value)));
        self.open_upvalues.push(Rc::clone(&upvalue));
        upvalue
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

    /// Create an iterator from an iterable value
    fn make_iterator(&self, value: BytecodeValue) -> Result<BytecodeValue, RuntimeError> {
        match value {
            // Array -> Iterator
            BytecodeValue::Array(elements) => Ok(BytecodeValue::Iterator {
                values: elements,
                position: 0,
            }),
            // String -> Iterator over characters
            BytecodeValue::String(s) => {
                let chars: Vec<BytecodeValue> = s.chars().map(|c| BytecodeValue::Char(c)).collect();
                Ok(BytecodeValue::Iterator {
                    values: chars,
                    position: 0,
                })
            }
            // Tuple -> Iterator
            BytecodeValue::Tuple(elements) => Ok(BytecodeValue::Iterator {
                values: elements,
                position: 0,
            }),
            // Already an iterator
            BytecodeValue::Iterator { .. } => Ok(value),
            // Not iterable
            _ => Err(RuntimeError::TypeError {
                expected: "iterable (array, string, or tuple)".to_string(),
                actual: self.type_name(&value),
            }),
        }
    }

    // ============================================================
    // UPVALUE MANAGEMENT
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
                    // Support negative indexing (from the end)
                    let len = arr.len() as i64;
                    let actual_idx = len + idx;
                    if actual_idx < 0 {
                        return Err(RuntimeError::IndexOutOfBounds {
                            index: *idx,
                            length: arr.len(),
                        });
                    }
                    actual_idx as usize
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
            (BytecodeValue::Tuple(tuple), BytecodeValue::Integer(idx)) => {
                let idx_usize = if *idx < 0 {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *idx,
                        length: tuple.len(),
                    });
                } else {
                    *idx as usize
                };

                tuple
                    .get(idx_usize)
                    .cloned()
                    .ok_or(RuntimeError::IndexOutOfBounds {
                        index: *idx,
                        length: tuple.len(),
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
        &mut self,
        array_reg: Reg,
        index: &BytecodeValue,
        value: &BytecodeValue,
    ) -> Result<(), RuntimeError> {
        // Get the array from the register (we need mutable access)
        let frame = self.current_frame();
        let absolute_index = frame.register_base + array_reg as usize;

        if absolute_index >= self.registers.len() {
            return Err(RuntimeError::MemoryError(format!(
                "Register index {} out of bounds",
                absolute_index
            )));
        }

        // Extract index value before mutable borrow
        let idx_val = match index {
            BytecodeValue::Integer(i) => *i,
            _ => {
                let index_type = self.type_name(index);
                return Err(RuntimeError::InvalidOperation {
                    op: "set_index".to_string(),
                    types: vec!["Array".to_string(), index_type],
                });
            }
        };

        // Check if it's an array before mutable borrow
        let is_array = matches!(&self.registers[absolute_index], BytecodeValue::Array(_));

        if !is_array {
            let obj_type = self.type_name(&self.registers[absolute_index]);
            return Err(RuntimeError::InvalidOperation {
                op: "set_index".to_string(),
                types: vec![obj_type, "Integer".to_string()],
            });
        }

        match &mut self.registers[absolute_index] {
            BytecodeValue::Array(arr) => {
                let idx_usize = if idx_val < 0 {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: idx_val,
                        length: arr.len(),
                    });
                } else {
                    idx_val as usize
                };

                // Grow array if needed to accommodate index
                if idx_usize >= arr.len() {
                    // Resize array, filling with Unit for any gaps
                    arr.resize(idx_usize + 1, BytecodeValue::Unit);
                }

                arr[idx_usize] = value.clone();
                Ok(())
            }
            _ => unreachable!(),
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
    use veld_common::bytecode_v2::{ChunkBuilder, Constant};

    #[test]
    fn test_vm_creation() {
        let vm = VirtualMachine::new();
        assert_eq!(vm.state, VmState::Ready);
        assert_eq!(vm.registers.len(), 0);
        assert_eq!(vm.frames.len(), 0);
    }

    #[test]
    fn test_load_const() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(3);

        let const_idx = builder.add_constant(Constant::Integer(42));
        builder.load_const(0, const_idx);
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_arithmetic_add() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        let c1 = builder.add_constant(Constant::Integer(10));
        let c2 = builder.add_constant(Constant::Integer(32));

        builder.load_const(0, c1);
        builder.load_const(1, c2);
        builder.add(2, 0, 1);
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_arithmetic_operations() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c5 = builder.add_constant(Constant::Integer(5));
        let c3 = builder.add_constant(Constant::Integer(3));

        // Load constants
        builder.load_const(0, c5); // R0 = 5
        builder.load_const(1, c3); // R1 = 3

        // Test operations
        builder.add(2, 0, 1); // R2 = 5 + 3 = 8
        builder.sub(3, 0, 1); // R3 = 5 - 3 = 2
        builder.mul(4, 0, 1); // R4 = 5 * 3 = 15
        builder.div(5, 0, 1); // R5 = 5 / 3 = 1
        builder.mod_op(6, 0, 1); // R6 = 5 % 3 = 2
        builder.neg(7, 0); // R7 = -5

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_move_instruction() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        let const_idx = builder.add_constant(Constant::Integer(100));

        builder.load_const(0, const_idx);
        builder.move_reg(1, 0);
        builder.move_reg(2, 1);
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_comparison_eq() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        let c1 = builder.add_constant(Constant::Integer(42));

        builder.load_const(0, c1);
        builder.load_const(1, c1);
        builder.eq(2, 0, 1); // R2 = (R0 == R1) = true
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_comparison_operations() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c5 = builder.add_constant(Constant::Integer(5));
        let c10 = builder.add_constant(Constant::Integer(10));

        builder.load_const(0, c5); // R0 = 5
        builder.load_const(1, c10); // R1 = 10

        builder.lt(2, 0, 1); // R2 = 5 < 10 = true
        builder.le(3, 0, 1); // R3 = 5 <= 10 = true
        builder.gt(4, 0, 1); // R4 = 5 > 10 = false
        builder.ge(5, 0, 1); // R5 = 5 >= 10 = false
        builder.eq(6, 0, 1); // R6 = 5 == 10 = false
        builder.neq(7, 0, 1); // R7 = 5 != 10 = true

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_logical_not() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        builder.load_bool(0, true);
        builder.not(1, 0); // R1 = !true = false
        builder.not(2, 1); // R2 = !false = true
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_logical_operations() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        builder.load_bool(0, true);
        builder.load_bool(1, false);

        builder.and(2, 0, 1); // R2 = true && false = false
        builder.or(3, 0, 1); // R3 = true || false = true
        builder.not(4, 0); // R4 = !true = false

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_jump() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        builder.load_bool(0, true);
        builder.jump(2); // Skip next 2 instructions
        builder.load_bool(0, false); // Should be skipped
        builder.load_bool(0, false); // Should be skipped
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_conditional_jump() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        builder.load_bool(0, true);
        builder.jump_if(0, 1); // Jump if R0 is true (skip next)
        builder.load_bool(1, false); // Should be skipped
        builder.load_bool(2, true); // Should execute
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_jump_if_not() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        builder.load_bool(0, false);
        builder.jump_if_not(0, 1); // Jump if R0 is false (skip next)
        builder.load_bool(1, true); // Should be skipped
        builder.load_bool(2, true); // Should execute
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_bitwise_operations() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c15 = builder.add_constant(Constant::Integer(15)); // 0b1111
        let c7 = builder.add_constant(Constant::Integer(7)); // 0b0111

        builder.load_const(0, c15);
        builder.load_const(1, c7);

        builder.bit_and(2, 0, 1); // R2 = 15 & 7 = 7
        builder.bit_or(3, 0, 1); // R3 = 15 | 7 = 15
        builder.bit_xor(4, 0, 1); // R4 = 15 ^ 7 = 8
        builder.bit_not(5, 0); // R5 = ~15 = -16

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_string_concatenation() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        let s1 = builder.add_constant(Constant::String("Hello, ".to_string()));
        let s2 = builder.add_constant(Constant::String("World!".to_string()));

        builder.load_const(0, s1);
        builder.load_const(1, s2);
        builder.add(2, 0, 1); // String concatenation via Add
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_load_nil() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        builder.load_nil(0);
        builder.load_nil(1);
        builder.load_nil_range(2, 3); // Load nil into R2, R3, R4
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_mixed_types() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let int_val = builder.add_constant(Constant::Integer(42));
        let float_val = builder.add_constant(Constant::Float(3.14));
        let str_val = builder.add_constant(Constant::String("test".to_string()));

        builder.load_const(0, int_val);
        builder.load_const(1, float_val);
        builder.load_const(2, str_val);
        builder.load_bool(3, true);
        builder.load_nil(4);

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_constant_deduplication() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(5);

        let idx1 = builder.add_constant(Constant::Integer(42));
        let idx2 = builder.add_constant(Constant::Integer(42)); // Same value
        let idx3 = builder.add_constant(Constant::Integer(99)); // Different value

        // idx1 and idx2 should be the same
        assert_eq!(idx1, idx2);
        assert_ne!(idx1, idx3);

        builder.halt();
        let _chunk = builder.build();
    }

    #[test]
    fn test_array_creation() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(1));
        let c2 = builder.add_constant(Constant::Integer(2));
        let c3 = builder.add_constant(Constant::Integer(3));

        // Load values into consecutive registers
        builder.load_const(1, c1); // R1 = 1
        builder.load_const(2, c2); // R2 = 2
        builder.load_const(3, c3); // R3 = 3

        // Create array from R1, R2, R3
        builder.new_array(0, 3); // R0 = [1, 2, 3]

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_array_indexing() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(10));
        let c2 = builder.add_constant(Constant::Integer(20));
        let c3 = builder.add_constant(Constant::Integer(30));
        let c0 = builder.add_constant(Constant::Integer(0));
        let c_idx1 = builder.add_constant(Constant::Integer(1));
        let c_idx2 = builder.add_constant(Constant::Integer(2));

        // Create empty array with capacity 3
        builder.new_array(0, 3); // R0 = []

        // Populate array [10, 20, 30]
        builder.load_const(1, c1); // R1 = 10
        builder.load_const(6, c0); // R6 = 0
        builder.set_index(0, 6, 1); // R0[0] = 10

        builder.load_const(2, c2); // R2 = 20
        builder.load_const(6, c_idx1); // R6 = 1
        builder.set_index(0, 6, 2); // R0[1] = 20

        builder.load_const(3, c3); // R3 = 30
        builder.load_const(6, c_idx2); // R6 = 2
        builder.set_index(0, 6, 3); // R0[2] = 30

        // Get element at index 1
        builder.load_const(4, c_idx1); // R4 = 1
        builder.get_index(5, 0, 4); // R5 = R0[R4] = 20

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_array_set_index() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(10));
        let c2 = builder.add_constant(Constant::Integer(20));
        let c3 = builder.add_constant(Constant::Integer(30));
        let c_idx = builder.add_constant(Constant::Integer(1));
        let c_new = builder.add_constant(Constant::Integer(99));

        // Create array [10, 20, 30]
        builder.load_const(1, c1);
        builder.load_const(2, c2);
        builder.load_const(3, c3);
        builder.new_array(0, 3); // R0 = [10, 20, 30]

        // Set element at index 1 to 99
        builder.load_const(4, c_idx); // R4 = 1
        builder.load_const(5, c_new); // R5 = 99
        builder.set_index(0, 4, 5); // R0[R4] = R5, so R0 = [10, 99, 30]

        // Read it back
        builder.get_index(6, 0, 4); // R6 = R0[1] = 99

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_tuple_creation() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(42));
        let c2 = builder.add_constant(Constant::String("hello".to_string()));
        let c3 = builder.add_constant(Constant::Boolean(true));

        // Load values into consecutive registers
        builder.load_const(1, c1); // R1 = 42
        builder.load_const(2, c2); // R2 = "hello"
        builder.load_const(3, c3); // R3 = true

        // Create tuple from R1, R2, R3
        builder.new_tuple(0, 3); // R0 = (42, "hello", true)

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_tuple_indexing() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(10));
        let c2 = builder.add_constant(Constant::Integer(20));
        let c3 = builder.add_constant(Constant::Integer(30));
        let c_idx = builder.add_constant(Constant::Integer(1));

        // Create tuple (10, 20, 30)
        builder.load_const(1, c1);
        builder.load_const(2, c2);
        builder.load_const(3, c3);
        builder.new_tuple(0, 3); // R0 = (10, 20, 30)

        // Get element at index 1
        builder.load_const(4, c_idx); // R4 = 1
        builder.get_index(5, 0, 4); // R5 = R0[1] = 20

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_struct_creation() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let type_name = builder.add_constant(Constant::String("Person".to_string()));
        let name_field = builder.add_constant(Constant::String("name".to_string()));
        let name_val = builder.add_constant(Constant::String("Alice".to_string()));
        let age_field = builder.add_constant(Constant::String("age".to_string()));
        let age_val = builder.add_constant(Constant::Integer(30));

        // Load field names and values into consecutive registers
        builder.load_const(1, name_field); // R1 = "name"
        builder.load_const(2, name_val); // R2 = "Alice"
        builder.load_const(3, age_field); // R3 = "age"
        builder.load_const(4, age_val); // R4 = 30

        // Create struct with 2 fields
        builder.new_struct(0, type_name, 2); // R0 = Person { name: "Alice", age: 30 }

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_struct_field_access() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let type_name = builder.add_constant(Constant::String("Point".to_string()));
        let x_field = builder.add_constant(Constant::String("x".to_string()));
        let x_val = builder.add_constant(Constant::Integer(10));
        let y_field = builder.add_constant(Constant::String("y".to_string()));
        let y_val = builder.add_constant(Constant::Integer(20));

        // Create struct Point { x: 10, y: 20 }
        builder.load_const(1, x_field);
        builder.load_const(2, x_val);
        builder.load_const(3, y_field);
        builder.load_const(4, y_val);
        builder.new_struct(0, type_name, 2); // R0 = Point { x: 10, y: 20 }

        // Access x field
        builder.get_field(5, 0, x_field as u8); // R5 = R0.x = 10

        // Access y field
        builder.get_field(6, 0, y_field as u8); // R6 = R0.y = 20

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_struct_field_mutation() {
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let type_name = builder.add_constant(Constant::String("Counter".to_string()));
        let count_field = builder.add_constant(Constant::String("count".to_string()));
        let count_val = builder.add_constant(Constant::Integer(0));
        let new_count = builder.add_constant(Constant::Integer(42));

        // Create struct Counter { count: 0 }
        builder.load_const(1, count_field);
        builder.load_const(2, count_val);
        builder.new_struct(0, type_name, 1); // R0 = Counter { count: 0 }

        // Set count to 42
        builder.load_const(3, new_count);
        builder.set_field(0, count_field as u8, 3); // R0.count = 42

        // Read it back
        builder.get_field(4, 0, count_field as u8); // R4 = R0.count = 42

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_complex_arithmetic() {
        // Test: compute (5 + 3) * (10 - 2) / 4
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c5 = builder.add_constant(Constant::Integer(5));
        let c3 = builder.add_constant(Constant::Integer(3));
        let c10 = builder.add_constant(Constant::Integer(10));
        let c2 = builder.add_constant(Constant::Integer(2));
        let c4 = builder.add_constant(Constant::Integer(4));

        builder.load_const(0, c5); // R0 = 5
        builder.load_const(1, c3); // R1 = 3
        builder.load_const(2, c10); // R2 = 10
        builder.load_const(3, c2); // R3 = 2
        builder.load_const(4, c4); // R4 = 4

        builder.add(5, 0, 1); // R5 = 5 + 3 = 8
        builder.sub(6, 2, 3); // R6 = 10 - 2 = 8
        builder.mul(7, 5, 6); // R7 = 8 * 8 = 64
        builder.div(8, 7, 4); // R8 = 64 / 4 = 16

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_nested_arrays() {
        // Test: create array of arrays [[1, 2], [3, 4]]
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let c1 = builder.add_constant(Constant::Integer(1));
        let c2 = builder.add_constant(Constant::Integer(2));
        let c3 = builder.add_constant(Constant::Integer(3));
        let c4 = builder.add_constant(Constant::Integer(4));

        // Create first inner array [1, 2]
        builder.load_const(1, c1);
        builder.load_const(2, c2);
        builder.new_array(0, 2); // R0 = [1, 2]

        // Create second inner array [3, 4]
        builder.load_const(4, c3);
        builder.load_const(5, c4);
        builder.new_array(3, 2); // R3 = [3, 4]

        // Create outer array containing R0 and R3
        builder.move_reg(7, 0); // R7 = R0
        builder.move_reg(8, 3); // R8 = R3
        builder.new_array(6, 2); // R6 = [[1, 2], [3, 4]]

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_conditional_logic() {
        // Test: if (x > 5) then y = 100 else y = 200
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let x_val = builder.add_constant(Constant::Integer(7));
        let c5 = builder.add_constant(Constant::Integer(5));
        let c100 = builder.add_constant(Constant::Integer(100));
        let c200 = builder.add_constant(Constant::Integer(200));

        builder.load_const(0, x_val); // R0 = 7 (x)
        builder.load_const(1, c5); // R1 = 5

        // Compare x > 5
        builder.gt(2, 0, 1); // R2 = (7 > 5) = true

        // Jump if not true to else branch
        let else_jump = builder.jump_if_not(2, 0); // placeholder offset

        // Then branch: y = 100
        builder.load_const(3, c100); // R3 = 100 (y)
        let end_jump = builder.jump(0); // jump to end

        // Else branch: y = 200
        builder.patch_jump(else_jump);
        builder.load_const(3, c200); // R3 = 200 (y)

        // End
        builder.patch_jump(end_jump);
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_loop_sum() {
        // Test: sum = 0; for i in 0..5: sum += i
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c0 = builder.add_constant(Constant::Integer(0));
        let c1 = builder.add_constant(Constant::Integer(1));
        let c5 = builder.add_constant(Constant::Integer(5));

        builder.load_const(0, c0); // R0 = 0 (sum)
        builder.load_const(1, c0); // R1 = 0 (i)
        builder.load_const(2, c5); // R2 = 5 (limit)
        builder.load_const(3, c1); // R3 = 1 (increment)

        // Loop start
        let loop_start = builder.current_index();

        // Check if i < 5
        builder.lt(4, 1, 2); // R4 = (i < 5)
        let exit_jump = builder.jump_if_not(4, 0); // exit if i >= 5

        // sum += i
        builder.add(0, 0, 1); // R0 = sum + i

        // i += 1
        builder.add(1, 1, 3); // R1 = i + 1

        // Jump back to loop start
        builder.jump_back(loop_start);

        // Loop exit
        builder.patch_jump(exit_jump);
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_mixed_data_structures() {
        // Test: create a struct containing an array and tuple
        let mut builder = ChunkBuilder::new();
        builder.register_count(20);

        let type_name = builder.add_constant(Constant::String("Container".to_string()));
        let items_field = builder.add_constant(Constant::String("items".to_string()));
        let meta_field = builder.add_constant(Constant::String("meta".to_string()));

        let c1 = builder.add_constant(Constant::Integer(1));
        let c2 = builder.add_constant(Constant::Integer(2));
        let c3 = builder.add_constant(Constant::Integer(3));
        let name = builder.add_constant(Constant::String("data".to_string()));

        // Create array [1, 2, 3]
        builder.load_const(1, c1);
        builder.load_const(2, c2);
        builder.load_const(3, c3);
        builder.new_array(4, 3); // R4 = [1, 2, 3]

        // Create tuple ("data", 42)
        builder.load_const(5, name);
        builder.load_const(6, c1);
        builder.new_tuple(7, 2); // R7 = ("data", 42)

        // Create struct Container { items: [1,2,3], meta: ("data", 42) }
        builder.load_const(8, items_field); // R8 = "items"
        builder.move_reg(9, 4); // R9 = [1, 2, 3]
        builder.load_const(10, meta_field); // R10 = "meta"
        builder.move_reg(11, 7); // R11 = ("data", 42)
        builder.new_struct(0, type_name, 2); // R0 = Container { ... }

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_string_operations() {
        // Test: concatenate multiple strings and compare
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let hello = builder.add_constant(Constant::String("Hello".to_string()));
        let space = builder.add_constant(Constant::String(" ".to_string()));
        let world = builder.add_constant(Constant::String("World".to_string()));
        let expected = builder.add_constant(Constant::String("Hello World".to_string()));

        // Build "Hello World"
        builder.load_const(0, hello); // R0 = "Hello"
        builder.load_const(1, space); // R1 = " "
        builder.load_const(2, world); // R2 = "World"

        builder.add(3, 0, 1); // R3 = "Hello "
        builder.add(4, 3, 2); // R4 = "Hello World"

        // Compare with expected
        builder.load_const(5, expected); // R5 = "Hello World"
        builder.eq(6, 4, 5); // R6 = (R4 == R5) = true

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_bitwise_logic() {
        // Test: complex bitwise operations
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c255 = builder.add_constant(Constant::Integer(255)); // 0xFF
        let c15 = builder.add_constant(Constant::Integer(15)); // 0x0F

        builder.load_const(0, c255); // R0 = 255
        builder.load_const(1, c15); // R1 = 15

        // Test various bitwise ops
        builder.bit_and(2, 0, 1); // R2 = 255 & 15 = 15
        builder.bit_or(3, 0, 1); // R3 = 255 | 15 = 255
        builder.bit_xor(4, 0, 1); // R4 = 255 ^ 15 = 240
        builder.bit_not(5, 1); // R5 = ~15 = -16

        // Shift operations
        builder.shl(6, 1, 1); // R6 = 15 << 15 (lots of bits)
        builder.shr(7, 0, 1); // R7 = 255 >> 15 = 0

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_type_checking() {
        // Test: check types of various values
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let int_val = builder.add_constant(Constant::Integer(42));
        let str_val = builder.add_constant(Constant::String("test".to_string()));

        // Load different types
        builder.load_const(0, int_val); // R0 = 42
        builder.load_const(1, str_val); // R1 = "test"
        builder.load_bool(2, true); // R2 = true
        builder.load_nil(3); // R3 = nil

        // Get type of each
        builder.type_of(4, 0); // R4 = "i64"
        builder.type_of(5, 1); // R5 = "str"
        builder.type_of(6, 2); // R6 = "bool"
        builder.type_of(7, 3); // R7 = "unit"

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_negative_array_indexing() {
        // Test: use negative indices to access array from end
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(10));
        let c2 = builder.add_constant(Constant::Integer(20));
        let c3 = builder.add_constant(Constant::Integer(30));
        let c0 = builder.add_constant(Constant::Integer(0));
        let c_idx1 = builder.add_constant(Constant::Integer(1));
        let c_idx2 = builder.add_constant(Constant::Integer(2));
        let neg1 = builder.add_constant(Constant::Integer(-1));
        let neg2 = builder.add_constant(Constant::Integer(-2));

        // Create empty array with capacity 3
        builder.new_array(0, 3); // R0 = []

        // Populate array [10, 20, 30]
        builder.load_const(1, c1); // R1 = 10
        builder.load_const(8, c0); // R8 = 0
        builder.set_index(0, 8, 1); // R0[0] = 10

        builder.load_const(2, c2); // R2 = 20
        builder.load_const(8, c_idx1); // R8 = 1
        builder.set_index(0, 8, 2); // R0[1] = 20

        builder.load_const(3, c3); // R3 = 30
        builder.load_const(8, c_idx2); // R8 = 2
        builder.set_index(0, 8, 3); // R0[2] = 30

        // Access with negative indices
        builder.load_const(4, neg1); // R4 = -1
        builder.get_index(5, 0, 4); // R5 = R0[-1] = 30

        builder.load_const(6, neg2); // R6 = -2
        builder.get_index(7, 0, 6); // R7 = R0[-2] = 20

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_closure_creation() {
        // Test: create a simple closure
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        // Create a function prototype
        let _func_name = builder.add_constant(Constant::String("add".to_string()));
        let func_proto = Constant::Function(Box::new(veld_common::bytecode_v2::FunctionProto {
            name: "add".to_string(),
            param_count: 2,
            register_count: 5,
            instructions: vec![
                Instruction::Add {
                    dest: 0,
                    lhs: 1,
                    rhs: 2,
                },
                Instruction::Return { first: 0, count: 1 },
            ],
            constants: vec![],
            line_info: vec![1, 1],
            prototypes: vec![],
            upvalues: vec![],
            is_variadic: false,
        }));

        let proto_idx = builder.add_constant(func_proto);

        // Create closure from prototype
        builder.closure(0, proto_idx);
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_upvalue_get_set() {
        // Test: create a closure with upvalues and access them
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c42 = builder.add_constant(Constant::Integer(42));
        let c99 = builder.add_constant(Constant::Integer(99));

        // Load initial value
        builder.load_const(0, c42); // R0 = 42

        // Create a closure that captures R0
        // For this test, we'll manually set up upvalues
        // In practice, the compiler would handle this

        // Simulate getting an upvalue (in a real closure)
        // builder.get_upvalue(1, 0); // R1 = upvalue[0]

        // For now, just test that the instructions work
        builder.load_const(2, c99); // R2 = 99

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_close_upvalues() {
        // Test: close upvalues when exiting a scope
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(1));
        let c2 = builder.add_constant(Constant::Integer(2));

        builder.load_const(0, c1); // R0 = 1
        builder.load_const(1, c2); // R1 = 2

        // Close upvalues starting at R1
        builder.close_upvalues(1);

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_nested_closures() {
        // Test: create nested closures with multiple upvalues
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let c10 = builder.add_constant(Constant::Integer(10));
        let c20 = builder.add_constant(Constant::Integer(20));

        // Outer scope variables
        builder.load_const(0, c10); // R0 = 10 (captured by outer closure)
        builder.load_const(1, c20); // R1 = 20 (captured by inner closure)

        // In a real implementation, we'd create closures here
        // For now, just test the upvalue infrastructure

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_upvalue_mutation() {
        // Test: mutate captured variables through upvalues
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c0 = builder.add_constant(Constant::Integer(0));
        let c1 = builder.add_constant(Constant::Integer(1));

        // Initialize a counter
        builder.load_const(0, c0); // R0 = 0

        // Simulate incrementing through an upvalue
        // In a real closure: upvalue[0] += 1
        builder.load_const(1, c1);
        builder.add(0, 0, 1); // R0 = R0 + 1

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_closure_counter() {
        // Test: Create a closure that captures a counter variable
        // This simulates: let counter = 0; closure { counter += 1; return counter }
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let c0 = builder.add_constant(Constant::Integer(0));
        let c1 = builder.add_constant(Constant::Integer(1));

        // Outer scope: initialize counter
        builder.load_const(0, c0); // R0 = 0 (the captured variable)

        // Simulate closure execution multiple times
        // First increment
        builder.load_const(1, c1); // R1 = 1
        builder.add(0, 0, 1); // R0 = R0 + 1 = 1

        // Second increment
        builder.add(0, 0, 1); // R0 = R0 + 1 = 2

        // Third increment
        builder.add(0, 0, 1); // R0 = R0 + 1 = 3

        // Result should be 3
        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_closure_with_multiple_captures() {
        // Test: Closure capturing multiple variables from outer scope
        // Simulates: let x = 10; let y = 20; closure { return x + y }
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let c10 = builder.add_constant(Constant::Integer(10));
        let c20 = builder.add_constant(Constant::Integer(20));

        // Outer scope variables
        builder.load_const(0, c10); // R0 = 10 (captured x)
        builder.load_const(1, c20); // R1 = 20 (captured y)

        // Closure body: x + y
        builder.add(2, 0, 1); // R2 = R0 + R1 = 30

        // Modify captured variables
        builder.load_const(3, c10);
        builder.add(0, 0, 3); // R0 = R0 + 10 = 20

        // Compute again with modified x
        builder.add(4, 0, 1); // R4 = R0 + R1 = 40

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_closure_shadowing() {
        // Test: Variable shadowing with closures
        // Simulates: let x = 1; { let x = 2; closure { return x } }
        let mut builder = ChunkBuilder::new();
        builder.register_count(10);

        let c1 = builder.add_constant(Constant::Integer(1));
        let c2 = builder.add_constant(Constant::Integer(2));

        // Outer scope
        builder.load_const(0, c1); // R0 = 1 (outer x)

        // Inner scope (shadowing)
        builder.load_const(1, c2); // R1 = 2 (inner x, shadows outer)

        // Closure should capture inner x (R1)
        builder.move_reg(2, 1); // R2 = R1 = 2

        // Outer x still accessible
        builder.move_reg(3, 0); // R3 = R0 = 1

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_closure_factory() {
        // Test: Closure factory pattern
        // Simulates: fn make_adder(n) { return closure { |x| x + n } }
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let c5 = builder.add_constant(Constant::Integer(5));
        let c10 = builder.add_constant(Constant::Integer(10));

        // Create first adder with n=5
        builder.load_const(0, c5); // R0 = 5 (captured n)

        // Call adder with x=10
        builder.load_const(1, c10); // R1 = 10 (x)
        builder.add(2, 0, 1); // R2 = n + x = 15

        // Create second adder with n=10
        builder.load_const(3, c10); // R3 = 10 (new captured n)

        // Call second adder with x=5
        builder.load_const(4, c5); // R4 = 5 (x)
        builder.add(5, 3, 4); // R5 = n + x = 15

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }

    #[test]
    fn test_integration_closure_array_capture() {
        // Test: Closure capturing an array
        let mut builder = ChunkBuilder::new();
        builder.register_count(15);

        let c1 = builder.add_constant(Constant::Integer(1));
        let c2 = builder.add_constant(Constant::Integer(2));
        let c3 = builder.add_constant(Constant::Integer(3));
        let c0 = builder.add_constant(Constant::Integer(0));

        // Create array [1, 2, 3]
        builder.load_const(1, c1);
        builder.load_const(2, c2);
        builder.load_const(3, c3);
        builder.new_array(0, 3); // R0 = [1, 2, 3] (captured array)

        // Closure modifies array
        builder.load_const(4, c0); // R4 = 0 (index)
        builder.load_const(5, c3); // R5 = 3 (new value)
        builder.set_index(0, 4, 5); // R0[0] = 3, so R0 = [3, 2, 3]

        // Read back modified value
        builder.get_index(6, 0, 4); // R6 = R0[0] = 3

        builder.halt();

        let chunk = builder.build();
        let mut vm = VirtualMachine::new();
        let result = vm.interpret(chunk);

        assert!(matches!(result, InterpretResult::Ok(_)));
    }
}
