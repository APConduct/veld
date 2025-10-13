use crate::chunk::Chunk;
use crate::instruction::Instruction;
use crate::value::{BytecodeValue, RuntimeError, UpvalueRef};
use std::collections::HashMap;
use tracing::{trace, warn};

/// The Veld virtual machine for executing bytecode
pub struct VirtualMachine {
    /// Stack for operands and temporary values
    stack: Vec<BytecodeValue>,

    /// Call frame stack
    frames: Vec<CallFrame>,

    /// Global variables
    globals: HashMap<String, BytecodeValue>,

    /// Open upvalues (for closures)
    open_upvalues: Vec<Upvalue>,

    /// Native function registry
    native_functions: HashMap<String, fn(&[BytecodeValue]) -> Result<BytecodeValue, RuntimeError>>,

    /// Maximum stack size to prevent overflow
    max_stack_size: usize,

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

    /// Stack base for this frame
    stack_base: usize,

    /// Number of locals in this frame
    local_count: usize,

    /// Closure upvalues (if this is a closure call)
    upvalues: Vec<UpvalueRef>,
}

/// Represents an upvalue (captured variable)
#[derive(Debug, Clone)]
pub struct Upvalue {
    /// Index in the stack where the value is stored
    stack_index: usize,

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
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: Vec::new(),
            native_functions: HashMap::new(),
            max_stack_size: 1024 * 64, // 64K stack slots
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

    /// Set the maximum stack size
    pub fn set_max_stack_size(&mut self, size: usize) {
        self.max_stack_size = size;
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
        self.stack.clear();
        self.frames.clear();

        // Create initial call frame
        let frame = CallFrame {
            chunk,
            ip: 0,
            stack_base: 0,
            local_count: 0,
            upvalues: Vec::new(),
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
            if self.frames.is_empty() {
                // No more frames, return top of stack or Unit
                return Ok(self.stack.pop().unwrap_or(BytecodeValue::Unit));
            }

            let instruction = self.read_instruction()?;

            if self.trace_execution {
                self.trace_instruction(&instruction);
            }

            match instruction {
                Instruction::LoadConstant(index) => {
                    let constant = self.read_constant(index)?;
                    self.push(constant)?;
                }

                Instruction::Pop => {
                    self.pop()?;
                }

                Instruction::Duplicate => {
                    let value = self.peek(0)?.clone();
                    self.push(value)?;
                }

                Instruction::Swap => {
                    let len = self.stack.len();
                    if len < 2 {
                        return Err(RuntimeError::StackUnderflow);
                    }
                    self.stack.swap(len - 1, len - 2);
                }

                Instruction::LoadLocal(index) => {
                    let value = self.get_local(index as usize)?.clone();
                    self.push(value)?;
                }

                Instruction::StoreLocal(index) => {
                    let value = self.pop()?;
                    self.set_local(index as usize, value)?;
                }

                Instruction::LoadGlobal(index) => {
                    let name = match self.read_constant(index)? {
                        BytecodeValue::String(name) => name,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "string".to_string(),
                                actual: "other".to_string(),
                            });
                        }
                    };

                    match self.globals.get(&name) {
                        Some(value) => self.push(value.clone())?,
                        None => return Err(RuntimeError::UndefinedVariable(name)),
                    }
                }

                Instruction::StoreGlobal(index) => {
                    let name = match self.read_constant(index)? {
                        BytecodeValue::String(name) => name,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "string".to_string(),
                                actual: "other".to_string(),
                            });
                        }
                    };

                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }

                // Arithmetic operations
                Instruction::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.add_values(&a, &b)?;
                    self.push(result)?;
                }
                Instruction::Subtract => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.subtract_values(&a, &b)?;
                    self.push(result)?;
                }
                Instruction::Multiply => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.multiply_values(&a, &b)?;
                    self.push(result)?;
                }
                Instruction::Divide => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.divide_values(&a, &b)?;
                    self.push(result)?;
                }
                Instruction::Modulo => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.modulo_values(&a, &b)?;
                    self.push(result)?;
                }
                Instruction::Negate => {
                    let a = self.pop()?;
                    let result = self.negate_value(&a)?;
                    self.push(result)?;
                }

                // Comparison operations
                Instruction::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(BytecodeValue::Boolean(a.equals(&b)))?;
                }

                Instruction::NotEqual => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(BytecodeValue::Boolean(!a.equals(&b)))?;
                }

                Instruction::Greater => self.comparison_op(|a, b| a > b)?,
                Instruction::GreaterEqual => self.comparison_op(|a, b| a >= b)?,
                Instruction::Less => self.comparison_op(|a, b| a < b)?,
                Instruction::LessEqual => self.comparison_op(|a, b| a <= b)?,

                // Logical operations
                Instruction::LogicalAnd => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.is_truthy() && b.is_truthy();
                    self.push(BytecodeValue::Boolean(result))?;
                }

                Instruction::LogicalOr => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = a.is_truthy() || b.is_truthy();
                    self.push(BytecodeValue::Boolean(result))?;
                }

                Instruction::LogicalNot => {
                    let value = self.pop()?;
                    self.push(BytecodeValue::Boolean(value.is_falsy()))?;
                }

                // Control flow
                Instruction::Jump(offset) => {
                    self.jump(offset as isize)?;
                }

                Instruction::JumpIfFalse(offset) => {
                    if self.peek(0)?.is_falsy() {
                        self.jump(offset as isize)?;
                    }
                }

                Instruction::JumpIfTrue(offset) => {
                    if self.peek(0)?.is_truthy() {
                        self.jump(offset as isize)?;
                    }
                }

                Instruction::PopJumpIfFalse(offset) => {
                    if self.pop()?.is_falsy() {
                        self.jump(offset as isize)?;
                    }
                }

                Instruction::PopJumpIfTrue(offset) => {
                    if self.pop()?.is_truthy() {
                        self.jump(offset as isize)?;
                    }
                }

                Instruction::Return => {
                    let result = if self.stack.is_empty() {
                        BytecodeValue::Unit
                    } else {
                        self.pop()?
                    };

                    // Close upvalues for this frame
                    self.close_upvalues(self.current_frame().stack_base);

                    // Pop the current frame
                    self.frames.pop();

                    if self.frames.is_empty() {
                        return Ok(result);
                    }

                    // Restore stack to frame base and push result
                    let frame_base = self.current_frame().stack_base;
                    self.stack.truncate(frame_base);
                    self.push(result)?;
                }

                Instruction::Call(argc) => {
                    self.call_value(argc as usize)?;
                }

                Instruction::NewArray(size) => {
                    let mut elements = Vec::new();
                    for _ in 0..(size as usize) {
                        elements.push(self.pop()?);
                    }
                    elements.reverse(); // They were popped in reverse order
                    self.push(BytecodeValue::Array(elements))?;
                }

                Instruction::GetIndex => {
                    let index = self.pop()?;
                    let array = self.pop()?;
                    let result = self.get_array_element(&array, &index)?;
                    self.push(result)?;
                }

                Instruction::SetIndex => {
                    let value = self.pop()?;
                    let index = self.pop()?;
                    let mut array = self.pop()?;
                    self.set_array_element(&mut array, &index, value)?;
                    self.push(array)?;
                }

                Instruction::NewTuple(size) => {
                    let mut elements = Vec::new();
                    for _ in 0..(size as usize) {
                        elements.push(self.pop()?);
                    }
                    elements.reverse();
                    self.push(BytecodeValue::Tuple(elements))?;
                }

                Instruction::Print => {
                    let value = self.pop()?;
                    println!("{}", value);
                }

                Instruction::Halt => {
                    self.state = VmState::Halted;
                    return Ok(self.stack.pop().unwrap_or(BytecodeValue::Unit));
                }

                Instruction::Nop => {
                    // Do nothing
                }

                _ => {
                    warn!("Unimplemented instruction: {:?}", instruction);
                    return Err(RuntimeError::InvalidOperation {
                        op: format!("{:?}", instruction),
                        types: vec!["any".to_string()],
                    });
                }
            }
        }
    }

    /// Read the next instruction
    fn read_instruction(&mut self) -> Result<Instruction, RuntimeError> {
        let frame = self.current_frame_mut();
        if frame.ip >= frame.chunk.instructions.len() {
            return Err(RuntimeError::InvalidOperation {
                op: "instruction pointer out of bounds".to_string(),
                types: vec![],
            });
        }

        let instruction = frame.chunk.instructions[frame.ip].clone();
        frame.ip += 1;
        Ok(instruction)
    }

    /// Read a constant from the constant pool
    fn read_constant(&self, index: u16) -> Result<BytecodeValue, RuntimeError> {
        let frame = self.current_frame();
        frame
            .chunk
            .constants
            .get(index as usize)
            .cloned()
            .ok_or_else(|| RuntimeError::InvalidOperation {
                op: "invalid constant index".to_string(),
                types: vec![index.to_string()],
            })
    }

    /// Push a value onto the stack
    fn push(&mut self, value: BytecodeValue) -> Result<(), RuntimeError> {
        if self.stack.len() >= self.max_stack_size {
            return Err(RuntimeError::StackOverflow);
        }

        self.stack.push(value);
        Ok(())
    }

    /// Pop a value from the stack
    fn pop(&mut self) -> Result<BytecodeValue, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    /// Peek at a value on the stack without removing it
    fn peek(&self, distance: usize) -> Result<&BytecodeValue, RuntimeError> {
        if distance >= self.stack.len() {
            return Err(RuntimeError::StackUnderflow);
        }

        Ok(&self.stack[self.stack.len() - 1 - distance])
    }

    /// Get the current call frame
    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect("No call frame")
    }

    /// Get the current call frame mutably
    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("No call frame")
    }

    /// Jump by the given offset
    fn jump(&mut self, offset: isize) -> Result<(), RuntimeError> {
        let frame = self.current_frame_mut();
        let new_ip = (frame.ip as isize + offset) as usize;

        if new_ip > frame.chunk.instructions.len() {
            return Err(RuntimeError::InvalidOperation {
                op: "jump out of bounds".to_string(),
                types: vec![],
            });
        }

        frame.ip = new_ip;
        Ok(())
    }

    /// Get a local variable
    fn get_local(&self, index: usize) -> Result<&BytecodeValue, RuntimeError> {
        let frame = self.current_frame();
        let stack_index = frame.stack_base + index;

        self.stack
            .get(stack_index)
            .ok_or_else(|| RuntimeError::InvalidOperation {
                op: "invalid local variable index".to_string(),
                types: vec![index.to_string()],
            })
    }

    /// Set a local variable
    fn set_local(&mut self, index: usize, value: BytecodeValue) -> Result<(), RuntimeError> {
        let frame_base = self.current_frame().stack_base;
        let stack_index = frame_base + index;

        if stack_index >= self.stack.len() {
            return Err(RuntimeError::InvalidOperation {
                op: "invalid local variable index".to_string(),
                types: vec![index.to_string()],
            });
        }

        self.stack[stack_index] = value;
        Ok(())
    }

    /// Perform a comparison operation
    fn comparison_op<F>(&mut self, op: F) -> Result<(), RuntimeError>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let b = self.pop()?;
        let a = self.pop()?;

        let (a_val, b_val) = match (&a, &b) {
            (BytecodeValue::Integer(a), BytecodeValue::Integer(b)) => (*a as f64, *b as f64),
            (BytecodeValue::Float(a), BytecodeValue::Float(b)) => (*a, *b),
            (BytecodeValue::Integer(a), BytecodeValue::Float(b)) => (*a as f64, *b),
            (BytecodeValue::Float(a), BytecodeValue::Integer(b)) => (*a, *b as f64),
            _ => {
                return Err(RuntimeError::InvalidOperation {
                    op: "comparison".to_string(),
                    types: vec![a.type_name().to_string(), b.type_name().to_string()],
                });
            }
        };

        let result = op(a_val, b_val);
        self.push(BytecodeValue::Boolean(result))
    }

    /// Call a value (function or closure)
    fn call_value(&mut self, argc: usize) -> Result<(), RuntimeError> {
        let function = self.peek(argc)?.clone();

        match function {
            BytecodeValue::Function { arity, .. } => {
                if argc != arity as usize {
                    return Err(RuntimeError::ArityMismatch {
                        expected: arity as usize,
                        actual: argc,
                    });
                }

                // This would need access to a chunk registry
                // For now, return an error
                Err(RuntimeError::UndefinedFunction(
                    "chunk registry not implemented".to_string(),
                ))
            }

            BytecodeValue::NativeFunction {
                function, arity, ..
            } => {
                if argc != arity as usize {
                    return Err(RuntimeError::ArityMismatch {
                        expected: arity as usize,
                        actual: argc,
                    });
                }

                // Collect arguments
                let mut args = Vec::new();
                for _ in 0..argc {
                    args.push(self.pop()?);
                }
                args.reverse();

                // Pop the function
                self.pop()?;

                // Call the native function
                let result = function(&args)?;
                self.push(result)
            }

            _ => Err(RuntimeError::TypeError {
                expected: "function".to_string(),
                actual: function.type_name().to_string(),
            }),
        }
    }

    /// Close upvalues from the given stack position
    fn close_upvalues(&mut self, stack_position: usize) {
        // Implementation would close upvalues
        // This is a simplified version
        self.open_upvalues
            .retain(|upvalue| upvalue.stack_index < stack_position);
    }

    /// Get an element from an array
    fn get_array_element(
        &self,
        array: &BytecodeValue,
        index: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (array, index) {
            (BytecodeValue::Array(arr), BytecodeValue::Integer(i)) => {
                if *i < 0 || *i as usize >= arr.len() {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *i,
                        length: arr.len(),
                    });
                }
                Ok(arr[*i as usize].clone())
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "array indexing".to_string(),
                types: vec![array.type_name().to_string(), index.type_name().to_string()],
            }),
        }
    }

    /// Set an element in an array
    fn set_array_element(
        &self,
        array: &mut BytecodeValue,
        index: &BytecodeValue,
        value: BytecodeValue,
    ) -> Result<(), RuntimeError> {
        let array_type = array.type_name().to_string();
        let index_type = index.type_name().to_string();
        match (array, index) {
            (BytecodeValue::Array(arr), BytecodeValue::Integer(i)) => {
                if *i < 0 || *i as usize >= arr.len() {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *i,
                        length: arr.len(),
                    });
                }
                arr[*i as usize] = value;
                Ok(())
            }
            _ => Err(RuntimeError::InvalidOperation {
                op: "array assignment".to_string(),
                types: vec![array_type, index_type],
            }),
        }
    }

    /// Trace instruction execution for debugging
    fn trace_instruction(&self, instruction: &Instruction) {
        let frame = self.current_frame();
        trace!(
            "[{:04}] {} | Stack: [{}]",
            frame.ip - 1,
            instruction,
            self.stack
                .iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    // Arithmetic helper methods
    fn add_values(
        &self,
        a: &BytecodeValue,
        b: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (a, b) {
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
                op: "addition".to_string(),
                types: vec![a.type_name().to_string(), b.type_name().to_string()],
            }),
        }
    }

    fn subtract_values(
        &self,
        a: &BytecodeValue,
        b: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (a, b) {
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
                op: "subtraction".to_string(),
                types: vec![a.type_name().to_string(), b.type_name().to_string()],
            }),
        }
    }

    fn multiply_values(
        &self,
        a: &BytecodeValue,
        b: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (a, b) {
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
                op: "multiplication".to_string(),
                types: vec![a.type_name().to_string(), b.type_name().to_string()],
            }),
        }
    }

    fn divide_values(
        &self,
        a: &BytecodeValue,
        b: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (a, b) {
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
                op: "division".to_string(),
                types: vec![a.type_name().to_string(), b.type_name().to_string()],
            }),
        }
    }

    fn modulo_values(
        &self,
        a: &BytecodeValue,
        b: &BytecodeValue,
    ) -> Result<BytecodeValue, RuntimeError> {
        match (a, b) {
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
                types: vec![a.type_name().to_string(), b.type_name().to_string()],
            }),
        }
    }

    fn negate_value(&self, a: &BytecodeValue) -> Result<BytecodeValue, RuntimeError> {
        match a {
            BytecodeValue::Integer(a) => Ok(BytecodeValue::Integer(-a)),
            BytecodeValue::Float(a) => Ok(BytecodeValue::Float(-a)),
            _ => Err(RuntimeError::InvalidOperation {
                op: "negation".to_string(),
                types: vec![a.type_name().to_string()],
            }),
        }
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new()
    }
}
