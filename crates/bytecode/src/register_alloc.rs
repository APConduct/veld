//! Register allocator for Veld bytecode compiler
//!
//! This module implements a register allocator for the register-based VM.
//! It manages the allocation and deallocation of virtual registers within
//! function scopes, following a strategy similar to Lua 5.x.
//!
//! # Design
//!
//! - Registers are allocated sequentially from 0 upwards
//! - Function parameters occupy the first N registers
//! - Local variables get fixed register assignments
//! - Temporary values use dynamically allocated registers
//! - Registers are freed when values go out of scope
//! - Maximum 256 registers per function (u8 indexing)
//!
//! # Example
//!
//! ```text
//! fn add(a: i32, b: i32) -> i32
//!     let temp = a + b
//!     return temp * 2
//! end
//!
//! Register allocation:
//!   R0: parameter 'a'
//!   R1: parameter 'b'
//!   R2: local 'temp'
//!   R3: temporary for multiplication result
//! ```

use std::collections::{HashMap, HashSet};

/// Register index (0-255)
pub type Reg = u8;

/// Register allocator manages virtual register allocation
#[derive(Debug, Clone)]
pub struct RegisterAllocator {
    /// Next free register index
    next_free: Reg,

    /// Currently allocated registers
    allocated: HashSet<Reg>,

    /// Stack of scope information
    scopes: Vec<Scope>,

    /// Named registers (variables)
    variables: HashMap<String, Variable>,

    /// Maximum register used (high water mark)
    max_register: Reg,

    /// Stack of temporary register ranges
    temp_stack: Vec<Reg>,
}

/// Scope information
#[derive(Debug, Clone)]
pub struct Scope {
    /// Scope depth (0 = global, 1+ = nested)
    depth: usize,

    /// First register in this scope
    first_register: Reg,

    /// Registers allocated in this scope
    registers: Vec<Reg>,

    /// Variables declared in this scope (name -> shadowed variable if any)
    variables: Vec<(String, Option<Variable>)>,
}

impl Scope {
    /// Create a new scope
    pub fn new(depth: usize, first_register: Reg) -> Self {
        Self {
            depth,
            first_register,
            registers: Vec::new(),
            variables: Vec::new(),
        }
    }

    pub fn depth(&self) -> usize {
        self.depth
    }
}

/// Variable information
#[derive(Debug, Clone, PartialEq)]
struct Variable {
    /// Variable name
    name: String,

    /// Assigned register
    register: Reg,

    /// Scope depth where declared
    depth: usize,

    /// Is this variable mutable?
    is_mutable: bool,

    /// Has this variable been captured by a closure?
    is_captured: bool,
}

impl RegisterAllocator {
    /// Create a new register allocator
    pub fn new() -> Self {
        Self {
            next_free: 0,
            allocated: HashSet::new(),
            scopes: vec![],
            variables: HashMap::new(),
            max_register: 0,
            temp_stack: vec![],
        }
    }

    /// Create allocator with pre-allocated parameter registers
    pub fn with_params(param_count: u8) -> Self {
        let mut allocator = Self::new();
        allocator.next_free = param_count;
        allocator.max_register = param_count.saturating_sub(1);

        // Mark parameter registers as allocated
        for i in 0..param_count {
            allocator.allocated.insert(i);
        }

        allocator
    }

    /// Begin a new scope
    pub fn begin_scope(&mut self) {
        let depth = self.scopes.len();
        let scope = Scope {
            depth,
            first_register: self.next_free,
            registers: Vec::new(),
            variables: Vec::new(),
        };
        self.scopes.push(scope);
    }

    /// End the current scope and free its registers
    pub fn end_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // Free all registers allocated in this scope
            for reg in &scope.registers {
                self.allocated.remove(reg);
            }

            // Restore or remove variables declared in this scope
            for (var_name, shadowed) in &scope.variables {
                if let Some(old_var) = shadowed {
                    // Restore shadowed variable
                    self.variables.insert(var_name.clone(), old_var.clone());
                } else {
                    // Remove variable (not shadowing anything)
                    self.variables.remove(var_name);
                }
            }

            // Reset next_free to start of scope if possible
            if self.allocated.is_empty()
                || self.allocated.iter().max().copied() < Some(scope.first_register)
            {
                self.next_free = scope.first_register;
            }
        }
    }

    /// Allocate a single register
    pub fn allocate(&mut self) -> Result<Reg, String> {
        self.allocate_range(1)
    }

    /// Allocate a range of consecutive registers
    pub fn allocate_range(&mut self, count: u8) -> Result<Reg, String> {
        if count == 0 {
            return Err("Cannot allocate 0 registers".to_string());
        }

        // Check if we would exceed maximum
        let start = self.next_free;
        if start.checked_add(count).is_none() {
            return Err("Register overflow".to_string());
        }

        let end = start + count;
        if end == 0 {
            return Err("Too many registers (max 256)".to_string());
        }

        // Allocate the range
        for i in start..end {
            self.allocated.insert(i);
        }

        self.next_free = end;

        // Update high water mark
        if end > self.max_register + 1 {
            self.max_register = end - 1;
        }

        // Add to current scope if in one
        if let Some(scope) = self.scopes.last_mut() {
            for i in start..end {
                scope.registers.push(i);
            }
        }

        Ok(start)
    }

    /// Allocate a register for a named variable
    pub fn allocate_variable(&mut self, name: String, is_mutable: bool) -> Result<Reg, String> {
        // Check if variable already exists in current scope
        let current_depth = self.scopes.len();
        if let Some(var) = self.variables.get(&name) {
            if var.depth == current_depth {
                return Err(format!(
                    "Variable '{}' already declared in this scope",
                    name
                ));
            }
        }

        let register = self.allocate()?;

        // Save the old variable if we're shadowing
        let shadowed = self.variables.get(&name).cloned();

        let variable = Variable {
            name: name.clone(),
            register,
            depth: current_depth,
            is_mutable,
            is_captured: false,
        };

        self.variables.insert(name.clone(), variable);

        // Add to current scope with shadowed variable
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables.push((name, shadowed));
        }

        Ok(register)
    }

    /// Free a specific register
    pub fn free(&mut self, reg: Reg) {
        self.allocated.remove(&reg);

        // If this was the highest allocated register, we can lower next_free
        if reg + 1 == self.next_free {
            self.next_free = self.allocated.iter().max().map(|r| r + 1).unwrap_or(0);
        }
    }

    /// Free a range of registers
    pub fn free_range(&mut self, start: Reg, count: u8) {
        for i in 0..count {
            if let Some(reg) = start.checked_add(i) {
                self.free(reg);
            }
        }
    }

    /// Mark a register as permanently allocated (e.g., for parameters)
    pub fn mark_allocated(&mut self, reg: Reg) {
        self.allocated.insert(reg);
        if reg >= self.next_free {
            self.next_free = reg + 1;
        }
        if reg > self.max_register {
            self.max_register = reg;
        }
    }

    /// Get the register assigned to a variable
    pub fn get_variable(&self, name: &str) -> Option<Reg> {
        self.variables.get(name).map(|v| v.register)
    }

    /// Check if a variable exists
    pub fn has_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Check if a variable is mutable
    pub fn is_mutable(&self, name: &str) -> bool {
        self.variables
            .get(name)
            .map(|v| v.is_mutable)
            .unwrap_or(false)
    }

    /// Mark a variable as captured (by a closure)
    pub fn mark_captured(&mut self, name: &str) {
        if let Some(var) = self.variables.get_mut(name) {
            var.is_captured = true;
        }
    }

    /// Check if a variable is captured
    pub fn is_captured(&self, name: &str) -> bool {
        self.variables
            .get(name)
            .map(|v| v.is_captured)
            .unwrap_or(false)
    }

    /// Push a temporary register onto the temp stack
    pub fn push_temp(&mut self) -> Result<Reg, String> {
        let reg = self.allocate()?;
        self.temp_stack.push(reg);
        Ok(reg)
    }

    /// Pop a temporary register from the temp stack
    pub fn pop_temp(&mut self) -> Option<Reg> {
        if let Some(reg) = self.temp_stack.pop() {
            self.free(reg);
            Some(reg)
        } else {
            None
        }
    }

    /// Get the current temporary register (top of temp stack)
    pub fn current_temp(&self) -> Option<Reg> {
        self.temp_stack.last().copied()
    }

    /// Get the maximum register used
    pub fn max_register(&self) -> Reg {
        self.max_register
    }

    /// Get the total number of registers needed
    pub fn register_count(&self) -> u8 {
        self.max_register + 1
    }

    /// Get current scope depth
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Check if a register is currently allocated
    pub fn is_allocated(&self, reg: Reg) -> bool {
        self.allocated.contains(&reg)
    }

    /// Get all currently allocated registers
    pub fn allocated_registers(&self) -> Vec<Reg> {
        let mut regs: Vec<_> = self.allocated.iter().copied().collect();
        regs.sort();
        regs
    }

    /// Reset the allocator (for new function)
    pub fn reset(&mut self) {
        self.next_free = 0;
        self.allocated.clear();
        self.scopes.clear();
        self.variables.clear();
        self.max_register = 0;
        self.temp_stack.clear();
    }

    /// Create a snapshot of the current allocation state
    pub fn snapshot(&self) -> AllocatorSnapshot {
        AllocatorSnapshot {
            next_free: self.next_free,
            allocated: self.allocated.clone(),
            variables: self.variables.clone(),
            temp_stack_depth: self.temp_stack.len(),
        }
    }

    /// Restore from a snapshot
    pub fn restore(&mut self, snapshot: AllocatorSnapshot) {
        self.next_free = snapshot.next_free;
        self.allocated = snapshot.allocated;
        self.variables = snapshot.variables;

        // Update max_register based on restored state
        self.max_register = self.allocated.iter().max().copied().unwrap_or(0);

        // Restore temp stack depth
        while self.temp_stack.len() > snapshot.temp_stack_depth {
            self.temp_stack.pop();
        }
    }
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// Snapshot of allocator state (for backtracking)
#[derive(Debug, Clone)]
pub struct AllocatorSnapshot {
    next_free: Reg,
    allocated: HashSet<Reg>,
    variables: HashMap<String, Variable>,
    temp_stack_depth: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_allocation() {
        let mut allocator = RegisterAllocator::new();

        let r0 = allocator.allocate().unwrap();
        assert_eq!(r0, 0);

        let r1 = allocator.allocate().unwrap();
        assert_eq!(r1, 1);

        assert_eq!(allocator.register_count(), 2);
    }

    #[test]
    fn test_range_allocation() {
        let mut allocator = RegisterAllocator::new();

        let start = allocator.allocate_range(3).unwrap();
        assert_eq!(start, 0);
        assert_eq!(allocator.next_free, 3);
        assert_eq!(allocator.register_count(), 3);
    }

    #[test]
    fn test_with_params() {
        let mut allocator = RegisterAllocator::with_params(2);

        // First two registers should be for parameters
        assert_eq!(allocator.next_free, 2);

        // Next allocation should be R2
        let r2 = allocator.allocate().unwrap();
        assert_eq!(r2, 2);
    }

    #[test]
    fn test_variable_allocation() {
        let mut allocator = RegisterAllocator::new();
        allocator.begin_scope();

        let r0 = allocator.allocate_variable("x".to_string(), false).unwrap();
        assert_eq!(r0, 0);

        assert_eq!(allocator.get_variable("x"), Some(0));
        assert!(allocator.has_variable("x"));
    }

    #[test]
    fn test_scope_management() {
        let mut allocator = RegisterAllocator::new();

        allocator.begin_scope();
        let r0 = allocator.allocate_variable("x".to_string(), false).unwrap();
        assert_eq!(r0, 0);

        allocator.begin_scope();
        let r1 = allocator.allocate_variable("y".to_string(), false).unwrap();
        assert_eq!(r1, 1);

        // Both variables visible
        assert!(allocator.has_variable("x"));
        assert!(allocator.has_variable("y"));

        allocator.end_scope();

        // y is gone, x remains
        assert!(allocator.has_variable("x"));
        assert!(!allocator.has_variable("y"));

        allocator.end_scope();

        // Both gone
        assert!(!allocator.has_variable("x"));
        assert!(!allocator.has_variable("y"));
    }

    #[test]
    fn test_shadowing() {
        let mut allocator = RegisterAllocator::new();

        allocator.begin_scope();
        let r0 = allocator.allocate_variable("x".to_string(), false).unwrap();
        assert_eq!(r0, 0);

        allocator.begin_scope();
        // Shadow x in inner scope
        let r1 = allocator.allocate_variable("x".to_string(), false).unwrap();
        assert_eq!(r1, 1);
        assert_eq!(allocator.get_variable("x"), Some(1)); // Inner x

        allocator.end_scope();

        assert_eq!(allocator.get_variable("x"), Some(0)); // Outer x restored
    }

    #[test]
    fn test_free_register() {
        let mut allocator = RegisterAllocator::new();

        let _r0 = allocator.allocate().unwrap();
        let r1 = allocator.allocate().unwrap();

        assert_eq!(allocator.next_free, 2);

        allocator.free(r1);
        assert_eq!(allocator.next_free, 1);

        // Can reuse freed register
        let r1_again = allocator.allocate().unwrap();
        assert_eq!(r1_again, 1);
    }

    #[test]
    fn test_temp_stack() {
        let mut allocator = RegisterAllocator::new();

        let t0 = allocator.push_temp().unwrap();
        assert_eq!(t0, 0);

        let t1 = allocator.push_temp().unwrap();
        assert_eq!(t1, 1);

        assert_eq!(allocator.current_temp(), Some(1));

        let popped = allocator.pop_temp();
        assert_eq!(popped, Some(1));
        assert_eq!(allocator.current_temp(), Some(0));
    }

    #[test]
    fn test_snapshot_restore() {
        let mut allocator = RegisterAllocator::new();

        allocator.allocate().unwrap();
        let snapshot = allocator.snapshot();

        allocator.allocate().unwrap();
        allocator.allocate().unwrap();

        assert_eq!(allocator.register_count(), 3);

        allocator.restore(snapshot);

        assert_eq!(allocator.register_count(), 1);
    }

    #[test]
    fn test_captured_variables() {
        let mut allocator = RegisterAllocator::new();
        allocator.begin_scope();

        allocator.allocate_variable("x".to_string(), false).unwrap();

        assert!(!allocator.is_captured("x"));

        allocator.mark_captured("x");

        assert!(allocator.is_captured("x"));
    }

    #[test]
    fn test_max_register_tracking() {
        let mut allocator = RegisterAllocator::new();

        allocator.allocate().unwrap(); // R0
        allocator.allocate().unwrap(); // R1
        assert_eq!(allocator.max_register(), 1);

        let r5 = 5;
        allocator.mark_allocated(r5);
        assert_eq!(allocator.max_register(), 5);
    }
}
