//! Register-based bytecode compiler for Veld
//!
//! This compiler generates register-based bytecode (bytecode_v2) using the
//! RegisterAllocator to manage register allocation for local variables and
//! temporary values.
//!
//! # Architecture
//!
//! - Uses RegisterAllocator for register management
//! - Emits Instruction (register-based, 3-address code)
//! - Each expression compiles to a target register
//! - Variables are assigned fixed registers
//! - Temporaries use dynamically allocated registers
//!
//! # Example
//!
//! ```text
//! let a = 10;
//! let b = 20;
//! let c = a + b;
//!
//! Compiled to:
//!   R0 <- LoadConst 0  (10)
//!   R1 <- LoadConst 1  (20)
//!   R2 <- Add R0, R1
//! ```

use std::collections::HashMap;
use tracing::{debug, trace, warn};

use veld_common::ast::{
    AST, Argument, BinaryOperator, Expr, Literal, MatchArm, MatchPattern, Statement,
    TypeAnnotation, UnaryOperator, VarKind,
};
use veld_common::bytecode_v2::{
    Chunk, ChunkBuilder, Constant, FunctionProto, UpvalueInfo as BytecodeUpvalueInfo,
};
use veld_error::{Result, VeldError};

use crate::register_alloc::{Reg, RegisterAllocator};

/// Register-based bytecode compiler
pub struct RegisterCompiler {
    /// Current chunk being compiled
    chunk: ChunkBuilder,

    /// Register allocator
    allocator: RegisterAllocator,

    /// Current scope depth
    scope_depth: usize,

    /// Variable name to register mapping
    variables: HashMap<String, VarInfo>,

    /// Scope stack for tracking variable shadowing
    scope_stack: Vec<ScopeInfo>,

    /// Loop context stack (for break/continue)
    loop_stack: Vec<LoopContext>,

    /// Current line number for debugging
    current_line: u32,

    /// Compilation options
    options: CompilerOptions,

    /// Upvalue tracking for closures
    upvalues: Vec<CompilerUpvalueInfo>,

    /// Parent compiler (for nested function compilation)
    parent: Option<Box<RegisterCompiler>>,

    /// Function depth (0 = top level, 1 = first function, etc.)
    function_depth: usize,
}

/// Variable information
#[allow(dead_code)] // TODO: Remove this once all fields are used
#[derive(Debug, Clone)]
struct VarInfo {
    register: Reg,
    is_mutable: bool,
    depth: usize,
    /// If this variable is captured by a closure
    is_captured: bool,
    /// If this variable is actually an upvalue (captured from parent scope)
    is_upvalue: bool,
    /// If this variable holds a type value (for enums/structs)
    is_type: bool,
    /// If this variable should be accessed via LoadGlobal (for top-level recursive functions)
    is_global_ref: bool,
}

/// Upvalue information for closures (compiler-side tracking)
#[derive(Debug, Clone)]
#[allow(dead_code)] // TODO: Remove this once all fields are used
struct CompilerUpvalueInfo {
    /// The name of the captured variable
    name: String,
    /// Register index in the enclosing function
    register: Reg,
    /// Is this upvalue itself from an enclosing function's upvalue?
    is_upvalue: bool,
    /// Index in the parent's upvalue list (if is_upvalue = true)
    parent_upvalue_index: Option<usize>,
    /// Is the captured variable mutable?
    is_mutable: bool,
}

/// Scope tracking for variable shadowing
#[derive(Debug, Clone)]
#[allow(dead_code)] // TODO: Remove this once all fields are used
struct ScopeInfo {
    depth: usize,
    /// Variables declared in this scope with their shadowed values
    variables: Vec<(String, Option<VarInfo>)>,
}

/// Loop context for break/continue
#[derive(Debug, Clone)]
#[allow(dead_code)] // TODO: Remove this once all fields are used
struct LoopContext {
    start_index: usize,
    break_jumps: Vec<usize>, // indices of Jump instructions to patch
    depth: usize,
}

/// Compiler options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub trace_compilation: bool,
    pub optimize: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            trace_compilation: false,
            optimize: false,
        }
    }
}

/// Result of compiling an expression: which register holds the result
#[derive(Debug, Clone, Copy)]
struct ExprResult {
    register: Reg,
    /// Is this a temporary register that can be freed?
    is_temp: bool,
}

impl ExprResult {
    fn new(register: Reg, is_temp: bool) -> Self {
        Self { register, is_temp }
    }

    fn temp(register: Reg) -> Self {
        Self::new(register, true)
    }

    fn var(register: Reg) -> Self {
        Self::new(register, false)
    }
}

impl RegisterCompiler {
    /// Create a new register compiler
    pub fn new() -> Self {
        Self::with_options(CompilerOptions::default())
    }

    /// Create a new compiler with options
    pub fn with_options(options: CompilerOptions) -> Self {
        Self {
            chunk: ChunkBuilder::new(),
            allocator: RegisterAllocator::new(),
            scope_depth: 0,
            variables: HashMap::new(),
            scope_stack: Vec::new(),
            loop_stack: Vec::new(),
            current_line: 1,
            options,
            upvalues: Vec::new(),
            parent: None,
            function_depth: 0,
        }
    }

    /// Compile an AST to register-based bytecode
    pub fn compile(&mut self, ast: &AST) -> Result<Chunk> {
        if self.options.trace_compilation {
            debug!("Starting register-based compilation");
        }

        // Track if the last statement is an expression that should be returned
        let last_stmt_index = ast.statements.len().saturating_sub(1);
        let mut last_expr_reg: Option<Reg> = None;

        // Compile all statements in the AST
        for (i, statement) in ast.statements.iter().enumerate() {
            if i == last_stmt_index {
                // Check if last statement is an expression statement
                if let Statement::ExprStatement(expr) = statement {
                    // Compile the expression and keep its result
                    let result = self.compile_expr_to_reg(expr)?;
                    last_expr_reg = Some(result.register);
                    // Don't free the temp - we need it for the final value
                } else {
                    self.compile_statement(statement)?;
                }
            } else {
                self.compile_statement(statement)?;
            }
        }

        // If we have a last expression value, place it in register 0 before Halt
        // so the VM can return it
        if let Some(expr_reg) = last_expr_reg {
            if expr_reg != 0 {
                self.chunk.move_reg(0, expr_reg);
            }
        }

        // Emit Halt at the end
        self.chunk.halt();

        // Set register count before building (important for VM frame initialization)
        let max_reg = self.allocator.max_register();
        let register_count = if max_reg == 0 && self.allocator.max_register() == 0 {
            // Empty program, need at least 1 register
            1
        } else {
            max_reg + 1
        };
        self.chunk.register_count(register_count);

        // Build the final chunk
        let chunk = std::mem::replace(&mut self.chunk, ChunkBuilder::new());
        Ok(chunk.build())
    }

    /// Compile a statement
    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        if self.options.trace_compilation {
            trace!("Compiling statement: {:?}", statement);
        }

        match statement {
            Statement::VariableDeclaration {
                pattern,
                value,
                var_kind,
                type_annotation: _,
                is_public: _,
            } => {
                self.compile_pattern_var_declaration(pattern, value, var_kind)?;
            }

            Statement::FunctionDeclaration {
                name,
                params,
                return_type: _,
                body,
                is_proc: _,
                is_public: _,
                generic_params: _,
            } => {
                self.compile_function_declaration(name, params, body)?;
            }

            Statement::ExprStatement(expr) => {
                // Compile expression and discard result
                let result = self.compile_expr_to_reg(expr)?;
                if result.is_temp {
                    self.free_temp(result.register);
                }
            }

            Statement::Assignment { name, value } => {
                self.compile_simple_assignment(name, value)?;
            }

            Statement::PropertyAssignment {
                target,
                operator,
                value,
            } => {
                self.compile_property_assignment(target, operator.as_ref(), value)?;
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_if(condition, then_branch, else_branch.as_deref())?;
            }

            Statement::While { condition, body } => {
                self.compile_while(condition, body)?;
            }

            Statement::For {
                iterator,
                iterable,
                body,
            } => {
                self.compile_for(iterator, iterable, body)?;
            }

            Statement::Return(value) => {
                self.compile_return(value.as_ref())?;
            }

            Statement::Break => {
                self.compile_break()?;
            }

            Statement::Continue => {
                self.compile_continue()?;
            }

            Statement::Match { value, arms } => {
                self.compile_match(value, arms)?;
            }

            Statement::BlockScope { body } => {
                self.begin_scope();
                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.end_scope();
            }

            Statement::StructDeclaration {
                name,
                fields,
                generic_params: _,
                methods: _,
                is_public: _,
            } => {
                self.compile_struct_declaration(name, fields)?;
            }

            Statement::EnumDeclaration {
                name,
                variants,
                generic_params: _,
                is_public: _,
            } => {
                self.compile_enum_declaration(name, variants)?;
            }

            _ => {
                return Err(VeldError::CompileError {
                    message: format!("Statement not yet implemented: {:?}", statement),
                    line: Some(self.current_line as usize),
                    column: None,
                });
            }
        }

        Ok(())
    }

    /// Compile a variable declaration
    fn compile_pattern_var_declaration(
        &mut self,
        pattern: &veld_common::ast::Pattern,
        value: &Expr,
        var_kind: &VarKind,
    ) -> Result<()> {
        // Evaluate the RHS expression first
        let value_result = self.compile_expr_to_reg(value)?;

        // Now bind the pattern
        self.compile_pattern_binding(pattern, value_result.register, var_kind)?;

        // Free the value register if it was temporary
        if value_result.is_temp {
            self.free_temp(value_result.register);
        }

        Ok(())
    }

    /// Compile pattern binding - recursively bind pattern elements to values
    fn compile_pattern_binding(
        &mut self,
        pattern: &veld_common::ast::Pattern,
        value_reg: u8,
        var_kind: &VarKind,
    ) -> Result<()> {
        use veld_common::ast::Pattern;

        match pattern {
            Pattern::Identifier(name) => {
                // Simple case: bind identifier to value
                self.compile_identifier_binding(name, value_reg, var_kind)?;
                Ok(())
            }
            Pattern::Wildcard => {
                // Wildcard doesn't bind anything, just discard
                Ok(())
            }
            Pattern::TuplePattern(patterns) => {
                // For each element in the tuple pattern, extract the corresponding element
                // from the value and recursively bind
                for (i, sub_pattern) in patterns.iter().enumerate() {
                    // Extract the i-th element of the tuple
                    let element_reg = self.allocate_temp()?;

                    // Load index as a constant
                    let idx_const = self.chunk.add_constant(Constant::Integer(i as i64));
                    let idx_reg = self.allocate_temp()?;
                    self.chunk.load_const(idx_reg, idx_const);

                    // Get tuple element: element_reg = value_reg[idx_reg]
                    self.chunk.get_index(element_reg, value_reg, idx_reg);

                    // Free the index register
                    self.free_temp(idx_reg);

                    // Recursively bind the sub-pattern
                    self.compile_pattern_binding(sub_pattern, element_reg, var_kind)?;

                    // Free the element register
                    self.free_temp(element_reg);
                }
                Ok(())
            }
            Pattern::Literal(_) | Pattern::EnumPattern { .. } | Pattern::StructPattern { .. } => {
                // These patterns are not supported in variable declarations
                Err(VeldError::CompileError {
                    message: format!(
                        "Pattern {:?} is not supported in variable declarations",
                        pattern
                    ),
                    line: Some(self.current_line as usize),
                    column: None,
                })
            }
        }
    }

    fn compile_identifier_binding(
        &mut self,
        name: &str,
        value_reg: u8,
        var_kind: &VarKind,
    ) -> Result<()> {
        let is_mutable = matches!(var_kind, VarKind::Var | VarKind::LetMut);

        // Allocate register for the variable
        let var_reg = self
            .allocator
            .allocate_variable(name.to_string(), is_mutable)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Move value from source register to variable's register
        if value_reg != var_reg {
            self.chunk.move_reg(var_reg, value_reg);
        }

        // Track variable with shadowing support
        let var_info = VarInfo {
            register: var_reg,
            is_mutable,
            depth: self.scope_depth,
            is_captured: false,
            is_upvalue: false,
            is_type: false,
            is_global_ref: false,
        };

        // Save the old variable if we're shadowing
        let shadowed = self.variables.insert(name.to_string(), var_info);

        // Add to current scope with shadowed variable
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.variables.push((name.to_string(), shadowed));
        }

        Ok(())
    }

    /// Compile a simple assignment (variable = value)
    fn compile_simple_assignment(&mut self, name: &str, value: &Expr) -> Result<()> {
        // Check if it's a local variable
        if let Some(var_info) = self.variables.get(name).cloned() {
            if !var_info.is_mutable {
                return Err(VeldError::CompileError {
                    message: format!("Cannot assign to immutable variable: {}", name),
                    line: Some(self.current_line as usize),
                    column: None,
                });
            }

            let result = self.compile_expr_to_reg(value)?;

            // Move to variable's register
            if result.register != var_info.register {
                self.chunk.move_reg(var_info.register, result.register);
            }

            if result.is_temp {
                self.free_temp(result.register);
            }

            return Ok(());
        }

        // Check if it's an upvalue
        if let Some(upvalue_idx) = self.find_upvalue(name) {
            let upvalue_info = &self.upvalues[upvalue_idx];

            if !upvalue_info.is_mutable {
                return Err(VeldError::CompileError {
                    message: format!("Cannot assign to immutable upvalue: {}", name),
                    line: Some(self.current_line as usize),
                    column: None,
                });
            }

            let result = self.compile_expr_to_reg(value)?;

            // Set the upvalue
            self.chunk.set_upvalue(upvalue_idx as u8, result.register);

            if result.is_temp {
                self.free_temp(result.register);
            }

            return Ok(());
        }

        // Variable not found
        Err(VeldError::CompileError {
            message: format!("Undefined variable: {}", name),
            line: Some(self.current_line as usize),
            column: None,
        })
    }

    /// Compile a property assignment (obj.field = value or obj[idx] = value)
    fn compile_property_assignment(
        &mut self,
        target: &Expr,
        operator: Option<&BinaryOperator>,
        value: &Expr,
    ) -> Result<()> {
        match target {
            Expr::Identifier(name) => {
                // Simple variable assignment via PropertyAssignment
                // This happens when parser generates PropertyAssignment for simple assignments
                if let Some(op) = operator {
                    // Compound assignment: x += value
                    // We need to: read current value, apply operator, store result back

                    // Check if it's a local variable or upvalue
                    if let Some(var_info) = self.variables.get(name).cloned() {
                        // It's a local variable
                        if !var_info.is_mutable {
                            return Err(VeldError::CompileError {
                                message: format!("Cannot assign to immutable variable: {}", name),
                                line: Some(self.current_line as usize),
                                column: None,
                            });
                        }

                        // 1. Current value is in var_info.register
                        // 2. Compile the value expression
                        let val_result = self.compile_expr_to_reg(value)?;

                        // 3. Allocate temp for result
                        let result_reg = self.allocate_temp()?;

                        // 4. Perform the operation
                        match op {
                            BinaryOperator::Add => {
                                self.chunk
                                    .add(result_reg, var_info.register, val_result.register);
                            }
                            BinaryOperator::Subtract => {
                                self.chunk
                                    .sub(result_reg, var_info.register, val_result.register);
                            }
                            BinaryOperator::Multiply => {
                                self.chunk
                                    .mul(result_reg, var_info.register, val_result.register);
                            }
                            BinaryOperator::Divide => {
                                self.chunk
                                    .div(result_reg, var_info.register, val_result.register);
                            }
                            _ => {
                                return Err(VeldError::CompileError {
                                    message: format!(
                                        "Unsupported compound assignment operator: {:?}",
                                        op
                                    ),
                                    line: Some(self.current_line as usize),
                                    column: None,
                                });
                            }
                        }

                        // 5. Move result back to variable's register
                        if result_reg != var_info.register {
                            self.chunk.move_reg(var_info.register, result_reg);
                        }

                        // Free temps
                        if val_result.is_temp {
                            self.free_temp(val_result.register);
                        }
                        self.free_temp(result_reg);

                        return Ok(());
                    } else if let Some(upvalue_idx) = self.find_upvalue(name) {
                        // It's an upvalue
                        let upvalue_info = &self.upvalues[upvalue_idx];

                        if !upvalue_info.is_mutable {
                            return Err(VeldError::CompileError {
                                message: format!("Cannot assign to immutable upvalue: {}", name),
                                line: Some(self.current_line as usize),
                                column: None,
                            });
                        }

                        // 1. Get current value from upvalue
                        let current_reg = self.allocate_temp()?;
                        self.chunk.get_upvalue(current_reg, upvalue_idx as u8);

                        // 2. Compile the value expression
                        let val_result = self.compile_expr_to_reg(value)?;

                        // 3. Allocate temp for result
                        let result_reg = self.allocate_temp()?;

                        // 4. Perform the operation
                        match op {
                            BinaryOperator::Add => {
                                self.chunk.add(result_reg, current_reg, val_result.register);
                            }
                            BinaryOperator::Subtract => {
                                self.chunk.sub(result_reg, current_reg, val_result.register);
                            }
                            BinaryOperator::Multiply => {
                                self.chunk.mul(result_reg, current_reg, val_result.register);
                            }
                            BinaryOperator::Divide => {
                                self.chunk.div(result_reg, current_reg, val_result.register);
                            }
                            _ => {
                                return Err(VeldError::CompileError {
                                    message: format!(
                                        "Unsupported compound assignment operator: {:?}",
                                        op
                                    ),
                                    line: Some(self.current_line as usize),
                                    column: None,
                                });
                            }
                        }

                        // 5. Set the upvalue to the result
                        self.chunk.set_upvalue(upvalue_idx as u8, result_reg);

                        // Free temps
                        self.free_temp(current_reg);
                        if val_result.is_temp {
                            self.free_temp(val_result.register);
                        }
                        self.free_temp(result_reg);

                        return Ok(());
                    } else {
                        // Variable not found
                        return Err(VeldError::CompileError {
                            message: format!("Undefined variable: {}", name),
                            line: Some(self.current_line as usize),
                            column: None,
                        });
                    }
                } else {
                    // Regular assignment
                    return self.compile_simple_assignment(name, value);
                }
            }

            Expr::IndexAccess { object, index } => {
                // Array/tuple index assignment: obj[idx] = value
                let obj_result = self.compile_expr_to_reg(object)?;
                let idx_result = self.compile_expr_to_reg(index)?;

                if let Some(op) = operator {
                    // Compound assignment: obj[idx] += value
                    // 1. Get current value: temp = obj[idx]
                    let temp_reg = self.allocate_temp()?;
                    self.chunk
                        .get_index(temp_reg, obj_result.register, idx_result.register);

                    // 2. Compile the value expression
                    let val_result = self.compile_expr_to_reg(value)?;

                    // 3. Perform the operation: temp = temp op value
                    let result_reg = self.allocate_temp()?;
                    match op {
                        BinaryOperator::Add => {
                            self.chunk.add(result_reg, temp_reg, val_result.register);
                        }
                        BinaryOperator::Subtract => {
                            self.chunk.sub(result_reg, temp_reg, val_result.register);
                        }
                        BinaryOperator::Multiply => {
                            self.chunk.mul(result_reg, temp_reg, val_result.register);
                        }
                        BinaryOperator::Divide => {
                            self.chunk.div(result_reg, temp_reg, val_result.register);
                        }
                        _ => {
                            return Err(VeldError::CompileError {
                                message: format!(
                                    "Unsupported compound assignment operator: {:?}",
                                    op
                                ),
                                line: Some(self.current_line as usize),
                                column: None,
                            });
                        }
                    }

                    // 4. Store result back: obj[idx] = result
                    self.chunk
                        .set_index(obj_result.register, idx_result.register, result_reg);

                    // Free temps
                    self.free_temp(temp_reg);
                    self.free_temp(result_reg);
                    if val_result.is_temp {
                        self.free_temp(val_result.register);
                    }
                } else {
                    // Regular assignment: obj[idx] = value
                    let val_result = self.compile_expr_to_reg(value)?;
                    self.chunk.set_index(
                        obj_result.register,
                        idx_result.register,
                        val_result.register,
                    );

                    if val_result.is_temp {
                        self.free_temp(val_result.register);
                    }
                }

                if obj_result.is_temp {
                    self.free_temp(obj_result.register);
                }
                if idx_result.is_temp {
                    self.free_temp(idx_result.register);
                }
            }

            Expr::PropertyAccess { object, property } => {
                // Struct field assignment: obj.field = value
                let obj_result = self.compile_expr_to_reg(object)?;
                let field_const = self.chunk.add_constant(Constant::String(property.clone()));

                if let Some(op) = operator {
                    // Compound assignment: obj.field += value
                    // 1. Get current value: temp = obj.field
                    let temp_reg = self.allocate_temp()?;
                    self.chunk
                        .get_field(temp_reg, obj_result.register, field_const as u8);

                    // 2. Compile the value expression
                    let val_result = self.compile_expr_to_reg(value)?;

                    // 3. Perform the operation: temp = temp op value
                    let result_reg = self.allocate_temp()?;
                    match op {
                        BinaryOperator::Add => {
                            self.chunk.add(result_reg, temp_reg, val_result.register);
                        }
                        BinaryOperator::Subtract => {
                            self.chunk.sub(result_reg, temp_reg, val_result.register);
                        }
                        BinaryOperator::Multiply => {
                            self.chunk.mul(result_reg, temp_reg, val_result.register);
                        }
                        BinaryOperator::Divide => {
                            self.chunk.div(result_reg, temp_reg, val_result.register);
                        }
                        _ => {
                            return Err(VeldError::CompileError {
                                message: format!(
                                    "Unsupported compound assignment operator: {:?}",
                                    op
                                ),
                                line: Some(self.current_line as usize),
                                column: None,
                            });
                        }
                    }

                    // 4. Store result back: obj.field = result
                    self.chunk
                        .set_field(obj_result.register, field_const as u8, result_reg);

                    // Free temps
                    self.free_temp(temp_reg);
                    self.free_temp(result_reg);
                    if val_result.is_temp {
                        self.free_temp(val_result.register);
                    }
                } else {
                    // Regular assignment: obj.field = value
                    let val_result = self.compile_expr_to_reg(value)?;
                    self.chunk.set_field(
                        obj_result.register,
                        field_const as u8,
                        val_result.register,
                    );

                    if val_result.is_temp {
                        self.free_temp(val_result.register);
                    }
                }

                if obj_result.is_temp {
                    self.free_temp(obj_result.register);
                }
            }

            _ => {
                return Err(VeldError::CompileError {
                    message: "Invalid assignment target".to_string(),
                    line: Some(self.current_line as usize),
                    column: None,
                });
            }
        }

        Ok(())
    }

    /// Compile an expression to a register and return which register
    fn compile_expr_to_reg(&mut self, expr: &Expr) -> Result<ExprResult> {
        match expr {
            Expr::Literal(lit) => self.compile_literal(lit),

            Expr::Identifier(name) => self.compile_identifier(name),

            Expr::BinaryOp {
                left,
                operator,
                right,
            } => self.compile_binary_op(left, operator, right),

            Expr::UnaryOp { operator, operand } => self.compile_unary_op(operator, operand),

            Expr::Call { callee, arguments } => self.compile_call(callee, arguments),

            Expr::IndexAccess { object, index } => self.compile_index(object, index),

            Expr::PropertyAccess { object, property } => {
                self.compile_property_access(object, property)
            }

            Expr::ArrayLiteral(elements) => self.compile_array(elements),

            Expr::TupleLiteral(elements) => self.compile_tuple(elements),

            Expr::StructCreate {
                struct_name,
                fields,
            } => self.compile_struct(struct_name, fields),

            Expr::EnumVariant {
                enum_name,
                variant_name,
                fields,
                type_args: _,
            } => self.compile_enum_variant(enum_name, variant_name, fields),

            Expr::Lambda {
                params,
                body,
                return_type,
                generic_params: _,
            } => self.compile_lambda(params, body, return_type),

            Expr::UnitLiteral => {
                let reg = self.allocate_temp()?;
                let nil_const = self.chunk.add_constant(Constant::Nil);
                self.chunk.load_const(reg, nil_const);
                Ok(ExprResult::temp(reg))
            }

            Expr::BlockExpression {
                statements,
                final_expr,
            } => {
                self.begin_scope();

                // Compile all statements in the block
                for stmt in statements {
                    self.compile_statement(stmt)?;
                }

                // Compile the final expression or return nil
                let result = if let Some(expr) = final_expr {
                    self.compile_expr_to_reg(expr)?
                } else {
                    // No final expression, return nil
                    let reg = self.allocate_temp()?;
                    let nil_const = self.chunk.add_constant(Constant::Nil);
                    self.chunk.load_const(reg, nil_const);
                    ExprResult::temp(reg)
                };

                self.end_scope();
                Ok(result)
            }

            Expr::LetIn {
                name,
                var_kind,
                type_annotation: _,
                value,
                body,
            } => {
                self.begin_scope();

                // Compile the value expression
                let value_result = self.compile_expr_to_reg(value)?;

                // Declare the variable in the current scope
                let var_reg = if value_result.is_temp {
                    // Reuse the temp register
                    value_result.register
                } else {
                    // Need to copy to a new register
                    let dest = self.allocate_temp()?;
                    self.chunk.move_reg(dest, value_result.register);
                    dest
                };

                // Register the variable
                let is_mutable = *var_kind == VarKind::Var || *var_kind == VarKind::LetMut;
                self.variables.insert(
                    name.clone(),
                    VarInfo {
                        register: var_reg,
                        is_mutable,
                        depth: self.scope_depth,
                        is_captured: false,
                        is_upvalue: false,
                        is_type: false,
                        is_global_ref: false,
                    },
                );

                // Compile the body expression with the variable in scope
                let result = self.compile_expr_to_reg(body)?;

                self.end_scope();
                Ok(result)
            }

            Expr::Match { value, arms } => self.compile_match_expression(value, arms),

            _ => Err(VeldError::CompileError {
                message: format!("Expression not yet implemented: {:?}", expr),
                line: Some(self.current_line as usize),
                column: None,
            }),
        }
    }

    /// Compile a literal value
    fn compile_literal(&mut self, lit: &Literal) -> Result<ExprResult> {
        let reg = self.allocate_temp()?;

        let constant = match lit {
            Literal::Integer(n) => Constant::Integer(*n),
            Literal::Float(f) => Constant::Float(*f),
            Literal::String(s) => Constant::String(s.clone()),
            Literal::Boolean(b) => Constant::Boolean(*b),
            Literal::Char(c) => Constant::String(c.to_string()), // Convert char to string
            Literal::Unit => Constant::Nil,
        };

        let const_idx = self.chunk.add_constant(constant);
        self.chunk.load_const(reg, const_idx);

        Ok(ExprResult::temp(reg))
    }

    /// Compile an identifier (variable access)
    fn compile_identifier(&mut self, name: &str) -> Result<ExprResult> {
        // First check if it's a local variable or upvalue
        if let Some(var_info) = self.variables.get(name) {
            if var_info.is_global_ref {
                // This is a recursive self-reference to a top-level function
                // Use LoadGlobal instead of upvalue to avoid circular dependency
                let dest = self.allocate_temp()?;
                let name_const = self.chunk.add_constant(Constant::String(name.to_string()));
                self.chunk.load_global(dest, name_const);
                return Ok(ExprResult::temp(dest));
            } else if var_info.is_upvalue {
                // This is an upvalue, need to use GetUpvalue instruction
                if let Some(upvalue_idx) = self.find_upvalue(name) {
                    let dest = self.allocate_temp()?;
                    self.chunk.get_upvalue(dest, upvalue_idx as u8);
                    return Ok(ExprResult::temp(dest));
                }
            } else {
                // Regular local variable, return its register
                return Ok(ExprResult::var(var_info.register));
            }
        }

        // Check if it's an upvalue (captured from parent scope)
        if let Some(upvalue_idx) = self.find_upvalue(name) {
            // Allocate a temp register for the upvalue
            let dest = self.allocate_temp()?;
            self.chunk.get_upvalue(dest, upvalue_idx as u8);
            return Ok(ExprResult::temp(dest));
        }

        // Variable not found
        Err(VeldError::CompileError {
            message: format!("Undefined variable: {}", name),
            line: Some(self.current_line as usize),
            column: None,
        })
    }

    /// Compile a binary operation
    fn compile_binary_op(
        &mut self,
        left: &Expr,
        op: &BinaryOperator,
        right: &Expr,
    ) -> Result<ExprResult> {
        let left_result = self.compile_expr_to_reg(left)?;
        let right_result = self.compile_expr_to_reg(right)?;

        let dest = self.allocate_temp()?;

        match op {
            BinaryOperator::Add => {
                self.chunk
                    .add(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Subtract => {
                self.chunk
                    .sub(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Multiply => {
                self.chunk
                    .mul(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Divide => {
                self.chunk
                    .div(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Modulo => {
                self.chunk
                    .mod_op(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Exponent => {
                self.chunk
                    .pow(dest, left_result.register, right_result.register);
            }

            // Comparisons
            BinaryOperator::EqualEqual => {
                self.chunk
                    .eq(dest, left_result.register, right_result.register);
            }
            BinaryOperator::NotEqual => {
                self.chunk
                    .neq(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Less => {
                self.chunk
                    .lt(dest, left_result.register, right_result.register);
            }
            BinaryOperator::LessEq => {
                self.chunk
                    .le(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Greater => {
                self.chunk
                    .gt(dest, left_result.register, right_result.register);
            }
            BinaryOperator::GreaterEq => {
                self.chunk
                    .ge(dest, left_result.register, right_result.register);
            }

            // Logical
            BinaryOperator::And => {
                self.chunk
                    .and(dest, left_result.register, right_result.register);
            }
            BinaryOperator::Or => {
                self.chunk
                    .or(dest, left_result.register, right_result.register);
            }

            BinaryOperator::Pipe => {
                warn!("Pipe operator not yet implemented");
                return Err(VeldError::CompileError {
                    message: "Pipe operator not yet implemented".to_string(),
                    line: Some(self.current_line as usize),
                    column: None,
                });
            }
        }

        // Free temporary registers
        if left_result.is_temp {
            self.free_temp(left_result.register);
        }
        if right_result.is_temp {
            self.free_temp(right_result.register);
        }

        Ok(ExprResult::temp(dest))
    }

    /// Compile a unary operation
    fn compile_unary_op(&mut self, op: &UnaryOperator, operand: &Expr) -> Result<ExprResult> {
        let operand_result = self.compile_expr_to_reg(operand)?;
        let dest = self.allocate_temp()?;

        match op {
            UnaryOperator::Negate => {
                self.chunk.neg(dest, operand_result.register);
            }
            UnaryOperator::Not => {
                self.chunk.not(dest, operand_result.register);
            }
        }

        if operand_result.is_temp {
            self.free_temp(operand_result.register);
        }

        Ok(ExprResult::temp(dest))
    }

    /// Compile a function call
    fn compile_call(&mut self, callee: &Expr, arguments: &[Argument]) -> Result<ExprResult> {
        // Compile callee expression
        let func_result = self.compile_expr_to_reg(callee)?;

        // If the function is not a temp (e.g., it's a variable), we need to move it
        // to a temp to preserve the original. Otherwise the return value will overwrite it.
        let call_func_reg = if func_result.is_temp {
            func_result.register
        } else {
            let temp_reg = self.allocate_temp()?;
            self.chunk.move_reg(temp_reg, func_result.register);
            temp_reg
        };

        // Compile arguments and move them to consecutive registers after func_reg
        // VM expects arguments at func_reg+1, func_reg+2, etc.
        let mut arg_regs = Vec::new();
        for (i, arg) in arguments.iter().enumerate() {
            let arg_expr = match arg {
                Argument::Positional(expr) => expr,
                Argument::Named { value, .. } => value,
            };
            let arg_result = self.compile_expr_to_reg(arg_expr)?;

            // Move argument to the expected position (func_reg + 1 + i)
            let target_reg = call_func_reg + 1 + i as u8;
            if arg_result.register != target_reg {
                self.chunk.move_reg(target_reg, arg_result.register);
            }

            // Free the temp if it's not the target register
            if arg_result.is_temp && arg_result.register != target_reg {
                self.free_temp(arg_result.register);
            }

            arg_regs.push(target_reg);
        }

        // Call instruction: call(func, arg_count, ret_count)
        let arg_count = arg_regs.len() as u8;
        self.chunk.call(call_func_reg, arg_count, 1);

        // Result is in call_func_reg after call (convention)
        // We need to move it to a new temp
        let result_reg = self.allocate_temp()?;
        self.chunk.move_reg(result_reg, call_func_reg);

        // Free the temp we used for the call
        self.free_temp(call_func_reg);

        Ok(ExprResult::temp(result_reg))
    }

    /// Compile array indexing
    fn compile_index(&mut self, object: &Expr, index: &Expr) -> Result<ExprResult> {
        let obj_result = self.compile_expr_to_reg(object)?;
        let idx_result = self.compile_expr_to_reg(index)?;

        let dest = self.allocate_temp()?;

        self.chunk
            .get_index(dest, obj_result.register, idx_result.register);

        if obj_result.is_temp {
            self.free_temp(obj_result.register);
        }
        if idx_result.is_temp {
            self.free_temp(idx_result.register);
        }

        Ok(ExprResult::temp(dest))
    }

    /// Compile property access
    fn compile_property_access(&mut self, object: &Expr, property: &str) -> Result<ExprResult> {
        // Special case: Check if this is enum variant access (EnumType.Variant)
        // If the object is an identifier that refers to an enum type, compile as enum variant
        if let Expr::Identifier(name) = object {
            if let Some(var_info) = self.variables.get(name) {
                if var_info.is_type {
                    // This is an enum type - compile as enum variant with no fields
                    return self.compile_enum_variant(name, property, &[]);
                }
            }
        }

        // Regular property access
        let obj_result = self.compile_expr_to_reg(object)?;
        let dest = self.allocate_temp()?;

        let field_const = self
            .chunk
            .add_constant(Constant::String(property.to_string()));

        self.chunk
            .get_field(dest, obj_result.register, field_const as u8);

        if obj_result.is_temp {
            self.free_temp(obj_result.register);
        }

        Ok(ExprResult::temp(dest))
    }

    /// Compile array literal
    fn compile_array(&mut self, elements: &[Expr]) -> Result<ExprResult> {
        let dest = self.allocate_temp()?;

        // Create array with size
        self.chunk.new_array(dest, elements.len() as u8);

        // For now, array creation needs proper initialization
        // This is simplified - a real implementation would populate the array
        for (i, elem) in elements.iter().enumerate() {
            let elem_result = self.compile_expr_to_reg(elem)?;
            let idx_reg = self.allocate_temp()?;
            let i_const = self.chunk.add_constant(Constant::Integer(i as i64));
            self.chunk.load_const(idx_reg, i_const);
            self.chunk.set_index(dest, idx_reg, elem_result.register);

            self.free_temp(idx_reg);
            if elem_result.is_temp {
                self.free_temp(elem_result.register);
            }
        }

        Ok(ExprResult::temp(dest))
    }

    /// Compile tuple literal
    fn compile_tuple(&mut self, elements: &[Expr]) -> Result<ExprResult> {
        let dest = self.allocate_temp()?;

        if elements.is_empty() {
            // Empty tuple - just create it
            self.chunk.new_tuple(dest, 0);
            return Ok(ExprResult::temp(dest));
        }

        // Allocate consecutive registers for tuple elements
        // NewTuple expects elements in consecutive registers starting at dest+1
        let elem_base = self
            .allocator
            .allocate_range(elements.len() as u8)
            .map_err(|e| VeldError::CompileError {
                message: format!("Failed to allocate registers for tuple: {}", e),
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Compile each element and move it to its consecutive register
        for (i, elem) in elements.iter().enumerate() {
            let result = self.compile_expr_to_reg(elem)?;
            let target_reg = elem_base + i as u8;

            // Move to target position if needed
            if result.register != target_reg {
                self.chunk.move_reg(target_reg, result.register);
            }

            // Free source register if it was a temp and different from target
            if result.is_temp && result.register != target_reg {
                self.free_temp(result.register);
            }
        }

        // Ensure elem_base is at dest+1 by moving if necessary
        // The NewTuple instruction expects elements at dest+1, dest+2, etc.
        if elem_base != dest + 1 {
            // Need to move all elements to the right positions
            for i in 0..elements.len() {
                let src_reg = elem_base + i as u8;
                let target_reg = dest + 1 + i as u8;
                self.chunk.move_reg(target_reg, src_reg);
            }
            // Free the old range
            for i in 0..elements.len() {
                self.free_temp(elem_base + i as u8);
            }
        }

        // Create the tuple from consecutive registers at dest+1
        self.chunk.new_tuple(dest, elements.len() as u8);

        Ok(ExprResult::temp(dest))
    }

    /// Compile struct literal
    fn compile_struct(&mut self, name: &str, fields: &[(String, Expr)]) -> Result<ExprResult> {
        let dest = self.allocate_temp()?;

        // VM expects fields in consecutive registers after dest:
        // dest+1: field_name_1, dest+2: field_value_1, dest+3: field_name_2, dest+4: field_value_2, ...

        let mut temp_regs = Vec::new();

        for (_i, (field_name, field_value)) in fields.iter().enumerate() {
            // Compile field value to a temporary register
            let value_result = self.compile_expr_to_reg(field_value)?;
            temp_regs.push((field_name.clone(), value_result));
        }

        // Now move field names and values into consecutive registers after dest
        for (i, (field_name, value_result)) in temp_regs.iter().enumerate() {
            // Load field name into dest + (i*2) + 1
            let name_reg = dest.wrapping_add((i as u8 * 2) + 1);
            let name_const = self
                .chunk
                .add_constant(Constant::String(field_name.clone()));
            self.chunk.load_const(name_reg, name_const);

            // Move field value into dest + (i*2) + 2
            let value_reg = dest.wrapping_add((i as u8 * 2) + 2);
            if value_result.register != value_reg {
                self.chunk.move_reg(value_reg, value_result.register);
            }
        }

        // Free temporary registers used for values
        for (_, value_result) in temp_regs {
            if value_result.is_temp {
                self.free_temp(value_result.register);
            }
        }

        // Emit NewStruct instruction
        let name_const = self.chunk.add_constant(Constant::String(name.to_string()));
        let field_count = fields.len() as u8;
        self.chunk.new_struct(dest, name_const, field_count);

        Ok(ExprResult::temp(dest))
    }

    /// Compile lambda expression
    fn compile_lambda(
        &mut self,
        params: &[(String, Option<TypeAnnotation>)],
        body: &Expr,
        _return_type: &Option<TypeAnnotation>,
    ) -> Result<ExprResult> {
        // Create a new compiler for the lambda body
        let mut lambda_compiler = RegisterCompiler::with_options(self.options.clone());

        // Allocate registers for parameters
        lambda_compiler.allocator = RegisterAllocator::with_params(params.len() as u8);

        // Register parameter variables
        for (i, (param_name, _)) in params.iter().enumerate() {
            lambda_compiler.variables.insert(
                param_name.clone(),
                VarInfo {
                    register: i as Reg,
                    is_mutable: false,
                    depth: 0,
                    is_captured: false,
                    is_upvalue: false,
                    is_type: false,
                    is_global_ref: false,
                },
            );
        }

        // Compile the body
        lambda_compiler.begin_scope();
        let result = lambda_compiler.compile_expr_to_reg(body)?;

        // Return the result
        lambda_compiler.chunk.return_vals(result.register, 1);

        if result.is_temp {
            lambda_compiler.free_temp(result.register);
        }
        lambda_compiler.end_scope();

        // Set register count before building
        let max_reg = lambda_compiler.allocator.max_register();
        let register_count = if max_reg == 0 {
            // Need at least enough registers for parameters
            params.len().max(1) as u8
        } else {
            (max_reg + 1).max(params.len() as u8)
        };
        lambda_compiler.chunk.register_count(register_count);

        let lambda_chunk = lambda_compiler.chunk.build();

        // Create function proto from chunk
        let mut proto = FunctionProto::new("<lambda>".to_string(), params.len() as u8);
        proto.instructions = lambda_chunk.main.instructions;
        proto.constants = lambda_chunk.main.constants;
        proto.line_info = lambda_chunk.main.line_info;
        proto.register_count = lambda_chunk.main.register_count;

        // Add as constant
        let proto_const = self.chunk.add_constant(Constant::Function(Box::new(proto)));

        // Create closure from proto
        let dest = self.allocate_temp()?;
        self.chunk.closure(dest, proto_const);

        Ok(ExprResult::temp(dest))
    }

    /// Compile function declaration
    fn compile_function_declaration(
        &mut self,
        name: &str,
        params: &[(String, TypeAnnotation)],
        body: &[Statement],
    ) -> Result<()> {
        // Allocate register for function variable BEFORE compiling body
        // This allows recursive calls to reference the function
        let func_reg = self
            .allocator
            .allocate_variable(name.to_string(), false)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Track function variable before compiling body (for recursion)
        self.variables.insert(
            name.to_string(),
            VarInfo {
                register: func_reg,
                is_mutable: false,
                depth: self.scope_depth,
                is_captured: false,
                is_upvalue: false,
                is_type: false,
                is_global_ref: false,
            },
        );

        // Analyze captures before creating the function compiler
        // Pass the function name so it can be excluded from captures (handled specially)
        let captures = self.analyze_captures_for_function(body, &self.variables, name);

        // Mark captured variables (need to do this separately to avoid borrow issues)
        let capture_names: Vec<String> = captures.iter().map(|c| c.name.clone()).collect();
        for name in capture_names {
            if let Some(var) = self.variables.get_mut(&name) {
                var.is_captured = true;
            }
        }

        // Create a new compiler for the function body
        let mut func_compiler = RegisterCompiler::with_options(self.options.clone());
        func_compiler.allocator = RegisterAllocator::with_params(params.len() as u8);
        func_compiler.function_depth = self.function_depth + 1;

        // Register parameter variables
        for (i, (param_name, _)) in params.iter().enumerate() {
            func_compiler.variables.insert(
                param_name.clone(),
                VarInfo {
                    register: i as Reg,
                    is_mutable: false,
                    depth: 0,
                    is_captured: false,
                    is_upvalue: false,
                    is_type: false,
                    is_global_ref: false,
                },
            );
        }

        // Set up upvalues in the nested compiler
        for capture in &captures {
            func_compiler.upvalues.push(capture.clone());
            // Add upvalues to variables map so nested functions can find them
            // Mark them as upvalues so they use GetUpvalue instruction
            func_compiler.variables.insert(
                capture.name.clone(),
                VarInfo {
                    register: capture.register, // This is the upvalue index in practice
                    is_mutable: capture.is_mutable,
                    depth: 0,
                    is_captured: false,
                    is_upvalue: true, // This is an upvalue, not a local variable
                    is_type: false,
                    is_global_ref: false,
                },
            );
        }

        // For recursive functions: Add the function name to nested compiler's variables
        // so it can reference itself.
        if self.function_depth == 0 {
            // Top-level function - use LoadGlobal for self-reference to avoid circular upvalue dependency
            func_compiler.variables.insert(
                name.to_string(),
                VarInfo {
                    register: 0, // Dummy register, will use LoadGlobal instead
                    is_mutable: false,
                    depth: 0,
                    is_captured: false,
                    is_upvalue: false,
                    is_type: false,
                    is_global_ref: true, // Mark as global reference
                },
            );
        }

        // Compile the body
        func_compiler.begin_scope();

        // Track if the last statement is an expression that should be returned
        let last_stmt_index = body.len().saturating_sub(1);
        let mut last_expr_reg: Option<Reg> = None;

        for (i, stmt) in body.iter().enumerate() {
            if i == last_stmt_index {
                // Check if last statement is an expression statement or if/else
                match stmt {
                    Statement::ExprStatement(expr) => {
                        // Compile the expression and keep its result
                        let result = func_compiler.compile_expr_to_reg(expr)?;
                        last_expr_reg = Some(result.register);
                        // Don't free the temp - we need it for return
                    }
                    Statement::If {
                        condition,
                        then_branch,
                        else_branch: Some(else_branch),
                    } => {
                        // If/else as expression - compile it and capture the result
                        let result_reg = func_compiler.allocate_temp()?;
                        func_compiler.compile_if_expression(
                            condition,
                            then_branch,
                            else_branch,
                            result_reg,
                        )?;
                        last_expr_reg = Some(result_reg);
                    }
                    _ => {
                        func_compiler.compile_statement(stmt)?;
                    }
                }
            } else {
                func_compiler.compile_statement(stmt)?;
            }
        }

        // Return the last expression or nil
        let return_reg = if let Some(reg) = last_expr_reg {
            reg
        } else {
            let nil_const = func_compiler.chunk.add_constant(Constant::Nil);
            let nil_reg = func_compiler.allocate_temp()?;
            func_compiler.chunk.load_const(nil_reg, nil_const);
            nil_reg
        };
        func_compiler.chunk.return_vals(return_reg, 1);

        func_compiler.end_scope();

        // Set register count before building
        let max_reg = func_compiler.allocator.max_register();
        let register_count = if max_reg == 0 {
            // Need at least enough registers for parameters
            params.len().max(1) as u8
        } else {
            (max_reg + 1).max(params.len() as u8)
        };
        func_compiler.chunk.register_count(register_count);

        let func_chunk = func_compiler.chunk.build();

        // Create function proto with upvalue count
        let mut proto = FunctionProto::new(name.to_string(), params.len() as u8);
        proto.instructions = func_chunk.main.instructions;
        proto.constants = func_chunk.main.constants;
        proto.line_info = func_chunk.main.line_info;
        proto.register_count = func_chunk.main.register_count;

        // Convert compiler upvalue info to bytecode upvalue info
        // Include both explicit captures and self-reference (if any)
        proto.upvalues = func_compiler
            .upvalues
            .iter()
            .map(|c| BytecodeUpvalueInfo {
                register: c.register,
                is_local: !c.is_upvalue,
                name: c.name.clone(),
            })
            .collect();

        // Add as constant
        let proto_const = self.chunk.add_constant(Constant::Function(Box::new(proto)));

        // func_reg was already allocated before compiling the body (for recursion support)
        // Now emit the closure instruction to create the function at runtime
        if captures.is_empty() {
            // No upvalues, simple closure
            self.chunk.closure(func_reg, proto_const);
        } else {
            // Emit closure with upvalue setup
            self.chunk.closure(func_reg, proto_const);

            // For each captured variable, we need to set up the upvalue
            // This is done by the VM when it executes the Closure instruction
            // The VM will look at the upvalue_count in the FunctionProto
            // and capture the appropriate registers/upvalues from the parent frame

            // NOTE: The actual upvalue capture happens at runtime in the VM
            // We just need to ensure the captures are tracked and the proto
            // knows how many upvalues to expect
        }

        // Function variable already tracked (done before compiling body)

        // For top-level functions, also store as global so recursive calls via LoadGlobal work
        if self.function_depth == 0 {
            let name_const = self.chunk.add_constant(Constant::String(name.to_string()));
            self.chunk.store_global(name_const, func_reg);
        }

        Ok(())
    }

    /// Compile struct declaration
    fn compile_struct_declaration(
        &mut self,
        name: &str,
        fields: &[veld_common::ast::StructField],
    ) -> Result<()> {
        // Extract field names
        let field_names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();

        // Create TypeInfo for this struct
        let type_info = veld_common::bytecode_v2::TypeInfo {
            name: name.to_string(),
            kind: veld_common::bytecode_v2::TypeKind::Struct {
                fields: field_names.clone(),
            },
        };

        // Add TypeInfo as a constant
        let type_const = self.chunk.add_constant(Constant::Type(type_info.clone()));

        // Allocate a register for the struct type value and load it
        let type_reg = self
            .allocator
            .allocate_variable(name.to_string(), false)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Load the type constant into the register
        self.chunk.load_const(type_reg, type_const);

        // Register the struct name as a variable holding the Type value
        self.variables.insert(
            name.to_string(),
            VarInfo {
                register: type_reg,
                is_mutable: false,
                depth: self.scope_depth,
                is_captured: false,
                is_upvalue: false,
                is_type: true, // Mark as a type value
                is_global_ref: false,
            },
        );

        // Also store metadata as JSON for backward compatibility
        let metadata_json = serde_json::json!({
            "type": "struct",
            "name": name,
            "fields": field_names,
        });
        let metadata_str = metadata_json.to_string();
        let _metadata_const = self.chunk.add_constant(Constant::String(metadata_str));

        Ok(())
    }

    /// Compile enum declaration
    fn compile_enum_declaration(
        &mut self,
        name: &str,
        variants: &[veld_common::ast::EnumVariant],
    ) -> Result<()> {
        // Extract variant names
        let variant_names: Vec<String> = variants.iter().map(|v| v.name.clone()).collect();

        // Create TypeInfo for this enum
        let type_info = veld_common::bytecode_v2::TypeInfo {
            name: name.to_string(),
            kind: veld_common::bytecode_v2::TypeKind::Enum {
                variants: variant_names.clone(),
            },
        };

        // Add TypeInfo as a constant
        let type_const = self.chunk.add_constant(Constant::Type(type_info.clone()));

        // Allocate a register for the enum type value and load it
        let type_reg = self
            .allocator
            .allocate_variable(name.to_string(), false)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Load the type constant into the register
        self.chunk.load_const(type_reg, type_const);

        // Register the enum name as a variable holding the Type value
        self.variables.insert(
            name.to_string(),
            VarInfo {
                register: type_reg,
                is_mutable: false,
                depth: self.scope_depth,
                is_captured: false,
                is_upvalue: false,
                is_type: true, // Mark as a type value
                is_global_ref: false,
            },
        );

        // Also store metadata as JSON for backward compatibility
        let variant_info: Vec<serde_json::Value> = variants
            .iter()
            .map(|v| {
                let field_count = v.fields.as_ref().map(|f| f.len()).unwrap_or(0);
                serde_json::json!({
                    "name": v.name.clone(),
                    "field_count": field_count,
                })
            })
            .collect();

        let metadata_json = serde_json::json!({
            "type": "enum",
            "name": name,
            "variants": variant_info,
        });
        let metadata_str = metadata_json.to_string();
        let _metadata_const = self.chunk.add_constant(Constant::String(metadata_str));

        Ok(())
    }

    /// Compile enum variant expression
    fn compile_enum_variant(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        fields: &[Expr],
    ) -> Result<ExprResult> {
        let dest = self.allocate_temp()?;

        // Compile all field values to temporary registers
        let mut temp_results = Vec::new();
        for field_expr in fields {
            let result = self.compile_expr_to_reg(field_expr)?;
            temp_results.push(result);
        }

        // Move field values into consecutive registers after dest
        for (i, result) in temp_results.iter().enumerate() {
            let target_reg = dest.wrapping_add((i as u8) + 1);
            if result.register != target_reg {
                self.chunk.move_reg(target_reg, result.register);
            }
        }

        // Free temporary registers
        for result in temp_results {
            if result.is_temp {
                self.free_temp(result.register);
            }
        }

        // Create variant metadata string "EnumName::VariantName"
        let variant_metadata = format!("{}::{}", enum_name, variant_name);
        let variant_const = self.chunk.add_constant(Constant::String(variant_metadata));

        // Emit NewEnum instruction
        let field_count = fields.len() as u8;
        self.chunk.new_enum(dest, variant_const, field_count);

        Ok(ExprResult::temp(dest))
    }

    /// Resolve an upvalue by name
    /// Returns the index in the upvalue list if found
    #[allow(dead_code)] // Note that this is unused; check again later
    fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        // Check if we already have this upvalue
        if let Some(idx) = self.upvalues.iter().position(|u| u.name == name) {
            return Some(idx);
        }

        // If no parent, this is not an upvalue
        if self.parent.is_none() {
            return None;
        }

        // Check if it's a local in the parent
        // For now, we'll use a simpler approach and check at compile time
        None
    }

    /// Find an upvalue index for a variable name
    fn find_upvalue(&self, name: &str) -> Option<usize> {
        self.upvalues.iter().position(|u| u.name == name)
    }

    /// Add an upvalue to the closure
    #[allow(dead_code)] // Note that this is unused; check again later
    fn add_upvalue(
        &mut self,
        name: String,
        register: Reg,
        is_mutable: bool,
        is_upvalue: bool,
        parent_upvalue_index: Option<usize>,
    ) -> usize {
        // Check if we already have this upvalue
        if let Some(idx) = self.upvalues.iter().position(|u| u.name == name) {
            return idx;
        }

        // Add new upvalue
        let idx = self.upvalues.len();
        self.upvalues.push(CompilerUpvalueInfo {
            name,
            register,
            is_upvalue,
            parent_upvalue_index,
            is_mutable,
        });
        idx
    }

    /// Analyze a function body to determine which variables are captured
    fn analyze_captures(
        &self,
        body: &[Statement],
        parent_vars: &HashMap<String, VarInfo>,
    ) -> Vec<CompilerUpvalueInfo> {
        let mut captures = Vec::new();
        self.find_captured_vars_in_statements(body, parent_vars, &mut captures);
        captures
    }

    /// Analyze captures for a function declaration, excluding self-references
    fn analyze_captures_for_function(
        &self,
        body: &[Statement],
        parent_vars: &HashMap<String, VarInfo>,
        function_name: &str,
    ) -> Vec<CompilerUpvalueInfo> {
        let mut captures = Vec::new();
        self.find_captured_vars_in_statements(body, parent_vars, &mut captures);
        // Remove self-reference from captures - handled specially for recursion
        captures.retain(|c| c.name != function_name);
        captures
    }

    /// Recursively find captured variables in statements
    fn find_captured_vars_in_statements(
        &self,
        statements: &[Statement],
        parent_vars: &HashMap<String, VarInfo>,
        captures: &mut Vec<CompilerUpvalueInfo>,
    ) {
        for stmt in statements {
            self.find_captured_vars_in_statement(stmt, parent_vars, captures);
        }
    }

    /// Find captured variables in a single statement
    fn find_captured_vars_in_statement(
        &self,
        statement: &Statement,
        parent_vars: &HashMap<String, VarInfo>,
        captures: &mut Vec<CompilerUpvalueInfo>,
    ) {
        match statement {
            Statement::VariableDeclaration { value, .. } => {
                self.find_captured_vars_in_expr(value, parent_vars, captures);
            }
            Statement::FunctionDeclaration { body, .. } => {
                // Nested function - analyze its body too
                self.find_captured_vars_in_statements(body, parent_vars, captures);
            }
            Statement::ExprStatement(expr) => {
                self.find_captured_vars_in_expr(expr, parent_vars, captures);
            }
            Statement::Assignment { value, .. } => {
                self.find_captured_vars_in_expr(value, parent_vars, captures);
            }
            Statement::PropertyAssignment { target, value, .. } => {
                self.find_captured_vars_in_expr(target, parent_vars, captures);
                self.find_captured_vars_in_expr(value, parent_vars, captures);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.find_captured_vars_in_expr(condition, parent_vars, captures);
                self.find_captured_vars_in_statements(then_branch, parent_vars, captures);
                if let Some(else_stmts) = else_branch {
                    self.find_captured_vars_in_statements(else_stmts, parent_vars, captures);
                }
            }
            Statement::While { condition, body } => {
                self.find_captured_vars_in_expr(condition, parent_vars, captures);
                self.find_captured_vars_in_statements(body, parent_vars, captures);
            }
            Statement::For { iterable, body, .. } => {
                self.find_captured_vars_in_expr(iterable, parent_vars, captures);
                self.find_captured_vars_in_statements(body, parent_vars, captures);
            }
            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.find_captured_vars_in_expr(expr, parent_vars, captures);
                }
            }
            Statement::Match { value, arms } => {
                self.find_captured_vars_in_expr(value, parent_vars, captures);
                for arm in arms {
                    self.find_captured_vars_in_expr(&arm.body, parent_vars, captures);
                    if let Some(guard) = &arm.guard {
                        self.find_captured_vars_in_expr(guard, parent_vars, captures);
                    }
                }
            }
            Statement::BlockScope { body } => {
                self.find_captured_vars_in_statements(body, parent_vars, captures);
            }
            Statement::StructDeclaration { .. } => {
                // Struct declarations don't capture variables
            }
            Statement::EnumDeclaration { .. } => {
                // Enum declarations don't capture variables
            }
            _ => {}
        }
    }

    /// Find captured variables in an expression
    fn find_captured_vars_in_expr(
        &self,
        expr: &Expr,
        parent_vars: &HashMap<String, VarInfo>,
        captures: &mut Vec<CompilerUpvalueInfo>,
    ) {
        match expr {
            Expr::Identifier(name) => {
                // Check if this variable is from the parent scope
                if let Some(var_info) = parent_vars.get(name) {
                    // Check if we already captured this variable
                    if !captures.iter().any(|c| c.name == *name) {
                        captures.push(CompilerUpvalueInfo {
                            name: name.clone(),
                            register: var_info.register,
                            is_upvalue: false,
                            parent_upvalue_index: None,
                            is_mutable: var_info.is_mutable,
                        });
                    }
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.find_captured_vars_in_expr(left, parent_vars, captures);
                self.find_captured_vars_in_expr(right, parent_vars, captures);
            }
            Expr::UnaryOp { operand, .. } => {
                self.find_captured_vars_in_expr(operand, parent_vars, captures);
            }
            Expr::FunctionCall { name: _, arguments } => {
                // Function name is not an expression in this variant
                for arg in arguments {
                    if let Argument::Positional(e) = arg {
                        self.find_captured_vars_in_expr(e, parent_vars, captures);
                    }
                }
            }
            Expr::Call { callee, arguments } => {
                // Handle general call expression - callee can be any expression including Identifier
                self.find_captured_vars_in_expr(callee, parent_vars, captures);
                for arg in arguments {
                    if let Argument::Positional(e) = arg {
                        self.find_captured_vars_in_expr(e, parent_vars, captures);
                    }
                }
            }
            Expr::Lambda { body, .. } => {
                // Lambda body is an expression, not statements
                self.find_captured_vars_in_expr(body, parent_vars, captures);
            }
            Expr::BlockLambda { body, .. } => {
                // BlockLambda body is statements
                self.find_captured_vars_in_statements(body, parent_vars, captures);
            }
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                self.find_captured_vars_in_expr(condition, parent_vars, captures);
                self.find_captured_vars_in_expr(then_expr, parent_vars, captures);
                if let Some(else_e) = else_expr {
                    self.find_captured_vars_in_expr(else_e, parent_vars, captures);
                }
            }
            Expr::BlockExpression {
                statements,
                final_expr,
            } => {
                self.find_captured_vars_in_statements(statements, parent_vars, captures);
                if let Some(final_e) = final_expr {
                    self.find_captured_vars_in_expr(final_e, parent_vars, captures);
                }
            }
            Expr::ArrayLiteral(elements) => {
                for elem in elements {
                    self.find_captured_vars_in_expr(elem, parent_vars, captures);
                }
            }
            Expr::IndexAccess { object, index } => {
                self.find_captured_vars_in_expr(object, parent_vars, captures);
                self.find_captured_vars_in_expr(index, parent_vars, captures);
            }
            Expr::PropertyAccess { object, .. } => {
                self.find_captured_vars_in_expr(object, parent_vars, captures);
            }
            Expr::LetIn {
                name, value, body, ..
            } => {
                // Find captured vars in the value expression
                self.find_captured_vars_in_expr(value, parent_vars, captures);
                // For the body, the let-bound variable is now local, so exclude it
                let mut body_vars = parent_vars.clone();
                body_vars.remove(name);
                self.find_captured_vars_in_expr(body, &body_vars, captures);
            }
            _ => {}
        }
    }

    /// Compile if/else as an expression that produces a value
    fn compile_if_expression(
        &mut self,
        condition: &Expr,
        then_branch: &[Statement],
        else_branch: &[Statement],
        result_reg: Reg,
    ) -> Result<()> {
        // Compile condition
        let cond_result = self.compile_expr_to_reg(condition)?;

        // Jump to else if condition is false
        let else_jump = self.chunk.jump_if_not(cond_result.register, 0);

        if cond_result.is_temp {
            self.free_temp(cond_result.register);
        }

        // Compile then branch - last expression goes to result_reg
        self.begin_scope();
        let then_last = then_branch.len().saturating_sub(1);
        for (i, stmt) in then_branch.iter().enumerate() {
            if i == then_last {
                if let Statement::ExprStatement(expr) = stmt {
                    let expr_result = self.compile_expr_to_reg(expr)?;
                    if expr_result.register != result_reg {
                        self.chunk.move_reg(result_reg, expr_result.register);
                    }
                    if expr_result.is_temp && expr_result.register != result_reg {
                        self.free_temp(expr_result.register);
                    }
                } else {
                    self.compile_statement(stmt)?;
                    // No explicit value, use unit
                    let unit_const = self.chunk.add_constant(Constant::Nil);
                    self.chunk.load_const(result_reg, unit_const);
                }
            } else {
                self.compile_statement(stmt)?;
            }
        }
        self.end_scope();

        // Jump over else branch
        let end_jump = self.chunk.jump(0);

        // Patch else jump
        self.chunk.patch_jump(else_jump);

        // Compile else branch - last expression goes to result_reg
        self.begin_scope();
        let else_last = else_branch.len().saturating_sub(1);
        for (i, stmt) in else_branch.iter().enumerate() {
            if i == else_last {
                if let Statement::ExprStatement(expr) = stmt {
                    let expr_result = self.compile_expr_to_reg(expr)?;
                    if expr_result.register != result_reg {
                        self.chunk.move_reg(result_reg, expr_result.register);
                    }
                    if expr_result.is_temp && expr_result.register != result_reg {
                        self.free_temp(expr_result.register);
                    }
                } else {
                    self.compile_statement(stmt)?;
                    // No explicit value, use unit
                    let unit_const = self.chunk.add_constant(Constant::Nil);
                    self.chunk.load_const(result_reg, unit_const);
                }
            } else {
                self.compile_statement(stmt)?;
            }
        }
        self.end_scope();

        // Patch end jump
        self.chunk.patch_jump(end_jump);

        Ok(())
    }

    /// Compile if statement
    fn compile_if(
        &mut self,
        condition: &Expr,
        then_branch: &[Statement],
        else_branch: Option<&[Statement]>,
    ) -> Result<()> {
        let cond_result = self.compile_expr_to_reg(condition)?;

        // Jump to else/end if condition is false
        let then_jump = self.chunk.jump_if_not(cond_result.register, 0);

        if cond_result.is_temp {
            self.free_temp(cond_result.register);
        }

        // Compile then branch (with its own scope)
        self.begin_scope();
        for stmt in then_branch {
            self.compile_statement(stmt)?;
        }
        self.end_scope();

        if let Some(else_stmts) = else_branch {
            // Jump over else branch
            let else_jump = self.chunk.jump(0);

            // Patch then_jump to here (else branch start)
            self.chunk.patch_jump(then_jump);

            // Compile else branch (with its own scope)
            self.begin_scope();
            for stmt in else_stmts {
                self.compile_statement(stmt)?;
            }
            self.end_scope();

            // Patch else_jump to end
            self.chunk.patch_jump(else_jump);
        } else {
            // Patch then_jump to end
            self.chunk.patch_jump(then_jump);
        }

        Ok(())
    }

    /// Compile while loop
    fn compile_while(&mut self, condition: &Expr, body: &[Statement]) -> Result<()> {
        let loop_start = self.chunk.current_index();

        // Push loop context
        self.loop_stack.push(LoopContext {
            start_index: loop_start,
            break_jumps: Vec::new(),
            depth: self.scope_depth,
        });

        // Compile condition
        let cond_result = self.compile_expr_to_reg(condition)?;

        // Jump to end if false
        let exit_jump = self.chunk.jump_if_not(cond_result.register, 0);

        if cond_result.is_temp {
            self.free_temp(cond_result.register);
        }

        // Compile body
        self.begin_scope();
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.end_scope();

        // Jump back to start
        self.chunk.jump_back(loop_start);

        // Patch exit jump
        self.chunk.patch_jump(exit_jump);

        // Patch break jumps
        if let Some(loop_ctx) = self.loop_stack.pop() {
            for break_idx in loop_ctx.break_jumps {
                // We need to manually calculate and patch
                let current = self.chunk.current_index();
                let offset = (current as i32 - break_idx as i32 - 1) as i16;
                // Note: This is a workaround - we'd need proper patch_jump with index
                // For now, mark as TODO
                warn!("Break jump patching needs refinement: {:?}", offset);
            }
        }

        Ok(())
    }

    /// Compile for loop with iterator protocol
    fn compile_for(
        &mut self,
        iterator: &veld_common::ast::Pattern,
        iterable: &Expr,
        body: &[Statement],
    ) -> Result<()> {
        self.begin_scope();

        // Compile iterable expression
        let iter_result = self.compile_expr_to_reg(iterable)?;

        // Generate a unique name for the iterator variable
        let iter_name = format!("$iter_{}", self.chunk.current_index());

        // Allocate iterator register as a variable (not temp) so it persists through loop
        let iter_reg = self
            .allocator
            .allocate_variable(iter_name, false)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Create iterator from iterable
        self.chunk.make_iterator(iter_reg, iter_result.register);

        if iter_result.is_temp {
            self.free_temp(iter_result.register);
        }

        // Allocate a temporary register to hold each element from the iterator
        let loop_var = self.allocate_temp()?;

        // Push loop context for break/continue
        let loop_start = self.chunk.current_index();
        self.loop_stack.push(LoopContext {
            start_index: loop_start,
            break_jumps: Vec::new(),
            depth: self.scope_depth,
        });

        // Check if iterator has next (ForIterator instruction)
        // This will jump to end if iterator is exhausted
        let end_jump = self.chunk.for_iterator(iter_reg, loop_var, 0);

        // Bind the iterator pattern to the loop variable
        self.compile_pattern_binding(iterator, loop_var, &VarKind::Let)?;

        // Compile loop body
        for stmt in body {
            self.compile_statement(stmt)?;
        }

        // Jump back to loop start
        // current_index() is where the Jump will be placed
        // After fetching Jump, PC will be at current_index() + 1
        // We want to jump to loop_start
        // offset = loop_start - (current_index() + 1)
        let current = self.chunk.current_index();
        let jump_offset = (loop_start as i32 - current as i32 - 1) as i16;
        self.chunk.jump(jump_offset);

        // Patch the end jump
        self.chunk.patch_jump(end_jump);

        // Patch break jumps
        let loop_ctx = self.loop_stack.pop().unwrap();
        for break_jump in loop_ctx.break_jumps {
            self.chunk.patch_jump(break_jump);
        }

        self.free_temp(loop_var);
        self.end_scope();

        Ok(())
    }

    /// Compile return statement
    fn compile_return(&mut self, value: Option<&Expr>) -> Result<()> {
        if let Some(expr) = value {
            let result = self.compile_expr_to_reg(expr)?;
            self.chunk.return_vals(result.register, 1);

            if result.is_temp {
                self.free_temp(result.register);
            }
        } else {
            // Return nil
            let nil_const = self.chunk.add_constant(Constant::Nil);
            let nil_reg = self.allocate_temp()?;
            self.chunk.load_const(nil_reg, nil_const);
            self.chunk.return_vals(nil_reg, 1);
            self.free_temp(nil_reg);
        }

        Ok(())
    }

    /// Compile break statement
    fn compile_break(&mut self) -> Result<()> {
        let loop_ctx = self
            .loop_stack
            .last_mut()
            .ok_or_else(|| VeldError::CompileError {
                message: "Break outside of loop".to_string(),
                line: Some(self.current_line as usize),
                column: None,
            })?;

        let jump_idx = self.chunk.jump(0);

        loop_ctx.break_jumps.push(jump_idx);

        Ok(())
    }

    /// Compile continue statement
    fn compile_continue(&mut self) -> Result<()> {
        let loop_ctx = self
            .loop_stack
            .last()
            .ok_or_else(|| VeldError::CompileError {
                message: "Continue outside of loop".to_string(),
                line: Some(self.current_line as usize),
                column: None,
            })?;

        let loop_start = loop_ctx.start_index;
        self.chunk.jump_back(loop_start);

        Ok(())
    }

    /// Compile match statement (simplified)
    fn compile_match(&mut self, value: &Expr, arms: &[MatchArm]) -> Result<()> {
        let match_value = self.compile_expr_to_reg(value)?;

        let mut arm_jumps = Vec::new();

        for arm in arms {
            // Begin a new scope for this match arm to prevent variable collisions
            self.begin_scope();

            // Compile pattern match (simplified)
            let pattern_matches = self.compile_match_pattern(&arm.pat, match_value.register)?;

            // Jump to next arm if pattern doesn't match
            let next_arm_jump = self.chunk.jump_if_not(pattern_matches.register, 0);

            if pattern_matches.is_temp {
                self.free_temp(pattern_matches.register);
            }

            // Compile guard if present
            if let Some(guard) = &arm.guard {
                let guard_result = self.compile_expr_to_reg(guard)?;
                let guard_jump = self.chunk.jump_if_not(guard_result.register, 0);

                if guard_result.is_temp {
                    self.free_temp(guard_result.register);
                }

                // Compile arm body (it's an Expr, not Statement)
                let _body_result = self.compile_expr_to_reg(&arm.body)?;

                let arm_end_jump = self.chunk.jump(0);
                arm_jumps.push(arm_end_jump);

                // End the match arm scope
                self.end_scope();

                self.chunk.patch_jump(guard_jump);
                self.chunk.patch_jump(next_arm_jump);
            } else {
                // Compile arm body
                let _body_result = self.compile_expr_to_reg(&arm.body)?;

                let arm_end_jump = self.chunk.jump(0);
                arm_jumps.push(arm_end_jump);

                // End the match arm scope
                self.end_scope();

                self.chunk.patch_jump(next_arm_jump);
            }
        }

        // Patch all arm end jumps to the end
        for jump_idx in arm_jumps {
            self.chunk.patch_jump(jump_idx);
        }

        if match_value.is_temp {
            self.free_temp(match_value.register);
        }

        Ok(())
    }

    /// Compile match expression (returns a value)
    fn compile_match_expression(&mut self, value: &Expr, arms: &[MatchArm]) -> Result<ExprResult> {
        let match_value = self.compile_expr_to_reg(value)?;

        // Allocate a register to hold the result of the match expression
        let result_reg = self.allocate_temp()?;

        let mut arm_jumps = Vec::new();

        for arm in arms {
            // Begin a new scope for this match arm to prevent variable collisions
            self.begin_scope();

            // Compile pattern match
            let pattern_matches = self.compile_match_pattern(&arm.pat, match_value.register)?;

            // Jump to next arm if pattern doesn't match
            let next_arm_jump = self.chunk.jump_if_not(pattern_matches.register, 0);

            if pattern_matches.is_temp {
                self.free_temp(pattern_matches.register);
            }

            // Compile guard if present
            if let Some(guard) = &arm.guard {
                let guard_result = self.compile_expr_to_reg(guard)?;
                let guard_jump = self.chunk.jump_if_not(guard_result.register, 0);

                if guard_result.is_temp {
                    self.free_temp(guard_result.register);
                }

                // Compile arm body and store result
                let body_result = self.compile_expr_to_reg(&arm.body)?;
                self.chunk.move_reg(result_reg, body_result.register);

                if body_result.is_temp {
                    self.free_temp(body_result.register);
                }

                let arm_end_jump = self.chunk.jump(0);
                arm_jumps.push(arm_end_jump);

                // End the match arm scope
                self.end_scope();

                self.chunk.patch_jump(guard_jump);
                self.chunk.patch_jump(next_arm_jump);
            } else {
                // Compile arm body and store result
                let body_result = self.compile_expr_to_reg(&arm.body)?;
                self.chunk.move_reg(result_reg, body_result.register);

                if body_result.is_temp {
                    self.free_temp(body_result.register);
                }

                let arm_end_jump = self.chunk.jump(0);
                arm_jumps.push(arm_end_jump);

                // End the match arm scope
                self.end_scope();

                self.chunk.patch_jump(next_arm_jump);
            }
        }

        // Patch all arm end jumps to the end
        for jump_idx in arm_jumps {
            self.chunk.patch_jump(jump_idx);
        }

        if match_value.is_temp {
            self.free_temp(match_value.register);
        }

        Ok(ExprResult::temp(result_reg))
    }

    /// Compile match pattern
    fn compile_match_pattern(
        &mut self,
        pattern: &MatchPattern,
        match_reg: Reg,
    ) -> Result<ExprResult> {
        let result = self.allocate_temp()?;

        match pattern {
            MatchPattern::Literal(lit) => {
                // Compare literal value with match value
                let pattern_result = self.compile_literal(lit)?;
                self.chunk.eq(result, match_reg, pattern_result.register);

                if pattern_result.is_temp {
                    self.free_temp(pattern_result.register);
                }
            }

            MatchPattern::Wildcard => {
                // Always matches - load true
                let true_const = self.chunk.add_constant(Constant::Boolean(true));
                self.chunk.load_const(result, true_const);
            }

            MatchPattern::Identifier(name) => {
                // Check if this is a wildcard pattern (_)
                if name == "_" {
                    // Wildcard - just always matches, don't bind
                    let true_const = self.chunk.add_constant(Constant::Boolean(true));
                    self.chunk.load_const(result, true_const);
                } else {
                    // Variable binding - always matches
                    // Bind the matched value to the variable
                    let var_reg = self
                        .allocator
                        .allocate_variable(name.clone(), false)
                        .map_err(|e| VeldError::CompileError {
                            message: e,
                            line: Some(self.current_line as usize),
                            column: None,
                        })?;

                    // Move matched value to the variable register
                    self.chunk.move_reg(var_reg, match_reg);

                    // Register the variable in scope
                    self.variables.insert(
                        name.clone(),
                        VarInfo {
                            register: var_reg,
                            is_mutable: false,
                            depth: self.scope_depth,
                            is_captured: false,
                            is_upvalue: false,
                            is_type: false,
                            is_global_ref: false,
                        },
                    );

                    // Pattern always matches
                    let true_const = self.chunk.add_constant(Constant::Boolean(true));
                    self.chunk.load_const(result, true_const);
                }
            }

            MatchPattern::Enum {
                name,
                variant,
                fields,
            } => {
                // Create pattern string "EnumName::VariantName"
                let pattern_str = format!("{}::{}", name, variant);
                let pattern_const = self.chunk.add_constant(Constant::String(pattern_str));

                // Use MatchPattern instruction to check if enum matches
                // For now, use a simpler approach: check type and variant at runtime
                // Load pattern string and use it for comparison
                let pattern_reg = self.allocate_temp()?;
                self.chunk.load_const(pattern_reg, pattern_const);

                // Generate code to check if match_reg is the right enum variant
                // For simplicity, we'll use a runtime helper
                // Set result to true for now (proper implementation would check variant)
                let true_const = self.chunk.add_constant(Constant::Boolean(true));
                self.chunk.load_const(result, true_const);

                // Extract and bind fields if present
                for (i, (field_name, field_pattern)) in fields.iter().enumerate() {
                    if let Some(pat) = field_pattern {
                        // Extract field from enum
                        let field_reg = self.allocate_temp()?;
                        // ExtractField instruction: dest, enum_value, field_idx
                        self.chunk.extract_field(field_reg, match_reg, i as u8);

                        // Recursively match the field pattern
                        let field_match = self.compile_match_pattern(pat, field_reg)?;

                        // AND the field match with overall result
                        let temp = self.allocate_temp()?;
                        self.chunk.and(temp, result, field_match.register);
                        self.chunk.move_reg(result, temp);

                        self.free_temp(temp);
                        if field_match.is_temp {
                            self.free_temp(field_match.register);
                        }
                        self.free_temp(field_reg);
                    } else if !field_name.is_empty() && field_name != "_" {
                        // Bind field to variable name (skip if it's a wildcard)
                        let field_reg = self.allocate_temp()?;
                        self.chunk.extract_field(field_reg, match_reg, i as u8);

                        // Move to variable register
                        let var_reg = self
                            .allocator
                            .allocate_variable(field_name.clone(), false)
                            .map_err(|e| VeldError::CompileError {
                                message: e,
                                line: Some(self.current_line as usize),
                                column: None,
                            })?;

                        self.chunk.move_reg(var_reg, field_reg);

                        self.variables.insert(
                            field_name.clone(),
                            VarInfo {
                                register: var_reg,
                                is_mutable: false,
                                depth: self.scope_depth,
                                is_captured: false,
                                is_upvalue: false,
                                is_type: false,
                                is_global_ref: false,
                            },
                        );

                        self.free_temp(field_reg);
                    }
                }

                self.free_temp(pattern_reg);
            }

            MatchPattern::Struct { name, fields } => {
                // Check if the value is a struct of the correct type
                // For now, assume it matches if it's a struct (simplified)
                let true_const = self.chunk.add_constant(Constant::Boolean(true));
                self.chunk.load_const(result, true_const);

                // Detect if this is an enum pattern (dotted name like "Option.some")
                // vs a struct pattern (simple name like "Point")
                let is_enum_pattern = name.contains('.');

                // Extract and bind fields
                for (i, (field_name, field_pattern)) in fields.iter().enumerate() {
                    if let Some(pat) = field_pattern {
                        // Get field value
                        let field_reg = self.allocate_temp()?;

                        if is_enum_pattern {
                            // For enum patterns, use ExtractField with numeric index
                            self.chunk.extract_field(field_reg, match_reg, i as u8);
                        } else {
                            // For struct patterns, use GetField with field name
                            let field_name_const = self
                                .chunk
                                .add_constant(Constant::String(field_name.clone()));
                            self.chunk
                                .get_field(field_reg, match_reg, field_name_const as u8);
                        }

                        // Recursively match the field pattern
                        let field_match = self.compile_match_pattern(pat, field_reg)?;

                        // AND the field match with overall result
                        let temp = self.allocate_temp()?;
                        self.chunk.and(temp, result, field_match.register);
                        self.chunk.move_reg(result, temp);

                        self.free_temp(temp);
                        if field_match.is_temp {
                            self.free_temp(field_match.register);
                        }
                        self.free_temp(field_reg);
                    } else if field_name != "_" {
                        // Bind field to variable with same name (skip wildcards)
                        let field_reg = self.allocate_temp()?;

                        if is_enum_pattern {
                            // For enum patterns, use ExtractField with numeric index
                            self.chunk.extract_field(field_reg, match_reg, i as u8);
                        } else {
                            // For struct patterns, use GetField with field name
                            let field_name_const = self
                                .chunk
                                .add_constant(Constant::String(field_name.clone()));
                            self.chunk
                                .get_field(field_reg, match_reg, field_name_const as u8);
                        }

                        // Allocate variable
                        let var_reg = self
                            .allocator
                            .allocate_variable(field_name.clone(), false)
                            .map_err(|e| VeldError::CompileError {
                                message: e,
                                line: Some(self.current_line as usize),
                                column: None,
                            })?;

                        self.chunk.move_reg(var_reg, field_reg);

                        self.variables.insert(
                            field_name.clone(),
                            VarInfo {
                                register: var_reg,
                                is_mutable: false,
                                depth: self.scope_depth,
                                is_captured: false,
                                is_upvalue: false,
                                is_type: false,
                                is_global_ref: false,
                            },
                        );

                        self.free_temp(field_reg);
                    }
                }
            }
        }

        Ok(ExprResult::temp(result))
    }

    /// Begin a new scope
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.allocator.begin_scope();

        // Push a new scope for tracking variable shadowing
        self.scope_stack.push(ScopeInfo {
            depth: self.scope_depth,
            variables: Vec::new(),
        });
    }

    /// End the current scope
    fn end_scope(&mut self) {
        // Pop the scope and restore shadowed variables
        if let Some(scope) = self.scope_stack.pop() {
            for (var_name, shadowed) in scope.variables {
                if let Some(old_var) = shadowed {
                    // Restore the shadowed variable
                    self.variables.insert(var_name, old_var);
                } else {
                    // Remove the variable (it wasn't shadowing anything)
                    self.variables.remove(&var_name);
                }
            }
        }

        self.allocator.end_scope();
        self.scope_depth -= 1;
    }

    /// Allocate a temporary register
    fn allocate_temp(&mut self) -> Result<Reg> {
        self.allocator
            .allocate()
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })
    }

    /// Free a temporary register
    fn free_temp(&mut self, reg: Reg) {
        self.allocator.free(reg);
    }
}

impl Default for RegisterCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_literal() {
        let mut compiler = RegisterCompiler::new();

        let expr = Expr::Literal(Literal::Integer(42));
        let result = compiler.compile_expr_to_reg(&expr).unwrap();

        assert!(result.is_temp);
        assert_eq!(result.register, 0);
    }

    #[test]
    fn test_compile_binary_add() {
        let mut compiler = RegisterCompiler::new();

        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Literal::Integer(10))),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::Literal(Literal::Integer(20))),
        };

        let result = compiler.compile_expr_to_reg(&expr).unwrap();
        assert!(result.is_temp);
    }

    #[test]
    fn test_compile_variable() {
        let mut compiler = RegisterCompiler::new();

        let stmt = Statement::VariableDeclaration {
            pattern: veld_common::ast::Pattern::Identifier("x".to_string()),
            value: Box::new(Expr::Literal(Literal::Integer(42))),
            var_kind: VarKind::Let,
            type_annotation: None,
            is_public: false,
        };

        compiler.compile_statement(&stmt).unwrap();

        assert!(compiler.variables.contains_key("x"));
    }

    #[test]
    fn test_variable_shadowing() {
        let mut compiler = RegisterCompiler::new();

        // First declaration
        compiler.begin_scope();
        let stmt1 = Statement::VariableDeclaration {
            pattern: veld_common::ast::Pattern::Identifier("x".to_string()),
            value: Box::new(Expr::Literal(Literal::Integer(10))),
            var_kind: VarKind::Let,
            type_annotation: None,
            is_public: false,
        };
        compiler.compile_statement(&stmt1).unwrap();

        // Shadow with new scope
        compiler.begin_scope();
        let stmt2 = Statement::VariableDeclaration {
            pattern: veld_common::ast::Pattern::Identifier("x".to_string()),
            value: Box::new(Expr::Literal(Literal::Integer(20))),
            var_kind: VarKind::Let,
            type_annotation: None,
            is_public: false,
        };
        compiler.compile_statement(&stmt2).unwrap();

        compiler.end_scope();
        compiler.end_scope();
    }
}
