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
use veld_common::bytecode_v2::{Chunk, ChunkBuilder, Constant, FunctionProto};
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
}

/// Variable information
#[derive(Debug, Clone)]
struct VarInfo {
    register: Reg,
    is_mutable: bool,
    depth: usize,
}

/// Scope tracking for variable shadowing
#[derive(Debug, Clone)]
struct ScopeInfo {
    depth: usize,
    /// Variables declared in this scope with their shadowed values
    variables: Vec<(String, Option<VarInfo>)>,
}

/// Loop context for break/continue
#[derive(Debug, Clone)]
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
        }
    }

    /// Compile an AST to register-based bytecode
    pub fn compile(&mut self, ast: &AST) -> Result<Chunk> {
        if self.options.trace_compilation {
            debug!("Starting register-based compilation");
        }

        // Compile all statements in the AST
        for statement in &ast.statements {
            self.compile_statement(statement)?;
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
                name,
                value,
                var_kind,
                type_annotation: _,
                is_public: _,
            } => {
                self.compile_var_declaration(name, value, var_kind)?;
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
    fn compile_var_declaration(
        &mut self,
        name: &str,
        value: &Box<Expr>,
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

        // Compile initializer
        let result = self.compile_expr_to_reg(value)?;

        // Move result to variable's register
        if result.register != var_reg {
            self.chunk.move_reg(var_reg, result.register);
        }

        if result.is_temp {
            self.free_temp(result.register);
        }

        // Track variable with shadowing support
        let var_info = VarInfo {
            register: var_reg,
            is_mutable,
            depth: self.scope_depth,
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
        let var_info = self
            .variables
            .get(name)
            .ok_or_else(|| VeldError::CompileError {
                message: format!("Undefined variable: {}", name),
                line: Some(self.current_line as usize),
                column: None,
            })?
            .clone();

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

        Ok(())
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
                return self.compile_simple_assignment(name, value);
            }

            Expr::IndexAccess { object, index } => {
                // Array/tuple index assignment: obj[idx] = value
                let obj_result = self.compile_expr_to_reg(object)?;
                let idx_result = self.compile_expr_to_reg(index)?;
                let val_result = self.compile_expr_to_reg(value)?;

                // TODO: handle compound assignment operators
                if operator.is_some() {
                    warn!("Compound assignment operators not yet implemented");
                }

                self.chunk.set_index(
                    obj_result.register,
                    idx_result.register,
                    val_result.register,
                );

                if obj_result.is_temp {
                    self.free_temp(obj_result.register);
                }
                if idx_result.is_temp {
                    self.free_temp(idx_result.register);
                }
                if val_result.is_temp {
                    self.free_temp(val_result.register);
                }
            }

            Expr::PropertyAccess { object, property } => {
                // Struct field assignment: obj.field = value
                let obj_result = self.compile_expr_to_reg(object)?;
                let val_result = self.compile_expr_to_reg(value)?;

                // TODO: handle compound assignment operators
                if operator.is_some() {
                    warn!("Compound assignment operators not yet implemented");
                }

                let field_const = self.chunk.add_constant(Constant::String(property.clone()));

                // Note: get_field takes u8 for field_idx, but we have ConstIdx (u16)
                // This is a limitation - for now, cast and hope it fits
                self.chunk
                    .set_field(obj_result.register, field_const as u8, val_result.register);

                if obj_result.is_temp {
                    self.free_temp(obj_result.register);
                }
                if val_result.is_temp {
                    self.free_temp(val_result.register);
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
        let var_info = self
            .variables
            .get(name)
            .ok_or_else(|| VeldError::CompileError {
                message: format!("Undefined variable: {}", name),
                line: Some(self.current_line as usize),
                column: None,
            })?
            .clone();

        // Variable is already in a register, return it (not a temp)
        Ok(ExprResult::var(var_info.register))
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

        // Compile arguments to consecutive registers
        let mut arg_regs = Vec::new();
        for arg in arguments {
            let arg_expr = match arg {
                Argument::Positional(expr) => expr,
                Argument::Named { value, .. } => value,
            };
            let arg_result = self.compile_expr_to_reg(arg_expr)?;
            arg_regs.push(arg_result);
        }

        // Call instruction: call(func, arg_count, ret_count)
        // The VM will handle setting up arguments
        let arg_count = arg_regs.len() as u8;

        self.chunk.call(func_result.register, arg_count, 1);

        // Result is in func_result.register after call (convention)
        // We need to move it to a new temp
        let result_reg = self.allocate_temp()?;
        self.chunk.move_reg(result_reg, func_result.register);

        // Free temporaries
        if func_result.is_temp {
            self.free_temp(func_result.register);
        }
        for arg_result in arg_regs {
            if arg_result.is_temp {
                self.free_temp(arg_result.register);
            }
        }

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

        // Compile all elements
        let mut elem_results = Vec::new();
        for elem in elements {
            let result = self.compile_expr_to_reg(elem)?;
            elem_results.push(result);
        }

        // NewTuple takes size
        self.chunk.new_tuple(dest, elem_results.len() as u8);

        // Free temporaries
        for result in elem_results {
            if result.is_temp {
                self.free_temp(result.register);
            }
        }

        Ok(ExprResult::temp(dest))
    }

    /// Compile struct literal
    fn compile_struct(&mut self, name: &str, fields: &[(String, Expr)]) -> Result<ExprResult> {
        let dest = self.allocate_temp()?;

        let name_const = self.chunk.add_constant(Constant::String(name.to_string()));
        let field_count = fields.len() as u8;

        self.chunk.new_struct(dest, name_const, field_count);

        // Set each field
        for (field_name, field_value) in fields {
            let value_result = self.compile_expr_to_reg(field_value)?;
            let field_const = self
                .chunk
                .add_constant(Constant::String(field_name.clone()));

            self.chunk
                .set_field(dest, field_const as u8, value_result.register);

            if value_result.is_temp {
                self.free_temp(value_result.register);
            }
        }

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
        // Create a new compiler for the function body
        let mut func_compiler = RegisterCompiler::with_options(self.options.clone());
        func_compiler.allocator = RegisterAllocator::with_params(params.len() as u8);

        // Register parameter variables
        for (i, (param_name, _)) in params.iter().enumerate() {
            func_compiler.variables.insert(
                param_name.clone(),
                VarInfo {
                    register: i as Reg,
                    is_mutable: false,
                    depth: 0,
                },
            );
        }

        // Compile the body
        func_compiler.begin_scope();
        for stmt in body {
            func_compiler.compile_statement(stmt)?;
        }

        // Add implicit return nil if no explicit return
        let nil_const = func_compiler.chunk.add_constant(Constant::Nil);
        let nil_reg = func_compiler.allocate_temp()?;
        func_compiler.chunk.load_const(nil_reg, nil_const);
        func_compiler.chunk.return_vals(nil_reg, 1);

        func_compiler.end_scope();

        let func_chunk = func_compiler.chunk.build();

        // Create function proto
        let mut proto = FunctionProto::new(name.to_string(), params.len() as u8);
        proto.instructions = func_chunk.main.instructions;
        proto.constants = func_chunk.main.constants;
        proto.line_info = func_chunk.main.line_info;
        proto.register_count = func_chunk.main.register_count;

        // Add as constant
        let proto_const = self.chunk.add_constant(Constant::Function(Box::new(proto)));

        // Allocate register for function variable
        let func_reg = self
            .allocator
            .allocate_variable(name.to_string(), false)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        // Create closure
        self.chunk.closure(func_reg, proto_const);

        // Track function variable
        self.variables.insert(
            name.to_string(),
            VarInfo {
                register: func_reg,
                is_mutable: false,
                depth: self.scope_depth,
            },
        );

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

    /// Compile for loop (simplified)
    fn compile_for(&mut self, iterator: &str, iterable: &Expr, body: &[Statement]) -> Result<()> {
        // Simplified: assume iterable is an array
        // Real implementation would handle iterators properly

        self.begin_scope();

        // Compile iterable
        let iter_result = self.compile_expr_to_reg(iterable)?;

        // Allocate loop variable
        let loop_var = self
            .allocator
            .allocate_variable(iterator.to_string(), false)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line as usize),
                column: None,
            })?;

        self.variables.insert(
            iterator.to_string(),
            VarInfo {
                register: loop_var,
                is_mutable: false,
                depth: self.scope_depth,
            },
        );

        // For now, emit a placeholder (full iterator support needed)
        warn!("For loop compilation is simplified");

        if iter_result.is_temp {
            self.free_temp(iter_result.register);
        }

        for stmt in body {
            self.compile_statement(stmt)?;
        }

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

                self.chunk.patch_jump(guard_jump);
                self.chunk.patch_jump(next_arm_jump);
            } else {
                // Compile arm body
                let _body_result = self.compile_expr_to_reg(&arm.body)?;

                let arm_end_jump = self.chunk.jump(0);
                arm_jumps.push(arm_end_jump);

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

    /// Compile match pattern (very simplified)
    fn compile_match_pattern(
        &mut self,
        pattern: &MatchPattern,
        match_reg: Reg,
    ) -> Result<ExprResult> {
        let result = self.allocate_temp()?;

        match pattern {
            MatchPattern::Literal(lit) => {
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

            MatchPattern::Identifier(_name) => {
                // Variable binding - always matches
                let true_const = self.chunk.add_constant(Constant::Boolean(true));
                self.chunk.load_const(result, true_const);
                // TODO: bind the variable
            }

            _ => {
                warn!("Match pattern not fully implemented: {:?}", pattern);
                let true_const = self.chunk.add_constant(Constant::Boolean(true));
                self.chunk.load_const(result, true_const);
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
    use veld_common::ast::*;

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
            name: "x".to_string(),
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
            name: "x".to_string(),
            value: Box::new(Expr::Literal(Literal::Integer(10))),
            var_kind: VarKind::Let,
            type_annotation: None,
            is_public: false,
        };
        compiler.compile_statement(&stmt1).unwrap();

        // Shadow with new scope
        compiler.begin_scope();
        let stmt2 = Statement::VariableDeclaration {
            name: "x".to_string(),
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
