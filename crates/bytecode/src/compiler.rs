use std::collections::HashMap;
use tracing::{trace, warn};
use veld_common::ast::{
    AST, Argument, BinaryOperator, Expr, FunctionDeclaration, Literal, MatchArm, MatchPattern,
    Statement, TypeAnnotation, UnaryOperator, VarKind,
};
use veld_common::bytecode::{Chunk, ChunkBuilder, Instruction};
use veld_common::value::Value;
use veld_error::{Result, VeldError};

/// Bytecode compiler for converting Veld AST to bytecode
pub struct BytecodeCompiler {
    /// Current chunk being compiled
    current_chunk: Chunk,

    /// Stack of chunks for nested functions
    chunk_stack: Vec<Chunk>,

    /// Local variable tracking
    locals: Vec<Local>,

    /// Current scope depth
    scope_depth: usize,

    /// Upvalue tracking for closures
    upvalues: Vec<Upvalue>,

    /// Label tracking for control flow
    labels: HashMap<String, usize>,

    /// Forward jump patches
    forward_jumps: Vec<ForwardJump>,

    /// Loop context stack
    loop_stack: Vec<LoopContext>,

    /// Function context stack
    function_stack: Vec<FunctionContext>,

    /// Global variable names
    globals: HashMap<String, usize>,

    /// Current line number for debugging
    current_line: usize,

    /// Compilation options
    options: CompilerOptions,
}

/// Local variable information
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: usize,
    is_captured: bool,
    is_mutable: bool,
    slot: usize,
}

/// Upvalue information for closures
#[derive(Debug, Clone)]
struct Upvalue {
    index: usize,
    is_local: bool,
    name: String,
}

/// Forward jump information for patching
#[derive(Debug, Clone)]
struct ForwardJump {
    instruction_index: usize,
    target_label: String,
    jump_type: JumpType,
}

#[derive(Debug, Clone)]
enum JumpType {
    Unconditional,
    IfFalse,
    IfTrue,
}

/// Loop context for break/continue
#[derive(Debug, Clone)]
struct LoopContext {
    start_label: String,
    end_label: String,
    scope_depth: usize,
}

/// Function context
#[derive(Debug, Clone)]
struct FunctionContext {
    name: String,
    arity: usize,
    local_count: usize,
    upvalue_count: usize,
}

/// Compiler options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub optimize: bool,
    pub debug_info: bool,
    pub trace_compilation: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            optimize: true,
            debug_info: true,
            trace_compilation: false,
        }
    }
}

/// Compilation result
#[derive(Debug)]
pub struct CompilationResult {
    pub main_chunk: Chunk,
    pub function_chunks: Vec<Chunk>,
    pub errors: Vec<VeldError>,
}

impl BytecodeCompiler {
    /// Create a new bytecode compiler
    pub fn new() -> Self {
        Self::with_options(CompilerOptions::default())
    }

    /// Create a new compiler with options
    pub fn with_options(options: CompilerOptions) -> Self {
        Self {
            current_chunk: Chunk::new(),
            chunk_stack: Vec::new(),
            locals: Vec::new(),
            scope_depth: 0,
            upvalues: Vec::new(),
            labels: HashMap::new(),
            forward_jumps: Vec::new(),
            loop_stack: Vec::new(),
            function_stack: Vec::new(),
            globals: HashMap::new(),
            current_line: 1,
            options,
        }
    }

    /// Compile an AST to bytecode
    pub fn compile(&mut self, ast: &AST) -> CompilationResult {
        let mut errors = Vec::new();

        // Initialize main chunk
        self.current_chunk = ChunkBuilder::new().with_name("main".to_string()).build();

        // Compile all statements
        for statement in &ast.statements {
            if let Err(error) = self.compile_statement(statement) {
                errors.push(error);
            }
        }

        // Add halt instruction at the end
        self.emit_instruction(Instruction::Halt, self.current_line);

        // Finalize chunk
        self.current_chunk.local_count = self.locals.len();
        self.current_chunk.calculate_max_stack_depth();
        if self.options.optimize {
            self.current_chunk.optimize();
        }

        // Patch forward jumps
        self.patch_forward_jumps();

        // Validate the chunk
        if let Err(validation_error) = self.current_chunk.validate() {
            errors.push(VeldError::CompileError {
                message: validation_error,
                line: None,
                column: None,
            });
        }

        CompilationResult {
            main_chunk: self.current_chunk.clone(),
            function_chunks: Vec::new(), // TODO: Collect function chunks
            errors,
        }
    }

    /// Compile a statement
    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        if self.options.trace_compilation {
            trace!("Compiling statement: {:?}", statement);
        }

        match statement {
            Statement::ExprStatement(expr) => {
                self.compile_expression(expr)?;
                // Pop expression result if not used
                self.emit_instruction(Instruction::Pop, self.current_line);
            }

            Statement::VariableDeclaration {
                var_kind,
                name,
                type_annotation: _,
                value,
                ..
            } => {
                self.compile_expression(value)?;
                let is_mutable = matches!(var_kind, VarKind::Var | VarKind::LetMut);
                self.declare_variable(name.clone(), is_mutable)?;
            }

            Statement::Assignment { name, value, .. } => {
                self.compile_expression(value)?;
                if let Some(local_index) = self.resolve_local(name) {
                    self.emit_instruction(
                        Instruction::StoreLocal(local_index as u16),
                        self.current_line,
                    );
                } else {
                    // Fallback: treat as global assignment
                    let name_index = self.current_chunk.add_constant(Value::String(name.clone()));
                    self.emit_instruction(
                        Instruction::StoreGlobal(name_index as u16),
                        self.current_line,
                    );
                }
            }

            Statement::PropertyAssignment { target, value, .. } => {
                self.compile_expression(value)?;
                self.compile_assignment_target(target)?;
            }

            Statement::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
                is_proc: _,
                is_public: _,
                generic_params: _,
            } => {
                let func_decl = FunctionDeclaration {
                    name: name.clone(),
                    params: params.clone(),
                    return_type: return_type.clone(),
                    body: body.clone(),
                    is_proc: false,
                    is_public: false,
                    generic_params: vec![],
                };
                self.compile_function(&func_decl)?;
            }

            Statement::Return(expr) => {
                if let Some(return_expr) = expr {
                    self.compile_expression(return_expr)?;
                } else {
                    let unit_index = self.current_chunk.add_constant(Value::Unit);
                    self.emit_instruction(
                        Instruction::LoadConstant(unit_index as u16),
                        self.current_line,
                    );
                }
                self.emit_instruction(Instruction::Return, self.current_line);
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                // then_branch and else_branch are Vec<Statement>, but compile_if_statement expects Statement
                // We need to convert them to BlockScope statements
                let then_stmt = &Statement::BlockScope {
                    body: then_branch.clone(),
                };
                let else_stmt = else_branch.as_ref().map(|stmts| Statement::BlockScope {
                    body: stmts.clone(),
                });

                self.compile_if_statement(condition, then_stmt, else_stmt.as_ref())?;
            }

            Statement::While {
                condition, body, ..
            } => {
                let body_stmt = &Statement::BlockScope { body: body.clone() };
                self.compile_while_loop(condition, body_stmt)?;
            }

            Statement::For {
                iterator,
                iterable,
                body,
                ..
            } => {
                let body_stmt = &Statement::BlockScope { body: body.clone() };
                self.compile_for_loop(iterator, iterable, body_stmt)?;
            }

            Statement::Match { value, arms, .. } => {
                self.compile_match_statement(value, arms)?;
            }

            Statement::Break => {
                if let Some(loop_ctx) = self.loop_stack.last().cloned() {
                    let jump_index = self.emit_instruction(Instruction::Jump(0), self.current_line);
                    self.forward_jumps.push(ForwardJump {
                        instruction_index: jump_index,
                        target_label: loop_ctx.end_label,
                        jump_type: JumpType::Unconditional,
                    });
                } else {
                    return Err(VeldError::CompileError {
                        message: "break outside of loop".to_string(),
                        line: Some(self.current_line),
                        column: None,
                    });
                }
            }

            Statement::Continue => {
                if let Some(loop_ctx) = self.loop_stack.last().cloned() {
                    let jump_index = self.emit_instruction(Instruction::Jump(0), self.current_line);
                    self.forward_jumps.push(ForwardJump {
                        instruction_index: jump_index,
                        target_label: loop_ctx.start_label,
                        jump_type: JumpType::Unconditional,
                    });
                } else {
                    return Err(VeldError::CompileError {
                        message: "continue outside of loop".to_string(),
                        line: Some(self.current_line),
                        column: None,
                    });
                }
            }

            Statement::BlockScope { body } => {
                self.begin_scope();
                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.end_scope();
            }
            _ => {
                warn!("Unhandled statement type: {:?}", statement);
            }
        }

        Ok(())
    }

    /// Compile an expression
    fn compile_expression(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Literal(literal) => {
                let value = self.literal_to_bytecode_value(literal);
                let index = self.current_chunk.add_constant(value);
                self.emit_instruction(Instruction::LoadConstant(index as u16), self.current_line);
            }

            Expr::Identifier(name) => {
                self.compile_variable_access(name)?;
            }

            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;
                self.compile_binary_operator(operator)?;
            }

            Expr::UnaryOp { operator, operand } => {
                self.compile_expression(operand)?;
                self.compile_unary_operator(operator)?;
            }

            Expr::Call {
                callee, arguments, ..
            } => {
                self.compile_expression(callee)?;

                // Compile arguments
                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.compile_expression(expr)?;
                        }
                        Argument::Named { value, .. } => {
                            self.compile_expression(value)?;
                        }
                    }
                }

                self.emit_instruction(Instruction::Call(arguments.len() as u8), self.current_line);
            }

            Expr::IndexAccess { object, index } => {
                self.compile_expression(object)?;
                self.compile_expression(index)?;
                self.emit_instruction(Instruction::GetIndex, self.current_line);
            }

            Expr::PropertyAccess { object, property } => {
                self.compile_expression(object)?;
                let field_index = self
                    .current_chunk
                    .add_constant(Value::String(property.clone()));
                self.emit_instruction(Instruction::GetField(field_index as u16), self.current_line);
            }

            Expr::ArrayLiteral(elements) => {
                for element in elements {
                    self.compile_expression(element)?;
                }
                self.emit_instruction(
                    Instruction::NewArray(elements.len() as u16),
                    self.current_line,
                );
            }

            Expr::TupleLiteral(elements) => {
                for element in elements {
                    self.compile_expression(element)?;
                }
                self.emit_instruction(
                    Instruction::NewTuple(elements.len() as u8),
                    self.current_line,
                );
            }

            Expr::Lambda {
                params: _,
                body: _,
                return_type: _,
                generic_params: _,
            } => {
                // Simplified lambda compilation
                warn!("Lambda compilation not fully implemented");
                let unit_index = self.current_chunk.add_constant(Value::Unit);
                self.emit_instruction(
                    Instruction::LoadConstant(unit_index as u16),
                    self.current_line,
                );
            }

            _ => {
                warn!("Unhandled expression type: {:?}", expr);
                // Push unit as placeholder
                let unit_index = self.current_chunk.add_constant(Value::Unit);
                self.emit_instruction(
                    Instruction::LoadConstant(unit_index as u16),
                    self.current_line,
                );
            }
        }

        Ok(())
    }

    /// Compile binary operator
    fn compile_binary_operator(&mut self, op: &BinaryOperator) -> Result<()> {
        let instruction = match op {
            BinaryOperator::Add => Instruction::Add,
            BinaryOperator::Subtract => Instruction::Subtract,
            BinaryOperator::Multiply => Instruction::Multiply,
            BinaryOperator::Divide => Instruction::Divide,
            BinaryOperator::Modulo => Instruction::Modulo,
            BinaryOperator::EqualEqual => Instruction::Equal,
            BinaryOperator::NotEqual => Instruction::NotEqual,
            BinaryOperator::Less => Instruction::Less,
            BinaryOperator::LessEq => Instruction::LessEqual,
            BinaryOperator::Greater => Instruction::Greater,
            BinaryOperator::GreaterEq => Instruction::GreaterEqual,
            BinaryOperator::And => Instruction::LogicalAnd,
            BinaryOperator::Or => Instruction::LogicalOr,

            _ => {
                return Err(VeldError::CompileError {
                    message: format!("Unsupported binary operator: {:?}", op),
                    line: Some(self.current_line),
                    column: None,
                });
            }
        };

        self.emit_instruction(instruction, self.current_line);
        Ok(())
    }

    /// Compile unary operator
    fn compile_unary_operator(&mut self, op: &UnaryOperator) -> Result<()> {
        let instruction = match op {
            UnaryOperator::Negate => Instruction::Negate,
            UnaryOperator::Not => Instruction::LogicalNot,
        };

        self.emit_instruction(instruction, self.current_line);
        Ok(())
    }

    /// Compile variable access
    fn compile_variable_access(&mut self, name: &str) -> Result<()> {
        // Check locals first
        if let Some(local_index) = self.resolve_local(name) {
            self.emit_instruction(
                Instruction::LoadLocal(local_index as u16),
                self.current_line,
            );
            return Ok(());
        }

        // Check upvalues
        if let Some(upvalue_index) = self.resolve_upvalue(name) {
            self.emit_instruction(
                Instruction::LoadUpvalue(upvalue_index as u16),
                self.current_line,
            );
            return Ok(());
        }

        // Assume global
        let name_index = self
            .current_chunk
            .add_constant(Value::String(name.to_string()));
        self.emit_instruction(
            Instruction::LoadGlobal(name_index as u16),
            self.current_line,
        );
        Ok(())
    }

    /// Compile assignment target
    fn compile_assignment_target(&mut self, target: &Expr) -> Result<()> {
        match target {
            Expr::Identifier(name) => {
                if let Some(local_index) = self.resolve_local(name) {
                    self.emit_instruction(
                        Instruction::StoreLocal(local_index as u16),
                        self.current_line,
                    );
                } else if let Some(upvalue_index) = self.resolve_upvalue(name) {
                    self.emit_instruction(
                        Instruction::StoreUpvalue(upvalue_index as u16),
                        self.current_line,
                    );
                } else {
                    let name_index = self.current_chunk.add_constant(Value::String(name.clone()));
                    self.emit_instruction(
                        Instruction::StoreGlobal(name_index as u16),
                        self.current_line,
                    );
                }
            }

            Expr::IndexAccess { object, index } => {
                self.compile_expression(object)?;
                self.compile_expression(index)?;
                self.emit_instruction(Instruction::SetIndex, self.current_line);
            }

            Expr::PropertyAccess { object, property } => {
                self.compile_expression(object)?;
                let field_index = self
                    .current_chunk
                    .add_constant(Value::String(property.clone()));
                self.emit_instruction(Instruction::SetField(field_index as u16), self.current_line);
            }

            _ => {
                return Err(VeldError::CompileError {
                    message: "Invalid assignment target".to_string(),
                    line: Some(self.current_line),
                    column: None,
                });
            }
        }

        Ok(())
    }

    /// Compile if statement
    fn compile_if_statement(
        &mut self,
        condition: &Expr,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Result<()> {
        // Compile condition
        self.compile_expression(condition)?;

        // Jump if false to else branch or end
        let then_jump = self.emit_instruction(Instruction::PopJumpIfFalse(0), self.current_line);

        // Compile then branch
        self.compile_statement(then_branch)?;

        if let Some(else_stmt) = else_branch {
            // Jump over else branch
            let else_jump = self.emit_instruction(Instruction::Jump(0), self.current_line);

            // Patch the then jump to here
            let else_start = self.current_chunk.instruction_count();
            self.current_chunk
                .patch_jump(then_jump, else_start)
                .map_err(|e| VeldError::CompileError {
                    message: e,
                    line: Some(self.current_line),
                    column: None,
                })?;

            // Compile else branch
            self.compile_statement(else_stmt)?;

            // Patch the else jump to here
            let end_pos = self.current_chunk.instruction_count();
            self.current_chunk
                .patch_jump(else_jump, end_pos)
                .map_err(|e| VeldError::CompileError {
                    message: e,
                    line: Some(self.current_line),
                    column: None,
                })?;
        } else {
            // Patch the then jump to here
            let end_pos = self.current_chunk.instruction_count();
            self.current_chunk
                .patch_jump(then_jump, end_pos)
                .map_err(|e| VeldError::CompileError {
                    message: e,
                    line: Some(self.current_line),
                    column: None,
                })?;
        }

        Ok(())
    }

    /// Compile while loop
    fn compile_while_loop(&mut self, condition: &Expr, body: &Statement) -> Result<()> {
        let loop_start = self.current_chunk.instruction_count();
        let start_label = format!("loop_start_{}", loop_start);
        let end_label = format!("loop_end_{}", loop_start);

        // Push loop context
        self.loop_stack.push(LoopContext {
            start_label: start_label.clone(),
            end_label: end_label.clone(),
            scope_depth: self.scope_depth,
        });

        // Add loop start label
        self.current_chunk.add_label(start_label);

        // Compile condition
        self.compile_expression(condition)?;

        // Jump if false to end
        let exit_jump = self.emit_instruction(Instruction::PopJumpIfFalse(0), self.current_line);

        // Compile body
        self.compile_statement(body)?;

        // Jump back to start
        let back_jump_offset =
            loop_start as i16 - self.current_chunk.instruction_count() as i16 - 1;
        self.emit_instruction(Instruction::Jump(back_jump_offset), self.current_line);

        // Patch exit jump
        let end_pos = self.current_chunk.instruction_count();
        self.current_chunk
            .patch_jump(exit_jump, end_pos)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line),
                column: None,
            })?;
        self.current_chunk.add_label(end_label);

        // Pop loop context
        self.loop_stack.pop();

        Ok(())
    }

    /// Compile for loop (simplified - assumes iterable is array)
    fn compile_for_loop(
        &mut self,
        variable: &str,
        iterable: &Expr,
        body: &Statement,
    ) -> Result<()> {
        self.begin_scope();

        // Compile iterable and create iterator
        self.compile_expression(iterable)?;
        self.emit_instruction(Instruction::MakeIterator, self.current_line);

        // Declare loop variable
        self.declare_variable(variable.to_string(), false)?;

        let loop_start = self.current_chunk.instruction_count();
        let start_label = format!("for_start_{}", loop_start);
        let end_label = format!("for_end_{}", loop_start);

        self.loop_stack.push(LoopContext {
            start_label: start_label.clone(),
            end_label: end_label.clone(),
            scope_depth: self.scope_depth,
        });

        // Loop start
        self.current_chunk.add_label(start_label);

        // Check if iterator has next
        self.emit_instruction(Instruction::Duplicate, self.current_line);
        self.emit_instruction(Instruction::IteratorHasNext, self.current_line);
        let exit_jump = self.emit_instruction(Instruction::PopJumpIfFalse(0), self.current_line);

        // Get next value and store in loop variable
        self.emit_instruction(Instruction::Duplicate, self.current_line);
        self.emit_instruction(Instruction::IteratorNext, self.current_line);
        if let Some(var_index) = self.resolve_local(variable) {
            self.emit_instruction(Instruction::StoreLocal(var_index as u16), self.current_line);
        }

        // Compile body
        self.compile_statement(body)?;

        // Jump back to start
        let back_jump_offset =
            loop_start as i16 - self.current_chunk.instruction_count() as i16 - 1;
        self.emit_instruction(Instruction::Jump(back_jump_offset), self.current_line);

        // Patch exit jump
        let end_pos = self.current_chunk.instruction_count();
        self.current_chunk
            .patch_jump(exit_jump, end_pos)
            .map_err(|e| VeldError::CompileError {
                message: e,
                line: Some(self.current_line),
                column: None,
            })?;
        self.current_chunk.add_label(end_label);

        // Pop iterator from stack
        self.emit_instruction(Instruction::Pop, self.current_line);

        self.loop_stack.pop();
        self.end_scope();

        Ok(())
    }

    /// Compile match statement (simplified)
    fn compile_match_statement(&mut self, expr: &Expr, arms: &[MatchArm]) -> Result<()> {
        // Compile the value to match
        self.compile_expression(expr)?;

        self.emit_instruction(Instruction::MatchStart, self.current_line);

        let mut arm_jumps = Vec::new();
        let mut end_jumps = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            if i > 0 {
                // Patch previous arm jump to here
                let current_pos = self.current_chunk.instruction_count();
                if let Some(jump_index) = arm_jumps.get(i - 1) {
                    self.current_chunk
                        .patch_jump(*jump_index, current_pos)
                        .map_err(|e| VeldError::CompileError {
                            message: e.clone(),
                            line: Some(self.current_line),
                            column: None,
                        })?;
                }
            }

            // Compile pattern matching (simplified)
            self.compile_match_pattern(&arm.pat)?;
            let pattern_jump = self.emit_instruction(Instruction::MatchJump(0), self.current_line);
            arm_jumps.push(pattern_jump);

            // Compile arm body
            if let Some(guard) = &arm.guard {
                self.compile_expression(guard)?;
                let guard_jump =
                    self.emit_instruction(Instruction::PopJumpIfFalse(0), self.current_line);
                arm_jumps.push(guard_jump);
            }

            self.compile_expression(&arm.body)?;

            // Jump to end after executing arm
            let end_jump = self.emit_instruction(Instruction::Jump(0), self.current_line);
            end_jumps.push(end_jump);
        }

        // Patch all end jumps to here
        let end_pos = self.current_chunk.instruction_count();
        for jump_index in end_jumps {
            self.current_chunk
                .patch_jump(jump_index, end_pos)
                .map_err(|e| VeldError::CompileError {
                    message: e,
                    line: Some(self.current_line),
                    column: None,
                })?;
        }

        self.emit_instruction(Instruction::MatchEnd, self.current_line);

        Ok(())
    }

    /// Compile match pattern (very simplified)
    fn compile_match_pattern(&mut self, pattern: &MatchPattern) -> Result<()> {
        match pattern {
            MatchPattern::Literal(literal) => {
                let value = self.literal_to_bytecode_value(literal);
                let index = self.current_chunk.add_constant(value);
                self.emit_instruction(Instruction::MatchPattern(index as u16), self.current_line);
            }
            MatchPattern::Identifier(_name) => {
                // Always matches - implement pattern binding
                // For now, just duplicate the value
                self.emit_instruction(Instruction::Duplicate, self.current_line);
            }
            _ => {
                warn!("Unhandled match pattern: {:?}", pattern);
            }
        }
        Ok(())
    }

    /// Compile function declaration
    fn compile_function(&mut self, func: &FunctionDeclaration) -> Result<()> {
        // Compile the function body into a new chunk
        let mut function_chunk = ChunkBuilder::new()
            .with_name(func.name.clone())
            .with_parameter_count(func.params.len())
            .build();

        // Save current state
        let prev_chunk = std::mem::replace(&mut self.current_chunk, function_chunk);
        let prev_locals = std::mem::replace(&mut self.locals, Vec::new());
        let prev_scope_depth = self.scope_depth;

        self.scope_depth = 0;

        // Declare parameters as locals
        for (i, param) in func.params.iter().enumerate() {
            let name = param.0.clone();
            self.locals.push(Local {
                name,
                depth: 0,
                is_captured: false,
                is_mutable: true,
                slot: i,
            });
        }

        // Compile the function body
        for stmt in &func.body {
            self.compile_statement(stmt)?;
        }

        // Ensure function returns Unit if no explicit return
        let unit_index = self.current_chunk.add_constant(Value::Unit);
        self.emit_instruction(
            Instruction::LoadConstant(unit_index as u16),
            self.current_line,
        );
        self.emit_instruction(Instruction::Return, self.current_line);

        // Restore previous state and get the completed chunk
        function_chunk = std::mem::replace(&mut self.current_chunk, prev_chunk);
        self.locals = prev_locals;
        self.scope_depth = prev_scope_depth;

        // Add the compiled function to the constant pool
        let func_value = Value::CompiledFunction {
            chunk: function_chunk,
            arity: func.params.len() as u8,
            name: Some(func.name.clone()),
        };
        let index = self.current_chunk.add_constant(func_value);

        // Load the function as a constant
        self.emit_instruction(Instruction::LoadConstant(index as u16), self.current_line);

        // Store in global
        let name_index = self
            .current_chunk
            .add_constant(Value::String(func.name.clone()));
        self.emit_instruction(
            Instruction::StoreGlobal(name_index as u16),
            self.current_line,
        );

        Ok(())
    }

    /// Compile lambda expression
    fn compile_lambda(
        &mut self,
        params: &[(String, Option<TypeAnnotation>)],
        body: &Expr,
    ) -> Result<()> {
        // Compile the lambda body into a new chunk
        let mut lambda_chunk = ChunkBuilder::new()
            .with_name("<lambda>".to_string())
            .with_parameter_count(params.len())
            .build();

        // Save current state
        let prev_chunk = std::mem::replace(&mut self.current_chunk, lambda_chunk);
        let prev_locals = std::mem::replace(&mut self.locals, Vec::new());
        let prev_scope_depth = self.scope_depth;

        self.scope_depth = 0;

        // Declare parameters as locals
        for (i, param) in params.iter().enumerate() {
            let name = param.0.clone();
            self.locals.push(Local {
                name,
                depth: 0,
                is_captured: false,
                is_mutable: true,
                slot: i,
            });
        }

        // Compile the lambda body expression
        self.compile_expression(body)?;

        // Ensure lambda returns Unit if no explicit return
        let unit_index = self.current_chunk.add_constant(Value::Unit);
        self.emit_instruction(
            Instruction::LoadConstant(unit_index as u16),
            self.current_line,
        );
        self.emit_instruction(Instruction::Return, self.current_line);

        // Restore previous state and get the completed chunk
        lambda_chunk = std::mem::replace(&mut self.current_chunk, prev_chunk);
        self.locals = prev_locals;
        self.scope_depth = prev_scope_depth;

        // Add the compiled lambda to the constant pool
        let func_value = Value::CompiledFunction {
            chunk: lambda_chunk,
            arity: params.len() as u8,
            name: None,
        };
        let index = self.current_chunk.add_constant(func_value);

        // Load the lambda as a constant
        self.emit_instruction(Instruction::LoadConstant(index as u16), self.current_line);

        Ok(())
    }

    /// Convert literal to bytecode value
    fn literal_to_bytecode_value(&self, literal: &Literal) -> Value {
        match literal {
            Literal::Integer(i) => Value::Integer(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Char(c) => Value::Char(*c),
            Literal::Unit => Value::Unit,
        }
    }

    /// Emit an instruction
    fn emit_instruction(&mut self, instruction: Instruction, line: usize) -> usize {
        self.current_chunk.add_instruction(instruction, line)
    }

    /// Begin a new scope
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// End the current scope
    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        // Remove locals from this scope
        while let Some(local) = self.locals.last() {
            if local.depth > self.scope_depth {
                self.locals.pop();
                self.emit_instruction(Instruction::Pop, self.current_line);
            } else {
                break;
            }
        }
    }

    /// Declare a new variable
    fn declare_variable(&mut self, name: String, is_mutable: bool) -> Result<()> {
        let slot = self.locals.len();

        let local = Local {
            name,
            depth: self.scope_depth,
            is_captured: false,
            is_mutable,
            slot,
        };

        self.locals.push(local);
        Ok(())
    }

    /// Resolve local variable
    fn resolve_local(&self, name: &str) -> Option<usize> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(i);
            }
        }
        None
    }

    /// Resolve upvalue (simplified)
    fn resolve_upvalue(&self, _name: &str) -> Option<usize> {
        // TODO: implement proper upvalue resolution
        None
    }

    /// Patch forward jumps
    fn patch_forward_jumps(&mut self) {
        for jump in &self.forward_jumps {
            if let Some(&target_pos) = self.labels.get(&jump.target_label) {
                if let Err(e) = self
                    .current_chunk
                    .patch_jump(jump.instruction_index, target_pos)
                {
                    warn!("Failed to patch jump: {}", e);
                }
            } else {
                warn!("Undefined label: {}", jump.target_label);
            }
        }
    }
}

impl Default for BytecodeCompiler {
    fn default() -> Self {
        Self::new()
    }
}
