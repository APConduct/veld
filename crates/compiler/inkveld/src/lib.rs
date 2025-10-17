//! Veld Compiler - LLVM-based compiler for the Veld programming language
//!
//! This crate provides the core compilation infrastructure for Veld, including:
//! - LLVM code generation
//! - Type system integration
//! - Function compilation (traditional and lambda syntax)
//! - Basic optimizations
//! - Memory management

use llvm_sys::LLVMIntPredicate;
use llvm_sys::analysis::*;
use llvm_sys::bit_writer::*;
use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::ptr;

// Use existing Veld type system
use veld_common::types::Type;

/// Initialize LLVM targets and execution engine
pub fn initialize_llvm() {
    unsafe {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
        LLVMLinkInMCJIT();
    }
}

/// Extension trait to add LLVM conversion to existing Veld Type system
pub trait TypeToLLVM {
    /// Convert Veld type to LLVM type
    fn to_llvm_type(&self, context: LLVMContextRef) -> LLVMTypeRef;
}

impl TypeToLLVM for Type {
    fn to_llvm_type(&self, context: LLVMContextRef) -> LLVMTypeRef {
        unsafe {
            match self {
                Type::I32 => LLVMInt32TypeInContext(context),
                Type::I64 => LLVMInt64TypeInContext(context),
                Type::F32 => LLVMFloatTypeInContext(context),
                Type::F64 => LLVMDoubleTypeInContext(context),
                Type::Bool => LLVMInt1TypeInContext(context),
                Type::String => LLVMPointerType(LLVMInt8TypeInContext(context), 0),
                Type::Unit => LLVMVoidTypeInContext(context),
                Type::Char => LLVMInt8TypeInContext(context),
                Type::U32 => LLVMInt32TypeInContext(context),
                Type::U64 => LLVMInt64TypeInContext(context),
                Type::U8 => LLVMInt8TypeInContext(context),
                Type::U16 => LLVMInt16TypeInContext(context),
                Type::I8 => LLVMInt8TypeInContext(context),
                Type::I16 => LLVMInt16TypeInContext(context),
                Type::Union { variants } => {
                    let variant_types: Vec<LLVMTypeRef> =
                        variants.iter().map(|v| v.to_llvm_type(context)).collect();
                    LLVMStructTypeInContext(
                        context,
                        variant_types.as_ptr() as *mut LLVMTypeRef,
                        variant_types.len() as u32,
                        0,
                    )
                }
                Type::Function {
                    params,
                    return_type,
                } => {
                    let param_types: Vec<LLVMTypeRef> =
                        params.iter().map(|p| p.to_llvm_type(context)).collect();
                    let return_llvm_type = return_type.to_llvm_type(context);

                    LLVMPointerType(
                        LLVMFunctionType(
                            return_llvm_type,
                            param_types.as_ptr() as *mut LLVMTypeRef,
                            param_types.len() as u32,
                            0, // not variadic
                        ),
                        0,
                    )
                }
                Type::Array(element_type) => LLVMPointerType(element_type.to_llvm_type(context), 0),
                Type::Tuple(types) => {
                    let field_types: Vec<LLVMTypeRef> =
                        types.iter().map(|t| t.to_llvm_type(context)).collect();
                    LLVMStructTypeInContext(
                        context,
                        field_types.as_ptr() as *mut LLVMTypeRef,
                        field_types.len() as u32,
                        0, // not packed
                    )
                }
                Type::GenericFunction { .. }
                | Type::Generic { .. }
                | Type::TypeParam(_)
                | Type::TypeVar(_) => {
                    // Generic types should be resolved before code generation
                    panic!("Cannot convert unresolved generic type to LLVM: {:?}", self)
                }
                Type::IntegerLiteral(_) => LLVMInt64TypeInContext(context), // Default to i64
                Type::FloatLiteral(_) => LLVMDoubleTypeInContext(context),  // Default to f64
                Type::Number => LLVMDoubleTypeInContext(context),           // Default to f64
                Type::Any => LLVMPointerType(LLVMInt8TypeInContext(context), 0), // Void pointer
                Type::Struct { .. }
                | Type::Record { .. }
                | Type::Enum { .. }
                | Type::KindSelf(_)
                | Type::Module(_)
                | Type::StructType(_)
                | Type::EnumType(_) => {
                    // TODO: implement more complex handling in a full implementation
                    LLVMPointerType(LLVMInt8TypeInContext(context), 0) // Placeholder as void pointer
                }
            }
        }
    }
}

/// Variable binding in the current scope
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub veld_type: Type,
    pub llvm_value: LLVMValueRef,
    pub is_mutable: bool,
}

/// Function signature for Veld functions
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub llvm_function: Option<LLVMValueRef>,
}

/// Compilation context maintaining LLVM state and symbol tables
pub struct CompilerContext {
    pub llvm_context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub execution_engine: Option<LLVMExecutionEngineRef>,

    // Symbol tables
    pub variables: HashMap<String, Variable>,
    pub functions: HashMap<String, FunctionSignature>,

    // Scope management
    pub scope_stack: Vec<HashMap<String, Variable>>,

    // Current function being compiled
    pub current_function: Option<LLVMValueRef>,
}

impl CompilerContext {
    /// Create a new compiler context
    pub fn new(module_name: &str) -> Result<Self, String> {
        unsafe {
            let context = LLVMContextCreate();
            let module_name_cstr = CString::new(module_name).map_err(|_| "Invalid module name")?;
            let module = LLVMModuleCreateWithNameInContext(module_name_cstr.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            Ok(CompilerContext {
                llvm_context: context,
                module,
                builder,
                execution_engine: None,
                variables: HashMap::new(),
                functions: HashMap::new(),
                scope_stack: Vec::new(),
                current_function: None,
            })
        }
    }

    /// Enter a new scope
    pub fn push_scope(&mut self) {
        let current_vars = self.variables.clone();
        self.scope_stack.push(current_vars);
    }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        if let Some(previous_vars) = self.scope_stack.pop() {
            self.variables = previous_vars;
        }
    }

    /// Add a variable to the current scope
    pub fn add_variable(&mut self, name: String, var: Variable) {
        self.variables.insert(name, var);
    }

    /// Look up a variable in the current scope chain
    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
    }

    /// Add a function to the symbol table
    pub fn add_function(&mut self, name: String, sig: FunctionSignature) {
        self.functions.insert(name, sig);
    }

    /// Look up a function
    pub fn get_function(&self, name: &str) -> Option<&FunctionSignature> {
        self.functions.get(name)
    }

    /// Create an LLVM constant integer
    pub fn const_i32(&self, value: i32) -> LLVMValueRef {
        unsafe {
            LLVMConstInt(
                LLVMInt32TypeInContext(self.llvm_context),
                value as u64,
                1, // sign extend
            )
        }
    }

    /// Create an LLVM constant boolean
    pub fn const_bool(&self, value: bool) -> LLVMValueRef {
        unsafe {
            LLVMConstInt(
                LLVMInt1TypeInContext(self.llvm_context),
                if value { 1 } else { 0 },
                0,
            )
        }
    }

    /// Create an LLVM constant float
    pub fn const_f64(&self, value: f64) -> LLVMValueRef {
        unsafe { LLVMConstReal(LLVMDoubleTypeInContext(self.llvm_context), value) }
    }

    /// Create a string constant
    pub fn const_string(&self, value: &str) -> LLVMValueRef {
        unsafe {
            let string_cstr = CString::new(value).unwrap();
            LLVMConstStringInContext(
                self.llvm_context,
                string_cstr.as_ptr(),
                value.len() as u32,
                1, // don't null terminate (we handle this ourselves)
            )
        }
    }

    /// Declare a function (create signature without body)
    pub fn declare_function(
        &mut self,
        name: &str,
        params: Vec<(String, Type)>,
        return_type: Type,
    ) -> Result<LLVMValueRef, String> {
        // Create LLVM function type
        let param_types: Vec<LLVMTypeRef> = params
            .iter()
            .map(|(_, t)| t.to_llvm_type(self.llvm_context))
            .collect();
        let return_llvm_type = return_type.to_llvm_type(self.llvm_context);

        let function_type = unsafe {
            LLVMFunctionType(
                return_llvm_type,
                param_types.as_ptr() as *mut LLVMTypeRef,
                param_types.len() as u32,
                0, // not variadic
            )
        };

        // Create the function
        let function_name = CString::new(name).map_err(|_| "Invalid function name")?;
        let function =
            unsafe { LLVMAddFunction(self.module, function_name.as_ptr(), function_type) };

        // Add function to symbol table
        let sig = FunctionSignature {
            name: name.to_string(),
            params,
            return_type,
            llvm_function: Some(function),
        };
        self.add_function(name.to_string(), sig);

        Ok(function)
    }

    /// Compile a Veld function definition
    pub fn compile_function(
        &mut self,
        name: &str,
        params: Vec<(String, Type)>,
        return_type: Type,
        body: &Expression,
    ) -> Result<LLVMValueRef, String> {
        // Check if function is already declared, if not declare it
        let function = if let Some(sig) = self.get_function(name) {
            sig.llvm_function
                .ok_or("Function declared but not compiled")?
        } else {
            self.declare_function(name, params.clone(), return_type.clone())?
        };

        // Create entry block
        let entry_block_name = CString::new("entry").unwrap();
        let entry_block = unsafe {
            LLVMAppendBasicBlockInContext(self.llvm_context, function, entry_block_name.as_ptr())
        };

        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, entry_block);
        }

        // Set current function
        let previous_function = self.current_function;
        self.current_function = Some(function);

        // Push new scope for function parameters
        self.push_scope();

        // Add function parameters as variables
        for (i, (param_name, param_type)) in params.iter().enumerate() {
            let llvm_param = unsafe { LLVMGetParam(function, i as u32) };

            // Set parameter name
            let param_name_cstr = CString::new(param_name.as_str()).unwrap();
            unsafe {
                LLVMSetValueName2(llvm_param, param_name_cstr.as_ptr(), param_name.len());
            }

            let var = Variable {
                name: param_name.clone(),
                veld_type: param_type.clone(),
                llvm_value: llvm_param,
                is_mutable: false, // Parameters are immutable by default
            };

            self.add_variable(param_name.clone(), var);
        }

        // Compile function body
        let result = self.compile_expression(body)?;

        // Handle return value
        unsafe {
            if return_type == Type::Unit {
                LLVMBuildRetVoid(self.builder);
            } else {
                LLVMBuildRet(self.builder, result);
            }
        }

        // Restore previous state
        self.pop_scope();
        self.current_function = previous_function;

        Ok(function)
    }

    /// Compile a Veld expression to LLVM IR
    pub fn compile_expression(&mut self, expr: &Expression) -> Result<LLVMValueRef, String> {
        match expr {
            Expression::IntLiteral(value) => Ok(self.const_i32(*value)),
            Expression::FloatLiteral(value) => Ok(self.const_f64(*value)),
            Expression::BoolLiteral(value) => Ok(self.const_bool(*value)),
            Expression::StringLiteral(value) => Ok(self.const_string(value)),

            Expression::Variable(name) => self
                .get_variable(name)
                .map(|var| var.llvm_value)
                .ok_or_else(|| format!("Undefined variable: {}", name)),

            Expression::BinaryOp { left, op, right } => {
                let left_val = self.compile_expression(left)?;
                let right_val = self.compile_expression(right)?;
                self.compile_binary_op(left_val, op, right_val)
            }

            Expression::FunctionCall { name, args } => {
                let function = self
                    .get_function(name)
                    .ok_or_else(|| format!("Undefined function: {}", name))?;

                let llvm_function = function
                    .llvm_function
                    .ok_or_else(|| format!("Function {} not compiled", name))?;

                let arg_values: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.compile_expression(arg))
                    .collect();
                let arg_values = arg_values?;

                let call_name = CString::new(format!("{}_call", name)).unwrap();
                unsafe {
                    Ok(LLVMBuildCall2(
                        self.builder,
                        LLVMGlobalGetValueType(llvm_function),
                        llvm_function,
                        arg_values.as_ptr() as *mut LLVMValueRef,
                        arg_values.len() as u32,
                        call_name.as_ptr(),
                    ))
                }
            }

            Expression::IfThenElse {
                condition,
                then_expr,
                else_expr,
            } => {
                let condition_val = self.compile_expression(condition)?;

                let function = self
                    .current_function
                    .ok_or("If expression outside of function")?;

                // Create basic blocks
                let then_block_name = CString::new("then").unwrap();
                let else_block_name = CString::new("else").unwrap();
                let merge_block_name = CString::new("merge").unwrap();

                let then_block = unsafe {
                    LLVMAppendBasicBlockInContext(
                        self.llvm_context,
                        function,
                        then_block_name.as_ptr(),
                    )
                };
                let else_block = unsafe {
                    LLVMAppendBasicBlockInContext(
                        self.llvm_context,
                        function,
                        else_block_name.as_ptr(),
                    )
                };
                let merge_block = unsafe {
                    LLVMAppendBasicBlockInContext(
                        self.llvm_context,
                        function,
                        merge_block_name.as_ptr(),
                    )
                };

                // Branch based on condition
                unsafe {
                    LLVMBuildCondBr(self.builder, condition_val, then_block, else_block);
                }

                // Compile then branch
                unsafe {
                    LLVMPositionBuilderAtEnd(self.builder, then_block);
                }
                let then_val = self.compile_expression(then_expr)?;
                unsafe {
                    LLVMBuildBr(self.builder, merge_block);
                }

                // Compile else branch
                unsafe {
                    LLVMPositionBuilderAtEnd(self.builder, else_block);
                }
                let else_val = self.compile_expression(else_expr)?;
                unsafe {
                    LLVMBuildBr(self.builder, merge_block);
                }

                // Merge block with PHI node
                unsafe {
                    LLVMPositionBuilderAtEnd(self.builder, merge_block);
                }

                let phi_name = CString::new("if_result").unwrap();
                let phi_node = unsafe {
                    LLVMBuildPhi(
                        self.builder,
                        LLVMTypeOf(then_val), // Assuming both branches have same type
                        phi_name.as_ptr(),
                    )
                };

                let mut values = [then_val, else_val];
                let mut blocks = [then_block, else_block];
                unsafe {
                    LLVMAddIncoming(phi_node, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
                }

                Ok(phi_node)
            }

            Expression::Let { name, value, .. } => {
                let val = self.compile_expression(value)?;

                // For now, we'll create an alloca and store the value
                // This enables mutation later if needed
                let var_name = CString::new(name.as_str()).unwrap();
                let alloca =
                    unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(val), var_name.as_ptr()) };

                unsafe {
                    LLVMBuildStore(self.builder, val, alloca);
                }

                let var = Variable {
                    name: name.clone(),
                    veld_type: Type::I32, // TODO: Infer type properly
                    llvm_value: alloca,
                    is_mutable: true,
                };

                self.add_variable(name.clone(), var);
                Ok(val)
            }
        }
    }

    /// Compile binary operations
    fn compile_binary_op(
        &self,
        left: LLVMValueRef,
        op: &BinaryOperator,
        right: LLVMValueRef,
    ) -> Result<LLVMValueRef, String> {
        let op_name = CString::new("binop").unwrap();

        unsafe {
            match op {
                BinaryOperator::Add => {
                    Ok(LLVMBuildAdd(self.builder, left, right, op_name.as_ptr()))
                }
                BinaryOperator::Sub => {
                    Ok(LLVMBuildSub(self.builder, left, right, op_name.as_ptr()))
                }
                BinaryOperator::Mul => {
                    Ok(LLVMBuildMul(self.builder, left, right, op_name.as_ptr()))
                }
                BinaryOperator::Div => {
                    Ok(LLVMBuildSDiv(self.builder, left, right, op_name.as_ptr()))
                }
                BinaryOperator::Equal => Ok(LLVMBuildICmp(
                    self.builder,
                    LLVMIntPredicate::LLVMIntEQ,
                    left,
                    right,
                    op_name.as_ptr(),
                )),
                BinaryOperator::Less => Ok(LLVMBuildICmp(
                    self.builder,
                    LLVMIntPredicate::LLVMIntSLT,
                    left,
                    right,
                    op_name.as_ptr(),
                )),
                BinaryOperator::Greater => Ok(LLVMBuildICmp(
                    self.builder,
                    LLVMIntPredicate::LLVMIntSGT,
                    left,
                    right,
                    op_name.as_ptr(),
                )),
            }
        }
    }

    /// Optimize the module
    pub fn optimize(&self) -> Result<(), String> {
        unsafe {
            // Verify the module
            let mut error_msg: *mut i8 = ptr::null_mut();
            let result = LLVMVerifyModule(
                self.module,
                llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                &mut error_msg,
            );

            if result != 0 {
                let error_str = if !error_msg.is_null() {
                    CStr::from_ptr(error_msg).to_string_lossy().into_owned()
                } else {
                    "Unknown verification error".to_string()
                };
                return Err(error_str);
            }
        }
        Ok(())
    }

    /// Write bitcode to file
    pub fn write_bitcode(&self, filename: &str) -> Result<(), String> {
        let filename_cstr = CString::new(filename).map_err(|_| "Invalid filename")?;
        unsafe {
            let result = LLVMWriteBitcodeToFile(self.module, filename_cstr.as_ptr());
            if result != 0 {
                Err("Failed to write bitcode".to_string())
            } else {
                Ok(())
            }
        }
    }

    /// Print the module IR to string
    pub fn print_ir(&self) -> String {
        unsafe {
            let ir_ptr = LLVMPrintModuleToString(self.module);
            let ir_str = CStr::from_ptr(ir_ptr).to_string_lossy().into_owned();
            LLVMDisposeMessage(ir_ptr);
            ir_str
        }
    }
}

impl Drop for CompilerContext {
    fn drop(&mut self) {
        unsafe {
            if let Some(ee) = self.execution_engine {
                LLVMDisposeExecutionEngine(ee);
            }
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.llvm_context);
        }
    }
}

/// Abstract Syntax Tree for Veld expressions (simple version for compiler)
#[derive(Debug, Clone)]
pub enum Expression {
    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    Variable(String),
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    IfThenElse {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    Let {
        name: String,
        veld_type: Option<Type>,
        value: Box<Expression>,
    },
}

/// Binary operators in Veld
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    Less,
    Greater,
}

/// High-level compilation API
pub struct VeldCompiler {
    context: CompilerContext,
}

impl VeldCompiler {
    /// Create a new Veld compiler
    pub fn new(module_name: &str) -> Result<Self, String> {
        initialize_llvm();
        let context = CompilerContext::new(module_name)?;
        Ok(VeldCompiler { context })
    }

    /// Compile a Veld program (list of function definitions)
    pub fn compile_program(&mut self, functions: Vec<VeldFunctionDef>) -> Result<(), String> {
        // First pass: declare all functions to enable recursion
        for func in &functions {
            self.context.declare_function(
                &func.name,
                func.params.clone(),
                func.return_type.clone(),
            )?;
        }

        // Second pass: compile function bodies
        for func in functions {
            self.context
                .compile_function(&func.name, func.params, func.return_type, &func.body)?;
        }
        self.context.optimize()
    }

    /// Get the compiled IR as a string
    pub fn get_ir(&self) -> String {
        self.context.print_ir()
    }

    /// Write the compiled module to a bitcode file
    pub fn write_bitcode(&self, filename: &str) -> Result<(), String> {
        self.context.write_bitcode(filename)
    }
}

/// Veld function definition
#[derive(Debug, Clone)]
pub struct VeldFunctionDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Expression,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_compilation() {
        let mut compiler = VeldCompiler::new("test_module").unwrap();

        // Test compiling a simple function: fn add(x: i32, y: i32) -> i32 => x + y
        let add_func = VeldFunctionDef {
            name: "add".to_string(),
            params: vec![("x".to_string(), Type::I32), ("y".to_string(), Type::I32)],
            return_type: Type::I32,
            body: Expression::BinaryOp {
                left: Box::new(Expression::Variable("x".to_string())),
                op: BinaryOperator::Add,
                right: Box::new(Expression::Variable("y".to_string())),
            },
        };

        compiler
            .compile_program(vec![add_func])
            .expect("Compilation failed");

        // Print the generated IR
        println!("Generated IR:\n{}", compiler.get_ir());
    }

    #[test]
    fn test_factorial_compilation() {
        let mut compiler = VeldCompiler::new("factorial_module").unwrap();

        // Test recursive factorial function
        let factorial_func = VeldFunctionDef {
            name: "factorial".to_string(),
            params: vec![("n".to_string(), Type::I32)],
            return_type: Type::I32,
            body: Expression::IfThenElse {
                condition: Box::new(Expression::BinaryOp {
                    left: Box::new(Expression::Variable("n".to_string())),
                    op: BinaryOperator::Less,
                    right: Box::new(Expression::IntLiteral(2)),
                }),
                then_expr: Box::new(Expression::IntLiteral(1)),
                else_expr: Box::new(Expression::BinaryOp {
                    left: Box::new(Expression::Variable("n".to_string())),
                    op: BinaryOperator::Mul,
                    right: Box::new(Expression::FunctionCall {
                        name: "factorial".to_string(),
                        args: vec![Expression::BinaryOp {
                            left: Box::new(Expression::Variable("n".to_string())),
                            op: BinaryOperator::Sub,
                            right: Box::new(Expression::IntLiteral(1)),
                        }],
                    }),
                }),
            },
        };

        compiler
            .compile_program(vec![factorial_func])
            .expect("Compilation failed");
        println!("Factorial IR:\n{}", compiler.get_ir());
    }

    #[test]
    fn test_type_system() {
        let i32_type = Type::I32;
        let func_type = Type::Function {
            params: vec![Type::I32, Type::F64],
            return_type: Box::new(Type::Bool),
        };

        // Test that types can be created and compared
        assert_eq!(i32_type, Type::I32);
        assert_ne!(i32_type, Type::F64);

        if let Type::Function {
            params,
            return_type,
        } = func_type
        {
            assert_eq!(params.len(), 2);
            assert_eq!(*return_type, Type::Bool);
        }
    }

    #[test]
    fn test_llvm_initialization() {
        // Test that LLVM can be initialized without crashing
        initialize_llvm();

        // Create a basic context to ensure LLVM is working
        let context = CompilerContext::new("test").unwrap();
        let const_val = context.const_i32(42);
        assert!(!const_val.is_null());
    }
}
