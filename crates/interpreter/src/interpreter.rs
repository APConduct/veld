use crate::expand_macros::expand_macros_in_ast;
use crate::native::{NativeFunctionRegistry, NativeMethodRegistry};
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::ControlFlow;
use std::path::Path;
use std::path::PathBuf;
use std::vec::IntoIter;
use veld_common::ast::{
    Argument, BinaryOperator, EnumVariant, Expr, FunctionDeclaration, GenericArgument, ImportItem,
    Literal, MacroExpansion, MacroPattern, MatchArm, MatchPattern, MethodImpl, Statement,
    StructField, StructMethod, TypeAnnotation, UnaryOperator, VarKind,
};
use veld_common::types::Type;
use veld_common::types::checker::TypeChecker;
use veld_common::value::module::{ExportedItem, ModuleManager};
use veld_expander::MacroSystem;

use veld_common::value::numeric::{FloatValue, IntegerValue, NumericValue};
use veld_error::{Result, VeldError};

// pub mod value;
pub use super::scope::Scope;
pub use veld_common::value::Value;

#[derive(Debug)]
enum PatternToken {
    Literal(String),
    Variable(String),
    Repetition {
        variable: String,
        separator: Option<String>,
        min: usize, // 0 for * (zero or more), 1 for + (one or more)
    },
}

#[derive(Debug, Clone, Default)]
pub struct StructInfo {
    fields: Vec<(String, TypeAnnotation)>,
    generic_params: Vec<GenericArgument>,
}

impl StructInfo {
    pub fn with_fields(fields: Vec<(String, TypeAnnotation)>) -> Self {
        Self {
            fields,
            generic_params: Vec::new(),
        }
    }
    pub fn with_generic_params(
        fields: Vec<(String, TypeAnnotation)>,
        generic_params: Vec<GenericArgument>,
    ) -> Self {
        Self {
            fields,
            generic_params,
        }
    }
    pub fn from(
        fields: Vec<(String, TypeAnnotation)>,
        generic_params: Vec<GenericArgument>,
    ) -> Self {
        Self {
            fields,
            generic_params,
        }
    }

    pub fn fields(&self) -> &Vec<(String, TypeAnnotation)> {
        &self.fields
    }

    pub fn generic_params(&self) -> &Vec<GenericArgument> {
        &self.generic_params
    }

    pub fn generic_params_mut(&mut self) -> &mut Vec<GenericArgument> {
        &mut self.generic_params
    }
}

pub struct Interpreter {
    scopes: Vec<Scope>,
    structs: HashMap<String, Vec<StructField>>, // struct name -> fields
    struct_methods: HashMap<String, HashMap<String, Value>>, // struct name -> (method name -> method)
    generic_structs: HashMap<String, StructInfo>,
    module_manager: ModuleManager,
    current_module: String,
    imported_modules: HashMap<String, String>, // alias -> module name
    enums: HashMap<String, Vec<EnumVariant>>,
    enum_methods: HashMap<String, HashMap<String, MethodImpl>>, // enum name -> (method name -> method)
    pub type_checker: TypeChecker,
    native_registry: NativeFunctionRegistry,
    native_method_registry: NativeMethodRegistry,
    recursion_depth: usize,
    pub allocator: std::sync::RwLock<veld_common::gc::allocator::GcAllocator>,
    pub collector: std::sync::RwLock<veld_common::gc::collector::GcCollector>,
    pub gc_config: veld_common::gc::GcConfig,
}

use veld_common::gc::root_set::RootSet;

impl Interpreter {
    /// Collect all GC roots from interpreter state (scopes, etc.)
    pub fn collect_gc_roots(&self) -> RootSet {
        let mut roots = RootSet::new();

        // Collect roots from all scopes (stack frames)
        for scope in &self.scopes {
            for (_name, value) in scope.vals() {
                Self::add_value_roots(&mut roots, value);
            }
        }

        // Collect roots from global scope (scopes[0])
        if let Some(global_scope) = self.scopes.get(0) {
            for (_name, value) in global_scope.vals() {
                // Only add roots if value is a veld_common::value::Value
                if let Some(val) = value.downcast_ref::<veld_common::value::Value>() {
                    Self::add_value_roots(&mut roots, val);
                }
                // Otherwise, skip (cannot add roots for Expr)
            }
        }

        // Collect roots from closure captured_vars in all stack frames
        for scope in &self.scopes {
            for (_name, value) in scope.vals() {
                if let Value::Function { captured_vars, .. } = value {
                    for (_var_name, var_value) in captured_vars {
                        Self::add_value_roots(&mut roots, var_value);
                    }
                }
            }
        }

        // Collect roots from loaded modules (exports/globals)
        // NOTE: Skipping module variable roots unless you have a runtime Value store for modules.
        // If you add a global/module value environment, traverse those values here for GC roots.

        // Collect roots from imported modules (by alias)
        for (_alias, module_name) in &self.imported_modules {
            if let Some(module) = self.module_manager.get_module(module_name) {
                for export in module.exports.values() {
                    match export {
                        veld_common::value::module::ExportedItem::Variable(idx) => {
                            if let Some(veld_common::ast::Statement::VariableDeclaration {
                                value,
                                ..
                            }) = module.statements.get(*idx)
                            {
                                // Skipping: cannot add roots for Expr, only for runtime Value.
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        roots
    }

    fn add_value_roots(roots: &mut RootSet, value: &Value) {
        match value {
            Value::GcRef(handle) => {
                roots.add_stack_root(handle.clone()).ok();
            }
            Value::Array(elements) | Value::Tuple(elements) => {
                for v in elements {
                    Self::add_value_roots(roots, v);
                }
            }
            Value::Struct { fields, .. } => {
                for v in fields.values() {
                    Self::add_value_roots(roots, v);
                }
            }
            // Add other composite types as needed
            _ => {}
        }
    }
    pub fn new<P: AsRef<Path>>(root_dir: P) -> Self {
        let span = tracing::info_span!("interpreter_new", root_dir = ?root_dir.as_ref());
        let _enter = span.enter();

        let mut interpreter = Self {
            scopes: vec![Scope::new(0)],
            structs: HashMap::new(),
            struct_methods: HashMap::new(),
            generic_structs: HashMap::new(),
            module_manager: ModuleManager::new(root_dir),
            current_module: "main".to_string(),
            imported_modules: Default::default(),
            enums: HashMap::new(),
            type_checker: TypeChecker::new(),
            native_registry: NativeFunctionRegistry::new(),
            native_method_registry: NativeMethodRegistry::new(),
            recursion_depth: 0,
            enum_methods: HashMap::new(),
            gc_config: veld_common::gc::GcConfig::default(),
            allocator: std::sync::RwLock::new(veld_common::gc::allocator::GcAllocator::default()),
            collector: std::sync::RwLock::new(veld_common::gc::collector::GcCollector::default()),
        };

        interpreter.initialize_std_modules();

        interpreter
    }

    pub fn interpret_ast(&mut self, ast: veld_common::ast::AST) -> Result<Value> {
        // Expand macros in the AST before interpretation
        let mut macro_system = MacroSystem::new();
        let expanded_ast = expand_macros_in_ast(ast, &mut macro_system)
            .map_err(|e| VeldError::RuntimeError(format!("Macro expansion failed: {:?}", e)))?;

        self.interpret(expanded_ast.statements)
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<Value> {
        let span = tracing::info_span!("interpret", statement_count = statements.len());
        let _enter = span.enter();

        // First pass: execute imports and register all declarations
        for stmt in &statements {
            match stmt {
                Statement::ImportDeclaration { .. } => {
                    self.execute_statement(stmt.clone())?;
                }
                Statement::FunctionDeclaration { .. } => {
                    self.execute_statement(stmt.clone())?;
                }
                Statement::StructDeclaration { .. } => {
                    self.execute_statement(stmt.clone())?;
                }
                Statement::EnumDeclaration { .. } => {
                    self.execute_statement(stmt.clone())?;
                }
                Statement::PlexDeclaration { .. } => {
                    self.execute_statement(stmt.clone())?;
                }
                _ => {}
            }
        }

        // Now type check the program after imports and declarations are processed
        self.type_checker.check_program(&statements)?;

        // Second pass: execute everything else
        let mut last_value = Value::Unit;
        for stmt in statements {
            if !matches!(
                stmt,
                Statement::FunctionDeclaration { .. }
                    | Statement::ImportDeclaration { .. }
                    | Statement::StructDeclaration { .. }
                    | Statement::EnumDeclaration { .. }
                    | Statement::PlexDeclaration { .. }
            ) {
                last_value = self.execute_statement(stmt)?;
            }
        }

        Ok(last_value)
    }

    fn initialize_std_modules(&mut self) {
        let span = tracing::info_span!("initialize_std_modules");
        let _enter = span.enter();

        // Find and register the stdlib path
        let exe_path = std::env::current_exe().unwrap_or_default();
        let exe_dir = exe_path.parent().unwrap_or_else(|| Path::new("."));

        // Try several potential stdlib locations
        let possible_paths = [
            exe_dir.join("stdlib"),
            exe_dir.join("../stdlib"),
            PathBuf::from("./stdlib"),
            PathBuf::from("./veld/stdlib"),
            PathBuf::from("../stdlib"),
            PathBuf::from("../veld/stdlib"),
            PathBuf::from("../../veld/stdlib"),
        ];

        let mut found = false;
        for path in possible_paths {
            if path.exists() && path.is_dir() {
                if let Err(e) = self.module_manager.register_stdlib_path(&path) {
                    tracing::warn!("Failed to register stdlib path {}: {}", path.display(), e);
                } else {
                    found = true;
                    break;
                }
            }
        }

        if !found {
            tracing::warn!("Could not find stdlib directory");
        }

        // Load critical standard library modules with their types and methods
        self.load_stdlib_module_with_types(&["std"]); // Load main std module for ToString and other kinds
        self.load_stdlib_module_with_types(&["std", "option"]);
        self.load_stdlib_module_with_types(&["std", "vec"]);

        // Initialize native methods for built-in types
        self.initialize_core_capabilities();
        self.initialize_string_capabilities();
        self.initialize_numeric_capabilities();
        self.initialize_array_methods();

        // Register other native functions
        self.register_math_functions();
        self.register_io_functions();
        let _ = self.initialize_operator_kinds();

        // Ensure all type parameter scopes are cleaned up after stdlib initialization
        while self.type_checker.env().has_type_param_scopes() {
            self.type_checker.env().pop_type_param_scope();
        }
    }

    fn register_math_functions(&mut self) {
        // Square root
        self.native_registry.register("std.math.sqrt", |_, args| {
            if args.len() != 1 {
                return Err(VeldError::RuntimeError(
                    "sqrt requires exactly one argument".to_string(),
                ));
            }

            match &args[0] {
                Value::Float(f) => Ok(Value::Float(f.sqrt())),
                Value::Integer(i) => {
                    let f = *i as f64;
                    Ok(Value::Float(f.sqrt()))
                }
                Value::Numeric(num) => {
                    // Handle your NumericValue type
                    match num {
                        NumericValue::Float(FloatValue::F64(f)) => Ok(Value::Float(f.sqrt())),
                        NumericValue::Float(FloatValue::F32(f)) => {
                            let f64_val = *f as f64;
                            Ok(Value::Float(f64_val.sqrt()))
                        }
                        NumericValue::Integer(i) => {
                            let f = i.clone().as_f64();
                            Ok(Value::Float(f.sqrt()))
                        }
                    }
                }
                _ => Err(VeldError::RuntimeError(
                    "sqrt requires a numeric argument".to_string(),
                )),
            }
        });

        // Power function
        self.native_registry.register("std.math.pow", |_, args| {
            if args.len() != 2 {
                return Err(VeldError::RuntimeError(
                    "pow requires exactly two arguments".to_string(),
                ));
            }

            let base = match &args[0] {
                Value::Float(f) => *f,
                Value::Integer(i) => *i as f64,
                Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => *f,
                Value::Numeric(NumericValue::Float(FloatValue::F32(f))) => *f as f64,
                Value::Numeric(NumericValue::Integer(i)) => i.clone().as_f64(),
                _ => {
                    return Err(VeldError::RuntimeError(
                        "pow base must be numeric".to_string(),
                    ));
                }
            };

            let exponent = match &args[1] {
                Value::Float(f) => *f,
                Value::Integer(i) => *i as f64,
                Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => *f,
                Value::Numeric(NumericValue::Float(FloatValue::F32(f))) => *f as f64,
                Value::Numeric(NumericValue::Integer(i)) => i.clone().as_f64(),
                _ => {
                    return Err(VeldError::RuntimeError(
                        "pow exponent must be numeric".to_string(),
                    ));
                }
            };

            Ok(Value::Float(base.powf(exponent)))
        });

        // Sine function
        self.native_registry.register("std.math.sin", |_, args| {
            if args.len() != 1 {
                return Err(VeldError::RuntimeError(
                    "sin requires exactly one argument".to_string(),
                ));
            }

            let val = match &args[0] {
                Value::Float(f) => *f,
                Value::Integer(i) => *i as f64,
                Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => *f,
                Value::Numeric(NumericValue::Float(FloatValue::F32(f))) => *f as f64,
                Value::Numeric(NumericValue::Integer(i)) => i.clone().as_f64(),
                _ => {
                    return Err(VeldError::RuntimeError(
                        "sin argument must be numeric".to_string(),
                    ));
                }
            };

            Ok(Value::Float(val.sin()))
        });

        // Cosine function
        self.native_registry.register("std.math.cos", |_, args| {
            if args.len() != 1 {
                return Err(VeldError::RuntimeError(
                    "cos requires exactly one argument".to_string(),
                ));
            }

            let val = match &args[0] {
                Value::Float(f) => *f,
                Value::Integer(i) => *i as f64,
                Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => *f,
                Value::Numeric(NumericValue::Float(FloatValue::F32(f))) => *f as f64,
                Value::Numeric(NumericValue::Integer(i)) => i.clone().as_f64(),
                _ => {
                    return Err(VeldError::RuntimeError(
                        "cos argument must be numeric".to_string(),
                    ));
                }
            };

            Ok(Value::Float(val.cos()))
        });

        self.native_registry.register("std.math.tan", |_, args| {
            if args.len() != 1 {
                return Err(VeldError::RuntimeError(
                    "tan requires exactly one argument".to_string(),
                ));
            }

            let val = match &args[0] {
                Value::Float(f) => *f,
                Value::Integer(i) => *i as f64,
                Value::Numeric(NumericValue::Float(FloatValue::F64(f))) => *f,
                Value::Numeric(NumericValue::Float(FloatValue::F32(f))) => *f as f64,
                Value::Numeric(NumericValue::Integer(i)) => i.clone().as_f64(),
                _ => {
                    return Err(VeldError::RuntimeError(
                        "tan argument must be numeric".to_string(),
                    ));
                }
            };

            Ok(Value::Float(val.tan()))
        });

        // TODO: Add more math functions as needed
    }

    fn io_print(&self, args: Vec<Value>) -> Result<Value> {
        if let Some(value) = args.get(0) {
            let actual_value = match value {
                Value::GcRef(handle) => {
                    let allocator = self.allocator.read().unwrap();
                    allocator
                        .get_value(handle)
                        .expect("dangling GC handle")
                        .clone()
                }
                _ => value.clone(),
            };
            match self.value_to_string(&actual_value) {
                Ok(s) => {
                    print!("{}", s);
                    use std::io::{self, Write};
                    io::stdout().flush().unwrap();
                    Ok(Value::Unit)
                }
                Err(e) => Err(e),
            }
        } else {
            Err(VeldError::RuntimeError(
                "print() requires an argument".to_string(),
            ))
        }
    }

    fn io_println(&self, args: Vec<Value>) -> Result<Value> {
        if args.is_empty() {
            println!();
            return Ok(Value::Unit);
        }

        if let Some(value) = args.get(0) {
            let actual_value = match value {
                Value::GcRef(handle) => {
                    let allocator = self.allocator.read().unwrap();
                    allocator
                        .get_value(handle)
                        .expect("dangling GC handle")
                        .clone()
                }
                _ => value.clone(),
            };
            match self.value_to_string(&actual_value) {
                Ok(s) => {
                    println!("{}", s);
                    Ok(Value::Unit)
                }
                Err(e) => Err(e),
            }
        } else {
            Err(VeldError::RuntimeError(
                "println() requires an argument".to_string(),
            ))
        }
    }

    fn io_format_string(&self, args: Vec<Value>) -> Result<Value> {
        if args.len() < 1 {
            return Err(VeldError::RuntimeError(
                "_format_string requires at least a format string".to_string(),
            ));
        }

        if let Value::String(format_str) = &args[0] {
            let mut result = String::new();
            let mut args_index = 1;

            let mut chars = format_str.chars().peekable();
            while let Some(c) = chars.next() {
                if c == '{' && chars.peek() == Some(&'}') {
                    // Found a {} placeholder
                    chars.next(); // Skip the closing }

                    if args_index < args.len() {
                        // Get the value to insert
                        let value = &args[args_index];
                        args_index += 1;

                        // Convert value to string
                        match self.value_to_string(value) {
                            Ok(s) => {
                                result.push_str(&s);
                            }
                            Err(e) => return Err(e),
                        }
                    } else {
                        // No more arguments
                        return Err(VeldError::RuntimeError(
                            "Not enough arguments for format string".to_string(),
                        ));
                    }
                } else {
                    // Regular character
                    result.push(c);
                }
            }

            // Check if we used all arguments
            if args_index < args.len() {
                return Err(VeldError::RuntimeError(
                    "Too many arguments for format string".to_string(),
                ));
            }

            let gc_ref = self
                .allocator
                .write()
                .unwrap()
                .allocate(Value::String(result))?;

            // Trigger GC after allocation
            let root_set = self.collect_gc_roots();
            self.collector.write().unwrap().collect(
                veld_common::gc::collector::CollectionStrategy::Incremental,
                &mut self.allocator.write().unwrap(),
                &root_set,
                &self.gc_config,
            );

            Ok(Value::GcRef(gc_ref))
        } else {
            Err(VeldError::RuntimeError(
                "Format string must be a string".to_string(),
            ))
        }
    }

    fn register_io_functions(&mut self) {
        // Register IO functions using closures that take &mut self if needed

        self.native_registry
            .register("std.io.print", |interpreter, args| {
                interpreter.io_print(args)
            });
        self.native_registry
            .register("std.io.println", |interpreter, args| {
                interpreter.io_println(args)
            });

        // io_format_string can use &self, so register with the standard API
        self.native_registry
            .register("std.io._format_string", |interpreter, args| {
                interpreter.io_format_string(args)
            });

        // File operations
        self.native_registry
            .register("std.io.read_file", |_, args| {
                if let Some(Value::String(path)) = args.get(0) {
                    match std::fs::read_to_string(path) {
                        Ok(content) => Ok(Value::String(content)),
                        Err(e) => Err(VeldError::RuntimeError(format!(
                            "Failed to read file '{}': {}",
                            path, e
                        ))),
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        "read_file() requires a string path argument".to_string(),
                    ))
                }
            });

        self.native_registry
            .register("std.io.write_file", |_, args| {
                if let (Some(Value::String(path)), Some(Value::String(content))) =
                    (args.get(0), args.get(1))
                {
                    match std::fs::write(path, content) {
                        Ok(_) => Ok(Value::Boolean(true)),
                        Err(e) => Err(VeldError::RuntimeError(format!(
                            "Failed to write to file '{}': {}",
                            path, e
                        ))),
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        "write_file() requires path and content string arguments".to_string(),
                    ))
                }
            });

        self.native_registry
            .register("std.io.file_exists", |_, args| {
                if let Some(Value::String(path)) = args.get(0) {
                    Ok(Value::Boolean(std::path::Path::new(path).exists()))
                } else {
                    Err(VeldError::RuntimeError(
                        "file_exists() requires a string path argument".to_string(),
                    ))
                }
            });

        // Read from standard input
        self.native_registry
            .register("std.io.read_line", |_, _args| {
                let mut input = String::new();
                match std::io::stdin().read_line(&mut input) {
                    Ok(_) => {
                        // Trim the trailing newline
                        if input.ends_with('\n') {
                            input.pop();
                            if input.ends_with('\r') {
                                input.pop();
                            }
                        }
                        Ok(Value::String(input))
                    }
                    Err(e) => Err(VeldError::RuntimeError(format!(
                        "Failed to read from stdin: {}",
                        e
                    ))),
                }
            });
    }

    fn collect_free_variables_expr(
        &self,
        expr: &Expr,
        bound_vars: &HashSet<String>,
        free_vars: &mut HashSet<String>,
    ) {
        match expr {
            Expr::SelfReference => {
                // 'self' is always bound in a method context, so it does not contribute to free variables.
            }
            Expr::MacroVar(_name) => {
                // Macro variables are not considered free variables in this context.
            }
            Expr::Identifier(name) => {
                if !bound_vars.contains(name) {
                    free_vars.insert(name.clone());
                }
            }
            Expr::BinaryOp {
                left,
                operator: _,
                right,
            } => {
                self.collect_free_variables_expr(left, bound_vars, free_vars);
                self.collect_free_variables_expr(right, bound_vars, free_vars);
            }
            Expr::FunctionCall { name: _, arguments } => {
                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.collect_free_variables_expr(expr, bound_vars, free_vars);
                        }
                        Argument::Named { name: _, value } => {
                            self.collect_free_variables_expr(value, bound_vars, free_vars);
                        }
                    }
                }
            }
            Expr::Lambda {
                params,
                body,
                return_type: _,
                generic_params: _,
            } => {
                let mut lambda_bound = bound_vars.clone();
                for (param_name, _) in params {
                    lambda_bound.insert(param_name.clone());
                }
                self.collect_free_variables_expr(body, &lambda_bound, free_vars);
            }
            Expr::BlockLambda {
                params,
                body,
                return_type: _,
                generic_params: _,
            } => {
                let mut lambda_bound = bound_vars.clone();
                for (param_name, _) in params {
                    lambda_bound.insert(param_name.clone());
                }
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, &lambda_bound, free_vars);
                }
            }
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_free_variables_expr(condition, bound_vars, free_vars);
                self.collect_free_variables_expr(then_expr, bound_vars, free_vars);
                if let Some(else_expr) = else_expr {
                    self.collect_free_variables_expr(else_expr, bound_vars, free_vars);
                }
            }
            Expr::BlockExpression {
                statements,
                final_expr,
            } => {
                let mut block_bound = bound_vars.clone();
                for stmt in statements {
                    // Add any variables declared in this block to bound_vars
                    if let Statement::VariableDeclaration { name, .. } = stmt {
                        block_bound.insert(name.clone());
                    }
                    self.collect_free_variables_stmt(stmt, &block_bound, free_vars);
                }
                if let Some(final_expr) = final_expr {
                    self.collect_free_variables_expr(final_expr, &block_bound, free_vars);
                }
            }
            Expr::MethodCall {
                object,
                method: _,
                arguments,
            } => {
                self.collect_free_variables_expr(object, bound_vars, free_vars);
                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.collect_free_variables_expr(expr, bound_vars, free_vars);
                        }
                        Argument::Named { name: _, value } => {
                            self.collect_free_variables_expr(value, bound_vars, free_vars);
                        }
                    }
                }
            }
            Expr::PropertyAccess {
                object,
                property: _,
            } => {
                self.collect_free_variables_expr(object, bound_vars, free_vars);
            }
            Expr::StructCreate {
                struct_name: _,
                fields,
            } => {
                for (_, expr) in fields {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::Record { fields: _fields } => todo!("Implement record field collection"),
            Expr::ArrayLiteral(exprs) => {
                for expr in exprs {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::IndexAccess { object, index } => {
                self.collect_free_variables_expr(object, bound_vars, free_vars);
                self.collect_free_variables_expr(index, bound_vars, free_vars);
            }
            Expr::EnumVariant {
                enum_name: _,
                variant_name: _,
                fields,
                type_args: _,
            } => {
                for expr in fields {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::TupleLiteral(exprs) => {
                for expr in exprs {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::TupleAccess { tuple, index: _ } => {
                self.collect_free_variables_expr(tuple, bound_vars, free_vars);
            }
            Expr::MacroExpr { name: _, arguments } => {
                for expr in arguments {
                    self.collect_free_variables_expr(expr, bound_vars, free_vars);
                }
            }
            Expr::TypeCast {
                expr,
                target_type: _,
            } => {
                self.collect_free_variables_expr(expr, bound_vars, free_vars);
            }
            // Literals don't reference variables
            Expr::Literal(_) | Expr::UnitLiteral => {}
            Expr::UnaryOp {
                operator: _,
                operand,
            } => {
                // Collect free variables from the operand
                self.collect_free_variables_expr(operand, bound_vars, free_vars);
            }
            Expr::Call { callee, arguments } => {
                // Collect free variables from the callee
                self.collect_free_variables_expr(callee, bound_vars, free_vars);
                // Collect free variables from each argument
                for arg in arguments {
                    match arg {
                        Argument::Positional(expr) => {
                            self.collect_free_variables_expr(expr, bound_vars, free_vars);
                        }
                        Argument::Named { name: _, value } => {
                            self.collect_free_variables_expr(value, bound_vars, free_vars);
                        }
                    }
                }
            }
            Expr::Range {
                start,
                end,
                inclusive: _,
            } => {
                // Collect free variables from start and end expressions
                if let Some(start_expr) = start {
                    self.collect_free_variables_expr(start_expr, bound_vars, free_vars);
                }
                if let Some(end_expr) = end {
                    self.collect_free_variables_expr(end_expr, bound_vars, free_vars);
                }
            }
            Expr::Match { value, arms } => {
                // Collect free variables from the matched value
                self.collect_free_variables_expr(value, bound_vars, free_vars);

                // Collect free variables from each match arm
                for arm in arms {
                    // TODO: Handle pattern bindings in arm.pat to update bound_vars
                    // For now, just collect from the arm body
                    self.collect_free_variables_expr(&arm.body, bound_vars, free_vars);

                    // Collect from guard expression if present
                    if let Some(guard) = &arm.guard {
                        self.collect_free_variables_expr(guard, bound_vars, free_vars);
                    }
                }
            }
        }
    }

    fn get_current_module(&self) -> &str {
        &self.current_module
    }

    /// Collect free variables in a statement
    fn collect_free_variables_stmt(
        &self,
        stmt: &Statement,
        bound_vars: &HashSet<String>,
        free_vars: &mut HashSet<String>,
    ) {
        match stmt {
            Statement::ExprStatement(expr) => {
                self.collect_free_variables_expr(expr, bound_vars, free_vars);
            }
            Statement::VariableDeclaration {
                name: _,
                var_kind: _,
                type_annotation: _,
                value,
                ..
            } => {
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::Assignment { name: _, value } => {
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::PropertyAssignment { target, value, .. } => {
                self.collect_free_variables_expr(target, bound_vars, free_vars);
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::CompoundAssignment {
                name: _,
                operator: _,
                value,
            } => {
                self.collect_free_variables_expr(value, bound_vars, free_vars);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_free_variables_expr(condition, bound_vars, free_vars);
                for stmt in then_branch {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                    }
                }
            }
            Statement::While { condition, body } => {
                self.collect_free_variables_expr(condition, bound_vars, free_vars);
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
            }
            Statement::For {
                iterator: _,
                iterable,
                body,
            } => {
                self.collect_free_variables_expr(iterable, bound_vars, free_vars);
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
            }
            Statement::Return(Some(expr)) => {
                self.collect_free_variables_expr(expr, bound_vars, free_vars);
            }
            Statement::BlockScope { body } => {
                for stmt in body {
                    self.collect_free_variables_stmt(stmt, bound_vars, free_vars);
                }
            }
            // Other statements don't reference variables in ways that matter for closures
            _ => {}
        }
    }

    /// Capture the current values of free variables
    fn capture_variables(&self, free_vars: &HashSet<String>) -> HashMap<String, Value> {
        let mut captured = HashMap::new();
        for var_name in free_vars {
            if let Some(value) = self.get_variable(var_name) {
                captured.insert(var_name.clone(), value);
            }
        }
        captured
    }

    /// Create a lambda with captured variables
    fn create_lambda(
        &self,
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Box<Expr>,
        return_type: Option<TypeAnnotation>,
    ) -> Value {
        // Collect parameter names as bound variables
        let mut bound_vars = HashSet::new();
        for (param_name, _) in &params {
            bound_vars.insert(param_name.clone());
        }

        // Find free variables in the lambda body
        let mut free_vars = HashSet::new();
        self.collect_free_variables_expr(&body, &bound_vars, &mut free_vars);

        // Start a span for lambda creation
        let span = tracing::debug_span!(
            "create_lambda",
            params = ?params.iter().map(|(n, _)| n).collect::<Vec<_>>(),
            free_var_count = free_vars.len()
        );
        let _enter = span.enter();

        // Capture the current values of free variables
        let captured_vars = self.capture_variables(&free_vars);

        let actual_return_type = match return_type {
            Some(type_anno) => type_anno,
            None => {
                // Try to infer return type from body
                match body.as_ref() {
                    Expr::Literal(Literal::String(_)) => TypeAnnotation::Basic("str".to_string()),
                    Expr::Literal(Literal::Integer(_)) => TypeAnnotation::Basic("i32".to_string()),
                    Expr::Literal(Literal::Float(_)) => TypeAnnotation::Basic("f64".to_string()),
                    Expr::Literal(Literal::Boolean(_)) => TypeAnnotation::Basic("bool".to_string()),
                    Expr::Literal(Literal::Char(_)) => TypeAnnotation::Basic("char".to_string()),
                    _ => TypeAnnotation::Basic("infer".to_string()),
                }
            }
        };

        Value::Function {
            params: params
                .into_iter()
                .map(|(name, type_anno)| (name, type_anno.unwrap_or(TypeAnnotation::Unit)))
                .collect(),
            body: vec![Statement::Return(Some(*body))],
            return_type: actual_return_type,
            captured_vars,
        }
    }

    /// Create a block lambda with captured variables
    fn create_block_lambda(
        &self,
        params: Vec<(String, Option<TypeAnnotation>)>,
        body: Vec<Statement>,
        return_type: Option<TypeAnnotation>,
    ) -> Value {
        // Collect parameter names as bound variables
        let mut bound_vars = HashSet::new();
        for (param_name, _) in &params {
            bound_vars.insert(param_name.clone());
        }

        // Find free variables in the lambda body
        let mut free_vars = HashSet::new();
        for stmt in &body {
            self.collect_free_variables_stmt(stmt, &bound_vars, &mut free_vars);
        }

        // Start a span for block lambda creation
        let span = tracing::debug_span!(
            "create_block_lambda",
            params = ?params.iter().map(|(n, _)| n).collect::<Vec<_>>(),
            free_var_count = free_vars.len()
        );
        let _enter = span.enter();

        // Capture the current values of free variables
        let captured_vars = self.capture_variables(&free_vars);

        let actual_return_type = return_type.unwrap_or(TypeAnnotation::Unit);

        Value::Function {
            params: params
                .into_iter()
                .map(|(name, type_anno)| (name, type_anno.unwrap_or(TypeAnnotation::Unit)))
                .collect(),
            body,
            return_type: actual_return_type,
            captured_vars,
        }
    }

    fn value_to_expr(&self, value: Value) -> Result<Expr> {
        match value {
            Value::Integer(n) => Ok(Expr::Literal(Literal::Integer(n))),
            Value::Float(f) => Ok(Expr::Literal(Literal::Float(f))),
            Value::String(s) => Ok(Expr::Literal(Literal::String(s))),
            Value::Boolean(b) => Ok(Expr::Literal(Literal::Boolean(b))),
            Value::Char(c) => Ok(Expr::Literal(Literal::Char(c))),
            Value::Unit => Ok(Expr::Literal(Literal::Unit)),
            Value::GcRef(handle) => {
                let allocator = self.allocator.read().unwrap();
                let actual_value = allocator.get_value(&handle).expect("dangling GC handle");
                self.value_to_expr(actual_value.clone())
            }
            Value::Numeric(num) => match num {
                NumericValue::Integer(int_val) => match int_val {
                    IntegerValue::I8(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                    IntegerValue::I16(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                    IntegerValue::I32(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                    IntegerValue::I64(i) => Ok(Expr::Literal(Literal::Integer(i))),
                    IntegerValue::U8(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                    IntegerValue::U16(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                    IntegerValue::U32(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                    IntegerValue::U64(i) => Ok(Expr::Literal(Literal::Integer(i as i64))),
                },
                NumericValue::Float(float_val) => match float_val {
                    FloatValue::F32(f) => Ok(Expr::Literal(Literal::Float(f as f64))),
                    FloatValue::F64(f) => Ok(Expr::Literal(Literal::Float(f))),
                },
            },
            Value::Array(elements) => {
                let mut exprs = Vec::new();
                for elem in elements {
                    exprs.push(self.value_to_expr(elem)?);
                }
                Ok(Expr::ArrayLiteral(exprs))
            }
            Value::Tuple(elems) => {
                let mut exprs = Vec::new();
                for elem in elems {
                    exprs.push(self.value_to_expr(elem)?);
                }
                Ok(Expr::TupleLiteral(exprs))
            }
            _ => Err(VeldError::RuntimeError(format!(
                "Cannot convert value to expression: {:?}",
                value
            ))),
        }
    }
}
struct BlockScope1 {
    stmts: IntoIter<Statement>,
    last_value: Result<Value>,
}

struct If1 {
    stmts: IntoIter<Statement>,
    last_value: Value,
}

enum Frame {
    BlockScope1(BlockScope1),
    If1(If1),
}
impl Interpreter {
    fn execute_statement(&mut self, statement: Statement) -> Result<Value> {
        let span = tracing::debug_span!("execute_statement", statement = ?statement);
        let _enter = span.enter();

        let mut stack = Vec::new();

        use std::ops::ControlFlow;
        let mut action: ControlFlow<Result<Value>, Statement> = ControlFlow::Continue(statement);
        loop {
            action = 'continue_case: {
                ControlFlow::Break(match action {
                    ControlFlow::Break(value) => {
                        if let Some(frame) = stack.pop() {
                            let result = value?;

                            match frame {
                                Frame::BlockScope1(BlockScope1 {
                                    mut stmts,
                                    mut last_value,
                                }) => match result {
                                    r @ (Value::Return(_) | Value::Break | Value::Continue) => {
                                        self.pop_scope();
                                        Ok(r)
                                    }
                                    r => {
                                        last_value = Ok(r);
                                        if let Some(stmt) = stmts.next() {
                                            stack.push(Frame::BlockScope1(BlockScope1 {
                                                stmts,
                                                last_value,
                                            }));
                                            break 'continue_case ControlFlow::Continue(stmt);
                                        } else {
                                            self.pop_scope();
                                            // When block is done, return the last_value
                                            break 'continue_case ControlFlow::Break(last_value);
                                        }
                                    }
                                },
                                Frame::If1(If1 {
                                    mut stmts,
                                    mut last_value,
                                }) => {
                                    if matches!(result, Value::Return(_)) {
                                        Ok(result)
                                    } else if let Some(stmt) = stmts.next() {
                                        // Update last_value with the result of the previous statement
                                        last_value = result;
                                        stack.push(Frame::If1(If1 { stmts, last_value }));
                                        break 'continue_case ControlFlow::Continue(stmt);
                                    } else {
                                        // When done, return the last_value instead of Unit
                                        Ok(last_value)
                                    }
                                }
                            }
                        } else {
                            return value;
                        }
                    }
                    ControlFlow::Continue(statement) => match statement {
                        Statement::VariableDeclaration {
                            name,
                            var_kind,
                            type_annotation,
                            value,
                            ..
                        } => self.execute_variable_declaration(
                            name,
                            var_kind,
                            type_annotation,
                            value,
                        ),
                        Statement::FunctionDeclaration {
                            name,
                            params,
                            return_type,
                            body,
                            ..
                        } => self.execute_function_declaration(name, params, &return_type, body),
                        Statement::ModuleDeclaration {
                            name,
                            body,
                            is_public,
                        } => self.execute_module_declaration(name, body, is_public),
                        Statement::ImportDeclaration {
                            path,
                            items,
                            alias,
                            is_public,
                        } => self.execute_import_declaration(path, items, alias, is_public),
                        Statement::CompoundAssignment {
                            name,
                            operator,
                            value,
                        } => self.execute_compound_assignment(name, operator, value)?,
                        Statement::EnumDeclaration { name, variants, .. } => {
                            self.enums.insert(name.clone(), variants);
                            // Register the enum type in the environment so Color.Red works
                            self.current_scope_mut().set(
                                name.clone(),
                                Value::EnumType {
                                    name: name.clone(),
                                    methods: None,
                                },
                            );
                            Ok(Value::Unit)
                        }
                        Statement::Break => Ok(Value::Break),
                        Statement::Continue => Ok(Value::Continue),
                        Statement::Match { value, arms } => self.execute_match(value, arms),
                        Statement::Assignment { name, value } => {
                            self.execute_assignment(name, value)
                        }
                        Statement::PropertyAssignment {
                            target,
                            operator,
                            value,
                        } => self.execute_property_assignment(target, &operator, value),
                        Statement::MacroInvocation { name, arguments } => {
                            self.execute_macro_invocation(&name, arguments)?
                        }
                        Statement::Return(expr_opt) => self.execute_return(expr_opt)?,
                        Statement::ProcDeclaration {
                            name, params, body, ..
                        } => self.execute_proc_declaration(name, params, body),
                        Statement::StructDeclaration {
                            name,
                            fields,
                            methods,
                            generic_params,
                            is_public: _,
                        } => self.execute_struct_declaration(name, fields, methods, generic_params),
                        Statement::Implementation {
                            type_name,
                            kind_name,
                            methods,
                            generic_args,
                            where_clause: _,
                        } => self.execute_trait_implementation(
                            type_name,
                            kind_name,
                            generic_args,
                            methods,
                        ),

                        Statement::InherentImpl {
                            type_name,
                            generic_params,
                            methods,
                        } => {
                            self.execute_inherent_implementation(type_name, generic_params, methods)
                        }

                        Statement::PlexDeclaration {
                            name,
                            type_annotation,
                            is_public: _,
                            generic_params,
                        } => self.execute_plex_declaration(name, type_annotation, generic_params),
                        Statement::ExprStatement(expr) => {
                            let value = self.evaluate_expression(expr.clone())?;
                            tracing::debug!(
                                target: "veld.mutself",
                                "ExprStatement evaluated: expr={:?}, value={:?}",
                                expr,
                                value
                            );
                            Ok(value.unwrap_return())
                        }

                        // Recursive cases
                        Statement::BlockScope { body } => {
                            let span =
                                tracing::debug_span!("block_scope", statement_count = body.len());
                            let _enter = span.enter();

                            // Create new scope for the block
                            self.push_scope();

                            // loop {
                            break 'continue_case self.block_flow(
                                &mut stack,
                                Ok(Value::Unit),
                                body.into_iter(),
                            );
                            // Block scope 1
                        }

                        Statement::If {
                            condition,
                            then_branch,
                            else_branch,
                        } => {
                            let cond_value = self.evaluate_expression(condition)?.unwrap_return();

                            let branch = if self.is_truthy(cond_value) {
                                then_branch
                            } else if let Some(else_statements) = else_branch {
                                else_statements
                            } else {
                                break 'continue_case ControlFlow::Break(Ok(Value::Unit));
                            };
                            // Execute all statements in the chosen branch, propagating the value of the last executed statement
                            let mut last_value = Value::Unit;
                            for stmt in branch {
                                let stmt_result = self.execute_statement(stmt)?;
                                if matches!(stmt_result, Value::Return(_)) {
                                    last_value = stmt_result;
                                    break;
                                }
                                last_value = stmt_result;
                            }
                            Ok(last_value)
                        }
                        Statement::While { condition, body } => {
                            // Save the parent scope index
                            let _parent_scope_index = self.scopes.len() - 1;

                            // Track sticky variables and their values
                            let mut sticky_vars: Option<Vec<String>> = None;
                            let mut sticky_vals: HashMap<String, Value> = HashMap::new();

                            'outer: loop {
                                // Push a new scope for the loop body
                                self.push_scope();

                                // If we have sticky values from a previous iteration, initialize them
                                for (name, val) in &sticky_vals {
                                    self.current_scope_mut().set(name.clone(), val.clone());
                                }

                                // Evaluate the condition in the parent scope
                                let cond_result =
                                    self.evaluate_expression(condition.clone())?.unwrap_return();
                                if !self.is_truthy(cond_result) {
                                    self.pop_scope();
                                    break;
                                }

                                // On the first iteration, collect declared variable names
                                if sticky_vars.is_none() {
                                    let mut vars = Vec::new();
                                    for stmt in &body {
                                        if let Statement::VariableDeclaration { name, .. } = stmt {
                                            vars.push(name.clone());
                                        }
                                    }
                                    sticky_vars = Some(vars);
                                }

                                // Execute the loop body
                                for stmt in body.clone() {
                                    let result = self.execute_statement(stmt)?;
                                    match result {
                                        Value::Return(_) => {
                                            self.pop_scope();
                                            return Ok(result);
                                        }
                                        Value::Break => {
                                            self.pop_scope();
                                            break 'outer;
                                        }
                                        Value::Continue => break,
                                        _ => {}
                                    }
                                }

                                // At the end of the iteration, update sticky_vals from the current scope
                                if let Some(vars) = &sticky_vars {
                                    for name in vars {
                                        if let Some(val) = self.current_scope().get(name) {
                                            sticky_vals.insert(name.clone(), val.clone());
                                        }
                                    }
                                }

                                self.pop_scope();
                            }
                            Ok(Value::Unit)
                        }
                        Statement::For {
                            iterator,
                            iterable,
                            body,
                        } => {
                            let iterable_value =
                                self.evaluate_expression(iterable.clone())?.unwrap_return();

                            match iterable_value {
                                Value::Array(elements) => {
                                    let mut inner_loop = None;
                                    let mut elements = elements.into_iter();
                                    loop {
                                        inner_loop = match inner_loop {
                                            None => match elements.next() {
                                                None => break,
                                                Some(element) => {
                                                    self.current_scope_mut()
                                                        .set(iterator.clone(), element);
                                                    Some(body.clone().into_iter())
                                                }
                                            },
                                            Some(mut stmts) => {
                                                if let Some(stmt) = stmts.next() {
                                                    let result = self.execute_statement(stmt)?;
                                                    match result {
                                                        Value::Return(_) => return Ok(result),
                                                        Value::Break => return Ok(Value::Unit),
                                                        Value::Continue => break,
                                                        _ => {}
                                                    }
                                                    Some(stmts)
                                                } else {
                                                    None
                                                }
                                            }
                                        };
                                    }
                                }
                                Value::String(s) => {
                                    let mut inner_loop = None;
                                    let mut chars = s.chars();
                                    loop {
                                        inner_loop = match inner_loop {
                                            None => match chars.next() {
                                                None => break,
                                                Some(c) => {
                                                    self.current_scope_mut().set(
                                                        iterator.clone(),
                                                        Value::String(c.to_string()),
                                                    );
                                                    Some(body.clone().into_iter())
                                                }
                                            },
                                            Some(mut stmts) => {
                                                if let Some(stmt) = stmts.next() {
                                                    let result = self.execute_statement(stmt)?;
                                                    match result {
                                                        Value::Return(_) => return Ok(result),
                                                        Value::Break => return Ok(Value::Unit),
                                                        Value::Continue => break,
                                                        _ => {}
                                                    }
                                                    Some(stmts)
                                                } else {
                                                    None
                                                }
                                            }
                                        };
                                    }
                                }
                                _ => {
                                    return Err(VeldError::RuntimeError(format!(
                                        "Cannot iterate over value of type {:?}",
                                        iterable_value
                                    )));
                                }
                            }
                            Ok(Value::Unit)
                        }

                        _ => Ok(Value::Unit),
                    },
                })
            }
        }
    }

    fn block_flow(
        &mut self,
        stack: &mut Vec<Frame>,
        last_value: Result<Value>,
        mut stmts: IntoIter<Statement>,
    ) -> ControlFlow<Result<Value>, Statement> {
        if let Some(stmt) = stmts.next() {
            stack.push(Frame::BlockScope1(BlockScope1 { stmts, last_value }));
            ControlFlow::Continue(stmt)
        } else {
            self.pop_scope();
            ControlFlow::Break(last_value)
        }
    }
}
impl Interpreter {
    fn execute_return(&mut self, expr_opt: Option<Expr>) -> Result<Result<Value>> {
        let val = if let Some(e) = expr_opt {
            self.evaluate_expression(e)?
        } else {
            Value::Unit
        };
        Ok(Ok(Value::Return(Box::new(val))))
    }

    fn execute_match(&mut self, value: Expr, arms: Vec<MatchArm>) -> Result<Value> {
        let match_value = self.evaluate_expression(value)?.unwrap_return();
        tracing::debug!(
            "execute_match: matching value {:?} against {} arms",
            match_value,
            arms.len()
        );

        for (arm_index, arm) in arms.iter().enumerate() {
            tracing::debug!(
                "execute_match: trying arm {} with pattern {:?}",
                arm_index,
                arm.pat
            );
            if let Some(bindings) = self.pattern_matches(&arm.pat, &match_value)? {
                tracing::debug!(
                    "execute_match: arm {} matched with bindings: {:?}",
                    arm_index,
                    bindings
                );
                if let Some(guard) = &arm.guard {
                    self.push_scope();

                    for (name, val) in &bindings {
                        self.current_scope_mut().set(name.clone(), val.clone());
                    }

                    let guard_result = self.evaluate_expression(guard.clone())?.unwrap_return();
                    let guard_passed = self.is_truthy(guard_result);

                    self.pop_scope();

                    if !guard_passed {
                        continue;
                    }
                }

                self.push_scope();

                for (name, val) in bindings {
                    self.current_scope_mut().set(name, val)
                }

                let result = self.evaluate_expression(arm.body.clone())?.unwrap_return();

                self.pop_scope();
                return Ok(result);
            }
        }
        Err(VeldError::RuntimeError(
            "No match arm matched the value".to_string(),
        ))
    }

    fn execute_macro_invocation(
        &mut self,
        name: &String,
        arguments: Vec<Expr>,
    ) -> Result<Result<Value>> {
        // Evaluate all arguments
        let mut evaluated_args = Vec::new();
        for arg in arguments {
            let arg_value = self.evaluate_expression(arg)?;
            evaluated_args.push(arg_value);
        }

        let current_mod = self.current_module.clone();
        let macro_def = self.find_macro(&current_mod, &name)?;

        Ok(match macro_def {
            Statement::MacroDeclaration {
                patterns,
                body: None,
                ..
            } => self.expand_pattern_macro(name.as_str(), &patterns, &evaluated_args),
            // Procedural macro
            Statement::MacroDeclaration {
                patterns: _,
                body: Some(body),
                ..
            } => self.execute_procedural_macro(name.as_str(), &body, &evaluated_args),
            _ => Err(VeldError::RuntimeError(format!(
                "Invalid macro definition for '{}'",
                name
            ))),
        })
    }

    fn execute_compound_assignment(
        &mut self,
        name: String,
        operator: BinaryOperator,
        value: Box<Expr>,
    ) -> Result<Result<Value>> {
        let current = self
            .get_variable(&name)
            .ok_or_else(|| VeldError::RuntimeError(format!("Undefined variable '{}'", name)))?;
        let new_value = self.evaluate_expression(*value)?;
        let result = self.evaluate_binary_op(current, operator, new_value)?;

        // Convert the result back to an expression for assignment
        let result_expr = self.value_to_expr(result)?;
        Ok(self.execute_assignment(name, Box::new(result_expr)))
    }

    fn execute_inherent_implementation(
        &mut self,
        type_name: String,
        generic_params: Vec<GenericArgument>,
        methods: Vec<MethodImpl>,
    ) -> Result<Value> {
        // Set up type parameter scope for generic impl blocks
        let has_generics = !generic_params.is_empty();
        if has_generics {
            tracing::debug!("Setting up type parameter scope for InherentImpl");
            self.type_checker.env().push_type_param_scope();
            for param in &generic_params {
                let param_name = match &param.name {
                    Some(name) => name.clone(),
                    None => {
                        if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                            base_name.clone()
                        } else {
                            "T".to_string()
                        }
                    }
                };
                tracing::debug!("Adding type parameter to scope: {}", param_name);
                self.type_checker.env().add_type_param(&param_name);
            }
        }

        let result = self.execute_implementation(type_name.clone(), methods.clone());

        // Register methods in type checker while type parameters are in scope
        for method in &methods {
            if !self
                .type_checker
                .env()
                .has_struct_method(&type_name, &method.name)
            {
                let method_type = self.type_checker.method_to_type(&method)?;
                self.type_checker
                    .env()
                    .add_struct_method(&type_name, &method.name, method_type);
            }
        }

        // Clean up type parameter scope
        if has_generics {
            tracing::debug!("Cleaning up type parameter scope for InherentImpl");
            self.type_checker.env().pop_type_param_scope();
        }

        result
    }

    fn execute_trait_implementation(
        &mut self,
        type_name: String,
        kind_name: Option<String>,
        generic_args: Vec<GenericArgument>,
        methods: Vec<MethodImpl>,
    ) -> Result<Value> {
        // Set up type parameter scope for generic trait impl blocks
        let has_generics = !generic_args.is_empty();
        if has_generics {
            tracing::debug!("Setting up type parameter scope for trait Implementation");
            self.type_checker.env().push_type_param_scope();
            for param in &generic_args {
                let param_name = match &param.name {
                    Some(name) => name.clone(),
                    None => {
                        if let TypeAnnotation::Basic(base_name) = &param.type_annotation {
                            base_name.clone()
                        } else {
                            "T".to_string()
                        }
                    }
                };
                tracing::debug!("Adding type parameter to scope: {}", param_name);
                self.type_checker.env().add_type_param(&param_name);
            }
        }

        let result = self.execute_implementation(type_name.clone(), methods.clone());

        // Register methods in type checker while type parameters are in scope
        tracing::debug!(
            "Registering trait implementation: type_name={}, kind_name={:?}, methods={:?}",
            type_name,
            kind_name,
            methods.iter().map(|m| &m.name).collect::<Vec<_>>()
        );

        if let Some(kind_name) = &kind_name {
            tracing::debug!(
                "Processing trait implementation {} for type {}",
                kind_name,
                type_name
            );
            // This is a trait implementation - register as implementation
            let mut impl_method_types = HashMap::new();
            for method in &methods {
                let method_type = self.type_checker.method_to_type(&method)?;
                tracing::debug!("Method {} has type {:?}", method.name, method_type);
                impl_method_types.insert(method.name.clone(), method_type);
            }

            tracing::debug!(
                "Adding implementation {} for type {} with {} methods",
                kind_name,
                type_name,
                impl_method_types.len()
            );
            self.type_checker.env().add_implementation(
                &type_name,
                kind_name,
                generic_args.clone(),
                impl_method_types,
            );
        } else {
            tracing::debug!("Processing inherent implementation for type {}", type_name);
            // This is an inherent implementation - register in struct_methods
            for method in &methods {
                if !self
                    .type_checker
                    .env()
                    .has_struct_method(&type_name, &method.name)
                {
                    let method_type = self.type_checker.method_to_type(&method)?;
                    tracing::debug!(
                        "Adding struct method {} with type {:?}",
                        method.name,
                        method_type
                    );
                    self.type_checker.env().add_struct_method(
                        &type_name,
                        &method.name,
                        method_type,
                    );
                }
            }
        }

        // Clean up type parameter scope
        if has_generics {
            tracing::debug!("Cleaning up type parameter scope for trait Implementation");
            self.type_checker.env().pop_type_param_scope();
        }

        result
    }

    fn execute_implementation(
        &mut self,
        type_name: String,
        methods: Vec<MethodImpl>,
    ) -> Result<Value> {
        // Check if this is a struct or enum type
        let is_struct = self.structs.contains_key(&type_name);
        let is_enum = self.enums.contains_key(&type_name);

        if is_struct {
            // Register methods for struct type - get or create the method map
            if !self.struct_methods.contains_key(&type_name) {
                self.struct_methods
                    .insert(type_name.clone(), HashMap::new());
            }
            let struct_method_map = self.struct_methods.get_mut(&type_name).unwrap();

            for method in &methods {
                // Convert MethodImpl to Value::Function for struct methods
                let function = Value::Function {
                    params: method.params.clone(),
                    body: method.body.clone(),
                    return_type: method.return_type.clone(),
                    captured_vars: HashMap::new(),
                };
                struct_method_map.insert(method.name.clone(), function);

                // Type checker registration is handled by the caller to ensure proper type parameter scope
            }
        } else if is_enum {
            // Register methods for enum type - get or create the method map
            if !self.enum_methods.contains_key(&type_name) {
                self.enum_methods.insert(type_name.clone(), HashMap::new());
            }
            let enum_method_map = self.enum_methods.get_mut(&type_name).unwrap();

            for method in methods {
                enum_method_map.insert(method.name.clone(), method);
            }
        } else {
            // Type not found - this could be for a generic type or forward declaration
            // For now, assume it's a struct and register it
            if !self.struct_methods.contains_key(&type_name) {
                self.struct_methods
                    .insert(type_name.clone(), HashMap::new());
            }
            let struct_method_map = self.struct_methods.get_mut(&type_name).unwrap();

            for method in &methods {
                // Convert MethodImpl to Value::Function for struct methods
                let function = Value::Function {
                    params: method.params.clone(),
                    body: method.body.clone(),
                    return_type: method.return_type.clone(),
                    captured_vars: HashMap::new(),
                };
                struct_method_map.insert(method.name.clone(), function);

                // Only register in type checker if not already registered (avoid re-processing during execution)
                if !self
                    .type_checker
                    .env()
                    .has_struct_method(&type_name, &method.name)
                {
                    // Type checker registration is handled by the caller to ensure proper type parameter scope
                }
            }
        }

        Ok(Value::Unit)
    }

    fn execute_struct_declaration(
        &mut self,
        name: String,
        fields: Vec<StructField>,
        methods: Vec<StructMethod>,
        generic_params: Vec<GenericArgument>,
    ) -> Result<Value> {
        // Register the struct type
        if generic_params.is_empty() {
            self.structs.insert(name.clone(), fields.clone());

            // Also register in type checker
            let field_map: HashMap<String, Type> = fields
                .iter()
                .map(|f| {
                    let field_type = self
                        .type_checker
                        .env()
                        .from_annotation(&f.type_annotation, None)
                        .unwrap_or(Type::Any);
                    (f.name.clone(), field_type)
                })
                .collect();
            self.type_checker.env().add_struct(&name, field_map);
        } else {
            let struct_info = StructInfo {
                fields: fields
                    .into_iter()
                    .map(|f| (f.name, f.type_annotation))
                    .collect(),
                generic_params: generic_params.clone(),
            };
            self.generic_structs.insert(name.clone(), struct_info);
            self.structs.insert(name.clone(), vec![]);
        };

        // Register methods if any
        if !methods.is_empty() {
            let mut method_map = HashMap::new();

            for method in &methods {
                // Register in runtime
                let method_value = Value::Function {
                    params: method.params.clone(),
                    body: method.body.clone(),
                    return_type: method.return_type.clone(),
                    captured_vars: HashMap::new(), // No captured vars for struct methods
                };

                method_map.insert(method.name.clone(), method_value);

                // Also register in type checker
                let method_type = self.type_checker.struct_method_to_type(&method)?;
                self.type_checker
                    .env()
                    .add_struct_method(&name, &method.name, method_type);
            }

            self.struct_methods.insert(name.clone(), method_map);
        }

        Ok(Value::Unit)
    }

    fn execute_proc_declaration(
        &mut self,
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        body: Vec<Statement>,
    ) -> Result<Value> {
        // Convert to a function with Unit return type
        let function = Value::Function {
            params: params
                .into_iter()
                .map(|(name, type_anno)| (name, type_anno))
                .collect(),
            body,
            return_type: TypeAnnotation::Unit,
            captured_vars: HashMap::new(),
        };

        self.current_scope_mut().set(name, function);
        Ok(Value::Unit)
    }

    fn execute_function_declaration(
        &mut self,
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        return_type: &TypeAnnotation,
        body: Vec<Statement>,
    ) -> Result<Value> {
        let span = tracing::info_span!("function_declaration", name = %name);
        let _enter = span.enter();

        let processed_body = if !body.is_empty() {
            // If we don't have any explicit returns and the last statement is an expression,
            // convert it to a return
            if !body.iter().any(|s| matches!(s, Statement::Return(_))) {
                let mut new_body = body.clone();
                if let Some(Statement::ExprStatement(expr)) = new_body.clone().last() {
                    new_body.pop();
                    new_body.push(Statement::Return(Some(expr.clone())));
                }
                new_body
            } else {
                body.clone()
            }
        } else {
            body.clone()
        };

        let actual_return_type = match &return_type {
            TypeAnnotation::Basic(name) if name == "infer" => {
                // Try to infer from the body
                if let Some(Statement::Return(Some(expr))) = processed_body.last() {
                    match expr {
                        Expr::Literal(Literal::String(_)) => {
                            TypeAnnotation::Basic("str".to_string())
                        }
                        Expr::Literal(Literal::Integer(_)) => {
                            TypeAnnotation::Basic("i32".to_string())
                        }
                        Expr::Literal(Literal::Float(_)) => {
                            TypeAnnotation::Basic("f64".to_string())
                        }
                        Expr::Literal(Literal::Boolean(_)) => {
                            TypeAnnotation::Basic("bool".to_string())
                        }
                        Expr::Literal(Literal::Char(_)) => {
                            TypeAnnotation::Basic("char".to_string())
                        }
                        _ => return_type.clone(),
                    }
                } else {
                    TypeAnnotation::Unit
                }
            }
            _ => return_type.clone(),
        };

        // Insert the function name into the environment first with an empty body to support mutual recursion
        self.current_scope_mut().set(
            name.clone(),
            Value::Function {
                params: params.clone(),
                body: vec![],
                return_type: actual_return_type.clone(),
                captured_vars: HashMap::new(),
            },
        );

        let function = Value::Function {
            params: params.clone(),
            body: processed_body,
            return_type: actual_return_type,
            captured_vars: HashMap::new(), // No captured vars for top-level functions
        };

        // Overwrite with the real function body
        self.current_scope_mut().set(name, function);
        Ok(Value::Unit)
    }

    fn execute_assignment(&mut self, name: String, value: Box<Expr>) -> Result<Value> {
        let new_value = self.evaluate_expression(*value)?.unwrap_return();

        // Find the variable in scopes (starting from innermost) and check mutability
        for scope in self.scopes.iter_mut().rev() {
            if scope.vals().contains_key(&name) {
                return match scope.assign(&name, new_value) {
                    Ok(_) => Ok(Value::Unit),
                    Err(e) => Err(e),
                };
            }
        }

        // If we get here, the variable wasn't found in any scope
        Err(VeldError::RuntimeError(format!(
            "Cannot assign to undefined variable '{}'",
            name
        )))
    }

    fn execute_property_assignment(
        &mut self,
        target: Box<Expr>,
        operator: &Option<BinaryOperator>,
        value: Box<Expr>,
    ) -> Result<Value> {
        let new_value = self.evaluate_expression(*value)?.unwrap_return();

        // Handle compound assignment by first getting the current value
        let final_value = if let Some(op) = operator {
            let current_value = self.evaluate_expression(*target.clone())?.unwrap_return();
            self.evaluate_binary_op(current_value, op.clone(), new_value)?
        } else {
            new_value
        };

        // Now perform the assignment based on the target expression
        match *target {
            Expr::Identifier(name) => {
                // Simple variable assignment - reuse existing logic
                for scope in self.scopes.iter_mut().rev() {
                    if scope.vals().contains_key(&name) {
                        return match scope.assign(&name, final_value) {
                            Ok(_) => Ok(Value::Unit),
                            Err(e) => Err(e),
                        };
                    }
                }
                Err(VeldError::RuntimeError(format!(
                    "Cannot assign to undefined variable '{}'",
                    name
                )))
            }
            Expr::PropertyAccess { object, property } => {
                // Property assignment like obj.field = value or self.field = value
                // Check if this is assignment to 'self' in a method scope
                if let Expr::Identifier(name) = object.as_ref() {
                    if name == "self" {
                        // Special handling for self property assignment in method scope
                        return self.assign_self_property(&property, final_value);
                    }
                }

                // Evaluate the object and check if we're in a method scope with self
                let object_value = self.evaluate_expression(*object.clone())?.unwrap_return();

                // If we're in a method scope and the object is a struct, treat it as self assignment
                if let Value::Struct { .. } = &object_value {
                    if self.scopes.last().unwrap().get("self").is_some() {
                        return self.assign_self_property(&property, final_value);
                    }
                }

                self.assign_property(object_value, &property, final_value)
            }
            Expr::IndexAccess { object, index } => {
                // Array indexing assignment like array[index] = value
                let object_value = self.evaluate_expression(*object)?.unwrap_return();
                let index_value = self.evaluate_expression(*index)?.unwrap_return();
                self.assign_index(object_value, index_value, final_value)
            }
            _ => Err(VeldError::RuntimeError(
                "Invalid assignment target".to_string(),
            )),
        }
    }

    fn assign_property(
        &mut self,
        mut object: Value,
        property: &str,
        value: Value,
    ) -> Result<Value> {
        match &mut object {
            Value::Struct { fields, .. } => {
                if fields.contains_key(property) {
                    fields.insert(property.to_string(), value);
                    Ok(Value::Unit)
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Property '{}' not found on struct",
                        property
                    )))
                }
            }
            _ => Err(VeldError::RuntimeError(format!(
                "Cannot assign property '{}' to non-struct value",
                property
            ))),
        }
    }

    fn assign_self_property(&mut self, property: &str, value: Value) -> Result<Value> {
        // Get a mutable reference to the 'self' value in the current scope.
        if let Some(self_value) = self.scopes.last_mut().and_then(|s| s.get_mut("self")) {
            match self_value {
                Value::Struct { fields, .. } => {
                    // Mutate the fields directly in place.
                    let is_data = property == "data";
                    tracing::debug!(
                        target: "veld.mutself",
                        property,
                        value = ?value,
                        fields_before = ?fields,
                        "About to assign property on self"
                    );
                    fields.insert(property.to_string(), value.clone());
                    tracing::debug!(
                        target: "veld.mutself",
                        property,
                        value = ?value,
                        fields_after = ?fields,
                        is_data,
                        "Assigned property on self"
                    );
                    // Print the current "self" value after assignment
                    if let Some(self_value) = self.scopes.last().unwrap().get("self") {
                        tracing::debug!(
                            target: "veld.mutself",
                            self_after_assignment = ?self_value,
                            "Self after property assignment"
                        );
                    }
                    Ok(Value::Unit)
                }
                _ => Err(VeldError::RuntimeError(format!(
                    "Cannot assign property '{}' to non-struct self value",
                    property
                ))),
            }
        } else {
            Err(VeldError::RuntimeError(
                "'self' not found in current scope".to_string(),
            ))
        }
    }

    fn assign_index(&mut self, mut object: Value, index: Value, value: Value) -> Result<Value> {
        match (&mut object, &index) {
            (Value::Array(elements), Value::Numeric(index_num)) => {
                let idx_num = index_num.to_i32().map_err(|_| {
                    VeldError::RuntimeError("Array index must be an integer".to_string())
                })?;
                let idx = match idx_num {
                    NumericValue::Integer(int_val) => int_val.as_i64().unwrap_or(0) as usize,
                    _ => {
                        return Err(VeldError::RuntimeError(
                            "Array index must be an integer".to_string(),
                        ));
                    }
                };

                if idx < elements.len() {
                    elements[idx] = value;
                    Ok(Value::Unit)
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Array index {} out of bounds (length: {})",
                        idx,
                        elements.len()
                    )))
                }
            }
            (Value::Array(_), _) => Err(VeldError::RuntimeError(
                "Array index must be a number".to_string(),
            )),
            _ => Err(VeldError::RuntimeError(
                "Cannot index into non-array value".to_string(),
            )),
        }
    }

    fn execute_variable_declaration(
        &mut self,
        name: String,
        var_kind: VarKind,
        type_annotation: Option<TypeAnnotation>,
        value: Box<Expr>,
    ) -> Result<Value> {
        // Check if this is a potentially recursive lambda assignment
        let is_recursive_lambda = matches!(value.as_ref(), Expr::Lambda { .. });

        let mut evaluated_value =
            if is_recursive_lambda && self.lambda_references_name(value.as_ref(), &name) {
                tracing::debug!("Detected recursive lambda for variable: {}", name);
                // For recursive lambdas, pre-bind the variable with a placeholder
                let placeholder = Value::Unit; // Temporary placeholder
                self.current_scope_mut()
                    .declare(name.clone(), placeholder, var_kind.clone())?;
                tracing::debug!("Pre-bound {} with placeholder", name);

                let lambda_value = self.evaluate_expression(*value)?.unwrap_return();
                tracing::debug!("Evaluated recursive lambda, got: {:?}", lambda_value);

                // Replace the placeholder with the actual lambda
                self.current_scope_mut()
                    .set(name.clone(), lambda_value.clone());
                tracing::debug!("Updated {} with actual lambda value", name);

                lambda_value
            } else {
                tracing::debug!("Non-recursive variable declaration for: {}", name);
                self.evaluate_expression(*value)?.unwrap_return()
            };

        // For const declarations, ensure the value is compile-time evaluable
        if matches!(var_kind, VarKind::Const) {
            if !self.is_compile_time_constant(&evaluated_value) {
                return Err(VeldError::RuntimeError(
                    "Const declarations must have compile-time constant values".to_string(),
                ));
            }

            // Constants can only be declared at module level
            if self.scopes.len() > 1 {
                return Err(VeldError::RuntimeError(
                    "Constants can only be declared at module level".to_string(),
                ));
            }
        }

        // Type checking with annotations
        if let Some(type_anno) = type_annotation {
            let expected_type = self.type_checker.env().from_annotation(&type_anno, None)?;

            // Try to coerce the value to match the expected type through safe widening
            if let Ok(coerced_value) = self.try_safe_coerce_value(&evaluated_value, &expected_type)
            {
                evaluated_value = coerced_value;
            } else if !self.types_compatible(&self.get_value_type(&evaluated_value), &expected_type)
            {
                return Err(VeldError::RuntimeError(format!(
                    "Type mismatch: expected {}, got {}",
                    expected_type,
                    self.get_value_type(&evaluated_value)
                )));
            }

            // For arrays, perform additional strict type checking
            if let (Value::Array(elements), Type::Array(expected_elem_type)) =
                (&evaluated_value, &expected_type)
            {
                for (i, element) in elements.iter().enumerate() {
                    let elem_type = self.get_value_type(element);
                    if !self.types_compatible(&elem_type, expected_elem_type) {
                        return Err(VeldError::RuntimeError(format!(
                            "Array element {} type mismatch: expected {}, got {}",
                            i, expected_elem_type, elem_type
                        )));
                    }
                }
            }
        }

        // Store variable with kind information (unless already stored for recursive lambdas)
        let variable_exists = self.current_scope_mut().get(&name).is_some();
        if !variable_exists {
            self.current_scope_mut()
                .declare(name.clone(), evaluated_value.clone(), var_kind)?;
        }

        tracing::debug!(
            target: "veld.mutself",
            "Returning from execute_variable_declaration, assigned_value={:?}, var_name={}",
            evaluated_value,
            name
        );
        Ok(evaluated_value)
    }

    fn is_compile_time_constant(&self, value: &Value) -> bool {
        match value {
            Value::Numeric(_)
            | Value::Integer(_)
            | Value::Float(_)
            | Value::String(_)
            | Value::Boolean(_)
            | Value::Char(_)
            | Value::Unit => true,
            Value::Array(elements) => elements.iter().all(|e| self.is_compile_time_constant(e)),
            Value::Tuple(elements) => elements.iter().all(|e| self.is_compile_time_constant(e)),
            _ => false,
        }
    }

    fn validate_value_type(&self, value: &Value, expected_type: &Type) -> Result<()> {
        let value_type = self.get_value_type(value);

        if !self.types_compatible(&value_type, expected_type) {
            return Err(VeldError::RuntimeError(format!(
                "Type mismatch: expected {}, got {}",
                expected_type, value_type
            )));
        }

        // For arrays, also validate element types
        if let (Value::Array(elements), Type::Array(expected_elem_type)) = (value, expected_type) {
            for element in elements {
                let elem_type = self.get_value_type(element);
                if !self.types_compatible(&elem_type, expected_elem_type) {
                    return Err(VeldError::RuntimeError(format!(
                        "Array element type mismatch: expected {}, got {}",
                        expected_elem_type, elem_type
                    )));
                }
            }
        }

        Ok(())
    }

    fn get_value_type(&self, value: &Value) -> Type {
        match value {
            Value::Numeric(nv) => nv.clone().type_of(),
            Value::Integer(_) => Type::I64,
            Value::Float(_) => Type::F64,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Bool,
            Value::Char(_) => Type::Char,
            Value::Unit => Type::Unit,
            Value::Array(elements) => {
                if elements.is_empty() {
                    Type::Array(Box::new(Type::Any))
                } else {
                    let elem_type = self.get_value_type(&elements[0]);

                    // Verify all elements have the same type
                    for element in elements.iter().skip(1) {
                        let element_type = self.get_value_type(element);
                        if !self.types_compatible(&elem_type, &element_type) {
                            // If types aren't compatible, fall back to Any
                            return Type::Array(Box::new(Type::Any));
                        }
                    }

                    Type::Array(Box::new(elem_type))
                }
            }
            Value::Tuple(elements) => {
                let elem_types = elements.iter().map(|e| self.get_value_type(e)).collect();
                Type::Tuple(elem_types)
            }
            Value::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params
                    .iter()
                    .map(|(_, type_anno)| {
                        // Convert TypeAnnotation to Type - simplified for now
                        match type_anno {
                            TypeAnnotation::Basic(name) => match name.as_str() {
                                "i32" => Type::I32,
                                "f64" => Type::F64,
                                "str" => Type::String,
                                "bool" => Type::Bool,
                                _ => Type::Any,
                            },
                            TypeAnnotation::Unit => Type::Unit,
                            _ => Type::Any,
                        }
                    })
                    .collect();

                let ret_type = match return_type {
                    TypeAnnotation::Basic(name) => match name.as_str() {
                        "i32" => Type::I32,
                        "f64" => Type::F64,
                        "str" => Type::String,
                        "bool" => Type::Bool,
                        _ => Type::Any,
                    },
                    TypeAnnotation::Unit => Type::Unit,
                    _ => Type::Any,
                };

                Type::Function {
                    params: param_types,
                    return_type: Box::new(ret_type),
                }
            }
            Value::Enum {
                enum_name,
                variant_name: _,
                fields: _,
            } => {
                // For enum values, we need to determine if it's a generic enum or simple enum
                // Check if this is a generic enum like Result<T, E>
                if enum_name == "Result" || enum_name == "Option" {
                    // For imported generic enums, return a generic type
                    Type::Generic {
                        base: enum_name.clone(),
                        type_args: vec![], // Empty for now - could be enhanced to infer from fields
                    }
                } else if let Some(_variants) = self.enums.get(enum_name) {
                    // For locally defined enums, we would need to convert ast::EnumVariant to types::base::EnumVariant
                    // For now, treat as generic to avoid type conversion issues
                    Type::Generic {
                        base: enum_name.clone(),
                        type_args: vec![],
                    }
                } else {
                    // Default case for unknown enums
                    Type::Generic {
                        base: enum_name.clone(),
                        type_args: vec![],
                    }
                }
            }
            _ => Type::Any,
        }
    }

    fn types_compatible(&self, actual: &Type, expected: &Type) -> bool {
        // Implement your type compatibility rules
        match (actual, expected) {
            // Exact match
            (a, b) if a == b => true,
            // Any type is compatible with anything
            (_, Type::Any) | (Type::Any, _) => true,
            // Numeric type compatibility
            (a, b) if self.is_numeric_type_compat(a) && self.is_numeric_type_compat(b) => true,
            // Generic types with same base are compatible if type args are compatible
            (
                Type::Generic {
                    base: base1,
                    type_args: args1,
                },
                Type::Generic {
                    base: base2,
                    type_args: args2,
                },
            ) => {
                // Same base name is required
                if base1 != base2 {
                    return false;
                }

                // If either has empty type args, consider them compatible (lenient mode)
                // This handles cases like Result<> being compatible with Result<i32, str>
                if args1.is_empty() || args2.is_empty() {
                    return true;
                }

                // Otherwise, check type args compatibility
                args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(a, b)| self.types_compatible(a, b))
            }
            // Enum types with same name are compatible
            (Type::Enum { name: name1, .. }, Type::Enum { name: name2, .. }) => name1 == name2,
            // Generic enum types are compatible with their base enum
            (Type::Generic { base, .. }, Type::Enum { name, .. }) => base == name,
            (Type::Enum { name, .. }, Type::Generic { base, .. }) => name == base,
            // Array types are compatible if element types are compatible
            (Type::Array(elem1), Type::Array(elem2)) => self.types_compatible(elem1, elem2),
            // Function types are compatible if parameters and return types are compatible
            (
                Type::Function {
                    params: params1,
                    return_type: ret1,
                },
                Type::Function {
                    params: params2,
                    return_type: ret2,
                },
            ) => {
                params1.len() == params2.len()
                    && params1
                        .iter()
                        .zip(params2.iter())
                        .all(|(a, b)| self.types_compatible(a, b))
                    && self.types_compatible(ret1, ret2)
            }
            // Record types are compatible if they have the same fields with compatible types
            (Type::Record { fields: fields1 }, Type::Record { fields: fields2 }) => {
                fields1.len() == fields2.len()
                    && fields1.iter().all(|(name, type1)| {
                        fields2
                            .get(name)
                            .map_or(false, |type2| self.types_compatible(type1, type2))
                    })
            }
            // Otherwise, not compatible
            _ => false,
        }
    }

    fn is_numeric_type_compat(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::F32
                | Type::F64
        )
    }

    fn pattern_matches(
        &self,
        pattern: &MatchPattern,
        value: &Value,
    ) -> Result<Option<HashMap<String, Value>>> {
        match pattern {
            MatchPattern::Wildcard => Ok(Some(HashMap::new())),
            MatchPattern::Literal(lit) => match (lit, value) {
                (Literal::Integer(a), Value::Integer(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::Float(a), Value::Float(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::String(a), Value::String(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::Boolean(a), Value::Boolean(b)) if a == b => Ok(Some(HashMap::new())),
                (Literal::Unit, Value::Unit) => Ok(Some(HashMap::new())),
                _ => Ok(None),
            },
            MatchPattern::Identifier(name) => {
                let mut bindings = HashMap::new();
                bindings.insert(name.clone(), value.clone());
                Ok(Some(bindings))
            }
            MatchPattern::Struct { name, fields } => {
                // Support matching enum variants as struct patterns
                if let Value::Enum {
                    enum_name,
                    variant_name,
                    fields: value_fields,
                } = value
                {
                    let expected_name = format!("{}.{}", enum_name, variant_name);
                    tracing::debug!(
                        "Pattern matching: pattern name='{}', expected_name='{}', enum_name='{}', variant_name='{}'",
                        name,
                        expected_name,
                        enum_name,
                        variant_name
                    );

                    // Support both qualified (Option.Some) and unqualified (Some) patterns
                    if name != &expected_name && name != variant_name {
                        tracing::debug!("Pattern name mismatch, returning None");
                        return Ok(None); // Wrong enum variant
                    }

                    let mut all_bindings = HashMap::new();

                    for (i, (field_name, field_pattern)) in fields.iter().enumerate() {
                        if let Some(field_value) = value_fields.get(i) {
                            if let Some(pattern) = field_pattern {
                                if let Some(bindings) =
                                    self.pattern_matches(&**pattern, field_value)?
                                {
                                    all_bindings.extend(bindings);
                                } else {
                                    return Ok(None); // Field did not match
                                }
                            } else {
                                tracing::debug!(
                                    "Binding variable '{}' to value: {:?}",
                                    field_name,
                                    field_value
                                );
                                all_bindings.insert(field_name.clone(), field_value.clone());
                            }
                        } else {
                            return Ok(None);
                        }
                    }
                    tracing::debug!("Pattern matched successfully, bindings: {:?}", all_bindings);
                    Ok(Some(all_bindings))
                } else if let Value::Struct {
                    name: value_name,
                    fields: value_fields,
                } = value
                {
                    if name != value_name {
                        return Ok(None); // Wrong struct name
                    }

                    let mut all_bindings = HashMap::new();

                    for (field_name, field_pattern) in fields {
                        if let Some(field_value) = value_fields.get(field_name) {
                            if let Some(pattern) = field_pattern {
                                if let Some(bindings) =
                                    self.pattern_matches(&**pattern, field_value)?
                                {
                                    all_bindings.extend(bindings);
                                } else {
                                    return Ok(None); // Field did not match
                                }
                            } else {
                                all_bindings.insert(field_name.clone(), field_value.clone());
                            }
                        } else {
                            return Ok(None);
                        }
                    }
                    Ok(Some(all_bindings))
                } else {
                    Ok(None) // Not a struct or enum
                }
            }
            MatchPattern::Enum {
                name,
                variant,
                fields,
            } => {
                if let Value::Enum {
                    enum_name,
                    variant_name,
                    fields: value_fields,
                } = value
                {
                    if name != enum_name || variant != variant_name {
                        return Ok(None); // Wrong enum or variant
                    }

                    let mut all_bindings = HashMap::new();

                    for (i, (field_name, field_pattern)) in fields.iter().enumerate() {
                        if let Some(field_value) = value_fields.get(i) {
                            if let Some(pattern) = field_pattern {
                                if let Some(bindings) =
                                    self.pattern_matches(&**pattern, field_value)?
                                {
                                    all_bindings.extend(bindings);
                                } else {
                                    return Ok(None); // Field did not match
                                }
                            } else {
                                all_bindings.insert(field_name.clone(), field_value.clone());
                            }
                        } else {
                            return Ok(None);
                        }
                    }
                    Ok(Some(all_bindings))
                } else {
                    Ok(None) // Not an enum
                }
            } // _ => Ok(None), // Not implemented yet
        }
    }

    fn cast_val_to_num(&self, value: Value, target_type: &TypeAnnotation) -> Result<Value> {
        let numeric_value = match value {
            Value::Numeric(nv) => nv,
            Value::Integer(i) => NumericValue::Integer(IntegerValue::I64(i)),
            Value::Float(f) => NumericValue::Float(FloatValue::F64(f)),
            _ => {
                return Err(VeldError::RuntimeError(format!(
                    "Cannot cast value {:?} to numeric type {:?}",
                    value, target_type
                )));
            }
        };

        let result = match target_type {
            TypeAnnotation::Basic(type_name) => match type_name.as_str() {
                "i8" => numeric_value.to_i8()?,
                "i16" => numeric_value.to_i16()?,
                "i32" => numeric_value.to_i32()?,
                "i64" => numeric_value.to_i64()?,
                "u8" => numeric_value.to_u8()?,
                "u16" => numeric_value.to_u16()?,
                "u32" => numeric_value.to_u32()?,
                "u64" => numeric_value.to_u64()?,
                "f32" => numeric_value.to_f32()?,
                "f64" => numeric_value.to_f64()?,
                _ => {
                    return Err(VeldError::RuntimeError(format!(
                        "Invalid cast from numeric type {:?} to {:?}",
                        numeric_value, type_name
                    )));
                }
            },
            _ => {
                return Err(VeldError::RuntimeError(format!(
                    "Cannot cast type {:?} to numeric type {:?}",
                    numeric_value, target_type
                )));
            }
        };
        Ok(Value::Numeric(result))
    }
}

impl Interpreter {
    fn evaluate_expression(&mut self, expr: Expr) -> Result<Value> {
        // let mut stack = Vec::new();
        let mut action: ControlFlow<Result<Value>, Expr> = ControlFlow::Continue(expr.to_owned());
        loop {
            action = '_cont: {
                ControlFlow::Break(match action {
                    ControlFlow::Break(result) => {
                        // If we hit a break, return the result for now
                        return result;
                    }
                    ControlFlow::Continue(expr) => {
                        match expr {
                            Expr::MacroVar(ref name) => {
                                // TODO: Implement macro variable lookup in macro expansion context.
                                // For now, return an error.
                                return Err(VeldError::RuntimeError(format!(
                                    "Macro variable ${} not supported in interpreter (implement macro expansion context lookup)",
                                    name
                                )));
                            }
                            Expr::SelfReference => self.get_variable("self").ok_or_else(|| {
                                VeldError::RuntimeError("self not found".to_string())
                            }),
                            Expr::Literal(lit) => evaluate_literal_expression(lit),
                            Expr::UnitLiteral => Ok(Value::Unit),
                            Expr::Identifier(name) => self.get_variable(&name).ok_or_else(|| {
                                VeldError::RuntimeError(format!("Undefined variable '{}'", name))
                            }),
                            Expr::BlockLambda {
                                params,
                                body,
                                return_type,
                                generic_params: _,
                            } => Ok(self.create_block_lambda(params, body, return_type)),
                            Expr::Lambda {
                                params,
                                body,
                                return_type,
                                generic_params: _,
                            } => Ok(self.create_lambda(params, body, return_type)),
                            Expr::IfExpression {
                                condition,
                                then_expr,
                                else_expr,
                            } => {
                                let cond_value =
                                    self.evaluate_expression(*condition)?.unwrap_return();

                                let truthy = self.is_truthy(cond_value.clone());

                                if truthy {
                                    self.evaluate_expression(*then_expr)
                                } else if let Some(else_expr) = else_expr {
                                    self.evaluate_expression(*else_expr)
                                } else {
                                    Ok(Value::Unit)
                                }
                            }
                            Expr::BinaryOp {
                                left,
                                operator,
                                right,
                            } => {
                                if operator == BinaryOperator::Pipe {
                                    // Handle pipe operator as a special case
                                    let left_val = self.evaluate_expression(*left)?.unwrap_return();
                                    match *right {
                                        Expr::Call { callee, arguments } => {
                                            // Insert left_val as the first argument
                                            let left_expr = self.value_to_expr(left_val.clone())?;
                                            let mut new_args =
                                                vec![Argument::Positional(left_expr)];
                                            new_args.extend(arguments);
                                            let new_call = Expr::Call {
                                                callee,
                                                arguments: new_args,
                                            };
                                            self.evaluate_expression(new_call)
                                        }
                                        _ => {
                                            // Treat as a function call with left_val as the only argument
                                            let left_expr = self.value_to_expr(left_val)?;
                                            let call = Expr::Call {
                                                callee: Box::new(*right),
                                                arguments: vec![Argument::Positional(left_expr)],
                                            };
                                            self.evaluate_expression(call)
                                        }
                                    }
                                } else {
                                    let left_val = self.evaluate_expression(*left)?.unwrap_return();
                                    let right_val =
                                        self.evaluate_expression(*right)?.unwrap_return();
                                    self.evaluate_binary_op(left_val, operator, right_val)
                                }
                            }
                            Expr::FunctionCall { name, arguments } => {
                                // Evaluate each argument and handle both named and positional arguments
                                let mut arg_values = Vec::new();
                                let mut named_args = HashMap::new();

                                for arg in arguments {
                                    match arg {
                                        Argument::Positional(expr) => {
                                            let value = self.evaluate_expression(expr)?;
                                            arg_values.push(value);
                                        }
                                        Argument::Named { name, value } => {
                                            let value = self.evaluate_expression(value)?;
                                            named_args.insert(name, value);
                                        }
                                    }
                                }

                                // Special handling for standard library functions
                                if name == "sqrt" {
                                    if arg_values.len() != 1 {
                                        return Err(VeldError::RuntimeError(
                                            "sqrt() takes exactly one argument".to_string(),
                                        ));
                                    }

                                    // Extract the argument and compute square root
                                    match arg_values[0] {
                                        Value::Float(f) => Ok(Value::Float(f.sqrt())),
                                        _ => Err(VeldError::RuntimeError(
                                            "sqrt() requires a floating-point argument".to_string(),
                                        )),
                                    }
                                } else {
                                    // Regular function call
                                    self.call_function_with_values(name, arg_values)
                                }
                            }
                            Expr::PropertyAccess { object, property } => {
                                let obj_value = self.evaluate_expression(*object)?;
                                self.get_property(obj_value, &property)
                            }
                            Expr::TypeCast { expr, target_type } => {
                                let value = self.evaluate_expression(*expr)?;
                                let target = self
                                    .type_checker
                                    .env()
                                    .from_annotation(&target_type, None)?;
                                self.cast_value(value, &target)
                            }
                            Expr::Call { callee, arguments } => {
                                // If callee is a PropertyAccess, treat as method call or module function call
                                if let Expr::PropertyAccess { object, property } = &*callee {
                                    let obj_value = self.evaluate_expression(*object.clone())?;
                                    // If the object is a module, treat as function call
                                    if let Value::Module(module) = &obj_value {
                                        // Look up the property in module.exports
                                        if let Some(export) = module.exports.get(property) {
                                            // If it's a function, call as function
                                            if let veld_common::value::module::ExportedItem::Function(idx) =
                                                export
                                            {
                                                if let Some(Statement::FunctionDeclaration {
                                                    params,
                                                    body,
                                                    return_type,
                                                    ..
                                                }) = module.statements.get(*idx)
                                                {
                                                    let func_val = Value::Function {
                                                        params: params.clone(),
                                                        body: body.clone(),
                                                        return_type: return_type.clone(),
                                                        captured_vars: HashMap::new(),
                                                    };
                                                    let mut arg_values = Vec::new();
                                                    for arg in arguments {
                                                        let expr = match arg {
                                                            Argument::Positional(expr) => expr,
                                                            Argument::Named { name: _, value } => {
                                                                value
                                                            }
                                                        };
                                                        let value =
                                                            self.evaluate_expression(expr)?;
                                                        arg_values.push(value);
                                                    }
                                                    return self
                                                        .call_function_value(func_val, arg_values);
                                                }
                                            }
                                        }
                                    }
                                    // Otherwise, treat as method call
                                    let mut arg_values = Vec::new();
                                    for arg in arguments {
                                        let expr = match arg {
                                            Argument::Positional(expr) => expr,
                                            Argument::Named { name: _, value } => value,
                                        };
                                        let value = self.evaluate_expression(expr)?;
                                        arg_values.push(value);
                                    }
                                    return self.call_method_value(
                                        obj_value,
                                        property.clone(),
                                        arg_values,
                                    );
                                }
                                // Otherwise, treat as regular function call
                                // Extract function name if callee is an identifier before evaluating
                                let function_name = if let Expr::Identifier(name) = &*callee {
                                    Some(name.clone())
                                } else {
                                    None
                                };
                                let callee_val = self.evaluate_expression(*callee)?;
                                let mut arg_values = Vec::new();
                                for arg in arguments {
                                    match arg {
                                        Argument::Positional(expr) => {
                                            arg_values.push(self.evaluate_expression(expr)?)
                                        }
                                        Argument::Named { name: _, value } => {
                                            arg_values.push(self.evaluate_expression(value)?)
                                        }
                                    }
                                }
                                self.call_function_value_with_name(
                                    callee_val,
                                    arg_values,
                                    function_name,
                                )
                            }

                            Expr::MethodCall {
                                object,
                                method,
                                arguments,
                            } => {
                                // Store the variable name if the object is an identifier
                                let variable_name = if let Expr::Identifier(name) = object.as_ref()
                                {
                                    Some(name.clone())
                                } else {
                                    None
                                };

                                let obj_value = self.evaluate_expression(*object)?;

                                // Check if this is a module function call
                                if let Value::Module(module) = &obj_value {
                                    // Special handling for std.io module - redirect to native implementations
                                    if module.name == "std.io" {
                                        let native_function_name = format!("std.io.{}", method);
                                        if self.native_registry.contains(&native_function_name) {
                                            let mut arg_values = Vec::new();
                                            for arg in arguments {
                                                let expr = match arg {
                                                    Argument::Positional(expr) => expr,
                                                    Argument::Named { name: _, value } => value,
                                                };
                                                let value = self.evaluate_expression(expr)?;
                                                arg_values.push(value);
                                            }

                                            let result = if let Some(handler) =
                                                self.native_registry.get(&native_function_name)
                                            {
                                                handler(self, arg_values)
                                            } else if self
                                                .native_registry
                                                .contains_static(&native_function_name)
                                            {
                                                self.native_registry.call_static(
                                                    &native_function_name,
                                                    self,
                                                    arg_values,
                                                )
                                            } else {
                                                return Err(VeldError::RuntimeError(format!(
                                                    "Native function '{}' not found",
                                                    native_function_name
                                                )));
                                            };
                                            return result;
                                        }
                                    }

                                    if let Some(export) = module.exports.get(&method) {
                                        if let veld_common::value::module::ExportedItem::Function(
                                            idx,
                                        ) = export
                                        {
                                            if let Some(Statement::FunctionDeclaration {
                                                params,
                                                body,
                                                return_type,
                                                ..
                                            }) = module.statements.get(*idx)
                                            {
                                                let func_val = Value::Function {
                                                    params: params.clone(),
                                                    body: body.clone(),
                                                    return_type: return_type.clone(),
                                                    captured_vars: HashMap::new(),
                                                };
                                                let mut arg_values = Vec::new();
                                                for arg in arguments {
                                                    let expr = match arg {
                                                        Argument::Positional(expr) => expr,
                                                        Argument::Named { name: _, value } => value,
                                                    };
                                                    let value = self.evaluate_expression(expr)?;
                                                    arg_values.push(value);
                                                }
                                                return self
                                                    .call_function_value(func_val, arg_values);
                                            }
                                        }
                                    }
                                    return Err(VeldError::RuntimeError(format!(
                                        "Function '{}' not found in module",
                                        method
                                    )));
                                }

                                // Handle array methods
                                match &obj_value {
                                    Value::Array(_) => {
                                        // Evaluate all arguments
                                        let mut arg_values = Vec::new();
                                        for arg in arguments {
                                            let expr = match arg {
                                                Argument::Positional(expr) => expr,
                                                Argument::Named { name: _, value } => value,
                                            };
                                            let value = self.evaluate_expression(expr)?;
                                            arg_values.push(value);
                                        }

                                        // Call the array method directly
                                        let result = self.call_method_value_with_mutation(
                                            obj_value,
                                            method.clone(),
                                            arg_values,
                                            variable_name.clone(),
                                        )?;
                                        Ok(result)
                                    }
                                    _ => {
                                        // Always call method for MethodCall expressions, even with zero arguments
                                        let mut arg_values = Vec::new();
                                        for arg in arguments {
                                            // Extract and evaluate the expression from the Argument
                                            let expr = match arg {
                                                Argument::Positional(expr) => expr,
                                                Argument::Named { name: _, value } => value,
                                            };
                                            let value = self.evaluate_expression(expr)?;
                                            arg_values.push(value);
                                        }

                                        // DEBUG: Print before/after for mut self
                                        let result = self.call_method_value_with_mutation(
                                            obj_value,
                                            method.clone(),
                                            arg_values,
                                            variable_name,
                                        )?;
                                        Ok(result)
                                    }
                                }
                            }

                            Expr::StructCreate {
                                struct_name,
                                fields,
                            } => {
                                // Check if struct exists
                                if !self.structs.contains_key(&struct_name) {
                                    return Err(VeldError::RuntimeError(format!(
                                        "Undefined struct '{}'",
                                        struct_name
                                    )));
                                }

                                let mut field_values = HashMap::new();

                                // Evaluate each field value
                                for (field_name, field_expr) in fields {
                                    let value = self.evaluate_expression(field_expr)?;
                                    field_values.insert(field_name, value.unwrap_return());
                                }

                                Ok(Value::Struct {
                                    name: struct_name,
                                    fields: field_values,
                                })
                            }
                            Expr::ArrayLiteral(elements) => {
                                let mut values = Vec::new();
                                let mut expected_type: Option<Type> = None;

                                for element in elements {
                                    let value = self.evaluate_expression(element)?.unwrap_return();
                                    let value_type = value.type_of();

                                    if let Some(ref expected) = expected_type {
                                        if !self.types_compatible(&value_type, expected) {
                                            return Err(VeldError::RuntimeError(format!(
                                                "Array elements must have the same type. Expected {}, got {}",
                                                expected, value_type
                                            )));
                                        }
                                    } else {
                                        expected_type = Some(value_type);
                                    }

                                    values.push(value);
                                }
                                let gc_ref = self
                                    .allocator
                                    .write()
                                    .unwrap()
                                    .allocate(Value::Array(values))?;

                                // Trigger GC after allocation
                                let root_set = self.collect_gc_roots();
                                self.collector.write().unwrap().collect(
                                    veld_common::gc::collector::CollectionStrategy::Incremental,
                                    &mut self.allocator.write().unwrap(),
                                    &root_set,
                                    &self.gc_config,
                                )?;

                                Ok(Value::GcRef(gc_ref))
                            }
                            Expr::IndexAccess { object, index } => {
                                let obj_value = self.evaluate_expression(*object)?.unwrap_return();
                                let idx_value = self.evaluate_expression(*index)?.unwrap_return();

                                match obj_value {
                                    Value::Array(elements) => match idx_value {
                                        Value::Integer(i) => {
                                            if i < 0 || i >= elements.len() as i64 {
                                                return Err(VeldError::RuntimeError(format!(
                                                    "Array index out of bounds: {}",
                                                    i
                                                )));
                                            }
                                            Ok(elements[i as usize].clone())
                                        }
                                        Value::Numeric(ref num_val) => {
                                            if let Ok(i) = num_val.to_i32() {
                                                let idx = match i {
                                                    NumericValue::Integer(int_val) => {
                                                        int_val.as_i64().unwrap_or(0)
                                                    }
                                                    _ => {
                                                        return Err(VeldError::RuntimeError(
                                                            "Array index must be an integer"
                                                                .to_string(),
                                                        ));
                                                    }
                                                };
                                                if idx < 0 || idx >= elements.len() as i64 {
                                                    tracing::error!(
                                                        "Array index out of bounds: {} (array length {})",
                                                        idx,
                                                        elements.len()
                                                    );
                                                    return Err(VeldError::RuntimeError(format!(
                                                        r#"Array index out of bounds: index was {}, but length of the elements ("{:?}") is {}"#,
                                                        idx,
                                                        elements,
                                                        elements.len()
                                                    )));
                                                }
                                                Ok(elements[idx as usize].clone())
                                            } else {
                                                Err(VeldError::RuntimeError(
                                                    "Array index must be an integer".to_string(),
                                                ))
                                            }
                                        }
                                        _ => Err(VeldError::RuntimeError(
                                            "Array index must be an integer".to_string(),
                                        )),
                                    },
                                    Value::String(s) => {
                                        // Allow indexing into strings
                                        match idx_value {
                                            Value::Integer(i) => {
                                                if i < 0 || i >= s.len() as i64 {
                                                    return Err(VeldError::RuntimeError(format!(
                                                        "String index out of bounds: {}",
                                                        i
                                                    )));
                                                }

                                                let char_value =
                                                    s.chars().nth(i as usize).unwrap().to_string();
                                                Ok(Value::String(char_value))
                                            }
                                            _ => Err(VeldError::RuntimeError(
                                                "String index must be an integer".to_string(),
                                            )),
                                        }
                                    }
                                    _ => Err(VeldError::RuntimeError(
                                        "Cannot index into non-array value".to_string(),
                                    )),
                                }
                            }
                            Expr::EnumVariant {
                                enum_name,
                                variant_name,
                                fields,
                                type_args,
                            } => {
                                // Check if enum exists
                                if !self.enums.contains_key(&enum_name) {
                                    // Fallback: Check if this is actually a struct method call
                                    if self.structs.contains_key(&enum_name) {
                                        // This is a struct method call like Vec.new()
                                        // Convert it to a proper method call
                                        let struct_expr = Expr::Identifier(enum_name.clone());
                                        let method_call = Expr::MethodCall {
                                            object: Box::new(struct_expr),
                                            method: variant_name.clone(),
                                            arguments: fields
                                                .iter()
                                                .map(|f| Argument::Positional(f.clone()))
                                                .collect(),
                                        };
                                        return self.evaluate_expression(method_call);
                                    }

                                    return Err(VeldError::RuntimeError(format!(
                                        "Undefined enum '{}'",
                                        enum_name
                                    )));
                                }

                                // Evaluate fields
                                let mut field_values = Vec::new();
                                for field in fields {
                                    let val = self.evaluate_expression(field)?;
                                    field_values.push(val.unwrap_return());
                                }

                                Ok(Value::Enum {
                                    enum_name,
                                    variant_name,
                                    fields: field_values,
                                })
                            }
                            Expr::TupleLiteral(elements) => {
                                let mut values = Vec::new();
                                for element in elements {
                                    values.push(self.evaluate_expression(element)?.unwrap_return());
                                }
                                Ok(Value::Tuple(values))
                            }
                            Expr::TupleAccess { tuple, index } => {
                                let tuple_val = self.evaluate_expression(*tuple)?.unwrap_return();

                                match tuple_val {
                                    Value::Tuple(elements) => {
                                        if index < elements.len() {
                                            Ok(elements[index].clone())
                                        } else {
                                            Err(VeldError::RuntimeError(format!(
                                                "Tuple index out of bounds: {}",
                                                index
                                            )))
                                        }
                                    }
                                    _ => Err(VeldError::RuntimeError(
                                        "Cannot access tuple field on non-tuple value".to_string(),
                                    )),
                                }
                            }
                            Expr::BlockExpression {
                                statements,
                                final_expr,
                            } => {
                                let span = tracing::debug_span!(
                                    "block_expression",
                                    statement_count = statements.len()
                                );
                                let _enter = span.enter();

                                // Create new scope for the block
                                self.push_scope();

                                // Execute all statements
                                for stmt in statements {
                                    let result = self.execute_statement(stmt)?;

                                    // Handle early returns
                                    match result {
                                        Value::Return(_) | Value::Break | Value::Continue => {
                                            self.pop_scope();
                                            return Ok(result);
                                        }
                                        _ => {} // Continue with next statement
                                    }
                                }

                                // Evaluate final expression or return unit
                                let result = if let Some(expr) = final_expr {
                                    self.evaluate_expression(*expr)?
                                } else {
                                    Value::Unit
                                };

                                self.pop_scope();
                                Ok(result)
                            }
                            Expr::MacroExpr { name, arguments } => {
                                // Evaluate all arguments
                                let mut evaluated_args = Vec::new();
                                for arg in arguments {
                                    let arg_value = self.evaluate_expression(arg)?;
                                    evaluated_args.push(arg_value);
                                }

                                // Find macro definition
                                let current_mod = self.current_module.clone();
                                let macro_def = self.find_macro(&current_mod, &name)?;

                                match macro_def {
                                    // Pattern-based macro
                                    Statement::MacroDeclaration {
                                        patterns,
                                        body: None,
                                        ..
                                    } => {
                                        self.expand_pattern_macro(&name, &patterns, &evaluated_args)
                                    }
                                    // Procedure macro
                                    Statement::MacroDeclaration {
                                        patterns: _,
                                        body: Some(body),
                                        ..
                                    } => {
                                        self.execute_procedural_macro(&name, &body, &evaluated_args)
                                    }
                                    _ => Err(VeldError::RuntimeError(format!(
                                        "Invalid macro definition for '{name}'"
                                    ))),
                                }
                            }
                            Expr::UnaryOp { operator, operand } => {
                                let operand_value =
                                    self.evaluate_expression(*operand)?.unwrap_return();

                                match operator {
                                    UnaryOperator::Negate => {
                                        match operand_value {
                                            Value::Integer(i) => Ok(Value::Integer(-i)),
                                            Value::Float(f) => Ok(Value::Float(-f)),
                                            Value::Numeric(num) => {
                                                match num {
                                                    NumericValue::Integer(int_val) => {
                                                        let negated = match int_val {
                                                            IntegerValue::I8(v) => {
                                                                IntegerValue::I8(-v)
                                                            }
                                                            IntegerValue::I16(v) => {
                                                                IntegerValue::I16(-v)
                                                            }
                                                            IntegerValue::I32(v) => {
                                                                IntegerValue::I32(-v)
                                                            }
                                                            IntegerValue::I64(v) => {
                                                                IntegerValue::I64(-v)
                                                            }
                                                            // For unsigned types, we will need special handling
                                                            // TODO: handle overflow
                                                            IntegerValue::U8(_)
                                                            | IntegerValue::U16(_)
                                                            | IntegerValue::U32(_)
                                                            | IntegerValue::U64(_) => {
                                                                return Err(VeldError::RuntimeError(
                                                                "Cannot negate unsigned integer"
                                                                    .to_string(),
                                                            ));
                                                            }
                                                        };
                                                        Ok(Value::Numeric(NumericValue::Integer(
                                                            negated,
                                                        )))
                                                    }
                                                    NumericValue::Float(float_val) => {
                                                        let negated = match float_val {
                                                            FloatValue::F32(v) => {
                                                                FloatValue::F32(-v)
                                                            }
                                                            FloatValue::F64(v) => {
                                                                FloatValue::F64(-v)
                                                            }
                                                        };
                                                        Ok(Value::Numeric(NumericValue::Float(
                                                            negated,
                                                        )))
                                                    }
                                                }
                                            }
                                            _ => Err(VeldError::RuntimeError(format!(
                                                "Cannot apply unary negation to {}",
                                                operand_value.type_of()
                                            ))),
                                        }
                                    }
                                    UnaryOperator::Not => match operand_value {
                                        Value::Boolean(b) => Ok(Value::Boolean(!b)),
                                        _ => Err(VeldError::RuntimeError(format!(
                                            "Cannot apply logical NOT to {}",
                                            operand_value.type_of()
                                        ))),
                                    },
                                }
                            }
                            Expr::Range {
                                start,
                                end,
                                inclusive,
                            } => self.evaluate_range(start, end, inclusive),
                            Expr::Record { fields } => self.eval_record_fields(fields),
                            Expr::Match { value, arms } => self.execute_match(*value, arms),
                        }
                    }
                })
            }
        }
    }

    fn eval_record_fields(&mut self, fields: Vec<(String, Expr)>) -> Result<Value> {
        // Evaluate each field expression and collect into a HashMap
        let mut field_values = std::collections::HashMap::new();
        for (field_name, field_expr) in fields {
            let value = self.evaluate_expression(field_expr)?;
            field_values.insert(field_name, value.unwrap_return());
        }
        // Create a Record value (anonymous struct)
        Ok(Value::Record(field_values))
    }

    fn evaluate_range(
        &mut self,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    ) -> Result<Value> {
        let start_val = if let Some(start_expr) = start {
            Some(self.evaluate_expression(*start_expr)?.unwrap_return())
        } else {
            None
        };

        let end_val = if let Some(end_expr) = end {
            Some(self.evaluate_expression(*end_expr)?.unwrap_return())
        } else {
            None
        };

        // Create appropriate range struct based on start/end presence and inclusivity
        match (start_val, end_val, inclusive) {
            // start..stop
            (Some(start), Some(stop), false) => Ok(Value::Struct {
                name: "Range".to_string(),
                fields: {
                    let mut fields = HashMap::new();
                    fields.insert("start".to_string(), start);
                    fields.insert("stop".to_string(), stop);
                    fields
                },
            }),
            // start..=stop
            (Some(start), Some(stop), true) => Ok(Value::Struct {
                name: "RangeInclusive".to_string(),
                fields: {
                    let mut fields = HashMap::new();
                    fields.insert("start".to_string(), start);
                    fields.insert("stop".to_string(), stop);
                    fields
                },
            }),
            // start..
            (Some(start), None, false) => Ok(Value::Struct {
                name: "RangeFrom".to_string(),
                fields: {
                    let mut fields = HashMap::new();
                    fields.insert("start".to_string(), start);
                    fields
                },
            }),
            // start..= (invalid)
            (Some(_start), None, true) => Err(VeldError::RuntimeError(
                "Invalid range: start..= requires an end value".to_string(),
            )),
            // ..stop
            (None, Some(stop), false) => Ok(Value::Struct {
                name: "RangeTo".to_string(),
                fields: {
                    let mut fields = HashMap::new();
                    fields.insert("stop".to_string(), stop);
                    fields
                },
            }),
            // ..=stop
            (None, Some(stop), true) => Ok(Value::Struct {
                name: "RangeToInclusive".to_string(),
                fields: {
                    let mut fields = HashMap::new();
                    fields.insert("stop".to_string(), stop);
                    fields
                },
            }),
            // ..
            (None, None, false) => Ok(Value::Struct {
                name: "RangeFull".to_string(),
                fields: HashMap::new(),
            }),
            // ..= (invalid)
            (None, None, true) => Err(VeldError::RuntimeError(
                "Invalid range: ..= requires a stop value".to_string(),
            )),
        }
    }

    fn find_generic_function(&mut self, name: &str) -> Option<FunctionDeclaration> {
        // Get the current module name
        let current_module_name = self.get_current_module();

        // Get the actual module from the module manager
        if let Some(current_module) = self.module_manager.get_module(&current_module_name) {
            // Search in current module
            for stmt in &current_module.statements {
                if let Statement::FunctionDeclaration {
                    name: fn_name,
                    params,
                    return_type,
                    body,
                    is_proc,
                    is_public: _,
                    generic_params,
                } = stmt
                {
                    if fn_name == name && !generic_params.is_empty() {
                        return Some(FunctionDeclaration {
                            name: fn_name.clone(),
                            params: params.clone(),
                            return_type: return_type.clone(),
                            body: body.clone(),
                            is_proc: *is_proc,
                            is_public: false, // Not relevant here
                            generic_params: generic_params.clone(),
                        });
                    }
                }
            }
        }

        // Check imported modules
        for (module_path, _) in &self.imported_modules {
            if let Ok(module) = self.module_manager.load_module(&[module_path.clone()]) {
                for stmt in &module.statements {
                    if let Statement::FunctionDeclaration {
                        name: fn_name,
                        params,
                        return_type,
                        body,
                        is_proc,
                        is_public,
                        generic_params,
                    } = stmt
                    {
                        if fn_name == name && !generic_params.is_empty() && *is_public {
                            return Some(FunctionDeclaration {
                                name: fn_name.clone(),
                                params: params.clone(),
                                return_type: return_type.clone(),
                                body: body.clone(),
                                is_proc: *is_proc,
                                is_public: *is_public,
                                generic_params: generic_params.clone(),
                            });
                        }
                    }
                }
            }
        }

        None
    }

    // Infer type arguments from values
    fn infer_type_args_from_values(
        &self,
        function: &FunctionDeclaration,
        args: &[Value],
    ) -> Result<Vec<Type>> {
        let mut type_args = Vec::new();

        // For each generic parameter, try to infer its type
        for param in &function.generic_params {
            // TODO: implement more sophisticated inference

            // Find where this type parameter is used in the function parameters
            let mut inferred_type = None;

            for (i, (_, param_type)) in function.params.iter().enumerate() {
                if let Some(arg) = args.get(i) {
                    // Check if this parameter uses our generic type
                    if self.uses_type_param(param_type, &param.type_annotation) {
                        // Get the type of the argument value
                        let arg_type = arg.type_of();
                        inferred_type = Some(arg_type);
                        break;
                    }
                }
            }

            if let Some(ty) = inferred_type {
                type_args.push(ty);
            } else {
                return Err(VeldError::RuntimeError(
                    "Could not infer generic type parameter".to_string(),
                ));
            }
        }

        Ok(type_args)
    }

    // Helper method to check if a type annotation uses a specific type parameter
    fn uses_type_param(&self, type_: &TypeAnnotation, type_param: &TypeAnnotation) -> bool {
        match (type_, type_param) {
            (TypeAnnotation::Basic(a), TypeAnnotation::Basic(b)) => a == b,
            (TypeAnnotation::Generic { type_args, .. }, _) => type_args
                .iter()
                .any(|arg| self.uses_type_param(arg, type_param)),
            (TypeAnnotation::Array(elem), _) => self.uses_type_param(elem, type_param),
            // Add more cases as needed
            _ => false,
        }
    }

    // Helper to substitute type parameters in annotations
    fn substitute_type_params_in_annotation(
        &self,
        type_: &TypeAnnotation,
        type_params: &[GenericArgument],
        type_args: &[Type],
    ) -> TypeAnnotation {
        match type_ {
            TypeAnnotation::Basic(name) => {
                // Check if this is a type parameter
                for (i, param) in type_params.iter().enumerate() {
                    if let TypeAnnotation::Basic(param_name) = &param.type_annotation {
                        if name == param_name {
                            // Convert the type argument to a type annotation
                            return self.type_to_annotation(&type_args[i]);
                        }
                    }
                }
                // Not a type parameter, keep as is
                type_.clone()
            }
            TypeAnnotation::Generic {
                base,
                type_args: params,
            } => {
                // Substitute in the type arguments
                let new_args = params
                    .iter()
                    .map(|arg| {
                        self.substitute_type_params_in_annotation(arg, type_params, type_args)
                    })
                    .collect();

                TypeAnnotation::Generic {
                    base: base.clone(),
                    type_args: new_args,
                }
            }
            TypeAnnotation::Array(elem) => TypeAnnotation::Array(Box::new(
                self.substitute_type_params_in_annotation(elem, type_params, type_args),
            )),
            // Add other cases as needed
            _ => type_.clone(),
        }
    }

    // Helper to convert Type to TypeAnnotation
    fn type_to_annotation(&self, type_: &Type) -> TypeAnnotation {
        match type_ {
            // Integer types
            Type::I8 => TypeAnnotation::Basic("i8".to_string()),
            Type::I16 => TypeAnnotation::Basic("i16".to_string()),
            Type::I32 => TypeAnnotation::Basic("i32".to_string()),
            Type::I64 => TypeAnnotation::Basic("i64".to_string()),

            // Unsigned integer types
            Type::U8 => TypeAnnotation::Basic("u8".to_string()),
            Type::U16 => TypeAnnotation::Basic("u16".to_string()),
            Type::U32 => TypeAnnotation::Basic("u32".to_string()),
            Type::U64 => TypeAnnotation::Basic("u64".to_string()),

            // Floating point types
            Type::F32 => TypeAnnotation::Basic("f32".to_string()),
            Type::F64 => TypeAnnotation::Basic("f64".to_string()),

            // Other basic types
            Type::Bool => TypeAnnotation::Basic("bool".to_string()),
            Type::String => TypeAnnotation::Basic("str".to_string()),
            Type::Char => TypeAnnotation::Basic("char".to_string()),
            Type::Unit => TypeAnnotation::Unit,
            Type::Any => TypeAnnotation::Basic("any".to_string()),

            // Number type (generic numeric)
            Type::Number => TypeAnnotation::Basic("Number".to_string()),

            // Literal types (which may need special handling)
            Type::IntegerLiteral(val) => TypeAnnotation::Basic(format!("int({})", val)),
            Type::FloatLiteral(val) => TypeAnnotation::Basic(format!("float({})", val)),

            // Function types
            Type::Function {
                params,
                return_type,
            } => {
                let param_types: Vec<TypeAnnotation> =
                    params.iter().map(|p| self.type_to_annotation(p)).collect();
                let return_annotation = Box::new(self.type_to_annotation(return_type));

                TypeAnnotation::Function {
                    params: param_types,
                    return_type: return_annotation,
                }
            }

            Type::Struct { name: _, fields: _ } => todo!("Handle struct types"),

            // Generic types
            Type::Generic { base, type_args } => {
                let type_arg_annotations: Vec<TypeAnnotation> = type_args
                    .iter()
                    .map(|arg| self.type_to_annotation(arg))
                    .collect();

                TypeAnnotation::Generic {
                    base: base.clone(),
                    type_args: type_arg_annotations,
                }
            }

            // Type parameters
            Type::TypeParam(name) => TypeAnnotation::Basic(name.clone()),

            // Enum types
            Type::Enum { name, variants: _ } => {
                // Simple conversion for enums
                TypeAnnotation::Basic(name.clone())
                // TODO: Handle enum variants properly
            }

            // Tuple types
            Type::Tuple(types) => {
                let type_annotations: Vec<TypeAnnotation> =
                    types.iter().map(|t| self.type_to_annotation(t)).collect();

                TypeAnnotation::Tuple(type_annotations)
            }

            // Array types
            Type::Array(element_type) => {
                let elem_annotation = self.type_to_annotation(element_type);
                TypeAnnotation::Array(Box::new(elem_annotation))
            }

            // Self type in kinds
            Type::KindSelf(_) => TypeAnnotation::Basic("Self".to_string()),

            // Type variables
            Type::TypeVar(id) => TypeAnnotation::Basic(format!("T{}", id)),

            // Add other cases as needed
            _ => TypeAnnotation::Basic(format!("{:?}", type_)),
        }
    }

    fn find_macro(&self, module_name: &str, macro_name: &str) -> Result<Statement> {
        // Check current module first
        if let Some(module) = self.module_manager.get_module(module_name) {
            for stmt in &module.statements {
                if let Statement::MacroDeclaration { name, .. } = stmt {
                    if name == macro_name {
                        return Ok(stmt.clone());
                    }
                }
            }
        }

        // Check imported modules
        for (_, imported_module_name) in &self.imported_modules {
            if let Some(module) = self.module_manager.get_module(imported_module_name) {
                for stmt in &module.statements {
                    if let Statement::MacroDeclaration { name, .. } = stmt {
                        if name == macro_name {
                            return Ok(stmt.clone());
                        }
                    }
                }
            }
        }

        Err(VeldError::RuntimeError(format!(
            "Macro '{macro_name}' not found'"
        )))
    }

    fn expand_pattern_macro(
        &mut self,
        macro_name: &str,
        patterns: &[(MacroPattern, MacroExpansion)],
        args: &[Value],
    ) -> Result<Value> {
        let args_str = args
            .iter()
            .map(|v| self.value_to_string(v))
            .collect::<Result<Vec<_>>>()?
            .join(" ");

        for (pattern, expansion) in patterns {
            // Try to match the pattern and extract bindings
            if let Some(bindings) = self.extract_pattern_bindings(&pattern.0, &args_str, args) {
                // Execute expansion with the extracted bindings
                return self.execute_macro_expansion_with_bindings(expansion, &bindings);
            }
        }

        Err(VeldError::RuntimeError(format!(
            "No matching pattern found for macro '{}' with arguments: {}",
            macro_name, args_str
        )))
    }

    fn extract_pattern_bindings(
        &self,
        pattern: &str,
        arg_str: &str,
        original_args: &[Value],
    ) -> Option<HashMap<String, Value>> {
        let pattern_tokens = self.tokenize_pattern(pattern);
        let args_tokens = self.tokenize_args(arg_str);

        let mut bindings = HashMap::new();
        let mut p_idx = 0;
        let mut a_idx = 0;

        while p_idx < pattern_tokens.len() && a_idx < args_tokens.len() {
            match &pattern_tokens[p_idx] {
                PatternToken::Literal(lit) => {
                    if &args_tokens[a_idx] != lit {
                        return None; // Pattern does not match
                    }
                    a_idx += 1;
                }
                PatternToken::Variable(var_name) => {
                    if a_idx < original_args.len() {
                        bindings.insert(var_name.clone(), original_args[a_idx].clone());
                    }
                    a_idx += 1;
                }
                PatternToken::Repetition {
                    variable,
                    separator,
                    ..
                } => {
                    // Handle repetition and collect all matches
                    let mut collected_values = Vec::new();
                    let mut matched = 0;

                    while a_idx < args_tokens.len() {
                        // Check for separator if not first match
                        if matched > 0 && separator.is_some() {
                            if a_idx >= args_tokens.len()
                                || &args_tokens[a_idx] != separator.as_ref().unwrap()
                            {
                                break;
                            }
                            a_idx += 1; // Skip separator
                        }

                        // Check if we hit the next pattern token
                        if p_idx + 1 < pattern_tokens.len() {
                            if let PatternToken::Literal(next_lit) = &pattern_tokens[p_idx + 1] {
                                if &args_tokens[a_idx] == next_lit {
                                    break;
                                }
                            }
                        }

                        if a_idx < original_args.len() {
                            collected_values.push(original_args[a_idx].clone());
                        }

                        matched += 1;
                        a_idx += 1;
                    }

                    // Store the collected values as an array
                    let gc_ref = self
                        .allocator
                        .write()
                        .unwrap()
                        .allocate(Value::Array(collected_values))
                        .ok()?;

                    // Trigger GC after allocation
                    let root_set = self.collect_gc_roots();
                    self.collector.write().unwrap().collect(
                        veld_common::gc::collector::CollectionStrategy::Incremental,
                        &mut self.allocator.write().unwrap(),
                        &root_set,
                        &self.gc_config,
                    );

                    bindings.insert(variable.clone(), Value::GcRef(gc_ref));
                }
            }
            p_idx += 1;
        }
        // Only return bindings if the entire pattern was matched
        if p_idx >= pattern_tokens.len() && a_idx >= args_tokens.len() {
            Some(bindings)
        } else {
            None
        }
    }

    fn execute_macro_expansion_with_bindings(
        &mut self,
        expansion: &MacroExpansion,
        bindings: &HashMap<String, Value>,
    ) -> Result<Value> {
        self.push_scope();

        // Bind extracted variables to the scope
        for (var_name, value) in bindings {
            self.current_scope_mut()
                .declare(var_name.clone(), value.clone(), VarKind::Let)?;
        }

        // Execute expansion statements
        let mut result = Value::Unit;
        for stmt in &expansion.0 {
            // Replace variables in statement before execution
            let expanded_stmt = self.substitute_variables_in_statement(stmt, bindings)?;
            match self.execute_statement(expanded_stmt) {
                Ok(value) => {
                    result = value;
                }
                Err(e) => {
                    self.pop_scope();
                    return Err(e);
                }
            }
        }
        self.pop_scope();
        Ok(result)
    }

    fn substitute_variables_in_statement(
        &self,
        stmt: &Statement,
        bindings: &HashMap<String, Value>,
    ) -> Result<Statement> {
        // Handle only the most common cases for now
        match stmt {
            Statement::ExprStatement(expr) => {
                let new_expr = self.substitute_variables_in_expression(expr, bindings)?;
                Ok(Statement::ExprStatement(new_expr))
            }
            Statement::Return(Some(expr)) => {
                let new_expr = self.substitute_variables_in_expression(expr, bindings)?;
                Ok(Statement::Return(Some(new_expr)))
            }
            // For other statements, just return the original statements for now
            _ => Ok(stmt.clone()),
        }
    }

    fn substitute_variables_in_expression(
        &self,
        expr: &Expr,
        bindings: &HashMap<String, Value>,
    ) -> Result<Expr> {
        match expr {
            Expr::Identifier(name) => {
                if let Some(value) = bindings.get(name) {
                    // Convert the value back to an expression
                    Ok(self.value_to_expr(value.clone())?)
                } else {
                    // If bindings not found, keep the original identifier
                    Ok(expr.clone())
                }
            }
            Expr::BinaryOp {
                left,
                operator,
                right: _,
            } => {
                let new_left = self.substitute_variables_in_expression(left, bindings)?;
                let new_right = self.substitute_variables_in_expression(expr, bindings)?;
                Ok(Expr::BinaryOp {
                    left: Box::new(new_left),
                    operator: operator.clone(),
                    right: Box::new(new_right),
                })
            }
            Expr::UnaryOp { operator, operand } => Ok(Expr::UnaryOp {
                operator: operator.clone(),
                operand: Box::new(self.substitute_variables_in_expression(operand, bindings)?),
            }),

            // For other expressions, just return the original for now
            // TODO: Handle other expression types
            _ => Ok(expr.clone()),
        }
    }

    fn tokenize_pattern(&self, pattern: &str) -> Vec<PatternToken> {
        let mut tokens = Vec::new();
        let mut chars = pattern.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '$' => {
                    // Check for repetition pattern: $($name)+, $($name)*, $($name),+, $($name),*
                    if chars.peek() == Some(&'(') {
                        chars.next(); // Consume '('
                        if chars.peek() == Some(&'$') {
                            chars.next(); // Consume '$'

                            // Read the variable name
                            let mut var_name = String::new();
                            while let Some(&next_c) = chars.peek() {
                                if next_c.is_alphanumeric() || next_c == '_' || next_c == ':' {
                                    var_name.push(chars.next().unwrap());
                                } else {
                                    break;
                                }
                            }

                            // Consume closing ')'
                            if chars.peek() == Some(&')') {
                                chars.peek();
                            } else {
                                // Syntax error, handle as literal
                                tokens.push(PatternToken::Literal(format!("$({})", var_name)));
                                continue;
                            }

                            // Check for repetition symbols
                            let mut seperator = None;
                            let min = match chars.peek() {
                                Some(&'+') => {
                                    chars.next();
                                    1 // One or more
                                }
                                Some(&'*') => {
                                    chars.next();
                                    0 // Zero or more
                                }
                                Some(&',') => {
                                    chars.next();
                                    seperator = Some(','.to_string());

                                    match chars.peek() {
                                        Some(&'+') => {
                                            chars.next();
                                            1 // One or more with separator
                                        }
                                        Some(&'*') => {
                                            chars.next();
                                            0 // Zero or more with separator
                                        }
                                        _ => {
                                            // Just a comma, no repetition
                                            tokens.push(PatternToken::Variable(var_name));
                                            tokens.push(PatternToken::Literal(",".to_string()));
                                            continue;
                                        }
                                    }
                                }
                                _ => {
                                    // Not a repetition, just a grouped variable
                                    tokens.push(PatternToken::Variable(var_name));
                                    continue;
                                }
                            };
                            tokens.push(PatternToken::Repetition {
                                variable: var_name,
                                separator: seperator,
                                min,
                            });
                        } else {
                            // Just $(something) without repetition
                            // Handle as literal for now
                            tokens.push(PatternToken::Literal("$(".to_string()));
                            // TODO: Handle the rest of the pattern branch
                        }
                    } else {
                        // Simple variable $name
                        let mut var_name = String::new();
                        while let Some(&next_c) = chars.peek() {
                            if next_c.is_alphanumeric() || next_c == '_' || next_c == ':' {
                                var_name.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        if !var_name.is_empty() {
                            tokens.push(PatternToken::Variable(var_name));
                        } else {
                            tokens.push(PatternToken::Literal("$".to_string()));
                        }
                    }
                }
                // TODO: Handle other special characters like `*`, `+`, etc.
                _ => {
                    let mut literal = String::new();
                    literal.push(c);

                    while let Some(&next_c) = chars.peek() {
                        if next_c != '$' && !next_c.is_whitespace() {
                            literal.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(PatternToken::Literal(literal));
                }
            }
        }
        tokens
    }

    fn tokenize_args(&self, args: &str) -> Vec<String> {
        // Simple tokenization by whitespace
        args.split_whitespace().map(|s| s.to_string()).collect()
    }

    fn execute_procedural_macro(
        &mut self,
        macro_name: &str,
        body: &[Statement],
        args: &[Value],
    ) -> Result<Value> {
        self.push_scope();

        // TODO: Change to properly bind named parameters
        for (i, arg) in args.iter().enumerate() {
            let param_name = format!("${}", i + 1);
            self.current_scope_mut()
                .declare(param_name.clone(), arg.clone(), VarKind::Let)?;
        }

        // Execute the macro body
        let mut result = Value::Unit;
        for stmt in body {
            match self.execute_statement(stmt.clone()) {
                Ok(value) => {
                    result = value;
                }

                Err(e) => {
                    self.pop_scope();
                    return Err(e);
                }
            }
        }

        self.pop_scope();
        Ok(result)
    }

    fn value_to_string(&self, value: &Value) -> Result<String> {
        match value {
            Value::Integer(i) => Ok(i.to_string()),
            Value::Float(f) => Ok(f.to_string()),
            Value::String(s) => Ok(s.clone()),
            Value::Boolean(b) => Ok(b.to_string()),
            Value::Char(c) => Ok(c.to_string()),
            Value::Struct { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.value_to_string(v).unwrap_or_default()))
                    .collect::<Vec<_>>()
                    .join(", ");
                Ok(format!("{}({})", name, fields_str))
            }
            _ => Ok(format!("{:?}", value)),
        }
    }

    fn cast_value(&self, value: Value, target_type: &Type) -> Result<Value> {
        match (value.clone(), target_type) {
            // Integer to integer casts
            (Value::Integer(i), Type::I8) => {
                if i >= i8::MIN as i64 && i <= i8::MAX as i64 {
                    Ok(Value::Integer(i as i8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i8: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::I16) => {
                if i >= i16::MIN as i64 && i <= i16::MAX as i64 {
                    Ok(Value::Integer(i as i16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i16: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::I32) => {
                if i >= i32::MIN as i64 && i <= i32::MAX as i64 {
                    Ok(Value::Integer(i as i32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i32: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::I64) => Ok(Value::Integer(i)),

            // Integer to unsigned casts
            (Value::Integer(i), Type::U8) => {
                if i >= 0 && i <= u8::MAX as i64 {
                    Ok(Value::Integer(i as u8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u8: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::U16) => {
                if i >= 0 && i <= u16::MAX as i64 {
                    Ok(Value::Integer(i as u16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u16: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::U32) => {
                if i >= 0 && i <= u32::MAX as i64 {
                    Ok(Value::Integer(i as u32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u32: out of range",
                        i
                    )))
                }
            }
            (Value::Integer(i), Type::U64) => {
                if i >= 0 {
                    Ok(Value::Integer(i as u64 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u64: negative value",
                        i
                    )))
                }
            }

            // Integer to float casts
            (Value::Integer(i), Type::F32) => Ok(Value::Float(i as f32 as f64)),
            (Value::Integer(i), Type::F64) => Ok(Value::Float(i as f64)),

            // Float to integer casts (with truncation)
            (Value::Float(f), Type::I8) => {
                if f >= i8::MIN as f64 && f <= i8::MAX as f64 {
                    Ok(Value::Integer(f as i8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i8: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::I16) => {
                if f >= i16::MIN as f64 && f <= i16::MAX as f64 {
                    Ok(Value::Integer(f as i16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i16: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::I32) => {
                if f >= i32::MIN as f64 && f <= i32::MAX as f64 {
                    Ok(Value::Integer(f as i32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i32: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::I64) => {
                if f >= i64::MIN as f64 && f <= i64::MAX as f64 {
                    Ok(Value::Integer(f as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to i64: out of range",
                        f
                    )))
                }
            }

            // Float to unsigned casts
            (Value::Float(f), Type::U8) => {
                if f >= 0.0 && f <= u8::MAX as f64 {
                    Ok(Value::Integer(f as u8 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u8: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::U16) => {
                if f >= 0.0 && f <= u16::MAX as f64 {
                    Ok(Value::Integer(f as u16 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u16: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::U32) => {
                if f >= 0.0 && f <= u32::MAX as f64 {
                    Ok(Value::Integer(f as u32 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u32: out of range",
                        f
                    )))
                }
            }
            (Value::Float(f), Type::U64) => {
                if f >= 0.0 && f <= u64::MAX as f64 {
                    Ok(Value::Integer(f as u64 as i64))
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot cast {} to u64: out of range",
                        f
                    )))
                }
            }

            // Float to float casts
            (Value::Float(f), Type::F32) => Ok(Value::Float(f as f32 as f64)),
            (Value::Float(f), Type::F64) => Ok(Value::Float(f)),
            (Value::Numeric(numeric_val), _) /*if target_ty.is_numeric()*/ => {
                let type_annotation = self.type_to_annotation(target_type);
                self.cast_val_to_num(Value::Numeric(numeric_val), &type_annotation)
            }

            // String conversions
            (Value::String(s), Type::I32) => match s.parse::<i32>() {
                Ok(i) => Ok(Value::Integer(i as i64)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as i32",
                    s
                ))),
            },
            (Value::String(s), Type::F64) => match s.parse::<f64>() {
                Ok(f) => Ok(Value::Float(f)),
                Err(_) => Err(VeldError::RuntimeError(format!(
                    "Cannot parse '{}' as f64",
                    s
                ))),
            },

            // To string conversions
            (Value::Integer(i), Type::String) => Ok(Value::String(i.to_string())),
            (Value::Float(f), Type::String) => Ok(Value::String(f.to_string())),

            // Record type casting - allow if structurally compatible
            (
                Value::Record(source_fields),
                Type::Record { fields: target_fields },
            ) => {
                let mut result_fields = std::collections::HashMap::new();

                // Check if all target fields exist in source and cast them
                for (field_name, target_field_type) in target_fields {
                    if let Some(source_value) = source_fields.get(field_name) {
                        // Recursively cast the field value to the target type
                        let casted_field = self.cast_value(source_value.clone(), target_field_type)?;
                        result_fields.insert(field_name.clone(), casted_field);
                    } else {
                        return Err(VeldError::RuntimeError(format!(
                            "Field '{}' not found in source record",
                            field_name
                        )));
                    }
                }

                Ok(Value::Record(result_fields))
            }

            // Same type conversion (no-op)
            (v, t) if v.type_of() == *t => Ok(v),

            // Invalid casts
            _ => Err(VeldError::RuntimeError(format!(
                "Invalid cast from {:?} to {:?}",
                value.type_of(),
                target_type
            ))),
        }
    }

    fn initialize_core_capabilities(&mut self) {
        // --- Implement ToString for built-in types ---

        // String - just return self
        self.native_method_registry
            .register("str", "to_string", |_, args| {
                if let Some(Value::String(s)) = args.get(0) {
                    Ok(Value::String(s.clone()))
                } else {
                    Err(VeldError::RuntimeError(
                        "to_string called on non-string".to_string(),
                    ))
                }
            });

        // All integer types - handle both old Value::Integer and new Value::Numeric
        let register_integer_to_string =
            |registry: &mut crate::native::NativeMethodRegistry, type_name: &str| {
                let type_name_owned = type_name.to_string();
                registry.register(type_name, "to_string", move |_, args| match args.get(0) {
                    Some(Value::Integer(n)) => Ok(Value::String(n.to_string())),
                    Some(Value::Numeric(numeric_val)) => {
                        let s = match numeric_val {
                            NumericValue::Integer(iv) => match iv {
                                IntegerValue::I8(v) => v.to_string(),
                                IntegerValue::I16(v) => v.to_string(),
                                IntegerValue::I32(v) => v.to_string(),
                                IntegerValue::I64(v) => v.to_string(),
                                IntegerValue::U8(v) => v.to_string(),
                                IntegerValue::U16(v) => v.to_string(),
                                IntegerValue::U32(v) => v.to_string(),
                                IntegerValue::U64(v) => v.to_string(),
                            },
                            NumericValue::Float(fv) => match fv {
                                FloatValue::F32(v) => v.to_string(),
                                FloatValue::F64(v) => v.to_string(),
                            },
                        };
                        Ok(Value::String(s))
                    }
                    _ => Err(VeldError::RuntimeError(format!(
                        "to_string called on non-numeric value for type {}",
                        type_name_owned
                    ))),
                });
            };

        // Register to_string for all integer types
        register_integer_to_string(&mut self.native_method_registry, "i8");
        register_integer_to_string(&mut self.native_method_registry, "i16");
        register_integer_to_string(&mut self.native_method_registry, "i32");
        register_integer_to_string(&mut self.native_method_registry, "i64");
        register_integer_to_string(&mut self.native_method_registry, "u8");
        register_integer_to_string(&mut self.native_method_registry, "u16");
        register_integer_to_string(&mut self.native_method_registry, "u32");
        register_integer_to_string(&mut self.native_method_registry, "u64");

        // Float types
        let register_float_to_string = |registry: &mut crate::native::NativeMethodRegistry,
                                        type_name: &str| {
            let type_name_owned = type_name.to_string();
            registry.register(type_name, "to_string", move |_, args| match args.get(0) {
                Some(Value::Float(n)) => Ok(Value::String(n.to_string())),
                Some(Value::Numeric(numeric_val)) => {
                    let s = match numeric_val {
                        NumericValue::Float(fv) => match fv {
                            FloatValue::F32(v) => v.to_string(),
                            FloatValue::F64(v) => v.to_string(),
                        },
                        NumericValue::Integer(iv) => match iv {
                            IntegerValue::I8(v) => v.to_string(),
                            IntegerValue::I16(v) => v.to_string(),
                            IntegerValue::I32(v) => v.to_string(),
                            IntegerValue::I64(v) => v.to_string(),
                            IntegerValue::U8(v) => v.to_string(),
                            IntegerValue::U16(v) => v.to_string(),
                            IntegerValue::U32(v) => v.to_string(),
                            IntegerValue::U64(v) => v.to_string(),
                        },
                    };
                    Ok(Value::String(s))
                }
                _ => Err(VeldError::RuntimeError(format!(
                    "to_string called on non-numeric value for type {}",
                    type_name_owned
                ))),
            });
        };

        register_float_to_string(&mut self.native_method_registry, "f32");
        register_float_to_string(&mut self.native_method_registry, "f64");

        // Boolean
        self.native_method_registry
            .register("bool", "to_string", |_, args| {
                if let Some(Value::Boolean(b)) = args.get(0) {
                    Ok(Value::String(if *b { "true" } else { "false" }.to_string()))
                } else {
                    Err(VeldError::RuntimeError(
                        "to_string called on non-boolean".to_string(),
                    ))
                }
            });

        // Character
        self.native_method_registry
            .register("char", "to_string", |_, args| {
                if let Some(Value::Char(c)) = args.get(0) {
                    Ok(Value::String(c.to_string()))
                } else {
                    Err(VeldError::RuntimeError(
                        "to_string called on non-character".to_string(),
                    ))
                }
            });

        // --- Register method types for type checker ---

        // to_string method type: (self) -> str
        let to_string_method_type = Type::Function {
            params: vec![Type::Any], // self parameter
            return_type: Box::new(Type::String),
        };

        // Register to_string for built-in types in type checker
        self.type_checker.env().add_struct_method(
            "str",
            "to_string",
            to_string_method_type.clone(),
        );
        // Register to_string method type for all numeric types in type checker
        let numeric_types = [
            "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
        ];
        for numeric_type in &numeric_types {
            self.type_checker.env().add_struct_method(
                numeric_type,
                "to_string",
                to_string_method_type.clone(),
            );
        }

        self.type_checker.env().add_struct_method(
            "bool",
            "to_string",
            to_string_method_type.clone(),
        );

        self.type_checker.env().add_struct_method(
            "char",
            "to_string",
            to_string_method_type.clone(),
        );

        // --- Implement Sized for collection types ---

        // String size (length)
        self.native_method_registry
            .register("str", "size", |_, args| {
                if let Some(Value::String(s)) = args.get(0) {
                    Ok(Value::Integer(s.len() as i64))
                } else {
                    Err(VeldError::RuntimeError(
                        "size called on non-string".to_string(),
                    ))
                }
            });

        // Array size (length)
        self.native_method_registry
            .register("array", "size", |_, args| {
                if let Some(Value::Array(arr)) = args.get(0) {
                    Ok(Value::Integer(arr.len() as i64))
                } else {
                    Err(VeldError::RuntimeError(
                        "size called on non-array".to_string(),
                    ))
                }
            });

        // Empty check for collections
        self.native_method_registry
            .register("str", "is_empty", |_, args| {
                if let Some(Value::String(s)) = args.get(0) {
                    Ok(Value::Boolean(s.is_empty()))
                } else {
                    Err(VeldError::RuntimeError(
                        "is_empty called on non-string".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("array", "is_empty", |interpreter, args| {
                if let Some(Value::Array(arr)) = args.get(0) {
                    Ok(Value::Boolean(arr.is_empty()))
                } else {
                    Err(VeldError::RuntimeError(
                        "is_empty called on non-array".to_string(),
                    ))
                }
            });

        // --- Implement Value core kind ---

        // Get type name as string
        self.native_method_registry
            .register("any", "type_name", |_, args| {
                if let Some(value) = args.get(0) {
                    Ok(Value::String(value.type_of().to_string()))
                } else {
                    Err(VeldError::RuntimeError(
                        "type_name called with no arguments".to_string(),
                    ))
                }
            });

        // --- Register additional method types for type checker ---

        // size method type: (self) -> i32
        let size_method_type = Type::Function {
            params: vec![Type::Any], // self parameter
            return_type: Box::new(Type::I32),
        };

        // is_empty method type: (self) -> bool
        let is_empty_method_type = Type::Function {
            params: vec![Type::Any], // self parameter
            return_type: Box::new(Type::Bool),
        };

        // type_name method type: (self) -> str
        let type_name_method_type = Type::Function {
            params: vec![Type::Any], // self parameter
            return_type: Box::new(Type::String),
        };

        // Register collection methods in type checker
        self.type_checker
            .env()
            .add_struct_method("str", "size", size_method_type.clone());
        self.type_checker
            .env()
            .add_struct_method("str", "is_empty", is_empty_method_type.clone());
        self.type_checker
            .env()
            .add_struct_method("array", "size", size_method_type.clone());
        self.type_checker.env().add_struct_method(
            "array",
            "is_empty",
            is_empty_method_type.clone(),
        );
        self.type_checker.env().add_struct_method(
            "any",
            "type_name",
            type_name_method_type.clone(),
        );
    }

    fn initialize_string_capabilities(&mut self) {
        // --- Implement Transformable for strings ---

        // Case transformations
        self.native_method_registry
            .register_string_method("to_upper", |s| s.to_uppercase());
        self.native_method_registry
            .register_string_method("to_lower", |s| s.to_lowercase());

        // Trimming
        self.native_method_registry
            .register_string_method("trim", |s| s.trim().to_string());
        self.native_method_registry
            .register_string_method("trim_start", |s| s.trim_start().to_string());
        self.native_method_registry
            .register_string_method("trim_end", |s| s.trim_end().to_string());

        // --- Implement Searchable for strings ---

        // Search operations
        self.native_method_registry
            .register_string_bool_method_with_string_param("contains", |s, substr| {
                s.contains(substr)
            });
        self.native_method_registry
            .register_string_bool_method_with_string_param("starts_with", |s, prefix| {
                s.starts_with(prefix)
            });
        self.native_method_registry
            .register_string_bool_method_with_string_param("ends_with", |s, suffix| {
                s.ends_with(suffix)
            });

        // Find index
        self.native_method_registry
            .register("str", "index_of", |_, args| {
                if let (Some(Value::String(s)), Some(Value::String(substring))) =
                    (args.get(0), args.get(1))
                {
                    match s.find(substring) {
                        Some(index) => Ok(Value::Integer(index as i64)),
                        None => Ok(Value::Integer(-1)),
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        "index_of called with invalid arguments".to_string(),
                    ))
                }
            });

        // --- Implement Manipulatable for strings ---

        // Substring
        self.native_method_registry
            .register("str", "substring", |_, args| {
                use veld_common::value::numeric::IntegerValue;
                if let (
                    Some(Value::String(s)),
                    Some(Value::Numeric(NumericValue::Integer(start))),
                    Some(Value::Numeric(NumericValue::Integer(end))),
                ) = (args.get(0), args.get(1), args.get(2))
                {
                    // Convert IntegerValue to i64, then to usize
                    fn integer_value_to_usize(iv: &IntegerValue) -> Option<usize> {
                        match iv {
                            IntegerValue::I8(v) => Some(*v as usize),
                            IntegerValue::I16(v) => Some(*v as usize),
                            IntegerValue::I32(v) => Some(*v as usize),
                            IntegerValue::I64(v) => Some(*v as usize),
                            IntegerValue::U8(v) => Some(*v as usize),
                            IntegerValue::U16(v) => Some(*v as usize),
                            IntegerValue::U32(v) => Some(*v as usize),
                            IntegerValue::U64(v) => Some(*v as usize),
                        }
                    }
                    let start = integer_value_to_usize(start).unwrap_or(0);
                    let end = integer_value_to_usize(end).unwrap_or(0).min(s.len());

                    if start <= end && start <= s.len() {
                        let substring = if start == s.len() {
                            "".to_string()
                        } else {
                            s[start..end].to_string()
                        };
                        Ok(Value::String(substring))
                    } else {
                        Ok(Value::String("".to_string()))
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        format!("substring called with invalid arguments: expected string, integer, integer, got {:?}", args),
                    ))
                }
            });

        // Split
        self.native_method_registry
            .register("str", "split", |interpreter, args| {
                if let (Some(Value::String(s)), Some(Value::String(delimiter))) =
                    (args.get(0), args.get(1))
                {
                    let parts: Vec<String> = s.split(delimiter).map(|s| s.to_string()).collect();
                    let values: Vec<Value> = parts.into_iter().map(Value::String).collect();
                    let gc_ref = interpreter
                        .allocator
                        .write()
                        .unwrap()
                        .allocate(Value::Array(values))?;

                    // Trigger GC after allocation
                    let root_set = interpreter.collect_gc_roots();
                    interpreter.collector.write().unwrap().collect(
                        veld_common::gc::collector::CollectionStrategy::Incremental,
                        &mut interpreter.allocator.write().unwrap(),
                        &root_set,
                        &interpreter.gc_config,
                    );

                    Ok(Value::GcRef(gc_ref))
                } else {
                    Err(VeldError::RuntimeError(
                        "split called with invalid arguments".to_string(),
                    ))
                }
            });

        // Replace
        self.native_method_registry
            .register("str", "replace", |_, args| {
                if let (
                    Some(Value::String(s)),
                    Some(Value::String(from)),
                    Some(Value::String(to)),
                ) = (args.get(0), args.get(1), args.get(2))
                {
                    Ok(Value::String(s.replace(from, to)))
                } else {
                    Err(VeldError::RuntimeError(
                        "replace called with invalid arguments".to_string(),
                    ))
                }
            });

        // Repeat
        self.native_method_registry
            .register("str", "repeat", |_, args| {
                use veld_common::value::numeric::IntegerValue;
                if let (
                    Some(Value::String(s)),
                    Some(Value::Numeric(NumericValue::Integer(count))),
                ) = (args.get(0), args.get(1))
                {
                    // Convert IntegerValue to usize properly
                    fn integer_value_to_usize(iv: &IntegerValue) -> Option<usize> {
                        match iv {
                            IntegerValue::I8(v) => Some(*v as usize),
                            IntegerValue::I16(v) => Some(*v as usize),
                            IntegerValue::I32(v) => Some(*v as usize),
                            IntegerValue::I64(v) => Some(*v as usize),
                            IntegerValue::U8(v) => Some(*v as usize),
                            IntegerValue::U16(v) => Some(*v as usize),
                            IntegerValue::U32(v) => Some(*v as usize),
                            IntegerValue::U64(v) => Some(*v as usize),
                        }
                    }
                    let count_usize = integer_value_to_usize(count).unwrap_or(0);
                    if count_usize == 0 {
                        return Ok(Value::String("".to_string()));
                    }
                    Ok(Value::String(s.repeat(count_usize)))
                } else {
                    Err(VeldError::RuntimeError(
                        "repeat called with invalid arguments".to_string(),
                    ))
                }
            });

        // Padding
        self.native_method_registry
            .register("str", "pad_start", |_, args| {
                use veld_common::value::numeric::IntegerValue;
                if let (
                    Some(Value::String(s)),
                    Some(Value::Numeric(NumericValue::Integer(length))),
                    Some(Value::String(pad_char)),
                ) = (args.get(0), args.get(1), args.get(2))
                {
                    // Convert IntegerValue to usize properly
                    fn integer_value_to_usize(iv: &IntegerValue) -> Option<usize> {
                        match iv {
                            IntegerValue::I8(v) => Some(*v as usize),
                            IntegerValue::I16(v) => Some(*v as usize),
                            IntegerValue::I32(v) => Some(*v as usize),
                            IntegerValue::I64(v) => Some(*v as usize),
                            IntegerValue::U8(v) => Some(*v as usize),
                            IntegerValue::U16(v) => Some(*v as usize),
                            IntegerValue::U32(v) => Some(*v as usize),
                            IntegerValue::U64(v) => Some(*v as usize),
                        }
                    }
                    let target_len = integer_value_to_usize(length).unwrap_or(0);
                    if s.len() >= target_len || pad_char.is_empty() {
                        return Ok(Value::String(s.clone()));
                    }

                    let pad_char = pad_char.chars().next().unwrap_or(' ');
                    let padding = pad_char.to_string().repeat(target_len - s.len());
                    Ok(Value::String(format!("{}{}", padding, s)))
                } else {
                    Err(VeldError::RuntimeError(
                        "pad_start called with invalid arguments".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("str", "pad_end", |_, args| {
                use veld_common::value::numeric::IntegerValue;
                if let (
                    Some(Value::String(s)),
                    Some(Value::Numeric(NumericValue::Integer(length))),
                    Some(Value::String(pad_char)),
                ) = (args.get(0), args.get(1), args.get(2))
                {
                    // Convert IntegerValue to usize properly
                    fn integer_value_to_usize(iv: &IntegerValue) -> Option<usize> {
                        match iv {
                            IntegerValue::I8(v) => Some(*v as usize),
                            IntegerValue::I16(v) => Some(*v as usize),
                            IntegerValue::I32(v) => Some(*v as usize),
                            IntegerValue::I64(v) => Some(*v as usize),
                            IntegerValue::U8(v) => Some(*v as usize),
                            IntegerValue::U16(v) => Some(*v as usize),
                            IntegerValue::U32(v) => Some(*v as usize),
                            IntegerValue::U64(v) => Some(*v as usize),
                        }
                    }
                    let target_len = integer_value_to_usize(length).unwrap_or(0);
                    if s.len() >= target_len || pad_char.is_empty() {
                        return Ok(Value::String(s.clone()));
                    }

                    let pad_char = pad_char.chars().next().unwrap_or(' ');
                    let padding = pad_char.to_string().repeat(target_len - s.len());
                    Ok(Value::String(format!("{}{}", s, padding)))
                } else {
                    Err(VeldError::RuntimeError(
                        "pad_end called with invalid arguments".to_string(),
                    ))
                }
            });

        // --- Implement Parsable for strings ---

        // Parse to integer
        self.native_method_registry
            .register("str", "to_int", |_, args| {
                if let Some(Value::String(s)) = args.get(0) {
                    match s.parse::<i64>() {
                        Ok(n) => Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![Value::Integer(n)],
                        }),
                        Err(_) => Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "None".to_string(),
                            fields: vec![],
                        }),
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        "to_int called on non-string value".to_string(),
                    ))
                }
            });

        // Parse to float
        self.native_method_registry
            .register("str", "to_float", |_, args| {
                if let Some(Value::String(s)) = args.get(0) {
                    match s.parse::<f64>() {
                        Ok(n) => Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![Value::Float(n)],
                        }),
                        Err(_) => Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "None".to_string(),
                            fields: vec![],
                        }),
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        "to_float called on non-string value".to_string(),
                    ))
                }
            });

        // Parse to boolean
        self.native_method_registry
            .register("str", "to_bool", |_, args| {
                if let Some(Value::String(s)) = args.get(0) {
                    let lowercase = s.to_lowercase();
                    if lowercase == "true" {
                        Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![Value::Boolean(true)],
                        })
                    } else if lowercase == "false" {
                        Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "Some".to_string(),
                            fields: vec![Value::Boolean(false)],
                        })
                    } else {
                        Ok(Value::Enum {
                            enum_name: "Option".to_string(),
                            variant_name: "None".to_string(),
                            fields: vec![],
                        })
                    }
                } else {
                    Err(VeldError::RuntimeError(
                        "to_bool called on non-string value".to_string(),
                    ))
                }
            });
    }

    fn initialize_numeric_capabilities(&mut self) {
        // --- Implement Numeric for both integer and float ---

        // Absolute value
        self.native_method_registry
            .register("i32", "abs", |_, args| {
                if let Some(Value::Integer(n)) = args.get(0) {
                    Ok(Value::Integer(n.abs()))
                } else {
                    Err(VeldError::RuntimeError(
                        "abs called on non-integer value".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("f64", "abs", |_, args| {
                if let Some(Value::Float(n)) = args.get(0) {
                    Ok(Value::Float(n.abs()))
                } else {
                    Err(VeldError::RuntimeError(
                        "abs called on non-float value".to_string(),
                    ))
                }
            });

        // Sign (-1, 0, 1)
        self.native_method_registry
            .register("i32", "signum", |_, args| {
                if let Some(Value::Integer(n)) = args.get(0) {
                    Ok(Value::Integer(n.signum()))
                } else {
                    Err(VeldError::RuntimeError(
                        "signum called on non-integer value".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("f64", "signum", |_, args| {
                if let Some(Value::Float(n)) = args.get(0) {
                    Ok(Value::Integer(if *n > 0.0 {
                        1
                    } else if *n < 0.0 {
                        -1
                    } else {
                        0
                    }))
                } else {
                    Err(VeldError::RuntimeError(
                        "signum called on non-float value".to_string(),
                    ))
                }
            });

        // --- Implement Integer-specific operations ---

        // Even/odd checks
        self.native_method_registry
            .register("i32", "is_even", |_, args| {
                if let Some(Value::Integer(n)) = args.get(0) {
                    Ok(Value::Boolean(n % 2 == 0))
                } else {
                    Err(VeldError::RuntimeError(
                        "is_even called on non-integer value".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("i32", "is_odd", |_, args| {
                if let Some(Value::Integer(n)) = args.get(0) {
                    Ok(Value::Boolean(n % 2 != 0))
                } else {
                    Err(VeldError::RuntimeError(
                        "is_odd called on non-integer value".to_string(),
                    ))
                }
            });

        // Convert to float
        self.native_method_registry
            .register("i32", "to_float", |_, args| {
                if let Some(Value::Integer(n)) = args.get(0) {
                    Ok(Value::Float(*n as f64))
                } else {
                    Err(VeldError::RuntimeError(
                        "to_float called on non-integer value".to_string(),
                    ))
                }
            });

        // --- Implement Float-specific operations ---

        // Rounding operations
        self.native_method_registry
            .register("f64", "floor", |_, args| {
                if let Some(Value::Float(n)) = args.get(0) {
                    Ok(Value::Float(n.floor()))
                } else {
                    Err(VeldError::RuntimeError(
                        "floor called on non-float value".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("f64", "ceil", |_, args| {
                if let Some(Value::Float(n)) = args.get(0) {
                    Ok(Value::Float(n.ceil()))
                } else {
                    Err(VeldError::RuntimeError(
                        "ceil called on non-float value".to_string(),
                    ))
                }
            });

        self.native_method_registry
            .register("f64", "round", |_, args| {
                if let Some(Value::Float(n)) = args.get(0) {
                    Ok(Value::Float(n.round()))
                } else {
                    Err(VeldError::RuntimeError(
                        "round called on non-float value".to_string(),
                    ))
                }
            });

        // Convert to integer
        self.native_method_registry
            .register("f64", "to_int", |_, args| {
                if let Some(Value::Float(n)) = args.get(0) {
                    Ok(Value::Integer(n.round() as i64))
                } else {
                    Err(VeldError::RuntimeError(
                        "to_int called on non-float value".to_string(),
                    ))
                }
            });
    }

    fn initialize_array_methods(&mut self) {
        // Disable problematic Array method registration to prevent infinite recursion
        // Array methods are handled natively in call_method_value instead

        // Don't register any methods in struct_methods["Array"] to avoid conflicts
        // with the native array method dispatch in call_method_value
    }

    // Helper method to call functions with pre-evaluated arguments
    fn call_function_with_values(&mut self, name: String, arg_values: Vec<Value>) -> Result<Value> {
        // Recursion depth tracking and limit
        self.recursion_depth += 1;
        const RECURSION_LIMIT: usize = 10000;
        if self.recursion_depth > RECURSION_LIMIT {
            return Err(VeldError::RuntimeError(
                "Recursion limit exceeded".to_string(),
            ));
        }

        // Check if this might be a method call on a struct
        if name.contains('.') {
            let parts: Vec<&str> = name.split('.').collect();
            if parts.len() == 2 {
                let object = self.get_variable(parts[0]).ok_or_else(|| {
                    VeldError::RuntimeError(format!("Undefined variable '{}'", parts[0]))
                })?;
                self.recursion_depth -= 1;
                return self.call_method_value(object, parts[1].to_string(), arg_values);
            }
        }

        // Check if this is a generic function that needs to be instantiated
        if let Some(generic_fn) = self.find_generic_function(&name) {
            // Infer type arguments from the provided values
            let type_args = self.infer_type_args_from_values(&generic_fn, &arg_values)?;

            // Check that the inferred types satisfy the constraints
            if !self
                .type_checker
                .check_generic_constraints(&generic_fn.generic_params, &type_args)
            {
                return Err(VeldError::RuntimeError(format!(
                    "Type arguments do not satisfy constraints for function {}",
                    name
                )));
            }

            // Create a specialized version of the function
            let mut specialized_params = Vec::new();

            // Substitute the type parameters in the function signature
            for (param_name, type_ann) in &generic_fn.params {
                let specialized_type = self.substitute_type_params_in_annotation(
                    type_ann,
                    &generic_fn.generic_params,
                    &type_args,
                );
                specialized_params.push((param_name.clone(), specialized_type));
            }

            // Substitute in the return type
            let specialized_return_type = self.substitute_type_params_in_annotation(
                &generic_fn.return_type,
                &generic_fn.generic_params,
                &type_args,
            );

            // Create a specialized function and call it with the provided arguments
            let _function = Value::Function {
                params: specialized_params.clone(),
                body: generic_fn.body.clone(),
                return_type: specialized_return_type,
                captured_vars: HashMap::new(),
            };

            // Now continue with the normal function call logic using this specialized function
            self.push_scope();

            // Bind arguments
            if arg_values.len() != specialized_params.len() {
                return Err(VeldError::RuntimeError(format!(
                    "Expected {} arguments but got {}",
                    specialized_params.len(),
                    arg_values.len()
                )));
            }

            // Bind parameters to arguments
            for (i, arg) in arg_values.iter().enumerate() {
                let param_name = specialized_params[i].0.clone();
                let arg_value = arg.clone();
                self.current_scope_mut().set(param_name, arg_value);
            }

            // Execute function body
            let mut result = Value::Unit;
            for stmt in &generic_fn.body {
                result = self.execute_statement(stmt.clone())?;
                if matches!(result, Value::Return(_)) {
                    break;
                }
            }

            // Remove function scope
            self.pop_scope();

            // For functions without explicit returns, if the last statement produced a value,
            // consider it the return value
            if !matches!(result, Value::Return(_)) && !matches!(result, Value::Unit) {
                result = Value::Return(Box::new(result));
            }

            return Ok(result.unwrap_return());
        }

        // Try to get the function from the current scope
        let function = if let Some(func) = self.get_variable(&name) {
            func
        } else {
            // Check if this is a native function
            if self.native_registry.contains(&name) {
                // Call native function directly
                let result = if let Some(handler) = self.native_registry.get(&name) {
                    handler(self, arg_values)
                } else if self.native_registry.contains_static(&name) {
                    // Try static functions if regular one not found
                    self.native_registry.call_static(&name, self, arg_values)
                } else {
                    return Err(VeldError::RuntimeError(format!(
                        "Native function '{}' not found",
                        name
                    )));
                };
                self.recursion_depth -= 1;
                return result;
            } else {
                return Err(VeldError::RuntimeError(format!(
                    "Undefined function '{}'",
                    name
                )));
            }
        };

        // Special handling for std.io module functions - redirect to native implementations
        if let Value::Function { params: _, .. } = &function {
            // Check if this is an imported std.io function with empty body
            let native_function_name = format!("std.io.{}", name);
            if self.native_registry.contains(&native_function_name) {
                // This is a std.io function, call the native implementation instead
                let result = if let Some(handler) = self.native_registry.get(&native_function_name)
                {
                    handler(self, arg_values)
                } else if self.native_registry.contains_static(&native_function_name) {
                    self.native_registry
                        .call_static(&native_function_name, self, arg_values)
                } else {
                    return Err(VeldError::RuntimeError(format!(
                        "Native function '{}' not found",
                        native_function_name
                    )));
                };
                self.recursion_depth -= 1;
                return result;
            }
        }

        // Check for native function redirects (handles all imported functions)
        let potential_native_names = [
            format!("std.io.{}", name),
            format!("std.math.{}", name),
            name.clone(), // Direct native function name
        ];

        for native_name in &potential_native_names {
            if self.native_registry.contains(native_name) {
                let result = if let Some(handler) = self.native_registry.get(native_name) {
                    handler(self, arg_values)
                } else if self.native_registry.contains_static(native_name) {
                    self.native_registry
                        .call_static(native_name, self, arg_values)
                } else {
                    continue;
                };
                self.recursion_depth -= 1;
                return result;
            }
        }

        match function {
            Value::Function {
                params,
                body,
                captured_vars,
                ..
            } => {
                // Check if this is an imported function with empty body that should redirect to native
                if body.is_empty() {
                    let potential_native_names = [
                        format!("std.io.{}", name),
                        format!("std.math.{}", name),
                        name.clone(),
                    ];

                    for native_name in &potential_native_names {
                        if self.native_registry.contains(native_name) {
                            let result =
                                if let Some(handler) = self.native_registry.get(native_name) {
                                    handler(self, arg_values)
                                } else if self.native_registry.contains_static(native_name) {
                                    self.native_registry
                                        .call_static(native_name, self, arg_values)
                                } else {
                                    continue;
                                };
                            self.recursion_depth -= 1;
                            return result;
                        }
                    }
                }

                // Create new scope for function
                self.push_scope();

                // First, set up captured variables in the new scope
                // Skip Unit placeholders to allow recursive lambdas to find their actual values in parent scopes
                for (var_name, var_value) in captured_vars {
                    if !matches!(var_value, Value::Unit) {
                        self.current_scope_mut().set(var_name, var_value);
                    }
                }

                // Bind arguments
                if arg_values.len() != params.len() {
                    return Err(VeldError::RuntimeError(format!(
                        "Expected {} arguments but got {}",
                        params.len(),
                        arg_values.len()
                    )));
                }

                // Bind parameters to arguments
                for (i, arg) in arg_values.iter().enumerate() {
                    let param_name = params[i].0.clone();
                    let arg_value = arg.clone();
                    self.current_scope_mut()
                        .set(param_name.clone(), arg_value.clone());
                }

                // Execute function body
                let mut result = Value::Unit;
                for stmt in body.into_iter() {
                    let stmt_result = self.execute_statement(stmt.clone())?;
                    if matches!(stmt_result, Value::Return(_)) {
                        result = stmt_result;
                        break;
                    }
                    result = stmt_result;
                }

                self.pop_scope();
                if matches!(result, Value::Return(_)) {
                    return Ok(result.unwrap_return());
                }
                // If the last statement produced a value (not Unit), return it
                if !matches!(result, Value::Unit) {
                    return Ok(result);
                }
                Ok(Value::Unit)
            }
            _ => Err(VeldError::RuntimeError(format!(
                "'{}' is not a function",
                name
            ))),
        }
    }

    fn get_property(&mut self, object: Value, property: &str) -> Result<Value> {
        match &object {
            Value::Struct { name: _, fields } => {
                if let Some(value) = fields.get(property) {
                    Ok(value.clone())
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Property '{}' not found",
                        property
                    )))
                }
            }
            Value::Record(fields) => {
                if let Some(value) = fields.get(property) {
                    Ok(value.clone())
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Property '{}' not found in record",
                        property
                    )))
                }
            }
            Value::Array(elements) => {
                // Handle array properties
                match property {
                    "len" => Ok(Value::Integer(elements.len() as i64)),
                    "last" | "first" | "init" | "tail" | "with" | "take" | "drop" | "map"
                    | "filter" => {
                        // Return a special function that, when called, will invoke the corresponding array method
                        Ok(Value::Function {
                            params: vec![
                                (
                                    "self".to_string(),
                                    TypeAnnotation::Basic("Array".to_string()),
                                ),
                                ("arg".to_string(), TypeAnnotation::Unit),
                            ],
                            body: vec![], // Empty body for built-in methods
                            return_type: TypeAnnotation::Basic("any".to_string()),
                            captured_vars: HashMap::new(),
                        })
                    }
                    _ => Err(VeldError::RuntimeError(format!(
                        "Property '{}' not found on array",
                        property
                    ))),
                }
            }
            Value::EnumType { name, methods: _ } => {
                // Property access on enum type, e.g., Option.None
                // Look up the enum and variant
                tracing::debug!(
                    "Looking up enum '{}' for variant access to '{}'",
                    name,
                    property
                );
                if let Some(variants) = self.enums.get(name.as_str()) {
                    if let Some(_variant) = variants.iter().find(|v| v.name == property) {
                        // Return a value representing the enum variant constructor (no fields)
                        Ok(Value::Enum {
                            enum_name: name.clone(),
                            variant_name: property.to_string(),
                            fields: vec![],
                            // methods,
                        })
                    } else {
                        Err(VeldError::RuntimeError(format!(
                            "Enum '{}' has no variant '{}'",
                            name, property
                        )))
                    }
                } else {
                    tracing::debug!(
                        "Enum '{}' not found in runtime enums. Available enums: {:?}",
                        name,
                        self.enums.keys().collect::<Vec<_>>()
                    );
                    Err(VeldError::RuntimeError(format!(
                        "Enum '{}' not found",
                        name
                    )))
                }
            }
            Value::StructType { name, .. } => {
                // Property access on struct type, e.g., Vec.new
                // Look up the struct method
                if let Some(methods) = self.struct_methods.get(name) {
                    if let Some(method) = methods.get(property) {
                        Ok(method.clone())
                    } else {
                        Err(VeldError::RuntimeError(format!(
                            "Struct '{}' has no method '{}'",
                            name, property
                        )))
                    }
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Struct '{}' not found",
                        name
                    )))
                }
            }
            Value::Module(module) => {
                // Debug: Show module exports when looking up property
                tracing::debug!(
                    "Looking up property '{}' in module '{}' with exports: {:?}",
                    property,
                    module.name,
                    module.exports.keys().collect::<Vec<_>>()
                );

                // Try to resolve as an exported item first
                if let Some(export) = module.exports.get(property) {
                    tracing::debug!(
                        "Found export '{}' in module '{}' with type: {:?}",
                        property,
                        module.name,
                        export
                    );
                    match export {
                        ExportedItem::Function(idx) => {
                            // Look up the function statement in module.statements
                            if let Some(Statement::FunctionDeclaration {
                                params,
                                body,
                                return_type,
                                ..
                            }) = module.statements.get(*idx)
                            {
                                Ok(Value::Function {
                                    params: params.clone(),
                                    body: body.clone(),
                                    return_type: return_type.clone(),
                                    captured_vars: HashMap::new(),
                                })
                            } else {
                                Err(VeldError::RuntimeError(format!(
                                    "Function '{}' not found in module '{}'",
                                    property, module.name
                                )))
                            }
                        }
                        veld_common::value::module::ExportedItem::Struct(idx) => {
                            // Return a StructType value that can be used for static method calls
                            let qualified_name = format!("{}.{}", module.name, property);
                            Ok(Value::StructType {
                                name: qualified_name,
                                methods: None,
                            })
                        }
                        ExportedItem::Variable(_idx) => Err(VeldError::RuntimeError(format!(
                            "Variable '{}' exported from module '{}' is not directly accessible yet (implement variable resolution here)",
                            property, module.name
                        ))),
                        ExportedItem::Kind(_idx) => Err(VeldError::RuntimeError(format!(
                            "Kind '{}' exported from module '{}' is not directly accessible yet (implement kind resolution here)",
                            property, module.name
                        ))),
                        ExportedItem::Enum(_idx) => {
                            // Return an EnumType value that can be used for variant access
                            let qualified_name = format!("{}.{}", module.name, property);
                            Ok(Value::EnumType {
                                name: qualified_name,
                                methods: None,
                            })
                        }
                        ExportedItem::Module(submod_name) => {
                            if let Some(submodule) = self.module_manager.get_module(submod_name) {
                                Ok(Value::Module(submodule.clone()))
                            } else {
                                Err(VeldError::RuntimeError(format!(
                                    "Submodule '{}' not found in module '{}'",
                                    submod_name, module.name
                                )))
                            }
                        }
                    }
                } else {
                    // Dynamic submodule resolution: try to load submodule by fully qualified name
                    let fq_name = format!("{}.{}", module.name, property);
                    // If the submodule is not loaded, load it now!
                    if !self.module_manager.is_module_loaded(&fq_name) {
                        let path: Vec<String> = fq_name.split('.').map(|s| s.to_string()).collect();
                        let _ = self.module_manager.load_module(&path);
                    }
                    if let Some(submodule) = self.module_manager.get_module(&fq_name) {
                        // Register enums and their methods from the dynamically loaded module
                        // This ensures that enum types like Option and their methods are available
                        for (_idx, statement) in submodule.statements.iter().enumerate() {
                            if let Statement::EnumDeclaration {
                                name: enum_name,
                                variants,
                                ..
                            } = statement
                            {
                                // Register the enum in the runtime
                                self.enums.insert(enum_name.clone(), variants.clone());

                                // Register the enum in the type environment for type checking
                                fn convert_ast_enum_variant_to_base(
                                    ast_variant: &veld_common::ast::EnumVariant,
                                ) -> veld_common::types::EnumVariant
                                {
                                    match &ast_variant.fields {
                                        None => veld_common::types::EnumVariant::Simple,
                                        Some(fields) => {
                                            let tuple_types: Vec<veld_common::types::Type> = fields
                                                .iter()
                                                .map(|anno| {
                                                    veld_common::types::Type::from_annotation(
                                                        anno, None,
                                                    )
                                                    .unwrap_or(veld_common::types::Type::Any)
                                                })
                                                .collect();
                                            veld_common::types::EnumVariant::Tuple(tuple_types)
                                        }
                                    }
                                }
                                let variant_map: std::collections::HashMap<
                                    String,
                                    veld_common::types::EnumVariant,
                                > = variants
                                    .iter()
                                    .map(|v| (v.name.clone(), convert_ast_enum_variant_to_base(v)))
                                    .collect();

                                self.type_checker.env().add_enum(enum_name, variant_map);

                                // Also register enum methods from implementation blocks
                                for impl_stmt in &submodule.statements {
                                    if let Statement::InherentImpl {
                                        type_name,
                                        methods,
                                        generic_params,
                                        ..
                                    } = impl_stmt
                                    {
                                        if type_name == enum_name {
                                            // Set up type parameter scope for generic impl blocks
                                            self.type_checker.env().push_type_param_scope();
                                            for generic_arg in generic_params {
                                                let param_name = match &generic_arg.name {
                                                    Some(name) => name.clone(),
                                                    None => {
                                                        if let veld_common::ast::TypeAnnotation::Basic(base_name) =
                                                            &generic_arg.type_annotation
                                                        {
                                                            base_name.clone()
                                                        } else {
                                                            "T".to_string()
                                                        }
                                                    }
                                                };
                                                self.type_checker.env().add_type_param(&param_name);
                                            }

                                            for method in methods {
                                                // Set up method-level type parameters
                                                self.type_checker.env().push_type_param_scope();
                                                for generic_arg in &method.generic_params {
                                                    let param_name = match &generic_arg.name {
                                                        Some(name) => name.clone(),
                                                        None => {
                                                            if let veld_common::ast::TypeAnnotation::Basic(base_name) =
                                                                &generic_arg.type_annotation
                                                            {
                                                                base_name.clone()
                                                            } else {
                                                                "U".to_string()
                                                            }
                                                        }
                                                    };
                                                    self.type_checker
                                                        .env()
                                                        .add_type_param(&param_name);
                                                }

                                                let param_types: Vec<veld_common::types::Type> =
                                                    method
                                                        .params
                                                        .iter()
                                                        .map(|(_, type_annotation)| {
                                                            self.type_checker
                                                                .env()
                                                                .from_annotation(
                                                                    type_annotation,
                                                                    None,
                                                                )
                                                                .unwrap_or(
                                                                    veld_common::types::Type::Any,
                                                                )
                                                        })
                                                        .collect();
                                                let return_type = self
                                                    .type_checker
                                                    .env()
                                                    .from_annotation(&method.return_type, None)
                                                    .unwrap_or(veld_common::types::Type::Any);
                                                let function_type =
                                                    veld_common::types::Type::Function {
                                                        params: param_types,
                                                        return_type: Box::new(return_type),
                                                    };

                                                self.type_checker.env().add_enum_method(
                                                    type_name,
                                                    &method.name,
                                                    function_type,
                                                );

                                                // Clean up method-level type parameter scope
                                                self.type_checker.env().pop_type_param_scope();
                                            }

                                            // Clean up type parameter scope
                                            self.type_checker.env().pop_type_param_scope();
                                        }
                                    }
                                }
                            }
                        }

                        Ok(Value::Module(submodule.clone()))
                    } else {
                        Err(VeldError::RuntimeError(format!(
                            "Module '{}' has no export or submodule '{}'",
                            module.name, property
                        )))
                    }
                }
            }
            Value::Enum {
                enum_name,
                variant_name,
                fields: _,
            } => {
                let enum_instance = object.clone(); // Now works, because object is not moved

                // First, check for methods on the variant itself
                if let Some(variants) = self.enums.get(enum_name) {
                    if let Some(variant) = variants.iter().find(|v| &v.name == variant_name) {
                        if let Some(method) = variant.methods.get(property) {
                            return Ok(Value::Function {
                                params: method.params.clone(),
                                body: method.body.clone(),
                                return_type: method.return_type.clone(),
                                captured_vars: HashMap::new(),
                            });
                        } else {
                        }
                    } else {
                    }
                } else {
                }

                // If not found, check for methods on the enum type itself

                if let Some(enum_methods) = self.enum_methods.get(enum_name) {
                    if let Some(method) = enum_methods.get(property) {
                        let mut captured_vars = HashMap::new();
                        captured_vars.insert("self".to_string(), enum_instance);
                        return Ok(Value::Function {
                            params: method.params.clone(),
                            body: method.body.clone(),
                            return_type: method.return_type.clone(),
                            captured_vars,
                        });
                    } else {
                    }
                } else {
                }

                Err(VeldError::RuntimeError(format!(
                    "Enum '{}' of variant '{}' has no property, method, or field named '{}'.",
                    enum_name, variant_name, property
                )))
            }
            _ => Err(VeldError::RuntimeError(
                "Cannot access property on non-struct value".to_string(),
            )),
        }
    }

    fn call_function_with_single_argument(&mut self, func: &Value, arg: Value) -> Result<Value> {
        match func {
            Value::Function { params, body, .. } => {
                // Direct invocation of function value
                self.push_scope();

                // Bind the argument to the first parameter
                if params.is_empty() {
                    return Err(VeldError::RuntimeError(
                        "Function must take at least one parameter".to_string(),
                    ));
                }

                self.current_scope_mut().set(params[0].0.clone(), arg);

                // Execute function body
                let mut result = Value::Unit;
                for stmt in body.into_iter() {
                    let stmt_result = self.execute_statement(stmt.clone())?;
                    if matches!(stmt_result, Value::Return(_)) {
                        result = stmt_result;
                        break;
                    }
                    result = stmt_result;
                }

                self.pop_scope();

                // If a return was encountered, propagate it immediately
                if matches!(result, Value::Return(_)) {
                    return Ok(result.unwrap_return());
                }
                // If the last statement produced a value (not Unit), return it
                if !matches!(result, Value::Unit) {
                    return Ok(result);
                }
                Ok(Value::Unit)
            }
            _ => Err(VeldError::RuntimeError(
                "Cannot call non-function value".to_string(),
            )),
        }
    }

    fn call_method_value(
        &mut self,
        object: Value,
        method_name: String,
        args: Vec<Value>,
    ) -> Result<Value> {
        self.call_method_value_with_mutation(object, method_name, args, None)
    }

    fn call_method_value_with_mutation(
        &mut self,
        object: Value,
        method_name: String,
        args: Vec<Value>,
        variable_name: Option<String>,
    ) -> Result<Value> {
        // Special handling for std.io module method calls - redirect to native implementations
        if let Value::Module(module) = &object {
            tracing::debug!(
                "Method call on module: {} method: {}",
                module.name,
                method_name
            );
            if module.name == "std.io" {
                let native_function_name = format!("std.io.{}", method_name);
                tracing::debug!("Checking for native function: {}", native_function_name);
                if self.native_registry.contains(&native_function_name) {
                    tracing::debug!("Found native function: {}", native_function_name);
                    let result =
                        if let Some(handler) = self.native_registry.get(&native_function_name) {
                            tracing::debug!("Calling regular native function");
                            handler(self, args)
                        } else if self.native_registry.contains_static(&native_function_name) {
                            tracing::debug!("Calling static native function");
                            self.native_registry
                                .call_static(&native_function_name, self, args)
                        } else {
                            return Err(VeldError::RuntimeError(format!(
                                "Native function '{}' not found",
                                native_function_name
                            )));
                        };
                    tracing::debug!("Native function call result: {:?}", result);
                    return result;
                } else {
                    tracing::debug!("Native function not found: {}", native_function_name);
                }
            }
        }

        // First check for native methods on built-in types
        let type_name = object.type_of().to_string();

        if self
            .native_method_registry
            .has_method(&type_name, &method_name)
        {
            let mut method_args = vec![object.clone()];
            method_args.extend(args.clone());

            if let Some(handler) = self.native_method_registry.get(&type_name, &method_name) {
                return handler(self, method_args);
            }
        }

        // Unified array method dispatch using native implementations
        if let Value::Array(elements) = &object {
            // Handle array methods natively without relying on struct_methods registration
            match method_name.as_str() {
                "get" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "get() takes exactly one argument".to_string(),
                        ));
                    }
                    match &args[0] {
                        Value::Integer(i) => {
                            if *i < 0 || *i >= elements.len() as i64 {
                                return Ok(Value::Enum {
                                    enum_name: "Option".to_string(),
                                    variant_name: "None".to_string(),
                                    fields: vec![],
                                });
                            }
                            return Ok(Value::Enum {
                                enum_name: "Option".to_string(),
                                variant_name: "Some".to_string(),
                                fields: vec![elements[*i as usize].clone()],
                            });
                        }
                        Value::Numeric(NumericValue::Integer(iv)) => {
                            let idx = match_num_val(iv).unwrap_or(usize::MAX) as i64;
                            if idx < 0 || idx >= elements.len() as i64 {
                                return Ok(Value::Enum {
                                    enum_name: "Option".to_string(),
                                    variant_name: "None".to_string(),
                                    fields: vec![],
                                });
                            }
                            return Ok(Value::Enum {
                                enum_name: "Option".to_string(),
                                variant_name: "Some".to_string(),
                                fields: vec![elements[idx as usize].clone()],
                            });
                        }
                        _ => {
                            return Err(VeldError::RuntimeError(
                                "get() argument must be an integer".to_string(),
                            ));
                        }
                    }
                }
                "set" => {
                    if args.len() != 2 {
                        return Err(VeldError::RuntimeError(
                            "set() takes exactly two arguments".to_string(),
                        ));
                    }
                    match &args[0] {
                        Value::Integer(i) => {
                            if *i < 0 || *i >= elements.len() as i64 {
                                return Ok(Value::Boolean(false));
                            }
                            return Ok(Value::Boolean(true));
                        }
                        Value::Numeric(NumericValue::Integer(iv)) => {
                            let idx = match_num_val(iv).unwrap_or(usize::MAX) as i64;
                            if idx < 0 || idx >= elements.len() as i64 {
                                return Ok(Value::Boolean(false));
                            }
                            return Ok(Value::Boolean(true));
                        }
                        _ => {
                            return Err(VeldError::RuntimeError(
                                "set() first argument must be an integer".to_string(),
                            ));
                        }
                    }
                }
                "last" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "last() takes no arguments".to_string(),
                        ));
                    }
                    if elements.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "Cannot get last element of empty array".to_string(),
                        ));
                    }
                    return Ok(elements.last().unwrap().clone());
                }
                "first" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "first() takes no arguments".to_string(),
                        ));
                    }
                    if elements.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "Cannot get first element of empty array".to_string(),
                        ));
                    }
                    return Ok(elements.first().unwrap().clone());
                }
                "init" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "init() takes no arguments".to_string(),
                        ));
                    }
                    if elements.is_empty() {
                        let gc_ref = self
                            .allocator
                            .write()
                            .unwrap()
                            .allocate(Value::Array(vec![]))?;

                        // Trigger GC after allocation
                        let root_set = self.collect_gc_roots();
                        self.collector.write().unwrap().collect(
                            veld_common::gc::collector::CollectionStrategy::Incremental,
                            &mut self.allocator.write().unwrap(),
                            &root_set,
                            &self.gc_config,
                        );

                        return Ok(Value::GcRef(gc_ref));
                    }
                    let mut new_elements = elements.clone();
                    new_elements.pop();
                    let gc_ref = self
                        .allocator
                        .write()
                        .unwrap()
                        .allocate(Value::Array(new_elements))?;

                    // Trigger GC after allocation
                    let root_set = self.collect_gc_roots();
                    self.collector.write().unwrap().collect(
                        veld_common::gc::collector::CollectionStrategy::Incremental,
                        &mut self.allocator.write().unwrap(),
                        &root_set,
                        &self.gc_config,
                    );

                    return Ok(Value::GcRef(gc_ref));
                }
                "tail" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "tail() takes no arguments".to_string(),
                        ));
                    }
                    if elements.is_empty() {
                        return Ok(Value::Array(vec![]));
                    }
                    let new_elements = elements.iter().skip(1).cloned().collect();
                    let gc_ref = self
                        .allocator
                        .write()
                        .unwrap()
                        .allocate(Value::Array(new_elements))?;

                    // Trigger GC after allocation
                    let root_set = self.collect_gc_roots();
                    self.collector.write().unwrap().collect(
                        veld_common::gc::collector::CollectionStrategy::Incremental,
                        &mut self.allocator.write().unwrap(),
                        &root_set,
                        &self.gc_config,
                    );

                    return Ok(Value::GcRef(gc_ref));
                }
                "with" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "with() takes exactly one argument".to_string(),
                        ));
                    }
                    let mut new_elements = elements.clone();
                    new_elements.push(args[0].clone());
                    tracing::debug!(
                        target: "veld.mutself",
                        method = "Array.with",
                        input = ?elements,
                        arg = ?args[0],
                        result = ?new_elements,
                        "Array.with called"
                    );
                    return Ok(Value::Array(new_elements));
                }
                "take" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "take() takes exactly one argument".to_string(),
                        ));
                    }
                    if let Value::Integer(n) = &args[0] {
                        if *n < 0 {
                            return Err(VeldError::RuntimeError(
                                "take() argument must be non-negative".to_string(),
                            ));
                        }
                        let n = *n as usize;
                        let new_elements = elements.iter().take(n).cloned().collect();
                        return Ok(Value::Array(new_elements));
                    } else {
                        return Err(VeldError::RuntimeError(
                            "take() argument must be an integer".to_string(),
                        ));
                    }
                }
                "drop" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "drop() takes exactly one argument".to_string(),
                        ));
                    }
                    if let Value::Integer(n) = &args[0] {
                        if *n < 0 {
                            return Err(VeldError::RuntimeError(
                                "drop() argument must be non-negative".to_string(),
                            ));
                        }
                        let n = *n as usize;
                        let new_elements = elements.iter().skip(n).cloned().collect();
                        return Ok(Value::Array(new_elements));
                    } else {
                        return Err(VeldError::RuntimeError(
                            "drop() argument must be an integer".to_string(),
                        ));
                    }
                }
                "len" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "len() takes no arguments".to_string(),
                        ));
                    }
                    return Ok(Value::Integer(elements.len() as i64));
                }
                "is_empty" => {
                    if !args.is_empty() {
                        return Err(VeldError::RuntimeError(
                            "is_empty() takes no arguments".to_string(),
                        ));
                    }
                    return Ok(Value::Boolean(elements.is_empty()));
                }
                "map" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "map() takes exactly one function argument".to_string(),
                        ));
                    }
                    let func = &args[0];
                    let mut result = Vec::new();
                    for element in elements {
                        let mapped =
                            self.call_function_with_single_argument(func, element.clone())?;
                        result.push(mapped);
                    }
                    return Ok(Value::Array(result));
                }
                "filter" => {
                    if args.len() != 1 {
                        return Err(VeldError::RuntimeError(
                            "filter() takes exactly one function argument".to_string(),
                        ));
                    }
                    let func = &args[0];
                    let mut result = Vec::new();
                    for element in elements {
                        let pred_result =
                            self.call_function_with_single_argument(func, element.clone())?;
                        if self.is_truthy(pred_result) {
                            result.push(element.clone());
                        }
                    }
                    return Ok(Value::Array(result));
                }
                _ => {
                    return Err(VeldError::RuntimeError(format!(
                        "Method '{}' not found on array",
                        method_name
                    )));
                }
            }
        }

        // For built-in methods like "sqrt" on numeric values
        match (&object.clone(), method_name.as_str()) {
            (Value::Float(f), "sqrt") => {
                if args.is_empty() {
                    Ok(Value::Float(f.sqrt()))
                } else {
                    Err(VeldError::RuntimeError(
                        "sqrt() takes no arguments".to_string(),
                    ))
                }
            }

            // For struct methods
            (Value::Struct { name, .. }, _) => {
                // Create a string representation for the struct's name
                let struct_name = name.clone();

                // Get the method from struct_methods
                let method = self
                    .struct_methods
                    .get(&struct_name)
                    .and_then(|methods| methods.get(&method_name))
                    .cloned()
                    .ok_or_else(|| {
                        VeldError::RuntimeError(format!(
                            "Method '{}' not found on '{}'",
                            method_name, struct_name
                        ))
                    })?;

                match method {
                    Value::Function { params, body, .. } => {
                        self.push_scope();

                        // Check if first parameter is 'mut self'
                        let has_mut_self = params.len() > 0 && params[0].0 == "mut self";

                        // Bind 'self' to the struct instance by moving it.
                        self.current_scope_mut()
                            .declare("self".to_string(), object, VarKind::Var)
                            .unwrap(); // Should not fail in a new scope

                        // Check argument count (excluding self)
                        if args.len() != params.len() - 1 {
                            return Err(VeldError::RuntimeError(format!(
                                "Method '{}' expects {} arguments but got {}",
                                method_name,
                                params.len() - 1,
                                args.len()
                            )));
                        }

                        // Bind arguments directly (already evaluated)
                        for (i, arg) in args.clone().into_iter().enumerate() {
                            // Start from index 1 to skip self
                            self.current_scope_mut().set(params[i + 1].0.clone(), arg);
                        }

                        // DEBUG: Print self before method body
                        let self_before = self.scopes.last().unwrap().get("self").clone();
                        tracing::debug!(
                            target: "veld.mutself",
                            method = %method_name,
                            has_mut_self,
                            variable_name = ?variable_name,
                            self_before = ?self_before,
                            "Entering mut self method"
                        );

                        // DEBUG: Print argument binding
                        for (i, arg) in args.iter().enumerate() {
                            tracing::debug!(
                                target: "veld.mutself",
                                method = %method_name,
                                arg_index = i,
                                arg_value = ?arg,
                                param_name = %params[i + 1].0,
                                "Binding argument to parameter"
                            );
                        }

                        // Execute method body
                        let mut result = Value::Unit;
                        for stmt in body.into_iter() {
                            let stmt_result = self.execute_statement(stmt.clone())?;
                            tracing::debug!(
                                target: "veld.mutself",
                                "After statement in method body: stmt={:?}, stmt_result={:?}",
                                stmt,
                                stmt_result
                            );
                            if matches!(stmt_result, Value::Return(_)) {
                                result = stmt_result;
                                break;
                            }
                            result = stmt_result;
                        }

                        // DEBUG: Print self after method body
                        let self_after = self.scopes.last().unwrap().get("self").clone();
                        tracing::debug!(
                            target: "veld.mutself",
                            method = %method_name,
                            has_mut_self,
                            variable_name = ?variable_name,
                            self_after = ?self_after,
                            "Exiting mut self method"
                        );

                        // DEBUG: Print result before return
                        tracing::debug!(
                            target: "veld.mutself",
                            method = %method_name,
                            result = ?result,
                            "Result before return or write-back"
                        );

                        // If method has 'mut self' and we have a variable name, update the original variable
                        if has_mut_self {
                            if let Some(var_name) = &variable_name {
                                // Get the updated self value
                                let updated_self =
                                    self.scopes.last().unwrap().get("self").map(|v| v.clone());

                                self.pop_scope();

                                // Update the variable in the correct scope
                                if let Some(updated_value) = updated_self {
                                    tracing::debug!(
                                        target: "veld.mutself",
                                        method = %method_name,
                                        var_name = %var_name,
                                        updated_value = ?updated_value,
                                        "Writing back mutated self to variable"
                                    );
                                    self.set_variable(var_name, updated_value)?;
                                } else {
                                    tracing::warn!(
                                        target: "veld.mutself",
                                        method = %method_name,
                                        var_name = %var_name,
                                        "No updated self found to write back"
                                    );
                                }

                                tracing::debug!(
                                    target: "veld.mutself",
                                    method = %method_name,
                                    "Popped scope after mut self write-back"
                                );
                                tracing::debug!(
                                    target: "veld.mutself",
                                    method = %method_name,
                                    "Returning from mut self method (write-back branch), return_value={:?}",
                                    result
                                );
                                if let Value::Return(val) = result {
                                    return Ok(*val);
                                }
                                tracing::debug!(
                                    target: "veld.mutself",
                                    method = %method_name,
                                    "Returning FINAL from mut self method (write-back branch), final_result={:?}",
                                    result
                                );
                                return Ok(result);
                            }
                        }

                        tracing::debug!(
                            target: "veld.mutself",
                            method = %method_name,
                            "Popped scope after mut self (no write-back)"
                        );
                        self.pop_scope();

                        tracing::debug!(
                            target: "veld.mutself",
                            method = %method_name,
                            "Returning from mut self method (no write-back branch), return_value={:?}",
                            result
                        );
                        if let Value::Return(val) = result {
                            return Ok(*val);
                        }
                        tracing::debug!(
                            target: "veld.mutself",
                            method = %method_name,
                            "Returning FINAL from mut self method (no write-back branch), final_result={:?}",
                            result
                        );
                        return Ok(result);
                    }
                    _ => Err(VeldError::RuntimeError(
                        "Internal error: method is not a function".to_string(),
                    )),
                }
            }

            // For struct type methods (static methods like Vec.new())
            (Value::StructType { name, .. }, _) => {
                let struct_name = name.clone();

                // Get the method from struct_methods
                let method = self
                    .struct_methods
                    .get(&struct_name)
                    .and_then(|methods| methods.get(&method_name))
                    .cloned()
                    .ok_or_else(|| {
                        VeldError::RuntimeError(format!(
                            "Method '{}' not found on '{}'",
                            method_name, struct_name
                        ))
                    })?;

                match method {
                    Value::Function { params, body, .. } => {
                        self.push_scope();

                        // For static methods, we don't bind 'self'
                        // Check argument count (no self parameter for static methods)
                        if args.len() != params.len() {
                            return Err(VeldError::RuntimeError(format!(
                                "Static method '{}' expects {} arguments but got {}",
                                method_name,
                                params.len(),
                                args.len()
                            )));
                        }

                        // Bind arguments directly (already evaluated)
                        for (i, arg) in args.into_iter().enumerate() {
                            self.current_scope_mut().set(params[i].0.clone(), arg);
                        }

                        // Execute method body
                        let mut result = Value::Unit;
                        for stmt in body.into_iter() {
                            let stmt_result = self.execute_statement(stmt.clone())?;
                            if matches!(stmt_result, Value::Return(_)) {
                                result = stmt_result;
                                break;
                            }
                            result = stmt_result;
                        }

                        self.pop_scope();

                        // If a return was encountered, propagate it immediately
                        if matches!(result, Value::Return(_)) {
                            return Ok(result.unwrap_return());
                        }
                        // If the last statement produced a value (not Unit), return it
                        if !matches!(result, Value::Unit) {
                            return Ok(result);
                        }
                        Ok(Value::Unit)
                    }
                    _ => Err(VeldError::RuntimeError(
                        "Internal error: method is not a function".to_string(),
                    )),
                }
            }
            // For enum instance methods
            (Value::Enum { enum_name, .. }, _) => {
                // TODO: When where clauses are fully implemented, need to check here
                // that any where clause constraints are satisfied before allowing
                // method calls. For example, Option<T>.to_string() should only be
                // callable when T implements ToString.

                // Get the method from enum_methods - try qualified name first, then simple name
                let method = self
                    .enum_methods
                    .get(enum_name)
                    .and_then(|methods| methods.get(&method_name))
                    .or_else(|| {
                        // If qualified name lookup fails, try simple name
                        let simple_name = enum_name.split('.').last().unwrap_or(enum_name);
                        self.enum_methods
                            .get(simple_name)
                            .and_then(|methods| methods.get(&method_name))
                    })
                    .cloned()
                    .ok_or_else(|| {
                        VeldError::RuntimeError(format!(
                            "Method '{}' not found on enum '{}'",
                            method_name, enum_name
                        ))
                    })?;

                match &method {
                    MethodImpl { params, body, .. } => {
                        self.push_scope();

                        // Bind 'self' to the enum instance by moving it.
                        self.current_scope_mut()
                            .declare("self".to_string(), object, VarKind::Var)
                            .unwrap(); // Should not fail in a new scope

                        // Debug: Show params and args before argument count check

                        // Assert argument count logic
                        assert_eq!(args.len(), params.len() - 1, "Argument count logic error!");

                        // Check argument count (excluding self)
                        if args.len() != params.len() - 1 {
                            return Err(VeldError::RuntimeError(format!(
                                "ENUM DISPATCH: Method '{}' expects {} arguments but got {}",
                                method_name,
                                params.len() - 1,
                                args.len()
                            )));
                        }

                        // Bind arguments directly (already evaluated)
                        for (i, arg) in args.into_iter().enumerate() {
                            self.current_scope_mut().set(params[i + 1].0.clone(), arg);
                        }

                        // Execute method body
                        let mut result = Value::Unit;
                        for stmt in body.into_iter() {
                            let stmt_result = self.execute_statement(stmt.clone())?;
                            if matches!(stmt_result, Value::Return(_)) {
                                result = stmt_result;
                                break;
                            }
                            result = stmt_result;
                        }

                        let has_mut_self = params.len() > 0 && params[0].0 == "mut self";
                        if has_mut_self {
                            if let Some(var_name) = &variable_name {
                                let updated_self =
                                    self.scopes.last().unwrap().get("self").map(|v| v.clone());
                                self.pop_scope();
                                if let Some(updated_value) = updated_self {
                                    self.set_variable(var_name, updated_value)?;
                                }
                                if matches!(result, Value::Return(_)) {
                                    return Ok(result.unwrap_return());
                                }

                                return Ok(Value::Unit);
                            }
                        }

                        self.pop_scope();

                        if matches!(result, Value::Return(_)) {
                            return Ok(result.unwrap_return());
                        }

                        // Return the actual result instead of Unit
                        Ok(result)
                    }
                    _ => Err(VeldError::RuntimeError(
                        "Internal error: enum method is not a MethodImpl".to_string(),
                    )),
                }
            }

            // Handle enum constructor calls like Option.Some(42) or Option.None
            (Value::EnumType { name, .. }, _) => {
                // Extract the simple enum name from qualified name like "std.option.Option" -> "Option"
                let enum_name_to_check = name.split('.').last().unwrap_or(name).to_string();

                // Check if we have the enum registered
                let enum_variants = if let Some(variants) = self.enums.get(name) {
                    variants.clone()
                } else if let Some(variants) = self.enums.get(&enum_name_to_check) {
                    variants.clone()
                } else {
                    return Err(VeldError::RuntimeError(format!(
                        "Unknown enum type: {}",
                        name
                    )));
                };

                // Find the variant by name from the Vec<EnumVariant>
                if let Some(variant) = enum_variants.iter().find(|v| v.name == method_name) {
                    match &variant.fields {
                        None => {
                            // Simple variant like None - should take no arguments
                            if !args.is_empty() {
                                return Err(VeldError::RuntimeError(format!(
                                    "Enum variant {} takes no arguments, got {}",
                                    method_name,
                                    args.len()
                                )));
                            }

                            // Create enum instance
                            Ok(Value::Enum {
                                enum_name: enum_name_to_check,
                                variant_name: method_name,
                                fields: vec![],
                            })
                        }
                        Some(field_types) => {
                            // Tuple variant like Some(T) - check argument count
                            if field_types.len() != args.len() {
                                return Err(VeldError::RuntimeError(format!(
                                    "Enum variant {} expects {} arguments, got {}",
                                    method_name,
                                    field_types.len(),
                                    args.len()
                                )));
                            }

                            // Create enum instance with field values
                            Ok(Value::Enum {
                                enum_name: enum_name_to_check,
                                variant_name: method_name,
                                fields: args,
                            })
                        }
                    }
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Enum {} has no variant {}",
                        name, method_name
                    )))
                }
            }

            _ => Err(VeldError::RuntimeError(format!(
                "Method '{}' not found",
                method_name
            ))),
        }
    }

    fn is_truthy(&self, value: Value) -> bool {
        match value {
            Value::Boolean(b) => b,
            Value::Integer(n) => n != 0,
            Value::Float(f) => f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Unit => false,
            _ => true,
        }
    }

    fn to_numeric_value(&self, value: Value) -> Result<Option<NumericValue>> {
        match value {
            Value::Numeric(nv) => Ok(Some(nv)),
            Value::Integer(i) => Ok(Some(NumericValue::Integer(IntegerValue::I64(i)))),
            Value::Float(f) => Ok(Some(NumericValue::Float(FloatValue::F64(f)))),
            _ => Ok(None),
        }
    }

    fn from_numeric_option(&self, numeric_opt: Option<NumericValue>) -> Value {
        match numeric_opt {
            Some(nv) => Value::Numeric(nv),
            None => Value::Unit, // This should not happen in normal execution
        }
    }

    fn compare_numeric_values(
        &self,
        a: &NumericValue,
        b: &NumericValue,
        op: &BinaryOperator,
    ) -> Result<bool> {
        let a_f64 = a.clone().as_f64();
        let b_f64 = b.clone().as_f64();

        Ok(match op {
            BinaryOperator::Less => a_f64 < b_f64,
            BinaryOperator::Greater => a_f64 > b_f64,
            BinaryOperator::LessEq => a_f64 <= b_f64,
            BinaryOperator::GreaterEq => a_f64 >= b_f64,
            BinaryOperator::EqualEqual => a_f64 == b_f64,
            BinaryOperator::NotEqual => a_f64 != b_f64,
            _ => {
                return Err(VeldError::RuntimeError(
                    "Invalid comparison operator".into(),
                ));
            }
        })
    }

    fn numeric_values_equal(&self, a: &NumericValue, b: &NumericValue) -> bool {
        let result = a.clone().as_f64() == b.clone().as_f64();
        result
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::Numeric(a), Value::Numeric(b)) => a.clone().as_f64() == b.clone().as_f64(),
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| self.values_equal(x, y))
            }
            (Value::Tuple(a), Value::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| self.values_equal(x, y))
            }
            (Value::Record(a), Value::Record(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .all(|(k, v)| b.get(k).map_or(false, |bv| self.values_equal(v, bv)))
            }
            (
                Value::Struct {
                    name: an,
                    fields: af,
                },
                Value::Struct {
                    name: bn,
                    fields: bf,
                },
            ) => {
                an == bn
                    && af.len() == bf.len()
                    && af
                        .iter()
                        .all(|(k, v)| bf.get(k).map_or(false, |bv| self.values_equal(v, bv)))
            }
            (
                Value::Enum {
                    enum_name: an,
                    variant_name: av,
                    fields: af,
                },
                Value::Enum {
                    enum_name: bn,
                    variant_name: bv,
                    fields: bf,
                },
            ) => {
                an == bn
                    && av == bv
                    && af.len() == bf.len()
                    && af
                        .iter()
                        .zip(bf.iter())
                        .all(|(x, y)| self.values_equal(x, y))
            }
            _ => false,
        }
    }

    fn evaluate_non_numeric_binary_op(
        &self,
        left: Option<NumericValue>,
        right: Option<NumericValue>,
        operator: BinaryOperator,
    ) -> Result<Value> {
        // Handle string concatenation and other non-numeric operations
        let left_val = self.from_numeric_option(left);
        let right_val = self.from_numeric_option(right);

        match (left_val, right_val) {
            (Value::String(a), Value::String(b)) => {
                let result = match operator {
                    BinaryOperator::Less => a < b,
                    BinaryOperator::LessEq => a <= b,
                    BinaryOperator::Greater => a > b,
                    BinaryOperator::GreaterEq => a >= b,
                    BinaryOperator::EqualEqual => a == b,
                    BinaryOperator::NotEqual => a != b,
                    _ => return Err(VeldError::RuntimeError("Invalid string comparison".into())),
                };
                Ok(Value::Boolean(result))
            }
            _ => Err(VeldError::RuntimeError(
                "Invalid non-numeric binary operation".into(),
            )),
        }
    }

    fn evaluate_binary_op(
        &mut self,
        left: Value,
        operator: BinaryOperator,
        right: Value,
    ) -> Result<Value> {
        if let Some(result) = self.try_call_operator_method(&left, &operator, &right)? {
            return Ok(result);
        }

        // Handle string concatenation before numeric conversion
        if matches!(operator, BinaryOperator::Add) {
            if let (Value::String(a), Value::String(b)) = (&left, &right) {
                return Ok(Value::String(a.clone() + b));
            }
        }

        let left_numeric = self.to_numeric_value(left.clone())?;
        let right_numeric = self.to_numeric_value(right.clone())?;

        match operator {
            BinaryOperator::Pipe => {
                // This is a fallback in case the special handling in evaluate_expression isn't used
                match right {
                    Value::Function { .. } => {
                        let temp_name = "___pipe_temp___".to_string();
                        self.current_scope_mut().set(temp_name.clone(), right);

                        let args = vec![left];
                        let result = self.call_function_with_values(temp_name, args);

                        result
                    }
                    _ => Err(VeldError::RuntimeError(format!(
                        "Cannot pipe into non-function value: {:?}",
                        right
                    ))),
                }
            }
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulo
            | BinaryOperator::Exponent => match (&left_numeric, &right_numeric) {
                (Some(a), Some(b)) => {
                    let result = a.perform_operation(&operator, b)?;
                    Ok(Value::Numeric(result))
                }
                _ => self.evaluate_non_numeric_binary_op(left_numeric, right_numeric, operator),
            },

            // Comparison operators
            BinaryOperator::LessEq
            | BinaryOperator::GreaterEq
            | BinaryOperator::Less
            | BinaryOperator::Greater => match (&left_numeric, &right_numeric) {
                (Some(a), Some(b)) => {
                    let result = self.compare_numeric_values(a, b, &operator)?;
                    Ok(Value::Boolean(result))
                }
                _ => self.evaluate_non_numeric_binary_op(left_numeric, right_numeric, operator),
            },

            // Equality operators
            BinaryOperator::EqualEqual | BinaryOperator::NotEqual => {
                let is_equal = match (&left_numeric, &right_numeric) {
                    (Some(a), Some(b)) => self.numeric_values_equal(a, b),
                    _ => self.values_equal(&left, &right),
                };

                let result = match operator {
                    BinaryOperator::EqualEqual => is_equal,
                    BinaryOperator::NotEqual => !is_equal,
                    _ => unreachable!(),
                };
                Ok(Value::Boolean(result))
            }

            // Logical operators
            BinaryOperator::And | BinaryOperator::Or => {
                let left_val = left;
                let right_val = right;

                match operator {
                    BinaryOperator::And => Ok(Value::Boolean(
                        self.is_truthy(left_val) && self.is_truthy(right_val),
                    )),
                    BinaryOperator::Or => Ok(Value::Boolean(
                        self.is_truthy(left_val) || self.is_truthy(right_val),
                    )),
                    _ => unreachable!(),
                }
            }
        }
    }

    // Call a function value (not just by name), supporting closures, property-accessed functions, etc.
    fn call_function_value(&mut self, func: Value, arg_values: Vec<Value>) -> Result<Value> {
        self.call_function_value_with_name(func, arg_values, None)
    }

    // Call a function value with optional name context for better native function resolution
    fn call_function_value_with_name(
        &mut self,
        func: Value,
        arg_values: Vec<Value>,
        function_name: Option<String>,
    ) -> Result<Value> {
        match func {
            Value::Function {
                params,
                body,
                return_type: _,
                captured_vars,
            } => {
                // Check if this is an imported function with empty body that should redirect to native
                if body.is_empty() {
                    // Build potential native function names, prioritizing the actual function name
                    let mut potential_native_names = Vec::new();

                    if let Some(ref name) = function_name {
                        // Try exact matches first based on the function name
                        potential_native_names.push(format!("std.io.{}", name));
                        potential_native_names.push(format!("std.math.{}", name));
                    }

                    // Add common fallbacks for common standard library functions
                    potential_native_names.extend([
                        "std.io.print".to_string(),
                        "std.io.println".to_string(),
                        "std.math.sqrt".to_string(),
                        "std.math.pow".to_string(),
                        "std.math.sin".to_string(),
                        "std.math.cos".to_string(),
                        "std.math.tan".to_string(),
                        "std.math.abs".to_string(),
                        "std.math.min".to_string(),
                        "std.math.max".to_string(),
                        "std.math.clamp".to_string(),
                    ]);

                    for native_name in &potential_native_names {
                        if self.native_registry.contains(native_name) {
                            let result =
                                if let Some(handler) = self.native_registry.get(native_name) {
                                    handler(self, arg_values)
                                } else if self.native_registry.contains_static(native_name) {
                                    self.native_registry
                                        .call_static(native_name, self, arg_values)
                                } else {
                                    continue;
                                };
                            return result;
                        }
                    }
                }
                self.push_scope();

                // Set up captured variables in the new scope
                // Skip Unit placeholders to allow recursive lambdas to find their actual values in parent scopes
                for (var_name, var_value) in captured_vars {
                    if !matches!(var_value, Value::Unit) {
                        self.current_scope_mut().set(var_name, var_value);
                    }
                }

                // Bind arguments
                if arg_values.len() != params.len() {
                    return Err(VeldError::RuntimeError(format!(
                        "Expected {} arguments but got {}",
                        params.len(),
                        arg_values.len()
                    )));
                }

                for (i, arg) in arg_values.iter().enumerate() {
                    let param_name = params[i].0.clone();
                    let arg_value = arg.clone();
                    self.current_scope_mut().set(param_name, arg_value);
                }

                // Execute function body
                let mut result = Value::Unit;
                for stmt in body.into_iter() {
                    let stmt_result = self.execute_statement(stmt.clone())?;
                    if matches!(stmt_result, Value::Return(_)) {
                        result = stmt_result;
                        break;
                    }
                    result = stmt_result;
                }

                self.pop_scope();

                if matches!(result, Value::Return(_)) {
                    return Ok(result.unwrap_return());
                }
                // If the last statement produced a value (not Unit), return it
                if !matches!(result, Value::Unit) {
                    return Ok(result);
                }
                Ok(Value::Unit)
            }
            // If you have native functions, add a case here
            // Value::NativeFunction(f) => f(arg_values),
            _ => Err(VeldError::RuntimeError("Value is not callable".to_string())),
        }
    }

    // Scope management

    /// Returns a mutable reference to the global scope.
    pub fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[0]
    }

    /// Returns a mutable reference to the current scope.
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        if self.scopes.is_empty() {
            let new_level = self.scopes.len();
            self.scopes.push(Scope::new(new_level));
        }
        self.scopes.last_mut().unwrap()
    }

    /// Returns a reference to the current scope.
    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    // Helper function to check if a lambda expression references a given variable name
    fn lambda_references_name(&self, expr: &Expr, name: &str) -> bool {
        match expr {
            Expr::Lambda { body, .. } => self.expr_references_name(body, name),
            _ => false,
        }
    }

    // Helper function to check if an expression references a given variable name
    fn expr_references_name(&self, expr: &Expr, name: &str) -> bool {
        match expr {
            Expr::Identifier(id) => id == name,
            Expr::Call { callee, arguments } => {
                self.expr_references_name(callee, name)
                    || arguments.iter().any(|arg| match arg {
                        Argument::Positional(e) => self.expr_references_name(e, name),
                        Argument::Named { name: _, value: e } => self.expr_references_name(e, name),
                    })
            }
            Expr::BinaryOp { left, right, .. } => {
                self.expr_references_name(left, name) || self.expr_references_name(right, name)
            }
            Expr::UnaryOp { operand, .. } => self.expr_references_name(operand, name),
            Expr::IfExpression {
                condition,
                then_expr,
                else_expr,
            } => {
                self.expr_references_name(condition, name)
                    || self.expr_references_name(then_expr, name)
                    || else_expr
                        .as_ref()
                        .map_or(false, |e| self.expr_references_name(e, name))
            }
            Expr::Lambda { body, .. } => self.expr_references_name(body, name),
            Expr::BlockExpression { statements, .. } => statements
                .iter()
                .any(|stmt| self.stmt_references_name(stmt, name)),
            // Add other expression types as needed
            _ => false,
        }
    }

    // Helper function to check if a statement references a given variable name
    fn stmt_references_name(&self, stmt: &Statement, name: &str) -> bool {
        match stmt {
            Statement::ExprStatement(expr) => self.expr_references_name(expr, name),
            Statement::VariableDeclaration { value, .. } => self.expr_references_name(value, name),
            Statement::Assignment { value, .. } => self.expr_references_name(value, name),
            // Add other statement types as needed
            _ => false,
        }
    }

    fn push_scope(&mut self) {
        let new_level = self.scopes.len();
        self.scopes.push(Scope::new(new_level));
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn get_variable(&self, name: &str) -> Option<Value> {
        // 1. Check local scopes (from innermost to outermost)
        for (_i, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }

        // 2. Check if it's a loaded root module (like "std")
        if self.module_manager.is_module_loaded(name) {
            if let Some(module) = self.module_manager.get_module(name) {
                // Return the module as a Value::Module
                return Some(Value::Module(module.clone()));
            }
        }

        // 3. Not found

        None
    }

    fn set_variable(&mut self, name: &str, value: Value) -> Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.vals().contains_key(name) {
                return scope.assign(name, value);
            }
        }
        Err(VeldError::RuntimeError(format!(
            "Cannot assign to undefined variable '{}'",
            name
        )))
    }

    fn execute_plex_declaration(
        &mut self,
        name: String,
        type_annotation: TypeAnnotation,
        generic_params: Vec<GenericArgument>,
    ) -> Result<Value> {
        // If it's a generic plex type, defer processing to type checker
        if !generic_params.is_empty() {
            // Just store the name for now, the type checker will handle the generic processing
            return Ok(Value::Unit);
        }

        // For non-generic plex types, process immediately
        let ty = self
            .type_checker
            .env()
            .from_annotation(&type_annotation, None)?;
        self.type_checker.env().add_type_alias(&name, ty);
        Ok(Value::Unit)
    }

    /// Check if a type conversion is a safe widening conversion (no data loss)
    fn is_safe_widening(&self, from_type: &Type, to_type: &Type) -> bool {
        match (from_type, to_type) {
            // Same type is always safe
            (a, b) if a == b => true,

            // Integer to larger integer is safe
            (Type::I8, Type::I16 | Type::I32 | Type::I64) => true,
            (Type::I16, Type::I32 | Type::I64) => true,
            (Type::I32, Type::I64) => true,

            // Unsigned to larger unsigned is safe
            (Type::U8, Type::U16 | Type::U32 | Type::U64) => true,
            (Type::U16, Type::U32 | Type::U64) => true,
            (Type::U32, Type::U64) => true,

            // Small integers to floats is safe (within precision limits)
            (Type::I8 | Type::I16 | Type::I32, Type::F32 | Type::F64) => true,
            (Type::U8 | Type::U16 | Type::U32, Type::F32 | Type::F64) => true,

            // I64/U64 to F64 is safe (F64 has 53 bits of precision)
            (Type::I64 | Type::U64, Type::F64) => true,

            // F32 to F64 is safe
            (Type::F32, Type::F64) => true,

            _ => false,
        }
    }

    /// Attempt to safely coerce a value to match the target type through safe widening
    fn try_safe_coerce_value(&self, value: &Value, target_type: &Type) -> Result<Value> {
        match (value, target_type) {
            // Enum coercion - handle cases like None being coerced to Option<i32>
            (
                Value::Enum {
                    enum_name,
                    variant_name: _,
                    fields: _,
                },
                Type::Generic { base, type_args: _ },
            ) => {
                // Check if the enum names are compatible
                let enum_simple_name = enum_name.split('.').last().unwrap_or(enum_name);
                if enum_simple_name == base {
                    // TODO: When where clauses are fully implemented, need to validate
                    // that the type arguments in target_type satisfy any where clause
                    // constraints on the enum's methods (e.g., Option<T>.to_string()
                    // should only be available when T: ToString)

                    // Return the same enum value but it's now considered to have the target type
                    Ok(value.clone())
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot coerce enum {} to {}",
                        enum_name, base
                    )))
                }
            }
            // Record coercion - coerce each field if possible
            (
                Value::Record(actual_fields),
                Type::Record {
                    fields: target_fields,
                },
            ) => {
                if actual_fields.len() != target_fields.len() {
                    return Err(VeldError::RuntimeError(
                        "Record field count mismatch".to_string(),
                    ));
                }

                let mut coerced_fields = std::collections::HashMap::new();

                for (field_name, target_field_type) in target_fields {
                    if let Some(actual_field_value) = actual_fields.get(field_name) {
                        let actual_field_type = actual_field_value.type_of();

                        if self.is_safe_widening(&actual_field_type, target_field_type) {
                            // Use existing cast_value for safe conversions
                            let coerced_field =
                                self.cast_value(actual_field_value.clone(), target_field_type)?;
                            coerced_fields.insert(field_name.clone(), coerced_field);
                        } else if actual_field_type == *target_field_type {
                            // Types already match
                            coerced_fields.insert(field_name.clone(), actual_field_value.clone());
                        } else {
                            return Err(VeldError::RuntimeError(format!(
                                "Cannot safely coerce field '{}' from {} to {}",
                                field_name, actual_field_type, target_field_type
                            )));
                        }
                    } else {
                        return Err(VeldError::RuntimeError(format!(
                            "Missing field '{}' in record",
                            field_name
                        )));
                    }
                }

                Ok(Value::Record(coerced_fields))
            }
            // Direct value coercion for safe widening
            _ => {
                let value_type = value.type_of();
                if self.is_safe_widening(&value_type, target_type) {
                    self.cast_value(value.clone(), target_type)
                } else if value_type == *target_type {
                    Ok(value.clone())
                } else {
                    Err(VeldError::RuntimeError(format!(
                        "Cannot safely coerce {} to {}",
                        value_type, target_type
                    )))
                }
            }
        }
    }

    fn execute_module_declaration(
        &mut self,
        name: String,
        body: Option<Vec<Statement>>,
        is_public: bool,
    ) -> Result<Value> {
        if let Some(statements) = body {
            // Save current module name
            let _previous_module = self.current_module.clone();
            self.current_module = name.clone();

            // Create new scope for module
            self.push_scope();

            // Execute all statements in the module
            let mut result = Value::Unit;
            let mut last_stmt_is_expr = false;
            let body_len = statements.len();
            for (i, stmt) in statements.into_iter().enumerate() {
                let stmt_result = self.execute_statement(stmt.clone())?;
                if matches!(stmt_result, Value::Return(_)) {
                    result = stmt_result;
                    break;
                }
                if i == body_len - 1 {
                    if let Statement::ExprStatement(_) = &stmt {
                        last_stmt_is_expr = true;
                        result = stmt_result;
                    } else {
                        result = stmt_result;
                    }
                }
            }

            self.pop_scope();

            if matches!(result, Value::Return(_)) {
                return Ok(result.unwrap_return());
            }
            if last_stmt_is_expr {
                return Ok(result);
            }
            Ok(Value::Unit)
        } else {
            // Just a module declaration referencing a file
            // Try to load the module from filesystem
            self.module_manager.load_module(&[name])?;
            Ok(Value::Unit)
        }
    }

    fn execute_import_declaration(
        &mut self,
        path: Vec<String>,
        items: Vec<ImportItem>,
        alias: Option<String>,
        is_public: bool,
    ) -> Result<Value> {
        let mut actual_path = path.clone();
        let mut actual_items = items.clone();

        // Check if this might be a single-item import by trying the fallback first
        if actual_path.len() > 1 && actual_items == vec![ImportItem::All] {
            // Try treating the last component as an item name
            let mut parent_path = actual_path.clone();
            let potential_item = parent_path.pop().unwrap();

            // Try to load parent module first
            match self.module_manager.load_module(&parent_path) {
                Ok(_) => {
                    // Successfully loaded parent module, treat last component as item
                    actual_path = parent_path;
                    actual_items = vec![ImportItem::Named(potential_item)];
                }
                Err(_) => {
                    // Parent module load failed, try original path
                    self.module_manager.load_module(&actual_path)?;
                }
            }
        } else {
            // Simple case - load module as specified
            self.module_manager.load_module(&actual_path)?;
        }

        // Use the adjusted path and items
        let actual_module_path_str = actual_path.join(".");

        // Process imports based on alias or direct imports
        if let Some(alias_name) = alias {
            // Store the module alias -> actual name mapping
            self.imported_modules
                .insert(alias_name, actual_module_path_str.clone());
        } else {
            // No alias, so we're importing specific items or the entire module

            // Get the exports we want
            let exports = self
                .module_manager
                .get_exports(&actual_module_path_str, &actual_items)?;

            // To avoid double borrowing, collect all needed data first
            let module_statements = {
                let module = self
                    .module_manager
                    .get_module(&actual_module_path_str)
                    .ok_or_else(|| VeldError::RuntimeError("Module not found".to_string()))?;
                module.statements.clone()
            };

            // To avoid double execution, track executed modules by name
            if !self.imported_modules.contains_key(&actual_module_path_str) {
                self.imported_modules.insert(
                    actual_module_path_str.clone(),
                    actual_module_path_str.clone(),
                );
                for statement in &module_statements {
                    self.execute_statement(statement.clone())?;
                }
            }

            // Process each export and add to current scope
            // First pass: process enums to ensure types are available for functions
            for (_name, export_item) in &exports {
                if let ExportedItem::Enum(idx) = export_item {
                    // To avoid borrow checker issues, clone what we need first
                    let (enum_name, variants) = if let Statement::EnumDeclaration {
                        name: enum_name,
                        variants,
                        ..
                    } = &module_statements[*idx]
                    {
                        (enum_name.clone(), variants.clone())
                    } else {
                        continue;
                    };

                    self.enums.insert(enum_name.clone(), variants.clone());
                    // Register the enum in the type environment for type checking
                    fn convert_ast_enum_variant_to_base(
                        ast_variant: &EnumVariant,
                    ) -> veld_common::types::EnumVariant {
                        match &ast_variant.fields {
                            None => veld_common::types::EnumVariant::Simple,
                            Some(fields) => {
                                // For now, treat all as Tuple (adjust if you support named fields)
                                let tuple_types: Vec<Type> = fields
                                    .iter()
                                    .map(|anno| {
                                        veld_common::types::Type::from_annotation(anno, None)
                                            .unwrap_or(veld_common::types::Type::Any)
                                    })
                                    .collect();
                                veld_common::types::EnumVariant::Tuple(tuple_types)
                            }
                        }
                    }
                    let variant_map: std::collections::HashMap<
                        String,
                        veld_common::types::EnumVariant,
                    > = variants
                        .iter()
                        .map(|v| (v.name.clone(), convert_ast_enum_variant_to_base(v)))
                        .collect();
                    self.type_checker.env().add_enum(&enum_name, variant_map);
                }
            }

            // Second pass: process other exports including functions
            for (name, export_item) in exports {
                match export_item {
                    ExportedItem::Function(idx) => {
                        if let Statement::FunctionDeclaration {
                            params,
                            return_type,
                            body,
                            generic_params,
                            ..
                        } = &module_statements[idx]
                        {
                            let function = Value::Function {
                                params: params.clone(),
                                body: body.clone(),
                                return_type: return_type.clone(),
                                captured_vars: HashMap::new(), // No captured vars for imports
                            };
                            self.current_scope_mut().set(name.clone(), function);

                            // Also add function to type environment for type checking
                            // Set up type parameter scope for generic functions
                            self.type_checker.env().push_type_param_scope();
                            for generic_param in generic_params {
                                let param_name = match &generic_param.name {
                                    Some(name) => name.clone(),
                                    None => {
                                        if let TypeAnnotation::Basic(base_name) =
                                            &generic_param.type_annotation
                                        {
                                            base_name.clone()
                                        } else {
                                            "T".to_string()
                                        }
                                    }
                                };
                                self.type_checker.env().add_type_param(&param_name);
                            }

                            let param_types: Vec<Type> = params
                                .iter()
                                .map(|(_, type_annotation)| {
                                    self.type_checker
                                        .env()
                                        .from_annotation(type_annotation, None)
                                        .unwrap_or(Type::Any)
                                })
                                .collect();
                            let resolved_return_type = self
                                .type_checker
                                .env()
                                .from_annotation(return_type, None)
                                .unwrap_or(Type::Any);
                            let function_type = Type::Function {
                                params: param_types,
                                return_type: Box::new(resolved_return_type),
                            };

                            // Clean up type parameter scope
                            self.type_checker.env().pop_type_param_scope();

                            self.type_checker.env().define(&name, function_type);
                        }
                    }
                    ExportedItem::Struct(idx) => {
                        // Handle struct imports - for now just remember the name
                        if let Statement::StructDeclaration {
                            name: _struct_name,
                            fields,
                            ..
                        } = &module_statements[idx]
                        {
                            // Register the struct type in this scope
                            self.structs.insert(name.clone(), fields.clone());

                            // Convert StructField to HashMap<String, Type> for type checker
                            let mut field_types = HashMap::new();
                            for field in fields {
                                let field_type = self
                                    .type_checker
                                    .env()
                                    .from_annotation(&field.type_annotation, None)
                                    .unwrap_or(Type::Any); // Fallback to Any type if conversion fails
                                field_types.insert(field.name.clone(), field_type);
                            }

                            // Register struct in type checker environment
                            self.type_checker.env().add_struct(&name, field_types);

                            // Also register the struct as an identifier in the type environment
                            // This allows struct types to be used in property access like Vec.new
                            let struct_type = Type::Generic {
                                base: name.clone(),
                                type_args: vec![],
                            };
                            self.type_checker.env().define(&name, struct_type);

                            // Also add the struct name to the current scope as a "type value"
                            // This allows Vec.new, etc
                            self.current_scope_mut().set(
                                name.clone(),
                                Value::StructType {
                                    name: name.clone(),
                                    methods: None,
                                },
                            );

                            // Also import any impl blocks for this struct
                            for (_stmt_idx, stmt) in module_statements.iter().enumerate() {
                                match stmt {
                                    Statement::InherentImpl {
                                        type_name,
                                        methods,
                                        generic_params,
                                        ..
                                    } => {
                                        if type_name == &name {
                                            // Execute the impl block to register methods
                                            self.execute_implementation(
                                                type_name.clone(),
                                                methods.clone(),
                                            )?;

                                            // Also add methods to type environment for type checking

                                            // Set up type parameter scope for generic impl blocks
                                            self.type_checker.env().push_type_param_scope();
                                            for generic_arg in generic_params {
                                                let param_name = match &generic_arg.name {
                                                    Some(name) => name.clone(),
                                                    None => {
                                                        if let TypeAnnotation::Basic(base_name) =
                                                            &generic_arg.type_annotation
                                                        {
                                                            base_name.clone()
                                                        } else {
                                                            "T".to_string()
                                                        }
                                                    }
                                                };
                                                self.type_checker.env().add_type_param(&param_name);
                                            }

                                            for method in methods {
                                                // Set up method-level type parameters
                                                self.type_checker.env().push_type_param_scope();
                                                for generic_arg in &method.generic_params {
                                                    let param_name = match &generic_arg.name {
                                                        Some(name) => name.clone(),
                                                        None => {
                                                            if let TypeAnnotation::Basic(
                                                                base_name,
                                                            ) = &generic_arg.type_annotation
                                                            {
                                                                base_name.clone()
                                                            } else {
                                                                "U".to_string()
                                                            }
                                                        }
                                                    };
                                                    self.type_checker
                                                        .env()
                                                        .add_type_param(&param_name);
                                                }

                                                let param_types: Vec<Type> = method
                                                    .params
                                                    .iter()
                                                    .map(|(_, type_annotation)| {
                                                        self.type_checker
                                                            .env()
                                                            .from_annotation(type_annotation, None)
                                                            .unwrap_or(Type::Any)
                                                    })
                                                    .collect();
                                                let return_type = self
                                                    .type_checker
                                                    .env()
                                                    .from_annotation(&method.return_type, None)
                                                    .unwrap_or(Type::Any);
                                                let function_type = Type::Function {
                                                    params: param_types,
                                                    return_type: Box::new(return_type),
                                                };
                                                self.type_checker.env().add_struct_method(
                                                    type_name,
                                                    &method.name,
                                                    function_type,
                                                );

                                                // Clean up method-level type parameter scope
                                                self.type_checker.env().pop_type_param_scope();
                                            }

                                            // Clean up type parameter scope
                                            self.type_checker.env().pop_type_param_scope();
                                        }
                                    }
                                    Statement::Implementation {
                                        type_name,
                                        kind_name: _,
                                        methods,
                                        generic_args,
                                        ..
                                    } => {
                                        if type_name == &name {
                                            // Execute the impl block to register methods
                                            self.execute_implementation(
                                                type_name.clone(),
                                                methods.clone(),
                                            )?;

                                            // Also add methods to type environment for type checking

                                            // Set up type parameter scope for generic impl blocks
                                            self.type_checker.env().push_type_param_scope();
                                            for generic_arg in generic_args {
                                                let param_name = match &generic_arg.name {
                                                    Some(name) => name.clone(),
                                                    None => {
                                                        if let TypeAnnotation::Basic(base_name) =
                                                            &generic_arg.type_annotation
                                                        {
                                                            base_name.clone()
                                                        } else {
                                                            "T".to_string()
                                                        }
                                                    }
                                                };
                                                self.type_checker.env().add_type_param(&param_name);
                                            }

                                            for method in methods {
                                                // Set up method-level type parameters
                                                self.type_checker.env().push_type_param_scope();
                                                for generic_arg in &method.generic_params {
                                                    let param_name = match &generic_arg.name {
                                                        Some(name) => name.clone(),
                                                        None => {
                                                            if let TypeAnnotation::Basic(
                                                                base_name,
                                                            ) = &generic_arg.type_annotation
                                                            {
                                                                base_name.clone()
                                                            } else {
                                                                "U".to_string()
                                                            }
                                                        }
                                                    };
                                                    self.type_checker
                                                        .env()
                                                        .add_type_param(&param_name);
                                                }

                                                let param_types: Vec<Type> = method
                                                    .params
                                                    .iter()
                                                    .map(|(_, type_annotation)| {
                                                        self.type_checker
                                                            .env()
                                                            .from_annotation(type_annotation, None)
                                                            .unwrap_or(Type::Any)
                                                    })
                                                    .collect();
                                                let return_type = self
                                                    .type_checker
                                                    .env()
                                                    .from_annotation(&method.return_type, None)
                                                    .unwrap_or(Type::Any);
                                                let function_type = Type::Function {
                                                    params: param_types,
                                                    return_type: Box::new(return_type),
                                                };
                                                self.type_checker.env().add_struct_method(
                                                    type_name,
                                                    &method.name,
                                                    function_type,
                                                );

                                                // Clean up method-level type parameter scope
                                                self.type_checker.env().pop_type_param_scope();
                                            }

                                            // Clean up type parameter scope
                                            self.type_checker.env().pop_type_param_scope();
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    ExportedItem::Enum(idx) => {
                        // Enum already processed in first pass, but we still need to add it to scope
                        if let Statement::EnumDeclaration {
                            name: enum_name, ..
                        } = &module_statements[idx]
                        {
                            // Also register the enum as an identifier in the type environment
                            // This allows enum types to be used in property access like Option.None
                            let enum_type = Type::Generic {
                                base: enum_name.clone(),
                                type_args: vec![],
                            };
                            self.type_checker.env().define(&name, enum_type);

                            // Also add the enum name to the current scope as a "type value"
                            // This allows Option.Some, etc
                            self.current_scope_mut().set(
                                name.clone(),
                                Value::EnumType {
                                    name: enum_name.clone(),
                                    methods: None,
                                },
                            );

                            // Also import any impl blocks for this enum
                            for (_stmt_idx, stmt) in module_statements.iter().enumerate() {
                                if let Statement::InherentImpl {
                                    type_name,
                                    methods,
                                    generic_params,
                                    ..
                                } = stmt
                                {
                                    if type_name == enum_name {
                                        // Execute the impl block to register methods
                                        self.execute_implementation(
                                            type_name.clone(),
                                            methods.clone(),
                                        )?;

                                        // Also add methods to type environment for type checking

                                        // Set up type parameter scope for generic impl blocks
                                        self.type_checker.env().push_type_param_scope();
                                        for generic_arg in generic_params {
                                            let param_name = match &generic_arg.name {
                                                Some(name) => name.clone(),
                                                None => {
                                                    if let TypeAnnotation::Basic(base_name) =
                                                        &generic_arg.type_annotation
                                                    {
                                                        base_name.clone()
                                                    } else {
                                                        "T".to_string()
                                                    }
                                                }
                                            };
                                            self.type_checker.env().add_type_param(&param_name);
                                        }

                                        for method in methods {
                                            // Set up method-level type parameters
                                            self.type_checker.env().push_type_param_scope();
                                            for generic_arg in &method.generic_params {
                                                let param_name = match &generic_arg.name {
                                                    Some(name) => name.clone(),
                                                    None => {
                                                        if let TypeAnnotation::Basic(base_name) =
                                                            &generic_arg.type_annotation
                                                        {
                                                            base_name.clone()
                                                        } else {
                                                            "U".to_string()
                                                        }
                                                    }
                                                };
                                                self.type_checker.env().add_type_param(&param_name);
                                            }

                                            let param_types: Vec<Type> = method
                                                .params
                                                .iter()
                                                .map(|(_, type_annotation)| {
                                                    self.type_checker
                                                        .env()
                                                        .from_annotation(type_annotation, None)
                                                        .unwrap_or(Type::Any)
                                                })
                                                .collect();
                                            let return_type = self
                                                .type_checker
                                                .env()
                                                .from_annotation(&method.return_type, None)
                                                .unwrap_or(veld_common::types::Type::Any);
                                            let function_type =
                                                veld_common::types::Type::Function {
                                                    params: param_types,
                                                    return_type: Box::new(return_type),
                                                };
                                            self.type_checker.env().add_enum_method(
                                                type_name,
                                                &method.name,
                                                function_type,
                                            );

                                            // Clean up method-level type parameter scope
                                            self.type_checker.env().pop_type_param_scope();
                                        }

                                        // Clean up type parameter scope
                                        self.type_checker.env().pop_type_param_scope();
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if is_public {
            let current_module = self.get_current_module().to_string();
            let exports = self
                .module_manager
                .get_exports(&actual_module_path_str, &actual_items)?;

            let module = self
                .module_manager
                .get_module_mut(&current_module)
                .ok_or_else(|| VeldError::RuntimeError("Current module not found".to_string()))?;

            for (name, export_item) in exports {
                module.exports.insert(name, export_item);
            }
        }

        Ok(Value::Unit)
    }

    fn load_stdlib_module_with_types(&mut self, path: &[&str]) {
        let path_vec: Vec<String> = path.iter().map(|s| s.to_string()).collect();
        let module_name = path_vec.join(".");

        // Load the module
        if let Err(e) = self.module_manager.load_module(&path_vec) {
            tracing::warn!("Failed to load stdlib module {}: {}", module_name, e);
            return;
        }

        // Get the loaded module
        if let Some(module) = self.module_manager.get_module(&module_name) {
            // Register structs and their methods from the module
            for statement in &module.statements {
                if let Statement::StructDeclaration {
                    name: struct_name,
                    fields,
                    generic_params,
                    ..
                } = statement
                {
                    // Create fully qualified struct name (e.g., "std.vec.Vec")
                    let qualified_name = format!("{}.{}", module_name, struct_name);
                    tracing::debug!(
                        "Registering struct: {} with qualified name: {}",
                        struct_name,
                        qualified_name
                    );

                    // Register the struct in the runtime
                    self.structs.insert(qualified_name.clone(), fields.clone());

                    // Also register with simple name for struct construction (e.g., Vec(...))
                    self.structs.insert(struct_name.clone(), fields.clone());

                    // Set up type parameter scope for struct fields processing
                    let has_generics = !generic_params.is_empty();
                    let mut shared_type_substitutions = std::collections::HashMap::new();

                    if has_generics {
                        self.type_checker.env().push_type_param_scope();

                        // Create shared type variable substitutions for this generic struct
                        for param in generic_params {
                            let param_name = match &param.name {
                                Some(name) => name.clone(),
                                None => {
                                    if let veld_common::ast::TypeAnnotation::Basic(base_name) =
                                        &param.type_annotation
                                    {
                                        base_name.clone()
                                    } else {
                                        "T".to_string()
                                    }
                                }
                            };
                            self.type_checker.env().add_type_param(&param_name);

                            // Create a shared type variable for this parameter
                            shared_type_substitutions
                                .insert(param_name, self.type_checker.env().fresh_type_var());
                        }
                    }

                    // Convert StructField to HashMap<String, Type> for type checker
                    let mut field_types = HashMap::new();
                    for field in fields {
                        let mut field_type = match self
                            .type_checker
                            .env()
                            .from_annotation(&field.type_annotation, None)
                        {
                            Ok(ty) => ty,
                            Err(e) => {
                                tracing::error!(
                                    "Failed to resolve field type annotation {:?} for struct {}: {}",
                                    field.type_annotation,
                                    struct_name,
                                    e
                                );
                                // For now, fall back to Any type instead of failing
                                Type::Any
                            }
                        };

                        // Use shared type variable substitutions
                        if has_generics && !shared_type_substitutions.is_empty() {
                            field_type = self
                                .type_checker
                                .env()
                                .substitute_type_params(&field_type, &shared_type_substitutions);
                        }

                        field_types.insert(field.name.clone(), field_type);
                    }

                    // Pop type parameter scope if we pushed one
                    if has_generics {
                        self.type_checker.env().pop_type_param_scope();
                    }

                    // Register struct in type checker environment with qualified name
                    self.type_checker
                        .env()
                        .add_struct(&qualified_name, field_types.clone());
                    tracing::debug!("Struct {} registered in type environment", qualified_name);

                    // Also register with simple name for generic type annotations like Vec<T>
                    self.type_checker
                        .env()
                        .add_struct(&struct_name, field_types);
                    tracing::debug!(
                        "Struct {} registered in type environment with simple name",
                        struct_name
                    );

                    // Register struct methods from inherent implementation blocks
                    for impl_stmt in &module.statements {
                        if let Statement::InherentImpl {
                            type_name,
                            methods,
                            generic_params,
                            ..
                        } = impl_stmt
                        {
                            if type_name == struct_name {
                                // Set up type parameter scope for generic impl blocks
                                self.type_checker.env().push_type_param_scope();
                                for generic_arg in generic_params {
                                    let param_name = match &generic_arg.name {
                                        Some(name) => name.clone(),
                                        None => {
                                            if let veld_common::ast::TypeAnnotation::Basic(
                                                base_name,
                                            ) = &generic_arg.type_annotation
                                            {
                                                base_name.clone()
                                            } else {
                                                "T".to_string()
                                            }
                                        }
                                    };
                                    self.type_checker.env().add_type_param(&param_name);
                                }

                                for method in methods {
                                    // Set up method-level type parameters
                                    self.type_checker.env().push_type_param_scope();
                                    for generic_arg in &method.generic_params {
                                        let param_name = match &generic_arg.name {
                                            Some(name) => name.clone(),
                                            None => {
                                                if let veld_common::ast::TypeAnnotation::Basic(
                                                    base_name,
                                                ) = &generic_arg.type_annotation
                                                {
                                                    base_name.clone()
                                                } else {
                                                    "U".to_string()
                                                }
                                            }
                                        };
                                        self.type_checker.env().add_type_param(&param_name);
                                    }

                                    let param_types: Vec<veld_common::types::Type> = method
                                        .params
                                        .iter()
                                        .map(|(_, type_annotation)| {
                                            match self.type_checker
                                                .env()
                                                .from_annotation(type_annotation, None)
                                            {
                                                Ok(ty) => ty,
                                                Err(e) => {
                                                    tracing::error!(
                                                        "Failed to resolve parameter type annotation {:?} for method {}.{}: {}",
                                                        type_annotation,
                                                        qualified_name,
                                                        method.name,
                                                        e
                                                    );
                                                    veld_common::types::Type::Any
                                                }
                                            }
                                        })
                                        .collect();
                                    let return_type = match self
                                        .type_checker
                                        .env()
                                        .from_annotation(&method.return_type, None)
                                    {
                                        Ok(ty) => ty,
                                        Err(e) => {
                                            tracing::error!(
                                                "Failed to resolve return type annotation {:?} for method {}.{}: {}",
                                                method.return_type,
                                                qualified_name,
                                                method.name,
                                                e
                                            );
                                            veld_common::types::Type::Any
                                        }
                                    };
                                    tracing::debug!(
                                        "Method {}.{} return type annotation: {:?} -> converted to: {:?}",
                                        qualified_name,
                                        method.name,
                                        method.return_type,
                                        return_type
                                    );
                                    let mut function_type = veld_common::types::Type::Function {
                                        params: param_types,
                                        return_type: Box::new(return_type),
                                    };

                                    // Instantiate any TypeParam with fresh type variables
                                    fn collect_type_params(
                                        ty: &veld_common::types::Type,
                                        substitutions: &mut std::collections::HashMap<
                                            String,
                                            veld_common::types::Type,
                                        >,
                                        env: &mut veld_common::types::TypeEnvironment,
                                    ) {
                                        match ty {
                                            veld_common::types::Type::TypeParam(name) => {
                                                if !substitutions.contains_key(name) {
                                                    substitutions
                                                        .insert(name.clone(), env.fresh_type_var());
                                                }
                                            }
                                            veld_common::types::Type::Function {
                                                params,
                                                return_type,
                                            } => {
                                                for param in params {
                                                    collect_type_params(param, substitutions, env);
                                                }
                                                collect_type_params(
                                                    return_type,
                                                    substitutions,
                                                    env,
                                                );
                                            }
                                            veld_common::types::Type::Generic {
                                                type_args, ..
                                            } => {
                                                for arg in type_args {
                                                    collect_type_params(arg, substitutions, env);
                                                }
                                            }
                                            veld_common::types::Type::Array(elem) => {
                                                collect_type_params(elem, substitutions, env);
                                            }
                                            veld_common::types::Type::Tuple(types) => {
                                                for t in types {
                                                    collect_type_params(t, substitutions, env);
                                                }
                                            }
                                            _ => {}
                                        }
                                    }

                                    let mut type_param_substitutions =
                                        std::collections::HashMap::new();
                                    collect_type_params(
                                        &function_type,
                                        &mut type_param_substitutions,
                                        self.type_checker.env(),
                                    );

                                    if !type_param_substitutions.is_empty() {
                                        // Merge with shared substitutions from struct-level generics
                                        let mut merged_substitutions =
                                            shared_type_substitutions.clone();
                                        merged_substitutions.extend(type_param_substitutions);

                                        function_type =
                                            self.type_checker.env().substitute_type_params(
                                                &function_type,
                                                &merged_substitutions,
                                            );
                                        tracing::debug!(
                                            "Instantiated type parameters for method {}.{}: {:?} -> {:?}",
                                            qualified_name,
                                            method.name,
                                            merged_substitutions,
                                            function_type
                                        );
                                    } else if has_generics {
                                        // Use only shared substitutions if no method-level generics
                                        function_type =
                                            self.type_checker.env().substitute_type_params(
                                                &function_type,
                                                &shared_type_substitutions,
                                            );
                                        tracing::debug!(
                                            "Used shared type parameters for method {}.{}: {:?} -> {:?}",
                                            qualified_name,
                                            method.name,
                                            shared_type_substitutions,
                                            function_type
                                        );
                                    }

                                    // Register method with qualified struct name
                                    self.type_checker.env().add_struct_method(
                                        &qualified_name,
                                        &method.name,
                                        function_type.clone(),
                                    );
                                    tracing::debug!(
                                        "Registered method {}.{} with type: {:?}",
                                        qualified_name,
                                        method.name,
                                        function_type
                                    );

                                    // Also register with simple name for consistency
                                    self.type_checker.env().add_struct_method(
                                        &struct_name,
                                        &method.name,
                                        function_type.clone(),
                                    );

                                    // Register method in runtime struct_methods for actual execution
                                    if !self.struct_methods.contains_key(&qualified_name) {
                                        self.struct_methods
                                            .insert(qualified_name.clone(), HashMap::new());
                                    }
                                    if let Some(runtime_methods) =
                                        self.struct_methods.get_mut(&qualified_name)
                                    {
                                        // Convert the method to a runtime Value::Function
                                        let runtime_method = Value::Function {
                                            params: method.params.clone(),
                                            body: method.body.clone(),
                                            return_type: method.return_type.clone(),
                                            captured_vars: HashMap::new(),
                                        };
                                        runtime_methods.insert(method.name.clone(), runtime_method);
                                        tracing::debug!(
                                            "Registered runtime method {}.{} for execution",
                                            qualified_name,
                                            method.name
                                        );
                                    }

                                    // Also register with simple name for runtime lookup
                                    if !self.struct_methods.contains_key(struct_name) {
                                        self.struct_methods
                                            .insert(struct_name.clone(), HashMap::new());
                                    }
                                    if let Some(runtime_methods) =
                                        self.struct_methods.get_mut(struct_name)
                                    {
                                        let runtime_method = Value::Function {
                                            params: method.params.clone(),
                                            body: method.body.clone(),
                                            return_type: method.return_type.clone(),
                                            captured_vars: HashMap::new(),
                                        };
                                        runtime_methods.insert(method.name.clone(), runtime_method);
                                    }

                                    // Clean up method-level type parameter scope
                                    self.type_checker.env().pop_type_param_scope();
                                }

                                // Clean up type parameter scope
                                self.type_checker.env().pop_type_param_scope();
                            }
                        }
                    }

                    // Register methods from trait implementations for this struct
                    for trait_impl_stmt in &module.statements {
                        if let Statement::Implementation {
                            type_name: impl_type_name,
                            kind_name,
                            methods: trait_methods,
                            generic_args: trait_generic_args,
                            where_clause: _,
                        } = trait_impl_stmt
                        {
                            tracing::debug!(
                                "Found trait implementation: {} for type {} (looking for {})",
                                kind_name.as_ref().unwrap_or(&"<none>".to_string()),
                                impl_type_name,
                                struct_name
                            );
                            if impl_type_name == struct_name {
                                tracing::debug!(
                                    "Processing trait implementation {} for struct {}",
                                    kind_name.as_ref().unwrap_or(&"<none>".to_string()),
                                    struct_name
                                );
                                // Set up type parameter scope for generic trait impl blocks
                                self.type_checker.env().push_type_param_scope();
                                for generic_arg in trait_generic_args {
                                    let param_name = match &generic_arg.name {
                                        Some(name) => name.clone(),
                                        None => {
                                            if let veld_common::ast::TypeAnnotation::Basic(
                                                base_name,
                                            ) = &generic_arg.type_annotation
                                            {
                                                base_name.clone()
                                            } else {
                                                "T".to_string()
                                            }
                                        }
                                    };
                                    self.type_checker.env().add_type_param(&param_name);
                                }

                                for method in trait_methods {
                                    // Set up method-level type parameters
                                    self.type_checker.env().push_type_param_scope();
                                    for generic_arg in &method.generic_params {
                                        let param_name = match &generic_arg.name {
                                            Some(name) => name.clone(),
                                            None => {
                                                if let veld_common::ast::TypeAnnotation::Basic(
                                                    base_name,
                                                ) = &generic_arg.type_annotation
                                                {
                                                    base_name.clone()
                                                } else {
                                                    "U".to_string()
                                                }
                                            }
                                        };
                                        self.type_checker.env().add_type_param(&param_name);
                                    }

                                    let param_types: Vec<veld_common::types::Type> = method
                                        .params
                                        .iter()
                                        .map(|(_, type_annotation)| {
                                            match self.type_checker
                                                .env()
                                                .from_annotation(type_annotation, None)
                                            {
                                                Ok(ty) => ty,
                                                Err(e) => {
                                                    tracing::error!(
                                                        "Failed to resolve parameter type annotation {:?} for trait method {}.{}: {}",
                                                        type_annotation,
                                                        qualified_name,
                                                        method.name,
                                                        e
                                                    );
                                                    veld_common::types::Type::Any
                                                }
                                            }
                                        })
                                        .collect();
                                    let return_type = match self
                                        .type_checker
                                        .env()
                                        .from_annotation(&method.return_type, None)
                                    {
                                        Ok(ty) => ty,
                                        Err(e) => {
                                            tracing::error!(
                                                "Failed to resolve return type annotation {:?} for trait method {}.{}: {}",
                                                method.return_type,
                                                qualified_name,
                                                method.name,
                                                e
                                            );
                                            veld_common::types::Type::Any
                                        }
                                    };
                                    tracing::debug!(
                                        "Trait method {}.{} return type annotation: {:?} -> converted to: {:?}",
                                        qualified_name,
                                        method.name,
                                        method.return_type,
                                        return_type
                                    );
                                    let mut function_type = veld_common::types::Type::Function {
                                        params: param_types,
                                        return_type: Box::new(return_type),
                                    };

                                    // For trait implementations, keep TypeParam as-is to allow proper substitution during method resolution
                                    tracing::debug!(
                                        "Trait method {}.{} type: {:?} (keeping TypeParam for proper substitution)",
                                        qualified_name,
                                        method.name,
                                        function_type
                                    );

                                    // Register trait method with qualified struct name
                                    self.type_checker.env().add_struct_method(
                                        &qualified_name,
                                        &method.name,
                                        function_type.clone(),
                                    );
                                    tracing::debug!(
                                        "Registered trait method {}.{} with type: {:?}",
                                        qualified_name,
                                        method.name,
                                        function_type
                                    );

                                    // Also register with simple name for consistency
                                    self.type_checker.env().add_struct_method(
                                        &struct_name,
                                        &method.name,
                                        function_type.clone(),
                                    );

                                    // Register trait method in runtime struct_methods for actual execution
                                    if !self.struct_methods.contains_key(&qualified_name) {
                                        self.struct_methods
                                            .insert(qualified_name.clone(), HashMap::new());
                                    }
                                    if let Some(runtime_methods) =
                                        self.struct_methods.get_mut(&qualified_name)
                                    {
                                        // Convert the method to a runtime Value::Function
                                        let runtime_method = Value::Function {
                                            params: method.params.clone(),
                                            body: method.body.clone(),
                                            return_type: method.return_type.clone(),
                                            captured_vars: HashMap::new(),
                                        };
                                        runtime_methods.insert(method.name.clone(), runtime_method);
                                        tracing::debug!(
                                            "Registered runtime trait method {}.{} for execution",
                                            qualified_name,
                                            method.name
                                        );
                                    }

                                    // Also register with simple name for runtime lookup
                                    if !self.struct_methods.contains_key(struct_name) {
                                        self.struct_methods
                                            .insert(struct_name.clone(), HashMap::new());
                                    }
                                    if let Some(runtime_methods) =
                                        self.struct_methods.get_mut(struct_name)
                                    {
                                        let runtime_method = Value::Function {
                                            params: method.params.clone(),
                                            body: method.body.clone(),
                                            return_type: method.return_type.clone(),
                                            captured_vars: HashMap::new(),
                                        };
                                        runtime_methods.insert(method.name.clone(), runtime_method);
                                    }

                                    // Clean up method-level type parameter scope
                                    self.type_checker.env().pop_type_param_scope();
                                }

                                // Clean up type parameter scope
                                self.type_checker.env().pop_type_param_scope();
                            }
                        }
                    }
                }
            }

            // Register enums and their methods from the module
            for statement in &module.statements {
                if let Statement::EnumDeclaration {
                    name: enum_name,
                    variants,
                    ..
                } = statement
                {
                    // Register the enum in the runtime with qualified name
                    let qualified_enum_name = format!("{}.{}", module_name, enum_name);
                    self.enums
                        .insert(qualified_enum_name.clone(), variants.clone());
                    tracing::debug!(
                        "Registered enum '{}' in runtime with {} variants",
                        qualified_enum_name,
                        variants.len()
                    );

                    // Also register with simple name for backwards compatibility
                    self.enums.insert(enum_name.clone(), variants.clone());
                    tracing::debug!(
                        "Registered enum '{}' in runtime with simple name",
                        enum_name
                    );

                    // Register the enum in the type environment for type checking
                    fn convert_ast_enum_variant_to_base(
                        ast_variant: &veld_common::ast::EnumVariant,
                    ) -> veld_common::types::EnumVariant {
                        match &ast_variant.fields {
                            None => veld_common::types::EnumVariant::Simple,
                            Some(fields) => {
                                let tuple_types: Vec<veld_common::types::Type> = fields
                                    .iter()
                                    .map(|anno| {
                                        veld_common::types::Type::from_annotation(anno, None)
                                            .unwrap_or(veld_common::types::Type::Any)
                                    })
                                    .collect();
                                veld_common::types::EnumVariant::Tuple(tuple_types)
                            }
                        }
                    }
                    let variant_map: std::collections::HashMap<
                        String,
                        veld_common::types::EnumVariant,
                    > = variants
                        .iter()
                        .map(|v| (v.name.clone(), convert_ast_enum_variant_to_base(v)))
                        .collect();
                    self.type_checker.env().add_enum(enum_name, variant_map);

                    // Also register enum methods from implementation blocks
                    for impl_stmt in &module.statements {
                        if let Statement::InherentImpl {
                            type_name,
                            methods,
                            generic_params,
                            ..
                        } = impl_stmt
                        {
                            if type_name == enum_name {
                                // Set up type parameter scope for generic impl blocks
                                self.type_checker.env().push_type_param_scope();
                                for generic_arg in generic_params {
                                    let param_name = match &generic_arg.name {
                                        Some(name) => name.clone(),
                                        None => {
                                            if let veld_common::ast::TypeAnnotation::Basic(
                                                base_name,
                                            ) = &generic_arg.type_annotation
                                            {
                                                base_name.clone()
                                            } else {
                                                "T".to_string()
                                            }
                                        }
                                    };
                                    self.type_checker.env().add_type_param(&param_name);
                                }

                                for method in methods {
                                    // Set up method-level type parameters
                                    self.type_checker.env().push_type_param_scope();
                                    for generic_arg in &method.generic_params {
                                        let param_name = match &generic_arg.name {
                                            Some(name) => name.clone(),
                                            None => {
                                                if let veld_common::ast::TypeAnnotation::Basic(
                                                    base_name,
                                                ) = &generic_arg.type_annotation
                                                {
                                                    base_name.clone()
                                                } else {
                                                    "U".to_string()
                                                }
                                            }
                                        };
                                        self.type_checker.env().add_type_param(&param_name);
                                    }

                                    let param_types: Vec<veld_common::types::Type> = method
                                        .params
                                        .iter()
                                        .map(|(_, type_annotation)| {
                                            self.type_checker
                                                .env()
                                                .from_annotation(type_annotation, None)
                                                .unwrap_or(veld_common::types::Type::Any)
                                        })
                                        .collect();
                                    let return_type = self
                                        .type_checker
                                        .env()
                                        .from_annotation(&method.return_type, None)
                                        .unwrap_or(veld_common::types::Type::Any);
                                    let function_type = veld_common::types::Type::Function {
                                        params: param_types,
                                        return_type: Box::new(return_type),
                                    };
                                    self.type_checker.env().add_enum_method(
                                        type_name,
                                        &method.name,
                                        function_type,
                                    );

                                    // Clean up method-level type parameter scope
                                    self.type_checker.env().pop_type_param_scope();
                                }

                                // Clean up type parameter scope
                                self.type_checker.env().pop_type_param_scope();

                                // Also register enum methods in runtime for actual execution
                                for method in methods {
                                    // Register enum method in runtime enum_methods
                                    if !self.enum_methods.contains_key(enum_name) {
                                        self.enum_methods.insert(enum_name.clone(), HashMap::new());
                                    }
                                    if let Some(runtime_methods) =
                                        self.enum_methods.get_mut(enum_name)
                                    {
                                        runtime_methods.insert(method.name.clone(), method.clone());
                                        tracing::debug!(
                                            "Registered runtime enum method {}.{} for execution",
                                            enum_name,
                                            method.name
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn initialize_operator_kinds(&mut self) -> Result<()> {
        let ordering_variants = vec![
            EnumVariant {
                name: "Less".to_string(),
                fields: None,
                methods: HashMap::new(),
            },
            EnumVariant {
                name: "Equal".to_string(),
                fields: None,
                methods: HashMap::new(),
            },
            EnumVariant {
                name: "Greater".to_string(),
                fields: None,
                methods: HashMap::new(),
            },
        ];

        self.enums.insert("Ordering".to_string(), ordering_variants);

        let mut add_kind_methods = HashMap::new();
        use veld_common::types::Type;

        let add_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };

        add_kind_methods.insert("add".to_string(), add_method_type);

        self.type_checker.env().add_kind(
            "Add",
            add_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        let mut sub_kind_methods = HashMap::new();
        let sub_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };
        sub_kind_methods.insert("sub".to_string(), sub_method_type);
        self.type_checker.env().add_kind(
            "Sub",
            sub_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        let mut mul_kind_methods = HashMap::new();
        let mul_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };
        mul_kind_methods.insert("mul".to_string(), mul_method_type);
        self.type_checker.env().add_kind(
            "Mul",
            mul_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        let mut div_kind_methods = HashMap::new();
        let div_method_type = Type::Function {
            params: vec![
                Type::TypeParam("Self".to_string()),
                Type::TypeParam("Rhs".to_string()),
            ],
            return_type: Box::new(Type::TypeParam("Output".to_string())),
        };
        div_kind_methods.insert("div".to_string(), div_method_type);
        self.type_checker.env().add_kind(
            "Div",
            div_kind_methods,
            HashMap::new(),
            vec!["Self".to_string(), "Rhs".to_string(), "Output".to_string()],
        );

        Ok(())
    }

    fn try_call_operator_method(
        &mut self,
        left: &Value,
        op: &BinaryOperator,
        right: &Value,
    ) -> Result<Option<Value>> {
        let method_name = match op {
            BinaryOperator::Add => "add",
            BinaryOperator::Subtract => "sub",
            BinaryOperator::Multiply => "mul",
            BinaryOperator::Divide => "div",
            BinaryOperator::Modulo => "rem",
            _ => return Ok(None), // Not an operator we can handle
        };

        if let Value::Struct {
            name: struct_name, ..
        } = left
        {
            if let Some(methods) = self.struct_methods.get(struct_name) {
                if let Some(_) = methods.get(method_name) {
                    let args = vec![right.clone()];
                    let result =
                        self.call_method_value(left.clone(), method_name.to_string(), args)?;
                    return Ok(Some(result));
                }
            }
        }
        Ok(None) // No operator method found
    }
}

fn match_num_val(iv: &IntegerValue) -> Option<usize> {
    match iv {
        IntegerValue::I8(v) => Some(*v as usize),
        IntegerValue::I16(v) => Some(*v as usize),
        IntegerValue::I32(v) => Some(*v as usize),
        IntegerValue::I64(v) => Some(*v as usize),
        IntegerValue::U8(v) => Some(*v as usize),
        IntegerValue::U16(v) => Some(*v as usize),
        IntegerValue::U32(v) => Some(*v as usize),
        IntegerValue::U64(v) => Some(*v as usize),
    }
}

fn evaluate_literal_expression(lit: Literal) -> Result<Value> {
    Ok(match lit {
        Literal::Integer(n) => {
            // Default to i32, but this could be context-sensitive in the future
            let int_val = if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                IntegerValue::I32(n as i32)
            } else {
                IntegerValue::I64(n)
            };
            Value::Numeric(NumericValue::Integer(int_val))
        }
        Literal::Float(n) => {
            // Default to f64, but could be context-sensitive in the future
            Value::Float(n)
        }
        Literal::String(s) => Value::String(s),
        Literal::Boolean(b) => Value::Boolean(b),
        Literal::Char(c) => Value::Char(c),
        Literal::Unit => Value::Unit,
    })
}
