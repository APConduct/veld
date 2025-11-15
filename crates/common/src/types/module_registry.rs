use crate::ast::{Statement, TypeAnnotation, VarKind};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::types::Type;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use veld_error::{Result, VeldError};

/// Registry of all loaded modules and their exported functions/types
#[derive(Debug, Clone)]
pub struct ModuleRegistry {
    /// Map of module path to Module
    modules: HashMap<String, Module>,
    /// Map of aliases to full module paths (e.g., "io" -> "std.io")
    aliases: HashMap<String, String>,
    /// Stdlib directory path
    stdlib_path: PathBuf,
}

/// Represents a loaded module with its exports
#[derive(Debug, Clone)]
pub struct Module {
    /// Full module path (e.g., "std.io")
    pub path: String,
    /// Exported functions
    pub functions: HashMap<String, FunctionSignature>,
    /// Exported types (structs, enums, type aliases)
    pub types: HashMap<String, Type>,
    /// Exported constants
    pub constants: HashMap<String, Type>,
}

/// Function signature extracted from module
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub is_public: bool,
    pub doc_comment: Option<String>,
}

impl ModuleRegistry {
    /// Create a new empty module registry
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            aliases: HashMap::new(),
            stdlib_path: PathBuf::from("./stdlib"),
        }
    }

    /// Create a registry with custom stdlib path
    pub fn with_stdlib_path<P: Into<PathBuf>>(stdlib_path: P) -> Self {
        Self {
            modules: HashMap::new(),
            aliases: HashMap::new(),
            stdlib_path: stdlib_path.into(),
        }
    }

    /// Load all standard library modules
    pub fn load_stdlib(&mut self) -> Result<()> {
        tracing::info!("Loading standard library from {:?}", self.stdlib_path);

        // Core stdlib modules to load
        let stdlib_modules = vec![
            "io",
            "option",
            "result",
            "collections/sequence",
            "collections/map",
            "collections/hash_map",
            "vec",
        ];

        for module_name in stdlib_modules {
            let module_path = format!("std.{}", module_name.replace('/', "."));
            match self.load_module(&module_path) {
                Ok(_) => {
                    tracing::debug!("Loaded stdlib module: {}", module_path);
                }
                Err(e) => {
                    // Log error but continue - some stdlib modules might not exist yet
                    tracing::warn!("Failed to load stdlib module {}: {}", module_path, e);
                }
            }
        }

        Ok(())
    }

    /// Load a specific module by path
    pub fn load_module(&mut self, module_path: &str) -> Result<Module> {
        tracing::debug!("Loading module: {}", module_path);

        // Convert module path to file path
        // e.g., "std.io" -> "./stdlib/io.veld"
        let file_path = self.module_path_to_file_path(module_path)?;

        // Read and parse the module file
        let source = fs::read_to_string(&file_path).map_err(|e| {
            VeldError::RuntimeError(format!("Failed to read module file {:?}: {}", file_path, e))
        })?;

        let module = self.parse_module(module_path, &source)?;

        // Register the module
        self.modules.insert(module_path.to_string(), module.clone());

        Ok(module)
    }

    /// Register an import alias
    pub fn register_alias(&mut self, alias: String, module_path: String) {
        tracing::debug!("Registering alias: {} -> {}", alias, module_path);
        self.aliases.insert(alias, module_path);
    }

    /// Look up a function in a module
    pub fn lookup_function(
        &self,
        module_path: &str,
        function_name: &str,
    ) -> Option<&FunctionSignature> {
        // First, try direct lookup
        if let Some(module) = self.modules.get(module_path) {
            if let Some(func) = module.functions.get(function_name) {
                tracing::debug!(
                    "Found function {}.{} via direct lookup",
                    module_path,
                    function_name
                );
                return Some(func);
            }
        }

        // Try resolving alias
        if let Some(resolved_path) = self.aliases.get(module_path) {
            if let Some(module) = self.modules.get(resolved_path) {
                if let Some(func) = module.functions.get(function_name) {
                    tracing::debug!(
                        "Found function {}.{} via alias {} -> {}",
                        function_name,
                        module_path,
                        module_path,
                        resolved_path
                    );
                    return Some(func);
                }
            }
        }

        // Try partial path match (e.g., "io" matches "std.io")
        for (path, module) in &self.modules {
            if path.ends_with(&format!(".{}", module_path)) || path == module_path {
                if let Some(func) = module.functions.get(function_name) {
                    tracing::debug!(
                        "Found function {}.{} via partial match in {}",
                        module_path,
                        function_name,
                        path
                    );
                    return Some(func);
                }
            }
        }

        tracing::debug!(
            "Function {}.{} not found in registry",
            module_path,
            function_name
        );
        None
    }

    /// Get a module by path
    pub fn get_module(&self, module_path: &str) -> Option<&Module> {
        // Try direct lookup
        if let Some(module) = self.modules.get(module_path) {
            return Some(module);
        }

        // Try alias resolution
        if let Some(resolved_path) = self.aliases.get(module_path) {
            return self.modules.get(resolved_path);
        }

        None
    }

    /// Check if a module is loaded
    pub fn has_module(&self, module_path: &str) -> bool {
        self.modules.contains_key(module_path) || self.aliases.contains_key(module_path)
    }

    /// Get all loaded modules
    pub fn modules(&self) -> &HashMap<String, Module> {
        &self.modules
    }

    /// Convert module path to file path
    fn module_path_to_file_path(&self, module_path: &str) -> Result<PathBuf> {
        // Strip "std." prefix if present
        let relative_path = if let Some(stripped) = module_path.strip_prefix("std.") {
            stripped
        } else {
            module_path
        };

        // Convert dots to slashes
        let file_path = relative_path.replace('.', "/");

        // Try two possible locations:
        // 1. file.veld (e.g., stdlib/io.veld)
        let direct_path = self.stdlib_path.join(format!("{}.veld", file_path));
        if direct_path.exists() {
            return Ok(direct_path);
        }

        // 2. file/mod.veld (e.g., stdlib/io/mod.veld)
        let mod_path = self.stdlib_path.join(file_path).join("mod.veld");
        if mod_path.exists() {
            return Ok(mod_path);
        }

        // If neither exists, return the direct path (will fail on read, but with a clear error)
        Ok(direct_path)
    }

    /// Parse a module from source code
    fn parse_module(&self, module_path: &str, source: &str) -> Result<Module> {
        // Lex and parse
        let mut lexer = Lexer::new(source);
        let tokens = lexer
            .collect_tokens()
            .map_err(|e| VeldError::LexerError(e))?;

        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;

        // Extract exports
        self.extract_module_exports(module_path, &statements)
    }

    /// Extract exported functions, types, and constants from AST
    fn extract_module_exports(
        &self,
        module_path: &str,
        statements: &[Statement],
    ) -> Result<Module> {
        let mut functions = HashMap::new();
        let mut types = HashMap::new();
        let mut constants = HashMap::new();

        for stmt in statements {
            match stmt {
                Statement::FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    is_public,
                    ..
                } => {
                    if *is_public {
                        let param_types: Vec<(String, Type)> = params
                            .iter()
                            .map(|(param_name, type_ann)| {
                                (param_name.clone(), self.type_annotation_to_type(type_ann))
                            })
                            .collect();

                        let ret_type = self.type_annotation_to_type(return_type);

                        functions.insert(
                            name.clone(),
                            FunctionSignature {
                                name: name.clone(),
                                params: param_types,
                                return_type: ret_type,
                                is_public: true,
                                doc_comment: None, // TODO: Extract from comments
                            },
                        );

                        tracing::debug!(
                            "Registered function: {}.{} with {} params -> {:?}",
                            module_path,
                            name,
                            params.len(),
                            self.type_annotation_to_type(return_type)
                        );
                    }
                }
                Statement::StructDeclaration {
                    name, is_public, ..
                } => {
                    if *is_public {
                        types.insert(name.clone(), Type::StructType(name.clone()));
                    }
                }
                Statement::EnumDeclaration {
                    name, is_public, ..
                } => {
                    if *is_public {
                        types.insert(name.clone(), Type::EnumType(name.clone()));
                    }
                }
                Statement::TypeDeclaration {
                    name,
                    type_annotation,
                    ..
                } => {
                    types.insert(name.clone(), self.type_annotation_to_type(type_annotation));
                }
                Statement::VariableDeclaration {
                    pattern,
                    var_kind,
                    type_annotation,
                    ..
                } => {
                    // Only register if it's a const and public (has type annotation)
                    if matches!(var_kind, VarKind::Const) {
                        if let Some(type_ann) = type_annotation {
                            // For now, only handle simple identifier patterns in consts
                            if let crate::ast::Pattern::Identifier(name) = pattern {
                                constants
                                    .insert(name.clone(), self.type_annotation_to_type(type_ann));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(Module {
            path: module_path.to_string(),
            functions,
            types,
            constants,
        })
    }

    /// Convert TypeAnnotation to Type
    fn type_annotation_to_type(&self, annotation: &TypeAnnotation) -> Type {
        match annotation {
            TypeAnnotation::Basic(name) => match name.as_str() {
                "i32" => Type::I32,
                "i64" => Type::I64,
                "i8" => Type::I8,
                "i16" => Type::I16,
                "u32" => Type::U32,
                "u64" => Type::U64,
                "u8" => Type::U8,
                "u16" => Type::U16,
                "f32" => Type::F32,
                "f64" => Type::F64,
                "bool" => Type::Bool,
                "str" | "String" => Type::String,
                "char" => Type::Char,
                "()" => Type::Unit,
                _ => Type::TypeParam(name.clone()),
            },
            TypeAnnotation::Generic { base, type_args } => Type::Generic {
                base: base.clone(),
                type_args: type_args
                    .iter()
                    .map(|a| self.type_annotation_to_type(a))
                    .collect(),
            },
            TypeAnnotation::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|p| self.type_annotation_to_type(p))
                    .collect(),
                return_type: Box::new(self.type_annotation_to_type(return_type)),
            },
            TypeAnnotation::Array(elem) => {
                Type::Array(Box::new(self.type_annotation_to_type(elem)))
            }
            TypeAnnotation::Tuple(types) => Type::Tuple(
                types
                    .iter()
                    .map(|t| self.type_annotation_to_type(t))
                    .collect(),
            ),
            TypeAnnotation::Constrained { base_type, .. } => {
                // For constrained types, just use the base type for now
                self.type_annotation_to_type(base_type)
            }
            TypeAnnotation::Union { variants } => Type::Union {
                variants: variants
                    .iter()
                    .map(|v| self.type_annotation_to_type(v))
                    .collect(),
            },
            TypeAnnotation::Record { fields } => Type::Record {
                fields: fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.type_annotation_to_type(ty)))
                    .collect(),
            },
            TypeAnnotation::Enum { name, .. } => Type::EnumType(name.clone()),
            TypeAnnotation::Self_ => Type::Any, // Self type context-dependent
            TypeAnnotation::Unit => Type::Unit,
        }
    }
}

impl Default for ModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_registry_creation() {
        let registry = ModuleRegistry::new();
        assert_eq!(registry.modules.len(), 0);
    }

    #[test]
    fn test_alias_registration() {
        let mut registry = ModuleRegistry::new();
        registry.register_alias("io".to_string(), "std.io".to_string());
        assert_eq!(registry.aliases.get("io"), Some(&"std.io".to_string()));
    }

    #[test]
    fn test_type_annotation_conversion() {
        let registry = ModuleRegistry::new();
        let ann = TypeAnnotation::Basic("i32".to_string());
        let ty = registry.type_annotation_to_type(&ann);
        assert!(matches!(ty, Type::I32));
    }
}
