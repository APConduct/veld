use crate::ast::{ImportItem, Statement};
use crate::error::{Result as VeldResult, VeldError};
use crate::lexer::Lexer;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub statements: Vec<Statement>,
    pub exports: HashMap<String, ExportedItem>,
    pub path: Option<PathBuf>,
}

impl Module {
    pub fn add_export(&mut self, name: String, item: ExportedItem) {
        self.exports.insert(name, item);
    }
}

#[derive(Debug, Clone)]
pub enum ExportedItem {
    Function(usize),
    Struct(usize),
    Variable(usize),
    Kind(usize),
    Enum(usize),
    Module(String),
}

pub struct ModuleManager {
    root_dir: PathBuf,
    modules: HashMap<String, Module>,
    module_search_paths: Vec<PathBuf>,
}

impl ModuleManager {
    pub fn new<P: AsRef<Path>>(root_dir: P) -> Self {
        let root = root_dir.as_ref().to_path_buf();

        let mut search_paths = vec![root.clone()]; // Current directory as the first search path

        // Add various potential stdlib directories to search paths
        let potential_stdlib_dirs = [
            root.join("stdlib"),
            PathBuf::from("./stdlib"),
            PathBuf::from("./veld/stdlib"),
            PathBuf::from("../stdlib"),
            PathBuf::from("../veld/stdlib"),
            PathBuf::from("../../veld/stdlib"),
        ];

        for path in &potential_stdlib_dirs {
            if path.exists() && path.is_dir() {
                search_paths.push(path.clone());
            }
        }

        Self {
            root_dir: root,
            modules: HashMap::new(),
            module_search_paths: search_paths,
        }
    }

    pub fn register_stdlib_path(&mut self, path: &std::path::Path) -> VeldResult<()> {
        if !path.exists() || !path.is_dir() {
            return Err(VeldError::ModuleError(format!(
                "Invalid stdlib path: {:?}",
                path
            )));
        }

        // Add the stdlib path to search paths with highest priority
        self.module_search_paths.insert(0, path.to_path_buf());

        // Pre-load core modules for better performance
        let core_modules = ["std", "std.ops", "std.math", "std.io", "std.collections"];
        for module_name in core_modules.iter() {
            let parts: Vec<&str> = module_name.split('.').collect();
            let module_parts: Vec<String> = parts.iter().map(|s| s.to_string()).collect();
            match self.load_module(&module_parts) {
                Ok(_) => (),
                Err(e) => tracing::error!("Failed to load module '{}': {}", module_name, e),
            }
        }

        Ok(())
    }

    pub fn add_search_path<P: AsRef<Path>>(&mut self, path: P) {
        let path_buf = path.as_ref().to_path_buf();
        if path_buf.exists() && !self.module_search_paths.contains(&path_buf) {
            self.module_search_paths.push(path_buf);
        }
    }

    pub fn load_module(&mut self, module_path: &[String]) -> Result<&Module, VeldError> {
        let full_name = module_path.join(".");

        if self.modules.contains_key(&full_name) {
            return Ok(&self.modules[&full_name]);
        }

        let mod_file = self.find_module_file(module_path)?;

        let src = fs::read_to_string(&mod_file)
            .map_err(|e| VeldError::RuntimeError(format!("Failed to read module file: {}", e)))?;

        let mut lexer = Lexer::new(&src);
        let tokens = lexer
            .collect_tokens()
            .map_err(|e| VeldError::LexerError(e))?;

        let mut parser = crate::parser::Parser::new(tokens);
        let statements = parser.parse()?;

        // Extract exports
        let exports = self.extract_exports(&statements);

        // Create and register the module
        let modules = Module {
            name: full_name.clone(),
            statements,
            exports,
            path: Some(mod_file),
        };

        self.modules.insert(full_name.clone(), modules);

        Ok(&self.modules[&full_name])
    }

    // fn resolve_module_path(&self, parts: &[String]) -> Option<PathBuf> {
    //     // Special case for std module and its submodules
    //     if parts.first().map_or(false, |p| p == "std") {
    //         // Skip the "std" part since it maps to the stdlib root
    //         let mut rel_path = PathBuf::new();
    //         for part in parts.iter().skip(1) {
    //             rel_path.push(part);
    //         }

    //         // First, try the directly available stdlib directory
    //         let stdlib_dir = self.root_dir.join("stdlib");
    //         if stdlib_dir.exists() {
    //             let module_path = if rel_path.as_os_str().is_empty() {
    //                 // Just "std" - use the root mod.veld
    //                 stdlib_dir.join("mod.veld")
    //             } else {
    //                 // std.submodule - look in the corresponding directory
    //                 stdlib_dir.join(&rel_path).join("mod.veld")
    //             };

    //             if module_path.exists() {
    //                 return Some(module_path);
    //             }
    //         }

    //         // Then check search paths for stdlib directories
    //         for search_path in &self.module_search_paths {
    //             let stdlib_dir = search_path.join("stdlib");
    //             if stdlib_dir.exists() {
    //                 let module_path = if rel_path.as_os_str().is_empty() {
    //                     // Just "std" - use the root mod.veld
    //                     stdlib_dir.join("mod.veld")
    //                 } else {
    //                     // std.submodule - look in the corresponding directory
    //                     stdlib_dir.join(&rel_path).join("mod.veld")
    //                 };

    //                 if module_path.exists() {
    //                     return Some(module_path);
    //                 }
    //             }
    //         }
    //     }

    //     // For non-std modules or if std module wasn't found
    //     for search_path in &self.module_search_paths {
    //         let mut module_path = search_path.clone();
    //         for part in parts {
    //             module_path.push(part);
    //         }

    //         // Try with .veld extension
    //         let with_ext = module_path.with_extension("veld");
    //         if with_ext.exists() {
    //             return Some(with_ext);
    //         }

    //         // Try with /mod.veld
    //         let with_mod = module_path.join("mod.veld");
    //         if with_mod.exists() {
    //             return Some(with_mod);
    //         }
    //     }

    //     None
    // }

    pub fn create_module(
        &mut self,
        name: &str,
        statements: Vec<Statement>,
    ) -> Result<&Module, VeldError> {
        let exports = self.extract_exports(&statements);

        let module = Module {
            name: name.to_string(),
            statements,
            exports,
            path: None,
        };

        self.modules.insert(name.to_string(), module);
        Ok(&self.modules[name])
    }

    fn find_module_file(&self, module_path: &[String]) -> Result<PathBuf, VeldError> {
        tracing::debug!("Searching for module: {:?}", module_path);
        // use dbug in
        tracing::debug!("Module search paths: {:?}", self.module_search_paths);

        // Special case for std module and its submodules
        if let Some(first) = module_path.first() {
            if first == "std" {
                // Try multiple potential locations for the stdlib directory
                let potential_stdlib_dirs = [
                    self.root_dir.join("stdlib"),
                    PathBuf::from("./stdlib"),
                    PathBuf::from("./veld/stdlib"),
                    PathBuf::from("../stdlib"),
                    PathBuf::from("../veld/stdlib"),
                    PathBuf::from("../../veld/stdlib"),
                ];

                tracing::debug!(
                    "Checking potential stdlib locations for module: {:?}",
                    module_path
                );

                for stdlib_dir in &potential_stdlib_dirs {
                    tracing::debug!(
                        "  Checking: {:?} exists: {}",
                        stdlib_dir,
                        stdlib_dir.exists()
                    );

                    if stdlib_dir.exists() {
                        if module_path.len() == 1 {
                            // Just "std" - map to stdlib/mod.veld
                            let mod_file = stdlib_dir.join("mod.veld");
                            tracing::debug!(
                                "  Checking mod file: {:?} exists: {}",
                                mod_file,
                                mod_file.exists()
                            );
                            if mod_file.exists() {
                                tracing::info!("  Found mod.veld at: {:?}", mod_file);
                                return Ok(mod_file);
                            }

                            let init_file = stdlib_dir.join("init.veld");
                            tracing::debug!(
                                "  Checking init file: {:?} exists: {}",
                                init_file,
                                init_file.exists()
                            );
                            if init_file.exists() {
                                tracing::info!("  Found init.veld at: {:?}", init_file);
                                return Ok(init_file);
                            }
                        } else {
                            // For submodules like std.ops, std.collections, etc.
                            let mut submodule_path = PathBuf::new();
                            for part in &module_path[1..] {
                                submodule_path.push(part);
                            }

                            // Try as a direct .veld file
                            let veld_file = stdlib_dir.join(&submodule_path).with_extension("veld");
                            tracing::debug!(
                                "  Checking direct file: {:?} exists: {}",
                                veld_file,
                                veld_file.exists()
                            );
                            if veld_file.exists() {
                                tracing::info!("  Found direct .veld file at: {:?}", veld_file);
                                return Ok(veld_file);
                            }

                            // Try as a directory with mod.veld
                            let mod_file = stdlib_dir.join(&submodule_path).join("mod.veld");
                            tracing::debug!(
                                "  Checking mod file: {:?} exists: {}",
                                mod_file,
                                mod_file.exists()
                            );
                            if mod_file.exists() {
                                tracing::info!("  Found mod.veld at: {:?}", mod_file);
                                return Ok(mod_file);
                            }

                            // Try as a directory with init.veld
                            let init_file = stdlib_dir.join(&submodule_path).join("init.veld");
                            tracing::debug!(
                                "  Checking init file: {:?} exists: {}",
                                init_file,
                                init_file.exists()
                            );
                            if init_file.exists() {
                                tracing::info!("  Found init.veld at: {:?}", init_file);
                                return Ok(init_file);
                            }
                        }
                    }
                }
            }
        }

        // Standard search method for all modules
        for search_path in &self.module_search_paths {
            let mut file_path = search_path.clone();
            for part in module_path {
                file_path.push(part);
            }

            // Try with .veld extension first (direct file)
            let veld_path = file_path.with_extension("veld");
            tracing::debug!(
                "  Checking file: {:?} exists: {}",
                veld_path,
                veld_path.exists()
            );
            if veld_path.exists() {
                tracing::info!("  Found direct .veld file at: {:?}", veld_path);
                return Ok(veld_path);
            }

            // Try with /mod.veld
            let mod_path = file_path.join("mod.veld");
            tracing::debug!(
                "  Checking mod file: {:?} exists: {}",
                mod_path,
                mod_path.exists()
            );
            if mod_path.exists() {
                tracing::info!("  Found mod.veld at: {:?}", mod_path);
                return Ok(mod_path);
            }

            // Try with /init.veld
            let init_path = file_path.join("init.veld");
            tracing::debug!(
                "  Checking init file: {:?} exists: {}",
                init_path,
                init_path.exists()
            );
            if init_path.exists() {
                tracing::info!("  Found init.veld at: {:?}", init_path);
                return Ok(init_path);
            }
        }

        tracing::debug!("Module not found: {}", module_path.join("."));
        Err(VeldError::RuntimeError(format!(
            "Module not found: {}",
            module_path.join(".")
        )))
    }

    fn extract_exports(&self, statements: &[Statement]) -> HashMap<String, ExportedItem> {
        let mut exports = HashMap::new();

        for (i, stmt) in statements.iter().enumerate() {
            match stmt {
                Statement::FunctionDeclaration {
                    name, is_public, ..
                } => {
                    // Only export public functions
                    if *is_public {
                        exports.insert(name.clone(), ExportedItem::Function(i));
                    }
                }

                Statement::StructDeclaration {
                    name, is_public, ..
                } => {
                    // Only export public structs
                    if *is_public {
                        exports.insert(name.clone(), ExportedItem::Struct(i));
                    }
                }

                Statement::KindDeclaration {
                    name, is_public, ..
                } => {
                    // Only export public kinds
                    if *is_public {
                        exports.insert(name.clone(), ExportedItem::Kind(i));
                    }
                }

                Statement::VariableDeclaration { name, .. } => {
                    if let Some(is_public) = self.is_public_variable(stmt) {
                        // Only export public enums
                        if is_public {
                            exports.insert(name.clone(), ExportedItem::Variable(i));
                        }
                    }
                }

                Statement::EnumDeclaration {
                    name, is_public, ..
                } => {
                    // Only export public enums
                    if *is_public {
                        exports.insert(name.clone(), ExportedItem::Variable(i));
                    }
                }

                Statement::ImportDeclaration {
                    path,
                    items,
                    alias,
                    is_public,
                } => {
                    // Only proccess public imports (re-exports)
                    if *is_public {
                        // Get the source module path as string
                        let source_module_path = path.join(".");

                        // If this module hasn't been loaded yet, we can't re-export from it
                        if !self.is_module_loaded(&source_module_path) {
                            continue; // Skip if module is not loaded
                        }

                        // Get the source module to re-export from
                        let source_module = match self.get_module(&source_module_path) {
                            Some(module) => module,
                            None => continue, // Skip if module is not found
                        };

                        // Process different import types
                        match items.as_slice() {
                            [] if alias.is_some() => {
                                let module_alias = alias.as_ref().unwrap();
                                exports.insert(
                                    module_alias.clone(),
                                    ExportedItem::Module(source_module_path.clone()),
                                );
                            }
                            [] => {
                                for (name, item) in &source_module.exports {
                                    exports.insert(name.clone(), item.clone());
                                }
                            }
                            _ => {
                                for item in items {
                                    match item {
                                        ImportItem::All => {
                                            for (name, item) in &source_module.exports {
                                                exports.insert(name.clone(), item.clone());
                                            }
                                        }
                                        ImportItem::Named(name) => {
                                            if let Some(item) = source_module.exports.get(name) {
                                                exports.insert(name.clone(), item.clone());
                                            }
                                        }
                                        ImportItem::NamedWithAlias { name, alias } => {
                                            if let Some(item) = source_module.exports.get(name) {
                                                exports.insert(alias.clone(), item.clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                _ => {}
            }
        }
        exports
    }

    fn is_public_variable(&self, stmt: &Statement) -> Option<bool> {
        match stmt {
            Statement::VariableDeclaration { is_public, .. } => Some(*is_public),
            _ => None,
        }
    }

    pub fn get_nested_module(&self, path: &[String]) -> Option<&Module> {
        if path.len() == 1 {
            return self.get_module(&path[0]);
        }

        // For nested paths, traverse the module hierarchy
        let mut current = self.get_module(&path[0])?;

        for i in 1..path.len() {
            let submodule_name = format!("{}.{}", current.name, &path[i]);
            current = self.get_module(&submodule_name)?;
        }

        Some(current)
    }

    /// Get a module by its name
    pub fn get_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }

    pub fn get_module_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.modules.get_mut(name)
    }

    pub fn is_module_loaded(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    pub fn get_exports(
        &self,
        module_name: &str,
        items: &[ImportItem],
    ) -> Result<HashMap<String, ExportedItem>, VeldError> {
        let module = self.get_module(module_name).ok_or_else(|| {
            VeldError::RuntimeError(format!("Module '{}' not found", module_name))
        })?;

        let mut result = HashMap::new();

        if items.is_empty() {
            // Return whole module as is
            return Ok(module.exports.clone());
        }

        // Process selective imports
        for item in items {
            match item {
                ImportItem::All => {
                    // Import item
                    return Ok(module.exports.clone());
                }
                ImportItem::Named(name) => {
                    // Import specific item
                    if let Some(export) = module.exports.get(name) {
                        result.insert(name.clone(), export.clone());
                    } else {
                        return Err(VeldError::RuntimeError(format!(
                            "Export '{}' not found in module '{}'",
                            name, module_name
                        )));
                    }
                }
                ImportItem::NamedWithAlias { name, alias } => {
                    // Import with alias
                    if let Some(export) = module.exports.get(name) {
                        result.insert(alias.clone(), export.clone());
                    } else {
                        return Err(VeldError::RuntimeError(format!(
                            "Export '{}' not found in module {}",
                            name, module_name
                        )));
                    }
                }
            }
        }
        Ok(result)
    }
}
