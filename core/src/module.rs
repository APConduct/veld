use crate::ast::{ImportItem, Statement};
use crate::error::VeldError;
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

#[derive(Debug, Clone)]
pub enum ExportedItem {
    Function(usize),
    Struct(usize),
    Variable(usize),
    Kind(usize),
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

        let std_path = root.join("std");
        if std_path.exists() {
            search_paths.push(std_path);
        }

        Self {
            root_dir: root,
            modules: HashMap::new(),
            module_search_paths: search_paths,
        }
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
        for search_path in &self.module_search_paths {
            let mut file_path = search_path.clone();
            for part in module_path {
                file_path.push(part);
            }
            let mut veld_path = file_path.clone();
            veld_path.set_extension("veld");
            if veld_path.exists() {
                return Ok(veld_path);
            }

            let init_path = file_path.join("init.veld");
            if init_path.exists() {
                return Ok(init_path);
            }
        }
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

                Statement::VariableDeclaration {
                    name,
                    var_kind,
                    value,
                    ..
                } => {
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
                    // Handle import declarations
                    todo!(
                        "Handle import declarations: path: {:?}, items: {:?}, alias: {:?}, is_public: {}",
                        path,
                        items,
                        alias,
                        is_public
                    );
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
