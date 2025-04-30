use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use crate::Lexer;
use crate::ast::{Statement, ImportItem};
use crate::error::VeldError;

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

pub struct ModuleManager{
    root_dir: PathBuf,
    modules: HashMap<String, Module>,
    module_search_paths: Vec<PathBuf>,
}

impl ModuleManager {
    pub fn new<P: AsRef<Path>>(root_dir: P) -> Self{
        let root = root_dir.as_ref().to_path_buf();
        
        let mut search_paths = vec![root.clone()]; // Current directory as the first search path
        
        let std_path = root.join("std");
        if std_path.exists() {
            search_paths.push(std_path);
        }
        
        Self{
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
        let tokens = lexer.collect_tokens()
            .map_err(|e| VeldError::LexerError(e))?;
        
        let mut parser = crate::parser::Parser::new(tokens);
        let statements = parser.parse()?;
        
        // Extract exports
        let exports = self.extract_exports(&statements);
        
        // Create and register the module
        let modules = Module{
            name: full_name.clone(),
            statements,
            exports,
            path: Some(mod_file),
        };
        
        self.modules.insert(full_name.clone(), modules);
        
        Ok(&self.modules[&full_name])
    }
    
    pub fn create_module(&mut self, name: &str, statements: Vec<Statement>) -> Result<&Module, VeldError> {
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
    
    fn find_module_file(&self, module_path: &[String]) -> Result<PathBuf, VeldError>{
        for search_path in &self.module_search_paths{
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
        Err(VeldError::RuntimeError(format!("Module not found: {}", module_path.join("."))))
    }
    
    fn extract_exports(&self, statements: &[Statement]) -> HashMap<String, ExportedItem>{
        let mut exports = HashMap::new();
        
        for (i, stmt) in statements.iter().enumerate() {
            match stmt {
                Statement::FunctionDeclaration {name, ..} => {
                    // For now, treat all functions as public
                    exports.insert(name.clone(), ExportedItem::Function(i));
                }
                
                Statement::StructDeclaration {name, ..} => {
                    // For now, treat all structs as public
                    exports.insert(name.clone(), ExportedItem::Struct(i));
                }
                
                Statement::KindDeclaration {name, ..} => {
                    // For now, treat all kinds as public
                    exports.insert(name.clone(), ExportedItem::Kind(i));
                }
                
                _ => {}
            }
        }
        exports
    }
    
    pub fn get_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }
    
    pub fn is_module_loaded(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }
    
}