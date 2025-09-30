//! Veld Package Manager
//!
//! This crate provides package management functionality for the Veld language.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub dependencies: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageManager {
    pub packages: HashMap<String, Package>,
}

impl PackageManager {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn add_package(&mut self, package: Package) -> Result<()> {
        self.packages.insert(package.name.clone(), package);
        Ok(())
    }

    pub fn get_package(&self, name: &str) -> Option<&Package> {
        self.packages.get(name)
    }
}

impl Default for PackageManager {
    fn default() -> Self {
        Self::new()
    }
}
