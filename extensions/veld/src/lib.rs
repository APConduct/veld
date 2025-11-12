use zed_extension_api::{self as zed, Result};

struct VeldExtension {
    cached_binary_path: Option<String>,
}

impl VeldExtension {
    fn language_server_binary_path(
        &mut self,
        language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<String> {
        if let Some(path) = &self.cached_binary_path {
            if std::fs::metadata(path).map_or(false, |stat| stat.is_file()) {
                return Ok(path.clone());
            }
        }

        // Try to find veld-lsp in PATH
        if let Some(path) = worktree.which("veld-lsp") {
            self.cached_binary_path = Some(path.clone());
            return Ok(path);
        }

        // Try to find in cargo target directory
        if let Some(project_root) = worktree.read_text_file("Cargo.toml").ok() {
            // If we're in the veld project, use the debug build
            let debug_path = format!("{}/target/debug/veld-lsp", worktree.root_path());
            if std::fs::metadata(&debug_path).map_or(false, |stat| stat.is_file()) {
                self.cached_binary_path = Some(debug_path.clone());
                return Ok(debug_path);
            }

            // Try release build
            let release_path = format!("{}/target/release/veld-lsp", worktree.root_path());
            if std::fs::metadata(&release_path).map_or(false, |stat| stat.is_file()) {
                self.cached_binary_path = Some(release_path.clone());
                return Ok(release_path);
            }
        }

        Err(format!(
            "veld-lsp not found. Please ensure it's installed and in your PATH, \
             or build it with: cargo build -p veld-lsp"
        ))
    }
}

impl zed::Extension for VeldExtension {
    fn new() -> Self {
        Self {
            cached_binary_path: None,
        }
    }

    fn language_server_command(
        &mut self,
        language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let binary_path = self.language_server_binary_path(language_server_id, worktree)?;

        Ok(zed::Command {
            command: binary_path,
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(VeldExtension);
