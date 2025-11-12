# Veld Editor Extensions

This directory contains editor extensions for the Veld programming language.

## Available Extensions

### [Zed Extension](./zed-veld/) - Git Submodule

Official extension for the [Zed editor](https://zed.dev).

**Repository:** https://github.com/APConduct/zed-veld

**Features:**
- ✅ Syntax highlighting (tree-sitter grammar)
- ✅ Real-time diagnostics (syntax and type errors)
- ✅ Hover information (type signatures)
- ✅ Go-to-definition
- ✅ Context-aware code completion
- ✅ Auto-closing brackets and quotes
- ✅ Code folding

**Status:** ✅ Functional (Beta)

## Setup Instructions

### First Time Clone

```bash
# Clone the veld repo with submodules
git clone --recursive https://github.com/APConduct/veld.git

# Or if already cloned:
git submodule update --init --recursive
```

### Installing the Extension

1. **Build the LSP server:**
   ```bash
   cd veld
   cargo build --release -p veld-lsp
   ```

2. **Link the extension to Zed:**
   ```bash
   mkdir -p ~/.config/zed/extensions
   ln -s "$(pwd)/extensions/zed-veld" ~/.config/zed/extensions/veld
   ```

3. **Restart Zed** and open a `.veld` file!

## Philosophy: Monorepo + Submodule Approach

We use a **git submodule** for the Zed extension because:

1. **Independent Publishing** - Extension can be published to Zed registry separately
2. **Synchronized Development** - Still linked to main repo for coordinated testing
3. **Version Control** - Pin specific extension versions in the main repo
4. **Separate Concerns** - Syntax highlighting lives in extension, LSP in main repo
5. **Flexibility** - Extension can be developed/tested independently

The LSP server (`crates/lsp/`) stays in the main repo because it's core Veld tooling.

This follows the pattern used by modern language projects (Rust, Zig, Gleam, etc.).

## Future Extensions

Planned support for other editors:

- **VS Code** - Visual Studio Code extension (planned)
- **Neovim** - Native LSP configuration guide (planned)
- **IntelliJ** - IntelliJ IDEA plugin (planned)
- **Emacs** - LSP mode configuration (planned)

## How Extensions Work

All extensions are thin wrappers around the Veld LSP server:

```
┌─────────────────┐
│  Editor (Zed)   │
└────────┬────────┘
         │ LSP Protocol
         │ (JSON-RPC)
┌────────▼────────┐
│   veld-lsp      │ ◄── Core intelligence
│  (LSP Server)   │     - Parsing
└────────┬────────┘     - Type checking
         │              - Symbol lookup
┌────────▼────────┐
│  Veld Compiler  │
│   (veld-common) │
└─────────────────┘
```

The LSP server (`crates/lsp/`) provides all the language intelligence. Extensions just:
1. Launch the LSP server binary
2. Configure file associations (`.veld` files)
3. Provide editor-specific UI integration

## Using the LSP Server Directly

If your editor supports LSP but isn't listed here, you can configure it manually:

### Generic LSP Configuration

```json
{
  "command": "veld-lsp",
  "filetypes": ["veld"],
  "rootPatterns": [".git", "veld.toml"],
  "settings": {}
}
```

### Build the LSP Server

```bash
# From veld root directory
cargo build --release -p veld-lsp

# Binary will be at:
# target/release/veld-lsp

# Install system-wide (optional):
cargo install --path crates/lsp
```

### Editor-Specific Setup

**Neovim with nvim-lspconfig:**
```lua
require('lspconfig').veld.setup{
    cmd = {'veld-lsp'},
    filetypes = {'veld'},
}
```

**VS Code (settings.json):**
```json
{
  "veld.lsp.path": "/path/to/veld-lsp"
}
```

**Emacs (lsp-mode):**
```elisp
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "veld-lsp")
                  :major-modes '(veld-mode)
                  :server-id 'veld-lsp))
```

## Contributing

### Updating the Zed Extension

The Zed extension is a git submodule. To make changes:

1. **Navigate to the submodule:**
   ```bash
   cd extensions/zed-veld
   ```

2. **Make your changes** (syntax highlighting, config, etc.)

3. **Test locally:**
   ```bash
   # Reload extension in Zed: Cmd+Shift+P → "zed: reload extensions"
   ```

4. **Commit to the extension repo:**
   ```bash
   git add .
   git commit -m "Your changes"
   git push origin main
   ```

5. **Update the main repo to use new version:**
   ```bash
   cd ../..  # Back to veld root
   git add extensions/zed-veld
   git commit -m "Update zed-veld extension"
   ```

### Improving the LSP

LSP improvements go in `crates/lsp/`:

1. Make changes in `crates/lsp/src/`
2. Test with `cargo test -p veld-lsp`
3. Rebuild: `cargo build --release -p veld-lsp`
4. Test in Zed (reload extension to pick up new binary)
5. Submit a PR to main repo

### Adding a New Editor Extension

1. Create a new repository for the extension
2. Add as submodule: `git submodule add <url> extensions/[editor-name]`
3. Make it a thin wrapper around `veld-lsp`
4. Update this README
5. Submit a PR

### Testing

Always test with real Veld code:
- Valid code (see `examples/`)
- Code with errors (see `crates/lsp/examples/with_errors.veld`)
- Large files (performance testing)

## LSP Capabilities

Current LSP features (provided by `veld-lsp`):

| Feature | Status | Quality |
|---------|--------|---------|
| Diagnostics | ✅ | Production |
| Hover | ✅ | Beta |
| Go-to-Definition | ✅ | Beta |
| Completion | ✅ | Beta |
| Document Symbols | 🚧 | Planned |
| Workspace Symbols | 🚧 | Planned |
| Signature Help | 🚧 | Planned |
| Code Actions | 🚧 | Planned |
| Formatting | 🚧 | Planned |
| Rename | 🚧 | Planned |

When new LSP features are added to `veld-lsp`, all editor extensions automatically benefit.

## Submodule Management

### Update Submodule to Latest

```bash
cd extensions/zed-veld
git pull origin main
cd ../..
git add extensions/zed-veld
git commit -m "Update zed-veld to latest"
```

### Check Submodule Status

```bash
git submodule status
```

### Clone with Submodules

```bash
git clone --recursive https://github.com/APConduct/veld.git
```

## Documentation

- [Veld LSP Server](../crates/lsp/README.md) - Core language server
- [Zed Extension](./zed-veld/README.md) - Zed extension (in submodule)
- [Tree-sitter Grammar](https://github.com/APConduct/tree-sitter-veld) - Syntax grammar
- [LSP Features](../crates/lsp/FEATURES.md) - Detailed feature documentation
- [Testing Guide](../crates/lsp/TESTING.md) - How to test LSP features

## Support

For issues:
- **LSP bugs** - Report in the main Veld repository
- **Editor-specific issues** - Report in the relevant extension subdirectory
- **Feature requests** - Discuss in Veld discussions/issues

## License

All extensions are licensed under the same license as Veld (see root LICENSE file).