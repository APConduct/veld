# Veld Editor Extensions

This directory contains editor extensions for the Veld programming language.

## Available Extensions

### [Zed Extension](./veld/)

Official extension for the [Zed editor](https://zed.dev).

**Features:**
- Real-time diagnostics (syntax and type errors)
- Hover information (type signatures)
- Go-to-definition
- Context-aware code completion
- Syntax highlighting

**Status:** ✅ Functional (Beta)

## Philosophy: Monorepo Approach

We keep editor extensions in the main Veld repository because:

1. **Synchronized Development** - LSP and extensions evolve together
2. **Version Coordination** - Extension versions match language versions
3. **Simplified Testing** - Test language features and editor support together
4. **Single Source of Truth** - All Veld tooling in one place
5. **Easier Contributions** - One repo for language improvements and editor support

This follows the pattern used by many modern language projects (Rust, Zig, Gleam, etc.).

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

### Adding a New Extension

1. Create a new directory: `extensions/[editor-name]/`
2. Follow the editor's extension guidelines
3. Make it a thin wrapper around `veld-lsp`
4. Update this README
5. Submit a PR

### Improving Existing Extensions

1. Make changes in the relevant `extensions/[editor]/` directory
2. Test with the editor
3. Update documentation
4. Submit a PR

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

## Documentation

- [Veld LSP Server](../crates/lsp/README.md) - Core language server
- [Zed Extension](./veld/README.md) - Zed-specific extension
- [LSP Features](../crates/lsp/FEATURES.md) - Detailed feature documentation
- [Testing Guide](../crates/lsp/TESTING.md) - How to test LSP features

## Support

For issues:
- **LSP bugs** - Report in the main Veld repository
- **Editor-specific issues** - Report in the relevant extension subdirectory
- **Feature requests** - Discuss in Veld discussions/issues

## License

All extensions are licensed under the same license as Veld (see root LICENSE file).