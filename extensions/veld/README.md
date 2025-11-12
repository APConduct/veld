# Veld Zed Extension

This is the official [Zed](https://zed.dev) editor extension for the Veld programming language.

## Features

- 🔍 **Real-time Diagnostics** - Syntax and type errors as you type
- 💡 **Hover Information** - Type information on hover
- 🎯 **Go-to-Definition** - Jump to symbol definitions
- ✨ **Smart Completions** - Context-aware code completion
- 🎨 **Syntax Highlighting** - Full Veld syntax support

## Installation

### From Zed Extension Registry (Coming Soon)

Once published, install directly from Zed:
1. Open Zed
2. Press `Cmd+Shift+P` (Mac) or `Ctrl+Shift+P` (Linux/Windows)
3. Type "extensions"
4. Search for "Veld"
5. Click Install

### Development Installation

If you're developing the extension or want to use the latest version:

1. **Build the LSP server first:**
   ```bash
   cd ../../  # Navigate to veld root
   cargo build --release -p veld-lsp
   ```

2. **Link the extension to Zed:**
   ```bash
   # Create Zed extensions directory if it doesn't exist
   mkdir -p ~/.config/zed/extensions
   
   # Symlink this extension
   ln -s $(pwd)/extensions/veld ~/.config/zed/extensions/veld
   ```

3. **Restart Zed**

4. **Verify installation:**
   - Open a `.veld` file
   - Check the bottom-right corner for "Veld LSP" status
   - Try hovering over variables to see type information

## Requirements

- **Veld LSP Server**: The extension requires the `veld-lsp` binary to be available
  - Built automatically if you're in the Veld project directory
  - Or install system-wide: `cargo install --path crates/lsp`
  - Or ensure `veld-lsp` is in your PATH

## Configuration

The extension works out of the box with no configuration needed. However, you can customize behavior in Zed's settings:

```json
{
  "lsp": {
    "veld-lsp": {
      "binary": {
        "path": "/custom/path/to/veld-lsp",
        "arguments": []
      }
    }
  }
}
```

## File Association

The extension automatically activates for files with the `.veld` extension.

## Features in Detail

### Diagnostics
- Lexer errors (invalid tokens)
- Parser errors (syntax issues)
- Type errors (type mismatches, undefined symbols)
- Non-exhaustive pattern matches

### Hover
Shows type information for:
- Variables: `x: i32`
- Functions: `fn(i32, i32) -> i32`
- Generics: `Option<T>`, `Result<T, E>`
- Structs and Enums

### Go-to-Definition
Jump to the declaration of:
- Functions
- Structs
- Enums and Types
- Variables

### Completions
Context-aware suggestions for:
- Keywords (`fn`, `let`, `struct`, `match`, etc.)
- Functions with signatures
- Structs and types
- Variables with type information

## Troubleshooting

### Extension not loading
1. Check if `veld-lsp` is in your PATH: `which veld-lsp`
2. Check Zed's log: `Cmd+Shift+P` → "Open Log"
3. Verify the extension is installed: `~/.config/zed/extensions/veld`

### LSP not starting
1. Ensure the LSP binary is built: `cargo build -p veld-lsp`
2. Check the binary location:
   - Debug: `target/debug/veld-lsp`
   - Release: `target/release/veld-lsp`
3. Test the LSP manually: `./target/debug/veld-lsp`

### No diagnostics/hover/completions
1. Check the LSP is running in Zed's status bar
2. Look for errors in Zed's log
3. Try reopening the file or restarting Zed

## Development

This extension is part of the Veld language repository and is developed alongside the language itself.

### Building the Extension

```bash
# Build the LSP server
cargo build -p veld-lsp

# The extension Rust code is compiled by Zed automatically
# when it loads the extension
```

### Testing

1. Make changes to `src/lib.rs` or configuration files
2. Reload Zed: `Cmd+Shift+P` → "Reload Extensions"
3. Test with example Veld files in `../../examples/`

### File Structure

```
extensions/veld/
├── extension.toml         # Extension metadata
├── Cargo.toml             # Rust dependencies
├── src/
│   └── lib.rs            # Extension entry point (finds LSP binary)
├── languages/
│   └── veld/
│       └── config.toml   # Language configuration
└── README.md             # This file
```

## Contributing

Contributions are welcome! Please submit issues and pull requests to the main Veld repository.

### Areas for Improvement

- Better syntax highlighting (tree-sitter grammar)
- Code actions (quick fixes)
- Refactoring support
- Signature help
- Document symbols

## Links

- [Veld Language Repository](https://github.com/yourusername/veld)
- [Veld LSP Documentation](../../crates/lsp/README.md)
- [Zed Extension Documentation](https://zed.dev/docs/extensions)

## License

Same as the Veld project (see root LICENSE file).