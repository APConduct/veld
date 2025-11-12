# Veld Language Server Protocol (LSP)

A Language Server Protocol implementation for the Veld programming language, providing IDE features like diagnostics, hover information, go-to-definition, and code completion.

## Features

### Currently Implemented

- ✅ **Diagnostics**: Real-time syntax and type error reporting
  - Lexer errors (tokenization issues)
  - Parser errors (syntax problems)
  - Type checker errors (type mismatches, undefined symbols)
  - Errors displayed inline with proper line/column ranges
  - Automatic diagnostics on document open and change

- ✅ **Document Synchronization**: Tracks open documents and updates
  - `textDocument/didOpen`
  - `textDocument/didChange`
  - `textDocument/didSave`
  - `textDocument/didClose`

- ✅ **Hover Information**: Shows type information for symbols
  - Displays type information for variables, functions, structs
  - Formatted type signatures with full generic support
  - Works with type checker's environment

- ✅ **Go-to-Definition**: Jump to symbol definitions
  - Navigate to function, struct, enum, and type declarations
  - Works for variables and top-level symbols
  - Returns precise source locations

- ✅ **Context-Aware Completion**: Smart completions based on type information
  - Keywords (fn, let, var, struct, enum, impl, match, if, etc.)
  - Functions with parameter signatures
  - Structs and types
  - Variables with type annotations
  - Pulls from type checker's symbol environment

### Planned Features

- 🚧 **Document Symbols**: Outline view of functions, structs, etc.
- 🚧 **Workspace Symbols**: Project-wide symbol search
- 🚧 **Code Actions**: Quick fixes and refactorings
- 🚧 **Formatting**: Auto-format Veld code
- 🚧 **Rename**: Rename symbols across the project
- 🚧 **Semantic Tokens**: Syntax highlighting based on semantic analysis
- 🚧 **Signature Help**: Show function parameter hints while typing

## Building

```bash
cargo build -p veld-lsp
```

## Running

The LSP server communicates over stdin/stdout using the Language Server Protocol:

```bash
cargo run -p veld-lsp
```

## Testing

Run the test suite:

```bash
cargo test -p veld-lsp
```

## Usage with Editors

### VS Code

1. Install the Veld extension (if available)
2. Configure the extension to use this LSP server
3. Point it to the built binary: `target/debug/veld-lsp` (or `target/release/veld-lsp`)

### Neovim

Add to your LSP configuration:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define the veld LSP
if not configs.veld then
  configs.veld = {
    default_config = {
      cmd = {'/path/to/veld/target/debug/veld-lsp'},
      filetypes = {'veld'},
      root_dir = lspconfig.util.root_pattern('.git', 'veld.toml'),
      settings = {},
    },
  }
end

-- Setup the server
lspconfig.veld.setup{}
```

### Zed

Create or edit `~/.config/zed/settings.json`:

```json
{
  "lsp": {
    "veld": {
      "binary": {
        "path": "/path/to/veld/target/debug/veld-lsp"
      }
    }
  },
  "languages": {
    "Veld": {
      "language_servers": ["veld"]
    }
  }
}
```

## Architecture

### Document Analysis Pipeline

```
Source Code
    ↓
Lexer (tokenize)
    ↓
Parser (AST)
    ↓
Type Checker
    ↓
Diagnostics + Symbol Information
```

### Module Structure

- **`main.rs`**: LSP server implementation, message handling
- **`rpc.rs`**: LSP message encoding/decoding
- **`analysis.rs`**: Document analysis (lexer → parser → type checker)

### How It Works

1. **Document Opens/Changes**: When a document is opened or modified, the server:
   - Runs the full analysis pipeline (lex → parse → type check)
   - Captures any errors and converts them to LSP diagnostics
   - Caches the AST and type checker for future queries
   - Sends `textDocument/publishDiagnostics` notifications to the client

2. **Hover Requests**: When the user hovers over code:
   - Finds the identifier at the cursor position in the AST
   - Looks up type information from the type checker's environment
   - Formats and returns the type signature (e.g., "x: i32", "fn(i32, i32) -> i32")
   - Supports all Veld types: primitives, functions, generics, structs, enums, etc.

3. **Go-to-Definition**: When the user requests definition:
   - Identifies the symbol at cursor position
   - Searches the AST for the symbol's declaration
   - Returns the source location (line/column) of the definition
   - Works for functions, structs, enums, types, and variables

4. **Completion Requests**: When the user triggers completion:
   - Extracts all top-level declarations from the AST
   - Provides keyword completions (fn, let, struct, match, etc.)
   - Lists available functions with their signatures
   - Lists structs, types, and variables with type information
   - Pulls symbol information from the type checker's environment

## Development

### Adding New Features

1. **Update `analysis.rs`**: Add methods to extract information from AST/type checker
2. **Update `main.rs`**: Wire up the LSP message handler
3. **Test**: Add test cases in the respective test modules

### Debugging

The server logs to `lsp_server.log` in the current directory. Enable with:

```rust
tracing::info!("Your debug message");
```

## Contributing

Contributions are welcome! Areas that need work:

- Improve position tracking for more precise hover/goto-definition
- Add support for method completions (after `.` or `::`)
- Implement document symbols for outline view
- Workspace-wide symbol search
- Performance optimizations (incremental parsing, better caching)
- Better error messages and diagnostics
- Signature help for function calls
- Code actions (quick fixes, refactorings)

## License

Same as the Veld project.