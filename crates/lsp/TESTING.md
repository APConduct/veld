# Testing the Veld LSP Server

This guide explains how to test the Veld Language Server Protocol implementation.

## Quick Test with Example Files

We provide two example files for testing:

1. **`examples/test.veld`** - Valid Veld code showcasing various language features
2. **`examples/with_errors.veld`** - Code with intentional errors to test diagnostics

## Manual Testing with `nc` (netcat)

You can test the LSP server manually using stdin/stdout:

### 1. Start the server

```bash
cargo run -p veld-lsp
```

### 2. Send LSP messages

The LSP protocol uses the following format:

```
Content-Length: <length>\r\n\r\n
<JSON payload>
```

Example initialize message:

```
Content-Length: 123\r\n\r\n
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"processId":null,"rootUri":null}}
```

## Automated Testing

### Unit Tests

Run the test suite:

```bash
cargo test -p veld-lsp
```

Tests include:
- Analyzer with valid code
- Analyzer with syntax errors
- Position extraction from error messages
- Line length calculation
- RPC message encoding/decoding

### Integration Testing with LSP Client

Use an LSP client library to test the server programmatically:

```python
# Example using python-lsp-jsonrpc
from pylspclient import lsp_client

# Connect to the LSP server
lsp = lsp_client.LspClient(lsp_endpoint)

# Initialize
lsp.initialize(...)

# Open document
lsp.didOpen(uri="file:///path/to/test.veld", content="let x = 42")

# Get diagnostics
diagnostics = lsp.getDiagnostics()
```

## Testing in Your Editor

### VS Code

1. Create a basic extension or use generic LSP extension
2. Configure to point to `target/debug/veld-lsp`
3. Open `.veld` files and verify:
   - Syntax errors show up as red squiggles
   - Type errors are reported
   - Hover shows (placeholder) information
   - Completion suggests keywords

### Neovim with nvim-lspconfig

```lua
require('lspconfig').veld.setup{
    cmd = {'cargo', 'run', '-p', 'veld-lsp'},
    filetypes = {'veld'},
}
```

Open a Veld file:
```bash
nvim examples/test.veld
```

Test features:
- `:LspInfo` - Check if server is attached
- Open `examples/with_errors.veld` - Should see diagnostics
- Hover over identifiers - Should see hover info

### Zed

1. Configure Zed to use the LSP (see main README)
2. Open `examples/test.veld`
3. Verify diagnostics panel shows no errors
4. Open `examples/with_errors.veld`
5. Verify diagnostics panel shows multiple errors

## Expected Diagnostics for `with_errors.veld`

The `with_errors.veld` file should produce diagnostics for:

1. **Line 8**: Incomplete expression (`let incomplete =`)
2. **Line 11**: Type mismatch (adding integer and string)
3. **Line 14**: Undefined function
4. **Line 17-19**: Function returns wrong type
5. **Line 22**: Incomplete function (missing body)
6. **Line 27-32**: Non-exhaustive match (missing Blue case)
7. **Line 38**: Invalid field access on non-struct type
8. **Line 43**: Using variable before declaration

## Verifying Diagnostics

### Check diagnostic format

Each diagnostic should have:
- `range`: start/end line and character positions
- `severity`: 1 (Error), 2 (Warning), 3 (Info), 4 (Hint)
- `source`: "veld"
- `message`: Human-readable error description

Example diagnostic JSON:

```json
{
  "range": {
    "start": {"line": 7, "character": 0},
    "end": {"line": 7, "character": 18}
  },
  "severity": 1,
  "source": "veld",
  "message": "Parser error: Unexpected end of input"
}
```

## Debugging

### Enable Logging

The LSP server logs to `lsp_server.log`. Check this file for:
- Message received/sent logs
- Analysis pipeline execution
- Error details

### Common Issues

1. **Server not starting**: Check cargo build succeeded
2. **No diagnostics**: Verify `textDocument/didOpen` was sent
3. **Wrong diagnostics**: Check analyzer pipeline in `analysis.rs`
4. **Performance issues**: Look for repeated analysis in logs

## Performance Testing

Test with larger files:

```bash
# Generate a large test file
for i in {1..1000}; do
  echo "let x$i = $i" >> large_test.veld
done

# Open in editor and verify responsiveness
```

Expected behavior:
- Analysis completes in < 1 second for files < 10,000 lines
- Diagnostics update within 100ms of typing (debounced)
- No blocking on typing

## CI/CD Testing

Add to your CI pipeline:

```yaml
- name: Test LSP
  run: |
    cargo test -p veld-lsp
    cargo build -p veld-lsp --release
    # Optionally: Run integration tests
```

## Next Steps

Once basic diagnostics work:

1. Test hover information with real type data
2. Test go-to-definition navigation
3. Test context-aware completions
4. Test document symbols
5. Test workspace-wide features

## Reporting Issues

When reporting LSP issues, include:
- LSP server log (`lsp_server.log`)
- Example Veld code that triggers the issue
- Expected vs actual behavior
- Editor/client being used