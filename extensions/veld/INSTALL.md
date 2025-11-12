# Quick Install Guide - Veld for Zed

Get Veld language support in Zed in 3 steps.

## Prerequisites

- [Zed editor](https://zed.dev) installed
- Rust toolchain (for building the LSP)

## Installation Steps

### 1. Build the LSP Server

From the Veld repository root:

```bash
cargo build --release -p veld-lsp
```

This creates the language server binary at `target/release/veld-lsp`.

### 2. Install the Extension

**Option A: Development Mode (Recommended for now)**

```bash
# Navigate to veld root
cd /path/to/veld

# Create Zed extensions directory
mkdir -p ~/.config/zed/extensions

# Symlink the extension
ln -s "$(pwd)/extensions/veld" ~/.config/zed/extensions/veld
```

**Option B: Copy Extension**

```bash
cp -r extensions/veld ~/.config/zed/extensions/
```

### 3. Restart Zed

Close and reopen Zed, then:

1. Open any `.veld` file
2. Look for "Veld LSP" in the bottom-right status bar
3. Try hovering over code to see type information!

## Verify Installation

Create a test file `test.veld`:

```veld
# This should show no errors
let x = 42
let message = "Hello, Veld!"

fn greet(name: str) -> str
    "Hello, " + name
end

# Hover over 'x' to see: x: i32
# Hover over 'greet' to see the function signature
# Try Ctrl+Click on 'greet' to jump to definition
```

**What to check:**
- ✅ No red squiggles (diagnostics working)
- ✅ Hover shows type info
- ✅ Completions appear when typing
- ✅ Go-to-definition works (Ctrl/Cmd+Click)

## Troubleshooting

### Extension not loading

```bash
# Check the extension exists
ls ~/.config/zed/extensions/veld

# Should show: extension.toml, languages/, src/, etc.
```

### LSP not starting

```bash
# Verify the binary exists
ls target/release/veld-lsp

# Test it manually
./target/release/veld-lsp
# Should start and wait for input (Ctrl+C to exit)
```

### No features working

1. Open Zed's log: `Cmd+Shift+P` → "zed: open log"
2. Look for "veld-lsp" messages
3. Check for errors about missing binary

### LSP binary not found

The extension looks for `veld-lsp` in:
1. Your system PATH
2. `target/debug/veld-lsp` (if in veld project)
3. `target/release/veld-lsp` (if in veld project)

**Quick fix:** Add to PATH:

```bash
# Add to ~/.zshrc or ~/.bashrc
export PATH="$PATH:/path/to/veld/target/release"
```

## Custom LSP Path

If you installed `veld-lsp` elsewhere, configure in Zed settings (`Cmd+,`):

```json
{
  "lsp": {
    "veld-lsp": {
      "binary": {
        "path": "/custom/path/to/veld-lsp"
      }
    }
  }
}
```

## Updating

```bash
# Pull latest changes
git pull

# Rebuild LSP
cargo build --release -p veld-lsp

# Reload Zed: Cmd+Shift+P → "zed: reload extensions"
```

## Uninstallation

```bash
# Remove the extension
rm -rf ~/.config/zed/extensions/veld

# Restart Zed
```

## Next Steps

- [Extension README](./README.md) - Full documentation
- [LSP Features](../../crates/lsp/FEATURES.md) - What the LSP can do
- [Example Code](../../crates/lsp/examples/) - Test files

## Getting Help

- **Issues:** Open an issue in the Veld repository
- **Questions:** Ask in discussions
- **Bugs:** Provide Zed logs + example code

---

Happy coding! 🎉