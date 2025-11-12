# Fixed LSP Integration Issues

**Date:** November 11, 2024  
**Status:** ✅ Critical configuration issues resolved

---

## The Problem

The Veld extension was not loading the LSP server in Zed. There was:
- No hover information appearing
- No LSP status icon visible
- No language server features working at all

## Root Cause Analysis

After examining actual Zed extensions (Gleam, Elixir) from the official repository, I discovered **three critical configuration errors**:

### Issue 1: Wrong `language` vs `languages` field in extension.toml

**WRONG:**
```toml
[language_servers.veld]
name = "Veld Language Server"
languages = ["Veld"]  # ❌ This is WRONG for single-language extensions
```

**CORRECT:**
```toml
[language_servers.veld]
name = "Veld Language Server"
language = "Veld"  # ✅ Singular "language" for single language
```

**Why:** Zed's extension API expects `language = "LanguageName"` (singular) for extensions that support a single language. The `languages = [...]` (plural) form is only used when a single language server supports multiple languages (like Elixir's LSP supporting both "Elixir" and "HEEX").

### Issue 2: Incorrect `[language_servers]` section in config.toml

The `languages/veld/config.toml` file had:

```toml
[language_servers]
veld = {}
```

**This should NOT be in config.toml!** Language server mappings belong ONLY in `extension.toml`. The `config.toml` file is for language-specific settings like brackets, indentation, and syntax rules.

**Fix:** Removed the `[language_servers]` section entirely from `config.toml`.

### Issue 3: Conflicting language.toml file

We had BOTH `language.toml` and `config.toml` in the `languages/veld/` directory:
- `language.toml` had `language_server = { name = "none" }` which **disabled the LSP**
- Both files were competing for configuration

**Fix:** Deleted `language.toml` entirely. Standard Zed extensions (Gleam, Elixir, etc.) only use `config.toml`.

---

## The Correct Structure

After studying official Zed extensions, here's the correct structure:

### extension.toml (Root Configuration)
```toml
id = "veld"
name = "Veld"
description = "Veld language support for Zed with LSP"
version = "0.2.0"
schema_version = 1
authors = ["Perry <aidanpj18@gmail.com>"]
repository = "https://github.com/APConduct/zed-veld"

[language_servers.veld]
name = "Veld Language Server"
language = "Veld"  # ✅ Singular, matches the language name

[grammars.veld]
repository = "https://github.com/APConduct/tree-sitter-veld"
rev = "de99575c6e30ac83e4348c4a9dbe80c3f6f7a5da"
```

### languages/veld/config.toml (Language Settings)
```toml
name = "Veld"
grammar = "veld"
path_suffixes = ["veld"]
line_comments = ["# ", "#|"]
block_comment = ["#[[", "]]"]
word_characters = ["_"]

[indent]
tab_size = 4
unit = "    "

brackets = [
    { start = "{", end = "}", close = true, newline = true },
    { start = "[", end = "]", close = true, newline = true },
    { start = "(", end = ")", close = true, newline = true },
    { start = "\"", end = "\"", close = true, newline = false, not_in = ["string"] },
    { start = "'", end = "'", close = true, newline = false, not_in = ["string", "comment"] },
    { start = "#[[", end = "]]", close = true, newline = false, not_in = ["string", "comment"] },
]

# NO [language_servers] section here!

[code_actions]
surrounding_pairs = [
    { start = "{", end = "}" },
    { start = "[", end = "]" },
    { start = "(", end = ")" },
    { start = "\"", end = "\"" },
    { start = "'", end = "'" },
]
```

### File Structure
```
extensions/zed-veld/
├── extension.toml           # Extension metadata + language server mapping
├── extension.wasm           # Compiled extension
├── Cargo.toml              # Rust build config
├── src/
│   └── lib.rs              # Extension implementation (binary discovery)
├── languages/
│   └── veld/
│       ├── config.toml     # ✅ ONLY THIS FILE (not language.toml)
│       ├── highlights.scm  # Syntax highlighting
│       ├── brackets.scm    # Bracket matching
│       └── indents.scm     # Indentation rules
└── grammars/
    └── veld.wasm           # Tree-sitter grammar
```

---

## Changes Made

1. ✅ Changed `languages = ["Veld"]` to `language = "Veld"` in `extension.toml`
2. ✅ Removed `[language_servers]` section from `languages/veld/config.toml`
3. ✅ Deleted `languages/veld/language.toml` (was disabling LSP with `language_server = { name = "none" }`)
4. ✅ Rebuilt extension: `cargo build --release --target wasm32-wasip1`
5. ✅ Updated `extension.wasm` with new build

---

## Testing Instructions

1. **Remove old extension** (if previously installed):
   ```bash
   # macOS
   rm -rf ~/Library/Application\ Support/Zed/extensions/veld
   
   # Linux
   rm -rf ~/.config/zed/extensions/veld
   ```

2. **Install the fixed extension**:
   ```bash
   ./extensions/install-extension.sh
   ```

3. **Ensure veld-lsp is available**:
   ```bash
   # Verify it exists
   ls -lh target/release/veld-lsp
   
   # Make it executable
   chmod +x target/release/veld-lsp
   
   # Test it runs
   ./target/release/veld-lsp
   ```

4. **Restart Zed completely** (not just reload)

5. **Open a `.veld` file** from within this project directory

6. **Verify LSP is working**:
   - Look for LSP status icon in the bottom status bar
   - Hover over symbols → should show type information
   - Cmd/Ctrl+Click → should jump to definitions
   - Type and see autocomplete suggestions
   - Check logs: Command Palette → "zed: open log" → search for "veld"

---

## How Zed Extension Loading Works

Based on examining the official extensions:

1. **Extension Registration**: `extension.toml` declares the extension and its capabilities
2. **Language Server Binding**: `[language_servers.NAME]` with `language = "LanguageName"` creates the connection
3. **Language Configuration**: `languages/NAME/config.toml` defines syntax, indentation, brackets
4. **Extension Code**: `src/lib.rs` implements `Extension` trait, especially `language_server_command()`
5. **Runtime**: When Zed opens a file matching `path_suffixes`, it:
   - Loads the language config
   - Looks for a language server with matching `language` field
   - Calls `language_server_command()` to get the binary path
   - Starts the LSP process

---

## What Was Learned

### From Official Extensions

**Gleam Extension:**
- Uses `language = "Gleam"` (singular) in extension.toml
- Has only `config.toml` in `languages/gleam/`
- NO `language.toml` file
- NO `[language_servers]` section in `config.toml`

**Elixir Extension:**
- Uses `languages = ["Elixir", "HEEX"]` (plural) because one LSP serves TWO languages
- Also has only `config.toml` per language
- NO `[language_servers]` in language config files

**Pattern:** 
- `extension.toml` = "what language server(s) exist and what language(s) they serve"
- `config.toml` = "how this specific language behaves (syntax, formatting, etc.)"

---

## Why It Was Failing Before

1. **`languages = ["Veld"]`** → Zed couldn't match the language server to the language
2. **`[language_servers] veld = {}`** in config.toml → Overriding/confusing the extension.toml mapping
3. **`language.toml` with `language_server = { name = "none" }`** → Explicitly disabling LSP!

These three issues combined prevented the language server from ever being discovered or started.

---

## Success Criteria

You'll know it's working when:

✅ Opening a `.veld` file shows "Veld" in the language indicator  
✅ LSP status icon appears in bottom status bar  
✅ Hovering over code shows information popups  
✅ Cmd/Ctrl+Click jumps to definitions  
✅ Typing shows autocomplete/IntelliSense  
✅ Errors/warnings appear inline as you type  
✅ Zed logs show "Starting language server: Veld Language Server"  

---

## If It Still Doesn't Work

1. **Check Zed logs** (Command Palette → "zed: open log"):
   - Search for "veld" or "language server"
   - Look for error messages about binary not found

2. **Verify binary discovery** - The extension tries to find veld-lsp in this order:
   - PATH (`which veld-lsp`)
   - `<worktree>/target/release/veld-lsp`
   - `<worktree>/target/debug/veld-lsp`
   - `/usr/local/bin/veld-lsp`
   - `/usr/bin/veld-lsp`
   - `~/.cargo/bin/veld-lsp`

3. **Test LSP manually**:
   ```bash
   ./target/release/veld-lsp
   # Should start and wait for JSON-RPC messages on stdin
   # Ctrl+C to exit
   ```

4. **Reinstall extension** with symlink for easier debugging:
   ```bash
   rm -rf ~/Library/Application\ Support/Zed/extensions/veld
   ln -s $(pwd)/extensions/zed-veld ~/Library/Application\ Support/Zed/extensions/veld
   ```

5. **Check file permissions**:
   ```bash
   chmod +x target/release/veld-lsp
   chmod +x extensions/zed-veld/extension.wasm
   ```

---

## References

- Official Zed extensions: https://github.com/zed-industries/extensions
- Gleam extension: https://github.com/gleam-lang/zed-gleam
- Elixir extension: https://github.com/zed-extensions/elixir
- Zed extension docs: https://zed.dev/docs/extensions

---

**The extension is now correctly configured. Reinstall it and restart Zed to test!**