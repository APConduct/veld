# Quick Start: Testing the Fixed Veld Extension

**Last Updated:** November 11, 2024  
**Status:** ✅ Extension fixed and ready to test

---

## What Was Fixed

Three critical configuration errors were preventing the LSP from loading:
1. ❌ `languages = ["Veld"]` → ✅ `language = "Veld"` in extension.toml
2. ❌ `[language_servers]` section in config.toml → ✅ Removed (belongs only in extension.toml)
3. ❌ `language.toml` file disabling LSP → ✅ Deleted (only config.toml should exist)

---

## Test in 3 Steps

### 1. Restart Zed
**Important:** Quit Zed completely (Cmd+Q), don't just reload the window.

### 2. Open a Veld File
Open any `.veld` file from within this project directory:
```bash
# Example: Open a test file
open -a Zed examples/some_file.veld
```

### 3. Check for LSP Features

✅ **Status Bar:** Look for "Veld" language indicator (bottom right)  
✅ **LSP Icon:** Should see language server status icon  
✅ **Hover:** Hover over code → type/documentation popup  
✅ **Go to Definition:** Cmd+Click on symbols  
✅ **Autocomplete:** Start typing → suggestions appear  
✅ **Diagnostics:** Errors/warnings shown inline  

---

## Debugging

### Check Zed Logs
1. Open Command Palette: `Cmd+Shift+P`
2. Type: `zed: open log`
3. Search for: `veld` or `language server`
4. Look for: "Starting language server: Veld Language Server"

### If LSP Not Starting

**Verify the binary exists and works:**
```bash
# Check it exists
ls -lh target/release/veld-lsp

# Make it executable
chmod +x target/release/veld-lsp

# Test it runs (Ctrl+C to exit)
./target/release/veld-lsp
```

**Check extension installation:**
```bash
# macOS
ls -la ~/Library/Application\ Support/Zed/extensions/veld

# Should be a symlink pointing to:
# /Users/aidanjost/projects/veld/extensions/zed-veld
```

**Reinstall if needed:**
```bash
./extensions/install-extension.sh
```

---

## Expected Behavior

When working correctly, you should see:

1. **On opening a .veld file:**
   - Status bar shows "Veld" as language
   - LSP status icon appears (may take 1-2 seconds)
   - No errors in Zed logs

2. **While editing:**
   - Hover over symbols shows information
   - Autocomplete suggestions appear
   - Errors/warnings display inline
   - Go-to-definition works (Cmd+Click)

3. **In Zed logs:**
   ```
   Starting language server: Veld Language Server
   Language server started successfully
   ```

---

## Still Not Working?

1. **Restart Zed again** (seriously, quit completely)
2. **Check you're opening .veld files from THIS project** (LSP binary discovery looks in project root)
3. **Review detailed docs:**
   - `extensions/FIXED_LSP_INTEGRATION.md` - Full technical details
   - `extensions/CRITICAL_FIX_SUMMARY.md` - What was broken and how it was fixed

---

## Extension Architecture

**How it finds veld-lsp (in order):**
1. System PATH (`which veld-lsp`)
2. Project release build: `target/release/veld-lsp`
3. Project debug build: `target/debug/veld-lsp`
4. Common locations: `/usr/local/bin`, `/usr/bin`, `~/.cargo/bin`

**Configuration:**
- `extension.toml` → Declares language server with `language = "Veld"`
- `languages/veld/config.toml` → Language syntax settings only
- `src/lib.rs` → Binary discovery logic

---

## Success!

If you see hover information and the LSP status icon, the extension is working! 🎉

The configuration issues are fixed. If it's still not working after following the debugging steps above, check the Zed logs for specific error messages.

---

**Need Help?** See `extensions/FIXED_LSP_INTEGRATION.md` for detailed troubleshooting.