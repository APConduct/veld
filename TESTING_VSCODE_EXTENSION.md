# VSCode Extension Testing Guide

This guide will help you test the Veld VSCode extension and verify that all LSP features are working correctly.

## Prerequisites

✅ VSCode extension installed: `veld-language-0.0.5.vsix`
✅ LSP binary built: `target/release/veld-lsp`
✅ Test file ready: `test_lsp_features.veld`

## Setup

1. **Open the project in VSCode**
   ```bash
   code /Users/aidanjost/projects/veld
   ```
   
   ⚠️ **IMPORTANT**: Open the entire project folder, not just a single file. The extension needs to find the LSP binary in `target/release/`.

2. **Open the test file**
   - Open `test_lsp_features.veld` in the editor
   - You should see syntax highlighting immediately

3. **Check that the LSP is running**
   - Open the Output panel: `View → Output` (or `Cmd+Shift+U`)
   - Select "Veld Language Server" from the dropdown
   - You should see initialization messages

4. **Monitor the LSP log**
   - In a terminal, run:
     ```bash
     tail -f lsp_server.log
     ```
   - This shows real-time LSP server activity

## Testing Checklist

### 1. ✅ Basic Connection

**Test**: Extension activates and connects to LSP
- **Expected**: Output panel shows "Veld Language Server starting" or similar
- **Expected**: `lsp_server.log` shows initialization and capability registration
- **If it fails**: 
  - Check if `veld-lsp` is in PATH or `target/release/`
  - Check Output panel for error messages
  - Check VSCode DevTools Console: `Help → Toggle Developer Tools`

---

### 2. ✅ Syntax Highlighting

**Test**: File is properly highlighted
- **Action**: Look at `test_lsp_features.veld`
- **Expected**: Keywords (`fn`, `let`, `if`, etc.) are colored
- **Expected**: Strings, numbers, and comments have distinct colors
- **If it fails**: The TextMate grammar may not be working (separate from LSP)

---

### 3. 🔍 Hover Information

**Test**: Hover shows type/signature information

**Test 3a - Function hover**:
- **Action**: Hover your mouse over `add` on line 8 (function definition)
- **Expected**: Tooltip shows function signature or documentation
- **Check log**: `lsp_server.log` should show `textDocument/hover` request

**Test 3b - Variable hover**:
- **Action**: Hover over `number` on line 30
- **Expected**: Shows type information (e.g., "Integer: 42")
- **Check log**: Hover request logged

**Test 3c - Function call hover**:
- **Action**: Hover over `add` inside the `calculate` function (line 15)
- **Expected**: Shows signature for the `add` function
- **Check log**: Position sent in hover request

**What to check if hover fails**:
- Does the log show the hover request arriving?
- Does it show a response being sent?
- Is the position correct? (row, column in the log)
- Check Output panel for errors

---

### 4. 🎯 Go to Definition

**Test**: Jump to symbol definitions

**Test 4a - Function definition**:
- **Action**: `Cmd+Click` (or `F12`) on `add` inside `calculate` function (line 15)
- **Expected**: Cursor jumps to `fn add(a, b)` on line 8
- **Check log**: `textDocument/definition` request logged

**Test 4b - Nested function**:
- **Action**: `Cmd+Click` on `adder` inside `make_adder` (line 75)
- **Expected**: Jumps to the nested `fn adder(y)` definition
- **Check log**: Definition request with correct position

**Test 4c - Recursive call**:
- **Action**: `Cmd+Click` on `factorial` inside the factorial function itself (line 44)
- **Expected**: Jumps to the `fn factorial(n)` definition
- **Check log**: Definition request

**What to check if go-to-definition fails**:
- Does the log show `textDocument/definition` requests?
- Does the response include location information?
- Check if the LSP is parsing the file correctly (look for parse errors in log)

---

### 5. 💡 Autocomplete

**Test**: Completions suggest available symbols

**Test 5a - Function completion**:
- **Action**: At the end of the file, type: `let test = fa`
- **Expected**: Completion menu shows `factorial`, `fibonacci`
- **Action**: Press `Esc` to cancel
- **Check log**: `textDocument/completion` request logged

**Test 5b - Scope-aware completion**:
- **Action**: Type: `let test2 = make_`
- **Expected**: Shows `make_adder`, `make_counter`
- **Check log**: Completion request and response

**Test 5c - All symbols**:
- **Action**: Type just a few letters that match many functions
- **Expected**: Dropdown shows relevant functions
- **Check log**: Response includes multiple completion items

**What to check if completion fails**:
- Is `completionProvider` capability advertised in initialization?
- Does the log show completion requests?
- Are completion items being returned? (check the JSON in log)

---

### 6. 🔴 Diagnostics (Errors)

**Test**: LSP detects errors

**Test 6a - Add an error**:
- **Action**: At the end of `test_lsp_features.veld`, add:
  ```veld
  let error_test = undefined_function(1, 2)
  ```
- **Expected**: Red squiggly line under `undefined_function`
- **Expected**: Hover over the error shows diagnostic message
- **Check log**: `textDocument/publishDiagnostics` notification sent

**Test 6b - Fix the error**:
- **Action**: Delete the line you just added
- **Expected**: Red squiggle disappears
- **Check log**: New diagnostics notification (possibly empty)

**What to check if diagnostics fail**:
- Does the LSP send `textDocument/publishDiagnostics`?
- Is the diagnostic range correct?
- Check if the analyzer is running on document changes

---

### 7. 📋 Document Symbols

**Test**: Outline view shows file structure

**Test 7a - Open outline**:
- **Action**: Open the Outline view in the sidebar (`Cmd+Shift+O` or view in Explorer)
- **Expected**: Shows functions like `add`, `multiply`, `factorial`, etc.
- **Check log**: `textDocument/documentSymbol` request

**Test 7b - Navigate via outline**:
- **Action**: Click on `factorial` in the outline
- **Expected**: Editor jumps to that function
- **Check log**: Symbol request and response with ranges

**What to check if symbols fail**:
- Does the log show `textDocument/documentSymbol` requests?
- Are symbols being returned in the response?

---

### 8. 🔍 Workspace Symbols

**Test**: Global symbol search

**Test 8a - Search symbols**:
- **Action**: Press `Cmd+T` (Go to Symbol in Workspace)
- **Action**: Type `factorial`
- **Expected**: Shows `factorial` function with file location
- **Check log**: `workspace/symbol` request

**What to check if workspace symbols fail**:
- Does `workspaceSymbolProvider` capability exist?
- Is the request reaching the server?

---

## Performance Tests

### 9. ⚡ Large File Handling

**Test**: LSP handles larger files

**Action**: Create a test file with many functions:
```bash
cat > large_test.veld << 'EOF'
fn func_1(x) x + 1 end
fn func_2(x) x + 2 end
fn func_3(x) x + 3 end
# ... (add 50-100 similar functions)
EOF
```

- **Expected**: LSP processes the file without lag
- **Expected**: Hover, completion, etc. still work
- **Check log**: Processing time logged

---

## Debugging Tips

### Check Extension Status
1. Open Command Palette (`Cmd+Shift+P`)
2. Type "Developer: Show Running Extensions"
3. Find "Veld Language" - should show "Activated"

### View All Logs
```bash
# Terminal 1: LSP server log
tail -f lsp_server.log

# Terminal 2: Check if LSP process is running
ps aux | grep veld-lsp
```

### VSCode DevTools Console
- `Help → Toggle Developer Tools`
- Check Console tab for extension errors
- Check Network tab if using TCP connection

### Force Restart LSP
1. Command Palette → "Developer: Reload Window"
2. Or restart VSCode entirely

### Check LSP Binary
```bash
# Test LSP manually (it should wait for input)
./target/release/veld-lsp

# Check version/help (if implemented)
./target/release/veld-lsp --version
./target/release/veld-lsp --help
```

---

## Expected Log Output Examples

### Successful Initialization
```
INFO veld_lsp: LSP Server starting...
INFO veld_lsp: Received initialize request
INFO veld_lsp: Sent capabilities
INFO veld_lsp: Received initialized notification
```

### Hover Request
```
DEBUG veld_lsp: Received textDocument/hover
DEBUG veld_lsp: File: file:///path/to/test.veld, Position: Line 15, Col 10
DEBUG veld_lsp: Found identifier: add
INFO veld_lsp: Sending hover response
```

### Completion Request
```
DEBUG veld_lsp: Received textDocument/completion
DEBUG veld_lsp: Returning 12 completion items
```

### Diagnostics
```
INFO veld_lsp: Publishing diagnostics for file:///path/to/test.veld
DEBUG veld_lsp: 1 diagnostic(s)
```

---

## Common Issues & Solutions

### Issue: "LSP binary not found"
**Solution**: 
- Build the LSP: `cargo build --release --bin veld-lsp`
- Add to PATH or open workspace folder in VSCode

### Issue: "EOF reached, shutting down"
**Solution**: 
- Rebuild LSP (the old version had this bug)
- Check that VSCode is sending data on stdin

### Issue: No hover/completion working
**Solution**:
- Check that capabilities are advertised in initialization
- Verify requests are reaching the server (check log)
- Test if the analyzer is working: check for parse errors in log

### Issue: Red squiggles everywhere (false errors)
**Solution**:
- Check if parser is compatible with current Veld syntax
- Look for parse errors in `lsp_server.log`
- May need to update analyzer to match language evolution

### Issue: Slow performance
**Solution**:
- Check if LSP is re-parsing on every keystroke
- Consider implementing incremental parsing
- Profile the analyzer functions

---

## Reporting Results

After testing, please report:

### ✅ What works:
- [ ] Extension activates
- [ ] LSP connects
- [ ] Syntax highlighting
- [ ] Hover information
- [ ] Go to definition
- [ ] Autocompletion
- [ ] Diagnostics
- [ ] Document symbols
- [ ] Workspace symbols

### ❌ What doesn't work:
- For each failing feature:
  - What you tried
  - What you expected
  - What actually happened
  - Relevant log excerpts from `lsp_server.log`
  - Relevant errors from Output panel

### 📋 Logs to include:
```bash
# Capture the full LSP log
cp lsp_server.log lsp_test_results.log

# Capture VSCode output
# (copy from Output panel → Veld Language Server)
```

---

## Next Steps After Testing

Based on test results, we'll prioritize:

1. **If hover fails**: Improve position-to-symbol mapping in analyzer
2. **If completion fails**: Enhance scope analysis and symbol collection
3. **If definition fails**: Fix identifier resolution logic
4. **If diagnostics fail**: Debug analyzer error detection
5. **If performance is slow**: Add caching and incremental updates

Good luck testing! 🚀