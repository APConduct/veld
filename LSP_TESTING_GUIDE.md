# LSP Testing Guide for Veld VSCode Extension

**Date:** November 12, 2024  
**Status:** Ready for Testing  
**Extension Version:** 0.0.5

---

## Quick Start Testing

### Prerequisites

1. **Install the extension:**
   ```bash
   code --uninstall-extension veld-dev.veld-language
   code --install-extension extensions/vscode-veld/veld-language-0.0.5.vsix
   ```

2. **Restart VSCode completely** (Cmd+Q on Mac, then reopen)

3. **Open the Veld project folder:**
   ```bash
   code /Users/aidanjost/projects/veld
   ```

4. **Open the test file:**
   - Open `test_lsp_features.veld` from the project root
   - Or create a new `.veld` file

---

## Feature Testing Checklist

### ✅ 1. Extension Activation

**What to check:**
- [ ] File extension `.veld` is recognized
- [ ] Bottom-right corner shows "Veld" as language
- [ ] No error notifications appear

**Success:** Language indicator shows "Veld"  
**Failure:** "Plain Text" or error notification

---

### ✅ 2. LSP Server Connection

**What to check:**
- [ ] Notification appears: "Veld LSP server is running"
- [ ] Output panel shows LSP activity
- [ ] Log file shows initialization

**To verify:**
```bash
# Check the log file
tail -f lsp_server.log

# Should see:
# "LSP Server starting..."
# "Received method: initialize"
# "Received method: initialized"
# "Received method: textDocument/didOpen"
```

**VSCode Output Panel:**
- View → Output
- Select "Veld Language Server" from dropdown
- Should show connection messages

**Success:** Log shows initialization messages  
**Failure:** "EOF reached" or no log activity

---

### ✅ 3. Syntax Highlighting

**What to check:**
- [ ] Keywords colored (fn, let, if, then, else, end)
- [ ] Strings colored
- [ ] Comments colored
- [ ] Numbers colored

**Test code:**
```veld
# This is a comment
fn test(a, b)
    let result = "hello"
    if a > 10 then
        42
    else
        0
    end
end
```

**Success:** Different colors for different syntax elements  
**Failure:** Everything same color (means grammar not loading)

---

### ✅ 4. Hover Information

**What to check:**
- [ ] Hover over function name shows signature
- [ ] Hover over variable shows type
- [ ] Hover over parameter shows info

**Test steps:**
1. Open `test_lsp_features.veld`
2. Hover mouse over `add` function name
3. Wait 1 second for hover popup

**Expected result:**
- Popup appears with function information
- Shows type or signature

**Test code:**
```veld
fn add(a, b)
    a + b
end

let result = add(5, 10)
# Hover over 'add' in line above
```

**Success:** Popup shows function info  
**Failure:** No popup or "Loading..." that never completes

---

### ✅ 5. Go to Definition

**What to check:**
- [ ] Cmd+Click (Mac) / Ctrl+Click (Win/Linux) jumps to definition
- [ ] Right-click → "Go to Definition" works
- [ ] F12 key works

**Test steps:**
1. Place cursor on `add` in `let result = add(5, 10)`
2. Press F12 or Cmd+Click
3. Should jump to the `fn add(a, b)` definition

**Test code:**
```veld
fn multiply(x, y)
    x * y
end

fn calculate()
    multiply(3, 4)
    # Cmd+Click on 'multiply' above
end
```

**Success:** Cursor jumps to function definition  
**Failure:** Nothing happens or "No definition found"

---

### ✅ 6. Autocomplete / IntelliSense

**What to check:**
- [ ] Typing shows completion suggestions
- [ ] Function names appear in suggestions
- [ ] Variable names appear in suggestions
- [ ] Suggestions update as you type

**Test steps:**
1. Type `fa` in the editor
2. Wait for completion popup (or press Ctrl+Space)
3. Should suggest `factorial` (if it exists in scope)

**Test code:**
```veld
fn factorial(n)
    if n <= 1 then 1 else n * factorial(n - 1) end
end

fn fibonacci(n)
    if n <= 1 then n else fibonacci(n - 1) + fibonacci(n - 2) end
end

# Type below this line:
# Try: "fa" → should suggest "factorial"
# Try: "fib" → should suggest "fibonacci"
```

**Success:** Completion popup shows relevant suggestions  
**Failure:** No popup or empty suggestions

---

### ✅ 7. Diagnostics (Error Detection)

**What to check:**
- [ ] Undefined variables show red squiggles
- [ ] Syntax errors highlighted
- [ ] Error messages appear on hover
- [ ] Problems panel shows errors

**Test steps:**
1. Type: `let x = undefined_variable`
2. Should see red squiggle under `undefined_variable`
3. Hover to see error message

**Test code:**
```veld
# This should show an error:
let broken = some_undefined_function(1, 2)

# This should also error:
let x = unknown_var + 5
```

**Success:** Red squiggles appear, Problems panel shows errors  
**Failure:** No visual indication of errors

---

### ✅ 8. Find References

**What to check:**
- [ ] Right-click → "Find All References" works
- [ ] Shows all usages of symbol
- [ ] References panel appears

**Test steps:**
1. Right-click on function name (e.g., `add`)
2. Select "Find All References" (or Shift+F12)
3. Should show all places where `add` is called

**Test code:**
```veld
fn utility(a)
    a * 2
end

let x = utility(5)
let y = utility(10)
let z = utility(15)
# Right-click 'utility' and find references
```

**Success:** References panel shows all 3 calls  
**Failure:** "No references found"

---

### ✅ 9. Document Symbols

**What to check:**
- [ ] Cmd+Shift+O (Mac) / Ctrl+Shift+O shows symbol list
- [ ] Functions appear in list
- [ ] Variables appear in list
- [ ] Clicking symbol jumps to definition

**Test steps:**
1. Press Cmd+Shift+O (Mac) or Ctrl+Shift+O (Win/Linux)
2. Should see dropdown with all symbols in file
3. Type to filter, select to jump

**Success:** Symbol list shows all functions/variables  
**Failure:** Empty list or command doesn't work

---

### ✅ 10. Code Actions (Quick Fixes)

**What to check:**
- [ ] Light bulb appears for fixable issues
- [ ] Cmd+. (Mac) / Ctrl+. shows actions
- [ ] Actions can be applied

**Test steps:**
1. Create an error (e.g., undefined variable)
2. Click on the error
3. Press Cmd+. to see code actions
4. Check if any fixes are suggested

**Success:** Light bulb icon appears, actions available  
**Failure:** No light bulb or empty actions list

---

## Debugging Failed Tests

### Issue: Extension Not Loading

**Symptoms:**
- Language shows "Plain Text" instead of "Veld"
- No LSP messages in Output panel

**Solutions:**
1. Check file extension is `.veld`
2. Restart VSCode completely
3. Check extension is installed: `code --list-extensions | grep veld`
4. Reinstall extension

---

### Issue: LSP Server Not Starting

**Symptoms:**
- Error: "Veld LSP server not found"
- Log shows "EOF reached" immediately
- No initialization messages

**Solutions:**
1. **Check binary exists:**
   ```bash
   ls -lh target/release/veld-lsp
   ```

2. **Rebuild LSP:**
   ```bash
   cargo build --release --bin veld-lsp
   ```

3. **Open from project root:**
   ```bash
   code /Users/aidanjost/projects/veld
   ```

4. **Add to PATH:**
   ```bash
   export PATH="$PATH:/Users/aidanjost/projects/veld/target/release"
   ```

5. **Check log for errors:**
   ```bash
   cat lsp_server.log
   ```

---

### Issue: Features Not Working

**Symptoms:**
- Extension loads but no hover/completion
- LSP seems to start but features don't work

**Check these:**

1. **Output Panel:**
   - View → Output → "Veld Language Server"
   - Look for error messages

2. **Developer Console:**
   - Help → Toggle Developer Tools
   - Console tab
   - Check for JavaScript errors

3. **LSP Log:**
   ```bash
   tail -50 lsp_server.log
   ```
   - Should see method calls like "textDocument/hover"

4. **Test with simple file:**
   ```veld
   fn test()
       42
   end
   ```
   - Hover over `test`
   - Should show something

---

### Issue: Slow or Unresponsive

**Symptoms:**
- Hover takes forever
- Completion is slow
- Editor freezes

**Possible causes:**
1. Large file (>1000 lines)
2. LSP analyzing complex code
3. Multiple instances running

**Check:**
```bash
# See if multiple LSPs running
ps aux | grep veld-lsp

# Check log for errors
tail -f lsp_server.log
```

---

## Log File Locations

### LSP Server Log
```bash
# In project root
cat lsp_server.log
tail -f lsp_server.log  # Watch in real-time
```

### VSCode Extension Log
- View → Output → "Veld Language Server"
- Or: Help → Toggle Developer Tools → Console

### VSCode Error Log
- Help → Toggle Developer Tools → Console tab
- Look for red errors

---

## Testing Workflow

### Recommended Order

1. **Basic Tests:**
   - [ ] Extension activates
   - [ ] Syntax highlighting works
   - [ ] LSP server starts

2. **Core Features:**
   - [ ] Hover information
   - [ ] Go to definition
   - [ ] Autocomplete

3. **Advanced Features:**
   - [ ] Diagnostics
   - [ ] Find references
   - [ ] Document symbols

4. **Edge Cases:**
   - [ ] Large files
   - [ ] Multiple files
   - [ ] Complex nested functions

---

## Reporting Issues

When reporting issues, include:

1. **What doesn't work:**
   - Specific feature (hover, completion, etc.)
   - Expected behavior
   - Actual behavior

2. **Environment:**
   - VSCode version: `code --version`
   - macOS version
   - Extension version

3. **Logs:**
   - LSP log: `cat lsp_server.log`
   - Output panel contents
   - Developer console errors

4. **Reproduction steps:**
   - Minimal code example
   - Steps to trigger issue

---

## Success Criteria

The extension is working correctly when:

- ✅ All 10 feature tests pass
- ✅ No error messages in logs
- ✅ LSP server stays running
- ✅ Features respond quickly (<1 second)
- ✅ No crashes or freezes

---

## Quick Commands Reference

```bash
# Install extension
code --install-extension extensions/vscode-veld/veld-language-0.0.5.vsix

# Uninstall extension
code --uninstall-extension veld-dev.veld-language

# List installed extensions
code --list-extensions | grep veld

# Watch LSP log
tail -f lsp_server.log

# Rebuild LSP
cargo build --release --bin veld-lsp

# Test LSP manually
echo 'Content-Length: 100\r\n\r\n{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ./target/release/veld-lsp
```

---

## Next Steps After Testing

1. **If everything works:**
   - Document working features
   - Create demo videos/GIFs
   - Prepare for marketplace publish

2. **If some features don't work:**
   - Note which ones fail
   - Collect logs and error messages
   - Report for fixing

3. **If nothing works:**
   - Check basic setup (binary exists, PATH set)
   - Try development mode (F5 in extension source)
   - Check for environment-specific issues

---

**Ready to test!** Start with the Quick Start section and work through the checklist. Good luck! 🚀