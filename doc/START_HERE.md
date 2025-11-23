# 🚀 Start Here: Testing Veld VSCode Extension

Welcome! This guide will get you testing the Veld Language Server integration in VSCode in **under 5 minutes**.

---

## ✅ Prerequisites (Already Done!)

- ✅ VSCode extension installed
- ✅ LSP server binary built (`target/release/veld-lsp`)
- ✅ Test file ready (`test_lsp_features.veld`)

---

## 🎯 Quick Start (2 minutes)

### Step 1: Open VSCode with the Project

VSCode is already open with the project. If not:

```bash
cd /Users/aidanjost/projects/veld
code .
```

**⚠️ Important:** Open the entire project folder, not just a single file!

### Step 2: Monitor the LSP

Open a terminal and run:

```bash
./monitor_lsp.sh
```

This shows real-time LSP activity with color-coded messages.

### Step 3: Open the Test File

In VSCode, open: `test_lsp_features.veld`

You should see:
- ✅ Syntax highlighting (keywords in color)
- ✅ Output panel shows "Veld Language Server" (View → Output)
- ✅ Log shows initialization messages

### Step 4: Run Quick Tests

Try these 5 quick tests:

| # | Test | Action | Expected Result |
|---|------|--------|-----------------|
| 1 | **Hover** | Hover mouse over `add` on line 8 | Tooltip appears with info |
| 2 | **Go-to-Def** | `Cmd+Click` on `add` in line 15 | Jumps to line 8 |
| 3 | **Complete** | Type `let x = fa` at end of file | Shows `factorial`, `fibonacci` |
| 4 | **Errors** | Add line: `let err = undefined()` | Red squiggle appears |
| 5 | **Symbols** | Press `Cmd+Shift+O` | Shows function list |

---

## 📊 Results

After testing, check one of these:

- ✅ **Everything works!** → See "What's Next" below
- ⚠️ **Some features work** → Fill out the test report (see below)
- ❌ **Nothing works** → Check "Troubleshooting" below

---

## 📝 Generate Test Report

To create a detailed report:

```bash
./generate_test_report.sh
```

This creates a markdown file with a comprehensive testing checklist.

Fill it out and share the results!

---

## 🔧 Troubleshooting

### Issue: LSP not starting

**Check:**
```bash
# Is the binary there?
ls -lh target/release/veld-lsp

# Rebuild if needed
cargo build --release --bin veld-lsp
```

### Issue: "EOF reached, shutting down"

**Fix:** You need the latest build with the stdin fix.

```bash
cargo build --release --bin veld-lsp
```

Then reload VSCode: `Cmd+Shift+P` → "Developer: Reload Window"

### Issue: No hover/completion

**Check:**
1. Is the file saved? (LSP needs saved content)
2. Check Output panel for errors
3. Look at `lsp_server.log` for request/response activity
4. Check if LSP advertised capabilities in initialization

### Issue: Extension not active

**Check:**
```
Cmd+Shift+P → "Developer: Show Running Extensions"
```

Look for "Veld Language" - should say "Activated"

---

## 📚 Documentation

- **Quick Checklist:** `QUICK_TEST_CHECKLIST.md` (2-minute test list)
- **Full Testing Guide:** `TESTING_VSCODE_EXTENSION.md` (comprehensive)
- **Conversation Summary:** See the attached thread for full project history

---

## 🎯 What's Next?

Based on your test results, we'll prioritize:

### If Everything Works ✅
1. **Polish**: Improve hover messages, add more completions
2. **More Features**: Signature help, refactoring, formatting
3. **Publish**: Package for VSCode Marketplace

### If Some Features Fail ⚠️
1. **Fix Position Mapping**: Make hover/definition more precise
2. **Enhance Analyzer**: Better scope analysis
3. **Add Caching**: Improve performance

### If Nothing Works ❌
1. **Debug Connection**: Check stdio communication
2. **Verify Capabilities**: Ensure LSP advertises features
3. **Test Parser**: Verify file is being parsed correctly

---

## 🛠️ Useful Commands

```bash
# Monitor LSP in real-time
./monitor_lsp.sh

# View raw log
tail -f lsp_server.log

# Generate test report
./generate_test_report.sh

# Rebuild LSP
cargo build --release --bin veld-lsp

# Reinstall extension
code --install-extension extensions/vscode-veld/veld-language-0.0.5.vsix

# Check running LSP process
ps aux | grep veld-lsp

# Kill stuck LSP
pkill veld-lsp
```

---

## 💬 Reporting Results

When reporting results, please include:

1. **What works** (checklist from quick tests above)
2. **What doesn't work** (specific features + what you expected)
3. **Logs** (excerpts from `lsp_server.log` and Output panel)
4. **Screenshots** (if applicable)

You can use the generated test report template or just describe what you found.

---

## 🎉 You're Ready!

Open VSCode, run `./monitor_lsp.sh` in a terminal, and start testing!

The LSP has already been running and handling requests successfully (check the backup log), so things should work. Now we need to verify each feature systematically.

**Good luck! 🚀**

---

## Quick Reference

| What | Where |
|------|-------|
| Test File | `test_lsp_features.veld` |
| LSP Binary | `target/release/veld-lsp` |
| LSP Log | `lsp_server.log` |
| Monitor Script | `./monitor_lsp.sh` |
| Quick Tests | `QUICK_TEST_CHECKLIST.md` |
| Full Guide | `TESTING_VSCODE_EXTENSION.md` |
| Report Generator | `./generate_test_report.sh` |

---

*Last updated: After successful extension installation and LSP build*