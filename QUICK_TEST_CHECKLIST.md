# Quick LSP Testing Checklist

## Setup (Do Once)
- [x] Extension installed: `code --install-extension extensions/vscode-veld/veld-language-0.0.5.vsix`
- [x] LSP built: `cargo build --release --bin veld-lsp`
- [ ] VSCode opened with project folder: `code .`
- [ ] Test file open: `test_lsp_features.veld`

## Monitoring
In a separate terminal, run:
```bash
./monitor_lsp.sh
# or
tail -f lsp_server.log
```

In VSCode:
- Output panel: `View → Output`, select "Veld Language Server"
- DevTools: `Help → Toggle Developer Tools` → Console tab

---

## Quick Tests (2 minutes)

### 1. Connection ✓
- [ ] Open `test_lsp_features.veld`
- [ ] Check Output panel shows LSP starting
- [ ] Check `lsp_server.log` has "LSP Server starting..." (no immediate EOF)

### 2. Hover 🔍
- [ ] Hover over `add` on line 8
- [ ] See tooltip with info
- [ ] Log shows `textDocument/hover` request

### 3. Go-to-Definition 🎯
- [ ] `Cmd+Click` on `add` in line 15 (inside `calculate`)
- [ ] Jumps to line 8 (`fn add`)
- [ ] Log shows `textDocument/definition`

### 4. Completion 💡
- [ ] At end of file, type: `let x = fa`
- [ ] Dropdown suggests `factorial`, `fibonacci`
- [ ] Log shows `textDocument/completion`

### 5. Diagnostics 🔴
- [ ] Add line: `let err = undefined_function()`
- [ ] Red squiggle appears
- [ ] Hover shows error message
- [ ] Delete line, squiggle goes away

---

## Results Template

Copy and fill this out:

```
## Test Results

Date: _______________
VSCode Version: _______________

### Working ✅
- [ ] Connection/Activation
- [ ] Syntax Highlighting
- [ ] Hover
- [ ] Go to Definition
- [ ] Completion
- [ ] Diagnostics
- [ ] Document Symbols
- [ ] Workspace Symbols

### Not Working ❌
(Describe what failed)

### Logs
(Paste relevant excerpts from lsp_server.log and Output panel)

### Screenshots
(If applicable)
```

---

## Common Issues

| Problem | Quick Fix |
|---------|-----------|
| "LSP not found" | Open project folder, not single file |
| "EOF reached" | Rebuild: `cargo build --release --bin veld-lsp` |
| No hover/completion | Check log for requests, verify capabilities |
| Extension not active | `Cmd+Shift+P` → "Developer: Show Running Extensions" |
| Stale state | Reload window: `Cmd+Shift+P` → "Developer: Reload Window" |

---

## Next Actions

After testing, report results and we'll:

1. **If mostly working**: Polish features, add more LSP capabilities
2. **If hover/definition broken**: Fix analyzer position mapping
3. **If completion broken**: Enhance symbol collection
4. **If diagnostics broken**: Debug error detection logic
5. **If performance slow**: Add caching/incremental parsing

---

## Full Testing Guide

See `TESTING_VSCODE_EXTENSION.md` for comprehensive testing instructions.