#!/bin/bash

# Generate comprehensive LSP test report
# Usage: ./generate_test_report.sh

OUTPUT_FILE="LSP_TEST_REPORT_$(date +%Y%m%d_%H%M%S).md"

cat > "$OUTPUT_FILE" << 'EOF'
# Veld LSP Test Report

**Date:** $(date)
**Tester:**
**VSCode Version:**
**Extension Version:** 0.0.5

---

## Environment

- **OS:** $(uname -s) $(uname -r)
- **Architecture:** $(uname -m)
- **LSP Binary:**
  ```
  $(ls -lh target/release/veld-lsp 2>/dev/null || echo "Not found")
  ```
- **Extension Status:**
  ```
  $(code --list-extensions | grep -i veld || echo "Not installed")
  ```

---

## Test Results Summary

### ✅ Working Features
- [ ] Extension Activation
- [ ] LSP Connection
- [ ] Syntax Highlighting
- [ ] Hover Information
- [ ] Go to Definition
- [ ] Autocompletion
- [ ] Diagnostics (Error Detection)
- [ ] Document Symbols (Outline)
- [ ] Workspace Symbols
- [ ] Code Actions
- [ ] Folding Ranges

### ❌ Failing Features
- [ ] None / List below

### ⚠️ Partially Working
- [ ] None / List below

---

## Detailed Test Results

### 1. Extension Activation & Connection

**Test:** Extension loads and connects to LSP server

**Steps:**
1. Open VSCode with project folder
2. Open `test_lsp_features.veld`
3. Check Output panel (View → Output → Veld Language Server)
4. Check `lsp_server.log`

**Result:** ☐ Pass ☐ Fail

**Notes:**


**Log Excerpt:**
```
(paste from lsp_server.log)
```

---

### 2. Hover Information

**Test:** Hovering shows type/signature info

**Steps:**
1. Hover over function name `add` on line 8
2. Hover over variable `number` on line 30
3. Hover over function call inside `calculate`

**Result:** ☐ Pass ☐ Fail ☐ Partial

**What works:**
-

**What doesn't work:**
-

**Log Excerpt:**
```
(paste hover requests from log)
```

**Screenshot:**
(optional)

---

### 3. Go to Definition

**Test:** Jump to symbol definitions

**Steps:**
1. Cmd+Click on `add` inside `calculate` function
2. Cmd+Click on `adder` inside `make_adder`
3. Cmd+Click on recursive call to `factorial`

**Result:** ☐ Pass ☐ Fail ☐ Partial

**What works:**
-

**What doesn't work:**
-

**Log Excerpt:**
```
(paste definition requests from log)
```

---

### 4. Autocompletion

**Test:** Completion suggests available symbols

**Steps:**
1. Type `let x = fa` at end of file
2. Check if dropdown shows `factorial`, `fibonacci`
3. Type `make_` and check suggestions

**Result:** ☐ Pass ☐ Fail ☐ Partial

**What works:**
-

**What doesn't work:**
-

**Log Excerpt:**
```
(paste completion requests from log)
```

**Screenshot:**
(optional)

---

### 5. Diagnostics (Error Detection)

**Test:** LSP detects errors and shows diagnostics

**Steps:**
1. Add line: `let err = undefined_function(1, 2)`
2. Check for red squiggle
3. Hover over error to see message
4. Delete line and verify squiggle disappears

**Result:** ☐ Pass ☐ Fail ☐ Partial

**What works:**
-

**What doesn't work:**
-

**Log Excerpt:**
```
(paste diagnostic notifications from log)
```

**Screenshot:**
(optional)

---

### 6. Document Symbols (Outline)

**Test:** Outline view shows file structure

**Steps:**
1. Open Outline panel (Cmd+Shift+O)
2. Check if functions are listed
3. Click on a function to navigate

**Result:** ☐ Pass ☐ Fail

**What works:**
-

**What doesn't work:**
-

**Log Excerpt:**
```
(paste documentSymbol requests from log)
```

---

### 7. Workspace Symbols

**Test:** Global symbol search works

**Steps:**
1. Press Cmd+T
2. Type "factorial"
3. Check if it appears with file location

**Result:** ☐ Pass ☐ Fail

**Notes:**


---

### 8. Performance

**Test:** LSP handles files without lag

**Steps:**
1. Open test file with ~150 lines
2. Type and observe responsiveness
3. Check hover/completion latency

**Result:** ☐ Fast ☐ Acceptable ☐ Slow

**Notes:**


---

## Issues Found

### Issue 1: [Title]

**Severity:** ☐ Critical ☐ Major ☐ Minor

**Description:**


**Steps to Reproduce:**
1.
2.
3.

**Expected Behavior:**


**Actual Behavior:**


**Logs:**
```
(paste relevant logs)
```

---

### Issue 2: [Title]

(Repeat above format)

---

## Full LSP Server Log

```
(paste entire lsp_server.log or attach as separate file)
```

---

## VSCode Output Panel Log

```
(paste from Output → Veld Language Server)
```

---

## Recommendations

### High Priority
-

### Medium Priority
-

### Low Priority
-

---

## Additional Notes


---

## Screenshots

(Attach any relevant screenshots)

1.
2.
3.

---

## Conclusion

Overall assessment: ☐ Ready for use ☐ Needs fixes ☐ Major issues

**Summary:**


**Next Steps:**
1.
2.
3.

EOF

# Now fill in dynamic values
sed -i.bak "s|\$(date)|$(date)|g" "$OUTPUT_FILE"
sed -i.bak "s|\$(uname -s)|$(uname -s)|g" "$OUTPUT_FILE"
sed -i.bak "s|\$(uname -r)|$(uname -r)|g" "$OUTPUT_FILE"
sed -i.bak "s|\$(uname -m)|$(uname -m)|g" "$OUTPUT_FILE"

# Try to get VSCode version
if command -v code &> /dev/null; then
    VSCODE_VERSION=$(code --version 2>/dev/null | head -n1)
    sed -i.bak "s|VSCode Version:** |VSCode Version:** $VSCODE_VERSION|g" "$OUTPUT_FILE"
fi

# Check for LSP binary
if [ -f "target/release/veld-lsp" ]; then
    LSP_INFO=$(ls -lh target/release/veld-lsp)
    sed -i.bak "s|\$(ls -lh target/release/veld-lsp.*)|$LSP_INFO|g" "$OUTPUT_FILE"
fi

# Check for extension
if command -v code &> /dev/null; then
    EXT_CHECK=$(code --list-extensions 2>/dev/null | grep -i veld || echo "Extension not found in installed list")
    sed -i.bak "s|\$(code --list-extensions.*)|$EXT_CHECK|g" "$OUTPUT_FILE"
fi

# Remove backup file
rm -f "$OUTPUT_FILE.bak"

echo "=========================================="
echo "Test Report Template Generated"
echo "=========================================="
echo ""
echo "File: $OUTPUT_FILE"
echo ""
echo "Please fill in the test results and notes."
echo "To view: cat $OUTPUT_FILE"
echo "To edit: code $OUTPUT_FILE"
echo ""
echo "After testing, this report will help track:"
echo "  - What features are working"
echo "  - What needs to be fixed"
echo "  - Priority of issues"
echo ""
echo "Tip: Keep lsp_server.log and VSCode Output panel"
echo "     open while testing to capture logs easily."
echo ""
