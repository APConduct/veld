#!/bin/bash
# Quick Bytecode Compilation Test
# Verifies that Veld bytecode compilation is working

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=== Quick Bytecode Compilation Test ===${NC}\n"

# Build veld if needed
if [ ! -f target/debug/veld ]; then
    echo "Building veld..."
    cargo build --bin veld
fi

VELD="./target/debug/veld"
TEST_DIR="/tmp/veld_quick_test_$$"
mkdir -p "$TEST_DIR"

# Test 1: Simple arithmetic
echo -e "${BLUE}Test 1: Simple Arithmetic${NC}"
cat > "$TEST_DIR/test1.veld" << 'EOF'
5 + 10 * 2
EOF

$VELD build "$TEST_DIR/test1.veld" "$TEST_DIR/test1.veldc" 2>&1 | grep -q "Success"
RESULT=$($VELD "$TEST_DIR/test1.veldc" 2>&1 | grep "Program result" | grep -oE "[0-9]+")

if [ "$RESULT" = "25" ]; then
    echo -e "${GREEN}✓ PASSED: Got expected result: 25${NC}\n"
else
    echo -e "${RED}✗ FAILED: Expected 25, got $RESULT${NC}\n"
    exit 1
fi

# Test 2: Functions
echo -e "${BLUE}Test 2: Functions${NC}"
cat > "$TEST_DIR/test2.veld" << 'EOF'
fn add(a, b)
    a + b
end
add(15, 27)
EOF

$VELD build "$TEST_DIR/test2.veld" "$TEST_DIR/test2.veldc" 2>&1 | grep -q "Success"
RESULT=$($VELD "$TEST_DIR/test2.veldc" 2>&1 | grep "Program result" | grep -oE "[0-9]+")

if [ "$RESULT" = "42" ]; then
    echo -e "${GREEN}✓ PASSED: Got expected result: 42${NC}\n"
else
    echo -e "${RED}✗ FAILED: Expected 42, got $RESULT${NC}\n"
    exit 1
fi

# Test 3: Multiple functions
echo -e "${BLUE}Test 3: Multiple Functions${NC}"
cat > "$TEST_DIR/test3.veld" << 'EOF'
fn add(a, b)
    a + b
end

fn multiply(x, y)
    x * y
end

let sum = add(5, 10)
let product = multiply(3, 4)
sum + product
EOF

$VELD build "$TEST_DIR/test3.veld" "$TEST_DIR/test3.veldc" 2>&1 | grep -q "Success"
RESULT=$($VELD "$TEST_DIR/test3.veldc" 2>&1 | grep "Program result" | grep -oE "[0-9]+")

if [ "$RESULT" = "27" ]; then
    echo -e "${GREEN}✓ PASSED: Got expected result: 27${NC}\n"
else
    echo -e "${RED}✗ FAILED: Expected 27, got $RESULT${NC}\n"
    exit 1
fi

# Test 4: Mutation
echo -e "${BLUE}Test 4: Mutable Variables${NC}"
cat > "$TEST_DIR/test4.veld" << 'EOF'
let mut counter = 10
counter = counter + 5
counter = counter * 2
counter
EOF

$VELD build "$TEST_DIR/test4.veld" "$TEST_DIR/test4.veldc" 2>&1 | grep -q "Success"
RESULT=$($VELD "$TEST_DIR/test4.veldc" 2>&1 | grep "Program result" | grep -oE "[0-9]+")

if [ "$RESULT" = "30" ]; then
    echo -e "${GREEN}✓ PASSED: Got expected result: 30${NC}\n"
else
    echo -e "${RED}✗ FAILED: Expected 30, got $RESULT${NC}\n"
    exit 1
fi

# Test 5: Disassembly
echo -e "${BLUE}Test 5: Disassembly${NC}"
DISASM=$($VELD disasm "$TEST_DIR/test1.veldc" 2>&1)

if echo "$DISASM" | grep -q "BYTECODE DISASSEMBLY"; then
    echo -e "${GREEN}✓ PASSED: Disassembly works${NC}\n"
else
    echo -e "${RED}✗ FAILED: Disassembly failed${NC}\n"
    exit 1
fi

# File size comparison
echo -e "${BLUE}=== File Size Comparison ===${NC}"
SOURCE_SIZE=$(stat -f%z "$TEST_DIR/test3.veld" 2>/dev/null || stat -c%s "$TEST_DIR/test3.veld" 2>/dev/null)
BYTECODE_SIZE=$(stat -f%z "$TEST_DIR/test3.veldc" 2>/dev/null || stat -c%s "$TEST_DIR/test3.veldc" 2>/dev/null)
echo "Source:   $SOURCE_SIZE bytes"
echo "Bytecode: $BYTECODE_SIZE bytes"

# Cleanup
rm -rf "$TEST_DIR"

echo -e "\n${GREEN}=== All Tests Passed! ===${NC}"
echo -e "${GREEN}✓ Bytecode compilation is working correctly${NC}"
echo -e "\n${BLUE}Quick Start:${NC}"
echo "  veld build program.veld    # Compile to bytecode"
echo "  veld program.veldc         # Run bytecode"
echo "  veld disasm program.veldc  # Inspect bytecode"
echo ""
echo "See BYTECODE_COMPILATION_GUIDE.md for full documentation."
