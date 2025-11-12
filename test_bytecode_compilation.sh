#!/bin/bash
# Comprehensive Bytecode Compilation Testing Script
# Tests various Veld language features in bytecode compilation

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VELD_BIN="$SCRIPT_DIR/target/debug/veld"
TEST_DIR="/tmp/veld_bytecode_tests"
PASSED=0
FAILED=0

# Build veld if needed
if [ ! -f "$VELD_BIN" ]; then
    echo -e "${YELLOW}Building veld...${NC}"
    cd "$SCRIPT_DIR"
    cargo build --bin veld
fi

# Create test directory
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

echo -e "${BLUE}=== Veld Bytecode Compilation Tests ===${NC}\n"

# Function to run a test
run_test() {
    local test_name="$1"
    local source_code="$2"
    local expected_output="$3"

    local source_file="$TEST_DIR/${test_name}.veld"
    local bytecode_file="$TEST_DIR/${test_name}.veldc"

    echo -e "${YELLOW}Test: $test_name${NC}"

    # Write source file
    echo "$source_code" > "$source_file"

    # Compile to bytecode
    if ! "$VELD_BIN" build "$source_file" "$bytecode_file" 2>&1 | grep -q "Success"; then
        echo -e "${RED}✗ FAILED: Compilation failed${NC}\n"
        FAILED=$((FAILED + 1))
        return 1
    fi

    # Check bytecode file exists
    if [ ! -f "$bytecode_file" ]; then
        echo -e "${RED}✗ FAILED: Bytecode file not created${NC}\n"
        FAILED=$((FAILED + 1))
        return 1
    fi

    # Run bytecode
    local output=$("$VELD_BIN" "$bytecode_file" 2>&1 | grep "Program result" | sed 's/.*Program result: [^"]*"\([^"]*\)".*/\1/')

    if [ -z "$output" ]; then
        # Try without quotes
        output=$("$VELD_BIN" "$bytecode_file" 2>&1 | grep "Program result" | awk -F': ' '{print $NF}' | sed 's/ColoredString.*//' | xargs)
    fi

    # Compare output
    if [ "$output" = "$expected_output" ]; then
        echo -e "${GREEN}✓ PASSED: Output matches expected ($output)${NC}\n"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ FAILED: Expected '$expected_output', got '$output'${NC}\n"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Test 1: Simple arithmetic
run_test "arithmetic" \
"5 + 10 * 2" \
"25"

# Test 2: Variable declaration
run_test "variables" \
"let x = 10
let y = 20
x + y" \
"30"

# Test 3: Function definition and call
run_test "functions" \
"fn add(a, b)
    a + b
end
add(15, 27)" \
"42"

# Test 4: Multiple functions
run_test "multiple_functions" \
"fn add(a, b)
    a + b
end

fn multiply(x, y)
    x * y
end

let sum = add(5, 10)
let product = multiply(3, 4)
sum + product" \
"27"

# Test 5: Nested function calls
run_test "nested_calls" \
"fn double(x)
    x * 2
end

fn square(x)
    x * x
end

square(double(5))" \
"100"

# Test 6: String literals
run_test "strings" \
'let greeting = "Hello"
greeting' \
"Hello"

# Test 7: Boolean operations
run_test "booleans" \
"let t = true
let f = false
t and not f" \
"true"

# Test 8: Conditionals
run_test "conditionals" \
"let x = 10
if x > 5 then
    100
else
    200
end" \
"100"

# Test 9: Mutation
run_test "mutation" \
"let mut counter = 10
counter = counter + 5
counter = counter * 2
counter" \
"30"

# Test 10: Comparison operators
run_test "comparisons" \
"let a = 10
let b = 20
a < b" \
"true"

# Test 11: Logical operators
run_test "logical_ops" \
"let x = true
let y = false
(x or y) and x" \
"true"

# Test 12: Multiple statements
run_test "multiple_statements" \
"let a = 5
let b = 10
let c = a + b
let d = c * 2
d" \
"30"

# Test 13: Function with multiple parameters
run_test "multi_param_function" \
"fn calculate(a, b, c)
    (a + b) * c
end
calculate(2, 3, 4)" \
"20"

# Test 14: Negative numbers
run_test "negative_numbers" \
"let x = -10
let y = 5
x + y" \
"-5"

# Test 15: Division
run_test "division" \
"let x = 100
let y = 4
x / y" \
"25"

# Test 16: Modulo
run_test "modulo" \
"let x = 17
let y = 5
x % y" \
"2"

# Test 17: Complex expression
run_test "complex_expression" \
"(10 + 5) * 2 - 8 / 4" \
"28"

# Test 18: Function returning function result
run_test "function_composition" \
"fn add_one(x)
    x + 1
end

fn times_two(x)
    x * 2
end

times_two(add_one(10))" \
"22"

# Test 19: Local variables in function
run_test "local_variables" \
"fn compute()
    let x = 10
    let y = 20
    let z = x + y
    z * 2
end
compute()" \
"60"

# Test 20: Shadowing
run_test "shadowing" \
"let x = 10
let x = x + 5
let x = x * 2
x" \
"30"

# Summary
echo -e "${BLUE}=== Test Summary ===${NC}"
echo -e "Total tests: $((PASSED + FAILED))"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"

# Check file sizes
echo -e "\n${BLUE}=== File Size Comparison ===${NC}"
if [ $PASSED -gt 0 ]; then
    total_source_size=0
    total_bytecode_size=0

    for veld_file in "$TEST_DIR"/*.veld; do
        if [ -f "$veld_file" ]; then
            veldc_file="${veld_file%.veld}.veldc"
            if [ -f "$veldc_file" ]; then
                source_size=$(stat -f%z "$veld_file" 2>/dev/null || stat -c%s "$veld_file" 2>/dev/null)
                bytecode_size=$(stat -f%z "$veldc_file" 2>/dev/null || stat -c%s "$veldc_file" 2>/dev/null)
                total_source_size=$((total_source_size + source_size))
                total_bytecode_size=$((total_bytecode_size + bytecode_size))
            fi
        fi
    done

    if [ $total_source_size -gt 0 ]; then
        echo "Total source size: $total_source_size bytes"
        echo "Total bytecode size: $total_bytecode_size bytes"
        reduction=$((100 - (total_bytecode_size * 100 / total_source_size)))
        if [ $reduction -gt 0 ]; then
            echo -e "${GREEN}Space savings: $reduction%${NC}"
        else
            overhead=$((total_bytecode_size * 100 / total_source_size - 100))
            echo -e "${YELLOW}Bytecode overhead: +$overhead%${NC}"
        fi
    fi
fi

# Test disassembly on one file
echo -e "\n${BLUE}=== Sample Disassembly ===${NC}"
if [ -f "$TEST_DIR/functions.veldc" ]; then
    echo "Disassembling functions.veldc:"
    "$VELD_BIN" disasm "$TEST_DIR/functions.veldc" 2>&1 | head -30
fi

# Cleanup option
echo -e "\n${YELLOW}Test files are in: $TEST_DIR${NC}"
read -p "Delete test files? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$TEST_DIR"
    echo "Test files deleted."
fi

# Exit with appropriate code
if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi
