#!/bin/bash
# Comprehensive Bytecode Compilation Test Suite
# Tests all fixed features including recursive functions, closures, and more

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VELD_BIN="$SCRIPT_DIR/target/debug/veld"
TEST_DIR="/tmp/veld_bytecode_complete_tests_$$"
PASSED=0
FAILED=0
TOTAL=0

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  Veld Bytecode Compilation - Comprehensive Test Suite     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}\n"

# Build veld if needed
if [ ! -f "$VELD_BIN" ]; then
    echo -e "${YELLOW}Building veld...${NC}"
    cd "$SCRIPT_DIR"
    cargo build --bin veld
fi

# Create test directory
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

# Function to run a test
run_test() {
    local test_name="$1"
    local source_code="$2"
    local expected_output="$3"
    local category="${4:-General}"

    TOTAL=$((TOTAL + 1))
    local source_file="$TEST_DIR/${test_name}.veld"
    local bytecode_file="$TEST_DIR/${test_name}.veldc"

    echo -e "${YELLOW}[$category] $test_name${NC}"

    # Write source file
    echo "$source_code" > "$source_file"

    # Compile to bytecode
    if ! "$VELD_BIN" build "$source_file" "$bytecode_file" 2>&1 | grep -q "Success"; then
        echo -e "${RED}  ✗ FAILED: Compilation failed${NC}"
        cat "$source_file"
        "$VELD_BIN" build "$source_file" "$bytecode_file" 2>&1
        FAILED=$((FAILED + 1))
        echo ""
        return 1
    fi

    # Run bytecode
    local output=$("$VELD_BIN" "$bytecode_file" 2>&1)
    local result=$(echo "$output" | grep "Program result" | grep -oE 'input: "[^"]*"' | sed 's/input: "\(.*\)"/\1/')

    # If that didn't work, try without quotes
    if [ -z "$result" ]; then
        result=$(echo "$output" | grep "Program result" | sed 's/.*Program result: [^"]*"\([^"]*\)".*/\1/')
    fi

    # If still no result, try extracting just the number/value
    if [ -z "$result" ]; then
        result=$(echo "$output" | grep "Program result" | grep -oE '[0-9]+' | tail -1)
    fi

    # Compare output
    if [ "$result" = "$expected_output" ]; then
        echo -e "${GREEN}  ✓ PASSED: $result${NC}\n"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}  ✗ FAILED: Expected '$expected_output', got '$result'${NC}"
        echo -e "${RED}     Full output: $output${NC}\n"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

echo -e "${BLUE}═══ Basic Arithmetic ═══${NC}\n"

run_test "add" \
"5 + 10" \
"15" \
"Arithmetic"

run_test "multiply" \
"6 * 7" \
"42" \
"Arithmetic"

run_test "complex_expr" \
"(10 + 5) * 2 - 8 / 4" \
"28" \
"Arithmetic"

run_test "negative" \
"-5 + 10" \
"5" \
"Arithmetic"

echo -e "${BLUE}═══ Variables ═══${NC}\n"

run_test "simple_var" \
"let x = 10
x" \
"10" \
"Variables"

run_test "multiple_vars" \
"let x = 10
let y = 20
x + y" \
"30" \
"Variables"

run_test "mutation" \
"let mut x = 10
x = x + 5
x = x * 2
x" \
"30" \
"Variables"

run_test "shadowing" \
"let x = 10
let x = x + 5
let x = x * 2
x" \
"30" \
"Variables"

echo -e "${BLUE}═══ Functions ═══${NC}\n"

run_test "simple_function" \
"fn add(a, b)
    a + b
end
add(15, 27)" \
"42" \
"Functions"

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
"27" \
"Functions"

run_test "nested_calls" \
"fn double(x)
    x * 2
end

fn square(x)
    x * x
end

square(double(5))" \
"100" \
"Functions"

run_test "function_with_locals" \
"fn compute()
    let x = 10
    let y = 20
    let z = x + y
    z * 2
end
compute()" \
"60" \
"Functions"

echo -e "${BLUE}═══ Conditionals ═══${NC}\n"

run_test "if_then" \
"let x = 10
if x > 5 then
    100
else
    200
end" \
"100" \
"Conditionals"

run_test "if_else" \
"let x = 3
if x > 5 then
    100
else
    200
end" \
"200" \
"Conditionals"

run_test "nested_if" \
"let x = 15
if x > 10 then
    if x > 20 then
        1
    else
        2
    end
else
    3
end" \
"2" \
"Conditionals"

echo -e "${BLUE}═══ Recursive Functions (FIXED!) ═══${NC}\n"

run_test "factorial" \
"fn factorial(n)
    if n <= 1 then
        1
    else
        n * factorial(n - 1)
    end
end

factorial(5)" \
"120" \
"Recursion"

run_test "fibonacci" \
"fn fib(n)
    if n <= 1 then
        n
    else
        fib(n - 1) + fib(n - 2)
    end
end

fib(10)" \
"55" \
"Recursion"

run_test "sum_range" \
"fn sum_range(n)
    if n <= 0 then
        0
    else
        n + sum_range(n - 1)
    end
end

sum_range(100)" \
"5050" \
"Recursion"

run_test "countdown" \
"fn countdown(n)
    if n <= 0 then
        0
    else
        countdown(n - 1)
    end
end

countdown(50)" \
"0" \
"Recursion"

run_test "power" \
"fn power(base, exp)
    if exp <= 0 then
        1
    else
        base * power(base, exp - 1)
    end
end

power(2, 10)" \
"1024" \
"Recursion"

echo -e "${BLUE}═══ Closures (FIXED!) ═══${NC}\n"

run_test "simple_closure" \
"fn make_adder(x)
    fn add(y)
        x + y
    end
    add
end

let add5 = make_adder(5)
add5(10)" \
"15" \
"Closures"

run_test "closure_multiply" \
"fn make_multiplier(x)
    fn multiply(y)
        x * y
    end
    multiply
end

let times3 = make_multiplier(3)
times3(7)" \
"21" \
"Closures"

run_test "nested_closure" \
"fn outer(a)
    fn middle(b)
        fn inner(c)
            a + b + c
        end
        inner
    end
    middle
end

let f = outer(1)
let g = f(10)
g(100)" \
"111" \
"Closures"

echo -e "${BLUE}═══ Comparisons & Logic ═══${NC}\n"

run_test "less_than" \
"let a = 5
let b = 10
if a < b then 1 else 0 end" \
"1" \
"Comparisons"

run_test "greater_than" \
"let a = 10
let b = 5
if a > b then 1 else 0 end" \
"1" \
"Comparisons"

run_test "equals" \
"let a = 10
let b = 10
if a == b then 1 else 0 end" \
"1" \
"Comparisons"

run_test "not_equals" \
"let a = 10
let b = 5
if a != b then 1 else 0 end" \
"1" \
"Comparisons"

echo -e "${BLUE}═══ Complex Programs ═══${NC}\n"

run_test "ackermann_small" \
"fn ack(m, n)
    if m == 0 then
        n + 1
    else
        if n == 0 then
            ack(m - 1, 1)
        else
            ack(m - 1, ack(m, n - 1))
        end
    end
end

ack(2, 2)" \
"7" \
"Complex"

run_test "gcd" \
"fn gcd(a, b)
    if b == 0 then
        a
    else
        gcd(b, a % b)
    end
end

gcd(48, 18)" \
"6" \
"Complex"

run_test "multiple_recursive" \
"fn even(n)
    if n == 0 then
        1
    else
        odd(n - 1)
    end
end

fn odd(n)
    if n == 0 then
        0
    else
        even(n - 1)
    end
end

even(10)" \
"1" \
"Complex"

# Summary
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                      Test Summary                          ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}\n"

echo -e "Total tests:  ${BLUE}$TOTAL${NC}"
echo -e "Passed:       ${GREEN}$PASSED${NC}"
echo -e "Failed:       ${RED}$FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                    ALL TESTS PASSED! 🎉                    ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════╝${NC}\n"

    # Calculate some stats
    echo -e "${BLUE}Statistics:${NC}"
    total_source_size=0
    total_bytecode_size=0
    count=0

    for veld_file in "$TEST_DIR"/*.veld; do
        if [ -f "$veld_file" ]; then
            veldc_file="${veld_file%.veld}.veldc"
            if [ -f "$veldc_file" ]; then
                source_size=$(stat -f%z "$veld_file" 2>/dev/null || stat -c%s "$veld_file" 2>/dev/null)
                bytecode_size=$(stat -f%z "$veldc_file" 2>/dev/null || stat -c%s "$veldc_file" 2>/dev/null)
                total_source_size=$((total_source_size + source_size))
                total_bytecode_size=$((total_bytecode_size + bytecode_size))
                count=$((count + 1))
            fi
        fi
    done

    if [ $count -gt 0 ]; then
        avg_source=$((total_source_size / count))
        avg_bytecode=$((total_bytecode_size / count))
        echo "  Average source size:   $avg_source bytes"
        echo "  Average bytecode size: $avg_bytecode bytes"
        echo "  Total tests compiled:  $count"
    fi
else
    echo -e "\n${RED}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║                  SOME TESTS FAILED                         ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════════════════════════╝${NC}\n"
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
    exit 0
else
    exit 1
fi
