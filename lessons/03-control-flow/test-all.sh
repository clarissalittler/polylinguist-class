#!/bin/bash
# Test script for Lesson 3: Control Flow
# Runs all control flow examples in different languages

echo "======================================"
echo "Testing Lesson 3: Control Flow"
echo "======================================"
echo

# Track results
PASSED=0
FAILED=0

# Helper function to run a test
run_test() {
    local name="$1"
    local command="$2"

    echo "Testing $name..."
    if eval "$command" > /dev/null 2>&1; then
        echo "✓ $name passed"
        ((PASSED++))
    else
        echo "✗ $name failed"
        ((FAILED++))
    fi
    echo
}

# Python
run_test "Python" "python3 control_flow.py"

# JavaScript
run_test "JavaScript" "node control_flow.js"

# C (compile and run)
run_test "C (compile)" "gcc -std=c99 control_flow.c -o control_flow_c"
if [ -f control_flow_c ]; then
    run_test "C (run)" "./control_flow_c"
    rm -f control_flow_c
fi

# Java (compile and run)
run_test "Java (compile)" "javac ControlFlow.java"
if [ -f ControlFlow.class ]; then
    run_test "Java (run)" "java ControlFlow"
    rm -f ControlFlow.class
fi

# Ruby
run_test "Ruby" "ruby control_flow.rb"

# Haskell (compile and run)
run_test "Haskell (compile)" "ghc -o control_flow_hs control_flow.hs 2>/dev/null"
if [ -f control_flow_hs ]; then
    run_test "Haskell (run)" "./control_flow_hs"
    rm -f control_flow_hs control_flow.hi control_flow.o
fi

# Racket
run_test "Racket" "racket control_flow.rkt"

# Prolog
run_test "Prolog" "swipl -q -t main -s control_flow.pl"

# Rust (compile and run)
run_test "Rust (compile)" "rustc control_flow.rs -o control_flow_rust 2>/dev/null"
if [ -f control_flow_rust ]; then
    run_test "Rust (run)" "./control_flow_rust"
    rm -f control_flow_rust
fi

# Summary
echo "======================================"
echo "Test Summary"
echo "======================================"
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo

if [ $FAILED -eq 0 ]; then
    echo "All tests passed! ✓"
    exit 0
else
    echo "Some tests failed. ✗"
    exit 1
fi
