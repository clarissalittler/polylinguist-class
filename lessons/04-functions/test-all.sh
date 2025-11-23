#!/bin/bash
# Test script for Lesson 4: Functions
# Runs all function examples in different languages

echo "======================================"
echo "Testing Lesson 4: Functions"
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
run_test "Python" "python3 functions.py"

# JavaScript
run_test "JavaScript" "node functions.js"

# C (compile and run)
run_test "C (compile)" "gcc -std=c99 functions.c -o functions_c"
if [ -f functions_c ]; then
    run_test "C (run)" "./functions_c"
    rm -f functions_c
fi

# Java (compile and run)
run_test "Java (compile)" "javac Functions.java"
if [ -f Functions.class ]; then
    run_test "Java (run)" "java Functions"
    rm -f Functions.class Functions\$DivisionResult.class
fi

# Ruby
run_test "Ruby" "ruby functions.rb"

# Haskell (compile and run)
run_test "Haskell (compile)" "ghc -o functions_hs functions.hs 2>/dev/null"
if [ -f functions_hs ]; then
    run_test "Haskell (run)" "./functions_hs"
    rm -f functions_hs functions.hi functions.o Main.hi Main.o
fi

# Racket
run_test "Racket" "racket functions.rkt"

# Prolog
run_test "Prolog" "swipl -q -t main -s functions.pl"

# Rust (compile and run)
run_test "Rust (compile)" "rustc functions.rs -o functions_rust 2>/dev/null"
if [ -f functions_rust ]; then
    run_test "Rust (run)" "./functions_rust"
    rm -f functions_rust
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
