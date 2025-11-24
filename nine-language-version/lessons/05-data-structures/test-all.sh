#!/bin/bash
# Test script for Lesson 5: Data Structures
# Runs all data structure examples in different languages

echo "======================================"
echo "Testing Lesson 5: Data Structures"
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
run_test "Python" "python3 data_structures.py"

# JavaScript
run_test "JavaScript" "node data_structures.js"

# C (compile and run)
run_test "C (compile)" "gcc -std=c99 data_structures.c -o data_structures_c"
if [ -f data_structures_c ]; then
    run_test "C (run)" "./data_structures_c"
    rm -f data_structures_c
fi

# Java (compile and run)
run_test "Java (compile)" "javac DataStructures.java"
if [ -f DataStructures.class ]; then
    run_test "Java (run)" "java DataStructures"
    rm -f DataStructures.class
fi

# Ruby
run_test "Ruby" "ruby data_structures.rb"

# Haskell (compile and run)
run_test "Haskell (compile)" "ghc -o data_structures_hs data_structures.hs 2>/dev/null"
if [ -f data_structures_hs ]; then
    run_test "Haskell (run)" "./data_structures_hs"
    rm -f data_structures_hs data_structures.hi data_structures.o Main.hi Main.o
fi

# Racket
run_test "Racket" "racket data_structures.rkt"

# Prolog
run_test "Prolog" "swipl -q -t main -s data_structures.pl"

# Rust (compile and run)
run_test "Rust (compile)" "rustc data_structures.rs -o data_structures_rust 2>/dev/null"
if [ -f data_structures_rust ]; then
    run_test "Rust (run)" "./data_structures_rust"
    rm -f data_structures_rust
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
