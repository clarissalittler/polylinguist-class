#!/bin/bash
# Test script for Lesson 6: Recursion

echo "========================================="
echo "Testing Recursion in All Languages"
echo "========================================="
echo ""

# Track results
PASSED=0
FAILED=0

# Python
echo "--- Testing Python ---"
if python3 recursion.py > /dev/null 2>&1; then
    echo "✓ Python recursion examples run successfully"
    PASSED=$((PASSED + 1))
else
    echo "✗ Python recursion examples failed"
    FAILED=$((FAILED + 1))
fi
echo ""

# JavaScript
echo "--- Testing JavaScript ---"
if node recursion.js > /dev/null 2>&1; then
    echo "✓ JavaScript recursion examples run successfully"
    PASSED=$((PASSED + 1))
else
    echo "✗ JavaScript recursion examples failed"
    FAILED=$((FAILED + 1))
fi
echo ""

# Haskell
echo "--- Testing Haskell ---"
if runhaskell recursion.hs > /dev/null 2>&1; then
    echo "✓ Haskell recursion examples run successfully"
    PASSED=$((PASSED + 1))
else
    echo "✗ Haskell recursion examples failed"
    FAILED=$((FAILED + 1))
fi
echo ""

# Rust
echo "--- Testing Rust ---"
if rustc recursion.rs -o recursion_rust 2>&1 | grep -q "error"; then
    echo "✗ Rust compilation failed"
    FAILED=$((FAILED + 1))
else
    if ./recursion_rust > /dev/null 2>&1; then
        echo "✓ Rust recursion examples run successfully"
        PASSED=$((PASSED + 1))
        rm -f recursion_rust
    else
        echo "✗ Rust recursion examples failed to run"
        FAILED=$((FAILED + 1))
    fi
fi
echo ""

# C
echo "--- Testing C ---"
if gcc recursion.c -o recursion_c 2>&1 | grep -q "error"; then
    echo "✗ C compilation failed"
    FAILED=$((FAILED + 1))
else
    if ./recursion_c > /dev/null 2>&1; then
        echo "✓ C recursion examples run successfully"
        PASSED=$((PASSED + 1))
        rm -f recursion_c
    else
        echo "✗ C recursion examples failed to run"
        FAILED=$((FAILED + 1))
    fi
fi
echo ""

# Java
echo "--- Testing Java ---"
if javac RecursionDemo.java 2>&1 | grep -q "error"; then
    echo "✗ Java compilation failed"
    FAILED=$((FAILED + 1))
else
    if java RecursionDemo > /dev/null 2>&1; then
        echo "✓ Java recursion examples run successfully"
        PASSED=$((PASSED + 1))
        rm -f RecursionDemo.class
    else
        echo "✗ Java recursion examples failed to run"
        FAILED=$((FAILED + 1))
    fi
fi
echo ""

# Ruby
echo "--- Testing Ruby ---"
if ruby recursion.rb > /dev/null 2>&1; then
    echo "✓ Ruby recursion examples run successfully"
    PASSED=$((PASSED + 1))
else
    echo "✗ Ruby recursion examples failed"
    FAILED=$((FAILED + 1))
fi
echo ""

# Racket
echo "--- Testing Racket ---"
if racket recursion.rkt > /dev/null 2>&1; then
    echo "✓ Racket recursion examples run successfully"
    PASSED=$((PASSED + 1))
else
    echo "✗ Racket recursion examples failed"
    FAILED=$((FAILED + 1))
fi
echo ""

# Prolog
echo "--- Testing Prolog ---"
if swipl -s recursion.pl -g run_tests -t halt > /dev/null 2>&1; then
    echo "✓ Prolog recursion examples run successfully"
    PASSED=$((PASSED + 1))
else
    echo "✗ Prolog recursion examples failed"
    FAILED=$((FAILED + 1))
fi
echo ""

# Summary
echo "========================================="
echo "Test Summary"
echo "========================================="
echo "Passed: $PASSED/9"
echo "Failed: $FAILED/9"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "🎉 All recursion examples passed!"
    exit 0
else
    echo "⚠️  Some tests failed"
    exit 1
fi
