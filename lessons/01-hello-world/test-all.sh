#!/bin/bash
# Test script to run all Hello World programs

echo "==================================="
echo "Testing Hello World in All Languages"
echo "==================================="

echo -e "\n[Python]"
python3 hello.py

echo -e "\n[JavaScript]"
node hello.js

echo -e "\n[Ruby]"
ruby hello.rb

echo -e "\n[Racket]"
racket hello.rkt

echo -e "\n[Prolog]"
swipl -q -s hello.pl

echo -e "\n[C]"
gcc hello.c -o hello_c
./hello_c
rm hello_c

echo -e "\n[Java]"
javac Hello.java
java Hello
rm Hello.class

echo -e "\n[Haskell]"
runghc hello.hs

echo -e "\n[OCaml]"
ocaml hello.ml

echo -e "\n[Rust]"
rustc hello.rs -o hello_rust
./hello_rust
rm hello_rust

echo -e "\n==================================="
echo "All tests completed!"
echo "==================================="
