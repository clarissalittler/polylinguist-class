# Lesson 1: Hello, World!

## Learning Objectives

- Write and run your first program in multiple languages
- Understand the difference between compiled and interpreted languages
- Learn about comments and basic program structure
- Experience different development workflows

## Concept: Your First Program

The "Hello, World!" program is a programming tradition. It's the simplest program that produces visible output, helping you verify that your development environment is working correctly.

## Key Concepts

### Compilation vs Interpretation

**Compiled Languages** (C, Java, Rust, Haskell, OCaml):
- Source code is translated to machine code before execution
- Requires a separate compilation step
- Generally faster execution
- Errors caught at compile-time

**Interpreted Languages** (Python, Ruby, JavaScript):
- Source code is executed directly by an interpreter
- No separate compilation step needed
- More flexible, often with a REPL (Read-Eval-Print Loop)
- Some errors only caught at runtime

**Mixed Approaches** (Java, Racket):
- Compile to intermediate bytecode
- Bytecode is then interpreted or JIT-compiled

## Examples

### Python (Interpreted, Multi-paradigm)
```python
# hello.py
print("Hello, World!")
```

**Run:**
```bash
python3 hello.py
```

**Key points:**
- Very simple syntax
- `print()` is a built-in function
- `#` for comments
- No semicolons needed

---

### JavaScript (Interpreted, Multi-paradigm)
```javascript
// hello.js
console.log("Hello, World!");
```

**Run:**
```bash
node hello.js
```

**Key points:**
- Similar to Python in simplicity
- `console.log()` for output
- `//` for single-line comments
- Semicolons are optional (but often used)

---

### C (Compiled, Procedural)
```c
// hello.c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

**Compile and run:**
```bash
gcc hello.c -o hello
./hello
```

**Key points:**
- Need to include standard I/O library
- Must have a `main()` function
- More verbose than Python
- Explicit newline `\n`
- Must return a status code

---

### Java (Compiled to bytecode, OOP)
```java
// Hello.java
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

**Compile and run:**
```bash
javac Hello.java
java Hello
```

**Key points:**
- Everything must be in a class
- Filename must match class name
- `main()` method is the entry point
- More ceremony than C

---

### Ruby (Interpreted, OOP)
```ruby
# hello.rb
puts "Hello, World!"
```

**Run:**
```bash
ruby hello.rb
```

**Key points:**
- Very clean syntax
- `puts` (put string) adds newline automatically
- No parentheses needed for method calls

---

### Haskell (Compiled, Pure Functional)
```haskell
-- hello.hs
main :: IO ()
main = putStrLn "Hello, World!"
```

**Compile and run:**
```bash
ghc hello.hs -o hello
./hello
```

**Or interpret:**
```bash
runghc hello.hs
```

**Key points:**
- Type signature `main :: IO ()`
- `main` is a special I/O action
- `putStrLn` is a function that performs I/O
- `--` for comments

---

### OCaml (Compiled, Functional)
```ocaml
(* hello.ml *)
let () = print_endline "Hello, World!"
```

**Compile and run:**
```bash
ocamlc hello.ml -o hello
./hello
```

**Or interpret:**
```bash
ocaml hello.ml
```

**Key points:**
- `let () =` binds the unit value
- `print_endline` adds newline
- `(* *)` for comments

---

### Racket (Interpreted, Functional)
```racket
; hello.rkt
#lang racket
(displayln "Hello, World!")
```

**Run:**
```bash
racket hello.rkt
```

**Key points:**
- Must specify language with `#lang`
- Lisp-style S-expressions (parentheses)
- `;` for comments
- Functions called as `(function arg)`

---

### Prolog (Interpreted, Logic)
```prolog
% hello.pl
:- initialization(main).

main :-
    write('Hello, World!'), nl,
    halt.
```

**Run:**
```bash
swipl -q -s hello.pl
```

**Key points:**
- Logic programming paradigm
- `:-` defines rules and directives
- `write/1` for output, `nl` for newline
- Different mindset: declaring facts and rules

---

### Rust (Compiled, Systems)
```rust
// hello.rs
fn main() {
    println!("Hello, World!");
}
```

**Compile and run:**
```bash
rustc hello.rs -o hello
./hello
```

**Key points:**
- Similar to C but safer
- `fn` declares functions
- `println!` is a macro (note the `!`)
- No manual memory management

---

## Exercise: Personalized Greetings

Modify each "Hello, World!" program to:
1. Print your name on a second line
2. Add a comment explaining what the program does
3. (Challenge) Accept your name as input instead of hardcoding it

## Comparison Table

| Language   | File Ext | Compilation | Execution Command | Lines of Code |
|------------|----------|-------------|-------------------|---------------|
| Python     | .py      | No          | `python3 file.py` | 1             |
| JavaScript | .js      | No          | `node file.js`    | 1             |
| Ruby       | .rb      | No          | `ruby file.rb`    | 1             |
| Racket     | .rkt     | No          | `racket file.rkt` | 2             |
| C          | .c       | Yes         | `gcc → ./a.out`   | 5             |
| Rust       | .rs      | Yes         | `rustc → ./a.out` | 3             |
| Java       | .java    | To bytecode | `javac → java`    | 5             |
| Haskell    | .hs      | Optional    | `ghc → ./a.out`   | 2             |
| OCaml      | .ml      | Optional    | `ocamlc → ./a.out`| 1             |
| Prolog     | .pl      | No          | `swipl -s file`   | 5             |

## Discussion Questions

1. Why do some languages require more code to print a simple message?
2. What are the tradeoffs between compiled and interpreted languages?
3. Which syntax feels most natural to you? Why?
4. What does the verbosity of "Hello, World!" tell us about the language's design philosophy?

## Next Lesson

In Lesson 2, we'll explore variables and data types across these languages, seeing how different type systems work.
