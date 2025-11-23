# Getting Started with Polylinguist Class

Welcome to the multilingual introduction to programming! This guide will help you set up your environment and get started with the course.

## Philosophy

This course is based on the idea that learning programming through multiple languages helps you:
- Understand concepts more deeply
- Recognize universal patterns
- Become a more adaptable programmer
- Think about problems in different ways

Don't worry about mastering every language! The goal is to see how different paradigms approach the same problems.

## Installation

All required languages are available in this environment. You can verify by running:

```bash
# Check all installations
python3 --version
node --version
ruby --version
javac --version
ghc --version
ocaml --version
racket --version
swipl --version
rustc --version
gcc --version
```

## Course Structure

```
polylinguist-class/
├── SYLLABUS.md              # Full course syllabus
├── GETTING_STARTED.md       # This file
├── lessons/                 # Lesson materials
│   ├── 01-hello-world/     # Lesson 1
│   │   ├── README.md       # Lesson content
│   │   ├── hello.py        # Python example
│   │   ├── hello.js        # JavaScript example
│   │   ├── hello.c         # C example
│   │   └── ...             # Examples in all languages
│   ├── 02-variables-types/ # Lesson 2
│   └── ...                 # More lessons
├── examples/               # Additional example programs
└── resources/              # Language references and guides
```

## How to Use This Course

### 1. Read the Lesson README
Start with the `README.md` in each lesson directory. It explains:
- The concept being taught
- How different languages approach it
- Key differences between paradigms

### 2. Study the Examples
Look at the code examples side by side. Notice:
- Similarities across languages
- Different syntax for the same concept
- Paradigm-specific approaches

### 3. Run the Code
Execute each example to see it work:

```bash
# Navigate to a lesson
cd lessons/01-hello-world

# Run examples in different languages
python3 hello.py
node hello.js
ruby hello.rb
racket hello.rkt

# Compile and run (C)
gcc hello.c -o hello
./hello

# Compile and run (Java)
javac Hello.java
java Hello

# And so on...
```

### 4. Complete Exercises
Each lesson includes exercises. Try implementing them in multiple languages!

### 5. Compare and Reflect
Think about:
- Which language felt most natural?
- Which was most concise?
- Which made the concept clearest?
- When would you choose each language?

## Language Quick Reference

### Running Programs

| Language   | File Extension | Run Command              | Compilation Required |
|------------|----------------|--------------------------|----------------------|
| Python     | `.py`          | `python3 file.py`        | No                   |
| JavaScript | `.js`          | `node file.js`           | No                   |
| Ruby       | `.rb`          | `ruby file.rb`           | No                   |
| Racket     | `.rkt`         | `racket file.rkt`        | No                   |
| C          | `.c`           | `gcc file.c && ./a.out`  | Yes                  |
| Java       | `.java`        | `javac F.java && java F` | Yes (to bytecode)    |
| Haskell    | `.hs`          | `runghc file.hs`         | Optional             |
| OCaml      | `.ml`          | `ocaml file.ml`          | Optional             |
| Prolog     | `.pl`          | `swipl -s file.pl`       | No                   |
| Rust       | `.rs`          | `rustc file.rs && ./file`| Yes                  |

### Interactive REPLs

Many languages have REPLs (Read-Eval-Print Loop) for interactive exploration:

```bash
python3          # Python REPL
node             # JavaScript REPL
irb              # Ruby REPL
racket           # Racket REPL
ghci             # Haskell REPL
ocaml            # OCaml REPL (or 'utop' for better experience)
swipl            # Prolog REPL
```

Type code, press Enter, see results immediately!

## Paradigm Overview

### Procedural (C, Python)
- Step-by-step instructions
- Variables and state
- Procedures/functions that modify state
- Good for: System programming, scripts

### Object-Oriented (Java, Ruby, Python)
- Organize code around objects
- Encapsulation, inheritance, polymorphism
- Methods operate on object state
- Good for: Large applications, modeling real-world entities

### Functional (Haskell, OCaml, Racket)
- Functions as primary building blocks
- Immutable data
- Function composition
- Good for: Data transformation, mathematical problems

### Logic (Prolog)
- Declare facts and rules
- Query for solutions
- Automatic reasoning
- Good for: AI, symbolic reasoning, constraint solving

### Multi-paradigm (Python, Ruby, JavaScript, Rust)
- Combine multiple paradigms
- Choose the right tool for each part
- Good for: Versatile applications

## Study Tips

### 1. Start Simple
Don't try to learn all languages at once. Focus on understanding the concept, then see how it appears in 2-3 languages.

### 2. Type the Code
Don't just read - type out the examples yourself. Muscle memory helps!

### 3. Break Things
Modify the code and see what happens. Make intentional errors to understand error messages.

### 4. Compare Pairs
Compare languages in pairs:
- Python vs JavaScript (dynamic typing)
- C vs Rust (systems programming)
- Haskell vs OCaml (functional)
- Java vs Ruby (OOP)

### 5. Use REPLs
For quick experiments, use the interactive REPLs. They're great for testing small snippets.

### 6. Ask "Why?"
When you see differences, ask:
- Why is this syntax different?
- What problem does this feature solve?
- When would this approach be better?

## Common Beginner Mistakes

### Mixing Syntax
It's normal to write Python code with semicolons or forget them in C. This is actually a good sign - you're learning!

### Comparing Languages
All languages are tools. None is "best" - they have different strengths.

### Trying to Memorize
Don't memorize syntax. Understand concepts and reference syntax as needed.

### Skipping "Boring" Parts
Even simple programs like "Hello, World!" teach important lessons about compilation, execution models, and language philosophy.

## Getting Help

### Documentation
- Python: https://docs.python.org/3/
- JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript
- Ruby: https://ruby-doc.org/
- Java: https://docs.oracle.com/en/java/
- Haskell: https://www.haskell.org/documentation/
- OCaml: https://ocaml.org/docs
- Racket: https://docs.racket-lang.org/
- Prolog: https://www.swi-prolog.org/pldoc/
- Rust: https://doc.rust-lang.org/
- C: https://en.cppreference.com/w/c

### Online REPLs (for when you don't have local setup)
- Python: https://repl.it/languages/python3
- JavaScript: Browser console (F12)
- Haskell: https://repl.it/languages/haskell
- And many more!

## Next Steps

1. Read the [SYLLABUS.md](SYLLABUS.md) for the full course outline
2. Start with [Lesson 1: Hello, World!](lessons/01-hello-world/README.md)
3. Work through lessons at your own pace
4. Experiment with the code
5. Have fun exploring different ways to think about programming!

## A Note on Difficulty

Some lessons will feel easier in certain languages. That's by design! Notice when a language makes something easy or hard - that tells you about its design goals.

Remember: The goal isn't to become an expert in 10 languages. It's to understand programming concepts deeply by seeing them from multiple perspectives.

Happy coding! 🚀
