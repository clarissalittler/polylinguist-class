# Polylinguist Class: Learn Programming Through Multiple Languages

A comprehensive introduction to computer science that teaches fundamental programming concepts through exploration of multiple programming languages and paradigms. Designed to be accessible to students of all backgrounds with no prior programming experience required.

## Why Learn Multiple Languages?

Traditional programming courses teach through a single language, but this course takes a different approach. By learning the same concepts in different languages, you will:

- **Understand concepts more deeply** - Seeing ideas from multiple perspectives reveals their universal nature
- **Think like a polyglot** - Recognize that programming concepts transcend specific syntax
- **Become adaptable** - Learn to pick up new languages quickly throughout your career
- **Choose the right tool** - Understand which languages and paradigms fit different problems
- **Appreciate language design** - See the tradeoffs different languages make

## Languages & Paradigms Covered

This course includes 10 programming languages representing different paradigms:

### Procedural & Systems
- **C** - Low-level systems programming, explicit memory management
- **Python** - High-level scripting, readability-focused

### Object-Oriented
- **Java** - Enterprise OOP, strong static typing
- **Ruby** - Expressive OOP, dynamic typing
- **Python** - Multi-paradigm flexibility

### Functional
- **Haskell** - Pure functional, advanced type system
- **OCaml** - Functional with pragmatism, excellent inference
- **Racket** - Lisp/Scheme dialect, language experimentation

### Logic
- **Prolog** - Declarative logic programming, automated reasoning

### Multi-Paradigm
- **JavaScript** - Web-oriented, event-driven
- **Rust** - Systems programming with safety guarantees

## Course Structure

### Lessons

1. **Hello, World!** - First programs, compilation vs interpretation
2. **Variables and Types** - Type systems, static vs dynamic typing
3. **Control Flow** - Conditionals, loops, boolean logic
4. **Functions** - Procedures, parameters, scope, purity
5. **Data Structures** - Lists, arrays, maps, immutability
6. **Recursion** - Recursive thinking, base cases, iteration comparison
7. **Higher-Order Functions** - Functions as values, map/filter/reduce
8. **Object-Oriented Programming** - Classes, inheritance, polymorphism
9. **Pattern Matching** - Algebraic data types, destructuring
10. **Type Systems** - Type inference, generics, type safety

Each lesson includes:
- Conceptual explanations
- Side-by-side code examples in multiple languages
- Runnable code samples
- Exercises and discussion questions
- Paradigm comparisons

## Quick Start

### Prerequisites

All required languages are already installed in this environment. Verify with:

```bash
python3 --version && node --version && ruby --version && javac --version && \
ghc --version && ocaml --version && racket --version && swipl --version && \
rustc --version && gcc --version
```

### Getting Started

1. **Read the course materials**
   - [SYLLABUS.md](SYLLABUS.md) - Complete course outline and learning outcomes
   - [GETTING_STARTED.md](GETTING_STARTED.md) - Setup guide and study tips

2. **Start with Lesson 1**
   ```bash
   cd lessons/01-hello-world
   cat README.md
   ```

3. **Run the examples**
   ```bash
   # Try programs in different languages
   python3 hello.py
   node hello.js
   ruby hello.rb

   # Or test all at once
   ./test-all.sh
   ```

4. **Explore and experiment**
   - Modify the code
   - Break things intentionally
   - Compare different approaches

### Repository Structure

```
polylinguist-class/
├── README.md                    # This file
├── SYLLABUS.md                  # Full course syllabus
├── GETTING_STARTED.md           # Setup and study guide
├── lessons/                     # Course lessons
│   ├── 01-hello-world/         # Lesson 1: Hello, World!
│   │   ├── README.md           # Lesson content
│   │   ├── hello.py            # Python example
│   │   ├── hello.js            # JavaScript example
│   │   ├── hello.c             # C example
│   │   ├── Hello.java          # Java example
│   │   ├── hello.rb            # Ruby example
│   │   ├── hello.hs            # Haskell example
│   │   ├── hello.ml            # OCaml example
│   │   ├── hello.rkt           # Racket example
│   │   ├── hello.pl            # Prolog example
│   │   ├── hello.rs            # Rust example
│   │   └── test-all.sh         # Test all examples
│   ├── 02-variables-types/     # Lesson 2: Variables & Types
│   │   ├── README.md
│   │   └── ... (examples in all languages)
│   └── ... (more lessons)
├── examples/                    # Additional example programs
└── resources/                   # Reference materials
    └── LANGUAGE_COMPARISON.md   # Side-by-side language comparisons
```

## Learning Philosophy

### Concepts Over Syntax

Don't worry about memorizing syntax. Focus on understanding:
- What is a variable? A function? A type?
- How do different paradigms approach problems?
- When is immutability useful?
- What makes code readable?

### Comparative Learning

Each concept is presented in multiple languages simultaneously. This helps you:
- Separate universal concepts from language-specific syntax
- Recognize patterns across paradigms
- Appreciate different design philosophies

### Practical Focus

Every lesson includes:
- Runnable code you can execute immediately
- Exercises to practice concepts
- Discussion questions to deepen understanding

### No Prior Experience Needed

This course assumes zero programming background. We start with "Hello, World!" and build up systematically.

## Example: The Same Concept in Different Languages

Here's how different languages print "Hello, World!":

```python
# Python - simple and readable
print("Hello, World!")
```

```javascript
// JavaScript - similar to Python
console.log("Hello, World!");
```

```c
// C - more verbose, explicit
#include <stdio.h>
int main() {
    printf("Hello, World!\n");
    return 0;
}
```

```haskell
-- Haskell - functional, with type signature
main :: IO ()
main = putStrLn "Hello, World!"
```

```racket
; Racket - Lisp-style S-expressions
#lang racket
(displayln "Hello, World!")
```

Notice how:
- The concept is the same (output text)
- The syntax varies significantly
- Each reflects its language's philosophy

## Resources

### Documentation
- [Language Comparison Guide](resources/LANGUAGE_COMPARISON.md) - Side-by-side syntax comparisons
- Official documentation linked in [GETTING_STARTED.md](GETTING_STARTED.md)

### Interactive Learning
- Use REPLs for experimentation (Python, Node, Ruby, Haskell, OCaml, Racket, Prolog)
- Modify example code and observe results
- Make intentional errors to learn from them

## Who Is This For?

- **Complete beginners** - No programming experience required
- **Self-taught programmers** - Fill gaps and gain broader perspective
- **Students** - Supplement traditional CS courses
- **Polyglots** - Learn to see patterns across languages
- **Career changers** - Build strong foundations

## Learning Outcomes

After completing this course, you will be able to:

1. Write basic programs in 5+ programming languages
2. Explain fundamental CS concepts (variables, functions, types, control flow)
3. Understand different programming paradigms (procedural, OOP, functional, logic)
4. Choose appropriate languages for different problem domains
5. Read and understand code in unfamiliar languages
6. Learn new programming languages independently
7. Recognize universal programming patterns

## Contributing

This is an educational project. Suggestions for improvements are welcome!

## License

See [LICENSE](LICENSE) for details.

## Get Started Now!

Ready to begin your multilingual programming journey?

1. Read [GETTING_STARTED.md](GETTING_STARTED.md)
2. Review the [SYLLABUS.md](SYLLABUS.md)
3. Start with [Lesson 1: Hello, World!](lessons/01-hello-world/README.md)

Happy coding! 🚀
