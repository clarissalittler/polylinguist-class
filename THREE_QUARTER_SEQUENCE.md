# Three-Quarter Polyglot Programming Sequence

A comprehensive three-quarter curriculum teaching computer science through multiple programming languages and paradigms. Each quarter builds on the previous, deepening understanding while introducing new concepts and languages.

## Overview

### Quarter 1: Foundations - Introduction to Programming
**Focus:** Core programming concepts, basic paradigms, computational thinking
**Languages:** Python, JavaScript, C, Java, Ruby, Haskell, OCaml, Racket, Prolog, Rust
**Level:** Beginner (no prerequisites)

### Quarter 2: Intermediate - Paradigms and Patterns
**Focus:** Deep paradigm exploration, advanced data structures, design patterns
**Languages:** All Quarter 1 + TypeScript, Clojure, Elixir, Go
**Level:** Intermediate (Quarter 1 or equivalent)

### Quarter 3: Advanced - Systems and Integration
**Focus:** Performance, concurrency, language interoperability, real-world systems
**Languages:** All Quarter 2 + Assembly (x86-64), Zig, Scala, Kotlin
**Level:** Advanced (Quarter 2 or equivalent)

---

# Quarter 1: Foundations - Introduction to Programming

## Course Description

Learn programming fundamentals through 10 different languages representing procedural, object-oriented, functional, and logic paradigms. Understand how the same concepts manifest differently across paradigms.

## Learning Objectives

By the end of Quarter 1, students will:
- Write programs in at least 5 different programming languages
- Understand fundamental CS concepts: variables, functions, control flow, recursion
- Explain differences between programming paradigms
- Choose appropriate data structures for problems
- Read and understand code in unfamiliar languages
- Debug programs using different language tools

## Weekly Schedule (10 weeks)

### Week 1: Hello, World! & Development Environments
**Concepts:** Compilation vs interpretation, REPLs, basic syntax, comments
**Languages:** All 10 languages
**Topics:**
- First programs in each language
- Understanding error messages
- Using documentation
- REPL-driven development

**Lab:** Write "Hello, World!" in all 10 languages, create personalized greetings

**Deliverable:** Personal greeting program in 3 chosen languages

---

### Week 2: Variables, Types, and Type Systems
**Concepts:** Static vs dynamic typing, type inference, strong vs weak typing
**Languages:** Python, JavaScript, C, Java, Haskell, OCaml, Rust
**Topics:**
- Primitive types across languages
- Type annotations and inference
- Type errors at compile-time vs runtime
- Mutability and immutability

**Lab:** Type system exploration - cause type errors intentionally, fix them

**Deliverable:** Comparison essay on static vs dynamic typing (2 pages)

---

### Week 3: Control Flow and Boolean Logic
**Concepts:** Conditionals, loops, short-circuit evaluation, truthiness
**Languages:** Python, C, Ruby, JavaScript, Haskell, Rust
**Topics:**
- If/else, switch/case/pattern matching
- For loops, while loops, and recursion as alternatives
- Boolean operators and truthiness differences
- Guard clauses and early returns

**Lab:** Implement FizzBuzz in 5 languages

**Deliverable:** Control flow comparison chart + FizzBuzz implementations

---

### Week 4: Functions and Scope
**Concepts:** Function definition, parameters, return values, scope, closures
**Languages:** Python, JavaScript, Haskell, C, Ruby, OCaml
**Topics:**
- Pure vs impure functions
- Function signatures and type annotations
- Lexical scope and closures
- First-class functions
- Recursion basics

**Lab:** Implement factorial (iterative and recursive) in multiple languages

**Deliverable:** Function implementations demonstrating scope and closure

---

### Week 5: Data Structures I - Collections
**Concepts:** Arrays, lists, tuples, sets, dictionaries/maps
**Languages:** Python, Java, Haskell, JavaScript, OCaml, Rust
**Topics:**
- Mutable vs immutable collections
- List operations: map, filter, indexing
- Hash tables/dictionaries
- Array bounds and safety
- Collection iteration patterns

**Lab:** Implement a contact manager using appropriate data structures

**Deliverable:** Contact manager in 2 languages (one functional, one imperative)

---

### Week 6: Recursion and Recursive Data Structures
**Concepts:** Recursive thinking, base cases, tail recursion, trees, lists
**Languages:** Haskell, Racket, OCaml, Python, Rust
**Topics:**
- Understanding recursion through visualization
- Tail call optimization
- Recursive data structures (linked lists, trees)
- Recursion vs iteration tradeoffs
- Stack overflow and limits

**Lab:** Implement tree traversal, recursive list operations

**Deliverable:** Binary tree implementation with traversals in 2 languages

---

### Week 7: Higher-Order Functions and Functional Patterns
**Concepts:** Functions as first-class values, map/filter/reduce, composition
**Languages:** Haskell, JavaScript, Python, Ruby, OCaml, Racket
**Topics:**
- Passing functions as arguments
- Returning functions (currying, partial application)
- Map, filter, reduce/fold patterns
- Function composition and pipelines
- List comprehensions vs higher-order functions

**Lab:** Data transformation pipeline using functional techniques

**Deliverable:** Data processing program using map/filter/reduce

---

### Week 8: Object-Oriented Programming
**Concepts:** Classes, objects, inheritance, encapsulation, polymorphism
**Languages:** Java, Ruby, Python, JavaScript (ES6 classes)
**Topics:**
- Class definition and instantiation
- Attributes and methods
- Inheritance vs composition
- Polymorphism and interfaces
- OOP vs functional approaches to state

**Lab:** Design a simple game (e.g., text adventure) using OOP

**Deliverable:** OOP design document + implementation in 2 languages

---

### Week 9: Pattern Matching and Algebraic Data Types
**Concepts:** Pattern matching, algebraic data types, tagged unions
**Languages:** Haskell, OCaml, Rust, Prolog, Racket
**Topics:**
- Pattern matching syntax
- Algebraic data types (sum and product types)
- Option/Maybe types
- Pattern exhaustiveness
- Destructuring in different languages

**Lab:** Implement expression evaluator using pattern matching

**Deliverable:** Expression evaluator with ADTs in 2 languages

---

### Week 10: Type Systems and Introduction to Logic Programming
**Concepts:** Type inference, generics, parametric polymorphism, logic programming
**Languages:** Haskell, OCaml, Java, TypeScript (preview), Prolog
**Topics:**
- Type inference algorithms
- Generics and parametric types
- Type classes and traits
- Introduction to Prolog and declarative thinking
- Unification and backtracking basics

**Lab:** Generic data structures, simple Prolog facts and queries

**Final Project:** Implement the same non-trivial program in 3 languages from different paradigms (procedural, OOP, functional), with written analysis comparing approaches

---

## Assessment

- **Weekly Labs (40%):** Hands-on coding in multiple languages
- **Deliverables (30%):** Written comparisons, implementations
- **Final Project (25%):** Multi-language implementation + analysis
- **Participation (5%):** Discussion, code reviews, peer feedback

---

# Quarter 2: Intermediate - Paradigms and Patterns

## Course Description

Deep dive into programming paradigms, advanced data structures, and design patterns. Introduces new languages while deepening expertise in Quarter 1 languages. Focus on software design and architectural thinking.

## New Languages Introduced

- **TypeScript:** Static typing for JavaScript, gradual typing
- **Clojure:** Modern Lisp, immutable data structures, concurrency
- **Elixir:** Functional, actor model, fault tolerance
- **Go:** Simple systems language, goroutines, interfaces

## Learning Objectives

By the end of Quarter 2, students will:
- Master functional programming concepts (functors, monads, lazy evaluation)
- Design and implement complex data structures
- Apply design patterns appropriately across paradigms
- Handle errors idiomatically in different languages
- Write concurrent programs safely
- Understand memory management models

## Weekly Schedule (10 weeks)

### Week 1: Advanced Functional Programming I - Functors and Applicatives
**Concepts:** Functors, applicative functors, function composition at scale
**Languages:** Haskell, OCaml, JavaScript (functional style), Clojure
**Topics:**
- Functor type class
- Mapping over different contexts
- Applicative functors
- Composing effectful computations
- Clojure introduction: immutability, persistent data structures

**Lab:** Build a validation library using functors and applicatives

---

### Week 2: Advanced Functional Programming II - Monads
**Concepts:** Monads, do-notation, monadic composition
**Languages:** Haskell, OCaml, Scala (preview), Rust (Result/Option)
**Topics:**
- Understanding the Monad abstraction
- Common monads: Maybe, Either, IO, List
- Do-notation and for-comprehensions
- Monad transformers introduction
- Error handling with monads

**Lab:** Implement a monadic parser combinator library

---

### Week 3: Lazy Evaluation and Infinite Data Structures
**Concepts:** Lazy evaluation, thunks, infinite sequences, memoization
**Languages:** Haskell, Clojure, Python (generators), JavaScript (generators)
**Topics:**
- Strict vs lazy evaluation
- Infinite lists and streams
- Generators and iterators
- Memoization and dynamic programming
- Space leaks and when to force evaluation

**Lab:** Solve Project Euler problems using lazy evaluation

---

### Week 4: Advanced Data Structures
**Concepts:** Trees, graphs, heaps, tries, persistent data structures
**Languages:** Java, Python, Haskell, Clojure, Rust
**Topics:**
- Balanced trees (AVL, Red-Black)
- Graph representations and algorithms
- Priority queues and heaps
- Persistent vs ephemeral data structures
- Structural sharing in functional languages

**Lab:** Implement a graph library with BFS/DFS in 2 languages

---

### Week 5: Type Systems Deep Dive
**Concepts:** Advanced types, GADTs, type families, dependent types (preview)
**Languages:** Haskell, OCaml, TypeScript, Rust
**Topics:**
- Generalized algebraic data types (GADTs)
- Type families and associated types
- Phantom types
- TypeScript's structural type system
- Rust's trait system vs Haskell's type classes

**Lab:** Type-safe embedded DSL using advanced types

---

### Week 6: Error Handling Across Paradigms
**Concepts:** Exceptions, Result types, Maybe/Option, error monads
**Languages:** Java, Python, Haskell, Rust, Go, Elixir
**Topics:**
- Exception-based error handling
- Result and Option types
- Railway-oriented programming
- Error handling in Go (multiple returns)
- Elixir's "let it crash" philosophy
- Supervisor trees introduction

**Lab:** Robust file processing with comprehensive error handling

---

### Week 7: Design Patterns Across Paradigms
**Concepts:** GOF patterns, functional alternatives, language-specific patterns
**Languages:** Java, Python, JavaScript, Haskell, Clojure
**Topics:**
- Classic OOP patterns: Factory, Strategy, Observer, Decorator
- Functional alternatives to OOP patterns
- Higher-order functions replacing patterns
- Clojure protocols and multimethods
- When patterns are language smells

**Lab:** Implement same system using OOP patterns and functional alternatives

---

### Week 8: Metaprogramming and Macros
**Concepts:** Code generation, macros, reflection, decorators
**Languages:** Racket, Clojure, Ruby, Python, Rust (procedural macros)
**Topics:**
- Lisp macros and homoiconicity
- Compile-time vs runtime metaprogramming
- Python decorators and metaclasses
- Ruby's open classes
- Rust's macro_rules! and procedural macros

**Lab:** Create a DSL using macros in Racket and Clojure

---

### Week 9: Concurrent Programming I - Threads and Locks
**Concepts:** Threads, mutexes, race conditions, deadlock
**Languages:** Java, Python, Go, Rust
**Topics:**
- Thread creation and management
- Shared memory and synchronization
- Mutexes, semaphores, condition variables
- Race conditions and data races
- Deadlock detection and prevention
- Go's goroutines introduction

**Lab:** Thread-safe producer-consumer implementation

---

### Week 10: Concurrent Programming II - Message Passing and Actors
**Concepts:** Actor model, channels, CSP, immutability for concurrency
**Languages:** Elixir, Go, Clojure, Rust
**Topics:**
- Actor model philosophy
- Message passing vs shared memory
- Channels and select
- Supervision and fault tolerance (Elixir)
- Immutability enabling safe concurrency
- STM in Clojure

**Lab:** Distributed chat server using actors (Elixir)

**Final Project:** Design and implement a multi-component system using at least 3 different paradigms. Examples: web scraper with concurrent workers, distributed key-value store, interpreter for a small language. Include architectural documentation explaining paradigm choices.

---

## Assessment

- **Weekly Labs (35%):** Advanced implementations
- **Design Exercises (25%):** Architectural decisions, pattern applications
- **Final Project (35%):** Multi-paradigm system with documentation
- **Participation (5%):** Code reviews, design discussions

---

# Quarter 3: Advanced - Systems and Integration

## Course Description

Advanced topics in programming language implementation, systems programming, performance optimization, and language interoperability. Students build real-world systems that integrate multiple languages and paradigms.

## New Languages Introduced

- **Assembly (x86-64):** Understanding the machine level
- **Zig:** Modern systems programming, comptime
- **Scala:** JVM functional/OOP fusion, advanced type system
- **Kotlin:** Modern JVM language, coroutines, DSLs

## Learning Objectives

By the end of Quarter 3, students will:
- Understand low-level memory management and performance
- Write concurrent and parallel programs correctly
- Optimize code based on performance profiling
- Design and implement programming language interpreters
- Integrate multiple languages in a single system
- Make informed language and paradigm choices for real problems

## Weekly Schedule (10 weeks)

### Week 1: Memory Management Deep Dive
**Concepts:** Stack vs heap, manual management, GC, ownership
**Languages:** C, Rust, Zig, Assembly (x86-64), Java
**Topics:**
- Memory layout and allocation
- Manual memory management (malloc/free)
- Garbage collection algorithms
- Rust's ownership system
- Memory leaks and use-after-free
- Inspecting assembly output

**Lab:** Implement a memory allocator, analyze assembly from high-level code

---

### Week 2: Performance and Optimization
**Concepts:** Profiling, algorithmic complexity, micro-optimizations
**Languages:** C, Rust, Go, Java, Python
**Topics:**
- Profiling tools for different languages
- Algorithmic vs constant-factor optimization
- Cache-friendly code
- SIMD and vectorization
- When to optimize, when not to
- Benchmarking methodology

**Lab:** Profile and optimize a slow program, compare across languages

---

### Week 3: Compilers and Interpreters I - Lexing and Parsing
**Concepts:** Lexical analysis, parsing, grammars, ASTs
**Languages:** OCaml, Haskell, Rust, Racket
**Topics:**
- Regular expressions and lexing
- Context-free grammars
- Parser combinators
- Recursive descent parsing
- Building ASTs
- Error recovery

**Lab:** Build a lexer and parser for a small language

---

### Week 4: Compilers and Interpreters II - Evaluation and Code Generation
**Concepts:** Tree-walking interpreters, bytecode, compilation
**Languages:** OCaml, Haskell, C, Rust
**Topics:**
- Tree-walking interpretation
- Environment and closures
- Bytecode virtual machines
- Stack-based vs register-based VMs
- Compilation to assembly
- Introduction to LLVM

**Lab:** Extend interpreter with functions and closures

---

### Week 5: Type Systems and Type Inference Implementation
**Concepts:** Hindley-Milner, unification, type checking
**Languages:** OCaml, Haskell, Scala
**Topics:**
- Algorithm W (Hindley-Milner)
- Unification algorithm
- Type environments
- Polymorphism and generics
- Type inference vs type checking
- Scala's advanced type features

**Lab:** Implement type inference for a simple ML-like language

---

### Week 6: Parallel Programming
**Concepts:** Parallelism vs concurrency, data parallelism, work stealing
**Languages:** Rust, Go, Java, Scala, Clojure
**Topics:**
- Parallelism fundamentals
- Data parallelism (map-reduce)
- Task parallelism
- Work-stealing schedulers
- Parallel collections
- Avoiding false sharing

**Lab:** Parallel data processing pipeline with performance analysis

---

### Week 7: Advanced Concurrency Patterns
**Concepts:** Lock-free programming, async/await, reactive programming
**Languages:** Rust, Kotlin, JavaScript, Elixir, Go
**Topics:**
- Lock-free data structures
- Async/await across languages
- Futures and promises
- Reactive streams
- Coroutines (Kotlin)
- Backpressure and flow control

**Lab:** Async web server with non-blocking I/O

---

### Week 8: Foreign Function Interfaces and Language Interop
**Concepts:** FFI, calling conventions, marshalling, binding generation
**Languages:** C, Python, Rust, Java (JNI), JavaScript (WASM)
**Topics:**
- C FFI from high-level languages
- Type marshalling and conversion
- Memory safety across boundaries
- Performance of FFI calls
- WebAssembly as compilation target
- Generating bindings automatically

**Lab:** Create Python/Rust library with FFI bindings

---

### Week 9: Domain-Specific Languages
**Concepts:** Internal vs external DSLs, DSL design
**Languages:** Racket, Kotlin, Ruby, Haskell
**Topics:**
- When to build a DSL
- Internal DSLs with host language features
- External DSL implementation
- Kotlin DSL builders
- Parser combinators for DSLs
- SQL, HTML, CSS as DSL examples

**Lab:** Design and implement a DSL for specific domain

---

### Week 10: Language Ecosystem and Tooling
**Concepts:** Build systems, package management, testing, documentation
**Languages:** All
**Topics:**
- Package managers across ecosystems
- Build systems (Make, Cargo, npm, Maven, etc.)
- Testing philosophies and frameworks
- Property-based testing
- Documentation generation
- Continuous integration

**Lab:** Set up complete development pipeline for multi-language project

**Final Project:** Substantial software system integrating multiple languages, demonstrating:
- Performance-critical components (C/Rust/Zig)
- High-level coordination (Python/Ruby/Elixir)
- Type-safe business logic (Haskell/OCaml/Scala)
- Concurrent/parallel execution
- Comprehensive testing
- Complete documentation

Examples:
- Database engine with SQL parser
- Web framework with templating DSL
- Distributed system with multiple services
- Programming language with compiler/interpreter
- Real-time data processing pipeline

---

## Assessment

- **Weekly Labs (30%):** Advanced implementations
- **Mini Projects (30%):** Compiler, concurrent system, performance optimization
- **Final Project (35%):** Substantial multi-language system
- **Participation (5%):** Design reviews, architectural discussions

---

# Cross-Quarter Themes

## Language Progression

### Quarter 1 (10 languages)
Core languages representing major paradigms - baseline understanding

### Quarter 2 (+4 languages = 14 total)
Add languages with unique features:
- TypeScript: Gradual typing
- Clojure: Modern Lisp
- Elixir: Actor model
- Go: Simplicity, concurrency

### Quarter 3 (+4 languages = 18 total)
Add systems and advanced languages:
- Assembly: Machine level
- Zig: Modern systems
- Scala: Advanced types
- Kotlin: Modern JVM

## Skill Development Arc

### Quarter 1: Understanding
- Read code in multiple languages
- Translate concepts between languages
- Recognize paradigm differences

### Quarter 2: Design
- Choose appropriate paradigms
- Apply patterns correctly
- Design systems across paradigms

### Quarter 3: Mastery
- Implement language features
- Optimize for performance
- Integrate multiple languages

## Project Complexity

### Quarter 1
Single-file programs, focused on concepts

### Quarter 2
Multi-file projects, architectural concerns

### Quarter 3
Multi-language systems, production considerations

---

# Prerequisites and Placement

## Entering Quarter 1
- No programming experience required
- Basic computer literacy
- Mathematical thinking helpful but not required

## Entering Quarter 2
- Quarter 1 or equivalent
- Comfortable with at least 2 programming languages
- Understanding of basic data structures and algorithms
- Can implement recursive functions

## Entering Quarter 3
- Quarter 2 or equivalent
- Strong understanding of at least one paradigm
- Experience with concurrent programming
- Can design multi-component systems

---

# Suggested Supplementary Topics

## Workshops (Optional)
- Git and version control
- Debugging techniques across languages
- Development environments and tooling
- Technical writing and documentation
- Code review best practices
- Open source contribution

## Guest Lectures
- Language designers discussing design decisions
- Industry practitioners on real-world language choices
- PL researchers on cutting-edge topics

---

# Learning Resources

## Required Texts
- "Structure and Interpretation of Computer Programs" (SICP) - Racket/Scheme
- "Thinking Functionally with Haskell" - Richard Bird
- "The Rust Programming Language" - Official Rust book
- "Learn You a Haskell for Great Good!" - Miran Lipovača

## Recommended Texts
- "Types and Programming Languages" - Benjamin Pierce (Q2/Q3)
- "Concurrent Programming in ML" - John Reppy (Q2)
- "Programming Language Pragmatics" - Michael Scott (Q3)
- "Crafting Interpreters" - Robert Nystrom (Q3)

## Online Resources
- Language documentation for all languages
- Exercism.io for practice in multiple languages
- Project Euler for algorithmic challenges
- Rosetta Code for language comparisons

---

# Career Pathways

This sequence prepares students for:

- **Software Engineering:** Ability to quickly learn new languages and frameworks
- **Systems Programming:** Deep understanding of memory, performance, concurrency
- **Programming Language Research:** Foundation in type theory, semantics, implementation
- **Technical Leadership:** Informed decision-making about language and architecture choices
- **Full-Stack Development:** Comfort with languages across the stack
- **Specialized Domains:** Finance (OCaml), Data Science (Python), Web (JavaScript), Systems (Rust)

---

# Assessment Philosophy

## Emphasizes
- Understanding over memorization
- Comparison and analysis
- Design decisions and tradeoffs
- Clear technical communication

## De-emphasizes
- Syntax minutiae
- Speed of completion
- Single "correct" solution

## Project-Based Learning
Each quarter culminates in substantial project demonstrating integration of concepts across multiple languages and paradigms.
