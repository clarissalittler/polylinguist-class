# Three-Language Computer Science Curriculum

**Teaching Computer Science through comparative programming with manageable cognitive load**

## Overview

This curriculum teaches introductory Computer Science through **three core languages** (Python, C++, Haskell) with supplementary modules in four additional languages (Racket, C, Rust, Prolog). The approach balances depth and breadth, enabling students to achieve genuine proficiency while experiencing diverse programming paradigms.

## Language Structure

### Core Languages (Throughout the Course)

These three languages are featured heavily in every lesson:

#### 1. **Python** 🐍
- **Paradigm**: Multi-paradigm (procedural, OOP, functional)
- **Type System**: Dynamic, strong
- **Why**: Accessible syntax, industry relevance, rapid prototyping
- **Teaching Focus**: Readability, expressiveness, practical problem-solving

#### 2. **C++** ⚡
- **Paradigm**: Multi-paradigm (procedural, OOP, generic)
- **Type System**: Static, strong
- **Why**: Industry standard, systems programming, AP/state alignment
- **Teaching Focus**: Modern C++ (C++17/20), OOP design, performance awareness
- **Note**: We teach *modern* C++ idioms, not "C with classes"

#### 3. **Haskell** λ
- **Paradigm**: Pure functional
- **Type System**: Static, inferred, advanced
- **Why**: Forces different thinking, mathematical rigor, type safety
- **Teaching Focus**: Immutability, recursion, type-driven development

### Minor Languages (1-3 Week Modules)

These languages are introduced in focused modules to expand paradigm exposure:

#### 4. **Racket** (Scheme/Lisp)
- **Module Duration**: 2-3 weeks
- **Focus**: Lisp philosophy, macros, language-oriented programming
- **Unique Contribution**: Code as data, metaprogramming, minimalism

#### 5. **C** (Systems Programming)
- **Module Duration**: 1-2 weeks
- **Focus**: Manual memory management, pointers, low-level operations
- **Unique Contribution**: Understanding what C++ abstracts, systems thinking

#### 6. **Rust** (Modern Systems)
- **Module Duration**: 1-2 weeks
- **Focus**: Ownership, borrowing, memory safety without GC
- **Unique Contribution**: Modern approach to systems programming

#### 7. **Prolog** (Logic Programming)
- **Module Duration**: 1-2 weeks
- **Focus**: Declarative programming, pattern matching, automated reasoning
- **Unique Contribution**: Completely different computational model

## Course Structure

### Lesson Sequence (16 Core Lessons)

1. **Hello, World!** - Introduction to compilation, interpretation, and basic syntax
2. **Variables and Types** - Type systems, mutability, declarations
3. **Control Flow** - Conditionals, loops, boolean logic
4. **Functions** - Procedures, parameters, scope, closures
5. **Data Structures** - Lists, arrays, maps, immutability
6. **Recursion** - Recursive thinking, tail recursion, stack behavior
7. **Object-Oriented Programming** - Classes, inheritance, polymorphism
8. **Higher-Order Functions** - Map, filter, reduce, function composition
9. **Pattern Matching** - Destructuring, guards, algebraic data types
10. **Type Systems** - Type inference, generics, type safety
11. **Error Handling** - Exceptions, Result types, Maybe/Optional
12. **Algorithm Analysis** - Big-O notation, complexity analysis
13. **Sorting & Searching** - Classic algorithms across paradigms
14. **Stacks, Queues, Lists** - Linear data structures
15. **Trees** - Binary trees, BSTs, traversals
16. **Graphs** - Graph representations and algorithms

### Modules

- **Module 0**: Computational Thinking (pre-course foundation)
- **File I/O**: Reading and writing files across languages
- **Git & Version Control**: Professional development workflow
- **Testing & Debugging**: TDD, unit testing, debugging strategies

### Projects (6 Progressive Projects)

Each project can be completed in any of the core languages or (for capstones) a minor language:

1. **Text Statistics Tool** (Easy-Medium, 3-5 hours)
2. **Number Guessing Game** (Easy-Medium, 2-4 hours)
3. **Todo List Manager** (Medium, 5-8 hours) - *Capstone option*
4. **Expression Evaluator** (Medium-Hard, 6-10 hours) - *Capstone option*
5. **Polyglot Build Tool** (Hard, 8-12 hours) - *Capstone option*
6. **Mini Programming Language** (Very Hard, 15-20 hours) - *Capstone option*

**Capstone Format**: Projects 3-6 can serve as capstones where students pick a minor language and implement the project, reflecting on paradigm-specific approaches.

## Pedagogical Approach

### Core Principles

**1. Three Deep, Four Broad**
- **Core 3 languages**: Students achieve comfort and proficiency
- **Minor 4 languages**: Students gain exposure and understanding
- **Goal**: "I can write programs in 3 languages, I understand concepts in 7"

**2. Concepts Before Syntax**
- Universal concepts presented first
- Language-specific syntax is just the expression
- Students explain concepts without referencing specific languages

**3. Comparative Learning**
- Side-by-side examples in core languages
- Explicit comparison of approaches
- "How does Python's approach differ from Haskell's?" is a constant refrain

**4. Progressive Paradigm Introduction**

**Weeks 1-3: Foundations (Python-heavy)**
- Python: ~50% of examples
- C++: ~30% of examples
- Haskell: ~20% of examples
- Build confidence with accessible syntax

**Weeks 4-6: Paradigm Contrast (Balanced)**
- All three languages equally featured
- Emphasis on paradigm differences
- Functional vs. OOP vs. multi-paradigm

**Weeks 7-8: Specialization (Language-specific deep dives)**
- **Week 7**: Racket module (functional + Lisp)
- **Week 8**: C module (systems programming)

**Weeks 9-10: Advanced Topics + Capstone**
- Return to core three languages
- **Rust or Prolog** module (1 week)
- **Capstone project** in chosen language

### Language Rotation Strategy

Each lesson includes:
- **Conceptual introduction** (language-agnostic)
- **Core examples** in all 3 core languages
- **Comparison table** highlighting differences
- **Exercises** requiring implementation in multiple languages
- **Discussion questions** about paradigm tradeoffs

### Cognitive Load Management

**Realistic Expectations:**
- **Week 1**: Comfort with Python, exposure to C++ and Haskell
- **Week 3**: Can write simple programs in all 3 core languages
- **Week 5**: Comfortable in Python and C++, proficient in Haskell basics
- **Week 7**: Add Racket (4 languages active)
- **Week 8**: Add C understanding
- **Week 10**: Exposure to 7 languages total, proficiency in 3

**Assessment Philosophy:**
- Students choose language for most exercises
- Must demonstrate understanding in at least 2 core languages
- Capstones encourage exploration of minor languages
- Emphasis on conceptual understanding over syntax perfection

## What Makes This Different

### Compared to Single-Language Intro Courses:
✅ **Language-agnostic thinking** from day one
✅ **Paradigm awareness** through constant comparison
✅ **Transfer skills** built explicitly
✅ **Adaptability** to new languages developed early
❌ But: More cognitive load, requires strong instruction

### Compared to Nine-Language Version:
✅ **More manageable** cognitive load (3 core vs. 9 simultaneous)
✅ **Deeper proficiency** in core languages
✅ **Clearer expectations** about what to learn deeply
✅ **Still paradigm-diverse** (7 languages total)
❌ But: Less breadth, fewer paradigms covered simultaneously

## Getting Started

### For Instructors:

1. **Read the Instructor Guide** (`INSTRUCTOR_GUIDE.md`) - Teaching strategies, pacing, common challenges
2. **Review Setup Guide** (`instructor-resources/setup-guide.md`) - Installation instructions for all languages
3. **Examine Lesson 1** - See the comparative teaching approach in action
4. **Plan Your Pacing** - Use provided pacing guide as starting point

### For Students:

1. **Read Getting Started** (`GETTING_STARTED.md`) - Installation and environment setup
2. **Install Core Languages First** - Python, C++, Haskell
3. **Complete Lesson 1** - Introduction to all three languages
4. **Work Through Exercises** - Progressive difficulty in each lesson

## Directory Structure

```
three-language-version/
├── README.md                    # This file
├── GETTING_STARTED.md          # Student setup guide
├── INSTRUCTOR_GUIDE.md         # Teaching strategies and tips
├── SYLLABUS.md                 # Sample syllabus
├── lessons/                    # Core lessons 1-16
│   ├── 01-hello-world/
│   ├── 02-variables-types/
│   └── ...
├── modules/                    # Supplementary modules
│   ├── computational-thinking/
│   ├── file-io/
│   ├── git-version-control/
│   ├── testing-debugging/
│   ├── racket-module/         # Week 7-8
│   ├── c-module/              # Week 8
│   ├── rust-module/           # Week 9-10
│   └── prolog-module/         # Week 9-10
├── projects/                   # 6 progressive projects
│   ├── 01-text-stats/
│   ├── 02-number-game/
│   └── ...
├── instructor-resources/       # Teaching materials
│   ├── pacing-guide.md
│   ├── setup-guide.md
│   ├── assessment-rubrics.md
│   └── ...
├── resources/                  # Reference materials
└── solutions/                  # Reference implementations
```

## Sample Weekly Schedule

**Week 1**: Lesson 1-2 (Hello World, Variables & Types)
- Python: Learn syntax basics
- C++: Understand compilation, types
- Haskell: See functional approach

**Week 2-3**: Lesson 3-4 (Control Flow, Functions)
- All three languages actively used
- Compare imperative vs. functional approaches

**Week 4-5**: Lesson 5-6 (Data Structures, Recursion)
- Deep dive into paradigm differences
- Immutability in Haskell vs. mutability in Python/C++

**Week 6**: Lesson 7 (Object-Oriented Programming)
- C++ and Python: OOP deep dive
- Haskell: Type classes and algebraic data types

**Week 7**: Lesson 8 + Racket Module (Higher-Order Functions + Lisp)
- Functional programming across all languages
- **Racket introduction**: Lisp philosophy, S-expressions, macros

**Week 8**: Lesson 9 + C Module (Pattern Matching + Systems)
- Pattern matching in Haskell and C++
- **C introduction**: Pointers, manual memory, low-level operations

**Week 9**: Lesson 10-12 (Types, Errors, Algorithm Analysis)
- **Choose**: Rust or Prolog module (1 week)

**Week 10**: Lesson 13-14 + **Capstone Project**
- Sorting, searching, data structures
- **Capstone**: Choose minor language for final project

## Assessment Strategy

### Formative Assessment:
- **Weekly exercises** (3-5 per lesson)
- **Code reading** (understanding others' implementations)
- **Comparative analysis** (explain paradigm differences)

### Summative Assessment:
- **Projects** (6 total, increasing difficulty)
- **Capstone projects** (using minor languages)
- **Conceptual exams** (explain ideas, not syntax)

### Grading Philosophy:
- **Concepts > Syntax**: Partial credit for understanding even if code doesn't compile
- **Multi-language demonstration**: Bonus for implementing in multiple languages
- **Reflection**: Students explain tradeoffs and choices
- **Process over product**: Version control, testing, iteration valued

## Success Metrics

By the end of the course, students should be able to:

✅ **Write functional programs** in Python, C++, and Haskell
✅ **Explain core CS concepts** (recursion, types, OOP, FP) without language-specific jargon
✅ **Choose appropriate languages** for different problem domains
✅ **Read and understand** code in all 7 languages
✅ **Learn new languages** quickly by recognizing universal patterns
✅ **Appreciate language design** tradeoffs and philosophy

## Philosophy

**Computer Science is not Python. It's not Java. It's not any single language.**

Computer Science is the study of computation, algorithms, and problem-solving. Languages are tools—important tools—but *ideas* are what matter.

This curriculum teaches students to think like computer scientists by showing them that the same concept can be expressed in radically different ways, and that understanding the *concept* is more valuable than memorizing syntax.

**Three languages deeply, four languages broadly, infinite languages eventually.**

---

## Next Steps

- 📖 **[Read Getting Started Guide](GETTING_STARTED.md)** - Set up your environment
- 👨‍🏫 **[Review Instructor Guide](INSTRUCTOR_GUIDE.md)** - Teaching strategies
- 🎯 **[Start Lesson 1](lessons/01-hello-world/)** - Begin your journey

**Questions?** Open an issue or check the instructor resources.

**Happy learning! 🚀**
