# CS 101: Introduction to Computer Science
## A Multi-Language, Multi-Paradigm Approach

**Course Version:** Three-Language Curriculum
**Prerequisites:** None (designed for new students with no prior programming experience)
**Credit Hours:** 4
**Recommended Schedule:** 10-15 weeks (adaptable to semester or quarter systems)

---

## Course Description

This course introduces fundamental computer science concepts through comparative programming in three core languages: Python, C++, and Haskell. By learning the same concepts expressed in different paradigms, students develop language-agnostic thinking skills that transfer to any future programming context.

Unlike traditional single-language introductory courses, this curriculum emphasizes that **Computer Science is not any single programming language**—it is the study of computation, algorithms, and problem-solving. Languages are tools; concepts are what matter.

Students will achieve **proficiency in three languages** (Python, C++, Haskell) while gaining **literacy in four additional languages** (Racket, C, Rust, Prolog) through focused modules.

---

## Learning Outcomes

By the end of this course, students will be able to:

### Core Competencies
1. **Write functional programs** in Python, C++, and Haskell
2. **Explain fundamental CS concepts** (variables, control flow, functions, recursion, data structures, algorithms) without language-specific jargon
3. **Compare and contrast programming paradigms** (procedural, object-oriented, functional)
4. **Analyze algorithm complexity** using Big-O notation
5. **Implement classic data structures** (lists, stacks, queues, trees, graphs)
6. **Apply problem-solving methodologies** including decomposition, pattern recognition, and abstraction

### Transfer Skills
7. **Read and understand code** in unfamiliar languages by recognizing universal patterns
8. **Learn new programming languages** rapidly by mapping new syntax to known concepts
9. **Choose appropriate tools** for different problem domains
10. **Appreciate language design tradeoffs** and understand why different languages exist

### Professional Skills
11. **Use version control** (Git) for code management and collaboration
12. **Write and run automated tests** to verify program correctness
13. **Debug programs systematically** using appropriate tools and techniques
14. **Work collaboratively** on programming projects

---

## Required Materials

### Software (Free)
- **Python 3.8+** - https://www.python.org/
- **C++ Compiler** (g++ with C++17 support) - included with Xcode (Mac), MinGW (Windows), or GCC (Linux)
- **Haskell (GHC 8.10+)** - https://www.haskell.org/ghcup/
- **Git** - https://git-scm.com/
- **VS Code** (recommended) - https://code.visualstudio.com/

### Additional Languages (Installed Later)
- **Racket** - https://racket-lang.org/ (Week 7)
- **Rust** - https://rustup.rs/ (Week 9)
- **SWI-Prolog** - https://www.swi-prolog.org/ (Week 10)

### Textbooks (Optional/Supplementary)
No required textbook. All materials provided in course repository. Recommended supplementary resources:
- *Learn You a Haskell for Great Good!* (free online)
- *Automate the Boring Stuff with Python* (free online)
- *learncpp.com* (free online)

---

## Course Structure

### Format
- **Lectures:** 3 hours/week (concepts, demonstrations, comparisons)
- **Labs:** 2 hours/week (hands-on programming in all three languages)
- **Office Hours:** 2 hours/week minimum

### Content Organization
- **16 Core Lessons** - Fundamental CS concepts taught in all three core languages
- **4 Practical Modules** - Professional skills (Git, testing, file I/O, computational thinking)
- **4 Language Modules** - Focused introductions to Racket, C, Rust, and Prolog
- **6 Progressive Projects** - Hands-on application from simple to capstone

---

## Weekly Schedule

### Week 1: Foundations
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Course Introduction & Philosophy | Syllabus, README |
| 2 | Environment Setup | GETTING_STARTED.md |
| 3 | **Lesson 1: Hello, World!** | lessons/01-hello-world |
| 4 | Lab: First Programs in 3 Languages | Exercises 1-4 |
| 5 | **Lesson 2: Variables & Types** | lessons/02-variables-types |

**Module:** Computational Thinking (integrated throughout)
**Languages:** Python (50%), C++ (30%), Haskell (20%)

---

### Week 2: Control Structures
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Variables & Types (continued) | Exercises |
| 2 | **Lesson 3: Control Flow** | lessons/03-control-flow |
| 3 | Conditionals Across Paradigms | Lab exercises |
| 4 | Loops vs Recursion (introduction) | Lab exercises |
| 5 | Review & Practice | Challenge exercises |

**Project 1 Assigned:** Text Statistics Tool
**Languages:** Python (50%), C++ (30%), Haskell (20%)

---

### Week 3: Functions & Abstraction
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 4: Functions** | lessons/04-functions |
| 2 | Parameters, Arguments, Scope | Lab exercises |
| 3 | Pure Functions vs Side Effects | Haskell focus |
| 4 | **Module: Git & Version Control** | modules/git-version-control |
| 5 | Lab: Git Workflow Practice | Git exercises |

**Project 1 Due**
**Languages:** Python (40%), C++ (30%), Haskell (30%)

---

### Week 4: Data Structures I
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 5: Data Structures** | lessons/05-data-structures |
| 2 | Lists, Arrays, Vectors | Lab exercises |
| 3 | Maps, Dictionaries, Hash Tables | Lab exercises |
| 4 | Immutability vs Mutability | Paradigm comparison |
| 5 | **Project 2 Assigned:** Number Guessing Game | projects/02-number-game |

**Languages:** Balanced (Python 33%, C++ 33%, Haskell 33%)

---

### Week 5: Recursion Deep Dive
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 6: Recursion** | lessons/06-recursion |
| 2 | Base Cases & Recursive Steps | Lab exercises |
| 3 | Tail Recursion & Optimization | Advanced topic |
| 4 | Recursion vs Iteration | Paradigm comparison |
| 5 | Review & Practice | Challenge exercises |

**Project 2 Due**
**Languages:** Balanced (emphasize Haskell for recursion)

---

### Week 6: Object-Oriented Programming
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 7: Object-Oriented Programming** | lessons/07-oop |
| 2 | Classes, Objects, Methods | C++ and Python focus |
| 3 | Inheritance & Polymorphism | Lab exercises |
| 4 | OOP vs Functional Approaches | Type classes in Haskell |
| 5 | **Midterm Review** | All lessons 1-7 |

**Midterm Exam (covers Lessons 1-7)**
**Languages:** Python and C++ for OOP, Haskell for comparison

---

### Week 7: Higher-Order Functions & Racket
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 8: Higher-Order Functions** | lessons/08-higher-order-functions |
| 2 | Map, Filter, Reduce | Lab exercises |
| 3 | **Racket Module Begins** | modules/racket-module |
| 4 | Racket: S-expressions & Lists | Racket lab |
| 5 | Racket: Functions & Recursion | Racket lab |

**Project 3 Assigned:** Todo List Manager
**Languages:** Core 3 + Racket introduction

---

### Week 8: Pattern Matching & C
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Racket: Higher-Order Functions | Racket exercises |
| 2 | **Lesson 9: Pattern Matching** | lessons/09-pattern-matching |
| 3 | Pattern Matching Deep Dive | Haskell focus |
| 4 | **C Module Begins** | modules/c-module |
| 5 | C: Pointers & Memory | C lab |

**Project 3 Due**
**Languages:** Core 3 + Racket + C

---

### Week 9: Type Systems & Modern Systems Programming
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | C: Manual Memory Management | C exercises |
| 2 | **Lesson 10: Type Systems** | lessons/10-type-systems |
| 3 | Type Safety & Type Inference | Comparison lab |
| 4 | **Rust Module Begins** | modules/rust-module |
| 5 | Rust: Ownership & Borrowing | Rust lab |

**Project 4 Assigned:** Expression Evaluator (Capstone Option)
**Languages:** Core 3 + Rust

---

### Week 10: Error Handling & Logic Programming
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Rust: Safety Without Garbage Collection | Rust exercises |
| 2 | **Lesson 11: Error Handling** | lessons/11-error-handling |
| 3 | Exceptions vs Result Types | Comparison lab |
| 4 | **Prolog Module Begins** | modules/prolog-module |
| 5 | Prolog: Logic Programming | Prolog lab |

**Choose:** Rust OR Prolog deep dive (student choice)
**Languages:** Core 3 + chosen module language

---

### Week 11: Algorithm Analysis
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Prolog: Rules & Queries | Prolog exercises |
| 2 | **Lesson 12: Algorithm Analysis** | lessons/12-algorithm-analysis |
| 3 | Big-O Notation | Lab exercises |
| 4 | Time vs Space Complexity | Comparison exercises |
| 5 | **Module: Testing & Debugging** | modules/testing-debugging |

**Project 4 Due**
**Languages:** Core 3 (return to deep focus)

---

### Week 12: Sorting & Searching
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Testing Frameworks Setup | Testing lab |
| 2 | **Lesson 13: Sorting & Searching** | lessons/13-sorting-searching |
| 3 | Classic Sorting Algorithms | Lab implementations |
| 4 | Search Algorithms | Lab implementations |
| 5 | Algorithm Analysis Practice | Complexity analysis |

**Project 5 Assigned:** Polyglot Build Tool (Capstone Option)
**Languages:** Core 3

---

### Week 13: Linear Data Structures
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 14: Stacks, Queues, Lists** | lessons/14-stacks-queues-lists |
| 2 | Stack Implementations | Lab exercises |
| 3 | Queue Implementations | Lab exercises |
| 4 | Linked Lists | Lab exercises |
| 5 | **Module: File I/O** | modules/file-io |

**Project 5 Due**
**Languages:** Core 3

---

### Week 14: Trees & Graphs
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | **Lesson 15: Trees** | lessons/15-trees |
| 2 | Binary Trees & BSTs | Lab exercises |
| 3 | Tree Traversals | Lab exercises |
| 4 | **Lesson 16: Graphs** | lessons/16-graphs |
| 5 | Graph Representations | Lab exercises |

**Final Project Assigned:** Mini Programming Language (Capstone)
**Languages:** Core 3 + chosen minor language

---

### Week 15: Capstone & Review
| Day | Topic | Materials |
|-----|-------|-----------|
| 1 | Graph Algorithms | Lab exercises |
| 2 | Final Project Work Time | Office hours available |
| 3 | Final Project Work Time | Office hours available |
| 4 | **Final Review** | All course material |
| 5 | Course Reflection & Next Steps | Discussion |

**Final Project Due (end of week)**

---

### Finals Week
| Day | Topic |
|-----|-------|
| TBD | **Final Exam** (2-3 hours, comprehensive) |

---

## Assessment & Grading

### Grade Breakdown

| Component | Weight | Description |
|-----------|--------|-------------|
| **Exercises** | 20% | Weekly programming exercises in multiple languages |
| **Projects** | 30% | 6 progressive projects (5% each) |
| **Midterm Exam** | 15% | Conceptual understanding (Lessons 1-7) |
| **Final Exam** | 20% | Comprehensive (all material) |
| **Capstone Project** | 10% | Final project in minor language |
| **Participation** | 5% | Lab attendance, discussions, reflections |

### Grading Scale

| Letter | Percentage | Description |
|--------|------------|-------------|
| A | 93-100% | Exceptional understanding across all languages |
| A- | 90-92% | Strong proficiency in all three core languages |
| B+ | 87-89% | Good understanding with minor gaps |
| B | 83-86% | Solid proficiency in at least two languages |
| B- | 80-82% | Adequate performance with some struggle |
| C+ | 77-79% | Basic competency achieved |
| C | 73-76% | Minimum acceptable performance |
| C- | 70-72% | Marginal pass |
| D | 60-69% | Below expectations |
| F | <60% | Not passing |

### Assessment Philosophy

**Concepts Over Syntax**
- Partial credit for demonstrating understanding even if code doesn't compile
- Emphasis on explaining *why*, not just *how*
- Language-agnostic questions on exams

**Multi-Language Demonstration**
- Must show competency in at least 2 of 3 core languages
- Bonus credit for implementing exercises in all three languages
- Capstone requires using a minor language

**Process Over Product**
- Version control usage is graded
- Testing and debugging attempts valued
- Iteration and improvement encouraged

---

## Projects Overview

### Project 1: Text Statistics Tool (Week 2-3)
**Difficulty:** Easy-Medium | **Time:** 3-5 hours
Build a command-line tool that analyzes text files, counting words, lines, characters, and finding the most common words.
- **Skills:** File I/O, string processing, basic data structures
- **Required:** Implement in Python OR C++
- **Bonus:** Implement in multiple languages

### Project 2: Number Guessing Game (Week 4-5)
**Difficulty:** Easy-Medium | **Time:** 2-4 hours
Create an interactive game where the computer picks a number and gives hints (too high/low).
- **Skills:** Control flow, input/output, random numbers, loops
- **Required:** Implement in any core language
- **Bonus:** Add difficulty levels, track statistics

### Project 3: Todo List Manager (Week 7-8)
**Difficulty:** Medium | **Time:** 5-8 hours
Build a persistent todo list application with add, complete, delete, and list operations.
- **Skills:** Data structures, file persistence, user interface design
- **Required:** Implement in any core language
- **Capstone Option:** Implement in Racket

### Project 4: Expression Evaluator (Week 9-10)
**Difficulty:** Medium-Hard | **Time:** 6-10 hours
Parse and evaluate mathematical expressions like "3 + 4 * 2" with proper operator precedence.
- **Skills:** Parsing, recursion, tree structures, evaluation
- **Required:** Implement in any core language
- **Capstone Option:** Implement in Haskell (parser combinators)

### Project 5: Polyglot Build Tool (Week 12-13)
**Difficulty:** Hard | **Time:** 8-12 hours
Create a simple build system that can compile/run programs in multiple languages based on file extensions.
- **Skills:** Process execution, file system operations, configuration
- **Required:** Implement in Python
- **Capstone Option:** Compare implementations across languages

### Project 6: Mini Programming Language (Week 14-15)
**Difficulty:** Very Hard | **Time:** 15-20 hours
Design and implement a simple programming language with variables, expressions, and control flow.
- **Skills:** Lexing, parsing, interpretation, language design
- **Required:** Implement in any core language
- **Capstone Requirement:** Use a minor language OR write detailed paradigm comparison

---

## Course Policies

### Attendance
- **Lectures:** Attendance expected but not mandatory. Material covered will be on exams.
- **Labs:** Attendance highly recommended. Lab work counts toward participation.
- **Remote Option:** If offered, synchronous participation expected.

### Late Work
- **Exercises:** 10% penalty per day, up to 3 days late. No credit after 3 days.
- **Projects:** 10% penalty per day, up to 5 days late. No credit after 5 days.
- **Extensions:** Available for documented emergencies. Contact instructor BEFORE deadline.

### Academic Integrity
- **Individual Work:** Exercises and exams must be completed individually unless specified.
- **Collaboration Allowed:** Discussion of concepts, debugging help, and study groups are encouraged.
- **Prohibited:** Copying code, sharing solutions, using AI to generate solutions without understanding.
- **Cite Everything:** If you reference external resources, cite them.

**Violation Consequences:**
- First offense: Zero on assignment + warning
- Second offense: Failing grade in course
- All violations reported to academic integrity office

### Code of Conduct
- Respect diverse backgrounds and learning speeds
- Support fellow students; programming is hard for everyone at first
- Ask questions—there are no stupid questions
- Be patient with yourself and others

---

## Resources & Support

### Getting Help
1. **Office Hours:** First stop for conceptual questions
2. **Lab TAs:** Best for debugging and syntax issues
3. **Discussion Forum:** Peer help (encouraged!)
4. **Documentation:** Language docs are your friend

### Language Resources

**Python:**
- Official Docs: https://docs.python.org/3/
- Tutorial: https://docs.python.org/3/tutorial/
- Practice: https://www.hackerrank.com/domains/python

**C++:**
- Reference: https://en.cppreference.com/
- Tutorial: https://learncpp.com/
- Practice: https://www.hackerrank.com/domains/cpp

**Haskell:**
- Learn You a Haskell: http://learnyouahaskell.com/
- Official Docs: https://www.haskell.org/documentation/
- Practice: https://exercism.org/tracks/haskell

### Accessibility
Students with documented disabilities should contact the accessibility office and inform the instructor early in the semester. Accommodations will be made for exams, labs, and assignments as needed.

---

## Frequently Asked Questions

**Q: Why three languages instead of one?**
A: Learning multiple languages simultaneously builds deeper understanding. You'll see that concepts like "loops" and "functions" exist in all languages, just with different syntax. This makes you a better programmer in *any* language.

**Q: Which language should I focus on if I'm struggling?**
A: Start with Python—it's the most forgiving syntax. Once you're comfortable with a concept in Python, the C++ and Haskell versions will make more sense.

**Q: I've never programmed before. Is this course too hard?**
A: No! This course is designed for beginners. The multi-language approach actually helps beginners because you see the same concept multiple times in different forms.

**Q: Will I be proficient in all seven languages by the end?**
A: You'll be **proficient** in Python, C++, and Haskell. You'll be **literate** in Racket, C, Rust, and Prolog—meaning you can read and understand code, even if you're not fluent.

**Q: What if I only want to learn one language (e.g., Python)?**
A: This course may not be the best fit. However, the multi-language approach will make you a *much* stronger Python programmer because you'll understand what Python is doing "under the hood" compared to other languages.

**Q: How much time should I spend outside of class?**
A: Expect 6-10 hours per week of work outside class (exercises, projects, studying). More at the beginning as you set up environments, less as you become comfortable.

---

## Acknowledgments

This curriculum draws on decades of computer science pedagogy research, particularly the principles of:
- **Constructivism:** Learning by doing
- **Comparative Learning:** Understanding through contrast
- **Spiral Curriculum:** Revisiting concepts with increasing depth
- **Multiple Representations:** Seeing ideas from different angles

---

## Schedule Summary

| Week | Lessons | Module | Project |
|------|---------|--------|---------|
| 1 | Hello World, Variables & Types | Computational Thinking | - |
| 2 | Control Flow | - | P1 Assigned |
| 3 | Functions | Git & Version Control | P1 Due |
| 4 | Data Structures | - | P2 Assigned |
| 5 | Recursion | - | P2 Due |
| 6 | Object-Oriented Programming | - | **Midterm** |
| 7 | Higher-Order Functions | Racket | P3 Assigned |
| 8 | Pattern Matching | C | P3 Due |
| 9 | Type Systems | Rust | P4 Assigned |
| 10 | Error Handling | Prolog | - |
| 11 | Algorithm Analysis | Testing & Debugging | P4 Due |
| 12 | Sorting & Searching | - | P5 Assigned |
| 13 | Stacks, Queues, Lists | File I/O | P5 Due |
| 14 | Trees, Graphs | - | P6 Assigned |
| 15 | Capstone Work | - | P6 Due |
| Final | - | - | **Final Exam** |

---

**Welcome to CS 101! Let's learn to think like computer scientists.**
