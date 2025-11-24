# Getting Started with the Three-Language Curriculum

Welcome to Computer Science through comparative programming! This guide will help you set up your development environment and start your journey toward language-agnostic thinking.

## Philosophy

This course teaches programming through **three core languages** (Python, C++, Haskell) plus exposure to **four additional languages** in focused modules. This approach helps you:

- **Understand concepts deeply** by seeing them from multiple perspectives
- **Recognize universal patterns** that transcend specific syntax
- **Become an adaptable programmer** who can learn new languages quickly
- **Think about problems differently** using multiple paradigms
- **Achieve real proficiency** in your core languages (not just exposure)

**Key principle**: We want you to be **proficient in 3 languages** and **literate in 7** by the end of the course.

## The Languages You'll Learn

### Core Languages (Used Throughout)

You'll work with these three languages in nearly every lesson:

#### 1. **Python** 🐍
- **Paradigm**: Multi-paradigm (procedural, OOP, functional)
- **Type System**: Dynamic, strong typing
- **Why**: Accessible syntax, widely used in industry, great for learning
- **You'll use it for**: Quick prototyping, data processing, general programming

#### 2. **C++** ⚡
- **Paradigm**: Multi-paradigm (procedural, OOP, generic)
- **Type System**: Static, strong typing
- **Why**: Industry standard, performance, systems programming
- **You'll use it for**: Understanding memory, OOP design, performance-critical code
- **Note**: We teach *modern C++* (C++17/20), not legacy C-style code

#### 3. **Haskell** λ
- **Paradigm**: Pure functional
- **Type System**: Static, inferred, advanced
- **Why**: Forces different thinking, mathematical rigor, type safety
- **You'll use it for**: Functional programming, type-driven development, pure functions

### Minor Languages (Focused Modules)

You'll study these languages in 1-3 week modules later in the course:

#### 4. **Racket** (Week 7-8)
- **Focus**: Lisp/Scheme philosophy, macros, language-oriented programming
- **Unique**: Code as data, S-expressions, homoiconicity

#### 5. **C** (Week 8)
- **Focus**: Low-level systems, manual memory management
- **Unique**: What C++ abstracts away, pointers, direct hardware control

#### 6. **Rust** (Week 9-10)
- **Focus**: Modern systems programming with safety
- **Unique**: Ownership model, zero-cost abstractions

#### 7. **Prolog** (Week 9-10)
- **Focus**: Logic programming, declarative thinking
- **Unique**: Completely different computational model

## Installation

### Step 1: Install Python

Python should already be installed on most systems. Verify:

```bash
python3 --version
```

**Should show**: Python 3.8 or later

**If not installed**:
- **macOS**: `brew install python3`
- **Linux**: `sudo apt-get install python3` (Ubuntu/Debian) or `sudo dnf install python3` (Fedora)
- **Windows**: Download from https://www.python.org/downloads/

### Step 2: Install C++ Compiler

**macOS**:
```bash
# Install Xcode Command Line Tools
xcode-select --install

# Verify
g++ --version
```

**Linux**:
```bash
# Install GCC
sudo apt-get install build-essential  # Ubuntu/Debian
sudo dnf install gcc-c++              # Fedora

# Verify
g++ --version
```

**Windows**:
- Install MinGW-w64: https://www.mingw-w64.org/
- Or install Visual Studio with C++ support
- Verify: `g++ --version` or `cl` (for Visual Studio)

**Target**: C++17 or C++20 support

### Step 3: Install Haskell

**All platforms** (recommended: GHCup):

```bash
# Install GHCup (Haskell installer)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Follow the prompts, then verify
ghc --version
ghci --version
```

**Alternative**:
- **macOS**: `brew install ghc`
- **Linux**: `sudo apt-get install haskell-platform` (Ubuntu/Debian)
- **Windows**: Download Haskell Platform from https://www.haskell.org/platform/

**Should show**: GHC 8.10 or later

### Step 4: Verify Your Setup

Run this verification script:

```bash
# Check all three core languages
python3 --version && echo "✓ Python installed"
g++ --version && echo "✓ C++ compiler installed"
ghc --version && echo "✓ Haskell installed"
```

### Step 5: Set Up a Text Editor or IDE

Choose one:

**For Beginners**:
- **VS Code** (https://code.visualstudio.com/) - recommended
  - Install extensions: Python, C/C++, Haskell
  - Free, cross-platform, great for all three languages

**For Advanced Users**:
- **PyCharm** (Python-focused)
- **CLion** (C++ focused)
- **Vim/Emacs** with appropriate plugins
- **Sublime Text**

**Minimal option**: Any text editor + terminal works fine!

## Course Structure

```
three-language-version/
├── README.md                # Course overview
├── GETTING_STARTED.md       # This file
├── INSTRUCTOR_GUIDE.md      # Teaching strategies (for instructors)
├── lessons/                 # Core lessons (16 total)
│   ├── 01-hello-world/
│   │   ├── README.md       # Lesson content
│   │   ├── hello.py        # Python example
│   │   ├── hello.cpp       # C++ example
│   │   └── hello.hs        # Haskell example
│   ├── 02-variables-types/
│   └── ...
├── modules/                 # Supplementary modules
│   ├── computational-thinking/
│   ├── file-io/
│   ├── git-version-control/
│   ├── testing-debugging/
│   ├── racket-module/      # Week 7-8
│   ├── c-module/           # Week 8
│   ├── rust-module/        # Week 9-10
│   └── prolog-module/      # Week 9-10
├── projects/               # 6 progressive projects
│   ├── 01-text-stats/
│   ├── 02-number-game/
│   └── ... (including capstone options)
└── solutions/              # Reference implementations
```

## How to Use This Course

### 1. Read the Lesson README

Start with `lessons/XX-topic/README.md`. Each lesson includes:
- **Conceptual introduction** (language-agnostic explanation)
- **Side-by-side examples** in Python, C++, and Haskell
- **Comparison tables** highlighting differences
- **Discussion questions** about paradigms and tradeoffs
- **Exercises** with varying difficulty

### 2. Study the Examples Side-by-Side

Look at all three implementations. Notice:
- **Similarities**: The core concept is the same
- **Differences**: How syntax and paradigm affect expression
- **Tradeoffs**: When one language makes something easier or harder

### 3. Run the Code

Execute each example to see it work:

```bash
# Navigate to a lesson
cd lessons/01-hello-world

# Run Python (interpreted)
python3 hello.py

# Compile and run C++
g++ -std=c++17 hello.cpp -o hello
./hello

# Run Haskell (interpreted mode for quick feedback)
runhaskell hello.hs

# Or compile Haskell for performance
ghc hello.hs -o hello_hs
./hello_hs
```

### 4. Experiment in REPLs

Use interactive environments for exploration:

```bash
# Python REPL
python3
>>> print("Hello!")
>>> 2 + 2
>>> exit()

# Haskell REPL (GHCi)
ghci
ghci> putStrLn "Hello!"
ghci> 2 + 2
ghci> :quit

# C++ doesn't have a standard REPL, but compile-run cycles are fast
```

**Pro tip**: REPLs are perfect for testing small snippets and understanding concepts interactively.

### 5. Complete Exercises

Each lesson has exercises ranging from "warmup" to "challenge":
- **Start with warmup exercises** to build confidence
- **Implement in your strongest language first** (probably Python)
- **Try the same exercise in another language** to deepen understanding
- **Compare your solutions** - which was easier? Why?

### 6. Compare and Reflect

After each lesson, think about:
- Which language felt most natural for this concept?
- Which was most concise?
- Which made the concept clearest?
- What does this tell you about the language's design goals?

## Running Programs: Quick Reference

### Python
```bash
# Run directly
python3 script.py

# Interactive REPL
python3
```
**No compilation needed** - interpreted language

### C++
```bash
# Compile with C++17 standard
g++ -std=c++17 program.cpp -o program

# Run the compiled binary
./program

# One-liner
g++ -std=c++17 program.cpp -o program && ./program
```
**Compilation required** - creates native executable

### Haskell
```bash
# Run interpreted (fast iteration)
runhaskell program.hs

# Or compile (better performance)
ghc program.hs -o program
./program

# Interactive REPL
ghci
```
**Flexible** - can interpret or compile

## Paradigm Overview

### Multi-Paradigm: Python
- **Procedural**: Step-by-step instructions, variables, loops
- **Object-Oriented**: Classes, objects, inheritance
- **Functional**: Functions as values, map/filter/reduce
- **Philosophy**: "There should be one obvious way to do it"
- **Good for**: General programming, scripting, data science, web

### Multi-Paradigm (OOP focus): C++
- **Procedural**: Like C, low-level control
- **Object-Oriented**: Classes, inheritance, polymorphism
- **Generic**: Templates for type-safe abstractions
- **Philosophy**: "Zero overhead abstraction"
- **Good for**: Systems programming, games, performance-critical applications

### Pure Functional: Haskell
- **Functional**: Everything is a function, immutable by default
- **Pure**: No side effects (except explicit IO)
- **Lazy**: Expressions evaluated only when needed
- **Philosophy**: "If it compiles, it probably works"
- **Good for**: Compilers, data transformation, type-driven development

## Study Tips

### 1. Focus on Concepts, Not Syntax

**Don't memorize** `std::cout <<` vs `print()` vs `putStrLn`

**DO understand** "output to screen" and how each language expresses it

Syntax can be looked up. Concepts must be understood.

### 2. Start with Python

Python has the gentlest syntax. When learning a new concept:
1. Understand it in Python first
2. See how C++ does it (usually more verbose, more explicit)
3. See how Haskell does it (often quite different!)

This progression helps build understanding.

### 3. Use the REPL Extensively

For Python and Haskell, the REPL is your friend:
- Test small snippets
- Explore library functions
- Debug confusing behavior
- Build intuition through experimentation

### 4. Type the Code Yourself

**Don't copy-paste**. Type it out. This builds:
- Muscle memory
- Familiarity with syntax
- Attention to details (you'll notice more)

### 5. Break Things Intentionally

- Remove a semicolon in C++
- Change types in Python
- Break a type signature in Haskell

**Learn from the error messages**. They're teaching tools!

### 6. Compare in Pairs

When studying a concept, compare two languages at a time:
- **Python vs C++**: Dynamic vs static, interpreted vs compiled
- **Python vs Haskell**: Imperative vs functional, mutable vs immutable
- **C++ vs Haskell**: Both compiled, both typed, very different paradigms

### 7. Ask "Why?" Constantly

- Why does Haskell need a type signature here?
- Why does C++ require explicit types?
- Why doesn't Python need semicolons?

Every difference reveals design philosophy.

## Common Beginner Challenges

### "I keep mixing up the syntax!"

**This is normal and actually good!** It means you're learning multiple languages. Over time, you'll develop mental "modes" for each language.

**Solution**: Use different editors/color schemes for each language to build visual association.

### "Haskell makes no sense!"

**Haskell is hard** because it's genuinely different. Functional programming rewires your brain.

**Solution**:
- Don't try to learn Haskell like Python
- Focus on the *what*, not the *how*
- Use types as documentation
- It will click around week 4-6

### "C++ error messages are incomprehensible!"

**C++ template errors are legendary** for being obscure.

**Solution**:
- Focus on the *first* error message
- Fix one thing at a time
- Use simpler types while learning
- It gets better with practice

### "I don't know which language to use for exercises!"

**Start with your strongest language** (probably Python), then try others.

**Goal**: Implement key exercises in at least 2 of the 3 core languages.

### "This seems like a lot!"

**It is ambitious**, but we've structured it carefully:
- Weeks 1-3: Heavy on Python (build confidence)
- Weeks 4-6: Balanced across all three
- Weeks 7-10: Add minor languages gradually

**You're not expected to master all 7 languages**. You're expected to:
- Be **proficient** in Python, C++, Haskell
- Be **literate** in Racket, C, Rust, Prolog

## Minor Languages (Later in Course)

You'll install these later when the modules arrive:

### Racket (Week 7)
```bash
# Install from https://racket-lang.org/
# Or: brew install racket (macOS)
racket --version
```

### C (Week 8)
```bash
# Usually installed with C++ compiler
gcc --version
```

### Rust (Week 9)
```bash
# Install Rustup: https://rustup.rs/
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustc --version
```

### Prolog (Week 10)
```bash
# Install SWI-Prolog: https://www.swi-prolog.org/
swipl --version
```

**Don't install these yet** - we'll guide you when the time comes.

## Getting Help

### Official Documentation

**Core Languages**:
- **Python**: https://docs.python.org/3/ (start here)
- **C++**: https://en.cppreference.com/ (reference) and https://learncpp.com/ (tutorial)
- **Haskell**: https://www.haskell.org/documentation/ and https://learnyouahaskell.com/

**Minor Languages** (for later):
- **Racket**: https://docs.racket-lang.org/
- **C**: https://en.cppreference.com/w/c
- **Rust**: https://doc.rust-lang.org/book/
- **Prolog**: https://www.swi-prolog.org/pldoc/

### Online Learning Resources

- **Python**: Automate the Boring Stuff (free online)
- **C++**: learncpp.com (free, comprehensive)
- **Haskell**: Learn You a Haskell (free online book)

### Asking for Help

When stuck:
1. **Read the error message carefully** (they're often more helpful than you think)
2. **Check the lesson README** (may have hints)
3. **Try the REPL** (isolate the problem)
4. **Search the error** (someone else has had it)
5. **Ask with context** (show your code, what you tried, what you expected)

## Next Steps

1. ✅ **Verify your installation** (Python, C++, Haskell)
2. 📖 **Read the course [README](README.md)** for overview
3. 🎯 **Start [Lesson 1: Hello, World!](lessons/01-hello-world/README.md)**
4. 💻 **Complete the exercises** in at least 2 languages
5. 🤔 **Reflect on differences** you notice

## Mindset for Success

### ✅ DO:
- **Embrace confusion** - it means you're learning
- **Compare constantly** - differences deepen understanding
- **Experiment freely** - code is cheap, try things
- **Focus on concepts** - syntax is just syntax
- **Ask "why"** - every difference has a reason

### ❌ DON'T:
- **Try to memorize everything** - understand, don't memorize
- **Rank languages as "better"** - they're tools, not competitors
- **Rush through lessons** - depth over speed
- **Skip the "easy" languages** - Python teaches important concepts
- **Fear making mistakes** - errors are learning opportunities

## A Note on Difficulty

**Week 1-2**: Should feel manageable (mostly Python)
**Week 3-4**: Getting challenging (all three languages active)
**Week 5-6**: Peak difficulty (paradigm contrasts)
**Week 7-8**: Adding breadth (minor languages)
**Week 9-10**: Integration and synthesis

**This is intentional**. The curve is designed to build confidence before challenging you.

## The Big Picture

By the end of this course, you won't just know 7 languages. You'll:

✨ **Think language-agnostically** - seeing concepts beyond syntax
✨ **Learn new languages rapidly** - recognizing universal patterns
✨ **Choose appropriate tools** - matching languages to problems
✨ **Write better code** - understanding paradigms deeply
✨ **Adapt to any codebase** - comfortable with diverse styles

**Remember**: You're not learning 7 languages. You're learning *how to think about programming*.

The languages are just the vehicle for that understanding.

---

**Ready to begin?** → **[Lesson 1: Hello, World!](lessons/01-hello-world/README.md)**

Happy coding! 🚀
