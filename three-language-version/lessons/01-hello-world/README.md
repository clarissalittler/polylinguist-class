# Lesson 1: Hello, World!

## Learning Objectives

- Write and run your first program in Python, C++, and Haskell
- Understand the difference between compiled and interpreted languages
- Learn about comments and basic program structure
- Experience different development workflows across paradigms
- Begin to see how the same concept can be expressed differently

## Concept: Your First Program

The "Hello, World!" program is a programming tradition dating back to the 1970s. It's the simplest program that produces visible output, helping you verify that your development environment is working correctly.

**More importantly**: This simple program already reveals fundamental differences in how languages approach computation.

## Key Concepts

### Compilation vs Interpretation

**Interpreted Languages** (Python):
- Source code is executed directly by an interpreter
- No separate compilation step needed
- More flexible, often with a REPL (Read-Eval-Print Loop)
- Some errors only caught at runtime
- **Analogy**: Like reading a recipe out loud as you cook

**Compiled Languages** (C++, Haskell):
- Source code is translated to machine code before execution
- Requires a separate compilation step
- Generally faster execution
- More errors caught at compile-time
- **Analogy**: Like translating a book before reading it

**Important**: This is a spectrum, not a binary distinction. Haskell compiles to native code, C++ compiles to machine code, and Python compiles to bytecode that's then interpreted. The key difference is whether you as the programmer experience a separate compilation step.

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
- Very simple, minimal syntax
- `print()` is a built-in function
- `#` for comments
- No semicolons or boilerplate needed
- Executed directly by the interpreter

**What's happening:**
1. Python interpreter reads the file
2. Parses the code into bytecode
3. Executes the bytecode immediately
4. Output appears on your screen

---

### C++ (Compiled, Multi-paradigm with OOP focus)

```cpp
// hello.cpp
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}
```

**Compile and run:**
```bash
g++ -std=c++17 hello.cpp -o hello
./hello
```

**Key points:**
- Need to include I/O library (`<iostream>`)
- Must have a `main()` function (entry point)
- More verbose than Python
- `//` for single-line comments
- `<<` is the output operator (stream insertion)
- `std::` is the namespace prefix
- Must return an integer status code (0 = success)

**What's happening:**
1. C++ compiler (`g++`) reads hello.cpp
2. Preprocessor includes the iostream library
3. Compiler translates to assembly, then machine code
4. Linker creates executable binary (`hello`)
5. You run the binary directly
6. Output appears on your screen

**Why so much more code?** C++ gives you low-level control, but that means you must be explicit about what you're using (iostream) and where your program starts (main).

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

**Or run interpreted (REPL):**
```bash
runhaskell hello.hs
```

**Key points:**
- `--` for comments
- `main :: IO ()` is a type signature (says "main has type IO of unit")
- `main` is defined using `=` (it's a definition, not a procedure)
- `putStrLn` (put line) is a function that returns an IO action
- No parentheses needed for function application
- Looks simple, but there's deep theory underneath

**What's happening:**
1. Haskell compiler (`ghc`) reads hello.hs
2. Type checker verifies that types are correct
3. Compiler translates to native code (very efficient)
4. You run the binary
5. The `main` IO action executes
6. Output appears on your screen

**Why the type signature?** Haskell is *pure functional* - most functions can't do I/O. The type `IO ()` explicitly marks that `main` can interact with the outside world.

---

## Side-by-Side Comparison

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| **Paradigm** | Multi-paradigm | Multi-paradigm (OOP focus) | Pure functional |
| **Typing** | Dynamic (types checked at runtime) | Static (types checked at compile time) | Static with inference |
| **Compilation** | Interpreted (bytecode) | Compiled (native code) | Compiled (native code) |
| **Syntax** | Minimal, clean | Verbose, explicit | Concise, mathematical |
| **Entry point** | Top of file | `main()` function | `main` definition |
| **Output** | `print()` function | `std::cout <<` stream | `putStrLn` function |
| **Boilerplate** | None | Medium (includes, main) | Minimal (type signature) |
| **Lines of code** | 1 | 5 | 2 |

**Critical insight**: All three programs do *exactly the same thing*, but the *how* reveals the language's philosophy:
- **Python**: "Be readable and practical"
- **C++**: "Be explicit and give me control"
- **Haskell**: "Be mathematically rigorous and pure"

## Development Workflow Comparison

### Python Workflow (Fast iteration)
```bash
# Edit hello.py
python3 hello.py          # Run immediately
# See output
# Edit again
python3 hello.py          # Run again
```
**No compilation step = fast feedback loop**

### C++ Workflow (Compile, then run)
```bash
# Edit hello.cpp
g++ -std=c++17 hello.cpp -o hello    # Compile
./hello                              # Run
# See output
# Edit again
g++ -std=c++17 hello.cpp -o hello    # Must recompile
./hello
```
**Compilation catches errors early, but slows iteration**

### Haskell Workflow (Hybrid)
```bash
# Fast iteration during development
runhaskell hello.hs       # Interpreted mode

# Or compile for production
ghc hello.hs -o hello     # Compiled mode
./hello
```
**Best of both worlds: REPL for exploration, compilation for performance**

## Interactive Exploration

### Python REPL
```bash
$ python3
>>> print("Hello, World!")
Hello, World!
>>> x = 42
>>> print(x)
42
```

### Haskell REPL (GHCi)
```bash
$ ghci
ghci> putStrLn "Hello, World!"
Hello, World!
ghci> let x = 42
ghci> x
42
```

### C++ (No standard REPL, but try cling)
C++ traditionally doesn't have a REPL because of compilation requirements, but tools like `cling` exist. Mostly you compile and run.

**Insight**: REPLs encourage *experimentation*. Languages without them encourage *planning*.

## Looking Ahead: Other Languages

Later in this course, you'll meet four more languages. Here's a preview of Hello World in each:

### Racket (Lisp/Scheme - Week 7)
```racket
#lang racket
(displayln "Hello, World!")
```
*Code as data, parentheses everywhere, homoiconicity*

### C (Week 8)
```c
#include <stdio.h>
int main() {
    printf("Hello, World!\n");
    return 0;
}
```
*Like C++ but more primitive, manual everything*

### Rust (Week 9-10)
```rust
fn main() {
    println!("Hello, World!");
}
```
*Modern systems language with safety guarantees*

### Prolog (Week 9-10)
```prolog
:- initialization(main).
main :- write('Hello, World!'), nl.
```
*Logic programming - you describe what's true, not what to do*

**Why not learn all seven now?** Cognitive load. We want you to achieve *proficiency* in Python, C++, and Haskell first. The others will make more sense once you have a foundation.

## Discussion Questions

1. **Why does Python's simplicity matter?** When might you choose Python over C++?

2. **What does the type signature `main :: IO ()` tell you in Haskell?** Why does Haskell require it when Python doesn't?

3. **Compiled vs. Interpreted**: What are the tradeoffs? When would you prefer compilation? When interpretation?

4. **Boilerplate**: C++ requires `#include`, `main()`, `return 0`. Haskell requires a type signature. Python requires nothing. What does this tell you about the language designers' priorities?

5. **Syntax differences**: `print()` vs. `std::cout <<` vs. `putStrLn`. These all do the same thing. Why do they *look* so different?

## Exercises

### Exercise 1: Customize Your Greeting (Very Easy)
Modify each Hello World program to print your name instead.

**In Python:**
```python
print("Hello, [Your Name]!")
```

**In C++:**
```cpp
std::cout << "Hello, [Your Name]!" << std::endl;
```

**In Haskell:**
```haskell
putStrLn "Hello, [Your Name]!"
```

**Goal**: Get comfortable editing and running programs in all three languages.

---

### Exercise 2: Multiple Lines (Easy)
Write a program that prints three lines:
```
Hello, World!
Welcome to Computer Science
This is my first program
```

**Hints:**
- **Python**: Call `print()` three times
- **C++**: Use `std::cout <<` three times, or use `\n` for newlines
- **Haskell**: Call `putStrLn` three times, or use `do` notation (we'll learn this soon)

**Challenge**: Find multiple ways to do this in each language.

---

### Exercise 3: Comments Everywhere (Easy)
Add comments to your Hello World programs explaining what each line does.

**In Python:**
```python
# This is the entry point of the program
print("Hello, World!")  # Output text to the screen
```

**In C++:**
```cpp
// Include the input/output stream library
#include <iostream>

// Main function - program starts here
int main() {
    // Output "Hello, World!" followed by a newline
    std::cout << "Hello, World!" << std::endl;

    // Return 0 to indicate successful execution
    return 0;
}
```

**In Haskell:**
```haskell
-- Type signature: main is an IO action that returns nothing
main :: IO ()
-- Define main as the action of putting a line of text
main = putStrLn "Hello, World!"
```

**Goal**: Practice writing clear comments in different syntax styles.

---

### Exercise 4: Compilation Investigation (Medium)
For the compiled languages (C++ and Haskell):

1. Compile the program
2. Look at the file size of the resulting executable
3. Try to open the executable in a text editor (it will look like gibberish - that's machine code!)
4. Now look at the source file size
5. Compare: How much bigger is the compiled executable?

**Questions:**
- Why is the executable so much larger than the source?
- What's inside that executable file?
- Could you run the executable on a different computer without the compiler?

**Goal**: Understand what compilation actually produces.

---

### Exercise 5: Timing Comparison (Medium)
Write a program that prints "Hello, World!" 10,000 times (use a loop - we'll learn these next lesson, but try to figure it out!).

**Python:**
```python
for i in range(10000):
    print("Hello, World!")
```

**C++:**
```cpp
#include <iostream>
int main() {
    for (int i = 0; i < 10000; i++) {
        std::cout << "Hello, World!" << std::endl;
    }
    return 0;
}
```

**Haskell:**
```haskell
main :: IO ()
main = mapM_ putStrLn (replicate 10000 "Hello, World!")
```

**Measure execution time:**
```bash
time python3 hello_loop.py
time ./hello_loop_cpp
time ./hello_loop_haskell
```

**Questions:**
- Which is fastest? Why?
- Which was easiest to write?
- What tradeoffs do you notice?

---

### Exercise 6: ASCII Art (Challenge)
Create a program that prints ASCII art. Get creative!

Example:
```
    *
   ***
  *****
 *******
*********
    |
```

**Implementation notes:**
- In Python: Just use `print()` for each line
- In C++: Use `std::cout <<` with `\n` or `std::endl`
- In Haskell: Use `putStrLn` for each line, or `mapM_ putStrLn [line1, line2, ...]`

**Challenge**: Make it print your initials or a simple picture.

---

### Exercise 7: Error Exploration (Medium)
Intentionally break each program and see what error messages you get:

**Try these mistakes:**

1. **Syntax error**: Remove a quotation mark
   - Python: `print("Hello, World!)`
   - C++: `std::cout << "Hello, World!;`
   - Haskell: `putStrLn "Hello, World!`

2. **Missing include/import**: Remove the `#include` in C++

3. **Wrong function name**: Change `print` to `pint` in Python

**Questions:**
- Which language gives the most helpful error messages?
- Which errors are caught at compile-time vs. runtime?
- Can you understand the error messages?

**Goal**: Get comfortable reading error messages. They're your friends!

---

### Exercise 8: Comparison Table (Medium)
Create a table comparing all three languages on these dimensions:

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| Type system | ? | ? | ? |
| Paradigm | ? | ? | ? |
| Speed | ? | ? | ? |
| Ease of learning | ? | ? | ? |
| When to use | ? | ? | ? |

Research and fill in the table based on what you've learned.

---

### Exercise 9: Language Detective (Challenge)
Given code snippets in unknown languages, can you identify which language it is?

```
1. puts "Hello, World!"
2. console.log("Hello, World!");
3. fmt.Println("Hello, World!")
4. printf("Hello, World!\n");
```

**Hints**: Some are languages we haven't covered yet. Use Google and reasoning!

---

### Exercise 10: Reflection (Important!)
Answer these questions in writing:

1. Which language felt most comfortable to you? Why?
2. Which language felt most confusing? What confused you?
3. What similarities did you notice across all three languages?
4. What differences surprised you the most?
5. Based on this lesson, what do you think "Computer Science is language-agnostic" means?

**Goal**: Reflect on your learning and start building language-agnostic intuition.

---

## Key Takeaways

1. **Same Concept, Different Syntax**: All three programs do the same thing, but express it differently
2. **Compiled vs. Interpreted**: Tradeoff between speed (compiled) and flexibility (interpreted)
3. **Paradigm Hints**: Even "Hello, World!" reveals paradigm differences (IO type in Haskell!)
4. **No "Best" Language**: Python is simpler, C++ gives more control, Haskell is more rigorous - all valid choices
5. **Learning to Learn**: Recognizing patterns across languages is a crucial skill

## Next Steps

- **Install all three language environments** if you haven't already (see GETTING_STARTED.md)
- **Get comfortable running programs** in each language
- **Experiment in the REPLs** (Python and Haskell)
- **Don't worry about understanding everything** - we'll revisit these concepts

**Next lesson**: Variables and Types - How languages handle data differently

---

## For Instructors

**Teaching Tips:**

1. **Live demo all three workflows** - show the edit-run cycle
2. **Use the REPL extensively** - Python and GHCi for immediate feedback
3. **Emphasize concepts over syntax** - "output to screen" not "print vs cout"
4. **Celebrate confusion** - It's okay to be overwhelmed! That's part of learning
5. **Pair programming** - Have students work in pairs, switching languages

**Common Student Questions:**

**Q: "Why do I need to learn three languages?"**
A: To learn programming concepts that transcend any single language. You're learning to think, not just to code.

**Q: "Which language should I focus on?"**
A: Start with Python (most accessible), but spend time with all three. The comparisons deepen understanding.

**Q: "Haskell's type signature is confusing!"**
A: Totally normal! It will make sense later. For now, just know it marks IO actions.

**Q: "C++ has so much boilerplate!"**
A: Yes, and that's intentional. C++ makes you be explicit. That's valuable for certain tasks.

**Common Misconceptions:**

❌ "Python is easier, so it's better"
✅ Python is more *accessible*, but all three have their place

❌ "Compiled languages are always faster"
✅ Generally true, but modern interpreters are very fast, and poorly written C++ can be slow

❌ "I need to memorize all the syntax"
✅ No! Focus on *concepts*. You can look up syntax.

**Pacing**: This lesson should take 1-2 class periods (2-4 hours total including exercises).

**Assessment**: Check that students can run all three programs and explain the compilation difference.
