# Lesson 1: Hello, World! - Exercises

## Instructions

Complete these exercises in **all three languages** (Python, C++, Haskell) unless otherwise specified. The goal is to get comfortable with different development workflows and see how the same concepts appear across languages.

**Setup checklist:**
- [ ] Python 3 installed (`python3 --version`)
- [ ] C++ compiler installed (`g++ --version`)
- [ ] Haskell/GHC installed (`ghc --version`)

---

## Warmup Exercises

### Exercise 1: Customize Your Greeting

**Difficulty:** Very Easy | **Time:** 5 minutes per language

Modify the Hello World program to print your name instead.

**Python** (`greeting.py`):
```python
print("Hello, [Your Name]!")
```

**C++** (`greeting.cpp`):
```cpp
#include <iostream>

int main() {
    std::cout << "Hello, [Your Name]!" << std::endl;
    return 0;
}
```

**Haskell** (`greeting.hs`):
```haskell
main :: IO ()
main = putStrLn "Hello, [Your Name]!"
```

**Tasks:**
1. Create and run each file
2. Verify you see your name in the output
3. Try running Haskell both ways: `runhaskell greeting.hs` and compiled with `ghc`

**Reflection:** Which workflow felt most natural to you?

---

### Exercise 2: Multiple Lines

**Difficulty:** Easy | **Time:** 10 minutes per language

Write a program that prints exactly this output:
```
Hello, World!
Welcome to Computer Science
This is my first program
```

**Approach A - Multiple statements:**

**Python:**
```python
print("Hello, World!")
print("Welcome to Computer Science")
print("This is my first program")
```

**C++:**
```cpp
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    std::cout << "Welcome to Computer Science" << std::endl;
    std::cout << "This is my first program" << std::endl;
    return 0;
}
```

**Haskell:**
```haskell
main :: IO ()
main = do
    putStrLn "Hello, World!"
    putStrLn "Welcome to Computer Science"
    putStrLn "This is my first program"
```

**Approach B - Single statement with newlines:**

Try rewriting each program using `\n` (newline character) to print all three lines with a single output statement.

**Questions:**
- What does `\n` do?
- What's the difference between `endl` and `\n` in C++?
- What does the `do` keyword mean in Haskell?

---

### Exercise 3: Comments Practice

**Difficulty:** Easy | **Time:** 10 minutes

Add comments to explain every line of your Hello World programs.

**Python comments:**
```python
# Single line comment
print("Hello")  # Inline comment

"""
Multi-line comment
(actually a docstring)
"""
```

**C++ comments:**
```cpp
// Single line comment
std::cout << "Hello";  // Inline comment

/*
   Multi-line comment
   in C++
*/
```

**Haskell comments:**
```haskell
-- Single line comment

{-
   Multi-line comment
   in Haskell
-}
```

**Task:** Write a fully-commented version of Hello World in each language, explaining what every line does as if teaching someone who has never programmed.

---

## Core Exercises

### Exercise 4: Compilation Investigation

**Difficulty:** Medium | **Time:** 15 minutes

Investigate what compilation actually produces.

**Steps:**
1. Write a simple Hello World in C++ and Haskell
2. Compile both:
   ```bash
   g++ -std=c++17 hello.cpp -o hello_cpp
   ghc hello.hs -o hello_hs
   ```
3. Check file sizes:
   ```bash
   ls -la hello.cpp hello_cpp
   ls -la hello.hs hello_hs
   ```
4. Try to view the executables:
   ```bash
   file hello_cpp
   file hello_hs
   head -c 100 hello_cpp  # First 100 bytes (will look like garbage)
   ```

**Questions to answer:**
1. How much larger is the executable compared to the source code?
2. Why is the executable so much bigger?
3. What does the `file` command tell you about the executable?
4. Could you run `hello_cpp` on a friend's computer without installing g++?
5. Could you run `hello.py` on a friend's computer without installing Python?

---

### Exercise 5: Error Messages

**Difficulty:** Medium | **Time:** 20 minutes

Intentionally break programs to learn from error messages.

**Syntax Errors - Try these mistakes:**

| Error Type | Python | C++ | Haskell |
|------------|--------|-----|---------|
| Missing quote | `print("Hello)` | `cout << "Hello;` | `putStrLn "Hello` |
| Typo in function | `prnt("Hello")` | `cot << "Hello";` | `putStrn "Hello"` |
| Missing semicolon | N/A | `cout << "Hello"` | N/A |
| Missing include | N/A | Remove `#include` | N/A |
| Wrong indentation | Mess up spacing | N/A | N/A |

**For each error:**
1. Create the broken code
2. Try to run/compile it
3. Read the error message carefully
4. Write down what the error message says
5. Fix the error

**Record your findings:**

| Language | Error | Message Summary | Helpful? (1-5) |
|----------|-------|-----------------|----------------|
| Python | Missing quote | ? | ? |
| C++ | Missing quote | ? | ? |
| Haskell | Missing quote | ? | ? |

**Reflection:** Which language gives the most helpful error messages? The least helpful?

---

### Exercise 6: Timing Comparison

**Difficulty:** Medium | **Time:** 20 minutes

Compare execution speed across languages.

**Create these files:**

**Python** (`loop.py`):
```python
for i in range(100000):
    print("Hello, World!")
```

**C++** (`loop.cpp`):
```cpp
#include <iostream>

int main() {
    for (int i = 0; i < 100000; i++) {
        std::cout << "Hello, World!\n";
    }
    return 0;
}
```

**Haskell** (`loop.hs`):
```haskell
import Control.Monad (replicateM_)

main :: IO ()
main = replicateM_ 100000 (putStrLn "Hello, World!")
```

**Time each one:**
```bash
time python3 loop.py > /dev/null
time ./loop_cpp > /dev/null
time ./loop_hs > /dev/null
```

(The `> /dev/null` suppresses output so we only measure computation time)

**Record results:**

| Language | Real Time | User Time | Sys Time |
|----------|-----------|-----------|----------|
| Python | ? | ? | ? |
| C++ | ? | ? | ? |
| Haskell | ? | ? | ? |

**Questions:**
1. Which was fastest? By how much?
2. Which was easiest to write?
3. Is speed always the most important factor?

---

## Challenge Exercises

### Exercise 7: ASCII Art

**Difficulty:** Challenge | **Time:** 30 minutes

Create a program that prints ASCII art. Be creative!

**Example - A tree:**
```
    *
   ***
  *****
 *******
*********
    |
    |
```

**Example - A house:**
```
   /\
  /  \
 /    \
+------+
|  []  |
|      |
+------+
```

**Requirements:**
1. Create the same ASCII art in all three languages
2. Use at least 5 lines of output
3. Make it your own - initials, favorite animal, abstract pattern

**Bonus challenges:**
- Use a loop to generate a pattern (like the tree)
- Create different art in each language showcasing their strengths

---

### Exercise 8: Language Detective

**Difficulty:** Challenge | **Time:** 15 minutes

Identify these mystery languages based on their Hello World programs:

```
1. puts "Hello, World!"

2. console.log("Hello, World!");

3. fmt.Println("Hello, World!")

4. System.out.println("Hello, World!");

5. print("Hello, World!")   -- Note: Not Python!

6. echo "Hello, World!"

7. IO.puts("Hello, World!")

8. (print "Hello, World!")
```

**Hints:**
- Use the syntax as clues (semicolons, parentheses, quotes)
- Google is allowed!
- Think about what language communities might use certain conventions

**Answers to research:**

| # | Language | How did you figure it out? |
|---|----------|---------------------------|
| 1 | ? | ? |
| 2 | ? | ? |
| 3 | ? | ? |
| 4 | ? | ? |
| 5 | ? | ? |
| 6 | ? | ? |
| 7 | ? | ? |
| 8 | ? | ? |

---

### Exercise 9: Build Your Own Comparison Table

**Difficulty:** Challenge | **Time:** 20 minutes

Research and complete this comparison table:

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| Year created | ? | ? | ? |
| Creator(s) | ? | ? | ? |
| Primary paradigm | ? | ? | ? |
| Type system | ? | ? | ? |
| Memory management | ? | ? | ? |
| Common uses | ? | ? | ? |
| Famous projects using it | ? | ? | ? |

**Sources to consult:**
- Wikipedia pages for each language
- Official language websites
- "Awesome [language]" GitHub lists

---

## Reflection Exercise (Required)

### Exercise 10: Learning Reflection

**Difficulty:** N/A | **Time:** 15 minutes

Write thoughtful answers to these questions (at least 2-3 sentences each):

1. **Comfort level:** Which language felt most comfortable to you? Why do you think that is?

2. **Challenge:** Which language felt most confusing or challenging? What specifically was difficult?

3. **Similarities:** What similarities did you notice across all three languages?

4. **Differences:** What differences surprised you the most?

5. **Workflow:** Which development workflow (edit-run for Python, edit-compile-run for C++) did you prefer? Why?

6. **Looking ahead:** Based on this first lesson, what are you most curious to learn more about?

7. **CS is language-agnostic:** What do you think this phrase means after completing these exercises?

---

## Self-Assessment Checklist

Before moving to Lesson 2, make sure you can:

- [ ] Write and run a Hello World program in Python
- [ ] Write, compile, and run a Hello World program in C++
- [ ] Write and run a Hello World program in Haskell (interpreted and compiled)
- [ ] Explain the difference between compiled and interpreted languages
- [ ] Write comments in all three languages
- [ ] Read and understand basic error messages
- [ ] Use a REPL (Python or GHCi)

**If you're struggling with any of these, that's okay!** Re-read the lesson, try the exercises again, or ask for help. Building a solid foundation now will make everything else easier.

---

## Optional: Going Further

Completed everything? Try these extensions:

1. **Explore REPLs deeply:** Spend 15 minutes in Python REPL and GHCi just experimenting. Try arithmetic, string operations, anything you're curious about.

2. **Read about language history:** Why was Haskell created? What problems was C++ designed to solve? What was Python's original purpose?

3. **Look at real code:** Find a small open-source project in each language on GitHub. Can you identify the Hello World equivalent (the entry point)?

4. **Try online compilers:** Use repl.it or similar to run code in languages you don't have installed locally.
