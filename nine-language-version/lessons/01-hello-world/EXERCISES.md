# Lesson 1: Hello World - Exercises

## Instructions

These exercises will help you get comfortable with running programs in different languages and understanding the basics of compilation vs interpretation.

---

## Exercise 1: Customize Your Greeting

**Difficulty:** Very Easy

Modify the "Hello, World!" programs to print a personalized greeting.

**Tasks:**
1. Change "Hello, World!" to "Hello, [Your Name]!"
2. Run the program in at least 3 different languages
3. Note any differences in how you ran each program

**Languages to try:** Python, JavaScript, and one compiled language (C, Java, Rust, or Haskell)

---

## Exercise 2: Multiple Lines

**Difficulty:** Easy

Modify the programs to print multiple lines:
```
Hello, World!
Welcome to polyglot programming!
Let's learn 9 languages!
```

**Tasks:**
1. Implement in Python
2. Implement in JavaScript
3. Implement in C or Java

**Questions:**
- How does each language handle printing multiple lines?
- Is there a difference between `print` statements and newline characters?

---

## Exercise 3: Compilation vs Interpretation

**Difficulty:** Easy (Conceptual)

**Tasks:**
1. Run the Python "Hello, World!" - note what happens
2. Run the JavaScript "Hello, World!" - note what happens
3. Compile and run the C "Hello, World!" - note the steps
4. Compile and run the Java "Hello, World!" - note the steps

**Questions:**
1. Which languages needed a compilation step?
2. Which languages created an executable file?
3. What's the advantage of compilation? Of interpretation?
4. Which approach feels faster to you? Why?

---

## Exercise 4: Error Messages

**Difficulty:** Easy

Intentionally break each program to see error messages:

1. **Syntax Error:** Remove a semicolon (C, Java) or add a typo
2. **Missing File:** Try to run a program that doesn't exist
3. **Compilation Error:** Try to compile broken code

**Tasks:**
- Break programs in Python, JavaScript, and C
- Read and understand the error messages
- Fix the errors

**Questions:**
- Which language gives the clearest error messages?
- Do compiled languages catch errors earlier than interpreted ones?

---

## Exercise 5: Add Comments

**Difficulty:** Easy

Add comments to your Hello World programs explaining what they do.

**Learn the comment syntax for each language:**
- Python: `#`
- JavaScript: `//` or `/* */`
- C: `//` or `/* */`
- Java: `//` or `/* */`
- Ruby: `#`
- Haskell: `--` or `{- -}`
- Racket: `;`
- Prolog: `%`
- Rust: `//` or `/* */`

**Task:** Add a multi-line comment at the top of each program describing what it does.

---

## Exercise 6: User Input

**Difficulty:** Medium

Modify the programs to ask for the user's name and greet them personally.

**Example (Python):**
```python
name = input("What's your name? ")
print(f"Hello, {name}!")
```

**Tasks:**
1. Implement in Python
2. Implement in JavaScript (use `process.stdin` or `prompt`)
3. Try in C (use `scanf`)

**Challenge:** Handle the case where the user doesn't enter anything.

---

## Exercise 7: Language Comparison Table

**Difficulty:** Medium (Research)

Create a comparison table:

| Language   | File Extension | Run Command | Compiled? | Typed? |
|------------|---------------|-------------|-----------|--------|
| Python     | .py           | python3 ... | No        | Dynamic |
| JavaScript | .js           | node ...    | No        | Dynamic |
| C          | .c            | gcc...; ./..| Yes       | Static  |
| ...        | ...           | ...         | ...       | ...     |

**Task:** Fill in the table for all 9 languages.

---

## Exercise 8: ASCII Art

**Difficulty:** Medium

Create a program that prints ASCII art:

```
  _    _      _ _         __        __         _     _ _
 | |  | |    | | |        \ \      / /__  _ __| | __| | |
 | |__| | ___| | | ___     \ \ /\ / / _ \| '__| |/ _` | |
 |  __  |/ _ \ | |/ _ \     \ V  V / (_) | |  | | (_| |_|
 |_|  |_|\___/_|_|\___( )    \_/\_/ \___/|_|  |_|\__,_(_)
                      |/
```

**Tasks:**
1. Create ASCII art (use an online generator if needed)
2. Print it in Python
3. Print it in JavaScript
4. Print it in C (watch out for escape characters!)

**Challenges:**
- Special characters like `\` need escaping
- Some languages need specific string syntax

---

## Exercise 9: Explore the REPL

**Difficulty:** Easy

Many interpreted languages have a Read-Eval-Print Loop (REPL) for interactive coding.

**Tasks:**
1. Start Python REPL: `python3`
2. Try: `print("Hello!")`, `2 + 2`, `"abc" * 3`
3. Start Node REPL: `node`
4. Try: `console.log("Hello!")`, `2 + 2`, `"abc".repeat(3)`

**Questions:**
- What's the advantage of a REPL?
- Do compiled languages have REPLs? (Hint: Try `ghci` for Haskell!)

---

## Exercise 10: Create a Script

**Difficulty:** Medium

Create a script that:
1. Prints a greeting
2. Waits a moment
3. Prints goodbye

**Example (Python):**
```python
import time

print("Hello!")
time.sleep(2)
print("Goodbye!")
```

**Tasks:**
- Implement in Python
- Implement in JavaScript (`setTimeout` or `sleep`)
- Make it executable with a shebang line (`#!/usr/bin/env python3`)

---

## Challenge Projects

### Challenge 1: Language Auto-Detector

Write a program that:
1. Asks the user which language to greet them in
2. Prints "Hello, World!" in that language (Spanish, French, German, etc.)

### Challenge 2: Rainbow Hello

Print "Hello, World!" in different colors (if your terminal supports it).

**Hint:** Use ANSI escape codes or libraries like `colorama` (Python) or `chalk` (JavaScript).

### Challenge 3: All Languages at Once

Write a script that runs "Hello, World!" in all 9 languages automatically and shows the output.

---

## Reflection Questions

1. Which language was easiest to get started with? Why?

2. Which language had the clearest syntax for beginners?

3. Did you prefer compiled or interpreted languages? Why?

4. What surprised you most about the different languages?

---

## Going Further

- **Explore:** Try writing "Hello, World!" in a language NOT in this course
- **Customize:** Make your greeting program interactive and fun
- **Compare:** Time how long each language takes to print (spoiler: they're all fast!)

Remember: Every expert programmer started by printing "Hello, World!" You're on your way!
