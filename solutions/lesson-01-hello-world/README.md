# Lesson 1: Hello World - Solution Guide

This guide provides example solutions for the Hello World exercises.

## General Notes

- **Multiple correct solutions exist**: Your solution may differ and still be correct
- **Focus on understanding**: Don't just copy-paste; understand why each solution works
- **Language idioms matter**: Each language has its own style
- **Try before looking**: Attempt each exercise before checking solutions

---

## Exercise 1: Customize Your Greeting

**Task:** Change "Hello, World!" to "Hello, [Your Name]!"

### Python Solution

```python
print("Hello, Alice!")
```

**Run:** `python3 hello.py`

### JavaScript Solution

```javascript
console.log("Hello, Alice!");
```

**Run:** `node hello.js`

### C Solution

```c
#include <stdio.h>

int main() {
    printf("Hello, Alice!\n");
    return 0;
}
```

**Compile and run:**
```bash
gcc hello.c -o hello
./hello
```

### Haskell Solution

```haskell
main :: IO ()
main = putStrLn "Hello, Alice!"
```

**Compile and run:**
```bash
ghc hello.hs -o hello
./hello
```

Or run directly: `runhaskell hello.hs`

**Key Differences:**
- Python/JavaScript: No compilation, direct execution
- C: Requires compilation, creates binary executable
- Haskell: Can be compiled OR interpreted with `runhaskell`

---

## Exercise 2: Multiple Lines

**Task:** Print three lines of text

### Python Solution

```python
print("Hello, World!")
print("Welcome to polyglot programming!")
print("Let's learn 9 languages!")
```

**Alternative (single print with newlines):**
```python
print("Hello, World!\nWelcome to polyglot programming!\nLet's learn 9 languages!")
```

**Alternative (triple-quoted string):**
```python
print("""Hello, World!
Welcome to polyglot programming!
Let's learn 9 languages!""")
```

### JavaScript Solution

```javascript
console.log("Hello, World!");
console.log("Welcome to polyglot programming!");
console.log("Let's learn 9 languages!");
```

**Alternative (template literals):**
```javascript
console.log(`Hello, World!
Welcome to polyglot programming!
Let's learn 9 languages!`);
```

### C Solution

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    printf("Welcome to polyglot programming!\n");
    printf("Let's learn 9 languages!\n");
    return 0;
}
```

**Alternative (single printf):**
```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n"
           "Welcome to polyglot programming!\n"
           "Let's learn 9 languages!\n");
    return 0;
}
```

**Language Differences:**
- **Python**: `print()` automatically adds newline, or use `\n` explicitly
- **JavaScript**: `console.log()` adds newline, or use template literals
- **C**: Must explicitly use `\n` for newlines

---

## Exercise 3: Compilation vs Interpretation

**This is a conceptual exercise - no code needed!**

### Expected Observations:

**Python (`python3 hello.py`):**
- No compilation step
- Runs directly from source code
- Creates `.pyc` bytecode files in `__pycache__` (cached compilation)
- Errors shown at runtime when reached

**JavaScript (`node hello.js`):**
- No explicit compilation step
- JIT (Just-In-Time) compilation happens internally
- Runs source code directly
- Modern engines (V8) compile to machine code

**C (`gcc hello.c -o hello; ./hello`):**
- **Two-step process**: Compile, then run
- Creates standalone executable binary
- Compilation catches syntax/type errors before running
- Executable can run without source code or compiler

**Java (`javac Hello.java; java Hello`):**
- Compiles to bytecode (`.class` file)
- Runs on Java Virtual Machine (JVM)
- Hybrid: compiled to bytecode, interpreted/JIT by JVM
- Platform-independent bytecode

**Haskell (`ghc hello.hs -o hello; ./hello`):**
- Compiles to native executable
- Strong type-checking at compile time
- Can also be interpreted with `runhaskell`

### Answers to Questions:

1. **Which needed compilation?** C, Java, Haskell (when using `ghc`), Rust
2. **Which created executable?** C, Haskell, Rust create native binaries; Java creates `.class` bytecode
3. **Advantages:**
   - **Compilation**: Catches errors early, faster execution, optimizations
   - **Interpretation**: Faster development cycle, easier debugging, platform independence
4. **Which feels faster?** Interpreted languages feel faster to develop (no compile step), but compiled programs run faster

---

## Exercise 4: Error Messages

**Task:** Break programs intentionally to see errors

### Examples of Errors:

#### Python Syntax Error:

**Broken code:**
```python
print("Hello, World!"
```

**Error message:**
```
  File "hello.py", line 1
    print("Hello, World!"
                        ^
SyntaxError: unexpected EOF while parsing
```

**What it means:** Missing closing parenthesis

#### JavaScript Syntax Error:

**Broken code:**
```javascript
console.log("Hello, World!);
```

**Error message:**
```
SyntaxError: Invalid or unexpected token
```

**What it means:** Unterminated string (missing closing quote)

#### C Compilation Error:

**Broken code:**
```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n")  // Missing semicolon
    return 0;
}
```

**Error message:**
```
hello.c:5:5: error: expected ';' before 'return'
     return 0;
     ^~~~~~
```

**What it means:** The compiler expected a semicolon before the `return` statement

### Learning Points:

- **Compiled languages** (C, Haskell, Rust) catch errors at compile time
- **Interpreted languages** (Python, JavaScript) catch errors at runtime
- **Python** has relatively clear error messages for beginners
- **C** error messages can be cryptic but point to line numbers
- **Haskell** has very detailed type error messages (sometimes overwhelming)

### Tips for Reading Errors:

1. **Look at the line number** - errors are often near (but not always exactly at) this line
2. **Read the error type** - `SyntaxError`, `TypeError`, `NameError`, etc.
3. **Look for the caret (^)** - points to where the error was detected
4. **Read the message** - it often tells you exactly what's wrong
5. **Google it** - error messages are searchable!

---

## Exercise 5: Add Comments

**Task:** Add comments explaining code

### Python Solution

```python
#!/usr/bin/env python3
"""
Hello World program in Python
This program prints a greeting to the console.
"""

# Print a greeting to the user
print("Hello, World!")
```

**Comment styles:**
- `#` for single-line comments
- `"""..."""` or `'''...'''` for multi-line docstrings

### JavaScript Solution

```javascript
#!/usr/bin/env node
/*
 * Hello World program in JavaScript
 * This program prints a greeting to the console.
 */

// Print a greeting to the user
console.log("Hello, World!");
```

**Comment styles:**
- `//` for single-line comments
- `/* ... */` for multi-line comments

### C Solution

```c
/*
 * Hello World program in C
 * This program prints a greeting to the console.
 */

#include <stdio.h>

int main() {
    // Print a greeting to the user
    printf("Hello, World!\n");
    return 0;
}
```

### Haskell Solution

```haskell
{-
 - Hello World program in Haskell
 - This program prints a greeting to the console.
 -}

-- Main function - entry point of the program
main :: IO ()
main = putStrLn "Hello, World!"
```

**Comment styles:**
- `--` for single-line comments
- `{- ... -}` for multi-line comments

---

## Exercise 6: User Input

**Task:** Ask for user's name and greet them

### Python Solution

```python
# Ask for the user's name
name = input("What's your name? ")

# Handle empty input
if name:
    print(f"Hello, {name}!")
else:
    print("Hello, stranger!")
```

**Alternative using walrus operator (Python 3.8+):**
```python
if (name := input("What's your name? ")):
    print(f"Hello, {name}!")
else:
    print("Hello, stranger!")
```

### JavaScript Solution (Node.js with readline)

```javascript
const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

rl.question("What's your name? ", (name) => {
    if (name) {
        console.log(`Hello, ${name}!`);
    } else {
        console.log("Hello, stranger!");
    }
    rl.close();
});
```

**Simpler alternative using prompt-sync:**
```javascript
// First: npm install prompt-sync
const prompt = require('prompt-sync')();

const name = prompt("What's your name? ");
if (name) {
    console.log(`Hello, ${name}!`);
} else {
    console.log("Hello, stranger!");
}
```

### C Solution

```c
#include <stdio.h>
#include <string.h>

int main() {
    char name[100];

    printf("What's your name? ");
    fgets(name, sizeof(name), stdin);

    // Remove newline character if present
    name[strcspn(name, "\n")] = '\0';

    if (strlen(name) > 0) {
        printf("Hello, %s!\n", name);
    } else {
        printf("Hello, stranger!\n");
    }

    return 0;
}
```

**Notes:**
- `fgets` is safer than `scanf` for string input
- Must manually remove newline character
- Buffer size must be specified to prevent overflow

---

## Exercise 7: Language Comparison Table

**Task:** Create comparison table for all 9 languages

### Complete Table:

| Language   | File Extension | Run Command | Compiled? | Typed? | Paradigm |
|------------|---------------|-------------|-----------|--------|----------|
| Python     | .py           | `python3 file.py` | No (bytecode) | Dynamic | Multi-paradigm |
| JavaScript | .js           | `node file.js` | No (JIT) | Dynamic | Multi-paradigm |
| C          | .c            | `gcc file.c -o prog; ./prog` | Yes | Static | Imperative |
| Java       | .java         | `javac File.java; java File` | Yes (bytecode) | Static | OOP |
| Ruby       | .rb           | `ruby file.rb` | No | Dynamic | OOP |
| Haskell    | .hs           | `ghc file.hs -o prog; ./prog` | Yes (or `runhaskell`) | Static | Functional |
| Racket     | .rkt          | `racket file.rkt` | No (JIT) | Dynamic | Functional |
| Prolog     | .pl           | `swipl -s file.pl` | No | Dynamic | Logic |
| Rust       | .rs           | `rustc file.rs -o prog; ./prog` | Yes | Static | Systems |

**Additional Notes:**

- **Compiled?** Some languages (Python, JavaScript) use intermediate bytecode or JIT compilation
- **Typed?** Static = checked at compile time; Dynamic = checked at runtime
- **Paradigm:** Most modern languages support multiple paradigms
- **REPL?** Python, JavaScript, Ruby, Haskell, Racket, Prolog all have REPLs

---

## Exercise 8: ASCII Art

**Task:** Print ASCII art

### Python Solution

```python
print(r"""
  _    _      _ _         __        __         _     _ _
 | |  | |    | | |        \ \      / /__  _ __| | __| | |
 | |__| | ___| | | ___     \ \ /\ / / _ \| '__| |/ _` | |
 |  __  |/ _ \ | |/ _ \     \ V  V / (_) | |  | | (_| |_|
 |_|  |_|\___/_|_|\___( )    \_/\_/ \___/|_|  |_|\__,_(_)
                      |/
""")
```

**Note:** The `r` prefix creates a "raw string" where backslashes don't need escaping

### JavaScript Solution

```javascript
console.log(`
  _    _      _ _         __        __         _     _ _
 | |  | |    | | |        \\ \\      / /__  _ __| | __| | |
 | |__| | ___| | | ___     \\ \\ /\\ / / _ \\| '__| |/ _\` | |
 |  __  |/ _ \\ | |/ _ \\     \\ V  V / (_) | |  | | (_| |_|
 |_|  |_|\\___/_|_|\\___( )    \\_/\\_/ \\___/|_|  |_|\\__,_(_)
                      |/
`);
```

**Note:** Template literals (backticks) allow multi-line strings; backslashes still need escaping

### C Solution

```c
#include <stdio.h>

int main() {
    printf("  _    _      _ _         __        __         _     _ _\n");
    printf(" | |  | |    | | |        \\ \\      / /__  _ __| | __| | |\n");
    printf(" | |__| | ___| | | ___     \\ \\ /\\ / / _ \\| '__| |/ _` | |\n");
    printf(" |  __  |/ _ \\ | |/ _ \\     \\ V  V / (_) | |  | | (_| |_|\n");
    printf(" |_|  |_|\\___/_|_|\\___( )    \\_/\\_/ \\___/|_|  |_|\\__,_(_)\n");
    printf("                      |/\n");
    return 0;
}
```

**Note:** Each backslash must be doubled (`\\`) in C strings

**Key Challenges:**
- **Backslashes**: Need escaping in most languages
- **Quotes**: Need escaping if same as string delimiter
- **Multi-line strings**: Different syntax in each language

---

## Exercise 9: Explore the REPL

**This is an interactive exercise - try the commands yourself!**

### Python REPL Examples:

```
$ python3
>>> print("Hello!")
Hello!
>>> 2 + 2
4
>>> "abc" * 3
'abcabcabc'
>>> x = 10
>>> x * 5
50
>>> exit()
```

### JavaScript REPL Examples:

```
$ node
> console.log("Hello!")
Hello!
undefined
> 2 + 2
4
> "abc".repeat(3)
'abcabcabc'
> let x = 10
undefined
> x * 5
50
> .exit
```

### Haskell REPL Examples:

```
$ ghci
Prelude> putStrLn "Hello!"
Hello!
Prelude> 2 + 2
4
Prelude> replicate 3 "abc"
["abc","abc","abc"]
Prelude> let x = 10
Prelude> x * 5
50
Prelude> :quit
```

**Advantages of REPL:**
- Quick experimentation
- Immediate feedback
- No need to create files
- Great for learning and testing

---

## Exercise 10: Create a Script

**Task:** Print greeting, wait, print goodbye

### Python Solution

```python
#!/usr/bin/env python3
import time

print("Hello!")
time.sleep(2)
print("Goodbye!")
```

**Make executable:**
```bash
chmod +x script.py
./script.py
```

### JavaScript Solution

```javascript
#!/usr/bin/env node

console.log("Hello!");

setTimeout(() => {
    console.log("Goodbye!");
}, 2000);
```

**Alternative using async/await:**
```javascript
#!/usr/bin/env node

const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));

async function main() {
    console.log("Hello!");
    await sleep(2000);
    console.log("Goodbye!");
}

main();
```

---

## Challenge Projects

### Challenge 1: Language Auto-Detector

```python
# Python solution
language = input("Which language? (en/es/fr/de): ").lower()

greetings = {
    'en': 'Hello, World!',
    'es': '¡Hola, Mundo!',
    'fr': 'Bonjour, Monde!',
    'de': 'Hallo, Welt!'
}

print(greetings.get(language, 'Hello, World!'))
```

### Challenge 2: Rainbow Hello

```python
# Python solution using colorama
from colorama import Fore, Style, init

init()  # Initialize colorama

colors = [Fore.RED, Fore.YELLOW, Fore.GREEN, Fore.CYAN, Fore.BLUE, Fore.MAGENTA]
text = "Hello, World!"

for i, char in enumerate(text):
    print(colors[i % len(colors)] + char, end='')

print(Style.RESET_ALL)  # Reset colors
```

---

## Common Mistakes

1. **Forgetting shebang permissions**: `chmod +x` needed for executable scripts
2. **Using wrong quotes**: Single vs double quotes matter in some contexts
3. **Missing newlines in C**: Forgetting `\n` in `printf`
4. **Escaping issues**: Forgetting to escape backslashes in strings
5. **REPL vs file execution**: Code that works in REPL might need modification for files

---

## Going Further

- Try writing Hello World in a language not in this course (Go, Swift, Kotlin, etc.)
- Create a program that prints "Hello" in all 9 languages
- Write a script that times how long each language takes to start and print
- Explore the bytecode generated by Python (`.pyc` files)
- Use `strace` or similar tools to see what compiled programs do

Remember: These are example solutions. Your solution can be different and equally correct!
