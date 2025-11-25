# Lab 1: Environment Setup Party

**Quarter 1, Week 1**
**Duration:** 90-120 minutes
**Format:** Individual with peer support

## Overview

Getting your development environment set up is the first real programming task you'll complete. This lab guides you through installing Python, C++, and Haskell, verifying everything works, and running your first programs.

## Objectives

By the end of this lab, you will:
- [ ] Have Python 3 installed and working
- [ ] Have a C++ compiler installed and working
- [ ] Have Haskell (GHC) installed and working
- [ ] Successfully run Hello World in all three languages
- [ ] Be comfortable with the basic development workflow for each

## What You'll Need

- A computer (macOS, Linux, or Windows)
- Internet connection
- About 2GB of free disk space
- Patience (installations can be slow!)

---

## Part 1: Python Setup (20 minutes)

### Step 1.1: Check if Python is installed

Open your terminal (Terminal on Mac/Linux, Command Prompt or PowerShell on Windows) and type:

```bash
python3 --version
```

**If you see** `Python 3.x.x` (where x is any number), Python is installed! Skip to Step 1.3.

**If you see** an error or `Python 2.x.x`, continue to Step 1.2.

### Step 1.2: Install Python

**macOS:**
```bash
# Option A: Using Homebrew (recommended if you have it)
brew install python3

# Option B: Download from python.org
# Visit https://www.python.org/downloads/ and download the installer
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt update
sudo apt install python3 python3-pip
```

**Windows:**
1. Visit https://www.python.org/downloads/
2. Download the latest Python 3 installer
3. Run the installer
4. **IMPORTANT**: Check "Add Python to PATH" during installation

### Step 1.3: Verify Python

```bash
python3 --version
# Should show: Python 3.x.x

python3 -c "print('Hello from Python!')"
# Should print: Hello from Python!
```

### Step 1.4: Try the Python REPL

```bash
python3
```

You should see a `>>>` prompt. Try:

```python
>>> 2 + 2
4
>>> print("Hello, World!")
Hello, World!
>>> exit()
```

### ✅ Checkpoint 1

Call over an instructor or peer to verify:
- [ ] `python3 --version` shows Python 3.8 or higher
- [ ] You can enter and exit the Python REPL

---

## Part 2: C++ Setup (25 minutes)

### Step 2.1: Check if a C++ compiler is installed

```bash
g++ --version
```

**If you see** version information, you have a compiler! Skip to Step 2.3.

**If you see** an error, continue to Step 2.2.

### Step 2.2: Install C++ compiler

**macOS:**
```bash
xcode-select --install
```
Click "Install" when prompted (this may take a while).

**Linux (Ubuntu/Debian):**
```bash
sudo apt update
sudo apt install build-essential
```

**Windows:**
- **Option A**: Install MinGW-w64 from https://www.mingw-w64.org/
- **Option B**: Install Visual Studio Community with C++ support
- **Option C**: Use WSL (Windows Subsystem for Linux) and follow Linux instructions

### Step 2.3: Verify C++ compiler

```bash
g++ --version
# Should show version information
```

### Step 2.4: Create and run a C++ program

Create a file called `hello.cpp` with this content:

```cpp
#include <iostream>

int main() {
    std::cout << "Hello from C++!" << std::endl;
    return 0;
}
```

Compile and run:

```bash
g++ -std=c++17 hello.cpp -o hello
./hello
# Should print: Hello from C++!
```

**Windows note:** Use `hello.exe` instead of `./hello`

### ✅ Checkpoint 2

Call over an instructor or peer to verify:
- [ ] `g++ --version` shows version information
- [ ] You can compile and run hello.cpp

---

## Part 3: Haskell Setup (30 minutes)

Haskell setup is the trickiest. Take your time!

### Step 3.1: Install GHCup (recommended method)

GHCup is the recommended installer for Haskell. It manages GHC (the compiler) and related tools.

**macOS/Linux:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Follow the prompts. When asked:
- Install HLS (Haskell Language Server)? Yes (recommended)
- Install Stack? Optional (we won't need it immediately)

**After installation, restart your terminal** or run:
```bash
source ~/.ghcup/env
```

**Windows:**
1. Visit https://www.haskell.org/ghcup/
2. Download and run the Windows installer
3. Follow the prompts

### Step 3.2: Verify Haskell installation

```bash
ghc --version
# Should show: The Glorious Glasgow Haskell Compilation System, version 9.x.x

ghci --version
# Should show: GHCi, version 9.x.x
```

### Step 3.3: Try GHCi (Haskell REPL)

```bash
ghci
```

You should see a `ghci>` prompt. Try:

```haskell
ghci> 2 + 2
4
ghci> putStrLn "Hello, World!"
Hello, World!
ghci> :quit
```

### Step 3.4: Create and run a Haskell program

Create a file called `hello.hs` with this content:

```haskell
main :: IO ()
main = putStrLn "Hello from Haskell!"
```

Run interpreted:
```bash
runhaskell hello.hs
# Should print: Hello from Haskell!
```

Compile and run:
```bash
ghc hello.hs -o hello_hs
./hello_hs
# Should print: Hello from Haskell!
```

### ✅ Checkpoint 3

Call over an instructor or peer to verify:
- [ ] `ghc --version` shows version information
- [ ] You can enter and exit GHCi
- [ ] You can run hello.hs with runhaskell
- [ ] You can compile hello.hs with ghc

---

## Part 4: All Together Now! (15 minutes)

### Step 4.1: Create a project folder

```bash
mkdir ~/cs101
cd ~/cs101
```

### Step 4.2: Create Hello World in all three languages

Create three files in your cs101 folder:

**hello.py:**
```python
print("Hello, World!")
```

**hello.cpp:**
```cpp
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}
```

**hello.hs:**
```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

### Step 4.3: Run all three

```bash
# Python
python3 hello.py

# C++
g++ -std=c++17 hello.cpp -o hello_cpp
./hello_cpp

# Haskell
runhaskell hello.hs
```

### Step 4.4: Compare the experience

Think about:
- Which was easiest to run?
- Which required the most steps?
- Which gave the fastest output?

### ✅ Checkpoint 4 (Final)

Verify with an instructor:
- [ ] All three programs run successfully
- [ ] You understand the different workflows

---

## Part 5: Editor Setup (Optional, 10 minutes)

If time permits, set up a code editor:

### VS Code (Recommended)

1. Download from https://code.visualstudio.com/
2. Install these extensions:
   - Python
   - C/C++
   - Haskell (or Haskell Syntax Highlighting)

### Verify syntax highlighting

1. Open your hello.py in VS Code
2. You should see colors indicating Python syntax
3. Try the same with hello.cpp and hello.hs

---

## Troubleshooting

### Python Issues

**"python3 not found"**
- Try just `python` instead
- Make sure Python is in your PATH
- Restart your terminal after installation

### C++ Issues

**"g++ not found"**
- macOS: Run `xcode-select --install` again
- Linux: Run `sudo apt install build-essential`
- Windows: Check that MinGW bin folder is in PATH

**Compilation errors**
- Make sure you copied the code exactly
- Check for typos in filename (`.cpp` not `.c`)

### Haskell Issues

**"ghc not found" after installing GHCup**
- Run `source ~/.ghcup/env`
- Or restart your terminal completely
- Check that `~/.ghcup/bin` is in your PATH

**"Could not find module" errors**
- Your Haskell installation might be incomplete
- Try reinstalling with GHCup

---

## Extensions (If You Finish Early)

### Extension 1: Personalize your programs

Change each Hello World to print your name and something interesting about you.

### Extension 2: Explore the REPLs

Spend 10 minutes in each REPL (Python and GHCi) trying:
- Math operations
- String operations
- Defining variables

### Extension 3: Find the differences

Create a file that documents:
- How many lines of code for Hello World in each language?
- How many steps to run each?
- What error message does each give for a typo?

---

## Wrap-Up

Congratulations! You now have a working development environment for three different programming languages. This setup will serve you throughout the entire course.

**Remember:**
- Python: Quick and easy, just `python3 script.py`
- C++: Compile first with `g++`, then run the executable
- Haskell: Can interpret with `runhaskell` or compile with `ghc`

**Next lab:** We'll explore types and see how differently these three languages handle data!
