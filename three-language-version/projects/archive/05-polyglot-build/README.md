# Project 5: Polyglot Build Tool

**Difficulty:** Hard
**Estimated Time:** 8-12 hours
**Prerequisites:** Lessons 1-12, File I/O Module, Testing Module
**Due:** End of Week 13

**Capstone Option:** Implement build tool with Makefile-style dependency tracking

---

## Overview

Create a simple build system that can detect, compile, and run programs in multiple languages based on file extensions. This project reinforces process execution, file system operations, configuration management, and tool building.

---

## Learning Objectives

By completing this project, you will:
- Execute external processes programmatically
- Work with file systems and paths
- Parse configuration files
- Handle multiple languages' build processes
- Implement dependency detection (optional)
- Build developer tools

---

## Requirements

### Basic Requirements (Must Complete)

Your build tool should:

1. **Detect language** from file extension (`.py`, `.cpp`, `.hs`, etc.)
2. **Compile if needed** (C++, Haskell, Rust, C)
3. **Run programs** with appropriate interpreter/executable
4. **Handle arguments** passed through to the program
5. **Report errors** clearly when build/run fails

**Example Session:**
```bash
$ polyglot run hello.py
[Python] Running hello.py...
Hello, World!

$ polyglot run hello.cpp
[C++] Compiling hello.cpp...
[C++] Running hello...
Hello, World!

$ polyglot run hello.hs
[Haskell] Compiling hello.hs...
[Haskell] Running hello...
Hello, World!

$ polyglot run program.py -- arg1 arg2
[Python] Running program.py with args: arg1 arg2
Arguments received: ['arg1', 'arg2']
```

### Standard Requirements (Complete Most)

Extend your tool with:

6. **Build-only mode**: `polyglot build hello.cpp`
7. **Clean mode**: `polyglot clean` removes compiled files
8. **Multiple files**: `polyglot run *.py` runs all Python files
9. **Configuration file**: `.polyglot.yaml` for custom settings
10. **Verbose mode**: `-v` flag shows commands being run

**Extended Example:**
```bash
$ polyglot build hello.cpp
[C++] Compiling hello.cpp...
Built: hello (12,456 bytes)

$ polyglot clean
Removed: hello
Removed: hello.o
Removed: hello.hi
Cleaned 3 files.

$ polyglot run -v hello.cpp
[DEBUG] Detected language: C++
[DEBUG] Command: g++ -std=c++17 hello.cpp -o hello
[C++] Compiling hello.cpp...
[DEBUG] Command: ./hello
[C++] Running hello...
Hello, World!
```

### Advanced Requirements (Challenge)

For additional challenge:

11. **Dependency tracking**: Only recompile if source changed
12. **Multiple source files**: Link multiple `.cpp` files
13. **Custom compilers**: Configure compiler paths/flags
14. **Watch mode**: `polyglot watch hello.cpp` rebuilds on change
15. **Test runner**: `polyglot test` finds and runs test files
16. **Project mode**: Build entire project with `polyglot.yaml`

**Advanced Example:**
```bash
$ polyglot run hello.cpp
[C++] hello.cpp is up-to-date, skipping compilation.
[C++] Running hello...
Hello, World!

$ polyglot watch hello.cpp
[Watch] Watching hello.cpp for changes...
[12:30:15] hello.cpp changed, rebuilding...
[C++] Compiling hello.cpp...
[C++] Running hello...
Hello, World!
[Watch] Waiting for changes...

$ polyglot test
[Test] Found 3 test files
[Python] Running test_math.py... PASS (0.2s)
[C++] Running test_utils.cpp... PASS (0.5s)
[Haskell] Running TestMain.hs... PASS (0.8s)
All tests passed!
```

---

## Technical Specifications

### Supported Languages

| Extension | Language | Compile Command | Run Command |
|-----------|----------|-----------------|-------------|
| `.py` | Python | (none) | `python3 {file}` |
| `.cpp` | C++ | `g++ -std=c++17 {file} -o {output}` | `./{output}` |
| `.c` | C | `gcc {file} -o {output}` | `./{output}` |
| `.hs` | Haskell | `ghc {file} -o {output}` | `./{output}` |
| `.rs` | Rust | `rustc {file} -o {output}` | `./{output}` |
| `.js` | JavaScript | (none) | `node {file}` |
| `.rkt` | Racket | (none) | `racket {file}` |
| `.pl` | Prolog | (none) | `swipl -q -t halt -s {file}` |

### Configuration File (`.polyglot.yaml`)

```yaml
# Custom compiler settings
compilers:
  cpp:
    command: "clang++"
    flags: ["-std=c++20", "-Wall", "-Werror"]
  haskell:
    command: "ghc"
    flags: ["-O2"]

# Default language settings
defaults:
  cpp:
    output_dir: "build"

# File patterns to ignore
ignore:
  - "*.test.py"
  - "build/*"
```

### Process Execution

**Python:**
```python
import subprocess
import sys

def run_command(cmd, capture=False):
    """Execute a command and handle results."""
    result = subprocess.run(
        cmd,
        capture_output=capture,
        text=True
    )
    if result.returncode != 0:
        print(f"Error: Command failed with code {result.returncode}")
        if capture:
            print(result.stderr)
        sys.exit(1)
    return result
```

**C++:**
```cpp
#include <cstdlib>
#include <array>
#include <memory>

std::string exec(const std::string& cmd) {
    std::array<char, 128> buffer;
    std::string result;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd.c_str(), "r"), pclose);
    if (!pipe) {
        throw std::runtime_error("popen() failed!");
    }
    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
    }
    return result;
}
```

**Haskell:**
```haskell
import System.Process
import System.Exit

runCommand :: String -> [String] -> IO ExitCode
runCommand cmd args = do
    (_, _, _, ph) <- createProcess (proc cmd args)
    waitForProcess ph
```

---

## Implementation Guide

### Step 1: Language Detection

```python
LANGUAGE_MAP = {
    '.py': 'python',
    '.cpp': 'cpp',
    '.c': 'c',
    '.hs': 'haskell',
    '.rs': 'rust',
    '.js': 'javascript',
    '.rkt': 'racket',
    '.pl': 'prolog'
}

def detect_language(filepath):
    ext = os.path.splitext(filepath)[1]
    return LANGUAGE_MAP.get(ext)
```

### Step 2: Build System

```python
class Builder:
    def __init__(self, config=None):
        self.config = config or {}

    def needs_compilation(self, lang):
        return lang in ['cpp', 'c', 'haskell', 'rust']

    def get_output_name(self, filepath):
        base = os.path.splitext(filepath)[0]
        return base  # On Windows, add .exe

    def compile(self, filepath, lang):
        output = self.get_output_name(filepath)
        commands = {
            'cpp': ['g++', '-std=c++17', filepath, '-o', output],
            'c': ['gcc', filepath, '-o', output],
            'haskell': ['ghc', filepath, '-o', output],
            'rust': ['rustc', filepath, '-o', output]
        }
        cmd = commands.get(lang)
        if cmd:
            subprocess.run(cmd, check=True)
        return output

    def run(self, filepath, lang, args=None):
        args = args or []
        if self.needs_compilation(lang):
            output = self.compile(filepath, lang)
            subprocess.run(['./' + output] + args)
        else:
            interpreters = {
                'python': ['python3', filepath],
                'javascript': ['node', filepath],
                'racket': ['racket', filepath],
                'prolog': ['swipl', '-q', '-t', 'halt', '-s', filepath]
            }
            cmd = interpreters.get(lang, [])
            subprocess.run(cmd + args)
```

### Step 3: CLI Interface

```python
import argparse

def main():
    parser = argparse.ArgumentParser(description='Polyglot Build Tool')
    subparsers = parser.add_subparsers(dest='command')

    # Run command
    run_parser = subparsers.add_parser('run', help='Build and run a program')
    run_parser.add_argument('file', help='Source file to run')
    run_parser.add_argument('args', nargs='*', help='Arguments to pass')
    run_parser.add_argument('-v', '--verbose', action='store_true')

    # Build command
    build_parser = subparsers.add_parser('build', help='Compile without running')
    build_parser.add_argument('file', help='Source file to build')

    # Clean command
    clean_parser = subparsers.add_parser('clean', help='Remove compiled files')

    args = parser.parse_args()
    builder = Builder()

    if args.command == 'run':
        lang = detect_language(args.file)
        if args.verbose:
            print(f"[DEBUG] Detected language: {lang}")
        print(f"[{lang.title()}] Running {args.file}...")
        builder.run(args.file, lang, args.args)

    elif args.command == 'build':
        # ...

if __name__ == '__main__':
    main()
```

---

## Test Suite

Create test files to verify your build tool:

### test_hello.py
```python
print("Hello from Python!")
```

### test_hello.cpp
```cpp
#include <iostream>
int main() {
    std::cout << "Hello from C++!" << std::endl;
    return 0;
}
```

### test_hello.hs
```haskell
main = putStrLn "Hello from Haskell!"
```

### test_args.py
```python
import sys
print(f"Arguments: {sys.argv[1:]}")
```

### Test Commands
```bash
# Should all produce "Hello from X!"
polyglot run test_hello.py
polyglot run test_hello.cpp
polyglot run test_hello.hs

# Should pass arguments
polyglot run test_args.py -- one two three
# Output: Arguments: ['one', 'two', 'three']
```

---

## Submission Checklist

- [ ] Detects Python, C++, and Haskell files
- [ ] Compiles and runs C++ correctly
- [ ] Runs Python without compilation
- [ ] Passes arguments to programs
- [ ] Clear error messages on failure
- [ ] At least 2 standard features (clean, verbose, etc.)
- [ ] Code is modular and well-organized
- [ ] `REFLECTION.md` completed

---

## Reflection Questions

1. How did you handle different compilation requirements across languages?
2. What was challenging about process execution in your chosen language?
3. How would you extend this to support multi-file projects?
4. How does this project connect to real build tools (Make, CMake, Cargo)?
5. What would you need to add for production use?

---

## Grading Rubric

| Criteria | Points |
|----------|--------|
| Language detection | 15 |
| Compilation (C++, Haskell) | 25 |
| Running programs | 20 |
| Argument passing | 10 |
| Standard features | 15 |
| Error handling | 10 |
| Code quality | 5 |
| **Total** | **100** |

**Bonus:** +15 for dependency tracking, +10 for watch mode

---

## Common Mistakes to Avoid

1. **Platform differences** - Paths, executables (`.exe` on Windows)
2. **Forgetting to handle compilation errors** - Check return codes
3. **Not cleaning up** - Leave intermediate files (`.o`, `.hi`)
4. **Hardcoded paths** - Use PATH for compilers
5. **Race conditions** - If implementing watch mode

---

## Extension Ideas

- Support for Java (compile + run with classpath)
- Build caching (like ccache)
- Parallel builds
- Cross-compilation support
- Integration with language-specific tools (pip, cargo, cabal)
- LSP integration for editor support

---

This project gives you hands-on experience with the tools that professional developers use every day!
