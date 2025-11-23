# Project 5: Polyglot Build Tool

**Difficulty**: ⭐⭐⭐⭐⭐ Hard
**Time Estimate**: 8-12 hours
**Prerequisites**: All Lessons 1-5, experience with all previous projects

## Project Description

Build a command-line build tool that can compile, test, and run programs across multiple languages. Think of it as a simplified, polyglot version of `make`, `gradle`, or `cargo`.

This project brings together everything from the course: file I/O, data structures, command-line parsing, and working with multiple programming languages.

### Example Usage

```bash
$ polybuild init
Created polybuild.json configuration file

$ polybuild add python greet.py
Added python file: greet.py

$ polybuild add haskell factorial.hs
Added haskell file: factorial.hs

$ polybuild build
[Python] greet.py - No compilation needed ✓
[Haskell] factorial.hs - Compiling...
[Haskell] factorial.hs - Compiled successfully ✓

$ polybuild test
[Python] Running tests for greet.py...
  test_hello: PASS ✓
  test_goodbye: PASS ✓
[Haskell] Running tests for factorial.hs...
  test_factorial_0: PASS ✓
  test_factorial_5: PASS ✓

All tests passed! (4/4)

$ polybuild run python greet.py
Hello from Python!

$ polybuild clean
Cleaned build artifacts
```

## Requirements

### Core Features (Required)

1. **Configuration file**: Store project settings in JSON
2. **Add files**: Register source files by language
3. **Build**: Compile files that need compilation
4. **Run**: Execute specific programs
5. **Clean**: Remove build artifacts

### Intermediate Features (Recommended)

6. **Test**: Run tests for each language
7. **Watch mode**: Auto-rebuild on file changes
8. **Dependencies**: Track which files depend on others
9. **Parallel builds**: Build independent files concurrently
10. **Status**: Show which files are up-to-date

### Advanced Features (Optional)

11. **Custom commands**: User-defined build steps
12. **Plugins**: Extensible language support
13. **Build caching**: Only rebuild what changed
14. **Multiple targets**: Debug vs release builds
15. **Cross-language dependencies**: Python imports compiled C module

## Implementation Requirements

### Implement in 1-2 languages:

**Recommended:**
- **Python**: Excellent for system scripting, subprocess management
- **Rust**: Type safety, excellent for CLI tools, concurrent builds
- **Haskell**: Pure functions for build logic, IO monad for effects

**Key challenges:**
- How to manage subprocess execution?
- How to track file modification times?
- How to structure language-specific logic?
- How to handle errors gracefully?

## Technical Specifications

### Configuration File Format

**polybuild.json:**
```json
{
  "name": "my-polyglot-project",
  "version": "1.0.0",
  "files": [
    {
      "language": "python",
      "source": "greet.py",
      "tests": "test_greet.py"
    },
    {
      "language": "haskell",
      "source": "factorial.hs",
      "output": "factorial",
      "tests": "FactorialTest.hs"
    },
    {
      "language": "c",
      "source": "hello.c",
      "output": "hello",
      "compile_flags": ["-Wall", "-O2"]
    }
  ],
  "build_dir": "build",
  "test_command": {
    "python": "pytest",
    "haskell": "runhaskell"
  }
}
```

### Language Support Table

| Language | Compilation | Run Command | Test Framework |
|----------|-------------|-------------|----------------|
| Python | Not needed | `python {file}` | pytest, unittest |
| JavaScript | Not needed | `node {file}` | jest, mocha |
| C | `gcc {file} -o {output}` | `./{output}` | Custom, check |
| Java | `javac {file}` | `java {class}` | JUnit |
| Ruby | Not needed | `ruby {file}` | rspec, minitest |
| Haskell | `ghc {file} -o {output}` | `./{output}` | HUnit, QuickCheck |
| Racket | Not needed | `racket {file}` | rackunit |
| Prolog | Not needed | `swipl -s {file}` | plunit |
| Rust | `rustc {file} -o {output}` | `./{output}` | cargo test |

### Build Process Flow

```
1. Read polybuild.json
2. For each file:
   a. Check if compilation needed
   b. Check if file modified since last build
   c. If needed, compile
   d. Track build status
3. Report results
```

## Step-by-Step Guide

### Part 1: Configuration Management

**Step 1**: Read and write configuration

```python
# Python
import json
from pathlib import Path

class PolyBuildConfig:
    def __init__(self, config_file='polybuild.json'):
        self.config_file = config_file
        self.config = self.load()

    def load(self):
        if Path(self.config_file).exists():
            with open(self.config_file, 'r') as f:
                return json.load(f)
        else:
            # Default config
            return {
                'name': 'polyglot-project',
                'version': '1.0.0',
                'files': [],
                'build_dir': 'build'
            }

    def save(self):
        with open(self.config_file, 'w') as f:
            json.dump(self.config, f, indent=2)

    def add_file(self, language, source, **kwargs):
        file_entry = {
            'language': language,
            'source': source,
            **kwargs
        }
        self.config['files'].append(file_entry)
        self.save()
```

### Part 2: Language Handlers

**Step 2**: Define compilation and execution logic per language

```python
import subprocess
from pathlib import Path

class LanguageHandler:
    def __init__(self, language):
        self.language = language

    def needs_compilation(self):
        """Does this language require compilation?"""
        return self.language in ['c', 'java', 'haskell', 'rust']

    def compile(self, source, output=None):
        """Compile source file"""
        if not self.needs_compilation():
            return True

        if self.language == 'c':
            output = output or Path(source).stem
            cmd = ['gcc', source, '-o', output]
        elif self.language == 'haskell':
            output = output or Path(source).stem
            cmd = ['ghc', source, '-o', output]
        elif self.language == 'rust':
            output = output or Path(source).stem
            cmd = ['rustc', source, '-o', output]
        elif self.language == 'java':
            cmd = ['javac', source]
        else:
            raise ValueError(f"Unknown compiled language: {self.language}")

        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=True
            )
            return True
        except subprocess.CalledProcessError as e:
            print(f"Compilation failed: {e.stderr}")
            return False

    def run(self, source, output=None):
        """Execute source file or compiled binary"""
        if self.needs_compilation():
            # Run compiled binary
            cmd = [f'./{output}']
        else:
            # Run interpreted
            if self.language == 'python':
                cmd = ['python', source]
            elif self.language == 'javascript':
                cmd = ['node', source]
            elif self.language == 'ruby':
                cmd = ['ruby', source]
            elif self.language == 'racket':
                cmd = ['racket', source]
            elif self.language == 'prolog':
                cmd = ['swipl', '-s', source]
            else:
                raise ValueError(f"Unknown language: {self.language}")

        result = subprocess.run(cmd, capture_output=True, text=True)
        return result.stdout
```

### Part 3: Build System

**Step 3**: Implement build logic

```python
import os
from pathlib import Path

class BuildSystem:
    def __init__(self, config):
        self.config = config
        self.build_dir = Path(config.config['build_dir'])
        self.build_dir.mkdir(exist_ok=True)

    def is_up_to_date(self, source, output):
        """Check if output is newer than source"""
        if not Path(output).exists():
            return False

        source_time = os.path.getmtime(source)
        output_time = os.path.getmtime(output)
        return output_time > source_time

    def build_all(self):
        """Build all files in configuration"""
        results = []

        for file_entry in self.config.config['files']:
            language = file_entry['language']
            source = file_entry['source']
            output = file_entry.get('output')

            print(f"[{language.capitalize()}] Building {source}...")

            handler = LanguageHandler(language)

            if handler.needs_compilation():
                # Check if rebuild needed
                if output and self.is_up_to_date(source, output):
                    print(f"  {source} is up-to-date ✓")
                    results.append(True)
                    continue

                # Compile
                success = handler.compile(source, output)
                if success:
                    print(f"  Compiled successfully ✓")
                else:
                    print(f"  Compilation failed ✗")
                results.append(success)
            else:
                print(f"  No compilation needed ✓")
                results.append(True)

        # Summary
        successful = sum(results)
        total = len(results)
        print(f"\nBuild complete: {successful}/{total} succeeded")

        return all(results)
```

### Part 4: Testing Integration

**Step 4**: Run tests for each language

```python
class TestRunner:
    def __init__(self, config):
        self.config = config

    def run_tests(self):
        """Run all tests"""
        all_passed = True
        total_tests = 0
        passed_tests = 0

        for file_entry in self.config.config['files']:
            if 'tests' not in file_entry:
                continue

            language = file_entry['language']
            test_file = file_entry['tests']

            print(f"[{language.capitalize()}] Running tests...")

            handler = LanguageHandler(language)

            # Run test command
            if language == 'python':
                cmd = ['pytest', test_file, '-v']
            elif language == 'haskell':
                cmd = ['runhaskell', test_file]
            elif language == 'ruby':
                cmd = ['rspec', test_file]
            elif language == 'racket':
                cmd = ['raco', 'test', test_file]
            else:
                print(f"  No test runner configured for {language}")
                continue

            try:
                result = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    check=True
                )
                print(f"  Tests passed ✓")
                print(result.stdout)
            except subprocess.CalledProcessError as e:
                print(f"  Tests failed ✗")
                print(e.stdout)
                all_passed = False

        return all_passed
```

### Part 5: Command-Line Interface

**Step 5**: Main CLI

```python
import sys

def main():
    if len(sys.argv) < 2:
        print("Usage: polybuild <command> [args]")
        print("Commands: init, add, build, test, run, clean, status")
        return

    command = sys.argv[1]

    config = PolyBuildConfig()

    if command == 'init':
        # Create new config
        config.save()
        print("Initialized polybuild.json")

    elif command == 'add':
        if len(sys.argv) < 4:
            print("Usage: polybuild add <language> <file>")
            return
        language = sys.argv[2]
        source = sys.argv[3]
        config.add_file(language, source)
        print(f"Added {language} file: {source}")

    elif command == 'build':
        builder = BuildSystem(config)
        builder.build_all()

    elif command == 'test':
        tester = TestRunner(config)
        tester.run_tests()

    elif command == 'run':
        if len(sys.argv) < 4:
            print("Usage: polybuild run <language> <file>")
            return
        language = sys.argv[2]
        source = sys.argv[3]
        handler = LanguageHandler(language)
        output = handler.run(source)
        print(output)

    elif command == 'clean':
        # Remove build artifacts
        import shutil
        build_dir = Path(config.config['build_dir'])
        if build_dir.exists():
            shutil.rmtree(build_dir)
            build_dir.mkdir()
        print("Cleaned build artifacts")

    elif command == 'status':
        # Show status of all files
        for file_entry in config.config['files']:
            lang = file_entry['language']
            src = file_entry['source']
            exists = Path(src).exists()
            status = "✓" if exists else "✗"
            print(f"[{lang}] {src} {status}")

    else:
        print(f"Unknown command: {command}")

if __name__ == '__main__':
    main()
```

## Language-Specific Hints

### Python
- Use `subprocess` module for running commands
- `pathlib.Path` for file operations
- `os.path.getmtime()` for modification times
- JSON for configuration
- Consider `click` or `argparse` for CLI

### Haskell
- `System.Process` for subprocess execution
- `Data.Aeson` for JSON parsing
- `System.Directory` for file operations
- IO monad for side effects
- Type-safe configuration structures

### Rust
- `std::process::Command` for subprocesses
- `serde_json` for JSON
- `std::fs` for file operations
- `clap` crate for CLI parsing
- Strong typing prevents many errors
- Can build concurrently with threads

### JavaScript (Node.js)
- `child_process` module for subprocess
- `fs` module for files
- JSON parsing built-in
- `commander` for CLI
- Async/await for concurrent builds

### Others
Less recommended for this project (system tasks harder):
- **C**: Harder, more manual
- **Java**: Verbose for scripting tasks
- **Ruby**: Good option, similar to Python
- **Racket**: Possible, less common for build tools
- **Prolog**: Not suited for this task

## Extensions

### Extension 1: Watch Mode

Auto-rebuild when files change:

```python
import time
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

class BuildHandler(FileSystemEventHandler):
    def __init__(self, build_system):
        self.build_system = build_system

    def on_modified(self, event):
        if event.src_path.endswith(('.py', '.hs', '.c', '.rs')):
            print(f"\nFile changed: {event.src_path}")
            print("Rebuilding...")
            self.build_system.build_all()

def watch():
    config = PolyBuildConfig()
    builder = BuildSystem(config)

    event_handler = BuildHandler(builder)
    observer = Observer()
    observer.schedule(event_handler, path='.', recursive=True)
    observer.start()

    print("Watching for changes... (Ctrl+C to stop)")
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
```

### Extension 2: Parallel Builds

Build independent files concurrently:

```python
from concurrent.futures import ThreadPoolExecutor, as_completed

def build_file(file_entry):
    """Build a single file"""
    language = file_entry['language']
    source = file_entry['source']
    handler = LanguageHandler(language)

    if handler.needs_compilation():
        return handler.compile(source, file_entry.get('output'))
    return True

def build_parallel(self):
    """Build all files in parallel"""
    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = {
            executor.submit(build_file, entry): entry
            for entry in self.config.config['files']
        }

        for future in as_completed(futures):
            entry = futures[future]
            try:
                success = future.result()
                status = "✓" if success else "✗"
                print(f"[{entry['language']}] {entry['source']} {status}")
            except Exception as e:
                print(f"Error building {entry['source']}: {e}")
```

### Extension 3: Dependency Tracking

Track file dependencies:

```json
{
  "language": "c",
  "source": "main.c",
  "output": "main",
  "dependencies": ["utils.c", "utils.h"]
}
```

Rebuild if dependencies change:

```python
def needs_rebuild(self, file_entry):
    """Check if file needs rebuilding based on dependencies"""
    source = file_entry['source']
    output = file_entry.get('output')

    if not output or not Path(output).exists():
        return True

    # Check source file
    if not self.is_up_to_date(source, output):
        return True

    # Check dependencies
    for dep in file_entry.get('dependencies', []):
        if not self.is_up_to_date(dep, output):
            return True

    return False
```

### Extension 4: Build Caching

Cache build results to speed up rebuilds:

```python
import hashlib
import json

class BuildCache:
    def __init__(self, cache_file='.polybuild_cache'):
        self.cache_file = cache_file
        self.cache = self.load()

    def load(self):
        if Path(self.cache_file).exists():
            with open(self.cache_file) as f:
                return json.load(f)
        return {}

    def save(self):
        with open(self.cache_file, 'w') as f:
            json.dump(self.cache, f)

    def get_file_hash(self, filepath):
        """Get SHA256 hash of file contents"""
        with open(filepath, 'rb') as f:
            return hashlib.sha256(f.read()).hexdigest()

    def is_cached(self, source):
        """Check if file unchanged since last build"""
        current_hash = self.get_file_hash(source)
        return self.cache.get(source) == current_hash

    def update(self, source):
        """Update cache with current file hash"""
        self.cache[source] = self.get_file_hash(source)
        self.save()
```

## Testing Checklist

- [ ] Can initialize new project
- [ ] Can add files for multiple languages
- [ ] Builds compiled languages (C, Haskell, Rust)
- [ ] Skips compilation for interpreted languages
- [ ] Detects when rebuild not needed
- [ ] Can run individual programs
- [ ] Can clean build artifacts
- [ ] Handles compilation errors gracefully
- [ ] Works with multiple files
- [ ] Status command shows accurate info

## Grading Rubric (75 points)

| Criteria | Points | Description |
|----------|--------|-------------|
| Configuration | 10 | Read/write JSON config correctly |
| Language Detection | 10 | Correctly identifies language requirements |
| Compilation | 15 | Compiles C, Haskell, Rust successfully |
| Execution | 10 | Runs programs correctly |
| File Tracking | 10 | Detects when rebuild needed |
| Error Handling | 10 | Graceful failures, clear messages |
| CLI | 5 | Command-line interface works well |
| Code Quality | 10 | Well-organized, clean code |
| Documentation | 3 | Clear README and usage |
| Extensions | 2 | Additional features |

## Real-World Applications

This project teaches:
- **Build systems**: How tools like make, cargo, gradle work
- **Subprocess management**: Running external programs
- **File system operations**: Tracking modifications
- **Configuration management**: JSON, YAML, TOML
- **CLI tool design**: User-friendly command-line tools
- **Concurrent programming**: Parallel builds
- **Incremental compilation**: Only rebuild what changed

These skills are used in:
- Build tools (make, cargo, gradle, webpack)
- CI/CD pipelines (Jenkins, GitHub Actions)
- Package managers (npm, pip, cargo)
- Development environments
- Automation scripts

## Architecture Patterns

### Plugin Architecture

Support extensible language handlers:

```python
class LanguagePlugin:
    """Base class for language plugins"""
    def name(self):
        raise NotImplementedError

    def compile(self, source, output):
        raise NotImplementedError

    def run(self, source):
        raise NotImplementedError

class PythonPlugin(LanguagePlugin):
    def name(self):
        return 'python'

    def compile(self, source, output):
        return True  # No compilation

    def run(self, source):
        return subprocess.run(['python', source], capture_output=True)

# Plugin registry
PLUGINS = {
    'python': PythonPlugin(),
    'haskell': HaskellPlugin(),
    # ...
}
```

### Builder Pattern

Construct build process step by step:

```python
class BuildPlan:
    def __init__(self):
        self.steps = []

    def add_compile_step(self, file):
        self.steps.append(('compile', file))
        return self

    def add_test_step(self, file):
        self.steps.append(('test', file))
        return self

    def execute(self):
        for action, file in self.steps:
            # Execute each step
            pass

# Usage
plan = BuildPlan()
    .add_compile_step('main.c')
    .add_compile_step('utils.c')
    .add_test_step('test_main.c')
    .execute()
```

## Reflection Questions

1. **How did you structure language-specific logic?**
   - Separate classes? Dictionary of functions? Plugins?
   - What were tradeoffs?

2. **How did you handle subprocess execution?**
   - Synchronous? Asynchronous? Parallel?
   - Error handling strategy?

3. **What was the hardest part?**
   - File tracking? Dependency management? Error handling?
   - Why?

4. **How would you extend this to support new languages?**
   - What would need to change?
   - How extensible is your design?

5. **What real-world build tools did you learn from?**
   - Make? Cargo? Gradle? NPM?
   - What features would you add?

## Next Steps

- Add more language support
- Implement watch mode
- Add dependency resolution
- Build Docker containers
- Create web dashboard for build status
- Move on to Project 6 (Mini Programming Language)

---

**This project shows how build tools really work!** You're implementing real systems programming concepts.
