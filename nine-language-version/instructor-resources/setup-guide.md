# Setup Guide for Students and Instructors

Complete installation guide for all 9 languages. Share this with students before the first class.

## Quick Start (TL;DR)

**Absolute minimum for Week 1:**
- ✅ Python 3
- ✅ Node.js (for JavaScript)
- ✅ One compiled language (Java recommended for ease)

**Can add later:**
- Week 2-3: C (GCC), Rust
- Week 3-4: Haskell, Racket
- Week 4-5: Prolog, Ruby

---

## Installation by Operating System

### macOS

#### Easiest: Homebrew (recommended)

1. **Install Homebrew** (if not already installed):
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. **Install all languages**:
   ```bash
   # Python (usually pre-installed)
   python3 --version

   # Node.js
   brew install node

   # Java
   brew install openjdk

   # GCC (C compiler)
   xcode-select --install

   # Ruby (usually pre-installed)
   ruby --version

   # Rust
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

   # Haskell
   brew install ghc cabal-install

   # Racket
   brew install --cask racket

   # Prolog
   brew install swi-prolog
   ```

3. **Verify installations**:
   ```bash
   python3 --version
   node --version
   java --version
   gcc --version
   ruby --version
   rustc --version
   ghc --version
   racket --version
   swipl --version
   ```

---

### Linux (Ubuntu/Debian)

```bash
# Update package list
sudo apt update

# Python (usually pre-installed)
python3 --version

# Node.js
curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
sudo apt install -y nodejs

# Java
sudo apt install -y default-jdk

# GCC (C compiler)
sudo apt install -y build-essential

# Ruby
sudo apt install -y ruby-full

# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Haskell
sudo apt install -y ghc cabal-install

# Racket
sudo apt install -y racket

# Prolog
sudo apt install -y swi-prolog
```

**Verify all installations** (same commands as macOS)

---

### Windows

#### Option 1: WSL (Windows Subsystem for Linux) - Recommended

1. **Enable WSL**:
   - Open PowerShell as Administrator
   - Run: `wsl --install`
   - Restart computer
   - Install Ubuntu from Microsoft Store

2. **Follow Linux instructions above** inside WSL

#### Option 2: Native Windows Installation

```powershell
# Install Chocolatey (package manager)
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install languages
choco install python
choco install nodejs
choco install openjdk
choco install mingw  # GCC for C
choco install ruby
choco install rust
choco install ghc  # Haskell
choco install racket
choco install swi-prolog
```

**Verify installations in PowerShell or Command Prompt**

---

## Cloud/Online Options (No Installation!)

### Replit (Highly Recommended for Beginners)

**Pros:**
- No installation needed
- Supports all 9 languages
- Share code easily
- Free tier available

**Cons:**
- Requires internet
- Limited resources on free tier

**How to use:**
1. Go to [replit.com](https://replit.com)
2. Create account
3. Click "Create Repl"
4. Choose language
5. Start coding!

**Recommended for:**
- Students with installation issues
- First 1-2 weeks while getting set up
- Quick demos in class

---

### Other Cloud Options

**GitHub Codespaces:**
- Full VS Code in browser
- Powerful but may need paid plan
- Great for collaborative work

**GitPod:**
- Similar to Codespaces
- Good free tier
- Easy GitHub integration

**CoCalc:**
- Academic-focused
- Great for Jupyter notebooks (Python)
- Some languages limited

---

## Recommended Text Editors / IDEs

### For Beginners

**VS Code (Highly Recommended)**
- Download: [code.visualstudio.com](https://code.visualstudio.com/)
- Free, powerful, works everywhere
- Extensions for all 9 languages

**Recommended VS Code Extensions:**
- Python: "Python" by Microsoft
- JavaScript: Built-in
- C/C++: "C/C++" by Microsoft
- Java: "Java Extension Pack" by Microsoft
- Rust: "rust-analyzer"
- Haskell: "Haskell"

**VS Code Setup:**
1. Install VS Code
2. Open Extensions (Ctrl+Shift+X / Cmd+Shift+X)
3. Search and install extensions above
4. Restart VS Code

---

### Language-Specific IDEs (Optional)

**Python:**
- PyCharm (powerful, free community edition)
- Jupyter Notebooks (for exploratory coding)

**Java:**
- IntelliJ IDEA (powerful, free community edition)
- Eclipse (free, widely used)

**Haskell:**
- VS Code with Haskell extension (best option)
- GHCi (interactive environment, comes with GHC)

**Racket:**
- DrRacket (comes with Racket installation, excellent for beginners)

---

## Testing Your Installation

### Quick Test Script

Create a file `test-setup.sh` (or `test-setup.bat` for Windows):

```bash
#!/bin/bash

echo "Testing installations..."
echo

echo "Python:"
python3 --version

echo "JavaScript (Node.js):"
node --version

echo "Java:"
java --version

echo "C (GCC):"
gcc --version

echo "Ruby:"
ruby --version

echo "Rust:"
rustc --version

echo "Haskell (GHC):"
ghc --version

echo "Racket:"
racket --version

echo "Prolog (SWI-Prolog):"
swipl --version

echo
echo "If all versions printed successfully, you're good to go!"
```

Run with: `bash test-setup.sh`

---

### Test Programs

Use the Hello World programs in `lessons/01-hello-world/` and run the test script:

```bash
cd lessons/01-hello-world
./test-all.sh
```

This will verify all languages work correctly!

---

## Common Installation Issues

### Python

**Issue**: `python: command not found`
**Fix**: Use `python3` instead, or create alias:
```bash
alias python=python3
```

**Issue**: Permission errors on Mac
**Fix**: Don't use system Python, install via Homebrew

---

### Node.js / JavaScript

**Issue**: `npm` commands fail
**Fix**: Ensure Node.js installation completed successfully
```bash
node --version
npm --version
```

**Issue**: Permission errors (Linux/Mac)
**Fix**: Don't use sudo for npm, use `nvm` instead:
```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
nvm install node
```

---

### Java

**Issue**: `JAVA_HOME not set`
**Fix** (macOS):
```bash
export JAVA_HOME=$(/usr/libexec/java_home)
```

**Fix** (Linux):
```bash
export JAVA_HOME=/usr/lib/jvm/default-java
```

Add to `.bashrc` or `.zshrc` to make permanent

---

### C (GCC)

**Issue**: `gcc: command not found` (macOS)
**Fix**: Install Xcode Command Line Tools:
```bash
xcode-select --install
```

**Issue**: `gcc: command not found` (Windows)
**Fix**: Install MinGW or use WSL

---

### Rust

**Issue**: `rustc: command not found` after installation
**Fix**: Restart terminal or run:
```bash
source $HOME/.cargo/env
```

**Issue**: Installation script fails
**Fix**: Check internet connection, try again

---

### Haskell

**Issue**: `ghc: command not found`
**Fix**: Ensure GHC is in PATH. On Windows, may need to restart terminal.

**Issue**: GHCi doesn't start
**Fix**:
```bash
ghci --version  # Check it's installed
ghci  # Should start interactive environment
```

**Issue**: Cabal errors
**Fix**:
```bash
cabal update
cabal install --lib base
```

---

### Racket

**Issue**: `racket: command not found`
**Fix**: Add Racket to PATH
- macOS: Usually in `/Applications/Racket/bin`
- Linux: Check `/usr/bin` or `/usr/local/bin`
- Windows: Add to PATH environment variable

**Issue**: DrRacket won't start
**Fix**: Reinstall Racket, check for conflicts

---

### Prolog

**Issue**: `swipl: command not found`
**Fix**: Ensure SWI-Prolog is installed and in PATH

**Issue**: Programs don't run
**Fix**: Check file has `.pl` extension
Check for syntax errors with:
```bash
swipl -c yourfile.pl
```

---

## Setup for Specific Lessons

### Lesson 1-2: Minimum Setup
- Python 3
- Node.js
- (Optional) Java or C

### Lesson 3: Add Compiled Languages
- C (GCC)
- Java (JDK)

### Lesson 4: Add Functional Languages
- Haskell (GHC)
- Racket

### Lesson 5: Complete Setup
- Ruby
- Rust
- Prolog

---

## Troubleshooting Steps

### General Debugging

1. **Check version**: Many issues are old versions
   ```bash
   <command> --version
   ```

2. **Check PATH**: Ensure language is in system PATH
   ```bash
   which python3  # macOS/Linux
   where python3  # Windows
   ```

3. **Restart terminal**: PATH changes need new shell

4. **Reinstall**: When all else fails, clean install
   - Uninstall completely
   - Delete configuration files
   - Fresh install

### Get Help

**When students are stuck:**
1. Check error message carefully
2. Google the exact error (often stack overflow has answer)
3. Check language-specific documentation
4. Ask classmates / instructor
5. Use cloud option (Replit) temporarily

**Resources:**
- [Stack Overflow](https://stackoverflow.com)
- Language-specific forums/discords
- Course discussion board/Slack

---

## Setup for Instructors

### Instructor Machine Setup

**Essential:**
- All 9 languages installed and tested
- VS Code with all extensions
- Test scripts run successfully
- Screen sharing setup for demos

**Highly Recommended:**
- Multiple terminal windows/tabs for switching languages
- Syntax highlighting configured for all languages
- Code snippets for common patterns
- Backup plan (Replit account ready)

### Lab Setup (if applicable)

**Computer Lab Requirements:**
- All languages pre-installed
- Student accounts with permissions
- Text editor installed (VS Code recommended)
- Internet access for documentation

**Testing Lab Setup:**
1. Run test scripts as student account
2. Verify file permissions
3. Check internet access
4. Test compilation for all languages

---

## Alternative: Docker Container (Advanced)

For instructors wanting reproducible environment:

**Dockerfile** (example):
```dockerfile
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    python3 \
    nodejs \
    default-jdk \
    build-essential \
    ruby \
    rustc \
    ghc \
    racket \
    swi-prolog

WORKDIR /workspace
```

Build and use:
```bash
docker build -t polyglot-course .
docker run -it -v $(pwd):/workspace polyglot-course
```

**Pros:** Consistent environment for all students
**Cons:** Requires Docker knowledge, extra overhead

---

## Pre-Course Checklist for Students

- [ ] Python 3 installed and tested
- [ ] Node.js installed and tested
- [ ] Java OR C installed and tested
- [ ] Text editor installed (VS Code recommended)
- [ ] Can run at least 3 Hello World programs
- [ ] Know how to navigate terminal/command line
- [ ] Have internet access for documentation

**If any checked boxes have issues:** Use Replit until resolved!

---

## Quick Reference Card

```
┌─────────────────────────────────────────────┐
│ Language Quick Reference                    │
├─────────────┬───────────────┬───────────────┤
│ Language    │ Run Command   │ File Extension│
├─────────────┼───────────────┼───────────────┤
│ Python      │ python3 file  │ .py           │
│ JavaScript  │ node file     │ .js           │
│ C           │ gcc f; ./a.out│ .c            │
│ Java        │ javac+java    │ .java         │
│ Ruby        │ ruby file     │ .rb           │
│ Rust        │ rustc f;./exe │ .rs           │
│ Haskell     │ ghc f; ./exe  │ .hs           │
│ Racket      │ racket file   │ .rkt          │
│ Prolog      │ swipl file    │ .pl           │
└─────────────┴───────────────┴───────────────┘
```

---

Remember: The goal is to write code, not fight with installations! When in doubt, use a cloud option and sort out local setup later.
