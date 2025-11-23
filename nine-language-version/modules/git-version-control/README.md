# Module: Git & Version Control

## Learning Objectives

By the end of this module, you will be able to:

1. Understand what version control is and why it matters
2. Initialize and configure Git repositories
3. Make commits with meaningful messages
4. Work with branches and merge changes
5. Collaborate using remote repositories (GitHub, GitLab)
6. Resolve merge conflicts
7. Use Git effectively in your development workflow

## Why Version Control?

**Problem:** How do you track changes to your code over time?

**Bad solutions:**
- `project_v1.py`, `project_v2.py`, `project_FINAL.py`, `project_FINAL_FINAL.py` ❌
- Commenting out old code instead of deleting it ❌
- No backup when things break ❌
- Can't collaborate without overwriting each other's work ❌

**Git solves this:**
- ✅ Complete history of every change
- ✅ Revert to any previous version
- ✅ Multiple people can work simultaneously
- ✅ Experiment safely with branches
- ✅ Backup your work remotely

---

## Part 1: Git Basics

### What is Git?

**Git** is a **distributed version control system**. It tracks changes to files over time, letting you:
- See what changed, when, and by whom
- Revert to previous versions
- Work on features independently (branches)
- Merge changes from multiple contributors

###

 Installation

Git is already installed in this environment. Verify with:

```bash
git --version
```

**If you need to install:**
- Linux: `sudo apt-get install git`
- Mac: `brew install git` or included with Xcode
- Windows: Download from git-scm.com

### Configuration

First time setup - tell Git who you are:

```bash
# Set your name and email
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"

# Check configuration
git config --list

# Set default branch name to 'main'
git config --global init.defaultBranch main
```

---

## Part 2: Basic Workflow

### Creating a Repository

```bash
# Create a new directory
mkdir my-project
cd my-project

# Initialize Git repository
git init

# What happened?
ls -la  # You'll see a .git directory (Git's database)
```

**Result:** You now have a Git repository! The `.git` folder tracks all changes.

### The Three Areas

Git has three important areas:

1. **Working Directory** - Your actual files
2. **Staging Area** (Index) - Changes you want to commit
3. **Repository** (.git directory) - Committed history

```
Working Directory  →  Staging Area  →  Repository
   (edit files)    git add    git commit
```

### Basic Commands

#### 1. Check Status

```bash
git status
```

Shows:
- Modified files
- Staged files
- Untracked files

#### 2. Stage Changes

```bash
# Stage specific file
git add myfile.py

# Stage all changes
git add .

# Stage specific files by pattern
git add *.py
```

#### 3. Commit Changes

```bash
# Commit with message
git commit -m "Add user authentication feature"

# Opens editor for longer message
git commit
```

**Commit messages should:**
- Be clear and descriptive
- Use present tense ("Add feature" not "Added feature")
- Explain **why**, not just what

Good:
```
Add user login validation

Validates email format and password strength before
allowing account creation. Prevents invalid accounts.
```

Bad:
```
stuff
fixed bug
changes
```

### Complete Workflow Example

```bash
# 1. Create some files
echo "print('Hello')" > hello.py

# 2. Check status
git status
# Output: hello.py is untracked

# 3. Stage the file
git add hello.py

# 4. Check status again
git status
# Output: Changes to be committed: new file: hello.py

# 5. Commit
git commit -m "Add hello world script"

# 6. View history
git log
```

---

## Part 3: Viewing History

### Git Log

```bash
# View commit history
git log

# Condensed one-line view
git log --oneline

# Show last 5 commits
git log -5

# Visual branch graph
git log --oneline --graph --all
```

### Git Diff

```bash
# See unstaged changes
git diff

# See staged changes
git diff --staged

# Compare to specific commit
git diff <commit-hash>
```

### Git Show

```bash
# Show details of last commit
git show

# Show specific commit
git show <commit-hash>
```

---

## Part 4: Branching

**Branches** let you work on features independently without affecting the main code.

### Why Branches?

```
main:     A---B---C---D---E
                   \
feature:            F---G
```

- **main** branch: stable, working code
- **feature** branch: experimental work

### Branch Commands

```bash
# List branches
git branch

# Create new branch
git branch feature-login

# Switch to branch
git checkout feature-login

# Create and switch in one command
git checkout -b feature-login

# Modern alternative (Git 2.23+)
git switch feature-login
git switch -c feature-login  # create and switch
```

### Branch Workflow Example

```bash
# 1. Start on main
git checkout main

# 2. Create feature branch
git checkout -b add-validation

# 3. Make changes
echo "def validate(x): return x > 0" > validation.py
git add validation.py
git commit -m "Add validation function"

# 4. Switch back to main
git checkout main

# 5. Merge feature into main
git merge add-validation

# 6. Delete feature branch (optional)
git branch -d add-validation
```

---

## Part 5: Merging and Conflicts

### Simple Merge

If branches haven't diverged, Git can **fast-forward**:

```bash
git checkout main
git merge feature-branch
```

### Merge Conflicts

**Conflict** occurs when same lines are changed in both branches.

Example conflict:

```python
<<<<<<< HEAD
print("Hello from main")
=======
print("Hello from feature")
>>>>>>> feature-branch
```

**Resolution:**

1. Open the file
2. Choose which version to keep (or combine them)
3. Remove conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
4. Stage and commit

```bash
# After resolving conflicts
git add conflicted_file.py
git commit -m "Merge feature-branch, resolve conflicts"
```

---

## Part 6: Remote Repositories

### What is a Remote?

A **remote** is a version of your repository hosted elsewhere (GitHub, GitLab, Bitbucket).

### GitHub Workflow

#### 1. Create Repository on GitHub

1. Go to github.com
2. Click "New repository"
3. Name it, add description
4. Don't initialize with README (we already have local repo)

#### 2. Connect Local to Remote

```bash
# Add remote
git remote add origin https://github.com/username/repo-name.git

# Verify
git remote -v
```

#### 3. Push to Remote

```bash
# Push main branch
git push -u origin main

# Subsequent pushes
git push
```

### Cloning

**Clone** = download a repository from remote:

```bash
git clone https://github.com/username/repo-name.git
cd repo-name
```

### Pull

**Pull** = download changes from remote:

```bash
# Pull latest changes
git pull origin main

# Or just
git pull
```

### Complete Collaboration Workflow

```bash
# 1. Clone repository
git clone https://github.com/team/project.git
cd project

# 2. Create feature branch
git checkout -b my-feature

# 3. Make changes
# ... edit files ...
git add .
git commit -m "Implement my feature"

# 4. Push your branch
git push -u origin my-feature

# 5. Create Pull Request on GitHub

# 6. After merge, update local main
git checkout main
git pull
```

---

## Part 7: Essential Commands Reference

### Status & History

```bash
git status              # Check current state
git log                 # View commit history
git log --oneline       # Condensed history
git diff                # See unstaged changes
git diff --staged       # See staged changes
```

### Making Changes

```bash
git add <file>          # Stage specific file
git add .               # Stage all changes
git commit -m "msg"     # Commit with message
git commit              # Commit with editor
```

### Branches

```bash
git branch              # List branches
git branch <name>       # Create branch
git checkout <branch>   # Switch branch
git checkout -b <name>  # Create and switch
git merge <branch>      # Merge branch
git branch -d <branch>  # Delete branch
```

### Remote

```bash
git clone <url>         # Clone repository
git remote -v           # List remotes
git push                # Push to remote
git pull                # Pull from remote
git fetch               # Download without merging
```

### Undoing

```bash
git checkout -- <file>  # Discard unstaged changes
git reset HEAD <file>   # Unstage file
git reset --hard HEAD   # Discard all changes (DANGER!)
git revert <commit>     # Create new commit undoing changes
```

---

## Part 8: Best Practices

### 1. Commit Often

- Small, focused commits
- Each commit should be a logical unit
- Easier to understand history
- Easier to revert if needed

**Bad:** One commit after 3 days of work
**Good:** 10 commits, each for a specific change

### 2. Write Good Commit Messages

```bash
# Good
git commit -m "Add email validation to signup form"

# Bad
git commit -m "changes"
git commit -m "fix"
git commit -m "asdf"
```

**Format:**
```
Short summary (50 chars or less)

More detailed explanation if needed. Explain WHY
the change was made, not just WHAT changed.

- Bullet points are OK
- Reference issue numbers: Fixes #123
```

### 3. Don't Commit Sensitive Data

**Never commit:**
- Passwords
- API keys
- Private keys
- `.env` files with secrets

**Use `.gitignore`:**

```bash
# Create .gitignore file
cat > .gitignore << EOF
# Secrets
.env
secrets.txt
*.key

# Dependencies
node_modules/
venv/
__pycache__/

# OS files
.DS_Store
Thumbs.db
EOF

git add .gitignore
git commit -m "Add .gitignore"
```

### 4. Branch Naming Conventions

```bash
# Good branch names
git checkout -b feature/user-authentication
git checkout -b bugfix/login-error
git checkout -b hotfix/security-patch

# Bad
git checkout -b stuff
git checkout -b branch1
```

### 5. Pull Before Push

```bash
# Before starting work
git pull

# Before pushing
git pull  # Make sure you have latest changes
git push
```

---

## Part 9: Common Workflows

### Solo Developer

```bash
# Daily workflow
git status
git add .
git commit -m "Descriptive message"
git push
```

### Team Collaboration (Feature Branch)

```bash
# 1. Start feature
git checkout main
git pull
git checkout -b feature/new-api

# 2. Work and commit
# ... make changes ...
git add .
git commit -m "Add API endpoint for users"

# 3. Push feature branch
git push -u origin feature/new-api

# 4. Create Pull Request on GitHub
# Team reviews your code

# 5. After merge, clean up
git checkout main
git pull
git branch -d feature/new-api
```

### Fixing Mistakes

```bash
# Oops, forgot to add a file
git add forgotten_file.py
git commit --amend --no-edit

# Oops, bad commit message
git commit --amend -m "Better message"

# Oops, committed to wrong branch
git reset --soft HEAD~1  # Undo commit, keep changes
git stash                # Save changes temporarily
git checkout correct-branch
git stash pop            # Apply changes
git add .
git commit -m "Now on correct branch"
```

---

## Part 10: Exercises

### Exercise 1: First Repository

Create your first Git repository:

```bash
mkdir git-practice
cd git-practice
git init
echo "# My First Repo" > README.md
git add README.md
git commit -m "Initial commit"
```

### Exercise 2: Make Changes

1. Create a Python file: `hello.py`
2. Add some code
3. Stage and commit
4. Modify the file
5. View the diff
6. Commit the change
7. View the log

### Exercise 3: Branching

1. Create a branch called `feature-greeting`
2. Add a new function
3. Commit on the branch
4. Switch back to `main`
5. Merge the feature branch

### Exercise 4: Simulate Conflict

1. Create branch `branch-a` and `branch-b`
2. In each branch, modify the same line differently
3. Merge one branch into `main`
4. Try to merge the other - conflict!
5. Resolve the conflict
6. Complete the merge

---

## Summary

### Git in 5 Commands

```bash
git clone <url>      # Get a repository
git add .            # Stage changes
git commit -m "msg"  # Save changes
git pull             # Get updates
git push             # Share changes
```

### Key Concepts

1. **Repository**: Project folder tracked by Git
2. **Commit**: Snapshot of your code at a point in time
3. **Branch**: Independent line of development
4. **Merge**: Combine branches
5. **Remote**: Online copy of your repository
6. **Pull**: Download changes from remote
7. **Push**: Upload changes to remote

---

## Next Steps

1. **Practice daily:** Use Git for all your projects
2. **Explore GitHub:** Create account, browse projects
3. **Learn advanced features:** Rebase, cherry-pick, bisect
4. **Contribute to open source:** Great way to learn!

## Resources

- **Official Git Book:** https://git-scm.com/book/en/v2
- **GitHub Guides:** https://guides.github.com/
- **Interactive Tutorial:** https://learngitbranching.js.org/
- **Git Cheat Sheet:** https://education.github.com/git-cheat-sheet-education.pdf

---

**Remember:** Git is a skill that improves with practice. Don't worry about memorizing commands - you'll learn them through use!
