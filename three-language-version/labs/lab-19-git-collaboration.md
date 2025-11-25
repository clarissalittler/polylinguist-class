# Lab 19: Git Collaboration

**Quarter 2, Week 9**
**Duration:** 90 minutes
**Format:** Team exercise (groups of 3-4)

## Overview

Git is essential for collaboration. This lab practices real-world team workflows including branching, merging, and resolving conflicts.

## Objectives

By the end of this lab, you will:
- [ ] Create and manage branches
- [ ] Merge branches and resolve conflicts
- [ ] Use pull requests for code review
- [ ] Follow a team git workflow

## Setup

- Form teams of 3-4
- One person creates a shared repository
- All team members clone it
- Create folder for practice: `lab19-git/`

---

## Part 1: Branch Basics (20 minutes)

### Activity 1.1: Creating Branches

```bash
# View current branch
git branch

# Create a new branch
git branch feature-login

# Switch to the branch
git checkout feature-login
# Or in one command:
git checkout -b feature-login

# Modern alternative:
git switch -c feature-login
```

### Activity 1.2: Team Setup

**Person A (Repository Owner):**
```bash
# Create new repository
mkdir team-project
cd team-project
git init

# Create initial file
echo "# Team Project" > README.md
git add README.md
git commit -m "Initial commit"

# Create remote (on GitHub/GitLab) and push
git remote add origin <URL>
git push -u origin main
```

**Everyone else:**
```bash
# Clone the repository
git clone <URL>
cd team-project
```

### Activity 1.3: Branch Naming Conventions

Good branch names:
```
feature/user-authentication
feature/add-shopping-cart
bugfix/login-error
bugfix/fix-calculation
hotfix/security-patch
docs/update-readme
refactor/cleanup-utils
```

### ✅ Checkpoint 1

Verify:
- [ ] Everyone has cloned the repo
- [ ] Everyone can create and switch branches
- [ ] Team agrees on branch naming

---

## Part 2: Feature Branch Workflow (25 minutes)

### Activity 2.1: The Workflow

```
main (stable)
  │
  ├── feature/person-a  (Person A works here)
  │
  ├── feature/person-b  (Person B works here)
  │
  └── feature/person-c  (Person C works here)
```

### Activity 2.2: Parallel Development

**Each person creates their own feature branch:**

**Person A:**
```bash
git checkout -b feature/add-header
# Create header.py
echo 'def get_header(): return "Welcome!"' > header.py
git add header.py
git commit -m "Add header function"
git push -u origin feature/add-header
```

**Person B:**
```bash
git checkout -b feature/add-footer
# Create footer.py
echo 'def get_footer(): return "Copyright 2024"' > footer.py
git add footer.py
git commit -m "Add footer function"
git push -u origin feature/add-footer
```

**Person C:**
```bash
git checkout -b feature/add-main
# Create main.py
echo 'def main(): print("Hello from main")' > main.py
git add main.py
git commit -m "Add main function"
git push -u origin feature/add-main
```

### Activity 2.3: Merging Features

**Person A merges first:**
```bash
git checkout main
git pull origin main  # Get latest
git merge feature/add-header
git push origin main
```

**Person B merges second:**
```bash
git checkout main
git pull origin main  # Get A's changes!
git merge feature/add-footer
git push origin main
```

**Person C merges third:**
```bash
git checkout main
git pull origin main  # Get A and B's changes!
git merge feature/add-main
git push origin main
```

### ✅ Checkpoint 2

Verify:
- [ ] All features are on main
- [ ] Everyone's code is in the repository
- [ ] No merge errors

---

## Part 3: Handling Conflicts (25 minutes)

### Activity 3.1: Create a Conflict

**Setup - Person A:**
```bash
git checkout main
git pull
# Edit README.md
echo "# Team Project - Version 1.0" > README.md
git add README.md
git commit -m "Update README title"
git push
```

**Meanwhile - Person B (before pulling):**
```bash
git checkout main
# Edit same file differently
echo "# Awesome Team Project" > README.md
git add README.md
git commit -m "Improve README title"
git push  # This will fail!
```

### Activity 3.2: Resolve the Conflict

```bash
# Person B sees:
# error: failed to push some refs
# hint: Updates were rejected because the remote contains work

# Pull and see conflict
git pull
# Auto-merging README.md
# CONFLICT (content): Merge conflict in README.md

# Open README.md - you'll see:
# <<<<<<< HEAD
# # Awesome Team Project
# =======
# # Team Project - Version 1.0
# >>>>>>> abc123
```

**Manual resolution:**
```bash
# Edit the file to resolve
# Remove conflict markers and choose/combine content
echo "# Awesome Team Project - Version 1.0" > README.md

# Mark as resolved
git add README.md
git commit -m "Merge remote changes, combine titles"
git push
```

### Activity 3.3: Practice Conflicts

**Exercise:** Each person modifies the same line of a shared file, then practice resolving.

Create `team_info.txt`:
```
Team Name: [Your name here]
Members: [List here]
Project: [Description here]
```

Each person changes the team name, creates conflict, resolves.

### ✅ Checkpoint 3

Verify:
- [ ] Successfully created a conflict
- [ ] Resolved conflict correctly
- [ ] Can explain conflict markers

---

## Part 4: Pull Request Workflow (15 minutes)

### Activity 4.1: Branch Protection

If using GitHub/GitLab, protect main branch:
- Require pull request reviews
- Require status checks
- No direct pushes to main

### Activity 4.2: Pull Request Process

**Create PR:**
```bash
# Create feature branch
git checkout -b feature/new-feature
# Make changes
git add .
git commit -m "Add new feature"
git push -u origin feature/new-feature

# On GitHub: Create Pull Request
# - Base: main
# - Compare: feature/new-feature
# - Add description
# - Request reviewers
```

**Review PR:**
- Look at changed files
- Leave comments on specific lines
- Approve or request changes

**Merge PR:**
- Squash and merge (clean history)
- Or merge commit (preserve all commits)
- Delete branch after merge

### Activity 4.3: Code Review Practice

**Each person:**
1. Create a small feature branch
2. Open a PR
3. Review another person's PR
4. Leave constructive comments
5. Approve and merge

**Review checklist:**
- [ ] Code works as intended
- [ ] Code is readable
- [ ] Tests included (if applicable)
- [ ] No obvious bugs
- [ ] Follows team conventions

---

## Part 5: Git Best Practices (5 minutes)

### Commit Messages

**Good:**
```
Add user authentication feature

- Implement login/logout endpoints
- Add password hashing
- Create user session management
```

**Bad:**
```
fixed stuff
update
WIP
asdfasdf
```

### Branch Hygiene

```bash
# Delete merged branches
git branch -d feature/old-feature

# Prune remote tracking branches
git fetch --prune

# See all branches
git branch -a
```

### Useful Commands

```bash
# See commit history
git log --oneline --graph

# See what changed
git diff
git diff --staged

# Undo last commit (keep changes)
git reset --soft HEAD~1

# Stash changes temporarily
git stash
git stash pop

# See who changed what
git blame filename.py
```

---

## Challenge: Simulate a Sprint

As a team, simulate a mini sprint:

1. **Planning (5 min)**
   - Create 3-4 issues/tasks
   - Assign to team members

2. **Development (15 min)**
   - Each person works on their task
   - Create branches, make commits

3. **Review (10 min)**
   - Open PRs
   - Review each other's code
   - Suggest improvements

4. **Integration (5 min)**
   - Merge PRs
   - Resolve any conflicts
   - Verify main works

---

## Reference: Common Scenarios

### Oops, committed to wrong branch
```bash
# Move commit to correct branch
git branch correct-branch  # Create branch at current commit
git reset --hard HEAD~1    # Remove from current branch
git checkout correct-branch
```

### Need to update feature branch with main
```bash
git checkout feature-branch
git merge main
# Or rebase (cleaner history):
git rebase main
```

### Accidentally deleted something
```bash
# Find the commit
git reflog
# Restore
git checkout <commit-hash> -- filename
```

---

## Wrap-Up

**Key takeaways:**

1. **Branches** isolate work - use them!
2. **Pull requests** enable code review
3. **Conflicts** are normal - learn to resolve them
4. **Commit messages** matter for history
5. **Communication** is key in team workflows

**Team workflow summary:**
1. Create feature branch
2. Make commits
3. Push branch
4. Open PR
5. Get review
6. Merge to main
7. Delete branch

**Next lab:** Design Workshop - planning larger systems!
