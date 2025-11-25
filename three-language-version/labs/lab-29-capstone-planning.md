# Lab 29: Capstone Planning

**Quarter 3, Week 9**
**Duration:** 90 minutes
**Format:** Individual/group workshop

## Overview

The capstone project is your chance to demonstrate everything you've learned. This lab helps you plan a project that showcases your skills across multiple paradigms and languages.

## Objectives

By the end of this lab, you will:
- [ ] Choose a project topic
- [ ] Define clear requirements
- [ ] Create a development plan
- [ ] Identify potential challenges

---

## Part 1: Project Brainstorming (20 minutes)

### Activity 1.1: What Interests You?

Consider projects in these categories:

**Games:**
- Text adventure with AI opponents
- Puzzle game with solver
- Simulation/strategy game

**Tools:**
- Code analysis/linting tool
- File organizer/renamer
- Data converter/transformer

**Data/Analysis:**
- Data visualization dashboard
- Pattern finder in datasets
- Log analyzer

**Web/API:**
- API client/wrapper
- Web scraper
- Chat application

**Language/Parsing:**
- Mini programming language
- Markdown/format converter
- Query language for data

**Educational:**
- Interactive tutorial
- Algorithm visualizer
- Quiz/flashcard system

### Activity 1.2: Project Requirements

Your capstone must include:

- [ ] **Multiple paradigms**: Use at least 2 (OOP, functional, procedural)
- [ ] **Data structures**: Implement or use advanced structures
- [ ] **Algorithm**: Include non-trivial algorithmic thinking
- [ ] **Documentation**: README, comments, usage guide
- [ ] **Testing**: Unit tests for core functionality
- [ ] **Reflection**: What you learned, challenges overcome

### Activity 1.3: Quick Ideas

Write down 3-5 project ideas:

1. _________________________________
2. _________________________________
3. _________________________________
4. _________________________________
5. _________________________________

For each, note:
- Why it interests you
- What's the core challenge
- What paradigm fits best

---

## Part 2: Project Scoping (20 minutes)

### Activity 2.1: Minimum Viable Project

Define the simplest version that would be complete:

**Project idea:** _________________________________

**MVP features:**
1. _________________________________
2. _________________________________
3. _________________________________

**Nice-to-have features:**
1. _________________________________
2. _________________________________
3. _________________________________

### Activity 2.2: Technical Assessment

**Core data structures needed:**
- [ ] Lists/Arrays
- [ ] Trees
- [ ] Graphs
- [ ] Hash tables
- [ ] Custom structures: _________________

**Algorithms/techniques:**
- [ ] Sorting/searching
- [ ] Graph traversal
- [ ] Dynamic programming
- [ ] Parsing
- [ ] Other: _________________

**Languages to use:**
- Primary: _________________
- Secondary (comparison/component): _________________

**Paradigms:**
- [ ] OOP (classes, inheritance)
- [ ] Functional (map/filter, immutability)
- [ ] Procedural
- [ ] Logic
- [ ] Other: _________________

### Activity 2.3: Risk Assessment

What could go wrong?

| Risk | Likelihood | Mitigation |
|------|------------|------------|
| Scope too large | | |
| Technical unknown | | |
| Time constraints | | |
| Integration issues | | |

---

## Part 3: Project Proposal (25 minutes)

### Activity 3.1: Write Your Proposal

Fill out this template:

```markdown
# Capstone Project Proposal

## Project Title
[Your title]

## One-Line Description
[What does it do in one sentence?]

## Problem Statement
[What problem does this solve? Who would use it?]

## Features
### Core Features (MVP)
1. [Feature 1]
2. [Feature 2]
3. [Feature 3]

### Extended Features (if time permits)
1. [Feature 4]
2. [Feature 5]

## Technical Approach

### Languages
- Primary: [Language] because [reason]
- Secondary: [Language] for [specific component]

### Paradigms
- [Paradigm 1] for [what]
- [Paradigm 2] for [what]

### Data Structures
- [Structure 1] for [purpose]
- [Structure 2] for [purpose]

### Algorithms
- [Algorithm/technique] for [purpose]

## Project Structure
```
project/
├── src/
│   ├── main.[ext]
│   ├── [module1]/
│   └── [module2]/
├── tests/
├── docs/
└── README.md
```

## Timeline
- Week 9: Planning and setup
- Week 10: Core implementation
- Week 11: Testing, polish, presentation

## Challenges Anticipated
1. [Challenge 1] - Plan to address: [how]
2. [Challenge 2] - Plan to address: [how]

## Success Criteria
How will you know it's done?
1. [Criterion 1]
2. [Criterion 2]
3. [Criterion 3]
```

### Activity 3.2: Peer Review

Pair up and review each other's proposals:

**Reviewer checklist:**
- [ ] Is the scope reasonable for 2 weeks?
- [ ] Are the technical requirements clear?
- [ ] Does it use multiple paradigms?
- [ ] Are risks identified?
- [ ] Is success criteria measurable?

**Feedback:**
- Strengths: _________________________________
- Concerns: _________________________________
- Suggestions: _________________________________

---

## Part 4: Development Plan (20 minutes)

### Activity 4.1: Task Breakdown

Break your project into concrete tasks:

**Setup (Week 9 remaining):**
- [ ] Create project structure
- [ ] Set up version control
- [ ] Create initial README
- [ ] Set up testing framework

**Core Implementation (Week 10):**
- [ ] Task 1: _________________________________
- [ ] Task 2: _________________________________
- [ ] Task 3: _________________________________
- [ ] Task 4: _________________________________
- [ ] Task 5: _________________________________

**Polish (Week 11):**
- [ ] Write tests
- [ ] Documentation
- [ ] Bug fixes
- [ ] Extended features
- [ ] Prepare presentation

### Activity 4.2: First Steps

What can you do TODAY to start?

1. _________________________________
2. _________________________________
3. _________________________________

### Activity 4.3: Define Interfaces

Before coding, sketch out your main interfaces:

```python
# Example: Game project
class Game:
    def __init__(self): ...
    def start(self): ...
    def process_command(self, cmd: str) -> str: ...
    def is_over(self) -> bool: ...

class Player:
    def __init__(self, name: str): ...
    def take_action(self, game: Game) -> Action: ...

class AIPlayer(Player):
    def take_action(self, game: Game) -> Action: ...
```

**Your interfaces:**

```
[Write your main class/function signatures]
```

---

## Part 5: Getting Started (5 minutes)

### Activity 5.1: Create Your Repo

```bash
mkdir capstone-project
cd capstone-project
git init
touch README.md
git add README.md
git commit -m "Initial commit"
```

### Activity 5.2: Initial README

Write a README with:
- Project title
- Brief description
- Setup instructions (even if "TBD")
- Feature list

### Activity 5.3: Set a Personal Deadline

**MVP complete by:** _________________

**Presentation ready by:** _________________

---

## Example Project Ideas (Detailed)

### Idea 1: Text Adventure Engine

**Description:** Create an engine for text adventure games with:
- Room/map graph traversal
- Inventory management
- NPCs with dialogue trees
- Save/load functionality

**Technical highlights:**
- OOP: Room, Player, Item classes
- Functional: Command parsing, state transformations
- Data structure: Graph for map
- Algorithm: Pathfinding for hints

### Idea 2: Expression Evaluator

**Description:** A calculator that parses and evaluates mathematical expressions:
- Infix notation parsing
- Variables and functions
- Graphing capability (optional)

**Technical highlights:**
- Parsing: Expression trees
- Functional: Tree evaluation
- OOP: Node types, operators

### Idea 3: Task Scheduler

**Description:** Priority-based task scheduler with dependencies:
- Tasks with priorities and deadlines
- Dependency graph
- Schedule optimization

**Technical highlights:**
- Graph: Dependency DAG
- Algorithm: Topological sort, scheduling
- OOP: Task, Scheduler classes

### Idea 4: Music Playlist Analyzer

**Description:** Analyze music listening data:
- Import from file
- Statistics and patterns
- Recommendation engine

**Technical highlights:**
- Data structures: Hash maps, heaps
- Functional: Data transformation pipeline
- Algorithm: Similarity matching

---

## Wrap-Up

**By next lab, have:**
- [ ] Finalized project choice
- [ ] Detailed proposal document
- [ ] Initial code structure
- [ ] First feature in progress

**Remember:**
- Start simple, add complexity
- Test as you go
- Ask for help early
- Document your journey

**Next lab:** Work Time - implement your vision!
