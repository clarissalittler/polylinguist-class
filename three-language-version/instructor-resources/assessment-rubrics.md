# Assessment Rubrics

This document provides detailed rubrics for all course assessments including projects, exams, exercises, and participation.

---

## General Assessment Philosophy

### Concepts Over Syntax
- Partial credit for demonstrating understanding even if code doesn't compile
- Language-agnostic questions on exams
- Accept multiple correct approaches

### Multi-Language Demonstration
- Bonus for implementing in multiple languages
- Must show competency in at least 2 core languages
- Capstones require minor language use

### Process Over Product
- Version control usage is graded
- Iteration and improvement valued
- Testing attempts recognized

---

## Project Rubrics

### Generic Project Rubric (100 points)

| Criterion | Excellent (A) | Good (B) | Satisfactory (C) | Needs Improvement (D) | Unsatisfactory (F) |
|-----------|--------------|----------|-----------------|---------------------|-------------------|
| **Correctness (40%)** | All requirements met, handles edge cases | Most requirements met, minor bugs | Basic requirements met | Major bugs or missing features | Does not run or completely wrong |
| | 36-40 | 32-35 | 28-31 | 24-27 | 0-23 |
| **Design (25%)** | Excellent organization, clear abstractions | Good structure, mostly clear | Adequate organization | Poor structure, confusing | No clear organization |
| | 23-25 | 20-22 | 18-19 | 15-17 | 0-14 |
| **Documentation (15%)** | Clear comments, good README, explained decisions | Adequate comments, basic README | Some comments | Minimal documentation | No documentation |
| | 14-15 | 12-13 | 11 | 9-10 | 0-8 |
| **Testing (10%)** | Comprehensive tests, edge cases covered | Good test coverage | Basic tests present | Minimal testing | No tests |
| | 10 | 8-9 | 7 | 5-6 | 0-4 |
| **Process (10%)** | Regular commits, good messages, clean history | Adequate commits | Some version control | Minimal commits | No version control |
| | 10 | 8-9 | 7 | 5-6 | 0-4 |

### Project-Specific Additions

#### Project 1: Text Statistics Tool
Additional criteria:
- File handling (+5 bonus for robust error handling)
- Number formatting (+3 bonus for locale-aware formatting)
- Multiple language implementation (+10 bonus)

#### Project 2: Number Guessing Game
Additional criteria:
- Input validation (+5 bonus for bulletproof validation)
- Statistics tracking (+5 bonus for persistent stats)
- Difficulty levels (+5 bonus)

#### Project 3: Todo List Manager
Additional criteria:
- Data persistence reliability (+5 bonus)
- Racket capstone implementation (+25 bonus with reflection)

#### Project 4: Expression Evaluator
Additional criteria:
- Operator precedence correctness (critical - 50% of correctness grade)
- Haskell parser combinators (+25 bonus with reflection)
- Error messages with position (+5 bonus)

#### Project 5: Polyglot Build Tool
Additional criteria:
- Number of languages supported (+2 per additional language beyond 3)
- Dependency tracking (+15 bonus)
- Watch mode (+10 bonus)

#### Project 6: Mini Programming Language (Capstone)
Additional criteria:
- Grammar design quality (10 points)
- Error messages with line numbers (+10 bonus)
- Functions with recursion (+15 bonus)
- Minor language implementation OR detailed report (required for full credit)

---

## Exercise Rubrics

### Weekly Exercises (5 points each)

| Score | Description |
|-------|-------------|
| 5 | Complete, correct, shows understanding |
| 4 | Mostly correct, minor errors |
| 3 | Partially correct, demonstrates effort |
| 2 | Attempted but significant errors |
| 1 | Minimal attempt |
| 0 | Not submitted or no meaningful attempt |

### Multi-Language Exercises

When exercise requires multiple languages:
- All languages attempted: full points possible
- 2 of 3 languages: maximum 85%
- 1 language only: maximum 70%

---

## Exam Rubrics

### Midterm Exam (100 points)

#### Section A: Conceptual Questions (30 points)

**Example Question Types:**
1. "Explain the difference between compiled and interpreted languages" (5 pts)
2. "What is recursion? Give an example without code" (5 pts)
3. "Compare mutable vs immutable data structures" (5 pts)

| Score | Criteria |
|-------|----------|
| 5 | Complete, accurate explanation with examples |
| 4 | Mostly accurate, minor omissions |
| 3 | Basic understanding demonstrated |
| 2 | Partial understanding, some errors |
| 1 | Minimal understanding |
| 0 | No understanding or not attempted |

#### Section B: Code Reading (30 points)

**Format:** Given code in Python, C++, or Haskell, explain what it does.

Example:
```haskell
mystery [] = 0
mystery (x:xs) = 1 + mystery xs
```

| Score | Criteria |
|-------|----------|
| 10 | Correctly identifies function (length), explains recursion |
| 8 | Identifies purpose, minor explanation gaps |
| 6 | Partial understanding |
| 4 | Some correct observations |
| 2 | Minimal understanding |
| 0 | Incorrect or not attempted |

#### Section C: Code Writing (40 points)

**Format:** Write a function to solve a problem (student chooses language).

Example: "Write a function that returns the sum of all even numbers in a list."

| Score | Criteria |
|-------|----------|
| 10 | Correct, efficient, handles edge cases |
| 8 | Correct with minor issues |
| 6 | Mostly correct, some bugs |
| 4 | Demonstrates understanding, doesn't fully work |
| 2 | Shows some relevant knowledge |
| 0 | Incorrect or not attempted |

**Partial credit guide:**
- Correct algorithm, syntax errors: 70-80%
- Correct approach, logic errors: 50-60%
- Shows understanding of concepts: 30-40%

### Final Exam (100 points)

Same structure as midterm, plus:

#### Section D: Paradigm Comparison (20 points)

**Format:** Compare approaches across languages/paradigms.

Example: "How would you implement a stack in Python (OOP), C++ (templates), and Haskell (algebraic types)? Compare the approaches."

| Score | Criteria |
|-------|----------|
| 20 | Thoughtful comparison, shows deep understanding |
| 16 | Good comparison, minor gaps |
| 12 | Basic comparison, some insight |
| 8 | Partial comparison |
| 4 | Minimal comparison |
| 0 | Not attempted |

#### Section E: Algorithm Analysis (10 points)

**Format:** Analyze time/space complexity of given code.

| Score | Criteria |
|-------|----------|
| 10 | Correct Big-O with justification |
| 8 | Correct Big-O, weak justification |
| 6 | Close but not exact |
| 4 | Shows understanding of analysis |
| 2 | Minimal understanding |
| 0 | Incorrect |

---

## Participation Rubric (5% of grade)

### Criteria

| Score | Description |
|-------|-------------|
| A (5%) | Consistently engaged, asks questions, helps peers, attends labs |
| B (4%) | Regularly engaged, occasional participation |
| C (3%) | Attends most sessions, minimal participation |
| D (2%) | Infrequent attendance or participation |
| F (0-1%) | Rarely attends or participates |

### Components
- Lab attendance (40%)
- Class participation (30%)
- Discussion forum contributions (20%)
- Peer assistance (10%)

---

## Reflection Rubrics

### Project Reflections (included in project grade)

Each project requires a REFLECTION.md file.

| Score | Criteria |
|-------|----------|
| 5 | Thoughtful, specific answers; shows learning |
| 4 | Good answers, some depth |
| 3 | Basic answers, surface level |
| 2 | Minimal effort |
| 1 | Incomplete |
| 0 | Missing |

### Capstone Paradigm Report (if choosing report over implementation)

1500+ word report on paradigm comparison.

| Score | Criteria |
|-------|----------|
| 25 | Deep analysis, specific examples, insightful comparisons |
| 20 | Good analysis, clear examples |
| 15 | Adequate analysis, some insight |
| 10 | Basic analysis, limited depth |
| 5 | Minimal effort |
| 0 | Missing |

---

## Late Work Policy

### Standard Penalties

| Days Late | Penalty |
|-----------|---------|
| 1 day | -10% |
| 2 days | -20% |
| 3 days | -30% |
| 4 days | -40% |
| 5 days | -50% |
| 6+ days | No credit (without extension) |

### Extensions
- Request before deadline
- Documentation required for emergencies
- One "free" 48-hour extension per semester (no questions asked)

---

## Academic Integrity

### Collaboration Guidelines

**Allowed:**
- Discussing concepts and approaches
- Helping debug (pointing out issues, not fixing)
- Study groups for exam prep
- Sharing resources and documentation

**Not Allowed:**
- Copying code from others
- Sharing solutions
- Using AI to generate solutions without understanding
- Submitting work done by others

### Violation Consequences

| Offense | Consequence |
|---------|-------------|
| First | Zero on assignment, warning |
| Second | Failing grade in course |
| Any | Reported to academic integrity office |

---

## Grade Calculation

### Weight Distribution

| Component | Weight |
|-----------|--------|
| Exercises | 20% |
| Projects (6 total) | 30% |
| Midterm | 15% |
| Final Exam | 20% |
| Capstone Project | 10% |
| Participation | 5% |

### Grade Boundaries

| Letter | Percentage |
|--------|------------|
| A | 93-100% |
| A- | 90-92% |
| B+ | 87-89% |
| B | 83-86% |
| B- | 80-82% |
| C+ | 77-79% |
| C | 73-76% |
| C- | 70-72% |
| D | 60-69% |
| F | <60% |

---

## Special Accommodations

### Extra Time
Students with documented accommodations receive 1.5x or 2x time on exams as specified.

### Alternative Assessments
For students who cannot complete standard assessments, discuss alternatives with instructor.

### Language Preference
Students may request to be assessed primarily in one core language while still demonstrating awareness of all three.

---

## Rubric Usage Tips for Instructors

1. **Calibrate with TAs** - Grade same submission, compare scores
2. **Be consistent** - Use rubric for every submission
3. **Provide feedback** - Points alone don't help learning
4. **Look for understanding** - Syntax errors < conceptual errors
5. **Recognize improvement** - Note when students address previous feedback

---

## Sample Feedback Comments

### Positive
- "Excellent use of recursion here - very idiomatic Haskell!"
- "Good error handling - you thought about edge cases"
- "Nice comparison in your reflection - you're developing language-agnostic thinking"

### Constructive
- "Consider extracting this repeated code into a function"
- "The algorithm is correct, but the time complexity could be improved"
- "Try to add comments explaining your approach"

### Concerning
- "This code doesn't compile - please test before submitting"
- "Missing required reflection questions"
- "This appears very similar to [other submission] - please see me"
