# Instructor Guide
## Teaching the Three-Language CS Curriculum

This guide provides teaching strategies, lesson tips, and pedagogical advice for instructors delivering the multi-language introductory computer science curriculum.

---

## Table of Contents

1. [Philosophy & Rationale](#philosophy--rationale)
2. [Before the Course](#before-the-course)
3. [Teaching Strategies](#teaching-strategies)
4. [Week-by-Week Teaching Notes](#week-by-week-teaching-notes)
5. [Lesson-Specific Tips](#lesson-specific-tips)
6. [Common Student Challenges](#common-student-challenges)
7. [Assessment Strategies](#assessment-strategies)
8. [Adapting the Curriculum](#adapting-the-curriculum)
9. [Resources for Instructors](#resources-for-instructors)

---

## Philosophy & Rationale

### Why Multiple Languages?

**Traditional approach problems:**
- Students conflate language syntax with CS concepts
- "I learned Python" becomes identity, hindering transfer
- Single-paradigm exposure limits thinking
- Difficulty adapting to workplace requirements

**Multi-language approach benefits:**
- Concepts are clearly separated from syntax
- Transfer skills built from day one
- Paradigm diversity expands problem-solving toolbox
- Students become adaptable learners

### The "Three Deep, Four Broad" Model

**Core Languages (Deep):** Python, C++, Haskell
- Used in every lesson
- Students achieve genuine proficiency
- Cover procedural, OOP, and functional paradigms

**Minor Languages (Broad):** Racket, C, Rust, Prolog
- 1-3 week focused modules
- Literacy, not fluency
- Expand paradigm exposure

### Key Principle: Concepts Before Syntax

Every concept should be taught language-agnostically first:

**Do:**
- "Today we learn about loops—ways to repeat actions"
- "Let's see how Python, C++, and Haskell each express this"

**Don't:**
- "Today we learn Python's for loop"
- Focus on syntax before understanding purpose

---

## Before the Course

### Instructor Preparation

#### 1. Environment Setup (Critical!)
Before the first class, ensure you can run:
```bash
python3 --version    # Python 3.8+
g++ --version        # C++17 support
ghc --version        # GHC 8.10+
```

Test-run all code examples in lessons 1-4 to catch environment issues.

#### 2. IDE/Editor Setup
Recommend VS Code with extensions:
- Python (Microsoft)
- C/C++ (Microsoft)
- Haskell (Haskell Language Server)

Prepare a live-coding setup with:
- Large font (18pt minimum)
- Split screens for side-by-side comparison
- Terminal visible for running programs

#### 3. Review All Three Languages
Even if you're an expert in one language, refresh yourself on all three. Students will ask questions about any of them.

Common instructor knowledge gaps:
- **Python experts:** Haskell's type signatures, do notation, monads
- **C++ experts:** Python's duck typing, Haskell's immutability
- **Haskell experts:** Python's OOP, C++ memory management

#### 4. Prepare Fallback Examples
Have simplified examples ready for each concept. When the planned example confuses students, pivot to something simpler.

### First Day Setup

#### Lab Environment Check
In the first lab, have students:
1. Verify all three languages installed
2. Run Hello World in each language
3. Test the REPL (Python, GHCi)
4. Create a test Git repository

Budget 30-60 minutes for this. Some students will have issues.

#### Common First-Day Problems
- macOS: Xcode command line tools not installed
- Windows: PATH not configured for compilers
- Linux: Missing packages (`build-essential`, etc.)
- All: Haskell installation failures (GHCup is most reliable)

---

## Teaching Strategies

### Live Coding Best Practices

#### Side-by-Side Demonstration
Use split screens or three terminal panes to show the same concept simultaneously:

```
┌─────────────────┬─────────────────┬─────────────────┐
│     Python      │      C++        │    Haskell      │
├─────────────────┼─────────────────┼─────────────────┤
│ def add(x, y):  │ int add(int x,  │ add :: Int ->   │
│     return x+y  │        int y) { │       Int -> Int│
│                 │   return x + y; │ add x y = x + y │
│ print(add(2,3)) │ }               │ main = print $  │
│                 │                 │   add 2 3       │
└─────────────────┴─────────────────┴─────────────────┘
```

#### Make Mistakes Intentionally
Students learn from seeing errors:
1. Forget a semicolon in C++
2. Use wrong types in Haskell
3. Make indentation errors in Python

Talk through the error messages. This normalizes debugging.

#### Type Code, Don't Paste
Typing code live:
- Shows the thinking process
- Demonstrates IDE features (autocomplete, error highlighting)
- Creates natural pause points for questions
- Makes mistakes (which is educational!)

### Pacing Strategies

#### The 50-30-20 Rule (Early Weeks)
For weeks 1-3:
- 50% of examples in Python (most accessible)
- 30% in C++ (adds compilation, types)
- 20% in Haskell (introduces functional)

#### The 33-33-33 Rule (Middle Weeks)
For weeks 4-8:
- Equal time on all three languages
- Every concept demonstrated in all three
- Exercises require at least 2 languages

#### Let Haskell Be Hard
Don't over-explain Haskell early on. It's okay for students to be confused:
- "This will make more sense in a few weeks"
- "Just notice how different this is"
- "The type signature tells us a lot—we'll unpack it later"

Students often have an "aha moment" around week 5-6 when Haskell suddenly clicks.

### Discussion Facilitation

#### Paradigm Comparison Questions
Regularly ask:
- "Which language made this easier? Why?"
- "Which was more readable?"
- "When might you choose C++ over Python here?"

#### Socratic Method
Instead of answering directly:
- Student: "Why doesn't Haskell have loops?"
- Instructor: "What do loops do? How does Haskell achieve the same thing?"

#### Think-Pair-Share
For conceptual questions:
1. Individual thinking (30 seconds)
2. Discuss with neighbor (1-2 minutes)
3. Share with class

### Lab Session Structure

Recommended 2-hour lab format:

| Time | Activity |
|------|----------|
| 0:00 | Warm-up exercise (review previous material) |
| 0:15 | New concept mini-lecture (10-15 min) |
| 0:30 | Guided practice (instructor demos, students follow) |
| 0:50 | Independent/pair practice |
| 1:30 | Challenge problems (optional) |
| 1:45 | Wrap-up, questions, preview next lab |

---

## Week-by-Week Teaching Notes

### Week 1: Foundations

**Goals:**
- All students can run programs in all three languages
- Basic understanding of compiled vs interpreted
- Comfort with edit-run cycle

**Common Issues:**
- Environment setup problems (expect this!)
- Overwhelmed by three languages at once
- Haskell type signatures confusing

**Tips:**
- First day: ONLY setup and Hello World
- Celebrate small wins ("You just ran Haskell!")
- Emphasize concepts over syntax
- Don't rush—foundation matters

**Key Message:** "You're not learning three languages—you're learning *programming* through three lenses."

### Week 2: Control Structures

**Goals:**
- Conditionals in all three languages
- Understanding of boolean logic
- Introduction to iteration

**Focus Areas:**
- Show how `if/else` is universal
- Compare loop syntax but emphasize the *concept*
- Python: `for x in range()`
- C++: `for (int i = 0; i < n; i++)`
- Haskell: recursion (loops come later via `map`)

**Tips:**
- Start with conditionals (easier transfer)
- Use real-world analogies ("If it's raining, take umbrella")
- Don't force Haskell recursion deeply yet—just show it exists

### Week 3: Functions & Abstraction

**Goals:**
- Define and call functions in all languages
- Understand parameters vs arguments
- Begin to see functions as building blocks

**Key Concepts:**
- Functions as reusable abstractions
- Scope (local vs global)
- Return values vs side effects

**Tips:**
- Live code a small function, then call it
- Build incrementally: start with no parameters, add them
- Highlight Haskell's pure functions: "No side effects—ever!"

**Git Module:**
- Integrate Git early (this week is good)
- Simple workflow: init, add, commit
- Don't overwhelm with branches yet

### Week 4: Data Structures

**Goals:**
- Work with lists/arrays in all languages
- Understand dictionaries/maps
- See immutability vs mutability

**Key Comparison:**
- Python: `list` (mutable), `dict`
- C++: `std::vector`, `std::map`
- Haskell: `[]` (list), `Map` (immutable)

**Tips:**
- Spend significant time on lists—they're fundamental
- Immutability in Haskell is a paradigm shift—explain it carefully
- "In Haskell, you don't change a list—you create a new one"

### Week 5: Recursion

**Goals:**
- Understand recursive thinking
- Write recursive functions in all languages
- Compare recursion vs iteration

**This is often the hardest lesson.** Budget extra time.

**Teaching Recursion:**
1. Start with concrete examples (factorial, fibonacci)
2. Draw the call stack visually
3. Emphasize base case + recursive case pattern
4. Show the same function in all three languages

**Haskell Shines Here:**
```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```
This is more natural than imperative loops for recursion.

**Common Misconceptions:**
- "Recursion is always slower" (not with tail recursion)
- "I should avoid recursion" (it's fundamental in FP)
- Confusing recursion with iteration

### Week 6: Object-Oriented Programming

**Goals:**
- Understand classes, objects, methods
- See inheritance and polymorphism
- Compare OOP with functional approaches

**Key Teaching Points:**
- OOP is ONE paradigm, not THE paradigm
- Python and C++ are heavily OOP
- Haskell uses different abstractions (type classes)

**Haskell Note:**
Don't force OOP onto Haskell. Instead, show how Haskell achieves similar goals differently:
- Type classes for polymorphism
- Data types for structured data
- "Haskell proves you don't need OOP for good code"

**Midterm Prep:**
- Review sessions
- Practice problems from all 7 lessons
- Emphasize conceptual understanding

### Week 7-8: Higher-Order Functions & Minor Languages

**Goals:**
- Master map, filter, reduce
- Introduce Racket (Lisp)
- Introduce C (systems programming)

**Racket Module:**
- Show that parentheses aren't scary
- "Code as data" is the key insight
- Emphasize minimalism: everything is a list or function

**C Module:**
- Manual memory management
- Pointers (basic intro)
- "This is what C++ does for you"

**Tips:**
- These modules are exposure, not mastery
- Keep examples simple
- Connect back to core languages constantly

### Week 9-10: Types, Errors & More Languages

**Goals:**
- Deep dive on type systems
- Error handling strategies
- Rust or Prolog exposure

**Rust Module:**
- Ownership model is key
- "Safe systems programming"
- Compare to C and C++

**Prolog Module:**
- Completely different paradigm
- Logic programming: describe *what*, not *how*
- Mind-bending—students will be confused (that's okay!)

**Student Choice:**
Let students choose Rust OR Prolog for deeper study. This personalizes the learning and reduces overwhelm.

### Week 11-13: Algorithms & Data Structures

**Goals:**
- Big-O analysis
- Classic algorithms
- Implement ADTs

**Teaching Tips:**
- Algorithm analysis is language-agnostic—emphasize this
- Implement sorting in all three languages
- Visual demonstrations help (sorting visualizations)

**Testing Module:**
- Integrate testing with data structure implementation
- "Write tests first" for at least one exercise
- Show different testing frameworks (pytest, catch2, HUnit)

### Week 14-15: Advanced Topics & Capstone

**Goals:**
- Trees and graphs
- Capstone project
- Course synthesis

**Capstone Support:**
- Office hours crucial
- Encourage minor language use
- Focus on process, not just product

**Final Review:**
- Comprehensive but emphasize concepts
- Multi-language comparison questions
- "You've learned to learn languages"

---

## Lesson-Specific Tips

### Lesson 1: Hello, World!

**Duration:** 2-3 hours including setup

**Key Points:**
- This is more about environment than code
- Celebrate successful runs
- Introduce the comparison approach

**Live Demo Script:**
1. "Let's write Hello World in Python" (write, run)
2. "Now the same thing in C++" (write, compile, run)
3. "And finally Haskell" (write, run with runhaskell)
4. "Notice: same task, different expressions"

**Discussion Questions:**
- Why does C++ require compilation?
- What's that type signature in Haskell doing?

### Lesson 2: Variables & Types

**Duration:** 3-4 hours

**Key Points:**
- Type systems are a spectrum
- Static vs dynamic typing
- Type inference (Haskell)

**Great Demonstration:**
```python
# Python: dynamic typing
x = 5
x = "hello"  # This works!
```

```cpp
// C++: static typing
int x = 5;
// x = "hello";  // Error!
```

```haskell
-- Haskell: static but inferred
x = 5  -- GHC knows x is a Num
```

### Lesson 3: Control Flow

**Duration:** 3-4 hours

**Key Points:**
- Conditionals are universal
- Loops vs recursion (paradigm difference)
- Guards in Haskell

**Teaching Tip:**
Show the same conditional in all three languages on one slide/screen:

```python
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")
```

```cpp
if (x > 0) {
    cout << "positive";
} else if (x < 0) {
    cout << "negative";
} else {
    cout << "zero";
}
```

```haskell
classify x
    | x > 0     = "positive"
    | x < 0     = "negative"
    | otherwise = "zero"
```

### Lesson 4: Functions

**Duration:** 4-5 hours (crucial lesson)

**Key Points:**
- Functions as first-class values
- Pure vs impure functions
- Closures (brief intro)

**Spend Extra Time On:**
- Function definition syntax differences
- Parameter passing (by value, reference)
- Return types (explicit vs implicit)

### Lesson 5: Data Structures

**Duration:** 4-5 hours

**Key Points:**
- Lists are fundamental
- Mutability vs immutability
- Choosing the right structure

**Comparison Exercise:**
Append to a list 1000 times. Compare:
- Python list: mutable, fast append
- C++ vector: mutable, fast append
- Haskell list: immutable, new list each time (slow naive approach)

Then show Haskell's efficient approach (building in reverse, then reversing).

### Lesson 6: Recursion

**Duration:** 5-6 hours (hardest lesson)

**Teaching Strategy:**
1. Concrete examples first (factorial)
2. Draw call stacks on the board
3. Pattern: base case + recursive case
4. Tail recursion optimization
5. Practice, practice, practice

**Visual Aid:**
Draw the recursive calls as a tree:
```
factorial(4)
├── 4 * factorial(3)
│       ├── 3 * factorial(2)
│       │       ├── 2 * factorial(1)
│       │       │       └── 1 * factorial(0)
│       │       │               └── 1 (base case)
```

### Lesson 7: Object-Oriented Programming

**Duration:** 4-5 hours

**Key Points:**
- Classes group data and behavior
- Inheritance for code reuse
- Polymorphism for flexibility

**Balance the Narrative:**
OOP is powerful but not universal. Show both:
- "OOP makes modeling real-world objects natural"
- "But Haskell proves you don't need OOP"

### Lesson 8: Higher-Order Functions

**Duration:** 3-4 hours

**Key Points:**
- Functions as values
- Map, filter, reduce
- Lambda expressions

**This is where Haskell excels:**
```haskell
map (*2) [1,2,3,4]     -- [2,4,6,8]
filter even [1,2,3,4]   -- [2,4]
foldl (+) 0 [1,2,3,4]   -- 10
```

Show Python and C++ equivalents, but emphasize Haskell's elegance.

### Lessons 9-11: Types, Pattern Matching, Errors

**Combined Duration:** 8-10 hours

**Key Points:**
- Type systems provide safety guarantees
- Pattern matching is powerful destructuring
- Error handling: exceptions vs Result types

**Teaching Order:**
Pattern matching → Type systems → Error handling
(Pattern matching makes the type system concrete)

### Lessons 12-16: Algorithms & Data Structures

**Combined Duration:** 15-20 hours

**Key Points:**
- Big-O for complexity analysis
- Classic algorithms (sort, search)
- Fundamental ADTs (stack, queue, tree, graph)

**Implementation Strategy:**
For each data structure:
1. Explain abstractly (what operations, what guarantees)
2. Show Python (simplest)
3. Show C++ (with types, templates)
4. Show Haskell (immutable, recursive)

---

## Common Student Challenges

### "I'm Mixing Up Syntax!"

**Response:** "That's normal and actually good! It means you're internalizing multiple languages. Over time, you'll develop mental 'modes' for each language."

**Tips:**
- Use different IDE themes per language
- Practice switching intentionally
- Emphasize concepts over syntax

### "Haskell Makes No Sense!"

**Response:** "Haskell is genuinely different. It's meant to rewire your thinking. Give it time—most students have an 'aha' moment around week 5-6."

**Tips:**
- Don't over-explain early
- Focus on reading Haskell, not just writing
- Use the REPL extensively
- Pattern match examples

### "C++ Has Too Much Boilerplate!"

**Response:** "Yes, C++ makes you be explicit. That explicitness gives you control that Python doesn't. There's a tradeoff between brevity and control."

**Tips:**
- Show what the boilerplate does
- Explain `#include`, `main()`, return codes
- Compare to Python's implicit behavior

### "Why Do I Need to Learn All Three?"

**Response:** "You're not really learning three languages—you're learning CS concepts that appear in every language. By seeing the same concept three times, you understand it more deeply than learning just one syntax."

### "Which Language Is Best?"

**Response:** "There is no best language—only better tools for specific jobs. Python for quick scripting, C++ for performance, Haskell for correctness. You're learning to choose the right tool."

### Struggling Students

**Signs:**
- Falling behind on exercises
- Only completing work in one language
- Confusion about basic concepts

**Interventions:**
- One-on-one office hours
- Focus on Python first (most accessible)
- Pair with stronger student
- Reduce language requirements temporarily

### Advanced Students

**Signs:**
- Finishing exercises quickly
- Asking deep questions
- Bored in lectures

**Extensions:**
- Challenge problems in lessons
- Minor language exploration early
- Contribute to course materials (bug fixes, extra examples)
- TA or peer tutoring

---

## Assessment Strategies

### Formative Assessment

#### Weekly Concept Checks
Quick questions at start/end of class:
- "Explain recursion without mentioning a programming language"
- "What's the difference between static and dynamic typing?"
- Exit tickets: "What's still confusing?"

#### Code Reviews
Pair students for code review:
- Student A explains their Python solution
- Student B explains their C++ solution
- Discussion: which is cleaner? why?

### Summative Assessment

#### Exam Design

**Do:**
- Ask conceptual questions
- Show code in multiple languages
- "What does this code do?" (any language)
- "How would you solve X?" (pseudocode okay)

**Don't:**
- Heavy syntax questions
- Language-specific trivia
- Memorization over understanding

**Sample Questions:**

1. *Conceptual:* "Explain the difference between compiled and interpreted languages. Give an example of each from this course."

2. *Code Reading:* "What does this Haskell function compute?"
   ```haskell
   mystery [] = 0
   mystery (x:xs) = 1 + mystery xs
   ```

3. *Comparison:* "Write a function that returns the sum of a list of numbers. Show your solution in TWO different languages from the course."

4. *Analysis:* "What is the time complexity of the following algorithm? Justify your answer."

#### Project Rubrics

Each project should assess:
- **Correctness (40%):** Does it work?
- **Design (25%):** Is it well-structured?
- **Documentation (15%):** Is it explained?
- **Testing (10%):** Are edge cases handled?
- **Process (10%):** Was version control used?

#### Capstone Assessment

The capstone should demonstrate:
- Ability to use a minor language
- Transfer of concepts from core languages
- Reflection on paradigm differences
- Complete, working software

---

## Adapting the Curriculum

### Shorter Courses (8-10 weeks)

**Remove:**
- One of Rust/Prolog
- Detailed Git module (basic only)
- Some algorithm lessons (14-16)

**Compress:**
- Combine variables/types + control flow
- Faster pace on OOP
- Shorter minor language modules

### Longer Courses (16+ weeks)

**Add:**
- More project time
- Deeper minor language modules
- Additional paradigms (logic, concurrent)
- Student presentations

### Different Language Choices

The curriculum can adapt to different core languages:

**Java instead of C++:**
- More emphasis on OOP
- Less on manual memory management
- Widely used in AP CS

**JavaScript instead of Python:**
- Web development context
- Async programming
- Less scientific computing

**OCaml/F# instead of Haskell:**
- More industry relevance
- Less "alien" syntax
- Still functional

### Non-CS Audiences

**For data science students:**
- Emphasize Python
- Add numpy/pandas examples
- Focus on data-centric problems

**For engineering students:**
- Emphasize C++
- Add numerical computing
- Focus on performance

---

## Resources for Instructors

### Books

- *How to Design Programs* (Felleisen et al.) - Pedagogical approach
- *Structure and Interpretation of Computer Programs* - Classic multi-paradigm
- *Programming Languages: Application and Interpretation* - PLT perspective

### Papers

- "The Camel Has Two Humps" (Dehnadi, Bornat) - Predicting success
- "Learning to Program is Easy" (Luxton-Reilly) - Debunking myths
- "Notional Machines" (Sorva) - Teaching mental models

### Online Communities

- SIGCSE (ACM Special Interest Group on CS Education)
- Computing Education Research Blog
- r/CSEducation on Reddit

### Language-Specific Resources

**Python Teaching:**
- Python.org Education page
- Real Python tutorials
- Runestone Interactive (free online textbook platform)

**C++ Teaching:**
- learncpp.com (free, comprehensive)
- isocpp.org (official resources)
- Compiler Explorer (godbolt.org)

**Haskell Teaching:**
- School of Haskell
- Haskell Wikibook
- LYAH (Learn You a Haskell)

---

## Final Thoughts

### Embrace the Philosophy

The multi-language approach is challenging for students AND instructors. Embrace it:
- Confusion is part of learning
- Comparisons deepen understanding
- No single language is "right"

### Model the Mindset

Students watch what you do:
- Show that you look up syntax
- Admit when you're unsure
- Demonstrate language-agnostic thinking

### Celebrate Progress

Multi-language proficiency is a significant achievement:
- Week 1: "I've never programmed"
- Week 10: "I can write code in 5 languages"

That's worth celebrating.

### Iterate and Improve

This curriculum is living:
- Collect student feedback
- Note what works and what doesn't
- Contribute improvements back

---

**Good luck, and happy teaching!**

For questions or to contribute to the curriculum, please open an issue in the repository.
