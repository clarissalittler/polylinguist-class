# Module 0: Computational Thinking

## Overview

**Computational thinking** is the mental process of approaching problems the way a computer scientist does. It's not about memorizing syntax or learning a specific language—it's about developing a **problem-solving mindset** that applies to any programming challenge.

This module should be studied **before or alongside** your first programming lessons. The skills you learn here will make you a better programmer regardless of which language you use.

## Learning Objectives

By the end of this module, you will be able to:

1. Break complex problems into smaller, manageable parts
2. Recognize patterns across different problems
3. Think abstractly to generalize solutions
4. Design step-by-step algorithms
5. Anticipate edge cases and errors
6. Approach debugging systematically
7. Communicate your problem-solving approach clearly

## What is Computational Thinking?

**Computational thinking** is a problem-solving process that includes:

1. **Decomposition** - Breaking problems into smaller pieces
2. **Pattern Recognition** - Finding similarities with problems you've solved before
3. **Abstraction** - Focusing on important information, ignoring irrelevant details
4. **Algorithm Design** - Creating step-by-step solutions

These skills apply **beyond programming**—to planning projects, organizing information, making decisions, and solving everyday problems.

---

## The Four Pillars of Computational Thinking

### 1. Decomposition: Breaking Down Problems

**Definition:** Divide a complex problem into smaller, more manageable sub-problems.

#### Example: Making a Sandwich

**Big problem:** "Make a sandwich"

**Decomposed:**
1. Get ingredients
   - Bread
   - Filling (cheese, ham, etc.)
   - Condiments
2. Prepare ingredients
   - Slice bread (if needed)
   - Slice vegetables (if needed)
3. Assemble sandwich
   - Place bread slice
   - Add fillings in order
   - Add top bread slice
4. Serve
   - Cut sandwich (optional)
   - Place on plate

**Why this helps:** Each small step is easier to think about and execute.

#### Example: Building a Contact Management System

**Big problem:** "Create a contact manager"

**Decomposed:**
1. Store contacts
   - Design contact data structure
   - Implement adding contacts
   - Implement removing contacts
2. Search contacts
   - Search by name
   - Search by phone
3. Display contacts
   - List all contacts
   - Show single contact details
4. Persistence
   - Save to file
   - Load from file

#### Programming Application

```python
# Bad: One giant function doing everything
def do_everything():
    # 200 lines of code...
    pass

# Good: Decomposed into smaller functions
def get_user_input():
    pass

def validate_input(data):
    pass

def process_data(data):
    pass

def save_results(results):
    pass

def main():
    data = get_user_input()
    if validate_input(data):
        results = process_data(data)
        save_results(results)
```

**Practice:** When faced with a problem, ask yourself:
- "What are the major steps?"
- "Can I break this step into smaller steps?"
- "What's the simplest version of this problem?"

---

### 2. Pattern Recognition: Finding Similarities

**Definition:** Identify similarities between problems to reuse solutions.

#### Example: Everyday Patterns

**Pattern:** "Getting to a destination"
- Going to work: wake up → get ready → travel → arrive
- Going shopping: plan → travel → shop → return
- Going on vacation: plan → pack → travel → arrive

**Solution:** Once you've solved "traveling to a destination" once, you can adapt it!

#### Example: Programming Patterns

**Pattern:** "Processing a list of items"

```python
# Pattern: Do something with each item in a list

# Count items
count = 0
for item in items:
    count += 1

# Find maximum
max_value = items[0]
for item in items:
    if item > max_value:
        max_value = item

# Sum items
total = 0
for item in items:
    total += item
```

**Recognition:** All three use the same pattern: **iterate through a list and accumulate a result**.

**Generalized solution:**
```python
def process_list(items, initial_value, operation):
    result = initial_value
    for item in items:
        result = operation(result, item)
    return result
```

#### Common Programming Patterns

1. **Loop pattern:** Do something repeatedly
2. **Search pattern:** Find an item matching criteria
3. **Count/accumulate pattern:** Keep a running total or count
4. **Filter pattern:** Select items matching criteria
5. **Transform pattern:** Convert items from one form to another
6. **Divide and conquer pattern:** Break problem in half, solve each half

**Practice:** When solving a problem, ask:
- "Have I solved something similar before?"
- "What patterns do I recognize?"
- "Can I adapt an existing solution?"

---

### 3. Abstraction: Focusing on What Matters

**Definition:** Hide complexity and focus on the essential information.

#### Example: Driving a Car

**Abstraction:** You press the gas pedal, the car goes faster.

**Hidden details:** Engine combustion, transmission gears, fuel injection, etc.

**Why useful:** You don't need to understand engine mechanics to drive!

#### Example: Using a Function

```python
# Using abstraction
result = sorted([3, 1, 4, 1, 5])

# You don't need to know HOW sorted() works
# You only need to know WHAT it does: sorts a list
```

#### Creating Abstractions

**Before abstraction:**
```python
# Calculate area of rectangles repeatedly
area1 = length1 * width1
area2 = length2 * width2
area3 = length3 * width3
```

**After abstraction:**
```python
def calculate_area(length, width):
    return length * width

area1 = calculate_area(5, 3)
area2 = calculate_area(10, 2)
area3 = calculate_area(7, 4)
```

**Benefits:**
- Reusable
- Easier to understand
- Easier to modify (change in one place)
- Hides implementation details

#### Levels of Abstraction

**High-level (abstract):**
```python
send_email(to="user@example.com", subject="Hello", body="Hi there!")
```

**Lower-level (more detail):**
```python
smtp_connection = connect_to_smtp_server()
message = format_email_message(to, subject, body)
smtp_connection.send(message)
smtp_connection.close()
```

**Even lower (implementation details):**
```python
# Opening sockets, formatting MIME, handling protocols...
```

**Practice:** When designing solutions, ask:
- "What's the essential information?"
- "What details can I hide?"
- "Can I describe this at a higher level?"
- "What should the user of this code know vs. what can be internal?"

---

### 4. Algorithm Design: Step-by-Step Solutions

**Definition:** Create clear, ordered instructions to solve a problem.

#### Example: Making Tea

**Algorithm:**
1. Fill kettle with water
2. Turn on kettle
3. Wait for water to boil
4. Put tea bag in cup
5. Pour boiling water into cup
6. Wait 3-5 minutes
7. Remove tea bag
8. Add milk/sugar (optional)
9. Stir
10. Drink

**Key properties:**
- **Clear:** Each step is unambiguous
- **Ordered:** Steps must happen in sequence
- **Finite:** It ends
- **Effective:** It produces the desired result

#### Example: Finding the Largest Number

**Problem:** Find the largest number in a list.

**Algorithm (in plain English):**
1. Start with the first number as the "current largest"
2. For each remaining number:
   - If it's bigger than the "current largest"
   - Make it the new "current largest"
3. Return the "current largest"

**In code:**
```python
def find_max(numbers):
    if not numbers:
        return None

    max_so_far = numbers[0]

    for num in numbers[1:]:
        if num > max_so_far:
            max_so_far = num

    return max_so_far
```

#### Algorithm Design Process

1. **Understand the problem**
   - What are the inputs?
   - What should the output be?
   - What are the constraints?

2. **Design the solution**
   - Write steps in plain language first
   - Don't worry about syntax yet!
   - Think about edge cases

3. **Trace through examples**
   - Walk through your algorithm with sample data
   - Does it work?
   - Does it handle edge cases?

4. **Implement**
   - Translate to code
   - Test with various inputs

5. **Refine**
   - Can it be simpler?
   - Can it be more efficient?
   - Is it readable?

**Practice:** For any problem:
1. Write the algorithm in plain English first
2. Test it mentally with examples
3. Only then write code

---

## Practical Problem-Solving Strategies

### Strategy 1: Start Simple

**Don't solve the full problem immediately!**

#### Example: Building a Calculator

**Don't start with:** "Build a calculator that handles +, -, ×, ÷, parentheses, decimals, and errors"

**Start with:**
1. Build a calculator that adds two numbers
2. Add subtraction
3. Add multiplication and division
4. Add support for more than two numbers
5. Add parentheses support
6. Add error handling
7. Add decimal support

**Why:** Each step is easier, and you can test as you go.

### Strategy 2: Work Backwards

Sometimes it's easier to think from the solution back to the start.

#### Example: Maze Solving

**Forward thinking:** "Where do I go from the start?"
**Backward thinking:** "What positions can reach the end?"

#### Example: Planning a Road Trip

**Forward:** "What should I do first?" (Overwhelming!)
**Backward:**
- "I need to be at the destination by 5pm"
- "So I need to leave by noon"
- "So I need to pack by 11am"
- "So I need to start packing at 9am"

### Strategy 3: Solve by Example

**Work through concrete examples before generalizing.**

#### Example: Reversing a String

**Example:** "hello" → "olleh"

**Trace it:**
- Take from end: 'o'
- Next: 'l'  → "ol"
- Next: 'l'  → "oll"
- Next: 'e'  → "olle"
- Next: 'h'  → "olleh"

**Pattern recognized:** "Take characters from end to start"

**Generalized:**
```python
def reverse_string(s):
    result = ""
    for char in reversed(s):
        result += char
    return result

# Or even simpler:
def reverse_string(s):
    return s[::-1]
```

### Strategy 4: Draw It Out

**Visual representations help understanding.**

- Lists → boxes in a row
- Trees → branching diagrams
- Algorithms → flowcharts
- State → state diagrams

#### Example: Understanding Recursion

Draw the call stack:
```
factorial(4)
  → 4 * factorial(3)
       → 3 * factorial(2)
            → 2 * factorial(1)
                 → 1
            → 2 * 1 = 2
       → 3 * 2 = 6
  → 4 * 6 = 24
```

---

## Thinking About Edge Cases

**Edge cases** are unusual or extreme inputs that might break your solution.

### Common Edge Cases

1. **Empty input**
   - Empty list: `[]`
   - Empty string: `""`
   - Zero: `0`
   - None/null

2. **Single item**
   - List with one element: `[5]`
   - String with one character: `"a"`

3. **Duplicates**
   - All same: `[3, 3, 3, 3]`
   - Some duplicates: `[1, 2, 2, 3]`

4. **Negative numbers**
   - What if input is negative when you expect positive?

5. **Very large numbers**
   - What if the number is too big to fit in memory?

6. **Special characters**
   - Spaces, punctuation, unicode

### Example: Finding Maximum

```python
def find_max(numbers):
    # Edge case: empty list
    if not numbers:
        return None  # or raise error?

    # Edge case: single element
    if len(numbers) == 1:
        return numbers[0]

    # Normal case
    max_so_far = numbers[0]
    for num in numbers[1:]:
        if num > max_so_far:
            max_so_far = num

    return max_so_far
```

**Practice:** For every solution, ask:
- "What if the input is empty?"
- "What if there's only one item?"
- "What if all items are the same?"
- "What if the input is negative/zero/huge?"

---

## The Debugging Mindset

**Debugging is problem-solving!** When your code doesn't work:

### 1. Reproduce the Problem

- Can you make it fail consistently?
- What are the exact steps to trigger it?

### 2. Simplify

- Can you create a minimal example that shows the bug?
- Remove unrelated code

### 3. Form a Hypothesis

- "I think the bug is because..."
- "I expect X to happen, but Y happens instead"

### 4. Test Your Hypothesis

- Add print statements
- Use a debugger
- Check intermediate values

### 5. Fix and Verify

- Make the fix
- Test that it works
- Test that you didn't break anything else

### Debugging Techniques

**Print debugging:**
```python
def mysterious_function(x):
    print(f"Input: {x}")  # What's coming in?
    result = x * 2 + 5
    print(f"Result: {result}")  # What's going out?
    return result
```

**Rubber duck debugging:**
- Explain your code line-by-line to a rubber duck (or friend)
- Often you'll spot the problem while explaining!

**Binary search debugging:**
- Comment out half the code
- Does the bug still occur?
- If yes, bug is in the remaining half
- If no, bug is in the commented half
- Repeat

**Read error messages carefully:**
- What line number?
- What type of error?
- What does the message say?

---

## Exercises

### Exercise 1: Decomposition Practice

**Problem:** Plan a birthday party for 20 people.

**Task:** Break this down into smaller sub-tasks. Go at least 3 levels deep.

<details>
<summary>Example Solution</summary>

1. Planning
   - Set date and time
   - Create guest list
   - Determine budget
2. Invitations
   - Design invitation
   - Send invitations
   - Track RSVPs
3. Venue
   - Choose location
   - Reserve venue
   - Arrange tables/chairs
4. Food and drinks
   - Plan menu
   - Order/prepare food
   - Get drinks
5. Entertainment
   - Plan activities
   - Prepare music
   - Organize games
6. Decorations
   - Buy decorations
   - Set up decorations
7. Day-of coordination
   - Set up venue
   - Receive guests
   - Run activities
   - Clean up
</details>

---

### Exercise 2: Pattern Recognition

**Find the pattern and predict the next items:**

1. Sequence: 2, 4, 6, 8, 10, ??, ??
2. Sequence: 1, 1, 2, 3, 5, 8, ??, ??
3. Sequence: 5, 10, 20, 40, ??, ??

<details>
<summary>Answers</summary>

1. Pattern: Add 2 each time → 12, 14
2. Pattern: Fibonacci (each is sum of previous two) → 13, 21
3. Pattern: Double each time → 80, 160
</details>

**Programming pattern:**

Identify the pattern in these three code snippets:

```python
# Snippet 1
total = 0
for num in numbers:
    total += num

# Snippet 2
product = 1
for num in numbers:
    product *= num

# Snippet 3
result = ""
for char in characters:
    result += char
```

<details>
<summary>Answer</summary>

Pattern: **Accumulator pattern**
- Start with initial value
- Loop through collection
- Update accumulated value with each item
</details>

---

### Exercise 3: Abstraction

**Create a higher-level abstraction for this code:**

```python
# Calculate total price with tax for multiple purchases
subtotal1 = quantity1 * price1
tax1 = subtotal1 * 0.08
total1 = subtotal1 + tax1

subtotal2 = quantity2 * price2
tax2 = subtotal2 * 0.08
total2 = subtotal2 + tax2

subtotal3 = quantity3 * price3
tax3 = subtotal3 * 0.08
total3 = subtotal3 + tax3
```

<details>
<summary>Solution</summary>

```python
def calculate_total_with_tax(quantity, price, tax_rate=0.08):
    subtotal = quantity * price
    tax = subtotal * tax_rate
    total = subtotal + tax
    return total

total1 = calculate_total_with_tax(quantity1, price1)
total2 = calculate_total_with_tax(quantity2, price2)
total3 = calculate_total_with_tax(quantity3, price3)
```
</details>

---

### Exercise 4: Algorithm Design

**Design an algorithm (in plain English) for:**

1. **Making change:** Given an amount owed and amount paid, return the change using fewest coins (quarters, dimes, nickels, pennies).

2. **Finding duplicates:** Given a list of numbers, find which numbers appear more than once.

3. **Palindrome checker:** Determine if a word reads the same forwards and backwards.

<details>
<summary>Example Solutions</summary>

**1. Making change:**
1. Calculate change amount = paid - owed
2. Count quarters: change ÷ 25, subtract quarters from change
3. Count dimes: remaining change ÷ 10, subtract dimes
4. Count nickels: remaining change ÷ 5, subtract nickels
5. Count pennies: remaining change (all remaining)
6. Return counts

**2. Finding duplicates:**
1. Create an empty set of "seen" numbers
2. Create an empty set of "duplicates"
3. For each number in the list:
   - If it's in "seen", add to "duplicates"
   - Otherwise, add to "seen"
4. Return "duplicates"

**3. Palindrome checker:**
1. Reverse the word
2. Compare original with reversed
3. If they're the same, it's a palindrome
4. Return true or false
</details>

---

### Exercise 5: Edge Cases

**For each function, list at least 3 edge cases to test:**

1. `def find_average(numbers):`
2. `def divide(a, b):`
3. `def get_first_word(sentence):`

<details>
<summary>Example Edge Cases</summary>

**1. find_average:**
- Empty list
- Single number
- All zeros
- Negative numbers
- Very large numbers

**2. divide:**
- Division by zero
- Divide zero by something
- Negative numbers
- Very large numbers (overflow?)

**3. get_first_word:**
- Empty string
- String with no spaces (one word)
- String starting with spaces
- String with multiple spaces between words
- String with punctuation
</details>

---

## Summary

### The Four Pillars

1. **Decomposition:** Break big problems into smaller ones
2. **Pattern Recognition:** Find similarities with solved problems
3. **Abstraction:** Hide details, focus on essentials
4. **Algorithm Design:** Create step-by-step solutions

### Problem-Solving Process

1. **Understand** the problem
2. **Decompose** into smaller parts
3. **Look for patterns** you recognize
4. **Design** an algorithm in plain language
5. **Consider edge cases**
6. **Implement** in code
7. **Test** thoroughly
8. **Debug** systematically
9. **Refine** and improve

### Key Mindsets

- **Start simple**, then build complexity
- **Work through examples** before generalizing
- **Draw diagrams** to visualize
- **Think about edge cases** early
- **Don't be afraid to fail** - debugging is learning!

---

## Next Steps

**Apply computational thinking to every problem:**

1. Before writing code, spend time **thinking**
2. Write algorithms in **plain English first**
3. **Draw diagrams** of your solution
4. Consider **edge cases** before implementing
5. **Break down** complex problems systematically

**Remember:** Programming languages are tools. Computational thinking is the **skill** that makes you a great programmer in **any** language!

---

## Additional Resources

- **Book:** "How to Solve It" by George Pólya (problem-solving classic)
- **Video:** "Computational Thinking" - Google CS Education
- **Practice:** Project Euler, LeetCode, HackerRank (apply these concepts!)
- **Practice:** Explain your thinking process out loud when solving problems

**Most importantly:** Practice, practice, practice! Computational thinking improves with every problem you solve.
