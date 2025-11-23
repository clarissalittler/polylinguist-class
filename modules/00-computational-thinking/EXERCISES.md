# Computational Thinking: Practice Exercises

These exercises help you develop problem-solving skills **before** you write code. Focus on the thinking process, not programming syntax.

---

## Part 1: Decomposition Exercises

### Exercise 1.1: Planning a Road Trip

**Problem:** You need to drive from San Francisco to New York (3,000 miles).

**Task:** Decompose this into a hierarchical list of tasks. Include at least 3 levels of detail.

**Format:**
```
1. Main Task
   1.1. Subtask
       1.1.1. Sub-subtask
       1.1.2. Sub-subtask
   1.2. Subtask
```

---

### Exercise 1.2: Building a Simple Website

**Problem:** Create a personal portfolio website.

**Task:** Break down this project into:
1. Major phases
2. Tasks within each phase
3. Specific actions for each task

Think about: Planning, Design, Development, Testing, Deployment

---

### Exercise 1.3: Organizing a Study Group

**Problem:** Organize a weekly study group for 10 people over a semester.

**Task:** Decompose into manageable tasks. Consider:
- Initial setup
- Recurring weekly tasks
- Communication
- Resource management

---

## Part 2: Pattern Recognition Exercises

### Exercise 2.1: Number Sequences

Find the pattern and predict the next 2-3 numbers:

1. 3, 6, 9, 12, 15, ?, ?
2. 1, 4, 9, 16, 25, ?, ?
3. 2, 6, 18, 54, ?, ?
4. 1, 1, 2, 3, 5, 8, 13, ?, ?
5. 100, 50, 25, 12.5, ?, ?

---

### Exercise 2.2: Code Pattern Recognition

**Identify the common pattern in these code snippets:**

```python
# Snippet A
largest = numbers[0]
for num in numbers:
    if num > largest:
        largest = num

# Snippet B
smallest = numbers[0]
for num in numbers:
    if num < smallest:
        smallest = num

# Snippet C
first_even = None
for num in numbers:
    if num % 2 == 0:
        first_even = num
        break
```

**Questions:**
1. What pattern do they all share?
2. How could you generalize this pattern?
3. Can you think of other problems that follow this pattern?

---

### Exercise 2.3: Real-World Patterns

**Identify similarities between these activities:**

1. Cooking a recipe
2. Following assembly instructions
3. Getting directions from a GPS
4. Following a morning routine

**Questions:**
1. What do they have in common?
2. How does recognizing this pattern help in programming?
3. What programming concept does this relate to?

---

## Part 3: Abstraction Exercises

### Exercise 3.1: Levels of Abstraction

**For each task, describe it at three levels of abstraction:**
- High-level (what it does, no implementation details)
- Medium-level (main steps)
- Low-level (detailed implementation)

**Example Task:** Sending an email

**High-level:** "Send an email to John about the meeting"

**Medium-level:**
1. Connect to email server
2. Compose message
3. Send message
4. Confirm sent

**Low-level:**
1. Open SMTP connection to server
2. Authenticate with credentials
3. Format message headers (To, From, Subject)
4. Encode message body
5. Transmit via SMTP protocol
6. Wait for server acknowledgment
7. Close connection

**Your turn - describe these at three levels:**

1. Withdrawing money from an ATM
2. Ordering food at a restaurant
3. Searching for a book in a library

---

### Exercise 3.2: Finding Unnecessary Details

**Which details are relevant and which can be abstracted away?**

**Scenario:** You're writing a program to calculate the final price of items in a shopping cart.

**Information provided:**
- Item name
- Item color
- Item price
- Item weight
- Tax rate
- Shipping address
- User's age
- Day of the week
- User's favorite color

**Questions:**
1. Which pieces of information are essential for calculating final price?
2. Which can be ignored/abstracted away?
3. In what situations might the "unnecessary" details become relevant?

---

### Exercise 3.3: Create an Abstraction

**Repetitive code:**

```python
# Calculate area of room 1
room1_area = room1_length * room1_width
room1_paint_needed = room1_area / 10  # 1 gallon per 10 sq ft
room1_cost = room1_paint_needed * 30  # $30 per gallon

# Calculate area of room 2
room2_area = room2_length * room2_width
room2_paint_needed = room2_area / 10
room2_cost = room2_paint_needed * 30

# Calculate area of room 3
room3_area = room3_length * room3_width
room3_paint_needed = room3_area / 10
room3_cost = room3_paint_needed * 30
```

**Task:** Create an abstraction (describe it in plain English) that eliminates the repetition.

---

## Part 4: Algorithm Design Exercises

### Exercise 4.1: Write Algorithms in Plain English

**Design step-by-step algorithms for these tasks:**

1. **Finding a book in a library**
   - Assume books are organized by call number
   - Write clear, numbered steps

2. **Making scrambled eggs**
   - Be specific enough that someone who's never cooked could follow
   - Include all necessary steps

3. **Finding the shortest route between two cities**
   - What information do you need?
   - What steps would you take?

**Requirements for each:**
- Clear, numbered steps
- Unambiguous instructions
- Should work every time
- Should end (finite)

---

### Exercise 4.2: Algorithm Comparison

**Problem:** Find if a number exists in a list.

**Two algorithms:**

**Algorithm A:**
1. Start at the beginning of the list
2. Check each number one by one
3. If you find the target, return "Found"
4. If you reach the end without finding it, return "Not found"

**Algorithm B:**
1. Sort the list first
2. Look at the middle number
3. If it's the target, return "Found"
4. If target is smaller, search the left half
5. If target is larger, search the right half
6. Repeat until found or no numbers left

**Questions:**
1. Which algorithm is simpler?
2. Which is faster for a large list?
3. What's the tradeoff?
4. In what situations would you choose each?

---

### Exercise 4.3: Design Your Own Algorithm

**Problem:** You have a shuffled deck of 52 playing cards. Design an algorithm to sort them by suit (clubs, diamonds, hearts, spades) and rank (A, 2, 3, ..., K) within each suit.

**Task:** Write a step-by-step algorithm in plain English.

**Consider:**
- How will you organize them?
- Will you make multiple passes?
- Where will you place cards as you sort?

---

## Part 5: Edge Cases and Error Handling

### Exercise 5.1: Identify Edge Cases

**For each scenario, list at least 5 edge cases to consider:**

1. **A function that divides two numbers**
   - What could go wrong?
   - What unusual inputs might break it?

2. **A program that finds the oldest person in a group**
   - What if the group is empty?
   - What other edge cases exist?

3. **A function that reverses a string**
   - Think beyond the typical "hello" → "olleh"

---

### Exercise 5.2: Handle the Unexpected

**For each scenario, describe what should happen:**

1. **Scenario:** User enters "five" when asked for a number
   - What should the program do?
   - How should it respond?

2. **Scenario:** User selects item #10 from a menu with only 5 items
   - What should happen?

3. **Scenario:** Program runs out of memory while processing a file
   - How should it handle this?

---

## Part 6: Problem-Solving Scenarios

### Exercise 6.1: The Broken Elevator

**Problem:** You're on the 10th floor. The elevator is broken. You need to get a heavy box to the ground floor. The stairs are an option, but the box is very heavy.

**Task:**
1. Decompose this problem
2. Identify patterns (similar problems you've solved)
3. Design multiple solutions
4. Choose the best solution and explain why

---

### Exercise 6.2: The Efficient Librarian

**Problem:** You're a librarian. Students constantly ask you where to find books. You want to minimize the time spent answering the same questions.

**Task:**
1. What patterns do you notice in the questions?
2. What solutions can you abstract from those patterns?
3. Design a system to solve this efficiently

---

### Exercise 6.3: The Lost Keys

**Problem:** You lost your keys somewhere in your house. You need to find them quickly.

**Task:**
1. Design a search algorithm
2. What strategies would be most efficient?
3. How does this relate to search algorithms in programming?

---

## Part 7: Debugging Mindset Exercises

### Exercise 7.1: Debug the Algorithm

**Algorithm:** "Make a cup of coffee"

**Steps given:**
1. Turn on coffee maker
2. Add water to reservoir
3. Drink coffee

**Problems:** The algorithm doesn't work!

**Task:**
1. Identify what's missing
2. Identify what's in the wrong order
3. Write a corrected algorithm

---

### Exercise 7.2: Find the Flaw

**Algorithm:** "Find the largest number in a list"

```
1. Set max to 0
2. For each number in the list:
   3. If the number is greater than max:
      4. Set max to that number
5. Return max
```

**Task:**
1. Trace through this algorithm with the list: [-5, -3, -10, -1]
2. What goes wrong?
3. How would you fix it?

---

### Exercise 7.3: Hypothesize and Test

**Scenario:** Your friend's recipe for cookies always burns them, but your recipe works fine. The recipes are identical except:
- Your friend's oven is older
- Your friend uses a metal pan, you use glass
- Your friend bakes at 350°F, you bake at 325°F

**Task:**
1. Form a hypothesis about why the cookies burn
2. Design an experiment to test your hypothesis
3. Explain how this relates to debugging code

---

## Part 8: Synthesis Exercises

### Exercise 8.1: The Complete Process

**Problem:** Design a simple phone contact manager

**Requirements:**
- Store contacts (name and phone number)
- Add new contacts
- Search for contacts by name
- Delete contacts

**Task:** Apply ALL four pillars of computational thinking:

1. **Decompose** the problem into components
2. **Recognize patterns** from similar problems
3. **Create abstractions** where appropriate
4. **Design the algorithm** for each operation

Present your solution as a structured document.

---

### Exercise 8.2: Compare Approaches

**Problem:** Sort a hand of playing cards

**Task:**
1. Design at least TWO different algorithms for sorting cards
2. Compare their approaches:
   - Which is simpler to understand?
   - Which would be faster?
   - Which is easier to execute manually?
3. Explain when you'd use each approach

---

### Exercise 8.3: Real-World Application

**Choose one of these real-world problems and apply computational thinking:**

1. Planning a wedding
2. Organizing a charity fundraiser
3. Learning a new language
4. Training for a marathon

**For your chosen problem:**
1. Decompose it into major components
2. Identify patterns from similar endeavors
3. Abstract away unnecessary details
4. Design a step-by-step plan (algorithm)
5. Identify potential problems (edge cases)
6. Create a monitoring/debugging strategy

---

## Part 9: Reflection Questions

After completing these exercises, reflect on these questions:

1. **Decomposition:** How did breaking problems down help you understand them better?

2. **Patterns:** Did you notice yourself recognizing patterns more easily as you progressed?

3. **Abstraction:** When is abstraction helpful? When might it hide too much detail?

4. **Algorithms:** What makes an algorithm "good"? Clear? Efficient? Simple?

5. **Edge Cases:** Why is it important to think about edge cases before implementing?

6. **Application:** How will you apply computational thinking to your next programming challenge?

---

## Answer Key (Selected Exercises)

### Part 2.1: Number Sequences

1. Add 3 each time → 18, 21
2. Perfect squares (1², 2², 3², ...) → 36, 49
3. Multiply by 3 each time → 162, 486
4. Fibonacci (sum of previous two) → 21, 34
5. Divide by 2 each time → 6.25, 3.125

### Part 2.2: Code Pattern

**Pattern:** "Find-first" or "Linear search with a condition"
- Iterate through a collection
- Check each item against a condition
- Keep/return the item that matches

### Part 7.2: Find the Flaw

**Problem:** Setting max to 0 fails when all numbers are negative!

**Fix:**
```
1. If list is empty, return None
2. Set max to the FIRST number in the list
3. For each remaining number:
   4. If the number is greater than max:
      5. Set max to that number
6. Return max
```

---

## Next Steps

These exercises develop the **thinking skills** that make programming easier. Keep practicing:

1. Before writing code, think through the problem
2. Draw diagrams and write algorithms in plain English
3. Consider edge cases early
4. Practice explaining your thinking to others

**Remember:** Computational thinking is a skill that improves with deliberate practice!
