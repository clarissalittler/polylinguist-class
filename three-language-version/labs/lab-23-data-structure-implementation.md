# Lab 23: Data Structure Implementation

**Quarter 3, Week 3**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Stacks and queues are fundamental data structures used everywhere from browser history to print queues. This lab implements them from scratch to understand how they work internally.

## Objectives

By the end of this lab, you will:
- [ ] Implement a stack (LIFO)
- [ ] Implement a queue (FIFO)
- [ ] Understand linked vs array-based implementations
- [ ] Know when to use each data structure

## Setup

- Partner up
- Create folder: `lab23-data-structures/`
- Files: `stack.py`, `queue.py`, `structures.hs`

---

## Part 1: Stack - Last In, First Out (30 minutes)

### Activity 1.1: Stack Concept

A stack is like a stack of plates:
- You can only add to the top (push)
- You can only remove from the top (pop)
- You can only see the top plate (peek)

```
Push 1, Push 2, Push 3:

    +---+
    | 3 |  <- top
    +---+
    | 2 |
    +---+
    | 1 |
    +---+

Pop returns 3, leaving:

    +---+
    | 2 |  <- top
    +---+
    | 1 |
    +---+
```

**Real-world uses:**
- Undo/Redo functionality
- Browser back button
- Function call stack
- Expression evaluation

### Activity 1.2: Array-Based Stack (Python)

```python
class Stack:
    """Stack implementation using a Python list."""

    def __init__(self):
        self._items = []

    def push(self, item):
        """Add item to top of stack."""
        self._items.append(item)

    def pop(self):
        """Remove and return top item."""
        if self.is_empty():
            raise IndexError("Pop from empty stack")
        return self._items.pop()

    def peek(self):
        """Return top item without removing it."""
        if self.is_empty():
            raise IndexError("Peek at empty stack")
        return self._items[-1]

    def is_empty(self):
        """Check if stack is empty."""
        return len(self._items) == 0

    def size(self):
        """Return number of items in stack."""
        return len(self._items)

    def __repr__(self):
        return f"Stack({self._items})"

# Test it
stack = Stack()
stack.push(1)
stack.push(2)
stack.push(3)
print(stack)           # Stack([1, 2, 3])
print(stack.pop())     # 3
print(stack.peek())    # 2
print(stack.size())    # 2
```

### Activity 1.3: Linked Stack

A stack can also be built with linked nodes:

```python
class Node:
    """A node in the linked stack."""
    def __init__(self, value, next_node=None):
        self.value = value
        self.next = next_node

class LinkedStack:
    """Stack implementation using linked nodes."""

    def __init__(self):
        self._top = None
        self._size = 0

    def push(self, item):
        """Add item to top."""
        new_node = Node(item, self._top)
        self._top = new_node
        self._size += 1

    def pop(self):
        """Remove and return top item."""
        if self.is_empty():
            raise IndexError("Pop from empty stack")
        value = self._top.value
        self._top = self._top.next
        self._size -= 1
        return value

    def peek(self):
        """Return top item without removing."""
        if self.is_empty():
            raise IndexError("Peek at empty stack")
        return self._top.value

    def is_empty(self):
        return self._top is None

    def size(self):
        return self._size
```

### Activity 1.4: Stack in Haskell

```haskell
-- Stack as a list (top is the head)
type Stack a = [a]

empty :: Stack a
empty = []

push :: a -> Stack a -> Stack a
push x s = x : s

pop :: Stack a -> (a, Stack a)
pop []     = error "Pop from empty stack"
pop (x:xs) = (x, xs)

peek :: Stack a -> a
peek []    = error "Peek at empty stack"
peek (x:_) = x

isEmpty :: Stack a -> Bool
isEmpty = null

main :: IO ()
main = do
    let s0 = empty
    let s1 = push 1 s0
    let s2 = push 2 s1
    let s3 = push 3 s2
    print s3             -- [3,2,1]
    print (peek s3)      -- 3
    let (v, s4) = pop s3
    print v              -- 3
    print s4             -- [2,1]
```

### ✅ Checkpoint 1

Verify:
- [ ] Array-based stack works
- [ ] Linked stack works
- [ ] Understand LIFO behavior

---

## Part 2: Queue - First In, First Out (30 minutes)

### Activity 2.1: Queue Concept

A queue is like a line at a store:
- People join at the back (enqueue)
- People leave from the front (dequeue)
- First person in line is served first

```
Enqueue 1, Enqueue 2, Enqueue 3:

Front -> [1] - [2] - [3] <- Back

Dequeue returns 1, leaving:

Front -> [2] - [3] <- Back
```

**Real-world uses:**
- Print job queue
- Task scheduling
- Message queues
- Breadth-first search

### Activity 2.2: Array-Based Queue

```python
class Queue:
    """Queue implementation using a Python list."""

    def __init__(self):
        self._items = []

    def enqueue(self, item):
        """Add item to back of queue."""
        self._items.append(item)

    def dequeue(self):
        """Remove and return front item."""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        return self._items.pop(0)  # Note: O(n) operation!

    def front(self):
        """Return front item without removing it."""
        if self.is_empty():
            raise IndexError("Front of empty queue")
        return self._items[0]

    def is_empty(self):
        return len(self._items) == 0

    def size(self):
        return len(self._items)

    def __repr__(self):
        return f"Queue({self._items})"

# Test it
queue = Queue()
queue.enqueue(1)
queue.enqueue(2)
queue.enqueue(3)
print(queue)            # Queue([1, 2, 3])
print(queue.dequeue())  # 1
print(queue.front())    # 2
```

### Activity 2.3: Efficient Queue with Deque

```python
from collections import deque

class EfficientQueue:
    """Queue using deque for O(1) operations."""

    def __init__(self):
        self._items = deque()

    def enqueue(self, item):
        self._items.append(item)  # O(1)

    def dequeue(self):
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        return self._items.popleft()  # O(1)!

    def front(self):
        if self.is_empty():
            raise IndexError("Front of empty queue")
        return self._items[0]

    def is_empty(self):
        return len(self._items) == 0

    def size(self):
        return len(self._items)
```

### Activity 2.4: Linked Queue

```python
class Node:
    def __init__(self, value, next_node=None):
        self.value = value
        self.next = next_node

class LinkedQueue:
    """Queue implementation using linked nodes."""

    def __init__(self):
        self._front = None
        self._back = None
        self._size = 0

    def enqueue(self, item):
        """Add item to back."""
        new_node = Node(item)
        if self.is_empty():
            self._front = new_node
            self._back = new_node
        else:
            self._back.next = new_node
            self._back = new_node
        self._size += 1

    def dequeue(self):
        """Remove and return front item."""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        value = self._front.value
        self._front = self._front.next
        if self._front is None:
            self._back = None
        self._size -= 1
        return value

    def front(self):
        if self.is_empty():
            raise IndexError("Front of empty queue")
        return self._front.value

    def is_empty(self):
        return self._front is None

    def size(self):
        return self._size
```

### Activity 2.5: Queue in Haskell

```haskell
-- Efficient functional queue using two lists
data Queue a = Queue [a] [a]
    deriving Show

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front back) = Queue front (x : back)

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] [])     = error "Dequeue from empty queue"
dequeue (Queue [] back)   = dequeue (Queue (reverse back) [])
dequeue (Queue (x:xs) back) = (x, Queue xs back)

front :: Queue a -> a
front (Queue [] [])     = error "Front of empty queue"
front (Queue [] back)   = head (reverse back)
front (Queue (x:_) _)   = x

isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue [] []) = True
isEmptyQueue _             = False

main :: IO ()
main = do
    let q0 = emptyQueue
    let q1 = enqueue 1 q0
    let q2 = enqueue 2 q1
    let q3 = enqueue 3 q2
    print q3                    -- Queue [] [3,2,1]
    let (v, q4) = dequeue q3
    print v                     -- 1
    print q4                    -- Queue [2,1] [3] (after reverse)
```

### ✅ Checkpoint 2

Verify:
- [ ] Simple queue works
- [ ] Understand FIFO behavior
- [ ] Know why deque is more efficient

---

## Part 3: Applications (20 minutes)

### Activity 3.1: Balanced Parentheses

Use a stack to check if parentheses are balanced:

```python
def is_balanced(text):
    """Check if parentheses/brackets are balanced."""
    stack = Stack()
    matching = {')': '(', ']': '[', '}': '{'}

    for char in text:
        if char in '([{':
            stack.push(char)
        elif char in ')]}':
            if stack.is_empty():
                return False
            if stack.pop() != matching[char]:
                return False

    return stack.is_empty()

# Test
print(is_balanced("()"))         # True
print(is_balanced("([])"))       # True
print(is_balanced("([)]"))       # False
print(is_balanced("((())"))      # False
print(is_balanced("{[()]}"))     # True
```

### Activity 3.2: Reverse Polish Notation Calculator

Use a stack to evaluate expressions like `3 4 + 5 *` (= (3+4)*5 = 35):

```python
def evaluate_rpn(expression):
    """Evaluate Reverse Polish Notation expression."""
    stack = Stack()
    tokens = expression.split()

    for token in tokens:
        if token in '+-*/':
            b = stack.pop()  # Second operand
            a = stack.pop()  # First operand
            if token == '+':
                stack.push(a + b)
            elif token == '-':
                stack.push(a - b)
            elif token == '*':
                stack.push(a * b)
            elif token == '/':
                stack.push(a / b)
        else:
            stack.push(float(token))

    return stack.pop()

# Test
print(evaluate_rpn("3 4 +"))        # 7.0
print(evaluate_rpn("3 4 + 5 *"))    # 35.0
print(evaluate_rpn("5 1 2 + 4 * + 3 -"))  # 14.0
```

### Activity 3.3: Print Queue Simulation

```python
import random

def simulate_print_queue(num_jobs, processing_time):
    """Simulate a print queue."""
    queue = Queue()

    # Add some jobs
    for i in range(num_jobs):
        job = f"Job {i+1}"
        queue.enqueue(job)
        print(f"Added: {job}")

    # Process jobs
    while not queue.is_empty():
        job = queue.dequeue()
        print(f"Printing: {job}...")
        # In real life, would wait here
        print(f"Completed: {job}")

simulate_print_queue(5, 2)
```

### Activity 3.4: Your Turn - Hot Potato

Implement the "Hot Potato" game using a queue:

```python
def hot_potato(names, num_passes):
    """
    Players pass a 'potato' around. After num_passes,
    the person holding it is eliminated.
    Continue until one person remains.
    """
    queue = Queue()
    for name in names:
        queue.enqueue(name)

    while queue.size() > 1:
        # Pass the potato num_passes times
        for _ in range(num_passes):
            # Move front person to back
            queue.enqueue(queue.dequeue())

        # Person holding potato is out
        eliminated = queue.dequeue()
        print(f"{eliminated} is eliminated!")

    return queue.dequeue()

# Test
winner = hot_potato(["Alice", "Bob", "Charlie", "Diana", "Eve"], 7)
print(f"Winner: {winner}")
```

### ✅ Checkpoint 3

Verify:
- [ ] Balanced parentheses checker works
- [ ] RPN calculator works
- [ ] Hot potato implemented

---

## Part 4: Complexity Analysis (10 minutes)

### Activity 4.1: Stack Complexity

| Operation | Array-Based | Linked |
|-----------|-------------|--------|
| push      | O(1)*       | O(1)   |
| pop       | O(1)        | O(1)   |
| peek      | O(1)        | O(1)   |
| isEmpty   | O(1)        | O(1)   |

*Amortized - occasionally O(n) when array resizes

### Activity 4.2: Queue Complexity

| Operation | List (naive) | Deque | Linked |
|-----------|--------------|-------|--------|
| enqueue   | O(1)         | O(1)  | O(1)   |
| dequeue   | O(n)         | O(1)  | O(1)   |
| front     | O(1)         | O(1)  | O(1)   |
| isEmpty   | O(1)         | O(1)  | O(1)   |

**Key insight:** Naive list queue is O(n) for dequeue because removing from front requires shifting all elements!

---

## Challenges

### Challenge 1: Min Stack

Create a stack that supports O(1) min() operation:

```python
class MinStack:
    def push(self, x): ...
    def pop(self): ...
    def peek(self): ...
    def get_min(self): ...  # Return minimum element in O(1)
```

Hint: Use an auxiliary stack to track minimums.

### Challenge 2: Queue Using Two Stacks

Implement a queue using only stack operations:

```python
class QueueFromStacks:
    def __init__(self):
        self.stack1 = Stack()
        self.stack2 = Stack()

    def enqueue(self, x): ...
    def dequeue(self): ...
```

### Challenge 3: Circular Queue

Implement a circular/ring buffer queue with fixed capacity.

---

## Wrap-Up

**Key takeaways:**

1. **Stack** = LIFO (Last In, First Out)
2. **Queue** = FIFO (First In, First Out)
3. **Implementation matters** - naive list queue is O(n) for dequeue
4. **Both are building blocks** for more complex algorithms

**When to use what:**
- Stack: undo/redo, parsing, DFS
- Queue: scheduling, BFS, buffering

**Next lab:** Tree Building - hierarchical data structures!
