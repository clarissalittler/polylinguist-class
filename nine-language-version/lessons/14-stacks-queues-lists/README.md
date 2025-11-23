# Lesson 14: Stacks, Queues, and Linked Lists

## Learning Objectives

By the end of this lesson, you will be able to:

1. Understand and implement stacks (LIFO data structure)
2. Understand and implement queues (FIFO data structure)
3. Understand and implement linked lists
4. Choose appropriate data structures for different problems
5. Analyze time complexity of operations
6. Recognize when to use each data structure
7. Implement these structures in multiple programming paradigms

## Why These Data Structures Matter

Arrays are great, but they have limitations:
- **Fixed size** (in some languages)
- **Expensive insertion/deletion** at the beginning: O(n)
- **Wasted space** if not full

**Stacks, Queues, and Linked Lists solve specific problems:**
- **Stacks:** Function call stack, undo/redo, backtracking, expression evaluation
- **Queues:** Task scheduling, breadth-first search, print queues, message queues
- **Linked Lists:** Dynamic size, efficient insertion/deletion, foundation for other structures

---

## Part 1: Stacks

### What is a Stack?

**Stack** = **LIFO** (Last-In-First-Out) data structure

Think of a **stack of plates**:
- Add a plate on top (push)
- Remove the top plate (pop)
- You can only access the top plate!

```
    [3]  ← top (most recent)
    [2]
    [1]  ← bottom
```

### Stack Operations

| Operation | Description | Time Complexity |
|-----------|-------------|-----------------|
| **push(item)** | Add item to top | O(1) |
| **pop()** | Remove and return top item | O(1) |
| **peek()** / **top()** | View top item without removing | O(1) |
| **is_empty()** | Check if stack is empty | O(1) |
| **size()** | Get number of items | O(1) |

### Stack Implementation (Array-Based)

#### Python

```python
class Stack:
    def __init__(self):
        self.items = []

    def push(self, item):
        """Add item to top of stack"""
        self.items.append(item)

    def pop(self):
        """Remove and return top item"""
        if self.is_empty():
            raise IndexError("pop from empty stack")
        return self.items.pop()

    def peek(self):
        """Return top item without removing"""
        if self.is_empty():
            raise IndexError("peek from empty stack")
        return self.items[-1]

    def is_empty(self):
        """Check if stack is empty"""
        return len(self.items) == 0

    def size(self):
        """Return number of items"""
        return len(self.items)

    def __str__(self):
        return f"Stack({self.items})"

# Usage
stack = Stack()
stack.push(1)
stack.push(2)
stack.push(3)
print(stack.pop())     # 3 (last in, first out)
print(stack.peek())    # 2
print(stack.size())    # 2
```

#### Java

```java
public class Stack<T> {
    private ArrayList<T> items;

    public Stack() {
        items = new ArrayList<>();
    }

    public void push(T item) {
        items.add(item);
    }

    public T pop() {
        if (isEmpty()) {
            throw new EmptyStackException();
        }
        return items.remove(items.size() - 1);
    }

    public T peek() {
        if (isEmpty()) {
            throw new EmptyStackException();
        }
        return items.get(items.size() - 1);
    }

    public boolean isEmpty() {
        return items.isEmpty();
    }

    public int size() {
        return items.size();
    }
}
```

### Stack Applications

#### 1. Balanced Parentheses Checker

**Problem:** Check if parentheses are balanced: `{[()]}` ✓, `{[(])}` ✗

**Algorithm:**
1. For each character:
   - Opening bracket → push to stack
   - Closing bracket → pop and check if it matches
2. At end, stack should be empty

```python
def is_balanced(expression):
    """Check if parentheses/brackets are balanced"""
    stack = []
    pairs = {'(': ')', '[': ']', '{': '}'}

    for char in expression:
        if char in pairs:  # Opening bracket
            stack.append(char)
        elif char in pairs.values():  # Closing bracket
            if not stack:
                return False  # Closing without opening
            if pairs[stack.pop()] != char:
                return False  # Mismatch

    return len(stack) == 0  # Stack should be empty

# Tests
print(is_balanced("()"))        # True
print(is_balanced("()[]{}"))    # True
print(is_balanced("([{}])"))    # True
print(is_balanced("(]"))        # False
print(is_balanced("(("))        # False
```

#### 2. Reverse a String

```python
def reverse_string(s):
    """Reverse string using stack"""
    stack = []

    # Push all characters
    for char in s:
        stack.append(char)

    # Pop all characters
    reversed_str = ""
    while stack:
        reversed_str += stack.pop()

    return reversed_str

print(reverse_string("hello"))  # "olleh"
```

#### 3. Undo/Redo Functionality

```python
class TextEditor:
    def __init__(self):
        self.text = ""
        self.undo_stack = []
        self.redo_stack = []

    def write(self, text):
        """Add text and save state for undo"""
        self.undo_stack.append(self.text)
        self.text += text
        self.redo_stack.clear()  # Clear redo history

    def undo(self):
        """Undo last write"""
        if self.undo_stack:
            self.redo_stack.append(self.text)
            self.text = self.undo_stack.pop()

    def redo(self):
        """Redo last undo"""
        if self.redo_stack:
            self.undo_stack.append(self.text)
            self.text = self.redo_stack.pop()

# Usage
editor = TextEditor()
editor.write("Hello")
editor.write(" World")
print(editor.text)  # "Hello World"
editor.undo()
print(editor.text)  # "Hello"
editor.redo()
print(editor.text)  # "Hello World"
```

---

## Part 2: Queues

### What is a Queue?

**Queue** = **FIFO** (First-In-First-Out) data structure

Think of a **line at a store**:
- People join at the back (enqueue)
- People leave from the front (dequeue)
- First person in line is served first!

```
Front [1] [2] [3] [4] ← Back
      ↑           ↑
    dequeue    enqueue
```

### Queue Operations

| Operation | Description | Time Complexity |
|-----------|-------------|-----------------|
| **enqueue(item)** | Add item to back | O(1) |
| **dequeue()** | Remove and return front item | O(1) |
| **front()** / **peek()** | View front item without removing | O(1) |
| **is_empty()** | Check if queue is empty | O(1) |
| **size()** | Get number of items | O(1) |

### Queue Implementation (Using Python List)

```python
from collections import deque  # More efficient than list for queue

class Queue:
    def __init__(self):
        self.items = deque()

    def enqueue(self, item):
        """Add item to back of queue"""
        self.items.append(item)

    def dequeue(self):
        """Remove and return front item"""
        if self.is_empty():
            raise IndexError("dequeue from empty queue")
        return self.items.popleft()

    def front(self):
        """Return front item without removing"""
        if self.is_empty():
            raise IndexError("front from empty queue")
        return self.items[0]

    def is_empty(self):
        """Check if queue is empty"""
        return len(self.items) == 0

    def size(self):
        """Return number of items"""
        return len(self.items)

    def __str__(self):
        return f"Queue({list(self.items)})"

# Usage
queue = Queue()
queue.enqueue(1)
queue.enqueue(2)
queue.enqueue(3)
print(queue.dequeue())  # 1 (first in, first out)
print(queue.front())     # 2
print(queue.size())      # 2
```

**Note:** Using `collections.deque` is more efficient than a list because `popleft()` is O(1), while `list.pop(0)` is O(n).

### Queue Applications

#### 1. Breadth-First Search (BFS)

```python
def bfs(graph, start):
    """Breadth-first search using queue"""
    visited = set()
    queue = Queue()
    queue.enqueue(start)
    visited.add(start)

    while not queue.is_empty():
        node = queue.dequeue()
        print(node, end=' ')  # Process node

        for neighbor in graph[node]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.enqueue(neighbor)

# Example graph
graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['B'],
    'E': ['B', 'F'],
    'F': ['C', 'E']
}

bfs(graph, 'A')  # A B C D E F
```

#### 2. Task Scheduler

```python
class TaskScheduler:
    def __init__(self):
        self.queue = Queue()

    def add_task(self, task):
        """Add task to queue"""
        self.queue.enqueue(task)
        print(f"Added task: {task}")

    def process_next(self):
        """Process next task in queue"""
        if self.queue.is_empty():
            print("No tasks to process")
            return None

        task = self.queue.dequeue()
        print(f"Processing task: {task}")
        return task

    def pending_tasks(self):
        """Return number of pending tasks"""
        return self.queue.size()

# Usage
scheduler = TaskScheduler()
scheduler.add_task("Send email")
scheduler.add_task("Generate report")
scheduler.add_task("Backup database")

while not scheduler.queue.is_empty():
    scheduler.process_next()
```

#### 3. Print Queue

```python
class PrintQueue:
    def __init__(self):
        self.queue = Queue()

    def add_document(self, doc):
        """Add document to print queue"""
        self.queue.enqueue(doc)
        print(f"Document '{doc}' added to queue")

    def print_next(self):
        """Print next document"""
        if self.queue.is_empty():
            print("Queue is empty")
            return

        doc = self.queue.dequeue()
        print(f"Printing: {doc}")

    def documents_waiting(self):
        """Number of documents in queue"""
        return self.queue.size()

# Usage
printer = PrintQueue()
printer.add_document("Report.pdf")
printer.add_document("Invoice.pdf")
printer.add_document("Memo.pdf")

while not printer.queue.is_empty():
    printer.print_next()
```

---

## Part 3: Linked Lists

### What is a Linked List?

**Linked List** = Linear data structure where elements are stored in **nodes**

Each node contains:
- **Data**: The value
- **Next pointer**: Reference to next node

```
Head → [1|·] → [2|·] → [3|·] → None
       data next
```

**Comparison with Arrays:**

| Feature | Array | Linked List |
|---------|-------|-------------|
| **Access time** | O(1) | O(n) |
| **Insert at beginning** | O(n) | O(1) |
| **Insert at end** | O(1)* | O(n) or O(1)** |
| **Insert in middle** | O(n) | O(n) |
| **Delete** | O(n) | O(1) if you have pointer |
| **Memory** | Contiguous | Scattered |
| **Size** | Fixed* | Dynamic |

\* Amortized for dynamic arrays
** O(1) if you maintain a tail pointer

### Singly Linked List Implementation

#### Node Class

```python
class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

    def __str__(self):
        return str(self.data)
```

#### LinkedList Class

```python
class LinkedList:
    def __init__(self):
        self.head = None

    def is_empty(self):
        """Check if list is empty"""
        return self.head is None

    def prepend(self, data):
        """Add node at beginning - O(1)"""
        new_node = Node(data)
        new_node.next = self.head
        self.head = new_node

    def append(self, data):
        """Add node at end - O(n)"""
        new_node = Node(data)

        if self.is_empty():
            self.head = new_node
            return

        # Traverse to end
        current = self.head
        while current.next:
            current = current.next

        current.next = new_node

    def insert_after(self, prev_node, data):
        """Insert node after given node - O(1)"""
        if prev_node is None:
            raise ValueError("Previous node cannot be None")

        new_node = Node(data)
        new_node.next = prev_node.next
        prev_node.next = new_node

    def delete(self, key):
        """Delete first node with given key - O(n)"""
        current = self.head

        # If head node holds the key
        if current and current.data == key:
            self.head = current.next
            return

        # Search for key
        prev = None
        while current and current.data != key:
            prev = current
            current = current.next

        # Key not found
        if current is None:
            return

        # Unlink node
        prev.next = current.next

    def search(self, key):
        """Search for node with given key - O(n)"""
        current = self.head

        while current:
            if current.data == key:
                return current
            current = current.next

        return None

    def size(self):
        """Count nodes - O(n)"""
        count = 0
        current = self.head

        while current:
            count += 1
            current = current.next

        return count

    def print_list(self):
        """Print all elements"""
        current = self.head
        elements = []

        while current:
            elements.append(str(current.data))
            current = current.next

        print(" → ".join(elements) + " → None")

    def __str__(self):
        elements = []
        current = self.head
        while current:
            elements.append(str(current.data))
            current = current.next
        return " → ".join(elements)
```

#### Usage

```python
# Create linked list
llist = LinkedList()

# Add elements
llist.append(1)
llist.append(2)
llist.append(3)
llist.print_list()  # 1 → 2 → 3 → None

# Prepend
llist.prepend(0)
llist.print_list()  # 0 → 1 → 2 → 3 → None

# Delete
llist.delete(2)
llist.print_list()  # 0 → 1 → 3 → None

# Search
node = llist.search(1)
print(f"Found: {node.data}")  # Found: 1

# Size
print(f"Size: {llist.size()}")  # Size: 3
```

### Doubly Linked List

**Doubly Linked List** = Each node has pointers to both next AND previous nodes

```
None ← [1|·] ↔ [2|·] ↔ [3|·] → None
       prev data next
```

**Advantages:**
- Can traverse in both directions
- Easier deletion (no need to track previous node)

**Disadvantages:**
- More memory (extra pointer per node)
- More complex to maintain

```python
class DNode:
    def __init__(self, data):
        self.data = data
        self.next = None
        self.prev = None

class DoublyLinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def append(self, data):
        """Add node at end - O(1) with tail pointer!"""
        new_node = DNode(data)

        if self.head is None:
            self.head = self.tail = new_node
            return

        self.tail.next = new_node
        new_node.prev = self.tail
        self.tail = new_node

    def prepend(self, data):
        """Add node at beginning - O(1)"""
        new_node = DNode(data)

        if self.head is None:
            self.head = self.tail = new_node
            return

        new_node.next = self.head
        self.head.prev = new_node
        self.head = new_node

    def delete(self, key):
        """Delete node with given key - O(n)"""
        current = self.head

        while current:
            if current.data == key:
                # Update previous node's next
                if current.prev:
                    current.prev.next = current.next
                else:  # Deleting head
                    self.head = current.next

                # Update next node's prev
                if current.next:
                    current.next.prev = current.prev
                else:  # Deleting tail
                    self.tail = current.prev

                return

            current = current.next

    def print_forward(self):
        """Print list from head to tail"""
        current = self.head
        elements = []
        while current:
            elements.append(str(current.data))
            current = current.next
        print(" ↔ ".join(elements))

    def print_backward(self):
        """Print list from tail to head"""
        current = self.tail
        elements = []
        while current:
            elements.append(str(current.data))
            current = current.prev
        print(" ↔ ".join(elements))
```

---

## Part 4: When to Use Each Data Structure

### Decision Guide

```
Need LIFO (Last-In-First-Out)?
    → Use STACK
    Examples: Function calls, undo/redo, backtracking

Need FIFO (First-In-First-Out)?
    → Use QUEUE
    Examples: Task scheduling, BFS, message queues

Need fast random access?
    → Use ARRAY
    Examples: Lookup tables, fixed-size collections

Need frequent insertion/deletion at beginning?
    → Use LINKED LIST
    Examples: Dynamic lists, implementing stacks/queues

Need to traverse both directions?
    → Use DOUBLY LINKED LIST
    Examples: Browser history, LRU cache
```

### Complexity Comparison

| Operation | Array | Linked List | Stack | Queue |
|-----------|-------|-------------|-------|-------|
| **Access by index** | O(1) | O(n) | N/A | N/A |
| **Insert at beginning** | O(n) | O(1) | N/A | N/A |
| **Insert at end** | O(1)* | O(n) or O(1)** | O(1) | O(1) |
| **Delete at beginning** | O(n) | O(1) | N/A | O(1) |
| **Delete at end** | O(1) | O(n) | O(1) | N/A |
| **Search** | O(n) | O(n) | N/A | N/A |
| **Push/Enqueue** | N/A | N/A | O(1) | O(1) |
| **Pop/Dequeue** | N/A | N/A | O(1) | O(1) |

\* Amortized
** O(1) with tail pointer

---

## Key Takeaways

1. **Stacks (LIFO):**
   - Last in, first out
   - O(1) push and pop
   - Uses: Function calls, undo/redo, expression evaluation

2. **Queues (FIFO):**
   - First in, first out
   - O(1) enqueue and dequeue
   - Uses: Task scheduling, BFS, message queues

3. **Linked Lists:**
   - Dynamic size
   - O(1) insertion/deletion at known position
   - O(n) access by index
   - Uses: When frequent insertion/deletion at beginning

4. **Choose based on:**
   - Access pattern (LIFO, FIFO, random)
   - Operation frequency (insert, delete, access)
   - Memory constraints

5. **Implementation matters:**
   - Stacks can be array-based or linked-list-based
   - Queues are better with `deque` than list in Python
   - Linked lists trade access speed for insertion speed

---

## Discussion Questions

1. Why is removing from the beginning of an array O(n) but removing from a linked list is O(1)?

2. When would you use a stack instead of an array?

3. How does a web browser's "back" button work? What data structure does it use?

4. Why is `deque` better than a list for implementing a queue in Python?

5. What are the tradeoffs between singly and doubly linked lists?

6. How would you implement a stack using a linked list instead of an array?

---

## Next Lesson Preview

In Lesson 15, we'll explore **Trees** - hierarchical data structures that extend the concepts of linked lists with multiple children per node. Trees are fundamental for file systems, databases, and many algorithms!

Understanding stacks, queues, and linked lists provides the foundation for more complex data structures!
