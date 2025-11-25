"""
Lesson 14: Stacks, Queues, and Lists - Python Examples
Implementing and using fundamental linear data structures
"""

from typing import Generic, TypeVar, Optional, List, Iterator
from collections import deque

T = TypeVar('T')

# =============================================================================
# STACK - Last In, First Out (LIFO)
# =============================================================================

class Stack(Generic[T]):
    """
    Stack: LIFO data structure
    Operations: push O(1), pop O(1), peek O(1)
    """

    def __init__(self):
        self._items: List[T] = []

    def push(self, item: T) -> None:
        """Add item to top of stack"""
        self._items.append(item)

    def pop(self) -> T:
        """Remove and return top item"""
        if self.is_empty():
            raise IndexError("Stack is empty")
        return self._items.pop()

    def peek(self) -> T:
        """Return top item without removing"""
        if self.is_empty():
            raise IndexError("Stack is empty")
        return self._items[-1]

    def is_empty(self) -> bool:
        return len(self._items) == 0

    def size(self) -> int:
        return len(self._items)

    def __repr__(self) -> str:
        return f"Stack({self._items})"


print("=== Stack (LIFO) ===")
stack = Stack[int]()
for item in [1, 2, 3, 4, 5]:
    stack.push(item)
print(f"After pushing 1-5: {stack}")
print(f"Peek: {stack.peek()}")
print(f"Pop: {stack.pop()}")
print(f"Pop: {stack.pop()}")
print(f"After two pops: {stack}")


# Stack Applications
def is_balanced_parentheses(s: str) -> bool:
    """Check if parentheses/brackets are balanced"""
    stack = Stack[str]()
    pairs = {')': '(', ']': '[', '}': '{'}

    for char in s:
        if char in '([{':
            stack.push(char)
        elif char in ')]}':
            if stack.is_empty() or stack.pop() != pairs[char]:
                return False

    return stack.is_empty()


print("\n=== Stack Application: Balanced Parentheses ===")
test_strings = ["(())", "([{}])", "(()", "([)]", "{[()]}"]
for s in test_strings:
    print(f"'{s}' is balanced: {is_balanced_parentheses(s)}")


def reverse_string(s: str) -> str:
    """Reverse a string using a stack"""
    stack = Stack[str]()
    for char in s:
        stack.push(char)

    result = []
    while not stack.is_empty():
        result.append(stack.pop())

    return ''.join(result)


print(f"\nReverse 'hello': {reverse_string('hello')}")


# =============================================================================
# QUEUE - First In, First Out (FIFO)
# =============================================================================

class Queue(Generic[T]):
    """
    Queue: FIFO data structure
    Operations: enqueue O(1)*, dequeue O(1)*, peek O(1)
    * Using deque for O(1) at both ends
    """

    def __init__(self):
        self._items: deque = deque()

    def enqueue(self, item: T) -> None:
        """Add item to back of queue"""
        self._items.append(item)

    def dequeue(self) -> T:
        """Remove and return front item"""
        if self.is_empty():
            raise IndexError("Queue is empty")
        return self._items.popleft()

    def front(self) -> T:
        """Return front item without removing"""
        if self.is_empty():
            raise IndexError("Queue is empty")
        return self._items[0]

    def is_empty(self) -> bool:
        return len(self._items) == 0

    def size(self) -> int:
        return len(self._items)

    def __repr__(self) -> str:
        return f"Queue({list(self._items)})"


print("\n=== Queue (FIFO) ===")
queue = Queue[str]()
for customer in ["Alice", "Bob", "Charlie", "Diana"]:
    queue.enqueue(customer)
print(f"Queue: {queue}")
print(f"Front: {queue.front()}")
print(f"Dequeue: {queue.dequeue()}")
print(f"Dequeue: {queue.dequeue()}")
print(f"After two dequeues: {queue}")


# Queue using two stacks (interview classic!)
class QueueFromStacks(Generic[T]):
    """Queue implemented using two stacks"""

    def __init__(self):
        self._in_stack = Stack[T]()
        self._out_stack = Stack[T]()

    def enqueue(self, item: T) -> None:
        self._in_stack.push(item)

    def dequeue(self) -> T:
        if self._out_stack.is_empty():
            while not self._in_stack.is_empty():
                self._out_stack.push(self._in_stack.pop())

        if self._out_stack.is_empty():
            raise IndexError("Queue is empty")

        return self._out_stack.pop()

    def is_empty(self) -> bool:
        return self._in_stack.is_empty() and self._out_stack.is_empty()


print("\n=== Queue from Two Stacks ===")
q2 = QueueFromStacks[int]()
for i in [1, 2, 3]:
    q2.enqueue(i)
print(f"Dequeue: {q2.dequeue()}")
q2.enqueue(4)
print(f"Dequeue: {q2.dequeue()}")


# =============================================================================
# DEQUE - Double-Ended Queue
# =============================================================================

print("\n=== Deque (Double-Ended Queue) ===")
dq = deque([1, 2, 3])
print(f"Initial: {list(dq)}")

dq.appendleft(0)
dq.append(4)
print(f"After appendleft(0), append(4): {list(dq)}")

dq.popleft()
dq.pop()
print(f"After popleft(), pop(): {list(dq)}")

# Rotating deque
dq = deque([1, 2, 3, 4, 5])
dq.rotate(2)
print(f"rotate(2): {list(dq)}")
dq.rotate(-2)
print(f"rotate(-2): {list(dq)}")


# =============================================================================
# LINKED LIST
# =============================================================================

class ListNode(Generic[T]):
    """Node for linked list"""

    def __init__(self, value: T):
        self.value: T = value
        self.next: Optional['ListNode[T]'] = None


class LinkedList(Generic[T]):
    """
    Singly Linked List
    Operations:
      - prepend: O(1)
      - append: O(n) or O(1) with tail pointer
      - insert at index: O(n)
      - delete: O(n)
      - search: O(n)
    """

    def __init__(self):
        self._head: Optional[ListNode[T]] = None
        self._size: int = 0

    def prepend(self, value: T) -> None:
        """Add to beginning - O(1)"""
        new_node = ListNode(value)
        new_node.next = self._head
        self._head = new_node
        self._size += 1

    def append(self, value: T) -> None:
        """Add to end - O(n)"""
        new_node = ListNode(value)
        if self._head is None:
            self._head = new_node
        else:
            current = self._head
            while current.next:
                current = current.next
            current.next = new_node
        self._size += 1

    def delete(self, value: T) -> bool:
        """Delete first occurrence - O(n)"""
        if self._head is None:
            return False

        if self._head.value == value:
            self._head = self._head.next
            self._size -= 1
            return True

        current = self._head
        while current.next:
            if current.next.value == value:
                current.next = current.next.next
                self._size -= 1
                return True
            current = current.next

        return False

    def search(self, value: T) -> bool:
        """Check if value exists - O(n)"""
        current = self._head
        while current:
            if current.value == value:
                return True
            current = current.next
        return False

    def __len__(self) -> int:
        return self._size

    def __iter__(self) -> Iterator[T]:
        current = self._head
        while current:
            yield current.value
            current = current.next

    def __repr__(self) -> str:
        return " -> ".join(str(x) for x in self) + " -> None"


print("\n=== Linked List ===")
ll = LinkedList[int]()
for i in [1, 2, 3, 4, 5]:
    ll.append(i)
print(f"List: {ll}")

ll.prepend(0)
print(f"After prepend(0): {ll}")

ll.delete(3)
print(f"After delete(3): {ll}")

print(f"Search for 4: {ll.search(4)}")
print(f"Search for 3: {ll.search(3)}")


# =============================================================================
# DOUBLY LINKED LIST
# =============================================================================

class DoublyListNode(Generic[T]):
    def __init__(self, value: T):
        self.value: T = value
        self.prev: Optional['DoublyListNode[T]'] = None
        self.next: Optional['DoublyListNode[T]'] = None


class DoublyLinkedList(Generic[T]):
    """
    Doubly Linked List
    Allows O(1) operations at both ends
    """

    def __init__(self):
        self._head: Optional[DoublyListNode[T]] = None
        self._tail: Optional[DoublyListNode[T]] = None
        self._size: int = 0

    def append(self, value: T) -> None:
        """Add to end - O(1)"""
        new_node = DoublyListNode(value)
        if self._tail is None:
            self._head = self._tail = new_node
        else:
            new_node.prev = self._tail
            self._tail.next = new_node
            self._tail = new_node
        self._size += 1

    def prepend(self, value: T) -> None:
        """Add to beginning - O(1)"""
        new_node = DoublyListNode(value)
        if self._head is None:
            self._head = self._tail = new_node
        else:
            new_node.next = self._head
            self._head.prev = new_node
            self._head = new_node
        self._size += 1

    def pop_back(self) -> T:
        """Remove from end - O(1)"""
        if self._tail is None:
            raise IndexError("List is empty")
        value = self._tail.value
        if self._head == self._tail:
            self._head = self._tail = None
        else:
            self._tail = self._tail.prev
            self._tail.next = None
        self._size -= 1
        return value

    def __repr__(self) -> str:
        items = []
        current = self._head
        while current:
            items.append(str(current.value))
            current = current.next
        return " <-> ".join(items)


print("\n=== Doubly Linked List ===")
dll = DoublyLinkedList[int]()
dll.append(1)
dll.append(2)
dll.append(3)
dll.prepend(0)
print(f"List: {dll}")
print(f"Pop back: {dll.pop_back()}")
print(f"After pop: {dll}")


# =============================================================================
# PRIORITY QUEUE
# =============================================================================

import heapq

print("\n=== Priority Queue (using heapq) ===")
pq = []
heapq.heappush(pq, (3, "Low priority"))
heapq.heappush(pq, (1, "High priority"))
heapq.heappush(pq, (2, "Medium priority"))

print("Processing by priority:")
while pq:
    priority, task = heapq.heappop(pq)
    print(f"  Priority {priority}: {task}")


# =============================================================================
# CIRCULAR QUEUE (Ring Buffer)
# =============================================================================

class CircularQueue(Generic[T]):
    """Fixed-size circular queue / ring buffer"""

    def __init__(self, capacity: int):
        self._items: List[Optional[T]] = [None] * capacity
        self._head: int = 0
        self._tail: int = 0
        self._size: int = 0
        self._capacity: int = capacity

    def enqueue(self, item: T) -> bool:
        """Add item, return False if full"""
        if self._size == self._capacity:
            return False
        self._items[self._tail] = item
        self._tail = (self._tail + 1) % self._capacity
        self._size += 1
        return True

    def dequeue(self) -> Optional[T]:
        """Remove and return front item"""
        if self._size == 0:
            return None
        item = self._items[self._head]
        self._head = (self._head + 1) % self._capacity
        self._size -= 1
        return item

    def is_full(self) -> bool:
        return self._size == self._capacity

    def is_empty(self) -> bool:
        return self._size == 0


print("\n=== Circular Queue ===")
cq = CircularQueue[int](3)
cq.enqueue(1)
cq.enqueue(2)
cq.enqueue(3)
print(f"Full: {cq.is_full()}")
print(f"Dequeue: {cq.dequeue()}")
cq.enqueue(4)
print(f"Items: {cq.dequeue()}, {cq.dequeue()}, {cq.dequeue()}")


# =============================================================================
# COMPARISON AND WHEN TO USE
# =============================================================================

print("\n=== When to Use Each Data Structure ===")
print("""
STACK (LIFO):
  - Undo/Redo functionality
  - Expression evaluation (postfix)
  - Backtracking algorithms
  - Function call stack
  - Syntax parsing (parentheses matching)

QUEUE (FIFO):
  - Task scheduling
  - BFS (Breadth-First Search)
  - Print job spooling
  - Message passing between processes
  - Buffering

DEQUE (Double-Ended):
  - When you need both LIFO and FIFO
  - Sliding window algorithms
  - Job stealing in parallel computing

LINKED LIST:
  - Frequent insertions/deletions
  - Unknown size
  - No random access needed
  - Implement other data structures

ARRAY/LIST:
  - Random access needed
  - Known size (or infrequent resizing)
  - Cache-friendly iteration

PRIORITY QUEUE:
  - Task prioritization
  - Dijkstra's algorithm
  - Event simulation
  - Huffman coding
""")
