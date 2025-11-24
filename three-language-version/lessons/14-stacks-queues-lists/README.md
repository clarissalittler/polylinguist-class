# Lesson 14: Stacks, Queues, and Lists

## Overview

Linear data structures with different access patterns.

## Stack (LIFO - Last In, First Out)

**Python:**
```python
class Stack:
    def __init__(self):
        self.items = []
    
    def push(self, item):
        self.items.append(item)
    
    def pop(self):
        return self.items.pop()
    
    def peek(self):
        return self.items[-1]
    
    def is_empty(self):
        return len(self.items) == 0
```

**C++:**
```cpp
template<typename T>
class Stack {
private:
    std::vector<T> items;
public:
    void push(const T& item) {
        items.push_back(item);
    }
    
    T pop() {
        T item = items.back();
        items.pop_back();
        return item;
    }
    
    const T& top() const {
        return items.back();
    }
    
    bool empty() const {
        return items.empty();
    }
};
```

**Haskell (Immutable):**
```haskell
type Stack a = [a]

push :: a -> Stack a -> Stack a
push x stack = x : stack

pop :: Stack a -> Maybe (a, Stack a)
pop [] = Nothing
pop (x:xs) = Just (x, xs)
```

## Queue (FIFO - First In, First Out)

**Python:**
```python
from collections import deque

class Queue:
    def __init__(self):
        self.items = deque()
    
    def enqueue(self, item):
        self.items.append(item)
    
    def dequeue(self):
        return self.items.popleft()
    
    def is_empty(self):
        return len(self.items) == 0
```

## Linked List

**C++:**
```cpp
struct Node {
    int data;
    Node* next;
    
    Node(int val) : data(val), next(nullptr) {}
};

class LinkedList {
private:
    Node* head;
public:
    LinkedList() : head(nullptr) {}
    
    void insert(int val) {
        Node* newNode = new Node(val);
        newNode->next = head;
        head = newNode;
    }
    
    bool search(int val) {
        Node* current = head;
        while (current != nullptr) {
            if (current->data == val) return true;
            current = current->next;
        }
        return false;
    }
};
```

**Haskell:**
```haskell
data List a = Empty | Node a (List a)

insert :: a -> List a -> List a
insert x list = Node x list

search :: Eq a => a -> List a -> Bool
search _ Empty = False
search target (Node x rest)
    | x == target = True
    | otherwise = search target rest
```

## Complexity Comparison

| Operation | Stack | Queue | Linked List |
|-----------|-------|-------|-------------|
| Push/Enqueue | O(1) | O(1) | O(1) |
| Pop/Dequeue | O(1) | O(1) | O(1) |
| Access | O(n) | O(n) | O(n) |
| Search | O(n) | O(n) | O(n) |

See EXERCISES.md for implementation practice.
