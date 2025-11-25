/**
 * Lesson 14: Stacks, Queues, and Lists - C++ Examples
 * Implementing and using fundamental linear data structures
 */

#include <iostream>
#include <vector>
#include <list>
#include <stack>
#include <queue>
#include <deque>
#include <string>
#include <memory>

using namespace std;

// =============================================================================
// STACK - Last In, First Out (LIFO)
// =============================================================================

template<typename T>
class MyStack {
private:
    vector<T> items;

public:
    void push(const T& item) {
        items.push_back(item);
    }

    T pop() {
        if (empty()) {
            throw runtime_error("Stack is empty");
        }
        T item = items.back();
        items.pop_back();
        return item;
    }

    T& top() {
        if (empty()) {
            throw runtime_error("Stack is empty");
        }
        return items.back();
    }

    bool empty() const {
        return items.empty();
    }

    size_t size() const {
        return items.size();
    }
};

void stack_demo() {
    cout << "=== Stack (LIFO) ===" << endl;

    // Using our implementation
    MyStack<int> myStack;
    for (int i = 1; i <= 5; i++) {
        myStack.push(i);
    }
    cout << "Custom stack - Top: " << myStack.top() << endl;
    cout << "Pop: " << myStack.pop() << endl;
    cout << "Pop: " << myStack.pop() << endl;

    // Using STL stack
    cout << "\nSTL stack:" << endl;
    stack<int> stlStack;
    stlStack.push(10);
    stlStack.push(20);
    stlStack.push(30);
    cout << "Top: " << stlStack.top() << endl;
    stlStack.pop();
    cout << "After pop, top: " << stlStack.top() << endl;
}

// Stack application: Balanced parentheses
bool isBalanced(const string& s) {
    stack<char> stk;
    for (char c : s) {
        if (c == '(' || c == '[' || c == '{') {
            stk.push(c);
        } else if (c == ')' || c == ']' || c == '}') {
            if (stk.empty()) return false;
            char top = stk.top();
            stk.pop();
            if ((c == ')' && top != '(') ||
                (c == ']' && top != '[') ||
                (c == '}' && top != '{')) {
                return false;
            }
        }
    }
    return stk.empty();
}

void balanced_demo() {
    cout << "\n=== Balanced Parentheses ===" << endl;
    vector<string> tests = {"(())", "([{}])", "(()", "([)]", "{[()]}"};
    for (const auto& s : tests) {
        cout << "'" << s << "' is " << (isBalanced(s) ? "balanced" : "not balanced") << endl;
    }
}

// =============================================================================
// QUEUE - First In, First Out (FIFO)
// =============================================================================

template<typename T>
class MyQueue {
private:
    deque<T> items;

public:
    void enqueue(const T& item) {
        items.push_back(item);
    }

    T dequeue() {
        if (empty()) {
            throw runtime_error("Queue is empty");
        }
        T item = items.front();
        items.pop_front();
        return item;
    }

    T& front() {
        if (empty()) {
            throw runtime_error("Queue is empty");
        }
        return items.front();
    }

    bool empty() const {
        return items.empty();
    }

    size_t size() const {
        return items.size();
    }
};

void queue_demo() {
    cout << "\n=== Queue (FIFO) ===" << endl;

    MyQueue<string> q;
    q.enqueue("Alice");
    q.enqueue("Bob");
    q.enqueue("Charlie");

    cout << "Front: " << q.front() << endl;
    cout << "Dequeue: " << q.dequeue() << endl;
    cout << "Dequeue: " << q.dequeue() << endl;
    cout << "New front: " << q.front() << endl;

    // STL queue
    cout << "\nSTL queue:" << endl;
    queue<int> stlQueue;
    stlQueue.push(1);
    stlQueue.push(2);
    stlQueue.push(3);
    cout << "Front: " << stlQueue.front() << ", Back: " << stlQueue.back() << endl;
}

// =============================================================================
// DEQUE - Double-Ended Queue
// =============================================================================

void deque_demo() {
    cout << "\n=== Deque (Double-Ended Queue) ===" << endl;

    deque<int> dq = {2, 3, 4};
    dq.push_front(1);
    dq.push_back(5);

    cout << "Contents: ";
    for (int x : dq) cout << x << " ";
    cout << endl;

    cout << "Front: " << dq.front() << ", Back: " << dq.back() << endl;

    dq.pop_front();
    dq.pop_back();
    cout << "After popping both ends: ";
    for (int x : dq) cout << x << " ";
    cout << endl;
}

// =============================================================================
// LINKED LIST
// =============================================================================

template<typename T>
class LinkedList {
private:
    struct Node {
        T value;
        unique_ptr<Node> next;
        Node(T val) : value(val), next(nullptr) {}
    };

    unique_ptr<Node> head;
    size_t length = 0;

public:
    void prepend(T value) {
        auto newNode = make_unique<Node>(value);
        newNode->next = move(head);
        head = move(newNode);
        length++;
    }

    void append(T value) {
        auto newNode = make_unique<Node>(value);
        if (!head) {
            head = move(newNode);
        } else {
            Node* current = head.get();
            while (current->next) {
                current = current->next.get();
            }
            current->next = move(newNode);
        }
        length++;
    }

    bool remove(T value) {
        if (!head) return false;

        if (head->value == value) {
            head = move(head->next);
            length--;
            return true;
        }

        Node* current = head.get();
        while (current->next) {
            if (current->next->value == value) {
                current->next = move(current->next->next);
                length--;
                return true;
            }
            current = current->next.get();
        }
        return false;
    }

    bool contains(T value) const {
        Node* current = head.get();
        while (current) {
            if (current->value == value) return true;
            current = current->next.get();
        }
        return false;
    }

    size_t size() const { return length; }

    void print() const {
        Node* current = head.get();
        while (current) {
            cout << current->value << " -> ";
            current = current->next.get();
        }
        cout << "null" << endl;
    }
};

void linked_list_demo() {
    cout << "\n=== Linked List ===" << endl;

    LinkedList<int> list;
    list.append(1);
    list.append(2);
    list.append(3);
    list.prepend(0);

    cout << "List: ";
    list.print();

    cout << "Contains 2: " << boolalpha << list.contains(2) << endl;
    cout << "Contains 5: " << list.contains(5) << endl;

    list.remove(2);
    cout << "After removing 2: ";
    list.print();
}

// STL list demo
void stl_list_demo() {
    cout << "\n=== STL list (Doubly Linked) ===" << endl;

    list<int> lst = {1, 2, 3, 4, 5};

    // Insert at beginning and end
    lst.push_front(0);
    lst.push_back(6);

    // Insert in middle
    auto it = lst.begin();
    advance(it, 3);
    lst.insert(it, 99);

    cout << "List: ";
    for (int x : lst) cout << x << " ";
    cout << endl;

    // Remove specific value
    lst.remove(99);
    cout << "After removing 99: ";
    for (int x : lst) cout << x << " ";
    cout << endl;

    // Reverse
    lst.reverse();
    cout << "Reversed: ";
    for (int x : lst) cout << x << " ";
    cout << endl;
}

// =============================================================================
// PRIORITY QUEUE
// =============================================================================

void priority_queue_demo() {
    cout << "\n=== Priority Queue ===" << endl;

    // Max heap by default
    priority_queue<int> maxHeap;
    maxHeap.push(3);
    maxHeap.push(1);
    maxHeap.push(4);
    maxHeap.push(1);
    maxHeap.push(5);

    cout << "Max heap (descending): ";
    while (!maxHeap.empty()) {
        cout << maxHeap.top() << " ";
        maxHeap.pop();
    }
    cout << endl;

    // Min heap
    priority_queue<int, vector<int>, greater<int>> minHeap;
    minHeap.push(3);
    minHeap.push(1);
    minHeap.push(4);
    minHeap.push(1);
    minHeap.push(5);

    cout << "Min heap (ascending): ";
    while (!minHeap.empty()) {
        cout << minHeap.top() << " ";
        minHeap.pop();
    }
    cout << endl;

    // Priority queue with custom objects
    struct Task {
        int priority;
        string name;
        bool operator<(const Task& other) const {
            return priority > other.priority;  // Lower number = higher priority
        }
    };

    priority_queue<Task> tasks;
    tasks.push({2, "Medium task"});
    tasks.push({1, "High priority"});
    tasks.push({3, "Low priority"});

    cout << "Tasks by priority:" << endl;
    while (!tasks.empty()) {
        cout << "  " << tasks.top().name << endl;
        tasks.pop();
    }
}

// =============================================================================
// CIRCULAR BUFFER
// =============================================================================

template<typename T>
class CircularBuffer {
private:
    vector<T> buffer;
    size_t head = 0;
    size_t tail = 0;
    size_t count = 0;
    size_t capacity;

public:
    CircularBuffer(size_t cap) : buffer(cap), capacity(cap) {}

    bool push(const T& item) {
        if (count == capacity) return false;
        buffer[tail] = item;
        tail = (tail + 1) % capacity;
        count++;
        return true;
    }

    bool pop(T& item) {
        if (count == 0) return false;
        item = buffer[head];
        head = (head + 1) % capacity;
        count--;
        return true;
    }

    bool full() const { return count == capacity; }
    bool empty() const { return count == 0; }
    size_t size() const { return count; }
};

void circular_buffer_demo() {
    cout << "\n=== Circular Buffer ===" << endl;

    CircularBuffer<int> cb(3);
    cb.push(1);
    cb.push(2);
    cb.push(3);
    cout << "Full: " << boolalpha << cb.full() << endl;

    int item;
    cb.pop(item);
    cout << "Popped: " << item << endl;

    cb.push(4);
    cout << "Items: ";
    while (!cb.empty()) {
        cb.pop(item);
        cout << item << " ";
    }
    cout << endl;
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    stack_demo();
    balanced_demo();
    queue_demo();
    deque_demo();
    linked_list_demo();
    stl_list_demo();
    priority_queue_demo();
    circular_buffer_demo();

    cout << "\n=== When to Use Each ===" << endl;
    cout << R"(
Stack (std::stack):
  - LIFO operations
  - Undo/redo, expression parsing
  - DFS, backtracking

Queue (std::queue):
  - FIFO operations
  - BFS, task scheduling
  - Buffering

Deque (std::deque):
  - Operations at both ends
  - Sliding window algorithms
  - Default for std::stack and std::queue

List (std::list):
  - Frequent insertions/deletions in middle
  - No random access needed
  - Stable iterators during modification

Vector (std::vector):
  - Random access needed
  - Cache-friendly iteration
  - Most common choice

Priority Queue:
  - Process by priority
  - Dijkstra's algorithm
  - Event simulation
)" << endl;

    return 0;
}
