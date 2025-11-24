# Project 3: Todo List Manager

**Difficulty**: ⭐⭐⭐ Medium
**Time Estimate**: 5-8 hours
**Prerequisites**: Lessons 1-5 (Hello World through Data Structures)

## Project Description

Build a command-line todo list manager that lets users create, view, complete, and delete tasks. Tasks should persist between program runs.

### Example Usage

```bash
$ python todo.py add "Buy groceries"
Added: Buy groceries

$ python todo.py add "Write essay"
Added: Write essay

$ python todo.py list
1. [ ] Buy groceries
2. [ ] Write essay

$ python todo.py complete 1
Completed: Buy groceries

$ python todo.py list
1. [✓] Buy groceries
2. [ ] Write essay

$ python todo.py delete 1
Deleted: Buy groceries

$ python todo.py list
1. [ ] Write essay
```

## Requirements

### Core Features (Required)

1. **Add task**: `todo add "Task description"`
2. **List tasks**: `todo list`
3. **Complete task**: `todo complete <id>`
4. **Delete task**: `todo delete <id>`
5. **Persist data**: Save/load from file

### Intermediate Features (Recommended)

6. **Edit task**: `todo edit <id> "New description"`
7. **Priority levels**: High, Medium, Low
8. **Due dates**: Assign dates to tasks
9. **Filter tasks**: Show only incomplete or only completed
10. **Search tasks**: `todo search "keyword"`

### Advanced Features (Optional)

11. **Categories/tags**: Organize tasks
12. **Recurring tasks**: Daily, weekly, etc.
13. **Statistics**: Show completion rate
14. **Export**: To CSV, JSON, or Markdown
15. **Undo**: Undo last action

## Implementation Requirements

### Implement in at least 2 languages from different paradigms:

**Recommended combinations:**
- **Python**: Dict/list, JSON for persistence
- **Haskell**: Immutable data, pure functions
- **Rust**: Ownership model, type safety
- **JavaScript**: Objects, file I/O

**Compare:**
- How did data persistence differ?
- Which language made CRUD operations easier?
- How did mutability affect implementation?

## Technical Specifications

### Data Structure

**Task representation:**
```python
# Python
{
    "id": 1,
    "description": "Buy groceries",
    "completed": False,
    "created": "2024-01-15",
    "priority": "medium"
}
```

**Haskell:**
```haskell
data Task = Task {
    taskId :: Int,
    description :: String,
    completed :: Bool,
    created :: String,
    priority :: Priority
}
```

### File Format

Use JSON for simplicity:
```json
{
    "nextId": 3,
    "tasks": [
        {
            "id": 1,
            "description": "Buy groceries",
            "completed": true
        },
        {
            "id": 2,
            "description": "Write essay",
            "completed": false
        }
    ]
}
```

### Command-Line Interface

Parse arguments:
- `python todo.py <command> [arguments]`
- Commands: add, list, complete, delete, edit
- Handle invalid commands gracefully

## Step-by-Step Guide

### Part 1: Basic Structure

**Step 1**: Define data structures
```python
# Python
class TodoList:
    def __init__(self):
        self.tasks = []
        self.next_id = 1

    def add_task(self, description):
        task = {
            'id': self.next_id,
            'description': description,
            'completed': False
        }
        self.tasks.append(task)
        self.next_id += 1
        return task
```

**Step 2**: Implement CRUD operations
```python
def list_tasks(self):
    return self.tasks

def complete_task(self, task_id):
    for task in self.tasks:
        if task['id'] == task_id:
            task['completed'] = True
            return task
    return None

def delete_task(self, task_id):
    self.tasks = [t for t in self.tasks if t['id'] != task_id]
```

### Part 2: File Persistence

**Step 3**: Save to file
```python
import json

def save(self, filename="todos.json"):
    data = {
        'next_id': self.next_id,
        'tasks': self.tasks
    }
    with open(filename, 'w') as f:
        json.dump(data, f, indent=2)
```

**Step 4**: Load from file
```python
def load(filename="todos.json"):
    try:
        with open(filename, 'r') as f:
            data = json.load(f)
            todo_list = TodoList()
            todo_list.tasks = data['tasks']
            todo_list.next_id = data['next_id']
            return todo_list
    except FileNotFoundError:
        return TodoList()  # Return empty list if file doesn't exist
```

### Part 3: Command-Line Interface

**Step 5**: Parse commands
```python
import sys

def main():
    if len(sys.argv) < 2:
        print("Usage: python todo.py <command> [arguments]")
        return

    command = sys.argv[1]
    todo_list = TodoList.load()

    if command == "add":
        description = " ".join(sys.argv[2:])
        task = todo_list.add_task(description)
        print(f"Added: {task['description']}")

    elif command == "list":
        for task in todo_list.list_tasks():
            status = "[✓]" if task['completed'] else "[ ]"
            print(f"{task['id']}. {status} {task['description']}")

    elif command == "complete":
        task_id = int(sys.argv[2])
        task = todo_list.complete_task(task_id)
        if task:
            print(f"Completed: {task['description']}")
        else:
            print(f"Task {task_id} not found")

    # ... other commands

    todo_list.save()
```

## Language-Specific Hints

### Python
- Use `json` module for persistence
- `sys.argv` for command-line arguments
- List comprehensions for filtering
- Dict for task representation

### JavaScript (Node.js)
- `fs` module for file I/O
- `process.argv` for arguments
- `JSON.parse()` and `JSON.stringify()`
- Objects or classes for tasks

### Haskell
- `aeson` library for JSON
- `System.Environment.getArgs` for arguments
- Immutable list of tasks
- `Map` for ID-based lookup
- State monad or just pass state through functions

### Rust
- `serde_json` for JSON serialization
- `std::env::args()` for arguments
- `Vec<Task>` for task list
- `struct Task` for task representation
- Error handling with `Result`

### C
- Manual JSON parsing (or library like `json-c`)
- `argc`, `argv` for arguments
- Structs for tasks
- Dynamic arrays or linked lists
- Manual memory management

## Extensions

### Extension 1: Priority Management

Add priority levels and sorting:
```bash
$ todo add "Urgent task" --priority high
$ todo list --sort priority
1. [!] Urgent task (HIGH)
2. [ ] Regular task (MEDIUM)
```

### Extension 2: Due Dates

Add due dates and overdue warnings:
```bash
$ todo add "Submit assignment" --due 2024-01-20
$ todo list
1. [ ] Submit assignment (Due: Jan 20) ⚠️ OVERDUE
```

### Extension 3: Rich CLI

Use colored output, progress bars:
- Red for overdue
- Green for completed
- Yellow for high priority
- Progress bar for completion percentage

### Extension 4: Interactive Mode

Instead of command-line arguments:
```
=== Todo List Manager ===
1. Add task
2. List tasks
3. Complete task
4. Delete task
5. Exit
Choose option: _
```

## Testing Checklist

- [ ] Can add tasks
- [ ] Can list tasks (including empty list)
- [ ] Can complete tasks
- [ ] Can delete tasks
- [ ] Data persists between runs
- [ ] Handles non-existent task IDs
- [ ] Handles invalid commands
- [ ] File created if doesn't exist
- [ ] Multiple instances don't corrupt data

## Grading Rubric (50 points)

| Criteria | Points | Description |
|----------|--------|-------------|
| Add Task | 5 | Can add tasks with descriptions |
| List Tasks | 5 | Displays all tasks correctly |
| Complete Task | 5 | Marks tasks as complete |
| Delete Task | 5 | Removes tasks |
| File Persistence | 10 | Saves/loads correctly |
| Command Parsing | 5 | Handles CLI arguments |
| Error Handling | 5 | Graceful failures |
| Code Quality | 5 | Clean, organized, documented |
| Multi-Language | 3 | Implemented in 2+ languages |
| Extensions | 2 | Additional features |

## Architecture Considerations

### Separation of Concerns

Good architecture separates:
1. **Data layer**: Task structures, CRUD operations
2. **Persistence layer**: File I/O, serialization
3. **UI layer**: Command parsing, output formatting

**Example structure:**
```
todo.py
├── Task class (data)
├── TodoList class (CRUD)
├── FileStorage class (persistence)
└── CLI class (user interface)
```

### Immutable vs Mutable

**Mutable approach (Python):**
- Modify list in place
- Simpler, more direct
- Must be careful with state

**Immutable approach (Haskell):**
- Create new list for each operation
- Safer, more predictable
- Requires passing state through functions

## Reflection Questions

1. **How did you structure your data?**
   - List? Dictionary? Custom class/struct?
   - Why that choice?

2. **What persistence format did you use?**
   - JSON? Plain text? Binary?
   - Tradeoffs?

3. **How did you handle IDs?**
   - Sequential? UUID?
   - What happens when tasks are deleted?

4. **If you implemented in multiple languages:**
   - Which was easier?
   - Did immutability (Haskell) or ownership (Rust) affect design?

5. **What would you improve with more time?**

## Real-World Applications

This project teaches:
- **CRUD operations**: Create, Read, Update, Delete
- **Data persistence**: Saving state between runs
- **File I/O**: Working with files
- **Command-line tools**: Building CLI applications
- **Data modeling**: Designing data structures
- **Error handling**: Graceful degradation

These skills are used in:
- Web backends (database CRUD)
- System administration tools
- Data processing pipelines
- Application development

## Next Steps

- Add more features (tags, recurring tasks, etc.)
- Build a web interface
- Sync across devices
- Move on to Project 4 (Expression Evaluator)

---

**This is a practical project you can actually use!** Many programmers start by building their own todo list manager.
