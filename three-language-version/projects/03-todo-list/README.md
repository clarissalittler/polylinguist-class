# Project 3: Todo List Manager

**Difficulty:** Medium
**Estimated Time:** 5-8 hours
**Prerequisites:** Lessons 1-6, File I/O Module
**Due:** End of Week 8

**Capstone Option:** Implement in Racket for capstone credit

---

## Overview

Build a persistent command-line todo list application with add, complete, delete, and list operations. This project reinforces data structures, file persistence, command-line interface design, and state management.

---

## Learning Objectives

By completing this project, you will:
- Design data structures for real applications
- Implement CRUD operations (Create, Read, Update, Delete)
- Persist data to files (save/load)
- Build a command-line interface with multiple commands
- Handle state changes and data integrity
- Work with dates and timestamps

---

## Requirements

### Basic Requirements (Must Complete)

Your todo manager should support:

1. **Add a task**: `todo add "Buy groceries"`
2. **List tasks**: `todo list` - Show all tasks with their status
3. **Complete a task**: `todo done 1` - Mark task #1 as complete
4. **Delete a task**: `todo delete 1` - Remove task #1
5. **Persistence**: Tasks saved to file, survive program restart

**Example Session:**
```bash
$ todo add "Buy groceries"
Added task #1: Buy groceries

$ todo add "Call mom"
Added task #2: Call mom

$ todo add "Finish project"
Added task #3: Finish project

$ todo list
Todo List:
  1. [ ] Buy groceries
  2. [ ] Call mom
  3. [ ] Finish project

$ todo done 1
Completed: Buy groceries

$ todo list
Todo List:
  1. [x] Buy groceries
  2. [ ] Call mom
  3. [ ] Finish project

$ todo delete 2
Deleted: Call mom

$ todo list
Todo List:
  1. [x] Buy groceries
  2. [ ] Finish project
```

### Standard Requirements (Complete Most)

Extend your manager with:

6. **Timestamps**: Show when tasks were created
7. **Due dates**: `todo add "Submit report" --due 2024-03-15`
8. **Priority levels**: `todo add "Urgent task" --priority high`
9. **Filter views**: `todo list --pending` or `todo list --completed`
10. **Edit task**: `todo edit 1 "New description"`

**Extended Example:**
```bash
$ todo add "Submit report" --due 2024-03-15 --priority high
Added task #1: Submit report (due: Mar 15, priority: HIGH)

$ todo list
Todo List:
  1. [ ] Submit report          (due: Mar 15, HIGH) created: Mar 10
  2. [ ] Buy groceries          (due: none, normal) created: Mar 10
  3. [x] Call mom              (due: none, normal) completed: Mar 10

$ todo list --pending
Pending Tasks:
  1. [ ] Submit report          (due: Mar 15, HIGH)
  2. [ ] Buy groceries          (due: none, normal)

$ todo list --overdue
Overdue Tasks:
  (none)
```

### Advanced Requirements (Challenge)

For additional challenge:

11. **Multiple lists**: Support different todo lists (work, personal, etc.)
12. **Tags/categories**: `todo add "Meeting" --tags work,urgent`
13. **Recurring tasks**: Tasks that reset after completion
14. **Search**: `todo search "report"` - Find tasks by keyword
15. **Export**: `todo export --format json` or `--format csv`
16. **Undo**: Undo last operation

**Advanced Example:**
```bash
$ todo add "Weekly review" --recurring weekly
Added recurring task #1: Weekly review (every week)

$ todo list --tags work
Tasks tagged 'work':
  2. [ ] Submit report
  4. [ ] Team meeting

$ todo export --format json > backup.json

$ todo delete 1
Deleted: Weekly review

$ todo undo
Restored: Weekly review
```

---

## Technical Specifications

### Data Model

Each task should have:
- **ID**: Unique identifier (integer)
- **Description**: Task text
- **Status**: Pending or completed
- **Created**: Timestamp of creation
- **Completed**: Timestamp of completion (if applicable)
- **Due date**: Optional deadline
- **Priority**: Low, normal, or high

### File Format

Choose a format for persistence:

**Simple text format:**
```
1|Buy groceries|pending|2024-03-10T10:30:00||normal|
2|Call mom|completed|2024-03-10T10:31:00|2024-03-10T14:22:00|normal|
```

**JSON format (recommended):**
```json
{
  "tasks": [
    {
      "id": 1,
      "description": "Buy groceries",
      "status": "pending",
      "created": "2024-03-10T10:30:00",
      "completed": null,
      "priority": "normal",
      "due": null
    }
  ],
  "next_id": 2
}
```

### Command-Line Interface

Support these patterns:
```bash
todo <command> [arguments] [options]

Commands:
  add <description>    Add a new task
  list                 List all tasks
  done <id>            Mark task as complete
  delete <id>          Delete a task
  edit <id> <desc>     Edit task description
  help                 Show help

Options:
  --due DATE           Set due date (YYYY-MM-DD)
  --priority LEVEL     Set priority (low, normal, high)
  --pending            Show only pending tasks
  --completed          Show only completed tasks
```

---

## Language-Specific Hints

### Python

```python
import json
import sys
from datetime import datetime
from pathlib import Path

TODO_FILE = Path.home() / ".todo.json"

def load_tasks():
    if TODO_FILE.exists():
        with open(TODO_FILE) as f:
            return json.load(f)
    return {"tasks": [], "next_id": 1}

def save_tasks(data):
    with open(TODO_FILE, 'w') as f:
        json.dump(data, f, indent=2)

def add_task(description, due=None, priority="normal"):
    data = load_tasks()
    task = {
        "id": data["next_id"],
        "description": description,
        "status": "pending",
        "created": datetime.now().isoformat(),
        "completed": None,
        "due": due,
        "priority": priority
    }
    data["tasks"].append(task)
    data["next_id"] += 1
    save_tasks(data)
    return task

def main():
    if len(sys.argv) < 2:
        print("Usage: todo <command> [arguments]")
        return

    command = sys.argv[1]

    if command == "add":
        description = " ".join(sys.argv[2:])
        task = add_task(description)
        print(f"Added task #{task['id']}: {description}")
    elif command == "list":
        # ... implement list
        pass
    # ... other commands

if __name__ == "__main__":
    main()
```

### C++

```cpp
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <ctime>
#include <nlohmann/json.hpp>  // Popular JSON library

using json = nlohmann::json;

struct Task {
    int id;
    std::string description;
    std::string status;
    std::string created;
    std::string completed;
    std::string priority;
    std::string due;
};

class TodoManager {
private:
    std::vector<Task> tasks;
    int next_id;
    std::string filename;

public:
    TodoManager(const std::string& file) : filename(file), next_id(1) {
        load();
    }

    void load() {
        std::ifstream f(filename);
        if (f.is_open()) {
            json j;
            f >> j;
            next_id = j["next_id"];
            for (const auto& t : j["tasks"]) {
                tasks.push_back({
                    t["id"],
                    t["description"],
                    t["status"],
                    t["created"],
                    t.value("completed", ""),
                    t.value("priority", "normal"),
                    t.value("due", "")
                });
            }
        }
    }

    void save() {
        json j;
        j["next_id"] = next_id;
        j["tasks"] = json::array();
        for (const auto& t : tasks) {
            j["tasks"].push_back({
                {"id", t.id},
                {"description", t.description},
                {"status", t.status},
                {"created", t.created},
                {"completed", t.completed},
                {"priority", t.priority},
                {"due", t.due}
            });
        }
        std::ofstream f(filename);
        f << j.dump(2);
    }

    void addTask(const std::string& desc) {
        // Implementation
    }
};

int main(int argc, char* argv[]) {
    TodoManager manager("todo.json");
    // Parse arguments and execute commands
    return 0;
}
```

### Haskell

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.Environment
import Data.Time

data Task = Task
    { taskId :: Int
    , description :: String
    , status :: String
    , created :: String
    , completed :: Maybe String
    , priority :: String
    , due :: Maybe String
    } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

data TodoData = TodoData
    { tasks :: [Task]
    , nextId :: Int
    } deriving (Show, Generic)

instance FromJSON TodoData
instance ToJSON TodoData

loadTasks :: FilePath -> IO TodoData
loadTasks path = do
    content <- B.readFile path
    case decode content of
        Just d  -> return d
        Nothing -> return $ TodoData [] 1

saveTasks :: FilePath -> TodoData -> IO ()
saveTasks path d = B.writeFile path (encode d)

addTask :: String -> TodoData -> IO TodoData
addTask desc d = do
    now <- getCurrentTime
    let task = Task
            { taskId = nextId d
            , description = desc
            , status = "pending"
            , created = show now
            , completed = Nothing
            , priority = "normal"
            , due = Nothing
            }
    return $ d { tasks = tasks d ++ [task], nextId = nextId d + 1 }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("add":rest) -> do
            let desc = unwords rest
            d <- loadTasks "todo.json"
            d' <- addTask desc d
            saveTasks "todo.json" d'
            putStrLn $ "Added task #" ++ show (nextId d) ++ ": " ++ desc
        ["list"] -> do
            d <- loadTasks "todo.json"
            mapM_ printTask (tasks d)
        _ -> putStrLn "Usage: todo <command> [arguments]"

printTask :: Task -> IO ()
printTask t = putStrLn $ show (taskId t) ++ ". [" ++ mark ++ "] " ++ description t
    where mark = if status t == "completed" then "x" else " "
```

---

## Capstone: Racket Implementation

For capstone credit, implement in Racket:

```racket
#lang racket

(require json)

(define TODO-FILE "todo.json")

(define (load-tasks)
  (if (file-exists? TODO-FILE)
      (call-with-input-file TODO-FILE
        (lambda (in) (read-json in)))
      (hasheq 'tasks '() 'next_id 1)))

(define (save-tasks data)
  (call-with-output-file TODO-FILE
    (lambda (out) (write-json data out))
    #:exists 'replace))

(define (add-task desc)
  (let* ([data (load-tasks)]
         [id (hash-ref data 'next_id)]
         [task (hasheq 'id id
                       'description desc
                       'status "pending"
                       'created (current-seconds))])
    (save-tasks
      (hash-set* data
                 'tasks (append (hash-ref data 'tasks) (list task))
                 'next_id (add1 id)))
    (printf "Added task #~a: ~a\n" id desc)))

(define (main args)
  (match args
    [(list "add" desc ...) (add-task (string-join desc " "))]
    [(list "list") (list-tasks)]
    [_ (displayln "Usage: racket todo.rkt <command> [args]")]))

(main (vector->list (current-command-line-arguments)))
```

---

## Submission Checklist

- [ ] Add, list, done, delete commands work
- [ ] Tasks persist to file
- [ ] IDs are handled correctly after deletions
- [ ] Timestamps recorded
- [ ] At least 2 standard features implemented
- [ ] Edge cases handled (empty list, invalid ID)
- [ ] Code is well-organized
- [ ] `REFLECTION.md` completed
- [ ] (Capstone) Racket implementation + comparison

---

## Reflection Questions

1. How did you design your data structure for tasks? What tradeoffs did you make?
2. How did you handle task IDs when tasks are deleted? (Renumber? Keep gaps?)
3. What file format did you choose and why?
4. How does mutability/immutability affect your implementation in different languages?
5. (Capstone) How did Racket's approach differ from your core language implementation?

---

## Grading Rubric

| Criteria | Points |
|----------|--------|
| Basic CRUD operations | 35 |
| File persistence | 20 |
| Standard features | 20 |
| Error handling | 10 |
| Code quality | 10 |
| Reflection | 5 |
| **Total** | **100** |

**Capstone Bonus:** +25 for complete Racket implementation with reflection

---

## Common Mistakes to Avoid

1. **Losing data on crash** - Save after every operation
2. **ID collisions** - Track next available ID properly
3. **Corrupted file handling** - What if JSON is malformed?
4. **Empty description** - Validate input
5. **Not handling missing file** - First run case

---

Good luck building your todo manager!
