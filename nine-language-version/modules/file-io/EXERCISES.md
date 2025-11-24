# File I/O: Practice Exercises

## Exercise 1: Basic File Reading

### 1a. Read and Count

Write a program that:
1. Reads a text file
2. Counts the number of lines
3. Counts the number of words
4. Counts the number of characters
5. Prints the statistics

**Test with:**
```
# sample.txt
Hello world
This is a test
Programming is fun
```

**Expected output:**
```
Lines: 3
Words: 9
Characters: 47
```

<details>
<summary>Solution (Python)</summary>

```python
def analyze_file(filename):
    try:
        with open(filename, 'r') as file:
            content = file.read()
            lines = content.split('\n')
            words = content.split()
            chars = len(content)

            print(f"Lines: {len(lines)}")
            print(f"Words: {len(words)}")
            print(f"Characters: {chars}")

    except FileNotFoundError:
        print(f"Error: {filename} not found")

analyze_file('sample.txt')
```
</details>

---

### 1b. Find and Replace

Write a program that:
1. Reads a file
2. Replaces all occurrences of a word
3. Writes the result to a new file

**Example:**
```python
replace_in_file('input.txt', 'output.txt', 'old_word', 'new_word')
```

<details>
<summary>Solution (Python)</summary>

```python
def replace_in_file(input_file, output_file, old_word, new_word):
    try:
        with open(input_file, 'r') as infile:
            content = infile.read()

        # Replace all occurrences
        new_content = content.replace(old_word, new_word)

        with open(output_file, 'w') as outfile:
            outfile.write(new_content)

        print(f"Replaced '{old_word}' with '{new_word}'")
        print(f"Output saved to {output_file}")

    except FileNotFoundError:
        print(f"Error: {input_file} not found")
    except Exception as e:
        print(f"Error: {e}")
```
</details>

---

## Exercise 2: CSV Processing

### 2a. CSV Reader

Given a CSV file of students:
```csv
name,grade,age
Alice,95,20
Bob,87,19
Charlie,92,21
```

Write a program to:
1. Read the CSV
2. Calculate average grade
3. Find the student with highest grade
4. Print results

<details>
<summary>Solution (Python)</summary>

```python
import csv

def analyze_students(filename):
    try:
        with open(filename, 'r') as file:
            reader = csv.DictReader(file)
            students = list(reader)

        # Calculate average grade
        total_grade = sum(int(s['grade']) for s in students)
        avg_grade = total_grade / len(students)

        # Find top student
        top_student = max(students, key=lambda s: int(s['grade']))

        print(f"Average grade: {avg_grade:.2f}")
        print(f"Top student: {top_student['name']} ({top_student['grade']})")

    except FileNotFoundError:
        print(f"Error: {filename} not found")

analyze_students('students.csv')
```
</details>

---

### 2b. CSV Filter

Given a CSV of products:
```csv
name,price,stock
Laptop,999.99,15
Mouse,29.99,50
Keyboard,79.99,30
Monitor,299.99,10
```

Write a program to:
1. Read the CSV
2. Filter products where stock < 20
3. Write low-stock items to a new CSV

<details>
<summary>Solution (Python)</summary>

```python
import csv

def filter_low_stock(input_file, output_file, threshold=20):
    try:
        with open(input_file, 'r') as infile, \
             open(output_file, 'w', newline='') as outfile:

            reader = csv.DictReader(infile)
            writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)

            writer.writeheader()

            for row in reader:
                if int(row['stock']) < threshold:
                    writer.writerow(row)

        print(f"Low stock items saved to {output_file}")

    except FileNotFoundError:
        print(f"Error: {input_file} not found")

filter_low_stock('products.csv', 'low_stock.csv')
```
</details>

---

## Exercise 3: JSON Processing

### 3a. JSON Reader

Given a JSON file:
```json
{
  "users": [
    {"name": "Alice", "age": 30, "active": true},
    {"name": "Bob", "age": 25, "active": false},
    {"name": "Charlie", "age": 35, "active": true}
  ]
}
```

Write a program to:
1. Read the JSON
2. Count active users
3. Calculate average age
4. List names of active users

<details>
<summary>Solution (Python)</summary>

```python
import json

def analyze_users(filename):
    try:
        with open(filename, 'r') as file:
            data = json.load(file)

        users = data['users']
        active_users = [u for u in users if u['active']]

        avg_age = sum(u['age'] for u in users) / len(users)

        print(f"Total users: {len(users)}")
        print(f"Active users: {len(active_users)}")
        print(f"Average age: {avg_age:.1f}")
        print("Active user names:", [u['name'] for u in active_users])

    except FileNotFoundError:
        print(f"Error: {filename} not found")
    except json.JSONDecodeError:
        print("Error: Invalid JSON format")

analyze_users('users.json')
```
</details>

---

### 3b. JSON Transformer

Read a JSON file of transactions and generate a summary:

**Input (transactions.json):**
```json
[
  {"date": "2024-01-01", "amount": 100.50, "category": "food"},
  {"date": "2024-01-02", "amount": 50.00, "category": "transport"},
  {"date": "2024-01-03", "amount": 75.25, "category": "food"}
]
```

**Output (summary.json):**
```json
{
  "total": 225.75,
  "by_category": {
    "food": 175.75,
    "transport": 50.00
  },
  "transaction_count": 3
}
```

<details>
<summary>Solution (Python)</summary>

```python
import json
from collections import defaultdict

def summarize_transactions(input_file, output_file):
    try:
        with open(input_file, 'r') as file:
            transactions = json.load(file)

        total = sum(t['amount'] for t in transactions)

        by_category = defaultdict(float)
        for t in transactions:
            by_category[t['category']] += t['amount']

        summary = {
            'total': round(total, 2),
            'by_category': dict(by_category),
            'transaction_count': len(transactions)
        }

        with open(output_file, 'w') as file:
            json.dump(summary, file, indent=2)

        print(f"Summary saved to {output_file}")

    except FileNotFoundError:
        print(f"Error: {input_file} not found")
    except json.JSONDecodeError:
        print("Error: Invalid JSON format")

summarize_transactions('transactions.json', 'summary.json')
```
</details>

---

## Exercise 4: Command-Line Programs

### 4a. Word Counter Tool

Create a command-line tool that counts words in a file:

```bash
python word_counter.py myfile.txt
python word_counter.py myfile.txt -w "hello"  # Count specific word
python word_counter.py myfile.txt -i         # Case-insensitive
```

<details>
<summary>Solution (Python)</summary>

```python
import argparse
from collections import Counter

def count_words(filename, word=None, case_insensitive=False):
    try:
        with open(filename, 'r') as file:
            text = file.read()

        if case_insensitive:
            text = text.lower()
            if word:
                word = word.lower()

        words = text.split()

        if word:
            count = words.count(word)
            print(f"'{word}' appears {count} times")
        else:
            word_counts = Counter(words)
            print(f"Total words: {len(words)}")
            print(f"Unique words: {len(word_counts)}")
            print("\nMost common:")
            for word, count in word_counts.most_common(10):
                print(f"  {word}: {count}")

    except FileNotFoundError:
        print(f"Error: {filename} not found")

def main():
    parser = argparse.ArgumentParser(description='Count words in a file')
    parser.add_argument('filename', help='File to analyze')
    parser.add_argument('-w', '--word', help='Specific word to count')
    parser.add_argument('-i', '--case-insensitive', action='store_true',
                       help='Ignore case')

    args = parser.parse_args()
    count_words(args.filename, args.word, args.case_insensitive)

if __name__ == '__main__':
    main()
```
</details>

---

### 4b. File Concatenator

Create a tool that combines multiple files:

```bash
python concat.py file1.txt file2.txt file3.txt -o combined.txt
```

<details>
<summary>Solution (Python)</summary>

```python
import argparse

def concatenate_files(input_files, output_file):
    try:
        with open(output_file, 'w') as outfile:
            for filename in input_files:
                try:
                    with open(filename, 'r') as infile:
                        outfile.write(f"\n--- {filename} ---\n")
                        outfile.write(infile.read())
                        outfile.write("\n")
                except FileNotFoundError:
                    print(f"Warning: {filename} not found, skipping")

        print(f"Combined {len(input_files)} files into {output_file}")

    except Exception as e:
        print(f"Error: {e}")

def main():
    parser = argparse.ArgumentParser(description='Concatenate files')
    parser.add_argument('files', nargs='+', help='Files to combine')
    parser.add_argument('-o', '--output', required=True,
                       help='Output file')

    args = parser.parse_args()
    concatenate_files(args.files, args.output)

if __name__ == '__main__':
    main()
```
</details>

---

## Exercise 5: Error Handling

### 5a. Robust File Reader

Write a function that handles all possible file errors gracefully:

```python
def read_file_robust(filename):
    """
    Read file with comprehensive error handling.
    Returns: (success: bool, content: str or None, error: str or None)
    """
    # Your implementation
```

<details>
<summary>Solution (Python)</summary>

```python
def read_file_robust(filename):
    """
    Read file with comprehensive error handling.
    Returns: (success: bool, content: str or None, error: str or None)
    """
    try:
        with open(filename, 'r') as file:
            content = file.read()
        return (True, content, None)

    except FileNotFoundError:
        return (False, None, f"File not found: {filename}")

    except PermissionError:
        return (False, None, f"Permission denied: {filename}")

    except IsADirectoryError:
        return (False, None, f"Is a directory: {filename}")

    except UnicodeDecodeError:
        return (False, None, f"Cannot decode file (not text?): {filename}")

    except Exception as e:
        return (False, None, f"Unexpected error: {e}")

# Usage
success, content, error = read_file_robust('data.txt')
if success:
    print(content)
else:
    print(f"Error: {error}")
```
</details>

---

## Exercise 6: Real-World Projects

### 6a. Log File Analyzer

Create a program that analyzes log files:

**Input (server.log):**
```
2024-01-01 10:00:00 INFO Server started
2024-01-01 10:05:00 ERROR Database connection failed
2024-01-01 10:06:00 WARNING Slow query detected
2024-01-01 10:10:00 INFO Request processed
2024-01-01 10:15:00 ERROR Timeout occurred
```

**Output:**
- Count of each log level
- List of all errors
- Save summary to JSON

<details>
<summary>Solution (Python)</summary>

```python
import re
import json
from collections import Counter
from datetime import datetime

def analyze_log(log_file, output_file=None):
    level_counts = Counter()
    errors = []

    try:
        with open(log_file, 'r') as file:
            for line in file:
                # Parse log line
                match = re.match(r'(\S+ \S+) (\w+) (.+)', line)
                if match:
                    timestamp, level, message = match.groups()
                    level_counts[level] += 1

                    if level == 'ERROR':
                        errors.append({
                            'timestamp': timestamp,
                            'message': message.strip()
                        })

        summary = {
            'total_lines': sum(level_counts.values()),
            'level_counts': dict(level_counts),
            'error_count': len(errors),
            'errors': errors
        }

        # Print summary
        print(f"Total log entries: {summary['total_lines']}")
        print("\nBy level:")
        for level, count in level_counts.items():
            print(f"  {level}: {count}")

        print(f"\nErrors found: {len(errors)}")
        for error in errors:
            print(f"  [{error['timestamp']}] {error['message']}")

        # Save to JSON if requested
        if output_file:
            with open(output_file, 'w') as file:
                json.dump(summary, file, indent=2)
            print(f"\nSummary saved to {output_file}")

    except FileNotFoundError:
        print(f"Error: {log_file} not found")

analyze_log('server.log', 'log_summary.json')
```
</details>

---

### 6b. CSV to JSON Converter

Create a general-purpose CSV to JSON converter:

```bash
python csv_to_json.py input.csv output.json
```

<details>
<summary>Solution (Python)</summary>

```python
import csv
import json
import argparse

def csv_to_json(csv_file, json_file, indent=2):
    try:
        with open(csv_file, 'r') as file:
            reader = csv.DictReader(file)
            data = list(reader)

        with open(json_file, 'w') as file:
            json.dump(data, file, indent=indent)

        print(f"Converted {len(data)} rows")
        print(f"Output saved to {json_file}")

    except FileNotFoundError:
        print(f"Error: {csv_file} not found")
    except Exception as e:
        print(f"Error: {e}")

def main():
    parser = argparse.ArgumentParser(description='Convert CSV to JSON')
    parser.add_argument('csv_file', help='Input CSV file')
    parser.add_argument('json_file', help='Output JSON file')
    parser.add_argument('--no-indent', action='store_true',
                       help='No indentation (compact JSON)')

    args = parser.parse_args()
    indent = None if args.no_indent else 2

    csv_to_json(args.csv_file, args.json_file, indent)

if __name__ == '__main__':
    main()
```
</details>

---

### 6c. Configuration File Manager

Create a program that reads/writes configuration in JSON:

```python
# Save configuration
config = {
    'database': {
        'host': 'localhost',
        'port': 5432,
        'name': 'mydb'
    },
    'logging': {
        'level': 'INFO',
        'file': 'app.log'
    }
}
save_config('config.json', config)

# Load and use configuration
config = load_config('config.json')
print(config['database']['host'])
```

<details>
<summary>Solution (Python)</summary>

```python
import json
from pathlib import Path

def save_config(filename, config):
    """Save configuration to JSON file"""
    try:
        with open(filename, 'w') as file:
            json.dump(config, file, indent=2)
        print(f"Configuration saved to {filename}")
    except Exception as e:
        print(f"Error saving config: {e}")

def load_config(filename, default=None):
    """Load configuration from JSON file"""
    try:
        with open(filename, 'r') as file:
            return json.load(file)
    except FileNotFoundError:
        print(f"Config file not found, using defaults")
        return default or {}
    except json.JSONDecodeError:
        print(f"Invalid JSON in config file")
        return default or {}

def get_config_value(config, path, default=None):
    """
    Get nested config value using dot notation.
    Example: get_config_value(config, 'database.host', 'localhost')
    """
    keys = path.split('.')
    value = config
    try:
        for key in keys:
            value = value[key]
        return value
    except (KeyError, TypeError):
        return default

# Example usage
if __name__ == '__main__':
    # Default configuration
    default_config = {
        'database': {
            'host': 'localhost',
            'port': 5432,
            'name': 'mydb'
        },
        'logging': {
            'level': 'INFO',
            'file': 'app.log'
        }
    }

    # Save default config if doesn't exist
    config_file = 'config.json'
    if not Path(config_file).exists():
        save_config(config_file, default_config)

    # Load configuration
    config = load_config(config_file, default_config)

    # Use configuration
    db_host = get_config_value(config, 'database.host', 'localhost')
    log_level = get_config_value(config, 'logging.level', 'INFO')

    print(f"Database host: {db_host}")
    print(f"Log level: {log_level}")
```
</details>

---

## Challenge Exercises

### Challenge 1: File Backup Tool

Create a tool that backs up files:
- Copy files from source to backup directory
- Add timestamp to backup filenames
- Skip files that haven't changed (compare modification time)
- Generate backup report

### Challenge 2: Markdown to HTML Converter

Create a simple Markdown to HTML converter:
- Read .md file
- Convert headers, lists, bold, italic
- Write to .html file

### Challenge 3: Data Pipeline

Create a data processing pipeline:
1. Read CSV of raw data
2. Clean data (remove invalid rows)
3. Transform data (calculations, formatting)
4. Export to both JSON and CSV
5. Generate summary statistics

---

## Summary Checklist

After completing these exercises, you should be able to:
- ✅ Read and write text files
- ✅ Process CSV and JSON data
- ✅ Handle file errors gracefully
- ✅ Work with file paths cross-platform
- ✅ Use command-line arguments
- ✅ Build practical file-processing tools
- ✅ Combine file operations with data structures

**Remember:** File I/O is how programs interact with persistent data!
