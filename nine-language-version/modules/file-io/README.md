# Module: File I/O and System Interaction

## Learning Objectives

By the end of this module, you will be able to:

1. Read and write text files in multiple languages
2. Handle file paths and directories correctly
3. Process CSV and JSON data files
4. Handle I/O errors gracefully
5. Work with binary files
6. Use command-line arguments
7. Redirect standard input/output/error
8. Apply best practices for file operations

## Why File I/O Matters

**Real programs interact with the outside world:**
- **Reading** configuration files, data files, user input
- **Writing** logs, reports, output data
- **Processing** CSV spreadsheets, JSON APIs, text documents
- **Persisting** data between program runs

Without file I/O, programs can only work with hardcoded data!

---

## Part 1: Reading Text Files

### Python: Reading Files

```python
# Method 1: Read entire file
with open('data.txt', 'r') as file:
    content = file.read()
    print(content)

# Method 2: Read line by line
with open('data.txt', 'r') as file:
    for line in file:
        print(line.strip())  # strip() removes newline

# Method 3: Read all lines into list
with open('data.txt', 'r') as file:
    lines = file.readlines()
    for line in lines:
        print(line.strip())

# Method 4: Read specific number of characters
with open('data.txt', 'r') as file:
    first_100_chars = file.read(100)
```

**Key points:**
- `with` statement ensures file is closed automatically
- `'r'` mode = read (default)
- Always close files when done (or use `with`)

### Java: Reading Files

```java
import java.io.*;
import java.nio.file.*;
import java.util.*;

// Method 1: Read entire file as string (Java 11+)
String content = Files.readString(Path.of("data.txt"));

// Method 2: Read all lines into list
List<String> lines = Files.readAllLines(Path.of("data.txt"));
for (String line : lines) {
    System.out.println(line);
}

// Method 3: Using BufferedReader (traditional)
try (BufferedReader reader = new BufferedReader(new FileReader("data.txt"))) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Method 4: Stream API (Java 8+)
try {
    Files.lines(Path.of("data.txt"))
         .forEach(System.out::println);
} catch (IOException e) {
    e.printStackTrace();
}
```

### C: Reading Files

```c
#include <stdio.h>
#include <stdlib.h>

// Method 1: Read line by line
FILE *file = fopen("data.txt", "r");
if (file == NULL) {
    perror("Error opening file");
    return 1;
}

char line[256];
while (fgets(line, sizeof(line), file) != NULL) {
    printf("%s", line);
}

fclose(file);

// Method 2: Read character by character
FILE *file = fopen("data.txt", "r");
int ch;
while ((ch = fgetc(file)) != EOF) {
    putchar(ch);
}
fclose(file);
```

**Key points:**
- Always check if `fopen()` returns NULL (error)
- Always `fclose()` when done
- Use `perror()` to print error messages

---

## Part 2: Writing Text Files

### Python: Writing Files

```python
# Method 1: Write string to file (overwrites)
with open('output.txt', 'w') as file:
    file.write("Hello, World!\n")
    file.write("This is line 2\n")

# Method 2: Write multiple lines
lines = ["Line 1\n", "Line 2\n", "Line 3\n"]
with open('output.txt', 'w') as file:
    file.writelines(lines)

# Method 3: Append to file (doesn't overwrite)
with open('output.txt', 'a') as file:
    file.write("Appended line\n")

# Method 4: Write with print()
with open('output.txt', 'w') as file:
    print("Hello", file=file)
    print("World", file=file)
```

**File modes:**
- `'w'` - Write (overwrites existing file)
- `'a'` - Append (adds to end of file)
- `'x'` - Exclusive create (fails if file exists)

### Java: Writing Files

```java
import java.io.*;
import java.nio.file.*;

// Method 1: Write string to file (Java 11+)
Files.writeString(Path.of("output.txt"), "Hello, World!\n");

// Method 2: Write lines
List<String> lines = Arrays.asList("Line 1", "Line 2", "Line 3");
Files.write(Path.of("output.txt"), lines);

// Method 3: Using BufferedWriter
try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.txt"))) {
    writer.write("Hello, World!");
    writer.newLine();
    writer.write("Line 2");
} catch (IOException e) {
    e.printStackTrace();
}

// Method 4: Append to file
try (BufferedWriter writer = new BufferedWriter(
        new FileWriter("output.txt", true))) {  // true = append
    writer.write("Appended line");
    writer.newLine();
} catch (IOException e) {
    e.printStackTrace();
}
```

### C: Writing Files

```c
#include <stdio.h>

// Write to file
FILE *file = fopen("output.txt", "w");
if (file == NULL) {
    perror("Error opening file");
    return 1;
}

fprintf(file, "Hello, World!\n");
fprintf(file, "Number: %d\n", 42);

fclose(file);

// Append to file
FILE *file = fopen("output.txt", "a");
fprintf(file, "Appended line\n");
fclose(file);
```

---

## Part 3: File Paths and Directories

### Working with Paths

**Absolute vs Relative Paths:**
- **Absolute:** `/home/user/data.txt` or `C:\Users\user\data.txt`
- **Relative:** `data.txt` (current directory) or `../data.txt` (parent directory)

### Python: Path Handling

```python
import os
from pathlib import Path

# Using pathlib (modern, recommended)
path = Path('data.txt')
print(path.exists())          # Check if exists
print(path.is_file())         # Is it a file?
print(path.is_dir())          # Is it a directory?
print(path.absolute())        # Get absolute path

# Join paths (cross-platform)
data_dir = Path('data')
file_path = data_dir / 'myfile.txt'  # Works on Windows and Unix!

# Using os.path (older, still common)
print(os.path.exists('data.txt'))
print(os.path.isfile('data.txt'))
print(os.path.isdir('data'))
print(os.path.abspath('data.txt'))

# Join paths
file_path = os.path.join('data', 'myfile.txt')
```

### Creating and Listing Directories

```python
import os
from pathlib import Path

# Create directory
Path('new_folder').mkdir(exist_ok=True)  # exist_ok prevents error if exists

# Create nested directories
Path('parent/child/grandchild').mkdir(parents=True, exist_ok=True)

# List files in directory
for file in Path('.').iterdir():
    print(file.name)

# List only .txt files
for file in Path('.').glob('*.txt'):
    print(file)

# Walk directory tree (using os)
for root, dirs, files in os.walk('.'):
    for filename in files:
        print(os.path.join(root, filename))
```

### Java: Path Handling

```java
import java.nio.file.*;
import java.io.IOException;

// Create Path objects
Path path = Paths.get("data.txt");
Path absPath = path.toAbsolutePath();

// Check existence
boolean exists = Files.exists(path);
boolean isFile = Files.isRegularFile(path);
boolean isDir = Files.isDirectory(path);

// Join paths (cross-platform)
Path dataDir = Paths.get("data");
Path filePath = dataDir.resolve("myfile.txt");

// Create directories
Files.createDirectory(Paths.get("new_folder"));
Files.createDirectories(Paths.get("parent/child/grandchild"));

// List files
try {
    Files.list(Paths.get("."))
         .forEach(System.out::println);
} catch (IOException e) {
    e.printStackTrace();
}
```

---

## Part 4: Error Handling

### Python: Try-Except for File Operations

```python
# Handling file not found
try:
    with open('nonexistent.txt', 'r') as file:
        content = file.read()
except FileNotFoundError:
    print("Error: File not found!")
except PermissionError:
    print("Error: Permission denied!")
except IOError as e:
    print(f"I/O error: {e}")

# More robust version
def read_file_safely(filename):
    try:
        with open(filename, 'r') as file:
            return file.read()
    except FileNotFoundError:
        print(f"Error: '{filename}' not found")
        return None
    except PermissionError:
        print(f"Error: Permission denied for '{filename}'")
        return None
    except Exception as e:
        print(f"Unexpected error: {e}")
        return None
```

### Java: Try-Catch for File Operations

```java
import java.io.*;
import java.nio.file.*;

public class FileReader {
    public static String readFileSafely(String filename) {
        try {
            return Files.readString(Path.of(filename));
        } catch (FileNotFoundException e) {
            System.err.println("Error: File not found: " + filename);
            return null;
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            return null;
        }
    }
}
```

### C: Error Checking

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

FILE *file = fopen("data.txt", "r");
if (file == NULL) {
    // Print error message with errno
    fprintf(stderr, "Error opening file: %s\n", strerror(errno));
    return 1;
}

// Use file...

fclose(file);
```

---

## Part 5: Working with CSV Files

### Python: CSV Reading and Writing

```python
import csv

# Reading CSV
with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)  # row is a list

# Reading CSV with headers
with open('data.csv', 'r') as file:
    reader = csv.DictReader(file)
    for row in reader:
        print(row['name'], row['age'])  # row is a dictionary

# Writing CSV
data = [
    ['Name', 'Age', 'City'],
    ['Alice', 30, 'NYC'],
    ['Bob', 25, 'LA'],
]

with open('output.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)

# Writing CSV with DictWriter
with open('output.csv', 'w', newline='') as file:
    fieldnames = ['name', 'age', 'city']
    writer = csv.DictWriter(file, fieldnames=fieldnames)

    writer.writeheader()
    writer.writerow({'name': 'Alice', 'age': 30, 'city': 'NYC'})
    writer.writerow({'name': 'Bob', 'age': 25, 'city': 'LA'})
```

### Example: Processing CSV Data

```python
import csv

# Read CSV, filter data, write results
with open('students.csv', 'r') as infile, \
     open('honors.csv', 'w', newline='') as outfile:

    reader = csv.DictReader(infile)
    writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)

    writer.writeheader()

    for row in reader:
        if float(row['gpa']) >= 3.5:
            writer.writerow(row)
```

---

## Part 6: Working with JSON Files

### Python: JSON Reading and Writing

```python
import json

# Reading JSON
with open('data.json', 'r') as file:
    data = json.load(file)  # Returns Python dict/list

print(data['name'])
print(data['scores'])

# Writing JSON
data = {
    'name': 'Alice',
    'age': 30,
    'scores': [95, 87, 92],
    'active': True
}

with open('output.json', 'w') as file:
    json.dump(data, file, indent=2)  # indent for pretty printing

# Convert between JSON string and Python object
json_string = json.dumps(data, indent=2)
parsed_data = json.loads(json_string)
```

### Example: API Data Processing

```python
import json

# Read JSON from file
with open('users.json', 'r') as file:
    users = json.load(file)

# Process data
active_users = [user for user in users if user['active']]

# Calculate average age
total_age = sum(user['age'] for user in active_users)
avg_age = total_age / len(active_users) if active_users else 0

# Save results
results = {
    'active_count': len(active_users),
    'average_age': avg_age,
    'users': active_users
}

with open('results.json', 'w') as file:
    json.dump(results, file, indent=2)
```

### Java: JSON with Gson

```java
import com.google.gson.*;
import java.io.*;
import java.nio.file.*;

// Reading JSON
Gson gson = new Gson();
try {
    String json = Files.readString(Path.of("data.json"));
    JsonObject obj = gson.fromJson(json, JsonObject.class);
    String name = obj.get("name").getAsString();
} catch (IOException e) {
    e.printStackTrace();
}

// Writing JSON
JsonObject data = new JsonObject();
data.addProperty("name", "Alice");
data.addProperty("age", 30);

try {
    Files.writeString(Path.of("output.json"),
                     new GsonBuilder().setPrettyPrinting()
                                      .create()
                                      .toJson(data));
} catch (IOException e) {
    e.printStackTrace();
}
```

---

## Part 7: Command-Line Arguments

### Python: sys.argv

```python
import sys

# Access command-line arguments
print(f"Script name: {sys.argv[0]}")
print(f"Number of arguments: {len(sys.argv) - 1}")

for i, arg in enumerate(sys.argv[1:], 1):
    print(f"Argument {i}: {arg}")

# Example usage
if len(sys.argv) < 3:
    print("Usage: python script.py <input> <output>")
    sys.exit(1)

input_file = sys.argv[1]
output_file = sys.argv[2]

# Run: python script.py data.txt results.txt
```

### Python: argparse (Recommended)

```python
import argparse

parser = argparse.ArgumentParser(description='Process some files')

# Positional arguments
parser.add_argument('input', help='Input file')
parser.add_argument('output', help='Output file')

# Optional arguments
parser.add_argument('-v', '--verbose', action='store_true',
                   help='Enable verbose output')
parser.add_argument('-n', '--count', type=int, default=10,
                   help='Number of items to process')

args = parser.parse_args()

print(f"Input: {args.input}")
print(f"Output: {args.output}")
if args.verbose:
    print("Verbose mode enabled")
print(f"Count: {args.count}")

# Run: python script.py input.txt output.txt -v -n 20
```

### Java: Command-Line Arguments

```java
public class FileProcessor {
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: java FileProcessor <input> <output>");
            System.exit(1);
        }

        String inputFile = args[0];
        String outputFile = args[1];

        System.out.println("Input: " + inputFile);
        System.out.println("Output: " + outputFile);

        // Process files...
    }
}

// Run: java FileProcessor data.txt results.txt
```

### C: Command-Line Arguments

```c
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <input> <output>\n", argv[0]);
        return 1;
    }

    char *input_file = argv[1];
    char *output_file = argv[2];

    printf("Input: %s\n", input_file);
    printf("Output: %s\n", output_file);

    // Process files...

    return 0;
}

// Compile: gcc program.c -o program
// Run: ./program data.txt results.txt
```

---

## Part 8: Standard I/O Streams

### Standard Input (stdin), Output (stdout), Error (stderr)

**Concept:** Every program has three streams:
- **stdin** - Standard input (keyboard by default)
- **stdout** - Standard output (terminal by default)
- **stderr** - Standard error (terminal by default, for errors)

### Python: Standard Streams

```python
import sys

# Read from standard input
line = input("Enter your name: ")
print(f"Hello, {line}!")

# Read all lines from stdin (useful for piping)
for line in sys.stdin:
    print(f"Received: {line.strip()}")

# Write to standard output
sys.stdout.write("This goes to stdout\n")
print("This also goes to stdout")

# Write to standard error
sys.stderr.write("This is an error message\n")
print("Error!", file=sys.stderr)
```

### Redirection and Piping

```bash
# Redirect input from file
python script.py < input.txt

# Redirect output to file
python script.py > output.txt

# Redirect errors to file
python script.py 2> errors.txt

# Redirect both output and errors
python script.py > output.txt 2>&1

# Pipe output to another program
cat data.txt | python process.py | sort > results.txt
```

### Example: Processing Piped Input

```python
# filter.py - Filter lines containing a keyword
import sys

keyword = sys.argv[1] if len(sys.argv) > 1 else "error"

for line in sys.stdin:
    if keyword.lower() in line.lower():
        print(line, end='')

# Usage:
# cat log.txt | python filter.py error
# python generate_data.py | python filter.py important | sort
```

---

## Part 9: Binary Files

### Python: Binary File I/O

```python
# Write binary data
data = bytes([0x48, 0x65, 0x6C, 0x6C, 0x6F])  # "Hello" in bytes
with open('data.bin', 'wb') as file:
    file.write(data)

# Read binary data
with open('data.bin', 'rb') as file:
    data = file.read()
    print(list(data))  # [72, 101, 108, 108, 111]

# Working with structs (fixed-size binary data)
import struct

# Write structured binary data
with open('data.bin', 'wb') as file:
    # Pack: int (4 bytes), float (4 bytes), char (1 byte)
    packed = struct.pack('if c', 42, 3.14, b'A')
    file.write(packed)

# Read structured binary data
with open('data.bin', 'rb') as file:
    data = file.read()
    num, pi, char = struct.unpack('if c', data)
    print(f"Number: {num}, Pi: {pi}, Char: {char}")
```

### C: Binary File I/O

```c
#include <stdio.h>

// Write binary data
int numbers[] = {1, 2, 3, 4, 5};
FILE *file = fopen("data.bin", "wb");
fwrite(numbers, sizeof(int), 5, file);
fclose(file);

// Read binary data
int buffer[5];
FILE *file = fopen("data.bin", "rb");
fread(buffer, sizeof(int), 5, file);
fclose(file);

for (int i = 0; i < 5; i++) {
    printf("%d ", buffer[i]);
}
```

---

## Part 10: Best Practices

### 1. Always Close Files

```python
# Bad: File might not close if error occurs
file = open('data.txt', 'r')
content = file.read()
file.close()

# Good: Use 'with' statement
with open('data.txt', 'r') as file:
    content = file.read()
# File automatically closed
```

### 2. Handle Errors Gracefully

```python
# Bad: Crashes if file doesn't exist
with open('data.txt', 'r') as file:
    content = file.read()

# Good: Handle errors
try:
    with open('data.txt', 'r') as file:
        content = file.read()
except FileNotFoundError:
    print("File not found")
    content = None
```

### 3. Use Appropriate File Modes

- `'r'` - Read (default)
- `'w'` - Write (overwrites!)
- `'a'` - Append
- `'x'` - Exclusive create (fails if exists)
- `'b'` - Binary mode (add to above: `'rb'`, `'wb'`)
- `'+'` - Read and write (`'r+'`, `'w+'`)

### 4. Use Path Libraries

```python
# Bad: String concatenation (breaks on Windows)
path = 'data/' + filename

# Good: Use Path
from pathlib import Path
path = Path('data') / filename
```

### 5. Validate Input

```python
def process_file(filename):
    path = Path(filename)

    # Check if file exists
    if not path.exists():
        raise FileNotFoundError(f"{filename} not found")

    # Check if it's actually a file
    if not path.is_file():
        raise ValueError(f"{filename} is not a file")

    # Check if readable
    if not os.access(path, os.R_OK):
        raise PermissionError(f"Cannot read {filename}")

    # Now safe to read
    with open(path, 'r') as file:
        return file.read()
```

### 6. Use Context Managers for Multiple Files

```python
# Process input file and write to output file
with open('input.txt', 'r') as infile, \
     open('output.txt', 'w') as outfile:
    for line in infile:
        processed = line.upper()
        outfile.write(processed)
```

---

## Complete Example: Log File Analyzer

```python
#!/usr/bin/env python3
import sys
import argparse
from pathlib import Path
from collections import Counter
import json

def analyze_log_file(input_file, output_file=None, verbose=False):
    """
    Analyze log file and generate statistics.
    """
    path = Path(input_file)

    if not path.exists():
        print(f"Error: {input_file} not found", file=sys.stderr)
        return 1

    # Statistics
    total_lines = 0
    error_count = 0
    warning_count = 0
    level_counts = Counter()

    try:
        with open(path, 'r') as file:
            for line in file:
                total_lines += 1

                # Count log levels
                if 'ERROR' in line:
                    error_count += 1
                    level_counts['ERROR'] += 1
                elif 'WARNING' in line:
                    warning_count += 1
                    level_counts['WARNING'] += 1
                elif 'INFO' in line:
                    level_counts['INFO'] += 1

                if verbose and 'ERROR' in line:
                    print(f"Error on line {total_lines}: {line.strip()}")

        # Generate report
        report = {
            'total_lines': total_lines,
            'errors': error_count,
            'warnings': warning_count,
            'level_distribution': dict(level_counts)
        }

        # Output results
        if output_file:
            with open(output_file, 'w') as file:
                json.dump(report, file, indent=2)
            print(f"Report saved to {output_file}")
        else:
            print(json.dumps(report, indent=2))

        return 0

    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return 1

def main():
    parser = argparse.ArgumentParser(description='Analyze log files')
    parser.add_argument('input', help='Input log file')
    parser.add_argument('-o', '--output', help='Output JSON file')
    parser.add_argument('-v', '--verbose', action='store_true',
                       help='Print errors as found')

    args = parser.parse_args()

    return analyze_log_file(args.input, args.output, args.verbose)

if __name__ == '__main__':
    sys.exit(main())
```

**Usage:**
```bash
python log_analyzer.py server.log
python log_analyzer.py server.log -o report.json
python log_analyzer.py server.log -v -o report.json
```

---

## Key Takeaways

1. **Always use `with` statement** (Python) or try-with-resources (Java) for automatic cleanup
2. **Handle errors gracefully** - files can be missing, inaccessible, or corrupted
3. **Use path libraries** for cross-platform compatibility
4. **Validate inputs** before processing
5. **Choose appropriate file formats** (CSV for tables, JSON for structured data, text for simple data)
6. **Close files** when done to free resources
7. **Use command-line arguments** for flexible programs
8. **Leverage standard streams** for piping and redirection

---

## Next Steps

Practice file I/O by building real projects:
- Log file analyzer
- CSV data processor
- Configuration file reader
- Data backup utility
- Text file search tool

**Remember:** File I/O is how programs interact with persistent data!
