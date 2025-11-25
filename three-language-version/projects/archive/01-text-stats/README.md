# Project 1: Text Statistics Tool

**Difficulty:** Easy-Medium
**Estimated Time:** 3-5 hours
**Prerequisites:** Lessons 1-3 (Hello World, Variables & Types, Control Flow)
**Due:** End of Week 3

---

## Overview

Build a command-line tool that analyzes text files and produces statistics about their contents. This project reinforces file I/O, string processing, loops, and basic data structures.

---

## Learning Objectives

By completing this project, you will:
- Read and process text files
- Work with strings and character data
- Use collections (lists, dictionaries/maps)
- Practice control flow (loops, conditionals)
- Handle command-line arguments
- Format output clearly

---

## Requirements

### Basic Requirements (Must Complete)

Your program should accept a filename as input and output:

1. **Line count** - Total number of lines in the file
2. **Word count** - Total number of words (whitespace-separated)
3. **Character count** - Total characters (including whitespace)
4. **Character count (no spaces)** - Characters excluding whitespace

**Example:**
```
$ python text_stats.py sample.txt

=== Text Statistics for sample.txt ===
Lines:            42
Words:            256
Characters:       1,523
Characters (no spaces): 1,267
```

### Standard Requirements (Complete Most)

Extend your tool with:

5. **Average word length** - Mean length of all words
6. **Average words per line** - Mean words per line
7. **Longest word** - The longest word found (or first if tie)
8. **Most common word** - The word that appears most often (case-insensitive)

**Extended Example:**
```
$ ./text_stats sample.txt

=== Text Statistics for sample.txt ===
Lines:                42
Words:                256
Characters:           1,523
Characters (no spaces): 1,267

Average word length:  4.9
Average words/line:   6.1
Longest word:         "programming"
Most common word:     "the" (appears 23 times)
```

### Advanced Requirements (Challenge)

For additional challenge:

9. **Top N words** - Show the N most common words (N as argument)
10. **Unique words** - Count of distinct words
11. **Lexical diversity** - Ratio of unique words to total words
12. **Sentence count** - Count sentences (end with `.`, `!`, `?`)
13. **Reading level** - Implement Flesch-Kincaid or similar metric

**Advanced Example:**
```
$ python text_stats.py sample.txt --top 5

=== Text Statistics for sample.txt ===
[... basic stats ...]

Top 5 words:
  1. "the" (23 times)
  2. "and" (18 times)
  3. "to" (15 times)
  4. "a" (12 times)
  5. "is" (11 times)

Unique words:     156
Lexical diversity: 0.61
Sentences:        28
Flesch-Kincaid Grade Level: 8.2
```

---

## Technical Specifications

### Input
- A text file (plain text, UTF-8 encoded)
- File path provided as command-line argument
- Handle file not found errors gracefully

### Output
- Formatted text to standard output
- Numbers should be formatted nicely (commas for thousands)
- Percentages/averages to 1-2 decimal places

### Edge Cases to Handle
- Empty file (should work, showing zeros)
- File with only whitespace
- Very long lines
- Non-ASCII characters (handle gracefully)
- File not found (helpful error message)

---

## Language-Specific Hints

### Python

```python
import sys

def main():
    if len(sys.argv) < 2:
        print("Usage: python text_stats.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]

    try:
        with open(filename, 'r') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        sys.exit(1)

    # Your analysis code here
    lines = content.split('\n')
    words = content.split()
    # ...

if __name__ == "__main__":
    main()
```

**Useful Python features:**
- `str.split()` for words
- `str.lower()` for case-insensitive comparison
- `collections.Counter` for word frequency
- f-strings for formatted output

### C++

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <sstream>

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << "Error: Cannot open file " << argv[1] << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(file, line)) {
        // Process each line
    }

    return 0;
}
```

**Useful C++ features:**
- `std::ifstream` for file reading
- `std::stringstream` for parsing words
- `std::map<std::string, int>` for word counts
- `std::transform` with `::tolower` for case conversion

### Haskell

```haskell
import System.Environment (getArgs)
import Data.List (sortBy, group, sort)
import Data.Char (toLower, isSpace)
import Data.Ord (comparing, Down(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let stats = analyzeText content
            printStats filename stats
        _ -> putStrLn "Usage: text_stats <filename>"

analyzeText :: String -> TextStats
analyzeText content = TextStats
    { lineCount = length (lines content)
    , wordCount = length (words content)
    -- ...
    }

-- Define TextStats data type
data TextStats = TextStats
    { lineCount :: Int
    , wordCount :: Int
    -- ...
    }
```

**Useful Haskell features:**
- `lines` and `words` functions
- `Data.List.group` and `sort` for frequency
- Pattern matching on `args`
- Custom data types for stats

---

## Sample Test Files

### sample1.txt (Simple)
```
Hello World
This is a test file.
It has three lines.
```

**Expected output:**
```
Lines: 3
Words: 11
Characters: 52
```

### sample2.txt (More Complex)
```
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
Pack my box with five dozen liquor jugs.
```

**Expected output:**
```
Lines: 3
Words: 27
Most common word: "the" (4 times)
```

---

## Submission Checklist

- [ ] Program compiles/runs without errors
- [ ] Basic requirements all working
- [ ] At least 3 standard requirements implemented
- [ ] Handles edge cases (empty file, file not found)
- [ ] Code is readable with comments
- [ ] Git commits show incremental progress
- [ ] `REFLECTION.md` completed

---

## Reflection Questions

Answer these in your `REFLECTION.md`:

1. Which language did you choose and why?
2. What was the hardest part of this project?
3. How did you handle counting words? Were there edge cases?
4. If you implemented in multiple languages, what differences did you notice?
5. What would you do differently if starting over?

---

## Grading Rubric

| Criteria | Points |
|----------|--------|
| Basic requirements (4 stats) | 40 |
| Standard requirements (4+ features) | 25 |
| Error handling | 10 |
| Code quality & organization | 10 |
| Documentation & comments | 10 |
| Reflection quality | 5 |
| **Total** | **100** |

**Bonus:** +10 for implementation in 2+ languages, +5 for advanced features

---

## Common Mistakes to Avoid

1. **Off-by-one errors** - Empty lines, trailing newlines
2. **Case sensitivity** - "The" and "the" should be same word
3. **Punctuation** - "word." vs "word" - consider stripping punctuation
4. **Division by zero** - Empty file edge case
5. **Not closing files** - Use `with` in Python, RAII in C++

---

## Extension Ideas

If you finish early:
- GUI version (tkinter for Python)
- Web interface
- Support for multiple files
- Export statistics to JSON/CSV
- Visualization (bar chart of word frequencies)
- Compare two files

---

Good luck, and have fun with your first project!
