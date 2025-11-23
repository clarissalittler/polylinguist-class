# Project 1: Text Statistics Tool

**Difficulty**: ⭐⭐ Easy-Medium
**Time Estimate**: 3-5 hours
**Prerequisites**: Lessons 1-3 (Hello World, Variables, Control Flow)

## Project Description

Build a command-line tool that analyzes text files and reports statistics.

### Example Usage

```bash
$ python text_stats.py sample.txt
=== Text Statistics ===
File: sample.txt
Lines: 42
Words: 287
Characters: 1,543
Average word length: 5.4
Longest word: "programming"
Most common word: "the" (23 times)
```

## Requirements

### Core Features (Required)

1. **Read a text file** from command-line argument
2. **Count lines** in the file
3. **Count words** in the file
4. **Count characters** (including spaces)
5. **Calculate average word length**
6. **Find the longest word**
7. **Display results** in a readable format

### Intermediate Features (Recommended)

8. **Count unique words**
9. **Find most common word** and its frequency
10. **Count sentences** (periods, question marks, exclamation points)
11. **Handle multiple files** (show stats for each)

### Advanced Features (Optional)

12. **Word frequency histogram** (top 10 words)
13. **Reading level analysis** (average sentence length)
14. **Export to CSV** or JSON
15. **Handle different encodings** (UTF-8, ASCII)

## Implementation Requirements

### Implement in at least 2 languages from different paradigms:

**Recommended combinations:**
- **Imperative**: Python or JavaScript
- **Systems**: C or Rust
- **Functional**: Haskell or Racket

**Compare:**
- Which language made file I/O easier?
- Which had better string manipulation?
- Which was more concise?

## Technical Specifications

### Input

- File path as command-line argument
- Should handle files that don't exist gracefully

### Output

- Print statistics to console
- Use clear formatting
- Include file name in output

### Error Handling

- File not found: Print helpful error message
- Empty file: Report 0 for all statistics
- Invalid encoding: Try to handle or report error

## Sample Test Files

Create test files with known statistics:

**test1.txt** (simple):
```
Hello world
This is a test
```
Expected: 2 lines, 6 words, 28 characters

**test2.txt** (complex):
```
The quick brown fox jumps over the lazy dog.
The dog was very lazy.
```
Expected: 2 lines, 13 words, 2 sentences, "the" appears 3 times

## Step-by-Step Guide

### Step 1: Read the file
```python
# Python example
with open(filename, 'r') as f:
    content = f.read()
```

### Step 2: Count lines
```python
lines = content.split('\n')
line_count = len(lines)
```

### Step 3: Count words
```python
words = content.split()
word_count = len(words)
```

### Step 4: Calculate statistics
```python
# Average word length
total_length = sum(len(word) for word in words)
avg_length = total_length / word_count if word_count > 0 else 0

# Longest word
longest = max(words, key=len) if words else ""
```

### Step 5: Format output
```python
print(f"=== Text Statistics ===")
print(f"Lines: {line_count}")
print(f"Words: {word_count}")
# ... etc
```

## Language-Specific Hints

### Python
- Use `split()` for words
- Use `Counter` from collections for word frequency
- String methods: `lower()`, `strip()`, etc.

### JavaScript
- Use `fs.readFileSync()` to read files
- Use `split()` and array methods
- Consider `Map` for word frequency

### C
- Use `fopen()`, `fgets()`, `fclose()`
- Use `strtok()` for splitting words
- Manual string manipulation required

### Haskell
- Use `readFile` for I/O
- `words` function splits on whitespace
- Use `group`, `sort` for frequency
- Pattern matching on lists

### Rust
- Use `std::fs::read_to_string()`
- Use `split_whitespace()` for words
- Use `HashMap` for frequency counting
- Handle errors with `Result`

## Extensions

### Extension 1: Detailed Word Analysis
- Count words by length (1-letter words, 2-letter words, etc.)
- Find palindromes
- Identify all-caps words

### Extension 2: Comparison Mode
- Compare two files
- Show which has more words, longer average, etc.
- Show unique words in each file

### Extension 3: Interactive Mode
- Prompt for file name if not provided
- Menu for different statistics
- Option to analyze multiple files

### Extension 4: Visualization
- ASCII bar chart of word frequencies
- Distribution of word lengths
- Line length visualization

## Grading Rubric (25 points)

| Criteria | Points | Description |
|----------|--------|-------------|
| File Reading | 3 | Successfully reads and handles files |
| Core Statistics | 8 | Lines, words, chars, avg length, longest word |
| Most Common Word | 4 | Correctly finds most frequent word |
| Code Quality | 4 | Clean, readable, well-organized |
| Error Handling | 2 | Gracefully handles errors |
| Multi-Language | 2 | Implemented in 2+ languages |
| Documentation | 2 | README with usage instructions |

## Reflection Questions

After completing the project, consider:

1. **Which language was easiest for this task? Why?**

2. **How did you handle word frequency counting?**
   - Dictionary/HashMap?
   - Sorting and grouping?
   - Something else?

3. **What was the hardest part?**
   - File I/O?
   - String manipulation?
   - Algorithm design?

4. **If you implemented in multiple languages, what differed most?**

5. **What would you do differently with more time?**

## Example Solutions

Solutions are provided in the `solutions/` directory for reference.

**Don't look until you've tried yourself!**

## Real-World Applications

This project teaches skills used in:
- Log file analysis
- Data preprocessing
- Document analysis
- Natural language processing
- Search engine indexing

## Next Steps

After completing this project:
- Add more features
- Try in a new language
- Move on to Project 2 (Number Guessing Game)
- Combine with Project 3 to analyze todo lists!

---

**Good luck!** This project teaches fundamental skills you'll use in every language.
