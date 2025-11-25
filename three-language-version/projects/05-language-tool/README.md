# Project 5: Language Tool

**Quarter 2, Weeks 5-7**
**Prerequisites:** Lessons 1-9 (through Pattern Matching)

---

## The Big Idea

Build a tool that **works with language**—human language, programming language, or any structured text. Process, analyze, transform, or generate text in useful ways.

Text processing is everywhere: compilers, search engines, spell checkers, translators, formatters, linters. This project connects you to that world.

---

## Base Requirements

Your language tool must:

1. **Process text input** (from file or user)
2. **Analyze or transform** the text in a meaningful way
3. **Produce useful output** (analysis results, transformed text, etc.)
4. **Handle edge cases** gracefully
5. **Be genuinely useful** for some task

### Core Features
- Clear input/output
- Non-trivial text processing
- Works on realistic inputs
- Helpful error messages

---

## Project Ideas

### Text Analysis
- **Readability scorer**: Flesch-Kincaid, Gunning Fog, etc.
- **Sentiment analyzer**: Positive/negative/neutral text
- **Plagiarism detector**: Find similar passages
- **Writing style analyzer**: Sentence variety, word choice
- **Language identifier**: Detect what language text is in

### Text Transformation
- **Markdown processor**: Convert markdown to HTML
- **Text formatter**: Wrap, justify, format paragraphs
- **Case converter**: camelCase ↔ snake_case ↔ kebab-case
- **Template engine**: Fill in placeholders with data
- **Obfuscator**: Encode/decode text (cipher, pig latin)

### Language Learning
- **Vocabulary builder**: Extract unique words, definitions
- **Flashcard generator**: Create study materials from text
- **Pronunciation guide**: Add phonetic transcriptions
- **Grammar checker**: Find common errors

### Code Tools
- **Comment stripper**: Remove comments from code
- **Code formatter**: Indent, align, beautify
- **Documentation generator**: Extract doc comments
- **Syntax highlighter**: Output code with color codes
- **Simple linter**: Check for common issues

### Creative Tools
- **Text generator**: Markov chain, mad libs
- **Poetry helper**: Rhyme finder, syllable counter
- **Name generator**: Character names, place names
- **Lorem ipsum generator**: Customized placeholder text

### Parsing
- **Config file parser**: INI, YAML-like, custom format
- **Log analyzer**: Parse and summarize log files
- **CSV transformer**: Query and transform CSV data
- **Simple expression parser**: Math expressions

---

## Example: Readability Analyzer

```
$ python readability.py document.txt

═══════════════════════════════════════════════════════════════
              READABILITY ANALYSIS
═══════════════════════════════════════════════════════════════

INPUT: document.txt (2,847 words)

BASIC STATISTICS
  Sentences: 142
  Words: 2,847
  Characters: 15,234
  Syllables: 4,521

  Average sentence length: 20.1 words
  Average word length: 5.4 characters
  Average syllables per word: 1.6

READABILITY SCORES
  Flesch Reading Ease: 52.3 (Fairly Difficult)
  Flesch-Kincaid Grade: 10.2 (Tenth Grade)
  Gunning Fog Index: 12.4 (High School Senior)
  SMOG Index: 11.1 (Eleventh Grade)
  Coleman-Liau Index: 11.8 (Twelfth Grade)

INTERPRETATION
  This text is appropriate for: High school students and above
  Target audience: Academic or professional readers

SUGGESTIONS
  • Some sentences exceed 30 words - consider splitting
  • 23% of words have 3+ syllables - may be complex
  • Good paragraph variety

DETAILED BREAKDOWN
  Shortest sentence: 4 words
  Longest sentence: 47 words ("The implementation of...")
  Most common long word: "implementation" (12 occurrences)

[Options: --verbose for sentence-by-sentence, --compare FILE]
```

---

## Example: Case Converter

```
$ ./case_convert --help

CASE CONVERTER
Convert between naming conventions

USAGE:
  case_convert [input] --to [format]

FORMATS:
  snake_case      lower_with_underscores
  camelCase       lowerCamelCase
  PascalCase      UpperCamelCase
  kebab-case      lower-with-dashes
  SCREAMING_SNAKE UPPER_WITH_UNDERSCORES
  Title Case      Capitalize Each Word

EXAMPLES:
  case_convert "hello world" --to camelCase
  → helloWorld

  case_convert myVariableName --to snake_case
  → my_variable_name

  case_convert "THE_CONSTANT" --to PascalCase
  → TheConstant

$ case_convert getUserById --to snake_case
get_user_by_id

$ case_convert "process HTTP request" --to PascalCase
ProcessHttpRequest

$ echo "some_function_name" | case_convert --to camelCase
someFunctionName
```

---

## Example: Markov Text Generator

```
$ python markov.py corpus.txt --generate 100

Training on corpus.txt (50,000 words)...
Building 2-gram model...
Done! Vocabulary: 8,234 unique words

═══════════════════════════════════════════════════════════════
              GENERATED TEXT (100 words)
═══════════════════════════════════════════════════════════════

The old house stood silently at the edge of memory, where
dreams collect like autumn leaves. She wondered if anyone
remembered the summer they spent counting stars and making
promises neither could keep. Time moves differently in small
towns, he thought, watching the familiar streets transform
under winter's first snow. The coffee shop on Main Street
still served the same bitter brew, though the faces had
changed. Nothing stays the same, yet everything feels
exactly as it was, suspended in that peculiar amber of
nostalgia.

[Commands: 'again' for new text, 'seed WORD' to start with word]
> seed library

The library was empty except for dust motes dancing in
afternoon light. Books lined the walls like patient friends...
```

---

## Technical Requirements

### Text Processing
- Handle different encodings (UTF-8)
- Process files of reasonable size
- Deal with edge cases (empty input, unusual characters)

### Pattern Matching
Use pattern matching, regular expressions, or parsing techniques:
- String operations (split, find, replace)
- Regex where appropriate
- Character-by-character processing if needed

### Code Organization
- Separate parsing from analysis from output
- Functions for different operations
- Clear data flow

### User Interface
- Clear input/output format
- Helpful usage instructions
- Meaningful error messages

---

## Creative Extensions

### More Sophisticated Analysis (+5 each)
- **N-gram analysis**: Common phrases, not just words
- **Part-of-speech tagging**: Nouns, verbs, etc.
- **Named entity recognition**: Find names, places, dates
- **Topic modeling**: What is this text about?

### Multiple Formats (+5 each)
- **Batch processing**: Handle multiple files
- **Streaming**: Process as you read
- **Different output formats**: JSON, HTML, plain text

### Interactivity (+5 each)
- **Interactive mode**: REPL for exploration
- **Live preview**: Show changes as you type
- **Undo/history**: Track transformations

### Integration (+5 each)
- **Pipe-friendly**: Work with Unix pipes
- **Configuration file**: Save preferences
- **Plugin system**: Extensible analysis

### Racket Bonus (+10)
- Implement in Racket
- Use S-expressions for data representation
- Reflect on Lisp's text-processing heritage

---

## Getting Started

### Step 1: Choose Your Tool
What text processing task interests you? What would be useful?

### Step 2: Define Input/Output
What goes in? What comes out? Write example interactions.

### Step 3: Handle Simple Cases
Get basic functionality working on simple inputs first.

### Step 4: Add Sophistication
Handle more complex cases, add features.

### Step 5: Polish
Good error messages, help text, edge cases.

---

## Language Hints

### Python
```python
import re

def analyze_readability(text):
    sentences = re.split(r'[.!?]+', text)
    words = text.split()
    syllables = sum(count_syllables(w) for w in words)

    # Flesch Reading Ease
    score = 206.835 - 1.015 * (len(words) / len(sentences)) \
                    - 84.6 * (syllables / len(words))
    return score

def count_syllables(word):
    # Simple syllable counter
    word = word.lower()
    vowels = "aeiouy"
    count = 0
    prev_vowel = False
    for char in word:
        is_vowel = char in vowels
        if is_vowel and not prev_vowel:
            count += 1
        prev_vowel = is_vowel
    return max(1, count)
```

### C++
```cpp
#include <regex>
#include <string>
#include <vector>

std::vector<std::string> split(const std::string& text,
                               const std::regex& pattern) {
    std::sregex_token_iterator it(text.begin(), text.end(),
                                  pattern, -1);
    return {it, {}};
}

std::string convertCase(const std::string& input,
                        CaseStyle from, CaseStyle to) {
    auto words = splitIntoWords(input, from);
    return joinWords(words, to);
}
```

### Haskell
```haskell
import Data.Char (toLower, isAlpha)
import Data.List (group, sort)

wordFrequency :: String -> [(String, Int)]
wordFrequency text =
    map (\ws -> (head ws, length ws))
    . group
    . sort
    . map (map toLower . filter isAlpha)
    . words
    $ text

-- Pattern matching on text structure
analyze :: String -> Analysis
analyze text = Analysis
    { sentences = splitSentences text
    , wordCount = length (words text)
    , uniqueWords = length . nub . words $ text
    }
```

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What tool did you build and why?** What problem does it solve?

2. **What text processing techniques did you use?** Regex? Pattern matching? Parsing?

3. **What edge cases surprised you?** Language is messy—what tripped you up?

4. **How did you test your tool?** What inputs did you try?

5. **Where could pattern matching (Haskell style) improve your code?**

6. **What would a professional version of this tool have?**

---

## Submission Checklist

- [ ] Tool processes text input correctly
- [ ] Produces useful analysis or transformation
- [ ] Handles edge cases gracefully
- [ ] Clear usage instructions
- [ ] Uses appropriate text processing techniques
- [ ] Git history shows development
- [ ] REFLECTION.md completed
- [ ] Ready for showcase!

---

## Showcase Presentation

5-7 minutes:
1. **What and why** (1-2 min) - What tool? What problem does it solve?
2. **Live demo** (3-4 min) - Show it working on various inputs
3. **Technical highlights** (1-2 min) - Interesting processing techniques, challenges

---

**Build something that makes working with text easier!**
