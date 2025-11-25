# Project 2: Data Explorer

**Quarter 1, Weeks 5-7**
**Prerequisites:** Lessons 1-5 (through Data Structures)

---

## The Big Idea

Build a tool that loads a dataset and lets you **explore it interactively**. Find patterns, answer questions, and discover insights in data you find interesting.

Data is everywhere. The ability to load it, filter it, and make sense of it is one of the most valuable skills you can develop.

---

## Base Requirements

Your data explorer must:

1. **Load data** from a file (CSV, JSON, or text format)
2. **Display summary statistics** (count, basic stats)
3. **Filter/search** the data based on user criteria
4. **Show results** in a clear, readable format
5. Run without crashing on valid input

### Required Features

1. **Load**: Read data from a file into memory
2. **Summary**: Show overview (how many records, what fields exist)
3. **Search**: Find records matching a criterion
4. **Filter**: Show subset of data based on conditions
5. **Display**: Format output readably

---

## Choose Your Dataset

Pick something YOU find interesting! Options:

### Provided Datasets (in `samples/`)
- `books.csv` - 100 books with title, author, year, genre, rating
- `movies.csv` - 100 movies with title, year, genre, rating, director
- `pokemon.csv` - Pokemon with stats, types, abilities
- `countries.csv` - Country data (population, area, GDP, continent)

### Find Your Own
- Sports statistics (your favorite team/sport)
- Video game data (Steam, speedruns, etc.)
- Music (your listening history, Billboard charts)
- Anything from [Kaggle](https://www.kaggle.com/datasets) or [data.gov](https://data.gov)

**The more you care about the data, the better your project will be!**

---

## Example Interaction

```
$ python data_explorer.py books.csv

📚 BOOK DATA EXPLORER
Loaded 100 books from books.csv

COMMANDS:
  summary     - Show dataset overview
  search      - Find books by keyword
  filter      - Filter by criteria
  top         - Show top N by rating
  stats       - Show statistics
  quit        - Exit

> summary

DATASET SUMMARY
═══════════════
Total books: 100
Years: 1813 - 2023
Genres: Fiction (34), Non-Fiction (28), Sci-Fi (18), Fantasy (12), Mystery (8)
Average rating: 4.1

> search tolkien

SEARCH RESULTS: "tolkien"
══════════════════════════
1. "The Hobbit" by J.R.R. Tolkien (1937) - Fantasy - ★★★★★
2. "The Fellowship of the Ring" by J.R.R. Tolkien (1954) - Fantasy - ★★★★★
3. "The Two Towers" by J.R.R. Tolkien (1954) - Fantasy - ★★★★★

Found 3 books matching "tolkien"

> filter genre=Sci-Fi rating>4

FILTERED RESULTS: genre=Sci-Fi AND rating>4.0
═════════════════════════════════════════════
1. "Dune" by Frank Herbert (1965) - ★★★★★
2. "Neuromancer" by William Gibson (1984) - ★★★★☆
3. "The Martian" by Andy Weir (2011) - ★★★★★
...

Showing 12 of 18 Sci-Fi books with rating > 4.0

> top 5

TOP 5 BOOKS BY RATING
═════════════════════
1. ★★★★★ "1984" by George Orwell (1949)
2. ★★★★★ "To Kill a Mockingbird" by Harper Lee (1960)
3. ★★★★★ "The Hobbit" by J.R.R. Tolkien (1937)
4. ★★★★★ "Dune" by Frank Herbert (1965)
5. ★★★★★ "Pride and Prejudice" by Jane Austen (1813)

> quit
Thanks for exploring! 📖
```

---

## Technical Requirements

### Data Handling
- Parse CSV, JSON, or structured text
- Store data in appropriate structures (lists, dictionaries/maps)
- Handle missing or malformed data gracefully

### User Interface
- Clear command prompt
- Helpful error messages
- "Help" command showing available options

### Code Structure
- Separate data loading from processing from display
- Use functions for each command
- Handle edge cases (empty results, invalid commands)

### Version Control
- Regular commits showing development
- Meaningful commit messages

---

## Creative Extensions

### More Powerful Queries (+5 each)
- **Multiple conditions**: `filter year>2000 AND genre=Fantasy`
- **Sorting**: `sort by year descending`
- **Grouping**: `group by genre` with counts
- **Aggregation**: `average rating by author`

### Visualization (+5 each)
- **ASCII bar charts**: Show distributions
- **ASCII histograms**: Year distribution, rating distribution
- **Tables**: Nicely formatted columns
- **Sparklines**: Trend visualization

### Data Operations (+5 each)
- **Export results**: Save filtered data to new file
- **Add/edit records**: Modify the dataset
- **Multiple files**: Compare or combine datasets
- **Derived fields**: Calculate new columns (age, days since, etc.)

### Advanced Features (+5 each)
- **Fuzzy search**: Find "tolken" when searching for "tolkien"
- **Recommendations**: "If you liked X, try Y"
- **Random pick**: "Give me a random Sci-Fi book rated 4+"
- **Statistics**: Standard deviation, correlation

### Multi-Language Bonus (+10)
- Implement in a second core language
- Compare data handling across languages

---

## Getting Started

### Step 1: Get Your Data
Choose a dataset (provided or your own). Look at its structure.

### Step 2: Load It
Write code to read the file and store it in memory. Print the first few records to verify.

### Step 3: Summary First
Implement the summary command. This forces you to understand your data.

### Step 4: Search
Add simple keyword search across all fields.

### Step 5: Filter
Add filtering by specific field values.

### Step 6: Polish
Make the output pretty. Add more features if time allows.

---

## Language Hints

### Python
```python
import csv

def load_data(filename):
    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        return list(reader)

def search(data, keyword):
    keyword = keyword.lower()
    return [row for row in data
            if keyword in str(row.values()).lower()]

def filter_data(data, field, value):
    return [row for row in data if row[field] == value]
```

### C++
```cpp
#include <fstream>
#include <sstream>
#include <vector>
#include <map>

struct Record {
    std::map<std::string, std::string> fields;
};

std::vector<Record> loadCSV(const std::string& filename) {
    std::vector<Record> data;
    std::ifstream file(filename);
    // Parse CSV...
    return data;
}
```

### Haskell
```haskell
import Data.List (isInfixOf)
import Data.Char (toLower)

type Record = [(String, String)]  -- Field name -> value

search :: String -> [Record] -> [Record]
search keyword = filter (containsKeyword keyword)
  where
    containsKeyword kw record =
        any (isInfixOf (map toLower kw) . map toLower . snd) record
```

---

## Provided Sample Data

### books.csv
```csv
title,author,year,genre,rating
The Hobbit,J.R.R. Tolkien,1937,Fantasy,5.0
1984,George Orwell,1949,Fiction,4.8
Dune,Frank Herbert,1965,Sci-Fi,4.7
...
```

### movies.csv
```csv
title,year,genre,rating,director
The Shawshank Redemption,1994,Drama,9.3,Frank Darabont
The Godfather,1972,Crime,9.2,Francis Ford Coppola
...
```

Full sample files are in the `samples/` directory.

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What dataset did you choose and why?** What questions did you want to answer?

2. **What insights did you find?** Anything surprising in the data?

3. **How did you structure your data in code?** Why that approach?

4. **What was the hardest query to implement?** How did you solve it?

5. **What would make this tool more useful?** What features would you add?

---

## Submission Checklist

- [ ] Loads data from file successfully
- [ ] Shows summary/overview of data
- [ ] Search functionality works
- [ ] Filter functionality works
- [ ] Clear, readable output
- [ ] Handles errors gracefully
- [ ] Git history shows progress
- [ ] REFLECTION.md completed
- [ ] Ready to demo at showcase!

---

## Showcase Presentation

5-7 minutes:
1. **Show your data** (1 min) - What dataset? Why?
2. **Demo features** (3-4 min) - Walk through searches, filters, discoveries
3. **Share insights** (1-2 min) - What did you learn from the data AND from building this?

---

**The goal: Build a tool that helps you understand data you care about!**
