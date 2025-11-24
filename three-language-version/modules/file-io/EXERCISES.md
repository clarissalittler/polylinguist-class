# File I/O: Practice Exercises

## Exercise 1: Basic File Reading

### 1a. Read and Count (All Languages)

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

<details>
<summary>Solution (C++)</summary>

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

void analyzeFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Cannot open " << filename << std::endl;
        return;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();

    int lineCount = 0;
    int wordCount = 0;
    int charCount = content.length();

    std::string line;
    std::istringstream contentStream(content);
    while (std::getline(contentStream, line)) {
        lineCount++;
        std::istringstream lineStream(line);
        std::string word;
        while (lineStream >> word) {
            wordCount++;
        }
    }

    std::cout << "Lines: " << lineCount << std::endl;
    std::cout << "Words: " << wordCount << std::endl;
    std::cout << "Characters: " << charCount << std::endl;
}

int main() {
    analyzeFile("sample.txt");
    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
import System.IO
import System.Directory

analyzeFile :: FilePath -> IO ()
analyzeFile path = do
    exists <- doesFileExist path
    if not exists
        then putStrLn $ "Error: " ++ path ++ " not found"
        else do
            content <- readFile path
            let lns = lines content
                ws = words content
                chars = length content

            putStrLn $ "Lines: " ++ show (length lns)
            putStrLn $ "Words: " ++ show (length ws)
            putStrLn $ "Characters: " ++ show chars

main :: IO ()
main = analyzeFile "sample.txt"
```
</details>

---

### 1b. Find and Replace (All Languages)

Write a program that:
1. Reads a file
2. Replaces all occurrences of a word
3. Writes the result to a new file

<details>
<summary>Solution (Python)</summary>

```python
def replace_in_file(input_file, output_file, old_word, new_word):
    try:
        with open(input_file, 'r') as infile:
            content = infile.read()

        new_content = content.replace(old_word, new_word)

        with open(output_file, 'w') as outfile:
            outfile.write(new_content)

        print(f"Replaced '{old_word}' with '{new_word}'")
        print(f"Output saved to {output_file}")

    except FileNotFoundError:
        print(f"Error: {input_file} not found")

replace_in_file('input.txt', 'output.txt', 'old_word', 'new_word')
```
</details>

<details>
<summary>Solution (C++)</summary>

```cpp
#include <fstream>
#include <string>
#include <iostream>

void replaceInFile(const std::string& input, const std::string& output,
                   const std::string& oldWord, const std::string& newWord) {
    std::ifstream infile(input);
    if (!infile.is_open()) {
        std::cerr << "Error: Cannot open " << input << std::endl;
        return;
    }

    std::stringstream buffer;
    buffer << infile.rdbuf();
    std::string content = buffer.str();
    infile.close();

    size_t pos = 0;
    while ((pos = content.find(oldWord, pos)) != std::string::npos) {
        content.replace(pos, oldWord.length(), newWord);
        pos += newWord.length();
    }

    std::ofstream outfile(output);
    outfile << content;
    outfile.close();

    std::cout << "Replaced '" << oldWord << "' with '" << newWord << "'" << std::endl;
    std::cout << "Output saved to " << output << std::endl;
}

int main() {
    replaceInFile("input.txt", "output.txt", "old_word", "new_word");
    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

replaceInFile :: FilePath -> FilePath -> String -> String -> IO ()
replaceInFile input output oldWord newWord = do
    content <- TIO.readFile input
    let replaced = T.replace (T.pack oldWord) (T.pack newWord) content
    TIO.writeFile output replaced
    putStrLn $ "Replaced '" ++ oldWord ++ "' with '" ++ newWord ++ "'"
    putStrLn $ "Output saved to " ++ output

main :: IO ()
main = replaceInFile "input.txt" "output.txt" "old_word" "new_word"
```
</details>

---

## Exercise 2: CSV Processing

### 2a. CSV Reader (All Languages)

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

        total_grade = sum(int(s['grade']) for s in students)
        avg_grade = total_grade / len(students)

        top_student = max(students, key=lambda s: int(s['grade']))

        print(f"Average grade: {avg_grade:.2f}")
        print(f"Top student: {top_student['name']} ({top_student['grade']})")

    except FileNotFoundError:
        print(f"Error: {filename} not found")

analyze_students('students.csv')
```
</details>

<details>
<summary>Solution (C++)</summary>

```cpp
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>

struct Student {
    std::string name;
    int grade;
    int age;
};

std::vector<Student> readStudents(const std::string& filename) {
    std::vector<Student> students;
    std::ifstream file(filename);
    std::string line;

    std::getline(file, line);  // Skip header

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        Student s;
        std::string gradeStr, ageStr;

        std::getline(ss, s.name, ',');
        std::getline(ss, gradeStr, ',');
        std::getline(ss, ageStr, ',');

        s.grade = std::stoi(gradeStr);
        s.age = std::stoi(ageStr);
        students.push_back(s);
    }

    return students;
}

int main() {
    auto students = readStudents("students.csv");

    double total = 0;
    for (const auto& s : students) {
        total += s.grade;
    }
    double avg = total / students.size();

    auto top = std::max_element(students.begin(), students.end(),
        [](const Student& a, const Student& b) { return a.grade < b.grade; });

    std::cout << "Average grade: " << avg << std::endl;
    std::cout << "Top student: " << top->name << " (" << top->grade << ")" << std::endl;

    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
import Data.List.Split (splitOn)
import Data.List (maximumBy)
import Data.Ord (comparing)

data Student = Student
    { name :: String
    , grade :: Int
    , age :: Int
    } deriving Show

parseStudent :: String -> Student
parseStudent line =
    let [n, g, a] = splitOn "," line
    in Student n (read g) (read a)

analyzeStudents :: FilePath -> IO ()
analyzeStudents path = do
    content <- readFile path
    let lns = lines content
        students = map parseStudent (tail lns)  -- Skip header
        avgGrade = fromIntegral (sum $ map grade students) /
                   fromIntegral (length students)
        topStudent = maximumBy (comparing grade) students

    putStrLn $ "Average grade: " ++ show avgGrade
    putStrLn $ "Top student: " ++ name topStudent ++
               " (" ++ show (grade topStudent) ++ ")"

main :: IO ()
main = analyzeStudents "students.csv"
```
</details>

---

### 2b. CSV Filter (All Languages)

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

<details>
<summary>Solution (C++)</summary>

```cpp
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <iostream>

struct Product {
    std::string name;
    double price;
    int stock;
};

void filterLowStock(const std::string& input, const std::string& output, int threshold = 20) {
    std::ifstream infile(input);
    std::ofstream outfile(output);
    std::string line;

    // Copy header
    std::getline(infile, line);
    outfile << line << "\n";

    while (std::getline(infile, line)) {
        std::stringstream ss(line);
        std::string name, priceStr, stockStr;

        std::getline(ss, name, ',');
        std::getline(ss, priceStr, ',');
        std::getline(ss, stockStr, ',');

        int stock = std::stoi(stockStr);

        if (stock < threshold) {
            outfile << line << "\n";
        }
    }

    std::cout << "Low stock items saved to " << output << std::endl;
}

int main() {
    filterLowStock("products.csv", "low_stock.csv");
    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
import Data.List.Split (splitOn)

data Product = Product
    { productName :: String
    , price :: Double
    , stock :: Int
    } deriving Show

parseProduct :: String -> Product
parseProduct line =
    let [n, p, s] = splitOn "," line
    in Product n (read p) (read s)

unparseProduct :: Product -> String
unparseProduct (Product n p s) =
    n ++ "," ++ show p ++ "," ++ show s

filterLowStock :: FilePath -> FilePath -> Int -> IO ()
filterLowStock input output threshold = do
    content <- readFile input
    let lns = lines content
        header = head lns
        products = map parseProduct (tail lns)
        lowStock = filter (\p -> stock p < threshold) products
        outputLines = header : map unparseProduct lowStock

    writeFile output (unlines outputLines)
    putStrLn $ "Low stock items saved to " ++ output

main :: IO ()
main = filterLowStock "products.csv" "low_stock.csv" 20
```
</details>

---

## Exercise 3: JSON Processing

### 3a. JSON Reader (All Languages)

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

<details>
<summary>Solution (C++)</summary>

```cpp
#include <nlohmann/json.hpp>
#include <fstream>
#include <iostream>
#include <vector>

using json = nlohmann::json;

int main() {
    std::ifstream file("users.json");
    json data = json::parse(file);

    auto users = data["users"];
    int activeCount = 0;
    double totalAge = 0;
    std::vector<std::string> activeNames;

    for (const auto& user : users) {
        totalAge += user["age"].get<int>();
        if (user["active"].get<bool>()) {
            activeCount++;
            activeNames.push_back(user["name"].get<std::string>());
        }
    }

    double avgAge = totalAge / users.size();

    std::cout << "Total users: " << users.size() << std::endl;
    std::cout << "Active users: " << activeCount << std::endl;
    std::cout << "Average age: " << avgAge << std::endl;
    std::cout << "Active user names: ";
    for (const auto& name : activeNames) {
        std::cout << name << " ";
    }
    std::cout << std::endl;

    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data User = User
    { name :: String
    , age :: Int
    , active :: Bool
    } deriving (Generic, Show)

instance FromJSON User

data UserData = UserData
    { users :: [User]
    } deriving (Generic, Show)

instance FromJSON UserData

analyzeUsers :: FilePath -> IO ()
analyzeUsers path = do
    jsonData <- B.readFile path
    case decode jsonData of
        Nothing -> putStrLn "Error: Invalid JSON format"
        Just (UserData us) -> do
            let activeUsers = filter active us
                avgAge = fromIntegral (sum $ map age us) /
                        fromIntegral (length us)
                activeNames = map name activeUsers

            putStrLn $ "Total users: " ++ show (length us)
            putStrLn $ "Active users: " ++ show (length activeUsers)
            putStrLn $ "Average age: " ++ show avgAge
            putStrLn $ "Active user names: " ++ show activeNames

main :: IO ()
main = analyzeUsers "users.json"
```
</details>

---

## Exercise 4: Command-Line Programs

### 4a. Word Counter Tool (All Languages)

Create a command-line tool that counts words in a file:

```bash
# Python
python word_counter.py myfile.txt
python word_counter.py myfile.txt -w "hello"  # Count specific word

# C++
./word_counter myfile.txt
./word_counter myfile.txt hello

# Haskell
./word_counter myfile.txt
./word_counter myfile.txt hello
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
            for w, count in word_counts.most_common(10):
                print(f"  {w}: {count}")

    except FileNotFoundError:
        print(f"Error: {filename} not found")

def main():
    parser = argparse.ArgumentParser(description='Count words in a file')
    parser.add_argument('filename', help='File to analyze')
    parser.add_argument('-w', '--word', help='Specific word to count')
    parser.add_argument('-i', '--case-insensitive', action='store_true')

    args = parser.parse_args()
    count_words(args.filename, args.word, args.case_insensitive)

if __name__ == '__main__':
    main()
```
</details>

<details>
<summary>Solution (C++)</summary>

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <sstream>
#include <algorithm>

void countWords(const std::string& filename, const std::string& targetWord = "") {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Cannot open " << filename << std::endl;
        return;
    }

    std::map<std::string, int> wordCounts;
    std::string word;
    int totalWords = 0;

    while (file >> word) {
        totalWords++;
        wordCounts[word]++;
    }

    if (!targetWord.empty()) {
        int count = wordCounts[targetWord];
        std::cout << "'" << targetWord << "' appears " << count << " times" << std::endl;
    } else {
        std::cout << "Total words: " << totalWords << std::endl;
        std::cout << "Unique words: " << wordCounts.size() << std::endl;
    }
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <filename> [word]" << std::endl;
        return 1;
    }

    std::string filename = argv[1];
    std::string word = (argc > 2) ? argv[2] : "";

    countWords(filename, word);
    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
import System.Environment
import System.Directory
import Data.List (group, sort)
import qualified Data.Map as M

countWords :: FilePath -> Maybe String -> IO ()
countWords path maybeWord = do
    exists <- doesFileExist path
    if not exists
        then putStrLn $ "Error: " ++ path ++ " not found"
        else do
            content <- readFile path
            let ws = words content
                totalWords = length ws
                wordCounts = M.fromListWith (+) [(w, 1) | w <- ws]

            case maybeWord of
                Just targetWord -> do
                    let count = M.findWithDefault 0 targetWord wordCounts
                    putStrLn $ "'" ++ targetWord ++ "' appears " ++ show count ++ " times"
                Nothing -> do
                    putStrLn $ "Total words: " ++ show totalWords
                    putStrLn $ "Unique words: " ++ show (M.size wordCounts)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> countWords filename Nothing
        [filename, word] -> countWords filename (Just word)
        _ -> putStrLn "Usage: program <filename> [word]"
```
</details>

---

## Exercise 5: Directory Operations

### 5a. File Finder (All Languages)

Write a program that:
1. Takes a directory path and file extension
2. Recursively finds all files with that extension
3. Prints the full paths

<details>
<summary>Solution (Python)</summary>

```python
import os
from pathlib import Path

def find_files(directory, extension):
    """Find all files with given extension in directory tree."""
    path = Path(directory)

    if not path.exists():
        print(f"Error: {directory} does not exist")
        return

    if not path.is_dir():
        print(f"Error: {directory} is not a directory")
        return

    files = list(path.rglob(f'*{extension}'))

    print(f"Found {len(files)} files with extension '{extension}':")
    for file in files:
        print(f"  {file.absolute()}")

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 3:
        print("Usage: python find_files.py <directory> <extension>")
    else:
        find_files(sys.argv[1], sys.argv[2])
```
</details>

<details>
<summary>Solution (C++)</summary>

```cpp
#include <filesystem>
#include <iostream>
#include <vector>

namespace fs = std::filesystem;

std::vector<fs::path> findFiles(const fs::path& directory, const std::string& extension) {
    std::vector<fs::path> results;

    if (!fs::exists(directory)) {
        std::cerr << "Error: Directory does not exist" << std::endl;
        return results;
    }

    for (const auto& entry : fs::recursive_directory_iterator(directory)) {
        if (fs::is_regular_file(entry) && entry.path().extension() == extension) {
            results.push_back(entry.path());
        }
    }

    return results;
}

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <directory> <extension>" << std::endl;
        return 1;
    }

    std::string directory = argv[1];
    std::string extension = argv[2];

    auto files = findFiles(directory, extension);

    std::cout << "Found " << files.size() << " files with extension '" << extension << "':" << std::endl;
    for (const auto& file : files) {
        std::cout << "  " << fs::absolute(file) << std::endl;
    }

    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
import System.Directory
import System.FilePath
import Control.Monad (filterM, forM)

findFiles :: FilePath -> String -> IO [FilePath]
findFiles dir ext = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents

    files <- filterM doesFileExist fullPaths
    dirs <- filterM doesDirectoryExist fullPaths

    let matchingFiles = filter (\f -> takeExtension f == ext) files

    subFiles <- forM dirs $ \d -> findFiles d ext
    return $ matchingFiles ++ concat subFiles

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir, ext] -> do
            files <- findFiles dir ext
            putStrLn $ "Found " ++ show (length files) ++ " files with extension '" ++ ext ++ "':"
            mapM_ (\f -> putStrLn $ "  " ++ f) files
        _ -> putStrLn "Usage: program <directory> <extension>"
```
</details>

---

## Exercise 6: Real-World Project

### 6a. Configuration Manager (All Languages)

Create a configuration manager that:
1. Loads configuration from JSON file
2. Provides default values if file doesn't exist
3. Allows updating configuration
4. Saves configuration back to file

**Requirements:**
- Handle missing files gracefully
- Validate configuration values
- Pretty-print JSON output

<details>
<summary>Solution (Python)</summary>

```python
import json
from pathlib import Path

class ConfigManager:
    def __init__(self, config_file='config.json'):
        self.config_file = config_file
        self.config = self.load()

    def load(self):
        """Load configuration from file or return defaults."""
        if not Path(self.config_file).exists():
            return self.get_defaults()

        try:
            with open(self.config_file, 'r') as f:
                return json.load(f)
        except json.JSONDecodeError:
            print("Warning: Invalid config file, using defaults")
            return self.get_defaults()

    def get_defaults(self):
        """Return default configuration."""
        return {
            'database': {
                'host': 'localhost',
                'port': 5432,
                'name': 'mydb'
            },
            'logging': {
                'level': 'INFO',
                'file': 'app.log'
            },
            'features': {
                'debug': False,
                'cache': True
            }
        }

    def get(self, path, default=None):
        """Get config value using dot notation."""
        keys = path.split('.')
        value = self.config
        try:
            for key in keys:
                value = value[key]
            return value
        except (KeyError, TypeError):
            return default

    def set(self, path, value):
        """Set config value using dot notation."""
        keys = path.split('.')
        config = self.config
        for key in keys[:-1]:
            if key not in config:
                config[key] = {}
            config = config[key]
        config[keys[-1]] = value

    def save(self):
        """Save configuration to file."""
        with open(self.config_file, 'w') as f:
            json.dump(self.config, f, indent=2)
        print(f"Configuration saved to {self.config_file}")

# Example usage
if __name__ == '__main__':
    config = ConfigManager()

    # Get values
    db_host = config.get('database.host')
    print(f"Database host: {db_host}")

    # Set values
    config.set('database.port', 3306)
    config.set('features.debug', True)

    # Save
    config.save()
```
</details>

<details>
<summary>Solution (C++)</summary>

```cpp
#include <nlohmann/json.hpp>
#include <fstream>
#include <iostream>
#include <filesystem>

using json = nlohmann::json;
namespace fs = std::filesystem;

class ConfigManager {
private:
    std::string configFile;
    json config;

    json getDefaults() {
        return json{
            {"database", {
                {"host", "localhost"},
                {"port", 5432},
                {"name", "mydb"}
            }},
            {"logging", {
                {"level", "INFO"},
                {"file", "app.log"}
            }},
            {"features", {
                {"debug", false},
                {"cache", true}
            }}
        };
    }

public:
    ConfigManager(const std::string& file = "config.json")
        : configFile(file) {
        load();
    }

    void load() {
        if (!fs::exists(configFile)) {
            config = getDefaults();
            return;
        }

        try {
            std::ifstream f(configFile);
            config = json::parse(f);
        } catch (json::parse_error& e) {
            std::cerr << "Warning: Invalid config file, using defaults" << std::endl;
            config = getDefaults();
        }
    }

    template<typename T>
    T get(const std::string& path, const T& defaultValue) {
        try {
            auto keys = split(path, '.');
            json value = config;
            for (const auto& key : keys) {
                value = value[key];
            }
            return value.get<T>();
        } catch (...) {
            return defaultValue;
        }
    }

    template<typename T>
    void set(const std::string& path, const T& value) {
        auto keys = split(path, '.');
        json* current = &config;

        for (size_t i = 0; i < keys.size() - 1; ++i) {
            if (!current->contains(keys[i])) {
                (*current)[keys[i]] = json::object();
            }
            current = &(*current)[keys[i]];
        }

        (*current)[keys.back()] = value;
    }

    void save() {
        std::ofstream f(configFile);
        f << config.dump(2);
        std::cout << "Configuration saved to " << configFile << std::endl;
    }

private:
    std::vector<std::string> split(const std::string& s, char delimiter) {
        std::vector<std::string> tokens;
        std::string token;
        std::istringstream tokenStream(s);
        while (std::getline(tokenStream, token, delimiter)) {
            tokens.push_back(token);
        }
        return tokens;
    }
};

// Example usage
int main() {
    ConfigManager config;

    std::string dbHost = config.get<std::string>("database.host", "localhost");
    std::cout << "Database host: " << dbHost << std::endl;

    config.set("database.port", 3306);
    config.set("features.debug", true);

    config.save();

    return 0;
}
```
</details>

<details>
<summary>Solution (Haskell)</summary>

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.Directory
import Data.Maybe (fromMaybe)

data DatabaseConfig = DatabaseConfig
    { host :: String
    , port :: Int
    , name :: String
    } deriving (Generic, Show)

instance FromJSON DatabaseConfig
instance ToJSON DatabaseConfig

data LoggingConfig = LoggingConfig
    { level :: String
    , file :: String
    } deriving (Generic, Show)

instance FromJSON LoggingConfig
instance ToJSON LoggingConfig

data FeaturesConfig = FeaturesConfig
    { debug :: Bool
    , cache :: Bool
    } deriving (Generic, Show)

instance FromJSON FeaturesConfig
instance ToJSON FeaturesConfig

data Config = Config
    { database :: DatabaseConfig
    , logging :: LoggingConfig
    , features :: FeaturesConfig
    } deriving (Generic, Show)

instance FromJSON Config
instance ToJSON Config

getDefaults :: Config
getDefaults = Config
    { database = DatabaseConfig "localhost" 5432 "mydb"
    , logging = LoggingConfig "INFO" "app.log"
    , features = FeaturesConfig False True
    }

loadConfig :: FilePath -> IO Config
loadConfig path = do
    exists <- doesFileExist path
    if not exists
        then return getDefaults
        else do
            jsonData <- B.readFile path
            return $ fromMaybe getDefaults (decode jsonData)

saveConfig :: FilePath -> Config -> IO ()
saveConfig path config = do
    B.writeFile path (encodePretty config)
    putStrLn $ "Configuration saved to " ++ path

-- Example usage
main :: IO ()
main = do
    config <- loadConfig "config.json"

    putStrLn $ "Database host: " ++ host (database config)

    let updatedConfig = config
            { database = (database config) { port = 3306 }
            , features = (features config) { debug = True }
            }

    saveConfig "config.json" updatedConfig
```
</details>

---

## Challenge Exercises

### Challenge 1: File Backup Tool

Create a tool that backs up files:
- Copy files from source to backup directory
- Add timestamp to backup filenames
- Skip files that haven't changed (compare modification time)
- Generate backup report (JSON)

### Challenge 2: Log Merger

Create a program that:
- Takes multiple log files as input
- Merges them by timestamp
- Filters by log level (ERROR, WARNING, INFO)
- Outputs sorted, merged log file

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
- ✅ Read and write text files in Python, C++, and Haskell
- ✅ Process CSV and JSON data
- ✅ Handle file errors gracefully across paradigms
- ✅ Work with file paths cross-platform
- ✅ Use command-line arguments
- ✅ Build practical file-processing tools in multiple languages
- ✅ Understand paradigm differences (imperative vs functional)

**Remember:** File I/O is how programs interact with persistent data!
