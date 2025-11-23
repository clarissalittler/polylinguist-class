# Module: File I/O and System Interaction

## Learning Objectives

By the end of this module, you will be able to:

1. Read and write text files in Python, C++, and Haskell
2. Handle file paths and directories correctly
3. Process CSV and JSON data files
4. Handle I/O errors gracefully
5. Work with binary files
6. Use command-line arguments
7. Redirect standard input/output/error
8. Apply best practices for file operations across paradigms

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
- `with` statement ensures file is closed automatically (context manager)
- `'r'` mode = read (default)
- Always close files when done (or use `with`)

### C++: Reading Files

```cpp
#include <iostream>
#include <fstream>
#include <string>

// Method 1: Read entire file into string (C++17)
#include <sstream>
std::ifstream file("data.txt");
std::stringstream buffer;
buffer << file.rdbuf();
std::string content = buffer.str();
file.close();

// Method 2: Read line by line
std::ifstream file("data.txt");
std::string line;
while (std::getline(file, line)) {
    std::cout << line << std::endl;
}
file.close();

// Method 3: Read word by word
std::ifstream file("data.txt");
std::string word;
while (file >> word) {
    std::cout << word << std::endl;
}
file.close();

// Method 4: Using RAII (recommended)
{
    std::ifstream file("data.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(file, line)) {
        std::cout << line << std::endl;
    }
    // File automatically closed when leaving scope
}
```

**Key points:**
- Always check if file opened successfully (`is_open()` or check stream state)
- Use RAII: file closes automatically when object goes out of scope
- `std::ifstream` for input, `std::ofstream` for output

### Haskell: Reading Files

```haskell
-- Method 1: Read entire file (lazy I/O)
import System.IO

main = do
    content <- readFile "data.txt"
    putStrLn content

-- Method 2: Read with explicit handle
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    content <- hGetContents handle
    putStrLn content
    hClose handle

-- Method 3: Read lines into list
main = do
    content <- readFile "data.txt"
    let lns = lines content
    mapM_ putStrLn lns

-- Method 4: Strict I/O (avoid space leaks)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main = do
    content <- TIO.readFile "data.txt"
    TIO.putStrLn content

-- Method 5: Using bracket for safe resource management
import System.IO
import Control.Exception (bracket)

main = bracket
    (openFile "data.txt" ReadMode)  -- acquire resource
    hClose                           -- release resource
    (\handle -> do                   -- use resource
        content <- hGetContents handle
        putStrLn content)
```

**Key points:**
- `readFile` uses lazy I/O (file read on demand)
- Use `Data.Text` for strict evaluation to avoid space leaks
- `bracket` ensures file is closed even if exceptions occur
- Pattern: acquire resource → use resource → release resource

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

### C++: Writing Files

```cpp
#include <fstream>
#include <string>

// Method 1: Write to file (overwrites)
std::ofstream file("output.txt");
file << "Hello, World!" << std::endl;
file << "This is line 2" << std::endl;
file.close();

// Method 2: Append to file
std::ofstream file("output.txt", std::ios::app);
file << "Appended line" << std::endl;
file.close();

// Method 3: Using RAII with automatic close
{
    std::ofstream file("output.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file for writing!" << std::endl;
        return 1;
    }
    file << "Hello, World!" << std::endl;
    // File automatically closed here
}

// Method 4: Write multiple lines
#include <vector>
std::vector<std::string> lines = {"Line 1", "Line 2", "Line 3"};
std::ofstream file("output.txt");
for (const auto& line : lines) {
    file << line << std::endl;
}
file.close();
```

### Haskell: Writing Files

```haskell
-- Method 1: Write string to file (overwrites)
main = writeFile "output.txt" "Hello, World!\nLine 2\n"

-- Method 2: Append to file
main = appendFile "output.txt" "Appended line\n"

-- Method 3: Write with explicit handle
import System.IO

main = do
    handle <- openFile "output.txt" WriteMode
    hPutStrLn handle "Hello, World!"
    hPutStrLn handle "Line 2"
    hClose handle

-- Method 4: Write lines from list
main = do
    let lns = ["Line 1", "Line 2", "Line 3"]
    writeFile "output.txt" (unlines lns)

-- Method 5: Safe writing with bracket
import System.IO
import Control.Exception (bracket)

main = bracket
    (openFile "output.txt" WriteMode)
    hClose
    (\handle -> do
        hPutStrLn handle "Hello, World!"
        hPutStrLn handle "Line 2")

-- Method 6: Strict text writing
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main = TIO.writeFile "output.txt" (T.pack "Hello, World!")
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

# Walk directory tree
for root, dirs, files in os.walk('.'):
    for filename in files:
        print(os.path.join(root, filename))
```

### C++: Path Handling (C++17 filesystem)

```cpp
#include <filesystem>
#include <iostream>

namespace fs = std::filesystem;

// Create path objects
fs::path path = "data.txt";
fs::path abs_path = fs::absolute(path);

// Check existence
bool exists = fs::exists(path);
bool is_file = fs::is_regular_file(path);
bool is_dir = fs::is_directory(path);

// Join paths (cross-platform)
fs::path data_dir = "data";
fs::path file_path = data_dir / "myfile.txt";

// Create directories
fs::create_directory("new_folder");
fs::create_directories("parent/child/grandchild");

// List files in directory
for (const auto& entry : fs::directory_iterator(".")) {
    std::cout << entry.path() << std::endl;
}

// Recursive directory iteration
for (const auto& entry : fs::recursive_directory_iterator(".")) {
    if (fs::is_regular_file(entry)) {
        std::cout << entry.path() << std::endl;
    }
}

// Get file size
std::uintmax_t size = fs::file_size("data.txt");

// Copy and move files
fs::copy("source.txt", "dest.txt");
fs::rename("old.txt", "new.txt");

// Remove files
fs::remove("file.txt");
fs::remove_all("directory");  // Remove directory and contents
```

### Haskell: Path Handling

```haskell
-- Using System.Directory
import System.Directory
import System.FilePath

main = do
    -- Check existence
    exists <- doesFileExist "data.txt"
    dirExists <- doesDirectoryExist "mydir"

    -- Get absolute path
    absPath <- makeAbsolute "data.txt"

    -- Join paths (cross-platform)
    let filePath = "data" </> "myfile.txt"

    -- Create directory
    createDirectory "new_folder"

    -- Create nested directories
    createDirectoryIfMissing True "parent/child/grandchild"

    -- List directory contents
    contents <- listDirectory "."
    mapM_ putStrLn contents

    -- Get full paths
    files <- getDirectoryContents "."
    mapM_ putStrLn files

    -- Copy and move files
    copyFile "source.txt" "dest.txt"
    renameFile "old.txt" "new.txt"

    -- Remove files and directories
    removeFile "file.txt"
    removeDirectory "mydir"
    removeDirectoryRecursive "mydir"  -- Remove with contents

-- Filter files by extension
import System.Directory
import System.FilePath
import Control.Monad (filterM)

getTextFiles :: FilePath -> IO [FilePath]
getTextFiles dir = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents
    files <- filterM doesFileExist fullPaths
    return $ filter (\f -> takeExtension f == ".txt") files
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

### C++: Exception Handling

```cpp
#include <fstream>
#include <iostream>
#include <stdexcept>

// Method 1: Check stream state
std::ifstream file("data.txt");
if (!file.is_open()) {
    std::cerr << "Error: Cannot open file!" << std::endl;
    return 1;
}

// Method 2: Enable exceptions
std::ifstream file;
file.exceptions(std::ifstream::failbit | std::ifstream::badbit);

try {
    file.open("data.txt");
    std::string content;
    // Read file...
    file.close();
} catch (std::ifstream::failure& e) {
    std::cerr << "Exception opening/reading file: " << e.what() << std::endl;
}

// Method 3: Using filesystem to check first
#include <filesystem>
namespace fs = std::filesystem;

std::string readFileSafe(const std::string& filename) {
    if (!fs::exists(filename)) {
        throw std::runtime_error("File does not exist");
    }

    if (!fs::is_regular_file(filename)) {
        throw std::runtime_error("Not a regular file");
    }

    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file");
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}
```

### Haskell: Error Handling

```haskell
-- Method 1: Using Maybe
import System.IO
import System.Directory
import Control.Exception

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path = do
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            return (Just content)
        else return Nothing

-- Method 2: Using Either for error messages
readFileSafeEither :: FilePath -> IO (Either String String)
readFileSafeEither path = do
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            return (Right content)
        else return (Left $ "File not found: " ++ path)

-- Method 3: Using exceptions (try/catch)
import Control.Exception
import System.IO.Error

readFileWithCatch :: FilePath -> IO (Maybe String)
readFileWithCatch path =
    (Just <$> readFile path) `catch` handler
  where
    handler :: IOException -> IO (Maybe String)
    handler e
        | isDoesNotExistError e = do
            putStrLn "File does not exist"
            return Nothing
        | isPermissionError e = do
            putStrLn "Permission denied"
            return Nothing
        | otherwise = do
            putStrLn $ "Error: " ++ show e
            return Nothing

-- Method 4: Using tryJust for specific errors
import Control.Exception
import System.IO.Error

readFileHandleErrors :: FilePath -> IO String
readFileHandleErrors path = do
    result <- tryJust selectErrors (readFile path)
    case result of
        Left errMsg -> error errMsg
        Right content -> return content
  where
    selectErrors e
        | isDoesNotExistError e = Just "File not found"
        | isPermissionError e = Just "Permission denied"
        | otherwise = Nothing
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

### C++: CSV Processing

```cpp
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <iostream>

// Simple CSV parser
std::vector<std::vector<std::string>> readCSV(const std::string& filename) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        std::vector<std::string> row;
        std::stringstream ss(line);
        std::string cell;

        while (std::getline(ss, cell, ',')) {
            row.push_back(cell);
        }
        data.push_back(row);
    }

    return data;
}

// Write CSV
void writeCSV(const std::string& filename,
              const std::vector<std::vector<std::string>>& data) {
    std::ofstream file(filename);

    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); ++i) {
            file << row[i];
            if (i < row.size() - 1) {
                file << ",";
            }
        }
        file << "\n";
    }
}

// Example usage
int main() {
    // Write CSV
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
        {"Alice", "30", "NYC"},
        {"Bob", "25", "LA"}
    };
    writeCSV("output.csv", data);

    // Read CSV
    auto csv_data = readCSV("data.csv");
    for (const auto& row : csv_data) {
        for (const auto& cell : row) {
            std::cout << cell << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}
```

### Haskell: CSV Processing

```haskell
-- Using cassava library for CSV
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (FromNamedRecord, ToNamedRecord, (.:), (.=))
import GHC.Generics (Generic)

-- Define record type
data Person = Person
    { name :: String
    , age :: Int
    , city :: String
    } deriving (Generic, Show)

instance FromNamedRecord Person
instance ToNamedRecord Person

-- Read CSV
readPersonsCSV :: FilePath -> IO (Either String [Person])
readPersonsCSV path = do
    csvData <- BL.readFile path
    case Csv.decodeByName csvData of
        Left err -> return $ Left err
        Right (_, rows) -> return $ Right $ V.toList rows

-- Write CSV
writePersonsCSV :: FilePath -> [Person] -> IO ()
writePersonsCSV path persons = do
    let csvData = Csv.encodeByName header persons
    BL.writeFile path csvData
  where
    header = V.fromList ["name", "age", "city"]

-- Manual CSV parsing (without library)
import Data.List.Split (splitOn)

parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . lines

writeCSV :: FilePath -> [[String]] -> IO ()
writeCSV path rows = writeFile path (unlines $ map (intercalate ",") rows)
  where intercalate sep = concat . intersperse sep

-- Example usage
main = do
    -- Write CSV manually
    let rows = [["Name", "Age", "City"],
                ["Alice", "30", "NYC"],
                ["Bob", "25", "LA"]]
    writeCSV "output.csv" rows

    -- Read CSV manually
    content <- readFile "data.csv"
    let csvData = parseCSV content
    mapM_ print csvData
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

### C++: JSON Processing

```cpp
// Using nlohmann/json library (header-only)
#include <nlohmann/json.hpp>
#include <fstream>
#include <iostream>

using json = nlohmann::json;

int main() {
    // Reading JSON
    std::ifstream file("data.json");
    json data = json::parse(file);

    std::cout << data["name"] << std::endl;
    std::cout << data["age"] << std::endl;

    // Writing JSON
    json output;
    output["name"] = "Alice";
    output["age"] = 30;
    output["scores"] = {95, 87, 92};
    output["active"] = true;

    std::ofstream outfile("output.json");
    outfile << output.dump(2);  // 2 = indent level

    // Accessing arrays
    for (auto& score : data["scores"]) {
        std::cout << score << std::endl;
    }

    // Iterating objects
    for (auto& [key, value] : data.items()) {
        std::cout << key << ": " << value << std::endl;
    }

    return 0;
}
```

### Haskell: JSON Processing

```haskell
-- Using aeson library
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

-- Define data type
data Person = Person
    { name :: String
    , age :: Int
    , scores :: [Int]
    , active :: Bool
    } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

-- Read JSON
readPersonJSON :: FilePath -> IO (Maybe Person)
readPersonJSON path = do
    jsonData <- B.readFile path
    return $ decode jsonData

-- Write JSON
writePersonJSON :: FilePath -> Person -> IO ()
writePersonJSON path person = do
    B.writeFile path (encode person)

-- Pretty print JSON
writePrettyJSON :: FilePath -> Person -> IO ()
writePrettyJSON path person = do
    B.writeFile path (encodePretty person)

-- Manual JSON parsing
import Data.Aeson (Value(..), Object, (.:))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

parseManual :: Value -> Maybe (String, Int)
parseManual (Object obj) = do
    name <- HM.lookup "name" obj >>= getString
    age <- HM.lookup "age" obj >>= getInt
    return (name, age)
  where
    getString (String s) = Just (T.unpack s)
    getString _ = Nothing
    getInt (Number n) = Just (round n)
    getInt _ = Nothing
parseManual _ = Nothing

-- Example usage
main = do
    -- Write JSON
    let person = Person "Alice" 30 [95, 87, 92] True
    writePersonJSON "output.json" person

    -- Read JSON
    maybePerson <- readPersonJSON "data.json"
    case maybePerson of
        Just p -> print p
        Nothing -> putStrLn "Failed to parse JSON"
```

---

## Part 7: Command-Line Arguments

### Python: sys.argv and argparse

```python
import sys

# Basic: sys.argv
print(f"Script name: {sys.argv[0]}")
print(f"Number of arguments: {len(sys.argv) - 1}")

for i, arg in enumerate(sys.argv[1:], 1):
    print(f"Argument {i}: {arg}")

# Better: argparse (Recommended)
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

### C++: Command-Line Arguments

```cpp
#include <iostream>
#include <string>
#include <vector>

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <input> <output>" << std::endl;
        return 1;
    }

    std::string input_file = argv[1];
    std::string output_file = argv[2];

    std::cout << "Input: " << input_file << std::endl;
    std::cout << "Output: " << output_file << std::endl;

    // Process optional flags
    bool verbose = false;
    for (int i = 3; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-v" || arg == "--verbose") {
            verbose = true;
        }
    }

    if (verbose) {
        std::cout << "Verbose mode enabled" << std::endl;
    }

    return 0;
}

// Compile: g++ program.cpp -o program
// Run: ./program input.txt output.txt -v
```

### Haskell: Command-Line Arguments

```haskell
-- Basic: System.Environment
import System.Environment

main = do
    args <- getArgs
    progName <- getProgName

    putStrLn $ "Program: " ++ progName
    putStrLn $ "Arguments: " ++ show args

    case args of
        [input, output] -> do
            putStrLn $ "Input: " ++ input
            putStrLn $ "Output: " ++ output
        _ -> putStrLn "Usage: program <input> <output>"

-- Better: optparse-applicative library
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { inputFile :: String
    , outputFile :: String
    , verbose :: Bool
    , count :: Int
    } deriving Show

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str (metavar "INPUT" <> help "Input file")
    <*> argument str (metavar "OUTPUT" <> help "Output file")
    <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
    <*> option auto (long "count" <> short 'n' <> value 10 <> help "Number of items")

main :: IO ()
main = do
    opts <- execParser $ info (optionsParser <**> helper)
        (fullDesc <> progDesc "Process some files")

    putStrLn $ "Input: " ++ inputFile opts
    putStrLn $ "Output: " ++ outputFile opts
    when (verbose opts) $ putStrLn "Verbose mode enabled"
    putStrLn $ "Count: " ++ show (count opts)

-- Compile: ghc program.hs
-- Run: ./program input.txt output.txt -v -n 20
```

---

## Part 8: Standard I/O Streams

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

# Example: Filter lines containing a keyword
import sys

keyword = sys.argv[1] if len(sys.argv) > 1 else "error"

for line in sys.stdin:
    if keyword.lower() in line.lower():
        print(line, end='')

# Usage: cat log.txt | python filter.py error
```

### C++: Standard Streams

```cpp
#include <iostream>
#include <string>

int main() {
    // Read from stdin
    std::string line;
    std::cout << "Enter your name: ";
    std::getline(std::cin, line);
    std::cout << "Hello, " << line << "!" << std::endl;

    // Read all lines from stdin (for piping)
    while (std::getline(std::cin, line)) {
        std::cout << "Received: " << line << std::endl;
    }

    // Write to stdout
    std::cout << "This goes to stdout" << std::endl;

    // Write to stderr
    std::cerr << "This is an error message" << std::endl;

    return 0;
}

// Compile and run with piping
// g++ program.cpp -o program
// cat data.txt | ./program
// ./program < input.txt > output.txt 2> errors.txt
```

### Haskell: Standard Streams

```haskell
-- Read from stdin
import System.IO

main = do
    putStr "Enter your name: "
    hFlush stdout  -- Ensure prompt appears
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"

-- Read all lines from stdin
main = do
    contents <- getContents  -- Lazy reading
    mapM_ putStrLn (lines contents)

-- Write to stderr
import System.IO

main = do
    hPutStrLn stdout "Normal output"
    hPutStrLn stderr "Error message"

-- Example: Filter program
main = do
    args <- getArgs
    let keyword = if null args then "error" else head args
    contents <- getContents
    let matching = filter (isInfixOf keyword . map toLower) (lines contents)
    mapM_ putStrLn matching

-- Usage: cat log.txt | ./filter error
```

### Redirection and Piping

```bash
# Redirect input from file
python script.py < input.txt
./program < input.txt

# Redirect output to file
python script.py > output.txt
./program > output.txt

# Redirect errors to file
python script.py 2> errors.txt
./program 2> errors.txt

# Redirect both output and errors
python script.py > output.txt 2>&1
./program > output.txt 2>&1

# Pipe output to another program
cat data.txt | python process.py | sort > results.txt
cat data.txt | ./program | sort > results.txt
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

### C++: Binary File I/O

```cpp
#include <fstream>
#include <vector>

// Write binary data
int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5};

    std::ofstream file("data.bin", std::ios::binary);
    file.write(reinterpret_cast<const char*>(numbers.data()),
               numbers.size() * sizeof(int));
    file.close();

    // Read binary data
    std::ifstream infile("data.bin", std::ios::binary);
    std::vector<int> buffer(5);
    infile.read(reinterpret_cast<char*>(buffer.data()),
                buffer.size() * sizeof(int));
    infile.close();

    for (int num : buffer) {
        std::cout << num << " ";
    }

    return 0;
}

// Write/read structs
struct Person {
    char name[50];
    int age;
    float height;
};

void writePerson(const std::string& filename, const Person& person) {
    std::ofstream file(filename, std::ios::binary);
    file.write(reinterpret_cast<const char*>(&person), sizeof(Person));
}

Person readPerson(const std::string& filename) {
    Person person;
    std::ifstream file(filename, std::ios::binary);
    file.read(reinterpret_cast<char*>(&person), sizeof(Person));
    return person;
}
```

### Haskell: Binary File I/O

```haskell
-- Using Data.ByteString
import qualified Data.ByteString as B
import Data.Word

-- Write binary data
main = do
    let bytes = B.pack [72, 101, 108, 108, 111]  -- "Hello"
    B.writeFile "data.bin" bytes

-- Read binary data
main = do
    bytes <- B.readFile "data.bin"
    print $ B.unpack bytes

-- Using Data.Binary for structured data
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

data Person = Person
    { name :: String
    , age :: Int
    , height :: Float
    } deriving (Show)

instance Binary Person where
    put (Person n a h) = do
        put n
        put a
        put h

    get = do
        n <- get
        a <- get
        h <- get
        return $ Person n a h

-- Write structured binary data
writePerson :: FilePath -> Person -> IO ()
writePerson path person = BL.writeFile path (encode person)

-- Read structured binary data
readPerson :: FilePath -> IO Person
readPerson path = do
    bytes <- BL.readFile path
    return $ decode bytes

-- Example usage
main = do
    let person = Person "Alice" 30 5.6
    writePerson "person.bin" person

    person' <- readPerson "person.bin"
    print person'
```

---

## Part 10: Best Practices

### 1. Always Close Files

```python
# Python: Use 'with' statement
with open('data.txt', 'r') as file:
    content = file.read()
# File automatically closed
```

```cpp
// C++: Use RAII (automatic cleanup)
{
    std::ifstream file("data.txt");
    // Use file...
} // File automatically closed
```

```haskell
-- Haskell: Use bracket for safe resource management
bracket
    (openFile "data.txt" ReadMode)
    hClose
    (\handle -> do
        -- Use handle...
    )
```

### 2. Handle Errors Gracefully

Always check for file existence, permissions, and handle I/O errors appropriately.

### 3. Use Appropriate File Modes

- Read: `'r'` (Python), `std::ios::in` (C++), `ReadMode` (Haskell)
- Write: `'w'` (Python), `std::ios::out` (C++), `WriteMode` (Haskell)
- Append: `'a'` (Python), `std::ios::app` (C++), `AppendMode` (Haskell)

### 4. Use Path Libraries

Use `pathlib` (Python), `std::filesystem` (C++17), or `System.FilePath` (Haskell) for cross-platform path handling.

### 5. Validate Input

Check file existence, permissions, and formats before processing.

### 6. Choose Appropriate I/O Strategy

- **Python:** Context managers (`with`)
- **C++:** RAII, filesystem library
- **Haskell:** Lazy I/O for large files, strict I/O for control, `bracket` for safety

---

## Complete Example: Log File Analyzer

### Python Version

```python
#!/usr/bin/env python3
import sys
import argparse
from pathlib import Path
from collections import Counter
import json

def analyze_log_file(input_file, output_file=None, verbose=False):
    """Analyze log file and generate statistics."""
    path = Path(input_file)

    if not path.exists():
        print(f"Error: {input_file} not found", file=sys.stderr)
        return 1

    total_lines = 0
    error_count = 0
    warning_count = 0
    level_counts = Counter()

    try:
        with open(path, 'r') as file:
            for line in file:
                total_lines += 1

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

        report = {
            'total_lines': total_lines,
            'errors': error_count,
            'warnings': warning_count,
            'level_distribution': dict(level_counts)
        }

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

### C++ Version

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <filesystem>
#include <nlohmann/json.hpp>

namespace fs = std::filesystem;
using json = nlohmann::json;

struct LogStats {
    int totalLines = 0;
    int errors = 0;
    int warnings = 0;
    std::map<std::string, int> levelCounts;
};

LogStats analyzeLogFile(const std::string& inputFile, bool verbose) {
    LogStats stats;

    if (!fs::exists(inputFile)) {
        throw std::runtime_error("File not found: " + inputFile);
    }

    std::ifstream file(inputFile);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + inputFile);
    }

    std::string line;
    while (std::getline(file, line)) {
        stats.totalLines++;

        if (line.find("ERROR") != std::string::npos) {
            stats.errors++;
            stats.levelCounts["ERROR"]++;
            if (verbose) {
                std::cout << "Error on line " << stats.totalLines
                         << ": " << line << std::endl;
            }
        } else if (line.find("WARNING") != std::string::npos) {
            stats.warnings++;
            stats.levelCounts["WARNING"]++;
        } else if (line.find("INFO") != std::string::npos) {
            stats.levelCounts["INFO"]++;
        }
    }

    return stats;
}

void saveReport(const std::string& outputFile, const LogStats& stats) {
    json report;
    report["total_lines"] = stats.totalLines;
    report["errors"] = stats.errors;
    report["warnings"] = stats.warnings;
    report["level_distribution"] = stats.levelCounts;

    std::ofstream file(outputFile);
    file << report.dump(2);
    std::cout << "Report saved to " << outputFile << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <input_file> [-o output] [-v]" << std::endl;
        return 1;
    }

    std::string inputFile = argv[1];
    std::string outputFile;
    bool verbose = false;

    for (int i = 2; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-o" && i + 1 < argc) {
            outputFile = argv[++i];
        } else if (arg == "-v") {
            verbose = true;
        }
    }

    try {
        LogStats stats = analyzeLogFile(inputFile, verbose);

        if (!outputFile.empty()) {
            saveReport(outputFile, stats);
        } else {
            json report;
            report["total_lines"] = stats.totalLines;
            report["errors"] = stats.errors;
            report["warnings"] = stats.warnings;
            report["level_distribution"] = stats.levelCounts;
            std::cout << report.dump(2) << std::endl;
        }

        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
}
```

### Haskell Version

```haskell
{-# LANGUAGE DeriveGeneric #-}

import System.Environment
import System.Directory
import System.IO
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data LogStats = LogStats
    { totalLines :: Int
    , errors :: Int
    , warnings :: Int
    , levelDistribution :: M.Map String Int
    } deriving (Generic, Show)

instance ToJSON LogStats

analyzeLine :: String -> (String, Bool, Bool)
analyzeLine line
    | "ERROR" `isInfixOf` line = ("ERROR", True, False)
    | "WARNING" `isInfixOf` line = ("WARNING", False, True)
    | "INFO" `isInfixOf` line = ("INFO", False, False)
    | otherwise = ("", False, False)

analyzeLog :: FilePath -> Bool -> IO LogStats
analyzeLog path verbose = do
    exists <- doesFileExist path
    if not exists
        then error $ "File not found: " ++ path
        else do
            content <- readFile path
            let lns = lines content
                results = map analyzeLine lns
                errorCount = length $ filter (\(_, e, _) -> e) results
                warningCount = length $ filter (\(_, _, w) -> w) results
                levelCounts = foldl updateCount M.empty results

            when verbose $ do
                let errorLines = zip [1..] $ filter (\(_, e, _) -> e) results
                mapM_ (\(n, _) -> putStrLn $ "Error on line " ++ show n) errorLines

            return $ LogStats
                { totalLines = length lns
                , errors = errorCount
                , warnings = warningCount
                , levelDistribution = levelCounts
                }
  where
    updateCount m (level, _, _)
        | null level = m
        | otherwise = M.insertWith (+) level 1 m

saveReport :: FilePath -> LogStats -> IO ()
saveReport path stats = do
    B.writeFile path (encode stats)
    putStrLn $ "Report saved to " ++ path

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: program <input_file> [-o output] [-v]"
        (inputFile:rest) -> do
            let (outputFile, verbose) = parseArgs rest Nothing False
            stats <- analyzeLog inputFile verbose

            case outputFile of
                Just path -> saveReport path stats
                Nothing -> B.putStrLn $ encode stats
  where
    parseArgs [] out verb = (out, verb)
    parseArgs ("-o":path:rest) _ verb = parseArgs rest (Just path) verb
    parseArgs ("-v":rest) out _ = parseArgs rest out True
    parseArgs (_:rest) out verb = parseArgs rest out verb
```

---

## Key Takeaways

1. **Always use proper resource management**
   - Python: `with` statements
   - C++: RAII and smart pointers
   - Haskell: `bracket` and lazy I/O appropriately

2. **Handle errors gracefully** - files can be missing, inaccessible, or corrupted

3. **Use path libraries** for cross-platform compatibility

4. **Validate inputs** before processing

5. **Choose appropriate file formats**
   - CSV for tables
   - JSON for structured data
   - Plain text for simple data
   - Binary for performance/size

6. **Consider the paradigm**
   - Python: imperative, context managers
   - C++: RAII, modern filesystem library
   - Haskell: lazy I/O, pure functions, monadic I/O

---

## Next Steps

Practice file I/O by building real projects:
- Log file analyzer
- CSV data processor
- Configuration file reader
- Data backup utility
- Text file search tool

**Remember:** File I/O is how programs interact with persistent data!
