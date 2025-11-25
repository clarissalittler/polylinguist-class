# Lab 3: Control Flow Adventure

**Quarter 1, Week 3**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Control flow is how programs make decisions and repeat actions. In this lab, you'll build a simple text-based adventure game while learning conditionals and loops in all three languages.

## Objectives

By the end of this lab, you will:
- [ ] Write if/else statements in Python, C++, and Haskell
- [ ] Use loops (while/for) in Python and C++
- [ ] Understand recursion as Haskell's approach to repetition
- [ ] Build a simple interactive program

## Setup

- Partner up
- Create a folder: `lab03-control-flow/`
- Have terminals ready for all three languages

---

## Part 1: Decision Making (25 minutes)

### Activity 1.1: Simple Conditionals

Write a program that asks for a number and tells the user if it's positive, negative, or zero.

**Python** (`sign.py`):
```python
num = int(input("Enter a number: "))

if num > 0:
    print("Positive!")
elif num < 0:
    print("Negative!")
else:
    print("Zero!")
```

**C++** (`sign.cpp`):
```cpp
#include <iostream>
using namespace std;

int main() {
    int num;
    cout << "Enter a number: ";
    cin >> num;

    if (num > 0) {
        cout << "Positive!" << endl;
    } else if (num < 0) {
        cout << "Negative!" << endl;
    } else {
        cout << "Zero!" << endl;
    }

    return 0;
}
```

**Haskell** (`sign.hs`):
```haskell
main :: IO ()
main = do
    putStr "Enter a number: "
    input <- getLine
    let num = read input :: Int
    putStrLn $ checkSign num

checkSign :: Int -> String
checkSign n
    | n > 0     = "Positive!"
    | n < 0     = "Negative!"
    | otherwise = "Zero!"
```

**Run all three and test with:** -5, 0, 42

### Activity 1.2: Multiple Conditions

Extend your program to also check if the number is even or odd.

**Example output:**
```
Enter a number: 7
Positive and Odd!
```

**Hints:**
- Use `%` (modulo) to check even/odd
- Python: `num % 2 == 0`
- C++: `num % 2 == 0`
- Haskell: `mod n 2 == 0` or `even n`

### ✅ Checkpoint 1

Show your partner:
- [ ] Sign checker works in all 3 languages
- [ ] Even/odd checker added to at least one language

---

## Part 2: Loops (25 minutes)

### Activity 2.1: Counting Loop

Print numbers 1 to 10.

**Python:**
```python
# Using for loop
for i in range(1, 11):
    print(i)

# Using while loop
i = 1
while i <= 10:
    print(i)
    i += 1
```

**C++:**
```cpp
#include <iostream>
using namespace std;

int main() {
    // Using for loop
    for (int i = 1; i <= 10; i++) {
        cout << i << endl;
    }

    // Using while loop
    int j = 1;
    while (j <= 10) {
        cout << j << endl;
        j++;
    }

    return 0;
}
```

**Haskell** (uses recursion or list functions):
```haskell
-- Using list printing
main :: IO ()
main = mapM_ print [1..10]

-- Or with explicit recursion
printNumbers :: Int -> Int -> IO ()
printNumbers current end
    | current > end = return ()
    | otherwise = do
        print current
        printNumbers (current + 1) end

-- main = printNumbers 1 10
```

### Activity 2.2: Sum Calculator

Calculate the sum of numbers from 1 to N (user input).

**Python:**
```python
n = int(input("Enter N: "))
total = 0
for i in range(1, n + 1):
    total += i
print(f"Sum: {total}")
```

**Your task:** Implement in C++ and Haskell.

**Haskell hint:** You can use `sum [1..n]` or write a recursive function.

### Activity 2.3: Loop Patterns

Create these patterns using loops:

**Pattern 1 - Counting:**
```
1
1 2
1 2 3
1 2 3 4
1 2 3 4 5
```

**Pattern 2 - Stars:**
```
*
**
***
****
*****
```

Implement at least one pattern in each language.

### ✅ Checkpoint 2

Show your partner:
- [ ] Sum calculator works
- [ ] At least one pattern implemented in 2 languages

---

## Part 3: Build an Adventure Game (30 minutes)

Now let's combine everything into a simple text adventure!

### Activity 3.1: The Basic Game

Create a simple adventure where the player makes choices.

**Python** (`adventure.py`):
```python
def game():
    print("=" * 40)
    print("WELCOME TO THE CAVE OF DECISIONS")
    print("=" * 40)
    print()

    print("You stand at the entrance of a dark cave.")
    print("Do you:")
    print("1. Enter the cave")
    print("2. Walk away")

    choice = input("> ")

    if choice == "1":
        enter_cave()
    elif choice == "2":
        print("You wisely decide to leave. THE END.")
    else:
        print("Invalid choice. A rock falls on your head. THE END.")

def enter_cave():
    print()
    print("You enter the dark cave...")
    print("You see two tunnels ahead.")
    print("1. Take the left tunnel (you hear water)")
    print("2. Take the right tunnel (you see a faint glow)")

    choice = input("> ")

    if choice == "1":
        print()
        print("You find an underground river!")
        print("There's a treasure chest on the other side.")
        print("1. Try to swim across")
        print("2. Look for another way")

        choice = input("> ")
        if choice == "1":
            print("You swim across and find GOLD! YOU WIN!")
        else:
            print("You find a bridge and safely get the treasure! YOU WIN!")

    elif choice == "2":
        print()
        print("The glow is from glowing mushrooms!")
        print("They're very valuable. You collect them and leave rich!")
        print("YOU WIN!")

    else:
        print("You trip in the dark. THE END.")

if __name__ == "__main__":
    game()
```

### Activity 3.2: Add a Loop

Modify the game so players can play again:

```python
def main():
    while True:
        game()
        print()
        again = input("Play again? (y/n): ")
        if again.lower() != 'y':
            print("Thanks for playing!")
            break

if __name__ == "__main__":
    main()
```

### Activity 3.3: Port to Another Language

Choose C++ or Haskell and port your adventure game.

**C++ starter:**
```cpp
#include <iostream>
#include <string>
using namespace std;

void enterCave();

void game() {
    cout << "========================================" << endl;
    cout << "WELCOME TO THE CAVE OF DECISIONS" << endl;
    cout << "========================================" << endl;
    cout << endl;

    cout << "You stand at the entrance of a dark cave." << endl;
    cout << "Do you:" << endl;
    cout << "1. Enter the cave" << endl;
    cout << "2. Walk away" << endl;

    string choice;
    cout << "> ";
    cin >> choice;

    if (choice == "1") {
        enterCave();
    } else if (choice == "2") {
        cout << "You wisely decide to leave. THE END." << endl;
    } else {
        cout << "Invalid choice. A rock falls on your head. THE END." << endl;
    }
}

void enterCave() {
    // Your code here - continue the adventure!
}

int main() {
    game();
    return 0;
}
```

**Haskell starter:**
```haskell
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

game :: IO ()
game = do
    putStrLn "========================================"
    putStrLn "WELCOME TO THE CAVE OF DECISIONS"
    putStrLn "========================================"
    putStrLn ""

    putStrLn "You stand at the entrance of a dark cave."
    putStrLn "Do you:"
    putStrLn "1. Enter the cave"
    putStrLn "2. Walk away"

    choice <- prompt "> "

    case choice of
        "1" -> enterCave
        "2" -> putStrLn "You wisely decide to leave. THE END."
        _   -> putStrLn "Invalid choice. A rock falls on your head. THE END."

enterCave :: IO ()
enterCave = do
    -- Your code here - continue the adventure!
    putStrLn "You enter the cave..."

main :: IO ()
main = game
```

### ✅ Checkpoint 3

Show your partner:
- [ ] Python adventure game works with at least 3 decision points
- [ ] Play-again loop works
- [ ] Started porting to C++ or Haskell

---

## Part 4: Comparison Discussion (10 minutes)

### Discussion Questions

With your partner, discuss:

1. **Syntax differences:** What are the main syntax differences for if/else between the three languages?

2. **Loops vs recursion:** Why does Haskell prefer recursion over loops? What are the trade-offs?

3. **Input handling:** How does getting user input differ between the languages? Which felt most natural?

4. **Code organization:** How do you organize code into functions/procedures in each language?

### Fill in this comparison:

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| If/else syntax | `if x:` | `if (x) {` | guards or `if then else` |
| For loop | `for i in range():` | `for (int i; i<n; i++)` | `map`, recursion |
| While loop | `while x:` | `while (x) {` | recursion |
| User input | `input()` | `cin >>` | `getLine` |
| String comparison | `==` | `==` | `==` |

---

## Extensions (If You Finish Early)

### Extension 1: Add Inventory

Add an inventory system to your game:
```python
inventory = []

# When player finds item:
inventory.append("gold key")

# Check if player has item:
if "gold key" in inventory:
    print("You unlock the door!")
```

### Extension 2: Add Health Points

```python
health = 100

# When player takes damage:
health -= 20

# Check if player is alive:
if health <= 0:
    print("You have perished. GAME OVER.")
```

### Extension 3: Random Events

```python
import random

if random.random() < 0.5:
    print("A bat flies at your face!")
else:
    print("The path is clear.")
```

### Extension 4: FizzBuzz

Implement FizzBuzz (classic interview problem) in all three languages:
- Print numbers 1 to 100
- For multiples of 3, print "Fizz"
- For multiples of 5, print "Buzz"
- For multiples of both, print "FizzBuzz"

---

## Wrap-Up

**Key takeaways:**

1. **Conditionals** work similarly across languages but with different syntax
2. **Loops** (Python, C++) vs **recursion** (Haskell) are different approaches to repetition
3. **Pattern matching** (Haskell's `case`) is a powerful alternative to if/else chains
4. **Building something fun** helps concepts stick!

**Challenge for homework:** Expand your adventure game with at least 2 more rooms/scenarios.

**Next lab:** We'll explore functions and how to organize code better!
