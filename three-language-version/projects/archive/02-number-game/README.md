# Project 2: Number Guessing Game

**Difficulty:** Easy-Medium
**Estimated Time:** 2-4 hours
**Prerequisites:** Lessons 1-4 (through Functions)
**Due:** End of Week 5

---

## Overview

Create an interactive number guessing game where the computer picks a random number and the player tries to guess it, receiving "too high" or "too low" hints. This project reinforces control flow, user input, random number generation, and game loop design.

---

## Learning Objectives

By completing this project, you will:
- Implement game loops with exit conditions
- Handle user input and validation
- Generate random numbers
- Use conditional logic for game mechanics
- Track statistics across multiple rounds
- Design a simple interactive program

---

## Requirements

### Basic Requirements (Must Complete)

Create a game that:

1. **Generates a random number** between 1 and 100 (inclusive)
2. **Prompts the player to guess**
3. **Gives feedback**: "Too high!", "Too low!", or "Correct!"
4. **Counts guesses** and reports total when player wins
5. **Offers to play again** after a game ends

**Example Session:**
```
=== Number Guessing Game ===
I'm thinking of a number between 1 and 100.

Your guess: 50
Too high!

Your guess: 25
Too low!

Your guess: 37
Too high!

Your guess: 31
Correct! You got it in 4 guesses!

Play again? (y/n): y

I'm thinking of a number between 1 and 100.
Your guess: ...
```

### Standard Requirements (Complete Most)

Extend your game with:

5. **Input validation** - Handle non-numeric input gracefully
6. **Range hints** - Show current valid range after each guess
7. **Guess limit** - Optional mode with limited guesses (e.g., 7 tries)
8. **Statistics tracking** - Track games played, wins, average guesses

**Extended Example:**
```
Your guess: fifty
Please enter a valid number.

Your guess: 150
Out of range! Please guess between 1 and 100.

Your guess: 50
Too high! (The number is between 1 and 49)

[After game]
Statistics:
  Games played: 5
  Games won: 4
  Best game: 3 guesses
  Average guesses: 5.2
```

### Advanced Requirements (Challenge)

For additional challenge:

9. **Difficulty levels**
   - Easy: 1-50, unlimited guesses
   - Medium: 1-100, 10 guesses
   - Hard: 1-1000, 10 guesses
10. **Optimal play analysis** - Tell player if they used binary search
11. **Reverse mode** - Player picks number, computer guesses
12. **Multiplayer** - Two players alternate, closest wins
13. **High score persistence** - Save best scores to file

**Advanced Example:**
```
Select difficulty:
  1. Easy (1-50, unlimited)
  2. Medium (1-100, 10 guesses)
  3. Hard (1-1000, 10 guesses)

Choice: 3

=== HARD MODE ===
I'm thinking of a number between 1 and 1000.
You have 10 guesses. Good luck!

Guess 1/10: 500
Too low! (501-1000)

Guess 2/10: 750
Too high! (501-749)

...

You found it in 8 guesses!
Optimal would be 10 guesses (log2(1000)).
Rating: Excellent!
```

---

## Technical Specifications

### Random Number Generation

Each language handles randomness differently:

**Python:**
```python
import random
secret = random.randint(1, 100)  # Inclusive both ends
```

**C++:**
```cpp
#include <random>
#include <ctime>

std::random_device rd;
std::mt19937 gen(rd());
std::uniform_int_distribution<> dist(1, 100);
int secret = dist(gen);

// Or simpler (less random):
srand(time(nullptr));
int secret = rand() % 100 + 1;
```

**Haskell:**
```haskell
import System.Random

main :: IO ()
main = do
    secret <- randomRIO (1, 100) :: IO Int
    -- ...
```

### Input Handling

**Python:**
```python
try:
    guess = int(input("Your guess: "))
except ValueError:
    print("Please enter a valid number.")
```

**C++:**
```cpp
int guess;
std::cout << "Your guess: ";
if (!(std::cin >> guess)) {
    std::cin.clear();
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    std::cout << "Please enter a valid number." << std::endl;
}
```

**Haskell:**
```haskell
import Text.Read (readMaybe)

getGuess :: IO (Maybe Int)
getGuess = do
    putStr "Your guess: "
    input <- getLine
    return (readMaybe input)
```

---

## Game Design Tips

### The Game Loop

```
1. Initialize (generate secret number)
2. Loop:
   a. Get player input
   b. Validate input
   c. Compare to secret
   d. Give feedback
   e. Check win condition
3. Display results
4. Ask to play again
```

### Binary Search Connection

The optimal strategy for this game is binary search:
- Always guess the middle of the remaining range
- Each guess eliminates half the possibilities
- Maximum guesses for range 1-100: ceil(log2(100)) = 7

This connects to algorithm analysis lessons!

---

## Sample Game Flows

### Winning Game
```
Secret: 73

Guess: 50  -> Too low!
Guess: 75  -> Too high!
Guess: 62  -> Too low!
Guess: 68  -> Too low!
Guess: 72  -> Too low!
Guess: 74  -> Too high!
Guess: 73  -> Correct! (7 guesses)
```

### With Guess Limit (Losing)
```
Secret: 42, Limit: 5 guesses

Guess 1/5: 50  -> Too high!
Guess 2/5: 25  -> Too low!
Guess 3/5: 37  -> Too low!
Guess 4/5: 43  -> Too high!
Guess 5/5: 40  -> Too low!

Game over! The number was 42.
```

---

## Language-Specific Starter Code

### Python

```python
import random

def play_game():
    secret = random.randint(1, 100)
    guesses = 0

    print("\nI'm thinking of a number between 1 and 100.")

    while True:
        try:
            guess = int(input("Your guess: "))
        except ValueError:
            print("Please enter a valid number.")
            continue

        guesses += 1

        if guess < secret:
            print("Too low!")
        elif guess > secret:
            print("Too high!")
        else:
            print(f"Correct! You got it in {guesses} guesses!")
            return guesses

def main():
    print("=== Number Guessing Game ===")

    while True:
        play_game()
        again = input("\nPlay again? (y/n): ").lower()
        if again != 'y':
            print("Thanks for playing!")
            break

if __name__ == "__main__":
    main()
```

### C++

```cpp
#include <iostream>
#include <random>
#include <limits>

int play_game() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(1, 100);
    int secret = dist(gen);
    int guesses = 0;
    int guess;

    std::cout << "\nI'm thinking of a number between 1 and 100.\n";

    while (true) {
        std::cout << "Your guess: ";

        if (!(std::cin >> guess)) {
            std::cin.clear();
            std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            std::cout << "Please enter a valid number.\n";
            continue;
        }

        guesses++;

        if (guess < secret) {
            std::cout << "Too low!\n";
        } else if (guess > secret) {
            std::cout << "Too high!\n";
        } else {
            std::cout << "Correct! You got it in " << guesses << " guesses!\n";
            return guesses;
        }
    }
}

int main() {
    std::cout << "=== Number Guessing Game ===\n";

    char again;
    do {
        play_game();
        std::cout << "\nPlay again? (y/n): ";
        std::cin >> again;
    } while (again == 'y' || again == 'Y');

    std::cout << "Thanks for playing!\n";
    return 0;
}
```

### Haskell

```haskell
import System.Random
import Text.Read (readMaybe)
import Control.Monad (when)

playGame :: IO Int
playGame = do
    secret <- randomRIO (1, 100) :: IO Int
    putStrLn "\nI'm thinking of a number between 1 and 100."
    gameLoop secret 0

gameLoop :: Int -> Int -> IO Int
gameLoop secret guesses = do
    putStr "Your guess: "
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Please enter a valid number."
            gameLoop secret guesses
        Just guess -> do
            let newGuesses = guesses + 1
            case compare guess secret of
                LT -> do
                    putStrLn "Too low!"
                    gameLoop secret newGuesses
                GT -> do
                    putStrLn "Too high!"
                    gameLoop secret newGuesses
                EQ -> do
                    putStrLn $ "Correct! You got it in " ++ show newGuesses ++ " guesses!"
                    return newGuesses

main :: IO ()
main = do
    putStrLn "=== Number Guessing Game ==="
    mainLoop

mainLoop :: IO ()
mainLoop = do
    _ <- playGame
    putStr "\nPlay again? (y/n): "
    again <- getLine
    when (again == "y" || again == "Y") mainLoop
    putStrLn "Thanks for playing!"
```

---

## Submission Checklist

- [ ] Basic game loop works correctly
- [ ] Random number changes each game
- [ ] Correct feedback (too high/low/correct)
- [ ] Guess count is accurate
- [ ] Play again functionality works
- [ ] Input validation handles errors
- [ ] Code is well-organized
- [ ] `REFLECTION.md` completed

---

## Reflection Questions

Answer these in your `REFLECTION.md`:

1. How did you structure your game loop? What controls when it ends?
2. How does random number generation differ between languages?
3. What's the optimal strategy for this game? How many guesses should it take?
4. How did you handle invalid input? What edge cases did you consider?
5. If you added difficulty levels, how did you organize that code?

---

## Grading Rubric

| Criteria | Points |
|----------|--------|
| Basic game mechanics | 35 |
| Input validation | 15 |
| Standard features (stats, range hints) | 20 |
| Code quality & organization | 15 |
| Documentation | 10 |
| Reflection quality | 5 |
| **Total** | **100** |

**Bonus:** +10 for difficulty levels, +5 for reverse mode, +5 for high score file

---

## Common Mistakes to Avoid

1. **Same random number every run** - Seed your RNG properly (or use proper sources)
2. **Off-by-one in range** - Is 100 included? Is 1 included?
3. **Infinite loop with bad input** - Clear input buffers in C++
4. **Not resetting state** - Each game needs a new secret number
5. **Guess counter issues** - Don't count invalid guesses

---

## Extension Ideas

If you finish early:
- Graphical version (pygame, SFML, etc.)
- Timer mode (guess within time limit)
- Hints system (spend guesses for hints)
- AI opponent that plays optimally
- Network multiplayer
- Sound effects

---

Have fun building your game!
