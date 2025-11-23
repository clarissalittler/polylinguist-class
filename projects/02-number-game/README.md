# Project 2: Number Guessing Game

**Difficulty**: ⭐⭐ Easy-Medium
**Time Estimate**: 2-4 hours
**Prerequisites**: Lessons 1-3 (Hello World, Variables, Control Flow)

## Project Description

Build an interactive number guessing game where the computer picks a random number and the player tries to guess it.

### Example Gameplay

```
=== Number Guessing Game ===
I'm thinking of a number between 1 and 100.
You have 7 guesses. Good luck!

Guess #1: 50
Too high!

Guess #2: 25
Too low!

Guess #3: 37
Too low!

Guess #4: 43
Too high!

Guess #5: 40
Correct! You won in 5 guesses!
```

## Requirements

### Core Features (Required)

1. **Generate random number** between 1 and 100
2. **Get user input** for guesses
3. **Provide feedback**: "Too high", "Too low", or "Correct"
4. **Count guesses** and display at the end
5. **End game** when number is guessed

### Intermediate Features (Recommended)

6. **Limit guesses** (e.g., 7 attempts maximum)
7. **Play again** option after game ends
8. **Difficulty levels**:
   - Easy: 1-50, 10 guesses
   - Medium: 1-100, 7 guesses
   - Hard: 1-1000, 10 guesses
9. **Input validation** (handle non-numbers gracefully)

### Advanced Features (Optional)

10. **High scores** (fewest guesses to win)
11. **Hint system** (use a hint, lose a guess)
12. **Computer guesses** your number (reverse mode)
13. **Two-player mode** (players take turns)

## Implementation Requirements

### Implement in at least 2 languages:

**Recommended:**
- **Python**: Easy user input, clean syntax
- **JavaScript**: Async I/O practice
- **Haskell**: Interesting I/O challenge!

**Compare:**
- How does each language handle random numbers?
- How does user input differ?
- Which was most fun to implement?

## Technical Specifications

### Game Flow

1. Display welcome message
2. Generate random number
3. Loop:
   - Prompt for guess
   - Validate input
   - Provide feedback
   - Check win condition
4. Display results
5. Offer to play again

### Random Number Generation

**Python:**
```python
import random
secret = random.randint(1, 100)
```

**JavaScript:**
```javascript
const secret = Math.floor(Math.random() * 100) + 1;
```

**Haskell:**
```haskell
import System.Random
gen <- getStdGen
let (secret, _) = randomR (1, 100) gen
```

### Input Handling

**Python:**
```python
guess = int(input("Guess: "))
```

**JavaScript (Node.js):**
```javascript
const readline = require('readline');
// ... setup interface
```

**C:**
```c
int guess;
scanf("%d", &guess);
```

## Step-by-Step Guide

### Part 1: Basic Game

**Step 1**: Generate random number
```python
import random
secret = random.randint(1, 100)
```

**Step 2**: Get user input
```python
guess = int(input("Your guess: "))
```

**Step 3**: Compare and give feedback
```python
if guess < secret:
    print("Too low!")
elif guess > secret:
    print("Too high!")
else:
    print("Correct!")
```

**Step 4**: Add a loop
```python
guesses = 0
while True:
    guesses += 1
    guess = int(input("Your guess: "))

    if guess == secret:
        print(f"Correct in {guesses} guesses!")
        break
    elif guess < secret:
        print("Too low!")
    else:
        print("Too high!")
```

### Part 2: Add Guess Limit

```python
MAX_GUESSES = 7
guesses = 0

while guesses < MAX_GUESSES:
    guesses += 1
    print(f"Guess #{guesses}/{MAX_GUESSES}: ", end="")
    guess = int(input())

    if guess == secret:
        print(f"You won in {guesses} guesses!")
        break
    elif guess < secret:
        print("Too low!")
    else:
        print("Too high!")
else:
    print(f"Out of guesses! The number was {secret}.")
```

### Part 3: Input Validation

```python
while True:
    try:
        guess = int(input("Your guess: "))
        if guess < 1 or guess > 100:
            print("Please guess between 1 and 100")
            continue
        break
    except ValueError:
        print("Please enter a valid number")
```

## Language-Specific Hints

### Python
- `random.randint(a, b)` for random numbers
- `input()` for user input
- Try/except for validation
- `while True:` with `break` for game loop

### JavaScript (Node.js)
- `Math.random()` for random numbers
- `readline` module for input
- Promise-based or callback-based I/O
- Consider using `prompt-sync` package for easier input

### C
- `rand()` and `srand(time(NULL))` for random
- `scanf()` for input
- Check `scanf()` return value for validation
- `while` loop for game

### Haskell
- `System.Random` for random numbers
- `getLine` for input
- `read` to parse strings to integers
- Recursive function or `replicateM` for game loop

### Rust
- `rand` crate for random numbers
- `std::io::stdin().read_line()` for input
- `parse()` with error handling
- `loop` with `break` for game flow

## Extensions

### Extension 1: Strategy Analysis

After the game, analyze the player's guessing strategy:
- Did they use binary search?
- How efficient were their guesses?
- Show optimal guessing path

### Extension 2: Computer Player

Implement AI that guesses YOUR number:
- You pick a number
- Computer guesses using binary search
- You tell computer if guess is correct/high/low
- See how few guesses computer needs!

**Challenge**: Catch cheating! If responses are inconsistent, call out the player.

### Extension 3: Multiplayer

- Two players take turns guessing
- Each has limited guesses
- First to guess wins
- Track wins across multiple rounds

### Extension 4: GUI Version

- Add a simple graphical interface
- Buttons for guessing
- Visual feedback (colors for hot/cold)
- History of guesses shown

## Grading Rubric (25 points)

| Criteria | Points | Description |
|----------|--------|-------------|
| Random Number | 2 | Generates random number correctly |
| Input/Output | 3 | Gets input, displays output |
| Game Logic | 6 | Correct feedback (high/low/correct) |
| Guess Counting | 2 | Tracks and displays guess count |
| Guess Limit | 3 | Enforces maximum guesses |
| Input Validation | 3 | Handles invalid input gracefully |
| Play Again | 2 | Option to replay |
| Code Quality | 2 | Clean, readable code |
| Multi-Language | 2 | Implemented in 2+ languages |

## Algorithms to Explore

### Binary Search Strategy

The optimal strategy is binary search:
1. Guess middle of range
2. Eliminate half the possibilities
3. Repeat

For 1-100, this guarantees finding the number in ≤7 guesses.

**Challenge**: Implement a computer player that uses binary search!

## Testing Checklist

- [ ] Game starts and displays instructions
- [ ] Random number is actually random (play multiple times)
- [ ] Feedback is correct for all cases
- [ ] Win condition triggers correctly
- [ ] Loss condition triggers at guess limit
- [ ] Can play multiple games in a row
- [ ] Handles non-numeric input gracefully
- [ ] Handles out-of-range input gracefully

## Reflection Questions

1. **How did you handle the game loop?**
   - While loop?
   - Recursion?
   - Something else?

2. **What was trickiest about user input?**
   - Different in each language?
   - Validation?
   - Parsing?

3. **Did you implement binary search AI? How?**

4. **If you made this in multiple languages, which was easiest? Why?**

5. **What would make this game more fun?**

## Real-World Skills

This project teaches:
- **Game loops**: Core concept in game development
- **User input**: Needed for interactive programs
- **Validation**: Critical for robust software
- **Random numbers**: Used in simulations, games, testing
- **State management**: Tracking game state

## Next Steps

After completing:
- Add more features (leaderboard, hints, etc.)
- Try in a new language
- Make a GUI version
- Move on to Project 3 (Todo List)

---

**This is a classic first project!** Every programmer should build a guessing game at least once.
