# Lesson 9: Pattern Matching - Exercises

This file contains exercises for practicing pattern matching across different languages. Choose exercises appropriate for your chosen language(s).

## Level 1: Basic Pattern Matching

### Exercise 1: Days of the Week
Write a function that takes a day number (1-7) and returns the day name.
- Input: 1 → Output: "Monday"
- Input: 7 → Output: "Sunday"
- Input: 0 or >7 → Output: "Invalid day"

**Concepts**: Basic literal patterns, default cases

### Exercise 2: Boolean to String
Write a function that pattern matches on boolean values:
- Input: true → Output: "yes"
- Input: false → Output: "no"

**Concepts**: Simple value matching

### Exercise 3: Sign Function
Implement a sign function using pattern matching:
- Negative numbers → -1
- Zero → 0
- Positive numbers → 1

**Concepts**: Guards, conditional patterns

### Exercise 4: Traffic Light Colors
Create a function that returns the next traffic light color:
- red → green
- green → yellow
- yellow → red

**Concepts**: Enum-like matching, state transitions

### Exercise 5: Single Character Classification
Pattern match on a single character to classify it:
- 'a'-'z' or 'A'-'Z' → "letter"
- '0'-'9' → "digit"
- ' ', '\t', '\n' → "whitespace"
- Other → "special character"

**Concepts**: Range patterns, multiple conditions

## Level 2: Destructuring and Tuples

### Exercise 6: Point Quadrant
Given a point (x, y), determine which quadrant it's in:
- (+, +) → "Quadrant I"
- (-, +) → "Quadrant II"
- (-, -) → "Quadrant III"
- (+, -) → "Quadrant IV"
- (0, _) or (_, 0) → "On axis"

**Concepts**: Tuple patterns, guards

### Exercise 7: 2D Vector Operations
Implement pattern matching for 2D vector operations:
- Add two vectors: (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
- Detect zero vector: (0, 0) → true
- Detect unit vectors: (1, 0), (0, 1), (-1, 0), (0, -1) → true

**Concepts**: Tuple destructuring, multiple patterns

### Exercise 8: RGB Color Names
Match RGB tuples to color names:
- (255, 0, 0) → "Red"
- (0, 255, 0) → "Green"
- (0, 0, 255) → "Blue"
- (255, 255, 255) → "White"
- (0, 0, 0) → "Black"
- Other → "Custom color (R, G, B)"

**Concepts**: Exact matching, default with formatting

### Exercise 9: Card Values
Represent playing cards as (rank, suit) and implement:
- Is it a face card? (J, Q, K)
- Is it an ace?
- Is it a red card? (hearts or diamonds)
- What's its point value? (Ace=11, face cards=10, others=rank)

**Concepts**: Nested conditions, OR patterns

### Exercise 10: Coordinate Distance
Pattern match on two points to calculate special cases:
- Same point → 0
- Same x-coordinate → |y2 - y1|
- Same y-coordinate → |x2 - x1|
- Otherwise → Euclidean distance

**Concepts**: Multiple tuple patterns, guards

## Level 3: Lists and Collections

### Exercise 11: List Length Categories
Categorize lists by length using pattern matching:
- [] → "empty"
- [_] → "singleton"
- [_, _] → "pair"
- [_, _, _] → "triple"
- Longer → "list of N elements"

**Concepts**: List patterns, length matching

### Exercise 12: Safe List Operations
Implement safe versions using pattern matching:
- safeHead: returns first element or Nothing/None
- safeTail: returns all but first or Nothing/None
- safeLast: returns last element or Nothing/None
- safeInit: returns all but last or Nothing/None

**Concepts**: List destructuring, Option types

### Exercise 13: List Sum with Patterns
Calculate sum of a list using pattern matching and recursion:
- [] → 0
- [x] → x
- [x, y] → x + y (base case optimization)
- [x | rest] → x + sum(rest)

**Concepts**: Recursive patterns, base cases

### Exercise 14: Running Total
Transform a list into running totals:
- [1, 2, 3, 4] → [1, 3, 6, 10]
- Use pattern matching to destructure and accumulate

**Concepts**: Accumulator patterns, list cons

### Exercise 15: Filter with Patterns
Implement a filter function using only pattern matching:
- Filter even numbers from [1,2,3,4,5,6] → [2,4,6]
- Filter strings longer than 3 chars
- Filter Some/Just values from a list of Options

**Concepts**: Predicate patterns, list recursion

## Level 4: Algebraic Data Types

### Exercise 16: Binary Tree Operations
Define a binary tree and implement with pattern matching:
```
data Tree = Empty | Node value left right
```
- size: count nodes
- depth: maximum depth
- sum: sum all values (for number trees)
- contains: check if value exists
- leaves: count leaf nodes

**Concepts**: Recursive ADT patterns, tree traversal

### Exercise 17: Expression Calculator
Extend the expression evaluator to support:
- Sub(left, right) for subtraction
- Div(left, right) for division (handle division by zero)
- Pow(base, exponent) for exponentiation
- Pretty-print expressions: Add(Num(2), Num(3)) → "2 + 3"

**Concepts**: ADT extension, pattern exhaustiveness

### Exercise 18: Option Chaining
Implement option/maybe operations using pattern matching:
- map: transform Some(x) → Some(f(x)), None → None
- flatMap/bind: for chaining optional operations
- getOrElse: extract value or return default
- filter: keep Some(x) only if predicate is true

**Concepts**: Option patterns, composition

### Exercise 19: Result/Either Handling
Implement result handling with pattern matching:
- Divide two numbers → Ok(result) or Err("division by zero")
- Chain operations that might fail
- Collect all errors from a list of Results
- Convert all Oks to a list, fail on first Err

**Concepts**: Error handling patterns, early return

### Exercise 20: JSON-like Data
Pattern match on nested JSON-like structures:
```
{ "user": { "name": "Alice", "age": 30 }, "active": true }
```
- Extract user name
- Check if user is adult (age >= 18)
- Handle missing fields gracefully
- Pretty-print the structure

**Concepts**: Nested patterns, hash/dictionary matching

## Level 5: Advanced Patterns

### Exercise 21: Parser Combinator
Build a simple parser using pattern matching:
- Parse integers: "123" → Some(123)
- Parse pairs: "(1,2)" → Some((1, 2))
- Parse lists: "[1,2,3]" → Some([1, 2, 3])
- Handle parse errors

**Concepts**: String patterns, recursive descent

### Exercise 22: State Machine
Implement a turnstile state machine:
- States: Locked, Unlocked
- Events: Coin, Push
- Locked + Coin → Unlocked
- Unlocked + Push → Locked
- Track total coins collected

**Concepts**: State patterns, event matching

### Exercise 23: Symbolic Differentiation
Implement symbolic differentiation using pattern matching:
- d/dx (x) = 1
- d/dx (c) = 0 (constant)
- d/dx (x^n) = n * x^(n-1)
- d/dx (f + g) = f' + g'
- d/dx (f * g) = f' * g + f * g'

**Concepts**: Mathematical patterns, term rewriting

### Exercise 24: Huffman Tree Decoder
Given a Huffman tree and bit string, decode the message:
- Leaf(char) → return char
- Node(left, right) + "0..." → go left
- Node(left, right) + "1..." → go right

**Concepts**: Tree patterns, string processing

### Exercise 25: Type Checker
Implement a simple type checker using pattern matching:
```
Expr: Num(int) | Bool(bool) | If(cond, then, else) | Add(left, right)
Types: TInt | TBool
```
- Num(_) → TInt
- Bool(_) → TBool
- If(cond, t, e) → check cond is TBool, t and e match
- Add(l, r) → check both are TInt

**Concepts**: Type patterns, validation

## Level 6: Real-World Applications

### Exercise 26: HTTP Request Router
Pattern match on HTTP requests to route to handlers:
```
(GET, "/users") → listUsers()
(GET, "/users/:id") → getUser(id)
(POST, "/users") → createUser(body)
(PUT, "/users/:id") → updateUser(id, body)
(DELETE, "/users/:id") → deleteUser(id)
```

**Concepts**: String patterns, parameter extraction

### Exercise 27: Game Move Validator
For a chess/checkers game, validate moves using pattern matching:
- Piece type + from + to positions
- Valid moves for each piece type
- Check if path is clear
- Detect captures

**Concepts**: Complex domain patterns, rules

### Exercise 28: CLI Argument Parser
Parse command-line arguments:
```
["--verbose"] → { verbose: true }
["--output", "file.txt"] → { output: Some("file.txt") }
["-v"] → { verbose: true }
["-o", "file.txt"] → { output: Some("file.txt") }
```

**Concepts**: List patterns, option parsing

### Exercise 29: Log Level Processor
Pattern match on log entries to filter and format:
```
("ERROR", timestamp, message) → red formatting
("WARN", timestamp, message) → yellow formatting
("INFO", timestamp, message) → normal formatting
("DEBUG", timestamp, message) → gray formatting (filter in production)
```

**Concepts**: Tuple patterns, configuration-based matching

### Exercise 30: Data Validator
Build a validator using pattern matching:
- Email: must match pattern "user@domain.ext"
- Phone: multiple formats accepted
- Credit card: validate format and checksum
- Return detailed error messages for failures

**Concepts**: Pattern validation, error reporting

## Bonus Challenges

### Challenge 1: Pattern Matching Interpreter
Write an interpreter for a mini pattern matching language.

### Challenge 2: Regex Engine
Implement a simple regex engine using pattern matching on regex AST.

### Challenge 3: Theorem Prover
Build a simple theorem prover using pattern matching for logical rules.

### Challenge 4: Query Language
Create a mini query language (like SQL) using pattern matching to parse and evaluate queries.

### Challenge 5: Lisp Interpreter
Implement a Lisp interpreter where evaluation is entirely pattern matching on S-expressions.

## Testing Your Solutions

For each exercise, test with:
1. **Happy path**: Normal expected inputs
2. **Edge cases**: Empty inputs, single elements, boundaries
3. **Invalid inputs**: Type mismatches, out of range values
4. **Complex cases**: Nested structures, large inputs

## Language-Specific Notes

### Python (3.10+)
- Use `match`/`case` syntax
- Guards with `if` in case clauses
- Class patterns with dataclasses

### Haskell
- Function clauses are patterns
- Guards with `|`
- Where clauses for bindings

### Rust
- `match` must be exhaustive
- Use `_` for wildcards
- Ref patterns with `&` and `ref`

### JavaScript
- Limited native support
- Use destructuring + if/else
- Consider ts-pattern library

### Java (17+)
- Pattern matching for instanceof
- Switch expressions with patterns
- Sealed classes for exhaustiveness

### Ruby (3.0+)
- `case`/`in` for pattern matching
- Pin operator `^` for variable matching
- Find patterns for arrays

### Racket
- `match` expression
- Quasiquote patterns
- Predicate patterns with `?`

### C
- Switch statements for basic cases
- Tagged unions for ADTs
- If/else for complex patterns

### Prolog
- Pattern matching IS unification
- Clause heads are patterns
- Backtracking explores all matches

---

**Remember**: The goal is to think in terms of patterns and structure, not just values. Pattern matching makes code more declarative and often more correct by forcing you to handle all cases explicitly.
