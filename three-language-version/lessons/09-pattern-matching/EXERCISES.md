# Lesson 9: Pattern Matching - Exercises

## Exercise 1: Basic Pattern Matching (Easy)

Implement a function that classifies numbers:
- 0: "zero"
- 1: "one"  
- 2-10: "small"
- 11-100: "medium"
- else: "large"

Implement in Haskell, Python 3.10+, and C++ (your choice of approach).

## Exercise 2: List Destructuring (Medium)

Create functions using pattern matching:
1. `first_two` - returns first two elements or None/Nothing
2. `last_element` - returns last element
3. `sum_first_three` - sums first 3 elements

## Exercise 3: Expression Evaluator (Medium-Hard)

Build an expression evaluator supporting:
- Numbers
- Addition, Subtraction, Multiplication, Division
- Variables (with environment lookup)

Use pattern matching to evaluate expressions.

## Exercise 4: JSON-like Data (Hard)

Create a simple JSON parser/evaluator using pattern matching:
```python
data Value = Null
           | Bool Bool
           | Number Int
           | String String
           | Array [Value]
           | Object [(String, Value)]
```

Implement: `get_field`, `array_length`, `deep_get`

## Exercise 5: State Machine (Hard)

Model a traffic light using pattern matching:
```
Red -> Green
Green -> Yellow
Yellow -> Red
```

Add timing logic and implement `next_state` function.

See README.md for pattern matching concepts and examples.
