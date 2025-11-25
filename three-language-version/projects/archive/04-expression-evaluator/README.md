# Project 4: Expression Evaluator

**Difficulty:** Medium-Hard
**Estimated Time:** 6-10 hours
**Prerequisites:** Lessons 1-8, especially Recursion and Data Structures
**Due:** End of Week 11

**Capstone Option:** Implement with parser combinators in Haskell

---

## Overview

Build a mathematical expression evaluator that parses and computes expressions like `3 + 4 * 2` with proper operator precedence. This project reinforces parsing, recursion, tree structures, and the evaluation pattern central to language implementation.

---

## Learning Objectives

By completing this project, you will:
- Parse structured text into data structures
- Build and traverse expression trees
- Implement operator precedence correctly
- Use recursion for tree operations
- Understand the basics of language implementation
- Handle syntax errors gracefully

---

## Requirements

### Basic Requirements (Must Complete)

Your evaluator should handle:

1. **Integer literals**: `42`, `-7`, `0`
2. **Basic operators**: `+`, `-`, `*`, `/`
3. **Operator precedence**: `*` and `/` before `+` and `-`
4. **Parentheses**: Override precedence with `(` and `)`
5. **REPL mode**: Interactive prompt for entering expressions

**Example Session:**
```
Expression Evaluator
Type 'quit' to exit.

> 2 + 3
5

> 2 + 3 * 4
14

> (2 + 3) * 4
20

> 10 / 2 - 3
2

> quit
Goodbye!
```

### Standard Requirements (Complete Most)

Extend your evaluator with:

6. **Floating point numbers**: `3.14`, `2.5`
7. **Unary minus**: `-5`, `-(3 + 2)`
8. **Exponentiation**: `2 ^ 3` (right-associative)
9. **Variables**: `x = 5`, then `x + 3`
10. **Error messages**: Helpful errors for invalid input

**Extended Example:**
```
> 3.14 * 2
6.28

> 2 ^ 3 ^ 2
512

> x = 10
x = 10

> x * 2 + 5
25

> y
Error: Undefined variable 'y'

> 3 + + 5
Error: Unexpected '+' at position 5
```

### Advanced Requirements (Challenge)

For additional challenge:

11. **Functions**: `sqrt(16)`, `max(3, 7)`, `sin(0)`
12. **User-defined functions**: `f(x) = x * 2`
13. **Show parse tree**: `tree 2 + 3 * 4` shows the AST
14. **Step-by-step evaluation**: Show reduction steps
15. **Type checking**: Distinguish integers and floats
16. **Multiple statements**: Evaluate multiple expressions

**Advanced Example:**
```
> sqrt(16) + max(3, 7)
11.0

> f(x) = x ^ 2 + 1
Defined: f(x)

> f(5)
26

> tree 2 + 3 * 4
    (+)
   /   \
  2    (*)
      /   \
     3     4

> step (2 + 3) * 4
= (2 + 3) * 4
= 5 * 4
= 20
```

---

## Technical Specifications

### Grammar (BNF)

Basic grammar for expressions:

```
expression  → term (('+' | '-') term)*
term        → factor (('*' | '/') factor)*
factor      → NUMBER | '(' expression ')' | '-' factor
NUMBER      → [0-9]+ ('.' [0-9]+)?
```

With exponentiation (right-associative):
```
factor      → power ('^' factor)?
power       → NUMBER | '(' expression ')' | '-' power
```

### Operator Precedence (Lowest to Highest)

1. `+`, `-` (addition, subtraction)
2. `*`, `/` (multiplication, division)
3. `^` (exponentiation, right-associative)
4. Unary `-` (negation)
5. `()` (parentheses)

### Data Structures

**Expression Tree (AST):**

```python
# Python
class Expr: pass

class Num(Expr):
    def __init__(self, value): self.value = value

class BinOp(Expr):
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

class UnaryOp(Expr):
    def __init__(self, op, operand):
        self.op = op
        self.operand = operand
```

```cpp
// C++
struct Expr {
    virtual ~Expr() = default;
    virtual double eval() = 0;
};

struct Num : Expr {
    double value;
    Num(double v) : value(v) {}
    double eval() override { return value; }
};

struct BinOp : Expr {
    char op;
    std::unique_ptr<Expr> left, right;
    double eval() override;
};
```

```haskell
-- Haskell
data Expr
    = Num Double
    | BinOp Char Expr Expr
    | UnaryOp Char Expr
    deriving Show
```

---

## Implementation Strategy

### Step 1: Tokenizer (Lexer)

Convert string to tokens:
```
"2 + 3 * 4" → [Num(2), Plus, Num(3), Star, Num(4)]
```

```python
import re

def tokenize(text):
    token_pattern = r'\d+\.?\d*|[+\-*/^()]'
    return re.findall(token_pattern, text)

# tokenize("2 + 3 * 4") → ['2', '+', '3', '*', '4']
```

### Step 2: Parser

Build expression tree using recursive descent:

```python
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def parse(self):
        return self.expression()

    def expression(self):
        left = self.term()
        while self.current() in ('+', '-'):
            op = self.advance()
            right = self.term()
            left = BinOp(op, left, right)
        return left

    def term(self):
        left = self.factor()
        while self.current() in ('*', '/'):
            op = self.advance()
            right = self.factor()
            left = BinOp(op, left, right)
        return left

    def factor(self):
        if self.current() == '(':
            self.advance()  # consume '('
            expr = self.expression()
            self.advance()  # consume ')'
            return expr
        elif self.current() == '-':
            self.advance()
            return UnaryOp('-', self.factor())
        else:
            return Num(float(self.advance()))

    def current(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def advance(self):
        token = self.current()
        self.pos += 1
        return token
```

### Step 3: Evaluator

Recursively evaluate the tree:

```python
def evaluate(expr):
    if isinstance(expr, Num):
        return expr.value
    elif isinstance(expr, BinOp):
        left = evaluate(expr.left)
        right = evaluate(expr.right)
        if expr.op == '+': return left + right
        if expr.op == '-': return left - right
        if expr.op == '*': return left * right
        if expr.op == '/': return left / right
        if expr.op == '^': return left ** right
    elif isinstance(expr, UnaryOp):
        if expr.op == '-':
            return -evaluate(expr.operand)
```

---

## Language-Specific Approaches

### Python: Classes + Recursive Descent

See implementation strategy above. Python's dynamic typing makes the AST straightforward.

### C++: Inheritance + Smart Pointers

```cpp
#include <memory>
#include <string>
#include <cmath>

class Expr {
public:
    virtual ~Expr() = default;
    virtual double eval() const = 0;
};

class Num : public Expr {
    double value;
public:
    Num(double v) : value(v) {}
    double eval() const override { return value; }
};

class BinOp : public Expr {
    char op;
    std::unique_ptr<Expr> left, right;
public:
    BinOp(char o, std::unique_ptr<Expr> l, std::unique_ptr<Expr> r)
        : op(o), left(std::move(l)), right(std::move(r)) {}

    double eval() const override {
        double l = left->eval();
        double r = right->eval();
        switch (op) {
            case '+': return l + r;
            case '-': return l - r;
            case '*': return l * r;
            case '/': return l / r;
            case '^': return std::pow(l, r);
        }
        return 0;
    }
};
```

### Haskell: Algebraic Data Types + Parser Combinators

Haskell excels at this project! Use parser combinators:

```haskell
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)

data Expr
    = Num Double
    | BinOp String Expr Expr
    | UnaryOp String Expr
    deriving Show

-- Lexer
lexer = Token.makeTokenParser emptyDef

parens = Token.parens lexer
number = Token.float lexer <|> fromIntegral <$> Token.integer lexer
reservedOp = Token.reservedOp lexer

-- Expression parser with precedence
expr :: Parser Expr
expr = buildExpressionParser table term

term :: Parser Expr
term = parens expr <|> (Num <$> number)

table =
    [ [Prefix (reservedOp "-" >> return (UnaryOp "-"))]
    , [Infix (reservedOp "^" >> return (BinOp "^")) AssocRight]
    , [Infix (reservedOp "*" >> return (BinOp "*")) AssocLeft,
       Infix (reservedOp "/" >> return (BinOp "/")) AssocLeft]
    , [Infix (reservedOp "+" >> return (BinOp "+")) AssocLeft,
       Infix (reservedOp "-" >> return (BinOp "-")) AssocLeft]
    ]

-- Evaluator
eval :: Expr -> Double
eval (Num n) = n
eval (UnaryOp "-" e) = -(eval e)
eval (BinOp "+" l r) = eval l + eval r
eval (BinOp "-" l r) = eval l - eval r
eval (BinOp "*" l r) = eval l * eval r
eval (BinOp "/" l r) = eval l / eval r
eval (BinOp "^" l r) = eval l ** eval r

-- REPL
main :: IO ()
main = do
    putStrLn "Expression Evaluator (type 'quit' to exit)"
    repl

repl :: IO ()
repl = do
    putStr "> "
    input <- getLine
    if input == "quit"
        then putStrLn "Goodbye!"
        else do
            case parse expr "" input of
                Left err -> print err
                Right e  -> print (eval e)
            repl
```

---

## Test Cases

```
# Basic operations
2 + 3 → 5
10 - 4 → 6
3 * 4 → 12
15 / 3 → 5

# Precedence
2 + 3 * 4 → 14
10 - 2 * 3 → 4
10 / 2 + 3 → 8

# Parentheses
(2 + 3) * 4 → 20
10 / (2 + 3) → 2
((2 + 3)) → 5

# Negative numbers
-5 → -5
3 + -2 → 1
-3 * -4 → 12

# Exponentiation
2 ^ 3 → 8
2 ^ 3 ^ 2 → 512 (right-associative)
4 ^ 0.5 → 2

# Complex expressions
(2 + 3) * (4 - 1) → 15
2 ^ (1 + 2) * 3 → 24
```

---

## Submission Checklist

- [ ] Basic operations (+, -, *, /) work correctly
- [ ] Operator precedence is correct
- [ ] Parentheses work
- [ ] REPL mode functional
- [ ] At least 2 standard features (floats, unary minus, etc.)
- [ ] Error handling for invalid input
- [ ] Code is well-structured
- [ ] `REFLECTION.md` completed
- [ ] (Capstone) Haskell parser combinators version

---

## Reflection Questions

1. How did you handle operator precedence in your parser?
2. What data structure did you use for the expression tree? Why?
3. How did recursion factor into your implementation?
4. What was the hardest edge case to handle?
5. (Capstone) How did parser combinators change your approach?

---

## Grading Rubric

| Criteria | Points |
|----------|--------|
| Basic operations | 25 |
| Operator precedence | 20 |
| Parentheses | 15 |
| Standard features | 20 |
| Error handling | 10 |
| Code quality | 10 |
| **Total** | **100** |

**Capstone Bonus:** +25 for Haskell parser combinators implementation

---

## Common Mistakes to Avoid

1. **Wrong precedence** - `2 + 3 * 4` should be 14, not 20
2. **Wrong associativity** - `2 ^ 3 ^ 2` should be 512 (right-associative)
3. **Missing parentheses handling** - Nested parens must work
4. **Division by zero** - Handle gracefully
5. **Unary minus confusion** - `3 - -2` is valid

---

## Extension Ideas

- Boolean expressions (`true && false`, `5 > 3`)
- String concatenation
- Lambda expressions
- Let bindings
- Full language interpreter (leads to Project 6!)

---

This project is a stepping stone to building your own programming language!
