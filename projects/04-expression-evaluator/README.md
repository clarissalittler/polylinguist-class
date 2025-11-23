# Project 4: Expression Evaluator

**Difficulty**: ⭐⭐⭐⭐ Medium-Hard
**Time Estimate**: 6-10 hours
**Prerequisites**: Lessons 1-5 (especially Lesson 4: Functions, Lesson 3: Control Flow)

## Project Description

Build a mathematical expression evaluator that can parse and evaluate arithmetic expressions. This project introduces parsing, abstract syntax trees (ASTs), and recursive evaluation.

### Example Usage

```bash
$ python eval.py "2 + 3"
5

$ python eval.py "2 * (3 + 4)"
14

$ python eval.py "10 - 2 * 3"
4

$ python eval.py "(5 + 3) * 2 - 4 / 2"
14

$ python eval.py "x = 5; y = 3; x + y"
8
```

## Requirements

### Core Features (Required)

1. **Basic arithmetic**: `+`, `-`, `*`, `/`
2. **Parentheses**: Respect order of operations
3. **Integer support**: Handle whole numbers
4. **Error handling**: Invalid expressions, division by zero
5. **REPL mode**: Interactive evaluation

### Intermediate Features (Recommended)

6. **Variables**: Assign and use variables (`x = 5`)
7. **Floating-point**: Support decimals
8. **Exponentiation**: `2 ^ 3 = 8`
9. **Unary operators**: Negative numbers (`-5`)
10. **Multiple expressions**: Semicolon-separated statements

### Advanced Features (Optional)

11. **Functions**: Define and call functions (`f(x) = x * 2`)
12. **Built-in functions**: `sqrt`, `abs`, `sin`, `cos`, etc.
13. **Comparison operators**: `>`, `<`, `==`, `!=`
14. **Boolean logic**: `and`, `or`, `not`
15. **Conditionals**: `if` expressions

## Implementation Requirements

### Implement in at least 2 languages from different paradigms:

**Recommended combinations:**
- **Python**: Classes for AST nodes, recursive eval
- **Haskell**: Algebraic data types, pattern matching
- **Rust**: Enums for AST, Result for errors
- **Racket**: S-expressions, symbolic computation

**Compare:**
- How did you represent the AST?
- Which language made parsing easier?
- How did pattern matching (Haskell) compare to if/else (Python)?

## Technical Specifications

### Abstract Syntax Tree (AST)

**Represent expressions as trees:**

Expression: `2 + 3 * 4`

```
    +
   / \
  2   *
     / \
    3   4
```

**Python representation:**
```python
# AST Node classes
class BinOp:
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class Number:
    def __init__(self, value):
        self.value = value

# Example: 2 + 3 * 4
ast = BinOp(
    Number(2),
    '+',
    BinOp(Number(3), '*', Number(4))
)
```

**Haskell representation:**
```haskell
data Expr
    = Number Double
    | BinOp Op Expr Expr
    | Variable String
    deriving (Show, Eq)

data Op = Add | Sub | Mul | Div | Exp
    deriving (Show, Eq)

-- Example: 2 + 3 * 4
expr = BinOp Add
        (Number 2)
        (BinOp Mul (Number 3) (Number 4))
```

### Parsing Approach

**Option 1: Recursive Descent Parser (Recommended)**

Parse based on operator precedence:
1. **Expression**: Addition/Subtraction (lowest precedence)
2. **Term**: Multiplication/Division
3. **Factor**: Numbers, parentheses, unary operators (highest precedence)

**Grammar (simplified):**
```
expression := term (('+' | '-') term)*
term       := factor (('*' | '/') factor)*
factor     := number | '(' expression ')' | '-' factor
number     := [0-9]+
```

**Option 2: Shunting Yard Algorithm**

Convert infix to postfix (Reverse Polish Notation), then evaluate.

## Step-by-Step Guide

### Part 1: Tokenization

**Step 1**: Convert string to tokens

```python
# Python
import re

def tokenize(expression):
    # Pattern: numbers, operators, parentheses
    pattern = r'\d+\.?\d*|[+\-*/()^]'
    return re.findall(pattern, expression)

# Example
tokens = tokenize("2 + 3 * 4")
# Result: ['2', '+', '3', '*', '4']
```

### Part 2: Build AST

**Step 2**: Parse tokens into AST using recursive descent

```python
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def current_token(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None

    def consume(self):
        token = self.current_token()
        self.pos += 1
        return token

    def parse_expression(self):
        # Handle + and -
        left = self.parse_term()

        while self.current_token() in ['+', '-']:
            op = self.consume()
            right = self.parse_term()
            left = BinOp(left, op, right)

        return left

    def parse_term(self):
        # Handle * and /
        left = self.parse_factor()

        while self.current_token() in ['*', '/']:
            op = self.consume()
            right = self.parse_factor()
            left = BinOp(left, op, right)

        return left

    def parse_factor(self):
        # Handle numbers and parentheses
        token = self.current_token()

        if token == '(':
            self.consume()  # consume '('
            expr = self.parse_expression()
            self.consume()  # consume ')'
            return expr
        else:
            # It's a number
            self.consume()
            return Number(float(token))
```

### Part 3: Evaluation

**Step 3**: Recursively evaluate AST

```python
def evaluate(node, env=None):
    if env is None:
        env = {}

    if isinstance(node, Number):
        return node.value

    elif isinstance(node, BinOp):
        left = evaluate(node.left, env)
        right = evaluate(node.right, env)

        if node.op == '+':
            return left + right
        elif node.op == '-':
            return left - right
        elif node.op == '*':
            return left * right
        elif node.op == '/':
            if right == 0:
                raise ValueError("Division by zero")
            return left / right

    else:
        raise ValueError(f"Unknown node type: {type(node)}")
```

### Part 4: REPL

**Step 4**: Interactive mode

```python
def repl():
    print("Expression Evaluator (type 'quit' to exit)")
    env = {}

    while True:
        try:
            expression = input("> ")
            if expression.lower() == 'quit':
                break

            tokens = tokenize(expression)
            parser = Parser(tokens)
            ast = parser.parse_expression()
            result = evaluate(ast, env)
            print(result)

        except Exception as e:
            print(f"Error: {e}")

if __name__ == "__main__":
    repl()
```

## Language-Specific Hints

### Python
- Use classes for AST nodes
- Regular expressions for tokenization
- Recursive functions for parsing
- Dictionary for variable environment
- `isinstance()` for type checking in eval

### JavaScript (Node.js)
- Classes or objects for AST
- `match()` for tokenization
- Recursive parsing
- Object for environment
- `typeof` and `instanceof` for dispatch

### Haskell
- Algebraic data types perfect for AST
- Pattern matching for evaluation
- `Data.Map` for environment
- Parser combinators (`parsec` library)
- Type-safe, elegant solution

### Rust
- `enum` for AST variants
- Pattern matching with `match`
- `HashMap` for variables
- `Result<T, E>` for error handling
- Parser combinator libraries (`nom`, `combine`)

### C
- Structs with type tags for AST
- Manual tokenization (harder)
- Recursive descent parsing
- Switch statements for evaluation
- Manual memory management for AST nodes

### Java
- Abstract class/interface for Expr
- Concrete classes for each node type
- Visitor pattern for evaluation
- HashMap for environment
- Exception handling for errors

### Ruby
- Classes for AST nodes
- `scan()` for tokenization
- Recursive parsing methods
- Hash for environment
- Duck typing for dispatch

### Racket
- Racket excels at this! Symbolic expressions
- `read` to parse S-expressions
- Pattern matching with `match`
- `eval` built-in (but build your own!)
- Represent: `(+ 2 (* 3 4))`

### Prolog
- Different approach: logical rules
- Define arithmetic as relations
- Example: `eval(plus(X,Y), Z) :- eval(X,Vx), eval(Y,Vy), Z is Vx + Vy.`
- Declarative evaluation
- Built-in expression evaluation with `is`

## Extensions

### Extension 1: Variables and Assignment

Support variable assignment and lookup:

```bash
$ python eval.py "x = 5"
5
$ python eval.py "y = 3"
3
$ python eval.py "x + y"
8
```

**Implementation:**
```python
class Assignment:
    def __init__(self, name, value):
        self.name = name
        self.value = value

class Variable:
    def __init__(self, name):
        self.name = name

def evaluate(node, env):
    # ... existing cases ...

    elif isinstance(node, Assignment):
        value = evaluate(node.value, env)
        env[node.name] = value
        return value

    elif isinstance(node, Variable):
        if node.name in env:
            return env[node.name]
        else:
            raise ValueError(f"Undefined variable: {node.name}")
```

### Extension 2: Functions

Define and call functions:

```bash
$ python eval.py "f(x) = x * 2"
Function defined: f
$ python eval.py "f(5)"
10
$ python eval.py "g(x, y) = x + y"
Function defined: g
$ python eval.py "g(3, 4)"
7
```

### Extension 3: Built-in Functions

Add mathematical functions:

```python
import math

BUILTINS = {
    'sqrt': math.sqrt,
    'abs': abs,
    'sin': math.sin,
    'cos': math.cos,
    'tan': math.tan,
    'log': math.log,
    'exp': math.exp,
}
```

### Extension 4: Pretty-print AST

Visualize the parse tree:

```bash
$ python eval.py "2 + 3 * 4" --show-ast
BinOp(+)
├─ Number(2)
└─ BinOp(*)
   ├─ Number(3)
   └─ Number(4)
Result: 14
```

## Testing Checklist

- [ ] Basic arithmetic: `2 + 3`, `5 - 2`, `4 * 3`, `8 / 2`
- [ ] Order of operations: `2 + 3 * 4` should be 14, not 20
- [ ] Parentheses: `(2 + 3) * 4` should be 20
- [ ] Nested parentheses: `((2 + 3) * (4 - 1))`
- [ ] Division by zero error
- [ ] Invalid syntax error
- [ ] Negative numbers: `-5 + 3`
- [ ] Floating-point: `2.5 * 4`
- [ ] Variables (if implemented)
- [ ] Functions (if implemented)

## Grading Rubric (50 points)

| Criteria | Points | Description |
|----------|--------|-------------|
| Tokenization | 5 | Correctly splits input into tokens |
| Parsing | 15 | Builds correct AST, handles precedence |
| Evaluation | 10 | Correctly evaluates AST |
| Error Handling | 5 | Gracefully handles errors |
| REPL | 5 | Interactive mode works |
| Code Quality | 5 | Clean, well-organized, documented |
| Multi-Language | 3 | Implemented in 2+ languages |
| Extensions | 2 | Additional features |

## Architecture Considerations

### Design Patterns

**Interpreter Pattern:**
1. **Lexer** (Tokenizer): String → Tokens
2. **Parser**: Tokens → AST
3. **Evaluator**: AST → Value

**Separation of concerns:**
```
eval.py
├── Lexer class (tokenization)
├── Parser class (AST construction)
├── AST node classes (representation)
├── Evaluator (interpretation)
└── REPL (user interface)
```

### Functional vs OOP Approach

**OOP (Python/Java):**
- Classes for each AST node type
- Methods on nodes for evaluation
- Visitor pattern for extensibility

**Functional (Haskell):**
- Algebraic data types for AST
- Separate evaluation function
- Pattern matching for dispatch
- Pure functions throughout

**Hybrid (Rust):**
- Enums for AST (like ADTs)
- Pattern matching
- Struct methods for organization

### Error Handling Strategies

**Python:**
```python
try:
    result = evaluate(ast)
except ValueError as e:
    print(f"Error: {e}")
except ZeroDivisionError:
    print("Error: Division by zero")
```

**Haskell:**
```haskell
-- Use Either for error handling
evaluate :: Expr -> Either String Double
evaluate (Number n) = Right n
evaluate (BinOp Div _ (Number 0)) = Left "Division by zero"
evaluate (BinOp op left right) = do
    l <- evaluate left
    r <- evaluate right
    Right $ applyOp op l r
```

**Rust:**
```rust
fn evaluate(expr: &Expr) -> Result<f64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        Expr::BinOp { op, left, right } => {
            let l = evaluate(left)?;
            let r = evaluate(right)?;
            apply_op(op, l, r)
        }
    }
}
```

## Reflection Questions

1. **How did you represent the AST?**
   - Classes? Enums? Data types?
   - What were the tradeoffs?

2. **What parsing approach did you use?**
   - Recursive descent? Shunting yard? Parser combinators?
   - Why that choice?

3. **How did you handle operator precedence?**
   - In the grammar? In the parser?
   - Was it difficult?

4. **If you implemented in multiple languages:**
   - Which made parsing easier?
   - Did pattern matching (Haskell/Rust) help?
   - How did error handling differ?

5. **What was the hardest part?**
   - Parsing? Evaluation? Error handling?
   - Why?

6. **What would you add with more time?**

## Real-World Applications

This project teaches:
- **Parsing**: Converting text to structured data
- **Recursive algorithms**: AST traversal
- **Language design**: How expressions work
- **Error handling**: Graceful failures
- **Tree data structures**: Hierarchical data

These skills are used in:
- Compilers and interpreters
- Query languages (SQL, GraphQL)
- Configuration file parsers
- Template engines
- Computer algebra systems
- Spreadsheet formulas

## Helpful Resources

### Parsing Algorithms

**Recursive Descent:**
- Simple, intuitive
- Easy to implement by hand
- Good for learning

**Shunting Yard (Dijkstra):**
- Handles infix notation elegantly
- Outputs postfix (RPN)
- Good for calculators

**Parser Combinators:**
- Functional approach
- Composable parsers
- Libraries: `parsec` (Haskell), `nom` (Rust)

### Example: Postfix Evaluation

Postfix notation (RPN): `2 3 4 * +` = 14

```python
def eval_postfix(tokens):
    stack = []
    for token in tokens:
        if token.isdigit():
            stack.append(int(token))
        else:
            right = stack.pop()
            left = stack.pop()
            if token == '+':
                stack.append(left + right)
            elif token == '-':
                stack.append(left - right)
            elif token == '*':
                stack.append(left * right)
            elif token == '/':
                stack.append(left / right)
    return stack[0]
```

## Next Steps

- Add more operators (modulo, exponentiation)
- Implement functions
- Add conditional expressions
- Build a compiler (AST → bytecode)
- Move on to Project 5 (Polyglot Build Tool)

---

**This project bridges the gap between simple programs and real language implementation!** Many production parsers use these same techniques.
