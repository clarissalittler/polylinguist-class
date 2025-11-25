# Project 6: Mini Programming Language

**Difficulty:** Very Hard
**Estimated Time:** 15-20 hours
**Prerequisites:** All lessons, especially Expression Evaluator (Project 4)
**Due:** End of Week 15 (Final Project)

**Capstone Requirement:** Implement in a minor language OR provide detailed paradigm comparison

---

## Overview

Design and implement a simple programming language with variables, expressions, control flow, and functions. This capstone project brings together parsing, tree structures, evaluation, and language design—the culmination of the course.

---

## Learning Objectives

By completing this project, you will:
- Design a programming language from scratch
- Implement a lexer (tokenizer)
- Build a parser for a complete grammar
- Create an interpreter or evaluator
- Handle variable scoping and environments
- Implement control flow structures
- (Optional) Add functions and recursion

---

## Requirements

### Basic Requirements (Must Complete)

Your language must support:

1. **Variables**: Declaration and assignment
2. **Arithmetic expressions**: `+`, `-`, `*`, `/`, parentheses
3. **Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=`
4. **Print statement**: Output values
5. **If/else**: Conditional execution
6. **While loops**: Iteration
7. **Comments**: Ignore lines starting with `#`

**Example Program (Mini Language):**
```
# Calculate factorial iteratively
n = 5
result = 1
i = 1

while i <= n {
    result = result * i
    i = i + 1
}

print result  # Should print 120
```

**Expected Output:**
```
120
```

### Standard Requirements (Complete Most)

Extend your language with:

8. **Boolean literals**: `true`, `false`
9. **Logical operators**: `and`, `or`, `not`
10. **String literals**: `"hello"` with concatenation
11. **For loops**: `for i in 1..10 { ... }`
12. **Input**: `read x` to get user input
13. **Multiple statements per line**: Using `;`

**Extended Example:**
```
# Greet the user
print "What is your name?"
read name
print "Hello, " + name + "!"

# Count to 5
for i in 1..5 {
    print i
}
```

### Advanced Requirements (Challenge)

For additional challenge:

14. **Functions**: Define and call functions with parameters
15. **Return values**: Functions can return results
16. **Recursion**: Functions can call themselves
17. **Local scope**: Variables local to functions
18. **Arrays/Lists**: Basic collection support
19. **Error messages**: Line numbers and helpful errors

**Advanced Example:**
```
# Define a function
func factorial(n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

# Use the function
print factorial(5)   # 120
print factorial(10)  # 3628800

# List example
numbers = [1, 2, 3, 4, 5]
for n in numbers {
    print n * 2
}
```

---

## Technical Specifications

### Suggested Grammar

```
program     → statement*
statement   → varDecl | assignment | printStmt | ifStmt | whileStmt | block
varDecl     → IDENTIFIER '=' expression
assignment  → IDENTIFIER '=' expression
printStmt   → 'print' expression
ifStmt      → 'if' expression block ('else' block)?
whileStmt   → 'while' expression block
block       → '{' statement* '}'

expression  → logic_or
logic_or    → logic_and ('or' logic_and)*
logic_and   → equality ('and' equality)*
equality    → comparison (('==' | '!=') comparison)*
comparison  → term (('<' | '>' | '<=' | '>=') term)*
term        → factor (('+' | '-') factor)*
factor      → unary (('*' | '/') unary)*
unary       → ('not' | '-') unary | primary
primary     → NUMBER | STRING | 'true' | 'false' | IDENTIFIER | '(' expression ')'
```

### Token Types

```python
class TokenType(Enum):
    # Literals
    NUMBER = 'NUMBER'
    STRING = 'STRING'
    IDENTIFIER = 'IDENTIFIER'
    TRUE = 'TRUE'
    FALSE = 'FALSE'

    # Operators
    PLUS = '+'
    MINUS = '-'
    STAR = '*'
    SLASH = '/'
    EQ = '=='
    NE = '!='
    LT = '<'
    GT = '>'
    LE = '<='
    GE = '>='
    ASSIGN = '='

    # Keywords
    PRINT = 'PRINT'
    IF = 'IF'
    ELSE = 'ELSE'
    WHILE = 'WHILE'
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'
    FUNC = 'FUNC'
    RETURN = 'RETURN'

    # Punctuation
    LPAREN = '('
    RPAREN = ')'
    LBRACE = '{'
    RBRACE = '}'
    SEMICOLON = ';'
    COMMA = ','

    # Special
    EOF = 'EOF'
    NEWLINE = 'NEWLINE'
```

### AST Node Types

```python
# Expressions
class Expr: pass
class Literal(Expr): pass      # Numbers, strings, booleans
class Variable(Expr): pass     # Variable reference
class Binary(Expr): pass       # a + b, a == b, etc.
class Unary(Expr): pass        # -x, not x
class Call(Expr): pass         # function(args)

# Statements
class Stmt: pass
class Print(Stmt): pass        # print expression
class VarDecl(Stmt): pass      # x = 5
class If(Stmt): pass           # if ... { } else { }
class While(Stmt): pass        # while ... { }
class Block(Stmt): pass        # { statements }
class FuncDecl(Stmt): pass     # func name(params) { }
class Return(Stmt): pass       # return expression
```

### Environment (Variable Storage)

```python
class Environment:
    def __init__(self, parent=None):
        self.values = {}
        self.parent = parent

    def define(self, name, value):
        self.values[name] = value

    def get(self, name):
        if name in self.values:
            return self.values[name]
        if self.parent:
            return self.parent.get(name)
        raise RuntimeError(f"Undefined variable: {name}")

    def assign(self, name, value):
        if name in self.values:
            self.values[name] = value
            return
        if self.parent:
            self.parent.assign(name, value)
            return
        raise RuntimeError(f"Undefined variable: {name}")
```

---

## Implementation Guide

### Step 1: Lexer

Convert source code to tokens:

```python
class Lexer:
    def __init__(self, source):
        self.source = source
        self.tokens = []
        self.start = 0
        self.current = 0
        self.line = 1

    def scan_tokens(self):
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()
        self.tokens.append(Token(TokenType.EOF, "", None, self.line))
        return self.tokens

    def scan_token(self):
        c = self.advance()
        if c == '(': self.add_token(TokenType.LPAREN)
        elif c == ')': self.add_token(TokenType.RPAREN)
        elif c == '{': self.add_token(TokenType.LBRACE)
        elif c == '}': self.add_token(TokenType.RBRACE)
        elif c == '+': self.add_token(TokenType.PLUS)
        elif c == '-': self.add_token(TokenType.MINUS)
        elif c == '*': self.add_token(TokenType.STAR)
        elif c == '/': self.add_token(TokenType.SLASH)
        elif c == '=':
            if self.match('='):
                self.add_token(TokenType.EQ)
            else:
                self.add_token(TokenType.ASSIGN)
        elif c == '<':
            self.add_token(TokenType.LE if self.match('=') else TokenType.LT)
        elif c == '>':
            self.add_token(TokenType.GE if self.match('=') else TokenType.GT)
        elif c == '!':
            if self.match('='):
                self.add_token(TokenType.NE)
        elif c == '#':
            # Comment - skip to end of line
            while self.peek() != '\n' and not self.is_at_end():
                self.advance()
        elif c == '"':
            self.string()
        elif c.isdigit():
            self.number()
        elif c.isalpha() or c == '_':
            self.identifier()
        elif c == '\n':
            self.line += 1
        elif c in ' \t\r':
            pass  # Ignore whitespace
        else:
            self.error(f"Unexpected character: {c}")
```

### Step 2: Parser

Build AST from tokens:

```python
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current = 0

    def parse(self):
        statements = []
        while not self.is_at_end():
            stmt = self.statement()
            if stmt:
                statements.append(stmt)
        return statements

    def statement(self):
        if self.match(TokenType.PRINT):
            return self.print_statement()
        if self.match(TokenType.IF):
            return self.if_statement()
        if self.match(TokenType.WHILE):
            return self.while_statement()
        if self.match(TokenType.LBRACE):
            return Block(self.block())
        return self.expression_statement()

    def if_statement(self):
        condition = self.expression()
        self.consume(TokenType.LBRACE, "Expected '{' after if condition")
        then_branch = Block(self.block())
        else_branch = None
        if self.match(TokenType.ELSE):
            self.consume(TokenType.LBRACE, "Expected '{' after else")
            else_branch = Block(self.block())
        return If(condition, then_branch, else_branch)

    def while_statement(self):
        condition = self.expression()
        self.consume(TokenType.LBRACE, "Expected '{' after while condition")
        body = Block(self.block())
        return While(condition, body)

    def expression(self):
        return self.logic_or()

    # ... implement each precedence level
```

### Step 3: Interpreter

Evaluate the AST:

```python
class Interpreter:
    def __init__(self):
        self.environment = Environment()

    def interpret(self, statements):
        for stmt in statements:
            self.execute(stmt)

    def execute(self, stmt):
        if isinstance(stmt, Print):
            value = self.evaluate(stmt.expression)
            print(value)
        elif isinstance(stmt, VarDecl):
            value = self.evaluate(stmt.initializer)
            self.environment.define(stmt.name, value)
        elif isinstance(stmt, If):
            if self.is_truthy(self.evaluate(stmt.condition)):
                self.execute(stmt.then_branch)
            elif stmt.else_branch:
                self.execute(stmt.else_branch)
        elif isinstance(stmt, While):
            while self.is_truthy(self.evaluate(stmt.condition)):
                self.execute(stmt.body)
        elif isinstance(stmt, Block):
            self.execute_block(stmt.statements)

    def evaluate(self, expr):
        if isinstance(expr, Literal):
            return expr.value
        elif isinstance(expr, Variable):
            return self.environment.get(expr.name)
        elif isinstance(expr, Binary):
            left = self.evaluate(expr.left)
            right = self.evaluate(expr.right)
            op = expr.operator
            if op == '+': return left + right
            if op == '-': return left - right
            if op == '*': return left * right
            if op == '/': return left / right
            if op == '==': return left == right
            if op == '!=': return left != right
            if op == '<': return left < right
            if op == '>': return left > right
            if op == '<=': return left <= right
            if op == '>=': return left >= right
        # ... handle other expressions
```

---

## Sample Programs

### program1.mini - FizzBuzz
```
# FizzBuzz from 1 to 20
i = 1
while i <= 20 {
    if i % 15 == 0 {
        print "FizzBuzz"
    } else {
        if i % 3 == 0 {
            print "Fizz"
        } else {
            if i % 5 == 0 {
                print "Buzz"
            } else {
                print i
            }
        }
    }
    i = i + 1
}
```

### program2.mini - Prime Checker
```
# Check if a number is prime
n = 17
is_prime = true
i = 2

while i * i <= n and is_prime {
    if n % i == 0 {
        is_prime = false
    }
    i = i + 1
}

if is_prime {
    print n
    print "is prime"
} else {
    print n
    print "is not prime"
}
```

### program3.mini - Fibonacci (with functions)
```
func fib(n) {
    if n <= 1 {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

# Print first 10 Fibonacci numbers
i = 0
while i < 10 {
    print fib(i)
    i = i + 1
}
```

---

## Capstone Requirements

For this final project, you must do ONE of:

### Option A: Minor Language Implementation
Implement your mini-language interpreter in one of:
- **Racket**: Natural fit for language implementation
- **Rust**: Systems language challenge
- **Prolog**: Logic programming approach

Include a reflection comparing the implementation experience.

### Option B: Paradigm Comparison Report
Implement in your core language of choice AND write a detailed report (1500+ words) covering:
- How each core language would approach this differently
- Specific examples of paradigm differences (FP vs OOP vs imperative)
- What features of your language design favor which paradigms
- Theoretical extensions and their implementation challenges

---

## Submission Checklist

- [ ] Lexer tokenizes all required tokens
- [ ] Parser builds correct AST
- [ ] Variables work (declaration, assignment, use)
- [ ] Arithmetic expressions evaluate correctly
- [ ] Comparisons and booleans work
- [ ] Print statement outputs values
- [ ] If/else executes correctly
- [ ] While loops work
- [ ] Comments are ignored
- [ ] At least 2 standard features
- [ ] Three working sample programs
- [ ] `REFLECTION.md` or paradigm report completed
- [ ] (Capstone) Minor language version OR detailed report

---

## Reflection Questions

1. What was the hardest part of building a language?
2. How did you design your grammar? What tradeoffs did you make?
3. How did your AST design affect the interpreter?
4. What feature would be hardest to add next? (closures? classes? types?)
5. What did this project teach you about how programming languages work?

---

## Grading Rubric

| Criteria | Points |
|----------|--------|
| Lexer (correct tokenization) | 15 |
| Parser (correct AST) | 20 |
| Variables | 15 |
| Expressions | 15 |
| Control flow (if, while) | 20 |
| Standard features | 10 |
| Code quality | 5 |
| **Total** | **100** |

**Capstone Requirement:** +30 for minor language implementation OR detailed report

---

## Common Mistakes to Avoid

1. **Forgetting operator precedence** - Test `2 + 3 * 4`
2. **Variable scoping bugs** - Blocks should create new scopes (for functions)
3. **Infinite loops** - Make sure while conditions can become false
4. **Off-by-one in parsing** - Token consumption bugs
5. **String handling** - Escape characters, multiline

---

## Resources

### Books
- *Crafting Interpreters* by Robert Nystrom (free online!) - Highly recommended
- *Writing an Interpreter in Go* by Thorsten Ball
- *Structure and Interpretation of Computer Programs*

### Tutorials
- https://craftinginterpreters.com/ - Step-by-step guide
- https://ruslanspivak.com/lsbasi-part1/ - "Let's Build a Simple Interpreter"

---

## Extension Ideas

If you finish the basic language:
- **Type system**: Static type checking
- **Classes**: Object-oriented features
- **Modules**: Import other files
- **Standard library**: Built-in functions
- **Compiler**: Generate bytecode or native code
- **Debugger**: Step through execution
- **REPL**: Interactive mode

---

Congratulations on reaching the capstone project! Building a programming language is one of the most educational experiences in computer science. You'll never look at languages the same way again.

**Good luck, and enjoy creating something from nothing!**
