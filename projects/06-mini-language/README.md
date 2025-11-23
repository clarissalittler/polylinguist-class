# Project 6: Design Your Own Mini Programming Language

**Difficulty**: ⭐⭐⭐⭐⭐⭐ Very Hard (Capstone Project)
**Time Estimate**: 15-20 hours
**Prerequisites**: All Lessons 1-5, All previous projects

## Project Description

Design and implement your own small programming language with an interpreter. This is the ultimate synthesis project that brings together everything you've learned about paradigms, data structures, parsing, and language design.

You'll create a language with variables, functions, control flow, and your choice of paradigm characteristics (functional, imperative, or mixed).

### Example Language: MiniScript

```
// Define variables
let x = 10;
let y = 20;

// Define function
fn add(a, b) {
    return a + b;
}

// Call function
let result = add(x, y);
print(result);  // Output: 30

// Control flow
if (result > 25) {
    print("Big number!");
} else {
    print("Small number");
}

// Loops
let i = 0;
while (i < 5) {
    print(i);
    i = i + 1;
}
```

### Example Language: FunLang (Functional Style)

```
# Everything is an expression
let double = fn(x) => x * 2

let numbers = [1, 2, 3, 4, 5]

# Higher-order functions
let doubled = map(double, numbers)

# Pattern matching
let factorial = fn(n) =>
    match n with
    | 0 => 1
    | n => n * factorial(n - 1)

print(factorial(5))  # 120
```

## Requirements

### Core Features (Required)

**Your language must support:**

1. **Variables**: Declaration and assignment
2. **Expressions**: Arithmetic, comparison, logical
3. **Functions**: Definition and invocation
4. **Control flow**: If/else or pattern matching
5. **Loops or recursion**: Repetition mechanism
6. **Print statement**: Output
7. **Comments**: Code documentation

### Intermediate Features (Recommended)

8. **Data structures**: Lists or dictionaries
9. **First-class functions**: Functions as values
10. **Closures**: Functions capturing environment
11. **Multiple data types**: Numbers, strings, booleans, null
12. **Error messages**: Clear parse/runtime errors
13. **REPL**: Interactive mode

### Advanced Features (Optional)

14. **Type checking**: Static or dynamic
15. **Standard library**: Built-in functions
16. **Modules**: Import/export
17. **Objects/Classes**: OOP features
18. **Garbage collection**: Memory management
19. **Bytecode compiler**: Compile to intermediate form
20. **JIT compilation**: On-the-fly compilation

## Implementation Requirements

### Implement in 1-2 languages:

**Highly Recommended:**
- **Python**: Great for prototyping interpreters
- **Haskell**: Perfect for language implementation, ADTs, pattern matching
- **Rust**: Type safety, performance, excellent error handling

**Also Good:**
- **Racket**: Meta-circular evaluator, ideal for Lisp-style language
- **JavaScript**: Accessible, good for web-based REPL

**Less Recommended:**
- **C**: Much harder, more manual work
- **Java**: More verbose
- **Prolog**: Different approach (logic-based)

### Required Components

1. **Lexer** (Tokenizer): Source code → Tokens
2. **Parser**: Tokens → Abstract Syntax Tree (AST)
3. **Interpreter** or **Compiler**: AST → Execution
4. **Environment**: Variable and function storage
5. **Error handling**: Syntax and runtime errors

## Technical Specifications

### Language Design Decisions

Before coding, design your language:

**1. Paradigm Choice:**
- Imperative (C-like, JavaScript-like)
- Functional (Haskell-like, ML-like)
- Mixed (Python-like, JavaScript-like)

**2. Syntax Style:**
- C-style: `{ }` blocks, `;` terminators
- Python-style: Indentation-based
- Lisp-style: S-expressions `(+ 1 2)`

**3. Type System:**
- Dynamic typing (like Python)
- Static typing (like Haskell)
- Type inference (like ML)

**4. Semantics:**
- Pass by value or reference?
- Mutable or immutable by default?
- How do functions capture variables?

### Architecture Overview

```
Source Code
    ↓
[Lexer] → Tokens
    ↓
[Parser] → AST
    ↓
[Interpreter] → Result
```

Or for compiled approach:

```
Source Code
    ↓
[Lexer] → Tokens
    ↓
[Parser] → AST
    ↓
[Compiler] → Bytecode
    ↓
[Virtual Machine] → Result
```

## Step-by-Step Guide

### Part 1: Design Your Language

**Step 1**: Write example programs in your language

Before implementing, write 5-10 example programs showing:
- Variable declaration
- Function definition
- If/else
- Loops or recursion
- Data structures

**Example design doc:**
```markdown
# MyLang Specification

## Variables
let x = 10;
var y = 20;  // mutable

## Functions
fn greet(name) {
    return "Hello, " + name;
}

## Control Flow
if (x > 5) {
    print("big");
}

## Loops
for i in range(10) {
    print(i);
}
```

### Part 2: Implement Lexer

**Step 2**: Tokenize source code

```python
# Python
import re
from enum import Enum, auto

class TokenType(Enum):
    # Literals
    NUMBER = auto()
    STRING = auto()
    IDENTIFIER = auto()

    # Keywords
    LET = auto()
    FN = auto()
    IF = auto()
    ELSE = auto()
    WHILE = auto()
    RETURN = auto()
    PRINT = auto()

    # Operators
    PLUS = auto()
    MINUS = auto()
    STAR = auto()
    SLASH = auto()
    EQUAL = auto()
    EQUAL_EQUAL = auto()
    LESS = auto()
    GREATER = auto()

    # Delimiters
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()
    SEMICOLON = auto()
    COMMA = auto()

    # Special
    EOF = auto()

class Token:
    def __init__(self, type, value, line):
        self.type = type
        self.value = value
        self.line = line

    def __repr__(self):
        return f"Token({self.type}, {self.value}, line {self.line})"

class Lexer:
    def __init__(self, source):
        self.source = source
        self.pos = 0
        self.line = 1
        self.keywords = {
            'let': TokenType.LET,
            'fn': TokenType.FN,
            'if': TokenType.IF,
            'else': TokenType.ELSE,
            'while': TokenType.WHILE,
            'return': TokenType.RETURN,
            'print': TokenType.PRINT,
        }

    def current_char(self):
        if self.pos >= len(self.source):
            return None
        return self.source[self.pos]

    def advance(self):
        if self.current_char() == '\n':
            self.line += 1
        self.pos += 1

    def skip_whitespace(self):
        while self.current_char() in ' \t\n\r':
            self.advance()

    def skip_comment(self):
        if self.current_char() == '/' and self.peek() == '/':
            while self.current_char() != '\n':
                self.advance()

    def peek(self):
        pos = self.pos + 1
        if pos >= len(self.source):
            return None
        return self.source[pos]

    def read_number(self):
        num_str = ''
        while self.current_char() and self.current_char().isdigit():
            num_str += self.current_char()
            self.advance()
        if self.current_char() == '.':
            num_str += '.'
            self.advance()
            while self.current_char() and self.current_char().isdigit():
                num_str += self.current_char()
                self.advance()
        return float(num_str)

    def read_identifier(self):
        id_str = ''
        while self.current_char() and (self.current_char().isalnum() or self.current_char() == '_'):
            id_str += self.current_char()
            self.advance()
        return id_str

    def read_string(self):
        string = ''
        self.advance()  # Skip opening quote
        while self.current_char() and self.current_char() != '"':
            string += self.current_char()
            self.advance()
        self.advance()  # Skip closing quote
        return string

    def next_token(self):
        while self.current_char():
            if self.current_char() in ' \t\n\r':
                self.skip_whitespace()
                continue

            if self.current_char() == '/' and self.peek() == '/':
                self.skip_comment()
                continue

            if self.current_char().isdigit():
                return Token(TokenType.NUMBER, self.read_number(), self.line)

            if self.current_char().isalpha() or self.current_char() == '_':
                id_str = self.read_identifier()
                token_type = self.keywords.get(id_str, TokenType.IDENTIFIER)
                return Token(token_type, id_str, self.line)

            if self.current_char() == '"':
                return Token(TokenType.STRING, self.read_string(), self.line)

            # Single-character tokens
            char = self.current_char()
            self.advance()

            if char == '+': return Token(TokenType.PLUS, '+', self.line)
            if char == '-': return Token(TokenType.MINUS, '-', self.line)
            if char == '*': return Token(TokenType.STAR, '*', self.line)
            if char == '/': return Token(TokenType.SLASH, '/', self.line)
            if char == '(': return Token(TokenType.LPAREN, '(', self.line)
            if char == ')': return Token(TokenType.RPAREN, ')', self.line)
            if char == '{': return Token(TokenType.LBRACE, '{', self.line)
            if char == '}': return Token(TokenType.RBRACE, '}', self.line)
            if char == ';': return Token(TokenType.SEMICOLON, ';', self.line)
            if char == ',': return Token(TokenType.COMMA, ',', self.line)
            if char == '<': return Token(TokenType.LESS, '<', self.line)
            if char == '>': return Token(TokenType.GREATER, '>', self.line)

            if char == '=':
                if self.current_char() == '=':
                    self.advance()
                    return Token(TokenType.EQUAL_EQUAL, '==', self.line)
                return Token(TokenType.EQUAL, '=', self.line)

            raise SyntaxError(f"Unknown character '{char}' at line {self.line}")

        return Token(TokenType.EOF, None, self.line)

    def tokenize(self):
        tokens = []
        while True:
            token = self.next_token()
            tokens.append(token)
            if token.type == TokenType.EOF:
                break
        return tokens
```

### Part 3: Define AST Nodes

**Step 3**: Create node types for your AST

```python
# Python
class ASTNode:
    pass

class Number(ASTNode):
    def __init__(self, value):
        self.value = value

class String(ASTNode):
    def __init__(self, value):
        self.value = value

class Identifier(ASTNode):
    def __init__(self, name):
        self.name = name

class BinaryOp(ASTNode):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class Assignment(ASTNode):
    def __init__(self, name, value):
        self.name = name
        self.value = value

class FunctionDef(ASTNode):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

class FunctionCall(ASTNode):
    def __init__(self, name, args):
        self.name = name
        self.args = args

class IfStatement(ASTNode):
    def __init__(self, condition, then_branch, else_branch=None):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

class WhileLoop(ASTNode):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

class ReturnStatement(ASTNode):
    def __init__(self, value):
        self.value = value

class PrintStatement(ASTNode):
    def __init__(self, value):
        self.value = value

class Block(ASTNode):
    def __init__(self, statements):
        self.statements = statements
```

### Part 4: Implement Parser

**Step 4**: Parse tokens into AST

```python
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def current_token(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return self.tokens[-1]  # EOF

    def advance(self):
        self.pos += 1

    def expect(self, token_type):
        if self.current_token().type != token_type:
            raise SyntaxError(
                f"Expected {token_type}, got {self.current_token().type} "
                f"at line {self.current_token().line}"
            )
        token = self.current_token()
        self.advance()
        return token

    def parse(self):
        """Parse program (list of statements)"""
        statements = []
        while self.current_token().type != TokenType.EOF:
            statements.append(self.parse_statement())
        return Block(statements)

    def parse_statement(self):
        """Parse a single statement"""
        token_type = self.current_token().type

        if token_type == TokenType.LET:
            return self.parse_let()
        elif token_type == TokenType.FN:
            return self.parse_function_def()
        elif token_type == TokenType.IF:
            return self.parse_if()
        elif token_type == TokenType.WHILE:
            return self.parse_while()
        elif token_type == TokenType.RETURN:
            return self.parse_return()
        elif token_type == TokenType.PRINT:
            return self.parse_print()
        elif token_type == TokenType.LBRACE:
            return self.parse_block()
        else:
            # Expression statement
            expr = self.parse_expression()
            self.expect(TokenType.SEMICOLON)
            return expr

    def parse_let(self):
        """let identifier = expression;"""
        self.expect(TokenType.LET)
        name = self.expect(TokenType.IDENTIFIER).value
        self.expect(TokenType.EQUAL)
        value = self.parse_expression()
        self.expect(TokenType.SEMICOLON)
        return Assignment(name, value)

    def parse_function_def(self):
        """fn name(param1, param2) { ... }"""
        self.expect(TokenType.FN)
        name = self.expect(TokenType.IDENTIFIER).value
        self.expect(TokenType.LPAREN)

        params = []
        if self.current_token().type != TokenType.RPAREN:
            params.append(self.expect(TokenType.IDENTIFIER).value)
            while self.current_token().type == TokenType.COMMA:
                self.advance()
                params.append(self.expect(TokenType.IDENTIFIER).value)

        self.expect(TokenType.RPAREN)
        body = self.parse_block()
        return FunctionDef(name, params, body)

    def parse_if(self):
        """if (condition) { ... } else { ... }"""
        self.expect(TokenType.IF)
        self.expect(TokenType.LPAREN)
        condition = self.parse_expression()
        self.expect(TokenType.RPAREN)
        then_branch = self.parse_block()

        else_branch = None
        if self.current_token().type == TokenType.ELSE:
            self.advance()
            else_branch = self.parse_block()

        return IfStatement(condition, then_branch, else_branch)

    def parse_while(self):
        """while (condition) { ... }"""
        self.expect(TokenType.WHILE)
        self.expect(TokenType.LPAREN)
        condition = self.parse_expression()
        self.expect(TokenType.RPAREN)
        body = self.parse_block()
        return WhileLoop(condition, body)

    def parse_return(self):
        """return expression;"""
        self.expect(TokenType.RETURN)
        value = self.parse_expression()
        self.expect(TokenType.SEMICOLON)
        return ReturnStatement(value)

    def parse_print(self):
        """print(expression);"""
        self.expect(TokenType.PRINT)
        self.expect(TokenType.LPAREN)
        value = self.parse_expression()
        self.expect(TokenType.RPAREN)
        self.expect(TokenType.SEMICOLON)
        return PrintStatement(value)

    def parse_block(self):
        """{statement1; statement2; ...}"""
        self.expect(TokenType.LBRACE)
        statements = []
        while self.current_token().type != TokenType.RBRACE:
            statements.append(self.parse_statement())
        self.expect(TokenType.RBRACE)
        return Block(statements)

    def parse_expression(self):
        """Parse expression with operator precedence"""
        return self.parse_comparison()

    def parse_comparison(self):
        """Parse <, >, =="""
        left = self.parse_term()

        while self.current_token().type in [TokenType.LESS, TokenType.GREATER, TokenType.EQUAL_EQUAL]:
            op = self.current_token().value
            self.advance()
            right = self.parse_term()
            left = BinaryOp(left, op, right)

        return left

    def parse_term(self):
        """Parse + and -"""
        left = self.parse_factor()

        while self.current_token().type in [TokenType.PLUS, TokenType.MINUS]:
            op = self.current_token().value
            self.advance()
            right = self.parse_factor()
            left = BinaryOp(left, op, right)

        return left

    def parse_factor(self):
        """Parse * and /"""
        left = self.parse_primary()

        while self.current_token().type in [TokenType.STAR, TokenType.SLASH]:
            op = self.current_token().value
            self.advance()
            right = self.parse_primary()
            left = BinaryOp(left, op, right)

        return left

    def parse_primary(self):
        """Parse numbers, strings, identifiers, function calls, parentheses"""
        token = self.current_token()

        if token.type == TokenType.NUMBER:
            self.advance()
            return Number(token.value)

        if token.type == TokenType.STRING:
            self.advance()
            return String(token.value)

        if token.type == TokenType.IDENTIFIER:
            name = token.value
            self.advance()

            # Function call?
            if self.current_token().type == TokenType.LPAREN:
                self.advance()
                args = []
                if self.current_token().type != TokenType.RPAREN:
                    args.append(self.parse_expression())
                    while self.current_token().type == TokenType.COMMA:
                        self.advance()
                        args.append(self.parse_expression())
                self.expect(TokenType.RPAREN)
                return FunctionCall(name, args)
            else:
                return Identifier(name)

        if token.type == TokenType.LPAREN:
            self.advance()
            expr = self.parse_expression()
            self.expect(TokenType.RPAREN)
            return expr

        raise SyntaxError(f"Unexpected token: {token}")
```

### Part 5: Implement Interpreter

**Step 5**: Evaluate the AST

```python
class Environment:
    """Variable and function storage"""
    def __init__(self, parent=None):
        self.vars = {}
        self.parent = parent

    def define(self, name, value):
        self.vars[name] = value

    def get(self, name):
        if name in self.vars:
            return self.vars[name]
        if self.parent:
            return self.parent.get(name)
        raise NameError(f"Undefined variable: {name}")

    def set(self, name, value):
        if name in self.vars:
            self.vars[name] = value
        elif self.parent:
            self.parent.set(name, value)
        else:
            raise NameError(f"Undefined variable: {name}")

class ReturnValue(Exception):
    """Used to implement return statements"""
    def __init__(self, value):
        self.value = value

class Interpreter:
    def __init__(self):
        self.global_env = Environment()

    def evaluate(self, node, env=None):
        if env is None:
            env = self.global_env

        if isinstance(node, Number):
            return node.value

        elif isinstance(node, String):
            return node.value

        elif isinstance(node, Identifier):
            return env.get(node.name)

        elif isinstance(node, BinaryOp):
            left = self.evaluate(node.left, env)
            right = self.evaluate(node.right, env)

            if node.op == '+': return left + right
            elif node.op == '-': return left - right
            elif node.op == '*': return left * right
            elif node.op == '/': return left / right
            elif node.op == '<': return left < right
            elif node.op == '>': return left > right
            elif node.op == '==': return left == right

        elif isinstance(node, Assignment):
            value = self.evaluate(node.value, env)
            env.define(node.name, value)
            return value

        elif isinstance(node, FunctionDef):
            # Store function in environment
            env.define(node.name, ('function', node.params, node.body, env))
            return None

        elif isinstance(node, FunctionCall):
            func = env.get(node.name)
            if not isinstance(func, tuple) or func[0] != 'function':
                raise TypeError(f"{node.name} is not a function")

            _, params, body, closure = func

            # Create new environment for function
            func_env = Environment(closure)

            # Bind arguments to parameters
            args = [self.evaluate(arg, env) for arg in node.args]
            for param, arg in zip(params, args):
                func_env.define(param, arg)

            # Execute function body
            try:
                self.evaluate(body, func_env)
                return None
            except ReturnValue as ret:
                return ret.value

        elif isinstance(node, IfStatement):
            condition = self.evaluate(node.condition, env)
            if condition:
                return self.evaluate(node.then_branch, env)
            elif node.else_branch:
                return self.evaluate(node.else_branch, env)

        elif isinstance(node, WhileLoop):
            while self.evaluate(node.condition, env):
                self.evaluate(node.body, env)

        elif isinstance(node, ReturnStatement):
            value = self.evaluate(node.value, env)
            raise ReturnValue(value)

        elif isinstance(node, PrintStatement):
            value = self.evaluate(node.value, env)
            print(value)
            return None

        elif isinstance(node, Block):
            result = None
            for statement in node.statements:
                result = self.evaluate(statement, env)
            return result

        else:
            raise RuntimeError(f"Unknown node type: {type(node)}")
```

### Part 6: REPL

**Step 6**: Interactive interpreter

```python
def repl():
    print("Welcome to MyLang REPL")
    print("Type 'exit' to quit")

    interpreter = Interpreter()

    while True:
        try:
            source = input(">>> ")
            if source.strip() == 'exit':
                break

            lexer = Lexer(source)
            tokens = lexer.tokenize()

            parser = Parser(tokens)
            ast = parser.parse()

            result = interpreter.evaluate(ast)
            if result is not None:
                print(result)

        except Exception as e:
            print(f"Error: {e}")

def run_file(filename):
    with open(filename) as f:
        source = f.read()

    lexer = Lexer(source)
    tokens = lexer.tokenize()

    parser = Parser(tokens)
    ast = parser.parse()

    interpreter = Interpreter()
    interpreter.evaluate(ast)

if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        run_file(sys.argv[1])
    else:
        repl()
```

## Language-Specific Hints

### Python
- Classes for AST nodes
- Exceptions for return statements
- Dictionary for environment
- Easy prototyping
- Rich standard library

### Haskell
- Algebraic data types perfect for AST
- Pattern matching for evaluation
- `Data.Map` for environment
- Type safety prevents errors
- Parser combinators (`parsec`)

Example Haskell AST:
```haskell
data Expr
    = Num Double
    | Str String
    | Var String
    | BinOp Op Expr Expr
    | FnCall String [Expr]
    | If Expr Expr Expr
    deriving (Show)

evaluate :: Expr -> Env -> IO Value
evaluate (Num n) env = return (NumVal n)
evaluate (BinOp Add e1 e2) env = do
    v1 <- evaluate e1 env
    v2 <- evaluate e2 env
    return $ NumVal (getNum v1 + getNum v2)
-- ...
```

### Rust
- Enums for AST variants
- Pattern matching with `match`
- `HashMap` for environment
- `Result<T, E>` for error handling
- Lifetime management can be tricky

### Racket
- Natural for Lisp-style language
- Meta-circular evaluator
- S-expressions as syntax
- `eval` built-in (but build your own!)

### JavaScript
- Classes or objects for AST
- Map for environment
- Easy to add web-based REPL
- Dynamic typing simplifies some things

## Extensions

### Extension 1: Lists and Higher-Order Functions

Add list data structure and map/filter:

```
let numbers = [1, 2, 3, 4, 5];

let double = fn(x) { return x * 2; };
let doubled = map(double, numbers);

print(doubled);  // [2, 4, 6, 8, 10]
```

### Extension 2: Type Checking

Add optional type annotations:

```
fn add(x: Int, y: Int) -> Int {
    return x + y;
}

let result: Int = add(5, 3);  // OK
let bad: Int = add(5, "hello");  // Type error!
```

### Extension 3: Modules

Support importing code:

```
// math.lang
fn factorial(n) {
    if (n == 0) {
        return 1;
    }
    return n * factorial(n - 1);
}

export factorial;

// main.lang
import math;
print(math.factorial(5));
```

### Extension 4: Compile to Bytecode

Instead of tree-walk interpreter, compile to bytecode:

```
# Bytecode instructions
LOAD_CONST 0    # Load constant 5
LOAD_CONST 1    # Load constant 3
ADD             # Add top two values
PRINT           # Print result
```

### Extension 5: JIT Compilation

Compile hot code paths at runtime for speed.

## Testing Checklist

- [ ] Variables work (declaration, assignment, lookup)
- [ ] Arithmetic expressions evaluate correctly
- [ ] Functions can be defined and called
- [ ] Recursion works
- [ ] If/else branches correctly
- [ ] Loops repeat correctly
- [ ] Closures capture environment
- [ ] Error messages are clear
- [ ] REPL works interactively
- [ ] Can run programs from files

## Grading Rubric (100 points)

| Criteria | Points | Description |
|----------|--------|-------------|
| Lexer | 15 | Correctly tokenizes source code |
| Parser | 20 | Builds correct AST |
| Variables | 10 | Declaration, assignment, lookup |
| Expressions | 10 | Arithmetic, comparison work |
| Functions | 15 | Definition, calls, parameters |
| Control Flow | 10 | If/else or pattern matching |
| Loops/Recursion | 5 | Repetition mechanism |
| Error Handling | 5 | Clear error messages |
| Code Quality | 5 | Well-organized, documented |
| Creativity | 5 | Unique language features |

## Reflection Questions

1. **What paradigm did you choose? Why?**
   - Imperative? Functional? Mixed?
   - How did it affect implementation?

2. **What was the hardest part?**
   - Lexing? Parsing? Evaluation?
   - Environment management?

3. **What language features did you add?**
   - What was easy? What was hard?
   - What would you add next?

4. **How does your language compare to real languages?**
   - Similar to Python? Haskell? JavaScript?
   - What did you learn about language design?

5. **If you could redesign, what would you change?**

## Real-World Applications

This project teaches:
- **Language implementation**: How programming languages work
- **Parsing**: Converting text to structured data
- **Interpretation**: Executing code
- **Compiler design**: Multi-stage processing
- **Type systems**: Ensuring correctness
- **Runtime systems**: Memory, environments

These skills are used in:
- Programming language design (Python, JavaScript, Rust)
- Domain-specific languages (SQL, RegEx, GraphQL)
- Template engines (Jinja, Handlebars)
- Configuration languages (YAML, TOML parsers)
- Game scripting languages
- Query languages

## Recommended Resources

### Books
- **Crafting Interpreters** by Robert Nystrom (excellent, free online)
- **Programming Language Pragmatics** by Michael Scott
- **Types and Programming Languages** by Benjamin Pierce

### Online
- Crafting Interpreters: https://craftinginterpreters.com/
- Build Your Own Lisp: http://www.buildyourownlisp.com/
- Write Yourself a Scheme in 48 Hours (Haskell)

## Example Languages to Build

### 1. Calculator Language (Simplest)
Just expressions, no variables:
```
2 + 3 * 4
(5 - 2) * 3
```

### 2. Scripting Language (Moderate)
Variables, functions, simple control flow:
```
let x = 10;
fn double(n) { return n * 2; }
print(double(x));
```

### 3. Functional Language (Advanced)
First-class functions, closures, recursion:
```
let map = fn(f, list) { ... };
let double = fn(x) { x * 2 };
map(double, [1, 2, 3]);
```

### 4. Lisp-Style (Different)
S-expressions, minimal syntax:
```
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

## Next Steps

- Add more data types (lists, objects, etc.)
- Implement type checking
- Write standard library
- Optimize interpreter
- Compile to bytecode or machine code
- Share your language with others!

---

**Congratulations!** You've built a programming language from scratch. This is one of the most challenging and rewarding projects in computer science.

**You now understand how languages work from the inside out.**
