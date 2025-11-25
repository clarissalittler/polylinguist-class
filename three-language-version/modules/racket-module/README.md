# Racket Module
## Introduction to Lisp and Language-Oriented Programming

**Duration:** 2-3 weeks (Weeks 7-8)
**Prerequisites:** Lessons 1-7, especially Functions and Higher-Order Functions
**Languages:** Racket (Scheme/Lisp family)

---

## Overview

Racket is a descendant of Scheme, which is itself a dialect of Lisp—one of the oldest programming languages still in active use (1958). This module introduces you to the Lisp family's unique approach to programming, where code is data and data is code.

### Why Learn Racket?

1. **Homoiconicity**: Code and data have the same structure (lists)
2. **Minimalism**: Very few syntactic rules to learn
3. **Macros**: Write code that writes code
4. **Functional Programming**: Pure functional style (like Haskell)
5. **Historical Importance**: Lisp influenced virtually every language you use

### What Makes Racket Different?

| Feature | Python/C++/Haskell | Racket |
|---------|-------------------|--------|
| Syntax | Keywords, operators | Parentheses, S-expressions |
| Code structure | Varied by construct | Everything is a list |
| Function calls | `f(x, y)` | `(f x y)` |
| Operators | `a + b` | `(+ a b)` |
| Extensibility | Limited | Macros can create new syntax |

---

## Installation

### macOS
```bash
brew install racket
```

### Linux (Ubuntu/Debian)
```bash
sudo apt-get install racket
```

### Windows
Download from https://racket-lang.org/download/

### Verify Installation
```bash
racket --version
# Should show: Racket v8.x
```

---

## Part 1: Basics (Day 1-2)

### Hello, World!

```racket
#lang racket

(displayln "Hello, World!")
```

Run with:
```bash
racket hello.rkt
```

### S-Expressions: The Core Concept

Everything in Racket is an **S-expression** (symbolic expression):
- Atoms: `42`, `"hello"`, `+`, `foo`
- Lists: `(1 2 3)`, `(+ 1 2)`, `(define x 5)`

**Key insight**: Function calls ARE lists. `(+ 1 2)` is a list with three elements: the symbol `+`, the number `1`, and the number `2`. Racket evaluates this list by calling the function `+` with arguments `1` and `2`.

### Basic Operations

```racket
#lang racket

; Arithmetic (prefix notation!)
(+ 1 2)        ; 3
(- 10 4)       ; 6
(* 3 4)        ; 12
(/ 15 3)       ; 5

; Nested expressions
(+ 1 (* 2 3))  ; 7 (1 + 2*3)
(* (+ 1 2) (+ 3 4))  ; 21 ((1+2) * (3+4))

; Comparison
(< 3 5)        ; #t (true)
(> 3 5)        ; #f (false)
(= 3 3)        ; #t
(equal? "hi" "hi")  ; #t

; Boolean logic
(and #t #f)    ; #f
(or #t #f)     ; #t
(not #t)       ; #f
```

### Variables and Definitions

```racket
#lang racket

; Define a variable
(define x 10)
(define name "Alice")

; Use variables
(displayln x)           ; 10
(displayln (+ x 5))     ; 15

; Define with expressions
(define y (+ x 10))     ; y = 20

; Local bindings with let
(let ([a 1]
      [b 2])
  (+ a b))  ; 3

; Sequential let (let*)
(let* ([a 1]
       [b (+ a 1)])  ; b can use a
  (+ a b))  ; 3
```

### Conditionals

```racket
#lang racket

; if expression (not statement!)
(if (> 5 3)
    "yes"
    "no")  ; "yes"

; cond for multiple branches
(define (grade score)
  (cond
    [(>= score 90) "A"]
    [(>= score 80) "B"]
    [(>= score 70) "C"]
    [(>= score 60) "D"]
    [else "F"]))

(grade 85)  ; "B"

; when/unless (one-branch conditionals)
(when (> x 0)
  (displayln "positive"))

(unless (zero? x)
  (displayln "not zero"))
```

---

## Part 2: Functions (Day 2-3)

### Defining Functions

```racket
#lang racket

; Basic function
(define (square x)
  (* x x))

(square 5)  ; 25

; Multiple parameters
(define (add a b)
  (+ a b))

(add 3 4)  ; 7

; Function with body (implicit begin)
(define (greet name)
  (display "Hello, ")
  (displayln name))

(greet "Alice")
; Hello, Alice
```

### Lambda (Anonymous Functions)

```racket
#lang racket

; Lambda expression
(lambda (x) (* x x))

; Use immediately
((lambda (x) (* x x)) 5)  ; 25

; Assign to variable (same as define)
(define square (lambda (x) (* x x)))

; Common pattern: pass to higher-order functions
(map (lambda (x) (* x 2)) '(1 2 3 4))  ; '(2 4 6 8)
```

### Higher-Order Functions

```racket
#lang racket

; map - apply function to each element
(map add1 '(1 2 3 4))        ; '(2 3 4 5)
(map square '(1 2 3 4 5))    ; '(1 4 9 16 25)

; filter - keep elements satisfying predicate
(filter even? '(1 2 3 4 5 6))  ; '(2 4 6)
(filter (lambda (x) (> x 3)) '(1 2 3 4 5))  ; '(4 5)

; foldl/foldr - reduce list to single value
(foldl + 0 '(1 2 3 4))       ; 10 (sum)
(foldl * 1 '(1 2 3 4))       ; 24 (product)

; Comparison with other languages:
; Python: map(f, lst), filter(p, lst), reduce(f, lst)
; Haskell: map f lst, filter p lst, foldl f init lst
```

### Recursion

```racket
#lang racket

; Factorial
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)  ; 120

; Sum of list
(define (sum lst)
  (if (null? lst)
      0
      (+ (first lst) (sum (rest lst)))))

(sum '(1 2 3 4 5))  ; 15

; Tail-recursive version
(define (sum-tail lst acc)
  (if (null? lst)
      acc
      (sum-tail (rest lst) (+ acc (first lst)))))

(define (sum-fast lst)
  (sum-tail lst 0))
```

---

## Part 3: Lists (Day 3-4)

### List Basics

```racket
#lang racket

; Create lists
'(1 2 3 4)              ; Quoted list literal
(list 1 2 3 4)          ; List constructor
(cons 1 '(2 3 4))       ; Cons (prepend)

; Empty list
'()
null
empty

; Check if empty
(null? '())             ; #t
(empty? '())            ; #t

; Access elements
(first '(1 2 3))        ; 1 (also: car)
(rest '(1 2 3))         ; '(2 3) (also: cdr)
(second '(1 2 3))       ; 2
(third '(1 2 3))        ; 3
(list-ref '(a b c) 1)   ; 'b (zero-indexed)

; Length
(length '(1 2 3 4))     ; 4
```

### List Operations

```racket
#lang racket

; Append lists
(append '(1 2) '(3 4))  ; '(1 2 3 4)

; Reverse
(reverse '(1 2 3))      ; '(3 2 1)

; Take and drop
(take '(1 2 3 4 5) 3)   ; '(1 2 3)
(drop '(1 2 3 4 5) 3)   ; '(4 5)

; Member (check if in list)
(member 3 '(1 2 3 4))   ; '(3 4) or #f

; Remove
(remove 3 '(1 2 3 4 3)) ; '(1 2 4 3) (removes first)
(remove* '(2 4) '(1 2 3 4 5))  ; '(1 3 5)
```

### Pattern Matching with `match`

```racket
#lang racket

; Match on structure
(define (describe lst)
  (match lst
    ['() "empty"]
    [(list x) (format "one element: ~a" x)]
    [(list x y) (format "two elements: ~a and ~a" x y)]
    [(cons x xs) (format "starts with ~a, ~a more" x (length xs))]))

(describe '())          ; "empty"
(describe '(1))         ; "one element: 1"
(describe '(1 2))       ; "two elements: 1 and 2"
(describe '(1 2 3 4))   ; "starts with 1, 3 more"

; Match with guards
(define (classify n)
  (match n
    [(? negative?) "negative"]
    [0 "zero"]
    [(? positive?) "positive"]))
```

---

## Part 4: Code as Data (Day 4-5)

### Quoting

```racket
#lang racket

; Quote prevents evaluation
'(+ 1 2)               ; The list (+ 1 2), NOT 3
(+ 1 2)                ; 3 (evaluated)

; Quote symbols
'hello                 ; The symbol hello
hello                  ; Error: hello is undefined

; Compare:
(define x 5)
x                      ; 5
'x                     ; The symbol x

; List of symbols
'(a b c)               ; '(a b c)
(list 'a 'b 'c)        ; '(a b c)
```

### Quasiquoting (Templates)

```racket
#lang racket

; Quasiquote with unquote
(define x 5)
`(the value is ,x)     ; '(the value is 5)

; Unquote-splicing
(define nums '(1 2 3))
`(before ,@nums after) ; '(before 1 2 3 after)

; Building code
(define (make-adder n)
  `(lambda (x) (+ x ,n)))

(make-adder 5)         ; '(lambda (x) (+ x 5))
```

### Evaluating Data as Code

```racket
#lang racket

; eval turns data into code
(define code '(+ 1 2))
(eval code)            ; 3

; Build and execute code dynamically
(define op '+)
(define a 10)
(define b 20)
(eval `(,op ,a ,b))    ; 30

; This is powerful but use carefully!
```

---

## Part 5: Macros (Day 5-6)

### Introduction to Macros

Macros are functions that transform code at compile time.

```racket
#lang racket

; Simple macro
(define-syntax-rule (twice expr)
  (begin expr expr))

(twice (displayln "hello"))
; Expands to: (begin (displayln "hello") (displayln "hello"))
; Prints "hello" twice

; Macro with multiple parts
(define-syntax-rule (swap! a b)
  (let ([tmp a])
    (set! a b)
    (set! b tmp)))

(define x 1)
(define y 2)
(swap! x y)
x  ; 2
y  ; 1
```

### Why Macros Matter

```racket
#lang racket

; Create your own control structures!
(define-syntax-rule (unless condition body ...)
  (if (not condition)
      (begin body ...)
      (void)))

(unless #f
  (displayln "condition was false"))

; Create domain-specific languages
(define-syntax-rule (with-timer body ...)
  (let ([start (current-inexact-milliseconds)])
    body ...
    (displayln (format "Time: ~a ms"
                       (- (current-inexact-milliseconds) start)))))

(with-timer
  (for ([i 1000000]) (void)))
```

---

## Exercises

### Exercise R1: Basic Calculations (Warmup)
Write the following calculations in Racket's prefix notation:
- `(3 + 4) * 5`
- `10 / (2 + 3)`
- `2^10` (hint: use `expt`)

### Exercise R2: List Operations
1. Write a function `(my-length lst)` that returns the length of a list (without using `length`)
2. Write a function `(my-reverse lst)` that reverses a list (without using `reverse`)
3. Write a function `(my-append lst1 lst2)` that appends two lists

### Exercise R3: Higher-Order Functions
1. Use `map` to double every number in a list
2. Use `filter` to keep only strings longer than 3 characters
3. Use `foldl` to find the maximum element in a list

### Exercise R4: Recursion
1. Write `(fibonacci n)` to compute the nth Fibonacci number
2. Write `(flatten lst)` to flatten a nested list: `'((1 2) (3 (4 5)))` → `'(1 2 3 4 5)`
3. Write `(deep-count lst)` to count all atoms in a nested list

### Exercise R5: Pattern Matching
Use `match` to write a function `(eval-expr expr)` that evaluates simple arithmetic expressions:
```racket
(eval-expr '(+ 1 2))      ; 3
(eval-expr '(* 3 4))      ; 12
(eval-expr '(+ 1 (* 2 3))) ; 7
```

### Exercise R6: Code Generation
Write a macro `(define-setters name ...)` that generates setter functions:
```racket
(define-setters x y z)
; Should define set-x!, set-y!, set-z! functions
```

### Exercise R7: Mini-Project
Implement a simple calculator REPL in Racket that:
1. Reads an expression from the user
2. Evaluates it
3. Prints the result
4. Loops until the user types "quit"

---

## Comparing with Core Languages

### Racket vs Python

| Feature | Python | Racket |
|---------|--------|--------|
| Function call | `f(x, y)` | `(f x y)` |
| List literal | `[1, 2, 3]` | `'(1 2 3)` |
| Anonymous function | `lambda x: x*2` | `(lambda (x) (* x 2))` |
| Map | `map(f, lst)` | `(map f lst)` |
| List comprehension | `[x*2 for x in lst]` | `(for/list ([x lst]) (* x 2))` |

### Racket vs Haskell

| Feature | Haskell | Racket |
|---------|---------|--------|
| Function call | `f x y` | `(f x y)` |
| List literal | `[1, 2, 3]` | `'(1 2 3)` |
| Pattern matching | Built-in | `match` macro |
| Types | Static, strong | Dynamic |
| Purity | Enforced | By convention |

### Racket vs C++

| Feature | C++ | Racket |
|---------|-----|--------|
| Memory | Manual/RAII | Garbage collected |
| Types | Static | Dynamic |
| Metaprogramming | Templates | Macros |
| Syntax | Complex | Minimal (just parentheses!) |

---

## Key Takeaways

1. **Prefix notation is consistent** - Everything is `(operator operand ...)`
2. **Lists are fundamental** - Code and data have the same shape
3. **Quoting controls evaluation** - `'x` vs `x` is profound
4. **Macros extend the language** - You can create new syntax
5. **Recursion is natural** - Especially with lists

---

## Resources

- **The Racket Guide**: https://docs.racket-lang.org/guide/
- **How to Design Programs** (free book using Racket): https://htdp.org/
- **Realm of Racket** (learn through games)
- **DrRacket IDE**: Built-in IDE with excellent features

---

## Next Steps

After this module:
- Use Racket for Project 3 (Todo List) for capstone credit
- Explore DrRacket's macro stepper
- Try #lang typed/racket for static typing
- Look into Racket's language-oriented programming features
