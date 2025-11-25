# Racket Module Exercises

## Warmup Exercises

### Exercise R.W1: Prefix Notation
Convert these expressions to Racket's prefix notation and evaluate them:
1. `5 + 3`
2. `(7 - 2) * 4`
3. `100 / (5 + 5)`
4. `2^8` (use `expt`)
5. `(3 + 4) * (5 - 2) / 3`

### Exercise R.W2: First Functions
Write these simple functions:

```racket
; 1. Returns the square of a number
(define (square x) ...)

; 2. Returns the absolute value (without using abs)
(define (my-abs x) ...)

; 3. Returns the larger of two numbers
(define (my-max a b) ...)
```

### Exercise R.W3: Conditionals
Write a function `(sign n)` that returns:
- `'positive` if n > 0
- `'negative` if n < 0
- `'zero` if n = 0

---

## Standard Exercises

### Exercise R.S1: List Basics
Without using built-in functions, implement:

```racket
; 1. Count elements in a list
(define (my-length lst) ...)
; (my-length '(1 2 3 4)) => 4

; 2. Get the last element
(define (my-last lst) ...)
; (my-last '(1 2 3 4)) => 4

; 3. Get nth element (0-indexed)
(define (my-nth lst n) ...)
; (my-nth '(a b c d) 2) => 'c
```

### Exercise R.S2: List Construction
Implement these list operations:

```racket
; 1. Append two lists
(define (my-append lst1 lst2) ...)
; (my-append '(1 2) '(3 4)) => '(1 2 3 4)

; 2. Reverse a list
(define (my-reverse lst) ...)
; (my-reverse '(1 2 3)) => '(3 2 1)

; 3. Take first n elements
(define (my-take lst n) ...)
; (my-take '(1 2 3 4 5) 3) => '(1 2 3)
```

### Exercise R.S3: Higher-Order Functions
Use `map`, `filter`, and `foldl` to:

```racket
; 1. Double every number in a list
(define (double-all lst) ...)
; (double-all '(1 2 3)) => '(2 4 6)

; 2. Keep only even numbers
(define (evens-only lst) ...)
; (evens-only '(1 2 3 4 5 6)) => '(2 4 6)

; 3. Sum all numbers
(define (sum lst) ...)
; (sum '(1 2 3 4)) => 10

; 4. Find the product of all numbers
(define (product lst) ...)
; (product '(1 2 3 4)) => 24

; 5. Find the maximum (using foldl)
(define (my-maximum lst) ...)
; (my-maximum '(3 1 4 1 5 9)) => 9
```

### Exercise R.S4: Recursion Practice
Write recursive functions:

```racket
; 1. Fibonacci
(define (fib n) ...)
; (fib 10) => 55

; 2. Count occurrences of x in lst
(define (count-occurrences x lst) ...)
; (count-occurrences 'a '(a b a c a)) => 3

; 3. Remove all occurrences of x
(define (remove-all x lst) ...)
; (remove-all 'a '(a b a c a)) => '(b c)

; 4. Insert x at position n
(define (insert-at x n lst) ...)
; (insert-at 'X 2 '(a b c d)) => '(a b X c d)
```

### Exercise R.S5: Pattern Matching
Use `match` to implement:

```racket
; 1. Describe a list's structure
(define (describe-list lst) ...)
; (describe-list '()) => "empty"
; (describe-list '(a)) => "singleton"
; (describe-list '(a b)) => "pair"
; (describe-list '(a b c d)) => "many"

; 2. Safe head (returns #f for empty list)
(define (safe-head lst) ...)
; (safe-head '(1 2 3)) => 1
; (safe-head '()) => #f

; 3. Second-to-last element
(define (second-to-last lst) ...)
; (second-to-last '(1 2 3 4)) => 3
```

---

## Advanced Exercises

### Exercise R.A1: Nested Lists
Work with nested list structures:

```racket
; 1. Flatten a nested list
(define (flatten lst) ...)
; (flatten '((1 2) (3 (4 5)) 6)) => '(1 2 3 4 5 6)

; 2. Count all atoms in a nested list
(define (count-atoms lst) ...)
; (count-atoms '((1 2) (3 (4 5)))) => 5

; 3. Deep map - apply f to every atom
(define (deep-map f lst) ...)
; (deep-map add1 '((1 2) (3 (4 5)))) => '((2 3) (4 (5 6)))
```

### Exercise R.A2: Association Lists
Implement a simple key-value store using association lists:

```racket
; Association list: '((key1 . val1) (key2 . val2) ...)

; 1. Look up a key
(define (lookup key alist) ...)
; (lookup 'b '((a . 1) (b . 2) (c . 3))) => 2

; 2. Add or update a key
(define (assoc-set key val alist) ...)
; (assoc-set 'b 99 '((a . 1) (b . 2))) => '((a . 1) (b . 99))

; 3. Remove a key
(define (assoc-remove key alist) ...)

; 4. Get all keys
(define (assoc-keys alist) ...)

; 5. Get all values
(define (assoc-values alist) ...)
```

### Exercise R.A3: Expression Evaluator
Build an arithmetic expression evaluator:

```racket
; Evaluate expressions like:
; '(+ 1 2) => 3
; '(* 3 (+ 1 2)) => 9
; '(- 10 (/ 6 2)) => 7

(define (eval-expr expr) ...)
```

Extend it to support:
- Variables: `(eval-expr '(+ x 1) '((x . 5)))` => 6
- Let bindings: `(let ((x 5)) (+ x 1))` => 6

### Exercise R.A4: Set Operations
Implement set operations (lists without duplicates):

```racket
; 1. Remove duplicates
(define (make-set lst) ...)
; (make-set '(1 2 2 3 3 3)) => '(1 2 3)

; 2. Set union
(define (set-union s1 s2) ...)
; (set-union '(1 2 3) '(2 3 4)) => '(1 2 3 4)

; 3. Set intersection
(define (set-intersection s1 s2) ...)
; (set-intersection '(1 2 3) '(2 3 4)) => '(2 3)

; 4. Set difference
(define (set-difference s1 s2) ...)
; (set-difference '(1 2 3) '(2 3 4)) => '(1)

; 5. Subset check
(define (subset? s1 s2) ...)
; (subset? '(1 2) '(1 2 3)) => #t
```

### Exercise R.A5: Tree Operations
Work with binary trees represented as nested lists:

```racket
; Tree: '() for empty, '(value left right) for node
; Example: '(5 (3 (1 () ()) (4 () ())) (7 (6 () ()) (9 () ())))
;
;        5
;       / \
;      3   7
;     / \ / \
;    1  4 6  9

; 1. Check if tree is empty
(define (tree-empty? tree) ...)

; 2. Get tree value, left, right
(define (tree-value tree) ...)
(define (tree-left tree) ...)
(define (tree-right tree) ...)

; 3. In-order traversal
(define (tree-inorder tree) ...)
; (tree-inorder example-tree) => '(1 3 4 5 6 7 9)

; 4. Tree height
(define (tree-height tree) ...)

; 5. Insert into BST
(define (bst-insert val tree) ...)

; 6. Search in BST
(define (bst-search val tree) ...)
```

---

## Challenge Exercises

### Exercise R.C1: Macro Magic
Write macros to extend Racket:

```racket
; 1. unless (opposite of when)
(define-syntax-rule (my-unless condition body ...) ...)
; (my-unless #f (displayln "ran!")) prints "ran!"

; 2. for-loop that collects results
(define-syntax-rule (collect-for ([var lst]) body ...) ...)
; (collect-for ([x '(1 2 3)]) (* x x)) => '(1 4 9)

; 3. with-logging that prints entry/exit
(define-syntax-rule (with-logging name body ...) ...)
; (with-logging "test" (+ 1 2))
; prints: "Entering test", "Leaving test", returns 3
```

### Exercise R.C2: Quine
Write a Racket program that prints its own source code.

### Exercise R.C3: Interpreter
Build a simple interpreter for a mini-language:

```racket
; Language:
; (val n) - number literal
; (add e1 e2) - addition
; (if e1 e2 e3) - conditional
; (var x) - variable reference
; (let x e1 e2) - local binding

(define (interpret expr env) ...)

; Examples:
; (interpret '(add (val 1) (val 2)) '()) => 3
; (interpret '(let x (val 5) (add (var x) (val 1))) '()) => 6
; (interpret '(if (val 0) (val 1) (val 2)) '()) => 2 (0 is falsy)
```

### Exercise R.C4: Y Combinator
Implement the Y combinator and use it to write recursive functions without explicit recursion:

```racket
; The Y combinator allows recursion in languages without built-in recursion
(define Y ...)

; Use it to define factorial without using define for recursion
(define factorial-y (Y (lambda (f) (lambda (n) ...))))
```

---

## Project: Mini Racket REPL

Build a Read-Eval-Print-Loop calculator that:

1. Reads expressions from standard input
2. Parses them (already handled by `read`)
3. Evaluates arithmetic expressions with variables
4. Supports:
   - Basic arithmetic: `+`, `-`, `*`, `/`
   - Variable definition: `(define x 5)`
   - Variable use: `x`
   - Quit command: `(quit)`

Example session:
```
> (+ 1 2)
3
> (* 3 4)
12
> (define x 10)
x = 10
> (+ x 5)
15
> (define y (+ x 1))
y = 11
> (* x y)
110
> (quit)
Goodbye!
```

### Starter Code

```racket
#lang racket

(define env (make-hash))

(define (my-eval expr)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (hash-ref env expr)]
    [(list? expr)
     (match expr
       [`(define ,var ,val-expr)
        (hash-set! env var (my-eval val-expr))
        (format "~a = ~a" var (hash-ref env var))]
       [`(+ ,a ,b) (+ (my-eval a) (my-eval b))]
       ; ... add more operations
       [_ (error "Unknown expression")])]
    [else (error "Invalid expression")]))

(define (repl)
  (display "> ")
  (let ([input (read)])
    (cond
      [(equal? input '(quit)) (displayln "Goodbye!")]
      [else
       (displayln (my-eval input))
       (repl)])))

(repl)
```

---

## Reflection Questions

1. How does prefix notation change the way you think about expressions?
2. What advantages does homoiconicity (code = data) provide?
3. How do Racket's macros compare to C++ templates or Python decorators?
4. When would you choose Racket over Python or Haskell?
5. What did you find most surprising about Lisp-style programming?
