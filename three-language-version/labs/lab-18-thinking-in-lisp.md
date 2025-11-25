# Lab 18: Thinking in Lisp

**Quarter 2, Week 7**
**Duration:** 90 minutes
**Format:** Exploratory, individual with discussion

## Overview

Lisp is one of the oldest programming languages (1958!) and has influenced nearly every language that came after. Racket is a modern Lisp that's great for learning. This lab introduces the Lisp way of thinking.

## Objectives

By the end of this lab, you will:
- [ ] Write basic Racket programs
- [ ] Understand prefix notation and S-expressions
- [ ] Use recursion as the primary looping mechanism
- [ ] Appreciate "code as data"

## Setup

- Install Racket: https://racket-lang.org/
- Open DrRacket IDE
- Create file: `lab18-lisp.rkt`
- Add `#lang racket` at the top

---

## Part 1: Prefix Notation (20 minutes)

### Activity 1.1: Everything is a Function Call

In Lisp, EVERYTHING uses prefix notation: `(function arg1 arg2 ...)`

```racket
#lang racket

; Arithmetic
(+ 1 2)           ; 3
(- 10 3)          ; 7
(* 4 5)           ; 20
(/ 15 3)          ; 5

; Multiple arguments!
(+ 1 2 3 4 5)     ; 15
(* 2 3 4)         ; 24

; Nested expressions
(+ (* 2 3) (* 4 5))   ; 6 + 20 = 26
(/ (+ 10 20) (- 8 3)) ; 30 / 5 = 6
```

### Activity 1.2: No Infix, No Problem

**Math class:** `(3 + 4) * (5 - 2)`

**Racket:** `(* (+ 3 4) (- 5 2))`

```racket
; The formula: area = π * r²
(define (circle-area r)
  (* pi r r))

(circle-area 5)  ; 78.53981633974483

; The formula: distance = √((x₂-x₁)² + (y₂-y₁)²)
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1))
           (sqr (- y2 y1)))))

(distance 0 0 3 4)  ; 5.0
```

### Activity 1.3: Practice - Convert These Formulas

Convert to Racket:

1. `5 + 3 * 2` = ?
2. `(10 - 4) / 2` = ?
3. `2³ + 4²` = ? (hint: use `expt` for power)
4. Quadratic formula: `(-b + √(b² - 4ac)) / (2a)`

```racket
; Your answers:
; 1.
; 2.
; 3.
; 4. (define (quadratic-plus a b c) ...)
```

### ✅ Checkpoint 1

Verify:
- [ ] Can write arithmetic in prefix notation
- [ ] Nested expressions work
- [ ] Completed practice problems

---

## Part 2: Functions and Definitions (20 minutes)

### Activity 2.1: Defining Functions

```racket
; Define a function
(define (square x)
  (* x x))

(square 5)    ; 25
(square -3)   ; 9

; Multiple parameters
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)  ; 9 + 16 = 25

; Using local definitions
(define (hypotenuse a b)
  (sqrt (sum-of-squares a b)))

(hypotenuse 3 4)  ; 5.0
```

### Activity 2.2: Conditionals

```racket
; if expression (not statement!)
(if (> 5 3)
    "yes"
    "no")  ; "yes"

; cond for multiple conditions
(define (grade score)
  (cond
    [(>= score 90) "A"]
    [(>= score 80) "B"]
    [(>= score 70) "C"]
    [(>= score 60) "D"]
    [else "F"]))

(grade 85)  ; "B"
(grade 45)  ; "F"

; Boolean operations
(and #t #f)      ; #f (false)
(or #t #f)       ; #t (true)
(not #t)         ; #f

; Comparison
(= 5 5)          ; #t
(< 3 5)          ; #t
(string=? "hi" "hi")  ; #t
```

### Activity 2.3: Let Bindings

```racket
; Local variables with let
(define (circle-stats r)
  (let ([area (* pi r r)]
        [circumference (* 2 pi r)])
    (list area circumference)))

(circle-stats 5)  ; '(78.53... 31.41...)

; let* for sequential bindings
(define (quadratic a b c)
  (let* ([discriminant (- (* b b) (* 4 a c))]
         [sqrt-disc (sqrt discriminant)]
         [x1 (/ (+ (- b) sqrt-disc) (* 2 a))]
         [x2 (/ (- (- b) sqrt-disc) (* 2 a))])
    (list x1 x2)))

(quadratic 1 -5 6)  ; '(3 2)
```

### Activity 2.4: Practice - Write These Functions

```racket
; 1. absolute value (without using abs)
(define (my-abs x)
  ; your code
  )

; 2. sign of a number (-1, 0, or 1)
(define (sign x)
  ; your code
  )

; 3. leap year checker
(define (leap-year? year)
  ; divisible by 4, except centuries unless divisible by 400
  )

; 4. fizzbuzz for a single number
(define (fizzbuzz n)
  ; your code
  )
```

### ✅ Checkpoint 2

Verify:
- [ ] Can define functions
- [ ] Understand if and cond
- [ ] Completed practice functions

---

## Part 3: Lists and Recursion (25 minutes)

### Activity 3.1: Lists

```racket
; Creating lists
(list 1 2 3 4 5)      ; '(1 2 3 4 5)
'(1 2 3 4 5)          ; same thing (quoted list)
(cons 1 '(2 3))       ; '(1 2 3) - add to front
(append '(1 2) '(3 4)); '(1 2 3 4)

; Accessing lists
(first '(1 2 3))      ; 1
(rest '(1 2 3))       ; '(2 3)
(second '(1 2 3))     ; 2
(last '(1 2 3))       ; 3

; List predicates
(empty? '())          ; #t
(cons? '(1 2))        ; #t (is it a cons cell?)
(list? '(1 2 3))      ; #t

; List info
(length '(1 2 3 4))   ; 4
```

### Activity 3.2: Recursion on Lists

In Lisp, recursion is the natural way to process lists:

```racket
; Sum a list
(define (my-sum lst)
  (if (empty? lst)
      0
      (+ (first lst) (my-sum (rest lst)))))

(my-sum '(1 2 3 4 5))  ; 15

; Length of a list
(define (my-length lst)
  (if (empty? lst)
      0
      (+ 1 (my-length (rest lst)))))

(my-length '(a b c d))  ; 4

; Map a function over a list
(define (my-map f lst)
  (if (empty? lst)
      '()
      (cons (f (first lst))
            (my-map f (rest lst)))))

(my-map square '(1 2 3 4))  ; '(1 4 9 16)
```

### Activity 3.3: More List Functions

```racket
; Filter a list
(define (my-filter pred lst)
  (cond
    [(empty? lst) '()]
    [(pred (first lst))
     (cons (first lst) (my-filter pred (rest lst)))]
    [else (my-filter pred (rest lst))]))

(my-filter even? '(1 2 3 4 5 6))  ; '(2 4 6)

; Reduce (fold) a list
(define (my-reduce f init lst)
  (if (empty? lst)
      init
      (f (first lst) (my-reduce f init (rest lst)))))

(my-reduce + 0 '(1 2 3 4 5))  ; 15
(my-reduce * 1 '(1 2 3 4 5))  ; 120

; Built-in equivalents
(map square '(1 2 3 4))           ; '(1 4 9 16)
(filter even? '(1 2 3 4 5 6))     ; '(2 4 6)
(foldl + 0 '(1 2 3 4 5))          ; 15
```

### Activity 3.4: Practice - List Functions

```racket
; 1. Reverse a list
(define (my-reverse lst)
  ; your code
  )

; 2. Check if element is in list
(define (my-member? x lst)
  ; your code
  )

; 3. Remove duplicates
(define (remove-dups lst)
  ; your code
  )

; 4. Take first n elements
(define (my-take n lst)
  ; your code
  )
```

### ✅ Checkpoint 3

Verify:
- [ ] Can create and access lists
- [ ] Understand recursive list processing
- [ ] Implemented practice functions

---

## Part 4: Code as Data (15 minutes)

### Activity 4.1: Homoiconicity

Lisp's secret power: code and data have the same structure!

```racket
; This is data (a quoted list):
'(+ 1 2)              ; '(+ 1 2)

; This is code:
(+ 1 2)               ; 3

; We can manipulate "code" as data:
(define code '(+ 1 2))
(first code)          ; '+
(second code)         ; 1
(third code)          ; 2

; And then evaluate it:
(eval code)           ; 3

; Build code dynamically!
(define (make-addition a b)
  (list '+ a b))

(make-addition 3 4)      ; '(+ 3 4)
(eval (make-addition 3 4)) ; 7
```

### Activity 4.2: Simple Expression Evaluator

```racket
; Our own tiny evaluator!
(define (my-eval expr)
  (cond
    [(number? expr) expr]  ; Numbers evaluate to themselves
    [(list? expr)
     (let ([op (first expr)]
           [args (map my-eval (rest expr))])
       (cond
         [(eq? op '+) (apply + args)]
         [(eq? op '*) (apply * args)]
         [(eq? op '-) (apply - args)]
         [else (error "Unknown operator")]))]
    [else (error "Unknown expression")]))

(my-eval '(+ 1 2))              ; 3
(my-eval '(* 3 4))              ; 12
(my-eval '(+ (* 2 3) (* 4 5)))  ; 26
```

### Activity 4.3: Macros Preview

Macros let you extend the language:

```racket
; Define a simple macro
(define-syntax-rule (when condition body ...)
  (if condition
      (begin body ...)
      (void)))

; Use it like built-in syntax
(when (> 5 3)
  (displayln "Five is greater")
  (displayln "Math works!"))

; Another macro: unless
(define-syntax-rule (unless condition body ...)
  (if (not condition)
      (begin body ...)
      (void)))

(unless (= 5 6)
  (displayln "They're not equal"))
```

---

## Part 5: Putting It Together (10 minutes)

### Activity 5.1: A Complete Program

```racket
#lang racket

; Student record as a list
(define (make-student name grade)
  (list name grade))

(define (student-name s) (first s))
(define (student-grade s) (second s))

; Sample data
(define students
  (list (make-student "Alice" 85)
        (make-student "Bob" 72)
        (make-student "Charlie" 90)
        (make-student "Diana" 68)))

; Average grade
(define (average-grade students)
  (/ (foldl + 0 (map student-grade students))
     (length students)))

; Passing students (grade >= 70)
(define (passing students)
  (filter (λ (s) (>= (student-grade s) 70)) students))

; Display results
(displayln "All students:")
(for-each (λ (s) (printf "  ~a: ~a~n" (student-name s) (student-grade s)))
          students)

(printf "~nAverage: ~a~n" (average-grade students))

(displayln "\nPassing students:")
(for-each (λ (s) (printf "  ~a~n" (student-name s)))
          (passing students))
```

### Activity 5.2: Compare to Other Languages

**Haskell:**
```haskell
students = [("Alice", 85), ("Bob", 72), ("Charlie", 90), ("Diana", 68)]
average = sum (map snd students) `div` length students
passing = filter (\(_, g) -> g >= 70) students
```

**Python:**
```python
students = [("Alice", 85), ("Bob", 72), ("Charlie", 90), ("Diana", 68)]
average = sum(g for _, g in students) / len(students)
passing = [(n, g) for n, g in students if g >= 70]
```

**Notice the similarities in functional style!**

---

## Challenges

### Challenge 1: Tree Operations

```racket
; Trees as nested lists: (value left right)
(define tree '(5 (3 (1 () ()) (4 () ())) (7 (6 () ()) (9 () ()))))

; Implement:
(define (tree-sum tree) ...)      ; Sum all values
(define (tree-height tree) ...)   ; Height of tree
(define (tree-inorder tree) ...)  ; Inorder traversal
```

### Challenge 2: Interpreter

Extend my-eval to support:
- Variables: `(let ((x 5)) (+ x 3))`
- Comparison: `(< 3 5)`, `(= 2 2)`
- Conditionals: `(if (< 3 5) 10 20)`

### Challenge 3: DSL

Create a domain-specific language for describing shapes:

```racket
(draw
  (circle 50 50 30)
  (rectangle 100 100 50 25)
  (line 0 0 200 200))
```

---

## Wrap-Up

**Key takeaways:**

1. **Prefix notation** - Everything is `(function args...)`
2. **Lists everywhere** - The fundamental data structure
3. **Recursion** - The natural way to loop
4. **Code = Data** - Programs can manipulate programs
5. **Minimalism** - Few special forms, maximum expressiveness

**Why learn Lisp?**
- Expands how you think about programming
- Influenced: Python, Ruby, JavaScript, Haskell
- Macros are uniquely powerful
- Great for DSLs and interpreters

**Paul Graham:** "Lisp is worth learning for the profound enlightenment experience you will have when you finally get it."

**Next lab:** Text Processing - practical string manipulation!
