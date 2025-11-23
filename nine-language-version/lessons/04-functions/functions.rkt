#!/usr/bin/env racket
#lang racket

; Lesson 4: Functions in Racket
; Demonstrates function definition, lambdas, closures, and higher-order functions

; ============================================
; 1. Basic Function Definition
; ============================================

(define (greet name)
  (string-append "Hello, " name "!"))

(define (add x y)
  (+ x y))

(define (square x)
  (* x x))

; ============================================
; 2. Parameters
; ============================================

; Optional parameters with default values
(define (describe-person name age [city "Unknown"])
  (format "~a is ~a years old and lives in ~a" name age city))

; Rest parameters (variadic)
(define (sum-all . numbers)
  (apply + numbers))

; Keyword arguments
(define (describe-person-kw #:name name #:age age #:city [city "Unknown"])
  (format "~a is ~a years old and lives in ~a" name age city))

; ============================================
; 3. Return Values
; ============================================

(define (divide x y)
  (if (= y 0)
      (values #f "Cannot divide by zero")  ; Multiple values
      (values (/ x y) #f)))

; ============================================
; 4. Anonymous Functions (Lambda)
; ============================================

; Lambda expression
(define double-lambda
  (lambda (x) (* x 2)))

; Shorthand (define already uses lambda internally)
(define triple
  (λ (x) (* x 3)))  ; λ is synonym for lambda

; ============================================
; 5. Closures
; ============================================

(define (make-multiplier factor)
  (lambda (x)
    (* x factor)))  ; Captures 'factor'

(define (make-adder n)
  (λ (x) (+ x n)))

(define (make-counter)
  (let ([count 0])  ; Private mutable variable
    (lambda ()
      (set! count (+ count 1))
      count)))

; ============================================
; 6. First-Class Functions
; ============================================

(define (apply-operation operation x y)
  (operation x y))

(define (compose f g)
  (lambda (x)
    (f (g x))))

; ============================================
; 7. Higher-Order Functions
; ============================================

(define (apply-twice f x)
  (f (f x)))

(define (apply-n-times f x n)
  (if (<= n 0)
      x
      (apply-n-times f (f x) (- n 1))))

; ============================================
; 8. Map, Filter, Fold (Built-in)
; ============================================

; map, filter, foldl are built-in
; map: (a -> b) -> [a] -> [b]
; filter: (a -> bool) -> [a] -> [a]
; foldl: (b a -> b) -> b -> [a] -> b

; ============================================
; 9. Function With Multiple Clauses
; ============================================

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Using match for pattern-like matching
(define (describe-number n)
  (match n
    [0 "Zero"]
    [1 "One"]
    [2 "Two"]
    [_ "Many"]))

; ============================================
; 10. Currying (Manual in Racket)
; ============================================

(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

; Binary add function for currying
(define (binary-add x y)
  (+ x y))

(define curried-add (curry binary-add))

; ============================================
; Main Program
; ============================================

(define (main)
  (displayln "=== Racket Functions ===")
  (newline)

  ; 1. Basic functions
  (displayln "1. Basic Functions:")
  (displayln (format "  (greet \"Alice\"): ~a" (greet "Alice")))
  (displayln (format "  (add 5 3): ~a" (add 5 3)))
  (displayln (format "  (square 7): ~a" (square 7)))

  ; 2. Parameters
  (displayln "\n2. Parameters:")
  (displayln (format "  (describe-person \"Alice\" 30): ~a" (describe-person "Alice" 30)))
  (displayln (format "  (describe-person \"Bob\" 25 \"NYC\"): ~a" (describe-person "Bob" 25 "NYC")))
  (displayln (format "  (sum-all 1 2 3 4 5): ~a" (sum-all 1 2 3 4 5)))

  (displayln "\n  Keyword arguments:")
  (displayln (format "  ~a" (describe-person-kw #:name "Charlie" #:age 35)))

  ; 3. Multiple returns
  (displayln "\n3. Multiple Return Values:")
  (let-values ([(result error) (divide 10 2)])
    (displayln (format "  (divide 10 2): result=~a, error=~a" result error)))
  (let-values ([(result error) (divide 10 0)])
    (displayln (format "  (divide 10 0): result=~a, error=~a" result error)))

  ; 4. Lambdas
  (displayln "\n4. Anonymous Functions (Lambdas):")
  (displayln (format "  (double-lambda 5): ~a" (double-lambda 5)))
  (displayln (format "  (triple 5): ~a" (triple 5)))
  (displayln (format "  ((lambda (x) (* x 2)) 5): ~a" ((lambda (x) (* x 2)) 5)))

  ; 5. Closures
  (displayln "\n5. Closures:")
  (define times-two (make-multiplier 2))
  (define times-three (make-multiplier 3))
  (displayln (format "  (times-two 5): ~a" (times-two 5)))
  (displayln (format "  (times-three 5): ~a" (times-three 5)))

  (define add-ten (make-adder 10))
  (displayln (format "  (add-ten 5): ~a" (add-ten 5)))

  ; Counter (stateful closure)
  (displayln "\n  Counter (stateful closure):")
  (define counter1 (make-counter))
  (define counter2 (make-counter))
  (displayln (format "  (counter1): ~a" (counter1)))  ; 1
  (displayln (format "  (counter1): ~a" (counter1)))  ; 2
  (displayln (format "  (counter2): ~a" (counter2)))  ; 1
  (displayln (format "  (counter1): ~a" (counter1)))  ; 3

  ; 6. First-class functions
  (displayln "\n6. First-Class Functions:")
  (displayln (format "  (apply-operation + 5 3): ~a" (apply-operation + 5 3)))
  (displayln (format "  (apply-operation * 5 3): ~a" (apply-operation * 5 3)))

  ; 7. Function composition
  (displayln "\n7. Function Composition:")
  (define increment (λ (x) (+ x 1)))
  (define square-then-increment (compose increment square))
  (displayln (format "  (square-then-increment 5): ~a" (square-then-increment 5)))

  ; 8. Higher-order functions
  (displayln "\n8. Higher-Order Functions:")
  (define double (λ (x) (* x 2)))
  (displayln (format "  (apply-twice double 5): ~a" (apply-twice double 5)))
  (displayln (format "  (apply-n-times double 5 3): ~a" (apply-n-times double 5 3)))

  ; 9. Map, filter, foldl
  (displayln "\n9. Map, Filter, Fold:")
  (define numbers '(1 2 3 4 5))
  (displayln (format "  numbers: ~a" numbers))
  (displayln (format "  (map square numbers): ~a" (map square numbers)))
  (displayln (format "  (filter even? numbers): ~a" (filter even? numbers)))
  (displayln (format "  (foldl + 0 numbers): ~a" (foldl + 0 numbers)))

  ; For/list comprehensions
  (displayln (format "  (for/list ([x numbers]) (* x x)): ~a"
                     (for/list ([x numbers]) (* x x))))
  (displayln (format "  (for/list ([x numbers] #:when (even? x)) x): ~a"
                     (for/list ([x numbers] #:when (even? x)) x)))

  ; 10. Pattern matching
  (displayln "\n10. Pattern Matching:")
  (displayln (format "  (describe-number 0): ~a" (describe-number 0)))
  (displayln (format "  (describe-number 2): ~a" (describe-number 2)))
  (displayln (format "  (describe-number 5): ~a" (describe-number 5)))

  ; 11. Currying
  (displayln "\n11. Currying:")
  (define add-five (curried-add 5))
  (displayln (format "  ((curried-add 5) 10): ~a" (add-five 10)))

  ; 12. Recursive functions
  (displayln "\n12. Recursive Functions:")
  (displayln (format "  (factorial 5): ~a" (factorial 5)))
  (displayln (format "  (factorial 0): ~a" (factorial 0))))

; Run main
(main)
