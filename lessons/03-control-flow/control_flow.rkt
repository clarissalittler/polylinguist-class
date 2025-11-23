#!/usr/bin/env racket
#lang racket

; Lesson 3: Control Flow in Racket
; Demonstrates conditionals, recursion, and functional patterns

(define (main)
  (displayln "=== Racket Control Flow ===")
  (newline)

  ; 1. Basic conditionals (if is an expression)
  (displayln "1. Conditionals (if expressions):")
  (define age 20)
  (define description
    (if (>= age 18)
        "Adult"
        (if (>= age 13)
            "Teenager"
            "Child")))
  (displayln (format "  Age ~a: ~a" age description))

  ; One-line if
  (define status (if (>= age 18) "Adult" "Minor"))
  (displayln (format "  Status: ~a" status))

  ; 2. Cond (cleaner for multiple conditions)
  (displayln "\n2. Cond (preferred for multiple conditions):")
  (define (describe-age age)
    (cond
      [(>= age 18) "Adult"]
      [(>= age 13) "Teenager"]
      [else "Child"]))
  (displayln (format "  Age ~a: ~a" age (describe-age age)))

  ; 3. No traditional loops - use recursion
  (displayln "\n3. Recursion (no traditional loops):")
  (displayln "  Count to 5:")
  (display "   ")
  (print-numbers 0 5)
  (newline)

  (displayln "  Iterate list:")
  (define fruits '("apple" "banana" "cherry"))
  (for-each
   (lambda (fruit)
     (displayln (format "    ~a" fruit)))
   fruits)

  ; 4. Named let (tail recursion)
  (displayln "\n4. Named Let (tail recursion):")
  (displayln "  Count to 5:")
  (display "   ")
  (let loop ([i 0])
    (when (< i 5)
      (display (format " ~a" i))
      (loop (+ i 1))))
  (newline)

  ; 5. For loops (Racket syntax)
  (displayln "\n5. For Loops (Racket style):")
  (displayln "  For/list (build a list):")
  (define squares
    (for/list ([x (in-range 1 6)])
      (* x x)))
  (displayln (format "   ~a" squares))

  (displayln "  For (side effects):")
  (display "   ")
  (for ([i (in-range 5)])
    (display (format " ~a" i)))
  (newline)

  ; 6. Boolean logic
  (displayln "\n6. Boolean Logic:")
  (define x 5)
  (define y 10)
  (displayln (format "  x=~a, y=~a" x y))
  (displayln (format "  (and (> x 3) (< y 20)): ~a" (and (> x 3) (< y 20))))
  (displayln (format "  (or (> x 10) (> y 5)): ~a" (or (> x 10) (> y 5))))
  (displayln (format "  (not (= x y)): ~a" (not (= x y))))

  (displayln "\n  Truthiness (only #f is falsy):")
  (define values (list #t #f 0 1 "" "hello" '() '(1 2)))
  (for-each
   (lambda (val)
     (define truthy (if val "truthy" "falsy"))
     (displayln (format "    ~s: ~a" val truthy)))
   values)

  ; 7. FizzBuzz
  (displayln "\n7. FizzBuzz (1-20):")
  (display " ")
  (for ([i (in-range 1 21)])
    (display (format " ~a" (fizzbuzz i))))
  (newline)

  ; 8. Pattern matching
  (displayln "\n8. Pattern Matching (match):")
  (displayln (format "  (0, 0): ~a" (describe-point '(0 0))))
  (displayln (format "  (0, 5): ~a" (describe-point '(0 5))))
  (displayln (format "  (3, 4): ~a" (describe-point '(3 4))))

  (displayln "\n  Pattern matching on lists:")
  (displayln (format "  '(): ~a" (describe-list '())))
  (displayln (format "  '(1): ~a" (describe-list '(1))))
  (displayln (format "  '(1 2 3): ~a" (describe-list '(1 2 3)))))

; Recursion example
(define (print-numbers current limit)
  (when (< current limit)
    (display (format " ~a" current))
    (print-numbers (+ current 1) limit)))

; FizzBuzz with cond
(define (fizzbuzz n)
  (cond
    [(= (modulo n 15) 0) "FizzBuzz"]
    [(= (modulo n 3) 0) "Fizz"]
    [(= (modulo n 5) 0) "Buzz"]
    [else (number->string n)]))

; Pattern matching example
(define (describe-point point)
  (match point
    ['(0 0) "Origin"]
    [(list 0 y) (format "Y-axis at ~a" y)]
    [(list x 0) (format "X-axis at ~a" x)]
    [(list x y) (format "Point at (~a, ~a)" x y)]))

; Pattern matching on lists
(define (describe-list lst)
  (match lst
    ['() "Empty list"]
    [(list _) "Single element"]
    [(list _ _) "Two elements"]
    [(list x y rest ...) (format "Starts with ~a and ~a" x y)]))

; Run main
(main)
