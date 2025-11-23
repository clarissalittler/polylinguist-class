#lang racket

;; Lesson 10: Type Systems in Racket
;;
;; Racket has two dialects:
;; - Untyped Racket (dynamic typing)
;; - Typed Racket (static typing with inference)
;;
;; This shows untyped Racket with comments on Typed Racket.

(displayln "=== Type Systems in Racket ===\n")

;; ====================
;; 1. Dynamic Typing (Untyped Racket)
;; ====================

(define (increment x)
  (+ x 1))

(define (add x y)
  (+ x y))

(displayln "1. Dynamic Typing:")
(displayln (format "   increment(5) = ~a" (increment 5)))
(displayln (format "   add(3, 7) = ~a" (add 3 7)))

;; ====================
;; 2. Generic Functions
;; ====================

(define (identity x) x)

(define (first lst)
  (if (null? lst)
      #f
      (car lst)))

(displayln "\n2. Generic Functions:")
(displayln (format "   identity(42) = ~a" (identity 42)))
(displayln (format "   identity('hello') = ~a" (identity "hello")))
(displayln (format "   first([1,2,3]) = ~a" (first '(1 2 3))))

;; ====================
;; 3. Contracts (Runtime Type Checking)
;; ====================

(provide
 (contract-out
  [safe-divide (-> number? number? (or/c number? false?))]))

(define (safe-divide x y)
  (if (= y 0)
      #f
      (/ x y)))

(displayln "\n3. Contracts (Runtime Checks):")
(displayln (format "   safe-divide(10, 2) = ~a" (safe-divide 10 2)))
(displayln (format "   safe-divide(10, 0) = ~a" (safe-divide 10 0)))

;; ====================
;; 4. Structs (Product Types)
;; ====================

(struct point (x y) #:transparent)

(define (point-distance p1 p2)
  (sqrt (+ (sqr (- (point-x p2) (point-x p1)))
           (sqr (- (point-y p2) (point-y p1))))))

(displayln "\n4. Structs:")
(define p1 (point 0 0))
(define p2 (point 3 4))
(displayln (format "   ~a" p1))
(displayln (format "   distance = ~a" (point-distance p1 p2)))

;; ====================
;; 5. Typed Racket Example (Commented)
;; ====================

(displayln "\n5. Typed Racket (#lang typed/racket):")
(displayln "   Typed Racket adds static types:")
(displayln "   #lang typed/racket")
(displayln "   (: increment (-> Integer Integer))")
(displayln "   (define (increment x) (+ x 1))")
(displayln "   ")
(displayln "   (: identity (All (A) (-> A A)))")
(displayln "   (define (identity x) x)")

;; ====================
;; 6. Predicates for Type Checking
;; ====================

(displayln "\n6. Predicates:")
(displayln (format "   (number? 42) = ~a" (number? 42)))
(displayln (format "   (string? 'hello') = ~a" (string? "hello")))
(displayln (format "   (list? [1,2,3]) = ~a" (list? '(1 2 3))))
(displayln (format "   (point? p1) = ~a" (point? p1)))

;; ====================
;; Summary
;; ====================

(displayln "\n=== Racket Type System Features ===")
(displayln "Untyped Racket:")
(displayln "- Dynamic typing")
(displayln "- Runtime contracts")
(displayln "- Predicates for type checking")
(displayln "")
(displayln "Typed Racket:")
(displayln "- Static typing with inference")
(displayln "- Polymorphic types (All)")
(displayln "- Occurrence typing (flow-sensitive)")
(displayln "- Gradual typing with untyped code")
