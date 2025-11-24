#lang racket

;; Lesson 11: Error Handling in Racket - Exceptions and contracts

(displayln "=== Error Handling in Racket ===\n")

;; 1. Basic exception handling
(displayln "1. Exception handling:")
(with-handlers ([exn:fail? (lambda (e) (displayln "   Error caught"))])
  (error "Something went wrong"))

;; 2. Specific exception types
(displayln "\n2. Specific exceptions:")
(with-handlers ([exn:fail:contract? (lambda (e) (displayln "   Contract violation"))])
  (/ 10 2)
  (displayln "   Success"))

;; 3. Contracts for runtime checking
(displayln "\n3. Contracts:")
(define/contract (divide x y)
  (-> number? (and/c number? (not/c zero?)) number?)
  (/ x y))

(displayln (format "   divide(10, 2) = ~a" (divide 10 2)))

;; Uncomment to see contract violation:
;; (divide 10 0)  ;; Contract violation!

(displayln "\n=== Racket Error Handling ===")
(displayln "- with-handlers for exceptions")
(displayln "- Contracts for runtime checking")
(displayln "- Specific exception types")
