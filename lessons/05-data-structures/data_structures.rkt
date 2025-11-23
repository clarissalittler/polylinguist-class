#!/usr/bin/env racket
#lang racket

; Lesson 5: Data Structures in Racket
; Demonstrates immutable lists, vectors, hashes, sets

(define (main)
  (displayln "=== Racket Data Structures ===")
  (newline)

  ; 1. Lists (immutable, linked)
  (displayln "1. Lists (Immutable, Linked):")
  (define numbers '(1 2 3 4 5))
  (displayln (format "  Original: ~a" numbers))

  ; "Modifying" creates new list
  (define with-zero (cons 0 numbers))
  (displayln (format "  cons 0: ~a" with-zero))
  (displayln (format "  Original unchanged: ~a" numbers))

  (define with-six (append numbers '(6)))
  (displayln (format "  append 6: ~a" with-six))

  ; List operations
  (displayln "\n  List operations:")
  (displayln (format "  first: ~a" (first numbers)))
  (displayln (format "  rest: ~a" (rest numbers)))
  (displayln (format "  length: ~a" (length numbers)))
  (displayln (format "  reverse: ~a" (reverse numbers)))

  ; 2. Vectors (mutable, indexed)
  (displayln "\n2. Vectors (Mutable, Indexed):")
  (define vec (vector 1 2 3 4 5))
  (displayln (format "  Vector: ~a" vec))

  ; Modify (mutates!)
  (vector-set! vec 0 10)
  (displayln (format "  After vector-set! 0 10: ~a" vec))

  ; Immutable version
  (define immut-vec (vector->immutable-vector (vector 1 2 3)))
  (displayln (format "  Immutable vector: ~a" immut-vec))

  ; 3. List operations (functional)
  (displayln "\n3. List Operations (Functional):")
  (define nums '(1 2 3 4 5))

  (define squared (map (lambda (x) (* x x)) nums))
  (displayln (format "  map square: ~a" squared))

  (define evens (filter even? nums))
  (displayln (format "  filter even: ~a" evens))

  (define sum (foldl + 0 nums))
  (displayln (format "  foldl +: ~a" sum))

  ; 4. Hashes (immutable by default)
  (displayln "\n4. Hashes (Immutable by Default):")
  (define person (hash 'name "Alice" 'age 30 'city "NYC"))
  (displayln (format "  Person: ~a" person))

  ; "Modify" - creates new hash
  (define updated (hash-set person 'age 31))
  (displayln (format "  After hash-set age 31: ~a" updated))
  (displayln (format "  Original unchanged: ~a" person))

  ; Access
  (displayln (format "  hash-ref 'name: ~a" (hash-ref person 'name)))
  (displayln (format "  hash-has-key? 'age: ~a" (hash-has-key? person 'age)))

  ; Mutable hash
  (displayln "\n  Mutable hash:")
  (define mutable-hash (make-hash))
  (hash-set! mutable-hash 'name "Bob")
  (hash-set! mutable-hash 'age 25)
  (displayln (format "  Mutable hash: ~a" mutable-hash))

  ; 5. Sets (immutable)
  (displayln "\n5. Sets (Immutable):")
  (define numbers-set (set 1 2 3 4 5))
  (displayln (format "  Set: ~a" (set->list numbers-set)))

  (define with-six-set (set-add numbers-set 6))
  (displayln (format "  After set-add 6: ~a" (set->list with-six-set)))

  (displayln (format "  set-member? 3: ~a" (set-member? numbers-set 3)))
  (displayln (format "  set-count: ~a" (set-count numbers-set)))

  ; Set operations
  (define evens-set (set 2 4 6 8))
  (define odds-set (set 1 3 5 7))
  (displayln (format "  union: ~a" (set->list (set-union evens-set odds-set))))
  (displayln (format "  intersect: ~a" (set->list (set-intersect evens-set odds-set))))
  (displayln (format "  subtract: ~a" (set->list (set-subtract evens-set odds-set))))

  ; 6. Pairs and cons cells
  (displayln "\n6. Pairs and Cons Cells:")
  (define pair (cons 3 4))
  (displayln (format "  Pair: ~a" pair))
  (displayln (format "  car: ~a, cdr: ~a" (car pair) (cdr pair)))

  ; Lists are chains of cons cells
  (define list-manual (cons 1 (cons 2 (cons 3 '()))))
  (displayln (format "  Manual list: ~a" list-manual))

  ; 7. Structs (custom data types)
  (displayln "\n7. Structs (Custom Data Types):")
  (struct point (x y) #:transparent)
  (define p (point 3 4))
  (displayln (format "  Point: ~a" p))
  (displayln (format "  x: ~a, y: ~a" (point-x p) (point-y p)))

  ; 8. For loops and comprehensions
  (displayln "\n8. For Loops and Comprehensions:")
  (define squares
    (for/list ([x (in-range 1 6)])
      (* x x)))
  (displayln (format "  for/list squares: ~a" squares))

  (define evens-comp
    (for/list ([x (in-range 1 11)]
               #:when (even? x))
      x))
  (displayln (format "  for/list evens: ~a" evens-comp))

  ; 9. Pattern matching on lists
  (displayln "\n9. Pattern Matching on Lists:")
  (displayln (format "  describe-list '(): ~a" (describe-list '())))
  (displayln (format "  describe-list '(1): ~a" (describe-list '(1))))
  (displayln (format "  describe-list '(1 2 3): ~a" (describe-list '(1 2 3))))

  ; 10. Key insights
  (displayln "\n10. Key Insights:")
  (displayln "  - Lists are immutable by default")
  (displayln "  - Vectors provide indexed, mutable access")
  (displayln "  - Hashes and sets can be mutable or immutable")
  (displayln "  - cons cells are fundamental building blocks")
  (displayln "  - Pattern matching works beautifully with lists")
  (displayln "  - Functional operations (map, filter, fold) are idiomatic"))

; Helper function
(define (describe-list lst)
  (match lst
    ['() "Empty"]
    [(list _) "Singleton"]
    [(list _ _) "Pair"]
    [_ "Many elements"]))

; Run main
(main)
