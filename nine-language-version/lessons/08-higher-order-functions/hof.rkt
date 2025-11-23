#lang racket

;; Lesson 8: Higher-Order Functions in Racket
;;
;; Racket (a Lisp dialect) is naturally functional:
;; - Functions are first-class
;; - map, filter, fold built-in
;; - λ (lambda) is a core construct
;; - Currying and partial application
;; - Function composition

;; ====================
;; 1. Functions as First-Class Values
;; ====================

(define (demonstrate-first-class)
  (define greet (λ (name) (format "Hello, ~a!" name)))

  (displayln (format "   ~a" (greet "Alice")))

  ;; Store in list
  (define operations (list (λ (x) (* x 2))
                           (λ (x) (+ x 1))
                           (λ (x) (* x x))))

  (displayln (format "   Apply all to 5: ~a" (map (λ (f) (f 5)) operations))))

;; ====================
;; 2. Functions Taking Functions
;; ====================

(define (apply-twice func x)
  (func (func x)))

(define (apply-n-times func n x)
  (if (= n 0)
      x
      (apply-n-times func (- n 1) (func x))))

;; ====================
;; 3. Functions Returning Functions
;; ====================

(define (make-multiplier n)
  (λ (x) (* x n)))

(define (make-adder n)
  (λ (x) (+ x n)))

;; ====================
;; 4. Map - Transform Each Element
;; ====================

(define (demonstrate-map)
  (define numbers '(1 2 3 4 5))

  ;; Map with lambda
  (displayln (format "   Doubled: ~a" (map (λ (x) (* x 2)) numbers)))

  ;; Map with procedure
  (displayln (format "   Squared: ~a" (map sqr numbers)))

  ;; Chaining maps
  (displayln (format "   Chained: ~a"
                     (map (λ (x) (* x 2))
                          (map (λ (x) (+ x 1)) numbers)))))

;; ====================
;; 5. Filter - Select Elements
;; ====================

(define (demonstrate-filter)
  (define numbers '(1 2 3 4 5 6 7 8 9 10))

  (displayln (format "   Evens: ~a" (filter even? numbers)))
  (displayln (format "   Odds: ~a" (filter odd? numbers)))
  (displayln (format "   Big odds: ~a"
                     (filter (λ (x) (and (odd? x) (> x 5))) numbers))))

;; ====================
;; 6. Fold (Reduce)
;; ====================

(define (demonstrate-fold)
  (define numbers '(1 2 3 4 5))

  ;; foldl - fold left
  (displayln (format "   Sum: ~a" (foldl + 0 numbers)))
  (displayln (format "   Product: ~a" (foldl * 1 numbers)))

  ;; foldr - fold right
  (define words '("Hello" "world" "from" "Racket"))
  (displayln (format "   foldr: ~a"
                     (foldr (λ (w acc) (string-append w " " acc)) "" words)))
  (displayln (format "   foldl: ~a"
                     (foldl (λ (w acc) (string-append acc " " w)) "" words))))

;; ====================
;; 7. Closures
;; ====================

(define (make-counter)
  (let ([count 0])
    (λ ()
      (set! count (+ count 1))
      count)))

(define (make-bank-account initial-balance)
  (let ([balance initial-balance])
    (λ (operation amount)
      (cond
        [(eq? operation 'deposit)
         (when (> amount 0)
           (set! balance (+ balance amount)))
         balance]
        [(eq? operation 'withdraw)
         (when (and (> amount 0) (<= amount balance))
           (set! balance (- balance amount)))
         balance]
        [(eq? operation 'balance)
         balance]))))

;; ====================
;; 8. Currying
;; ====================

(define (demonstrate-curry)
  ;; Manual currying
  (define curried-add
    (λ (a)
      (λ (b)
        (λ (c)
          (+ a b c)))))

  (displayln (format "   Curried: ~a" (((curried-add 1) 2) 3)))

  ;; Racket's curry
  (define (power base exp)
    (expt base exp))

  (define curried-power (curry power))
  (define square (curried-power 2))

  (displayln (format "   square 5 = ~a" (square 5))))

;; ====================
;; 9. Function Composition
;; ====================

(define (demonstrate-composition)
  (define add-one (λ (x) (+ x 1)))
  (define double (λ (x) (* x 2)))

  ;; Racket's compose
  (define f (compose double add-one))
  (displayln (format "   compose(double, add-one)(5) = ~a" (f 5)))

  ;; Chain multiple
  (define pipeline (compose sqr double add-one))
  (displayln (format "   Complex pipeline(5) = ~a" (pipeline 5))))

;; ====================
;; 10. Common Higher-Order Functions
;; ====================

(define (demonstrate-common)
  (define numbers '(1 2 3 4 5))

  ;; andmap - all elements satisfy predicate
  (displayln (format "   andmap positive? ~a" (andmap positive? numbers)))

  ;; ormap - any element satisfies predicate
  (displayln (format "   ormap even? ~a" (ormap even? numbers)))

  ;; findf - first element matching
  (displayln (format "   findf even? ~a" (findf even? numbers)))

  ;; partition - split into matching/non-matching
  (define-values (evens odds) (partition even? numbers))
  (displayln (format "   partition: evens=~a, odds=~a" evens odds))

  ;; sort with key
  (define words '("apple" "pie" "zoo" "a"))
  (displayln (format "   sort by length: ~a"
                     (sort words < #:key string-length))))

;; ====================
;; 11. Custom HOFs
;; ====================

(define (custom-map func lst)
  (if (null? lst)
      '()
      (cons (func (car lst))
            (custom-map func (cdr lst)))))

(define (custom-filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst))
     (cons (car lst) (custom-filter pred (cdr lst)))]
    [else (custom-filter pred (cdr lst))]))

(define (custom-foldl func init lst)
  (if (null? lst)
      init
      (custom-foldl func (func (car lst) init) (cdr lst))))

;; ====================
;; 12. Real-World Example
;; ====================

(struct user (name age active) #:transparent)

(define (process-users users)
  (take
   (sort
    (map (λ (u)
           (struct-copy user u
                        [name (string-downcase (string-trim (user-name u)))]))
         (filter user-active users))
    >
    #:key user-age)
   10))

;; ====================
;; Main Demonstration
;; ====================

(define (main)
  (displayln "=== Higher-Order Functions in Racket ===\n")

  ;; 1. First-class
  (displayln "1. Functions as First-Class Values:")
  (demonstrate-first-class)

  ;; 2. Functions taking functions
  (displayln "\n2. Functions Taking Functions:")
  (displayln (format "   apply-twice (λ (x) (+ x 1)) 5 = ~a"
                     (apply-twice (λ (x) (+ x 1)) 5)))
  (displayln (format "   apply-n-times (λ (x) (* x 2)) 3 2 = ~a"
                     (apply-n-times (λ (x) (* x 2)) 3 2)))

  ;; 3. Functions returning functions
  (displayln "\n3. Functions Returning Functions:")
  (define times-three (make-multiplier 3))
  (define add-ten (make-adder 10))
  (displayln (format "   times-three 7 = ~a" (times-three 7)))
  (displayln (format "   add-ten 5 = ~a" (add-ten 5)))

  ;; 4. Map
  (displayln "\n4. Map - Transform Each Element:")
  (demonstrate-map)

  ;; 5. Filter
  (displayln "\n5. Filter - Select Elements:")
  (demonstrate-filter)

  ;; 6. Fold
  (displayln "\n6. Fold - Combine to Single Value:")
  (demonstrate-fold)

  ;; 7. Closures
  (displayln "\n7. Closures:")
  (define counter (make-counter))
  (displayln (format "   counter() = ~a" (counter)))
  (displayln (format "   counter() = ~a" (counter)))

  (define account (make-bank-account 1000))
  (displayln (format "   Initial: $~a" (account 'balance 0)))
  (account 'deposit 500)
  (displayln (format "   After deposit: $~a" (account 'balance 0)))

  ;; 8. Currying
  (displayln "\n8. Currying:")
  (demonstrate-curry)

  ;; 9. Composition
  (displayln "\n9. Function Composition:")
  (demonstrate-composition)

  ;; 10. Common HOFs
  (displayln "\n10. Common Higher-Order Functions:")
  (demonstrate-common)

  ;; 11. Custom implementations
  (displayln "\n11. Custom HOF Implementations:")
  (define numbers '(1 2 3 4 5))
  (displayln (format "   custom-map: ~a" (custom-map (λ (x) (* x 2)) numbers)))
  (displayln (format "   custom-filter: ~a" (custom-filter even? numbers)))
  (displayln (format "   custom-foldl: ~a" (custom-foldl + 0 numbers)))

  ;; 12. Real-world example
  (displayln "\n12. Real-World Data Pipeline:")
  (define users (list (user " Alice " 25 #t)
                     (user "BOB" 17 #t)
                     (user "Charlie " 30 #f)
                     (user "DIANA" 28 #t)))
  (displayln (format "   Processed: ~a" (process-users users))))

(main)
