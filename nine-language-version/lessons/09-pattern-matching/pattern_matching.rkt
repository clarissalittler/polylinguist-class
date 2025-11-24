#lang racket

;; Lesson 9: Pattern Matching in Racket
;;
;; Racket has excellent pattern matching built-in!
;; - match expression for pattern matching
;; - Destructuring lists, structs, and more
;; - Guard clauses with #:when
;; - Nested patterns
;; - Pattern-based function definitions (match-lambda)

;; ====================
;; 1. Basic Match Expression
;; ====================

(define (describe-number n)
  (match n
    [0 "zero"]
    [1 "one"]
    [2 "two"]
    [_ (format "many: ~a" n)]))

;; ====================
;; 2. Tuple/List Patterns
;; ====================

(define (describe-point point)
  (match point
    ['(0 0) "origin"]
    [`(0 ,y) (format "on y-axis at y=~a" y)]
    [`(,x 0) (format "on x-axis at x=~a" x)]
    [`(,x ,y) (format "point at (~a, ~a)" x y)]))

(define (describe-list lst)
  (match lst
    ['() "empty list"]
    [(list x) (format "single element: ~a" x)]
    [(list x y) (format "two elements: ~a, ~a" x y)]
    [(cons first rest) (format "first: ~a, rest: ~a" first rest)]))

;; ====================
;; 3. Struct Patterns
;; ====================

(struct circle (radius) #:transparent)
(struct rectangle (width height) #:transparent)
(struct triangle (a b c) #:transparent)

(define (area shape)
  (match shape
    [(circle r) (* pi r r)]
    [(rectangle w h) (* w h)]
    [(triangle a b c)
     (let ([s (/ (+ a b c) 2)])
       (sqrt (* s (- s a) (- s b) (- s c))))]))

(define (describe-shape shape)
  (match shape
    [(circle r) (format "Circle with radius ~a" r)]
    [(rectangle w h) (format "Rectangle ~ax~a" w h)]
    [(triangle a b c) (format "Triangle with sides ~a, ~a, ~a" a b c)]))

;; ====================
;; 4. Custom Data Types
;; ====================

(struct num (value) #:transparent)
(struct add (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct neg (expr) #:transparent)

(define (eval-expr expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval-expr l) (eval-expr r))]
    [(mul l r) (* (eval-expr l) (eval-expr r))]
    [(neg e) (- (eval-expr e))]))

;; ====================
;; 5. Guards (#:when)
;; ====================

(define (classify n)
  (match n
    [(? (λ (x) (< x 0))) "negative"]
    [0 "zero"]
    [(? (λ (x) (< x 10))) "small positive"]
    [(? (λ (x) (< x 100))) "medium positive"]
    [_ "large positive"]))

;; Alternative with #:when
(define (classify-when n)
  (match n
    [x #:when (< x 0) "negative"]
    [0 "zero"]
    [x #:when (< x 10) "small positive"]
    [x #:when (< x 100) "medium positive"]
    [_ "large positive"]))

;; ====================
;; 6. Or Patterns
;; ====================

(define (is-weekend day)
  (match (string-downcase day)
    [(or "saturday" "sunday") #t]
    [_ #f]))

;; ====================
;; 7. Range-like Patterns
;; ====================

(define (grade-letter score)
  (match score
    [s #:when (and (>= s 90) (<= s 100)) 'A]
    [s #:when (>= s 80) 'B]
    [s #:when (>= s 70) 'C]
    [s #:when (>= s 60) 'D]
    [_ 'F]))

;; ====================
;; 8. Option-like Patterns
;; ====================

(define (describe-maybe opt)
  (match opt
    ['nothing "nothing"]
    [`(just ,x) (format "just ~a" x)]))

(define (describe-result res)
  (match res
    [`(ok ,val) (format "success: ~a" val)]
    [`(err ,e) (format "error: ~a" e)]))

;; ====================
;; 9. Nested Patterns
;; ====================

(struct user (name age active) #:transparent)

(define (describe-user u)
  (match u
    [(user name age #t) #:when (>= age 18)
     (format "Active adult: ~a" name)]
    [(user name _ #t)
     (format "Active minor: ~a" name)]
    [(user name _ #f)
     (format "Inactive: ~a" name)]))

;; ====================
;; 10. Hash/Dictionary Patterns
;; ====================

(define (process-command cmd)
  (match cmd
    [(hash-table ['action "quit"])
     "Quitting..."]
    [(hash-table ['action "move"] ['direction dir])
     (format "Moving ~a" dir)]
    [(hash-table ['action "attack"] ['target target] ['damage dmg])
     (format "Attacking ~a for ~a damage" target dmg)]
    [_ "Invalid command"]))

;; ====================
;; 11. Quasiquote Patterns
;; ====================

(define (analyze-nested data)
  (match data
    [`(user (,name ,age) active)
     (format "Active user: ~a (~a)" name age)]
    [`(user (,name) inactive)
     (format "Inactive user: ~a" name)]
    [`(error ,msg)
     (format "Error: ~a" msg)]
    [_ "Unknown data"]))

;; ====================
;; 12. Match-Lambda (Pattern-Based Functions)
;; ====================

(define describe-shape-lambda
  (match-lambda
    [(circle r) (format "Circle: r=~a" r)]
    [(rectangle w h) (format "Rectangle: ~ax~a" w h)]
    [(triangle a b c) (format "Triangle: ~a,~a,~a" a b c)]))

;; ====================
;; 13. Cons Patterns (Head/Tail)
;; ====================

(define (list-sum lst)
  (match lst
    ['() 0]
    [(cons head tail) (+ head (list-sum tail))]))

(define (list-length lst)
  (match lst
    ['() 0]
    [(cons _ tail) (+ 1 (list-length tail))]))

;; ====================
;; 14. Vector Patterns
;; ====================

(define (describe-vector vec)
  (match vec
    [(vector) "empty vector"]
    [(vector x) (format "single: ~a" x)]
    [(vector x y) (format "pair: (~a, ~a)" x y)]
    [(vector x y z) (format "triple: (~a, ~a, ~a)" x y z)]
    [_ (format "vector of length ~a" (vector-length vec))]))

;; ====================
;; 15. Multiple Values Pattern
;; ====================

(define (quotient-and-remainder a b)
  (values (quotient a b) (remainder a b)))

(define (describe-division a b)
  (define-values (q r) (quotient-and-remainder a b))
  (format "~a / ~a = ~a remainder ~a" a b q r))

;; ====================
;; 16. Regexp Patterns
;; ====================

(define (parse-email email)
  (match email
    [(regexp #rx"^([^@]+)@([^@]+)$" (list _ user domain))
     (format "User: ~a, Domain: ~a" user domain)]
    [_ "Invalid email"]))

;; ====================
;; 17. Predicate Patterns
;; ====================

(define (describe-value val)
  (match val
    [(? number? n) (format "number: ~a" n)]
    [(? string? s) (format "string: '~a'" s)]
    [(? list? l) (format "list with ~a items" (length l))]
    [(? hash? h) (format "hash with ~a keys" (hash-count h))]
    [_ "unknown type"]))

;; ====================
;; 18. Tree Patterns
;; ====================

(struct leaf (value) #:transparent)
(struct node (value left right) #:transparent)

(define (tree-height tree)
  (match tree
    [(leaf _) 1]
    [(node _ left right)
     (+ 1 (max (tree-height left) (tree-height right)))]))

(define (tree-contains? tree val)
  (match tree
    [(leaf v) (equal? v val)]
    [(node v left right)
     (or (equal? v val)
         (tree-contains? left val)
         (tree-contains? right val))]))

;; ====================
;; 19. State Machine
;; ====================

(define (traffic-light-next current action)
  (match (list current action)
    [(list _ "emergency") "red"]
    [(list "red" "timer") "green"]
    [(list "green" "timer") "yellow"]
    [(list "yellow" "timer") "red"]
    [(list state _) state]))  ; No change

;; ====================
;; 20. And Patterns
;; ====================

(define (describe-and-match val)
  (match val
    [(and (? number?) (? positive?) x)
     (format "positive number: ~a" x)]
    [(and (? string?) (? (λ (s) (> (string-length s) 5))) s)
     (format "long string: ~a" s)]
    [_ "other"]))

;; ====================
;; Main Demonstration
;; ====================

(define (main)
  (displayln "=== Pattern Matching in Racket ===\n")

  ;; 1. Basic matching
  (displayln "1. Basic Match:")
  (for ([n '(0 1 2 5)])
    (displayln (format "   ~a -> ~a" n (describe-number n))))

  ;; 2. Tuple patterns
  (displayln "\n2. Tuple/List Patterns:")
  (for ([p '((0 0) (0 5) (3 0) (2 3))])
    (displayln (format "   ~a -> ~a" p (describe-point p))))

  ;; 3. List patterns
  (displayln "\n3. List Patterns:")
  (for ([lst '(() (1) (1 2) (1 2 3 4))])
    (displayln (format "   ~a -> ~a" lst (describe-list lst))))

  ;; 4. Struct patterns
  (displayln "\n4. Struct Patterns:")
  (define shapes (list (circle 5) (rectangle 4 6) (triangle 3 4 5)))
  (for ([shape shapes])
    (displayln (format "   ~a" (describe-shape shape)))
    (displayln (format "   area = ~a" (~r (area shape) #:precision 2))))

  ;; 5. Guards
  (displayln "\n5. Guards (#:when):")
  (for ([n '(-5 0 3 50 500)])
    (displayln (format "   ~a -> ~a" n (classify-when n))))

  ;; 6. Or patterns
  (displayln "\n6. Or Patterns:")
  (for ([day '("Monday" "Saturday" "Sunday" "Wednesday")])
    (displayln (format "   ~a is weekend? ~a" day (is-weekend day))))

  ;; 7. Range patterns
  (displayln "\n7. Range-like Patterns:")
  (for ([score '(95 85 75 65 55)])
    (displayln (format "   ~a -> ~a" score (grade-letter score))))

  ;; 8. Option patterns
  (displayln "\n8. Option-like Patterns:")
  (displayln (format "   ~a" (describe-maybe '(just 42))))
  (displayln (format "   ~a" (describe-maybe 'nothing)))
  (displayln (format "   ~a" (describe-result '(ok 100))))
  (displayln (format "   ~a" (describe-result '(err "failed"))))

  ;; 9. Expression evaluator
  (displayln "\n9. Expression Evaluator:")
  (define expr (mul (add (num 2) (num 3)) (num 4)))
  (displayln (format "   (2 + 3) * 4 = ~a" (eval-expr expr)))
  (define expr2 (neg (add (num 5) (num 3))))
  (displayln (format "   -(5 + 3) = ~a" (eval-expr expr2)))

  ;; 10. User patterns
  (displayln "\n10. User Patterns:")
  (define users (list (user "Alice" 25 #t)
                      (user "Bob" 16 #t)
                      (user "Charlie" 30 #f)))
  (for ([u users])
    (displayln (format "   ~a" (describe-user u))))

  ;; 11. Nested patterns
  (displayln "\n11. Nested Patterns:")
  (for ([data '((user ("Alice" 30) active)
                (user ("Bob") inactive)
                (error "Not found"))])
    (displayln (format "   ~a" (analyze-nested data))))

  ;; 12. Match-lambda
  (displayln "\n12. Match-Lambda:")
  (for ([shape shapes])
    (displayln (format "   ~a" (describe-shape-lambda shape))))

  ;; 13. Cons patterns
  (displayln "\n13. Cons Patterns (Recursion):")
  (define numbers '(1 2 3 4 5))
  (displayln (format "   sum of ~a = ~a" numbers (list-sum numbers)))
  (displayln (format "   length of ~a = ~a" numbers (list-length numbers)))

  ;; 14. Vector patterns
  (displayln "\n14. Vector Patterns:")
  (for ([vec (list (vector) (vector 1) (vector 1 2) (vector 1 2 3) (vector 1 2 3 4 5))])
    (displayln (format "   ~a -> ~a" vec (describe-vector vec))))

  ;; 15. Predicate patterns
  (displayln "\n15. Predicate Patterns:")
  (for ([val (list 42 "hello" '(1 2 3) (make-hash '((a . 1))))])
    (displayln (format "   ~a" (describe-value val))))

  ;; 16. Email parsing
  (displayln "\n16. Regexp Patterns:")
  (displayln (format "   ~a" (parse-email "user@example.com")))
  (displayln (format "   ~a" (parse-email "invalid")))

  ;; 17. Tree operations
  (displayln "\n17. Tree Patterns:")
  (define tree (node 5 (node 3 (leaf 1) (leaf 4)) (node 7 (leaf 6) (leaf 9))))
  (displayln (format "   Tree height: ~a" (tree-height tree)))
  (displayln (format "   Contains 7? ~a" (tree-contains? tree 7)))
  (displayln (format "   Contains 8? ~a" (tree-contains? tree 8)))

  ;; 18. State machine
  (displayln "\n18. State Machine (Traffic Light):")
  (define state
    (for/fold ([s "red"])
              ([action '("timer" "timer" "timer" "emergency")])
      (define next (traffic-light-next s action))
      (displayln (format "   After '~a': ~a" action next))
      next))

  ;; 19. And patterns
  (displayln "\n19. And Patterns:")
  (displayln (format "   ~a" (describe-and-match 42)))
  (displayln (format "   ~a" (describe-and-match "hello world")))
  (displayln (format "   ~a" (describe-and-match "hi")))

  (displayln "\n=== Racket Pattern Matching Notes ===")
  (displayln "- match expression is very powerful")
  (displayln "- Destructuring for lists, structs, vectors, hashes")
  (displayln "- Guards with #:when or predicates (?)")
  (displayln "- Or patterns, and patterns, not patterns")
  (displayln "- match-lambda for pattern-based functions")
  (displayln "- Quasiquote patterns with `")
  (displayln "- Regexp patterns for string matching"))

(main)
