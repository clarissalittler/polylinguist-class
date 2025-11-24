#lang racket
;; Lesson 6: Recursion in Racket
;;
;; Racket is a Lisp dialect where recursion is the primary iteration mechanism.
;; Racket has full tail call optimization, making recursion very efficient.

(provide (all-defined-out))

;; ====================
;; 1. Simple Recursion
;; ====================

(define (factorial n)
  ;; Calculate n! recursively
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-tail n)
  ;; Tail-recursive factorial (optimized by Racket)
  (define (go n acc)
    (if (<= n 1)
        acc
        (go (- n 1) (* n acc))))
  (go n 1))

;; ====================
;; 2. Fibonacci
;; ====================

(define (fibonacci n)
  ;; Fibonacci (inefficient - exponential)
  (cond
    [(<= n 1) n]
    [else (+ (fibonacci (- n 1))
             (fibonacci (- n 2)))]))

(define (fibonacci-memo n [memo (make-hash)])
  ;; Fibonacci with memoization
  (cond
    [(hash-has-key? memo n) (hash-ref memo n)]
    [(<= n 1) n]
    [else
     (define result (+ (fibonacci-memo (- n 1) memo)
                       (fibonacci-memo (- n 2) memo)))
     (hash-set! memo n result)
     result]))

;; ====================
;; 3. List Recursion
;; ====================

(define (sum-list lst)
  ;; Sum all elements
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(define (list-length lst)
  ;; Calculate length
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))

(define (reverse-list lst)
  ;; Reverse list
  (if (null? lst)
      '()
      (append (reverse-list (cdr lst)) (list (car lst)))))

(define (reverse-list-tail lst)
  ;; Tail-recursive reverse
  (define (go lst acc)
    (if (null? lst)
        acc
        (go (cdr lst) (cons (car lst) acc))))
  (go lst '()))

(define (max-element lst)
  ;; Find maximum element
  (cond
    [(null? lst) (error "Empty list")]
    [(null? (cdr lst)) (car lst)]
    [else (max (car lst) (max-element (cdr lst)))]))

;; ====================
;; 4. String Recursion
;; ====================

(define (reverse-string s)
  ;; Reverse string recursively
  (list->string (reverse (string->list s))))

(define (is-palindrome s)
  ;; Check if palindrome
  (define clean (list->string
                 (filter (lambda (c) (not (char-whitespace? c)))
                        (string->list (string-downcase s)))))
  (define (check s)
    (cond
      [(<= (string-length s) 1) #t]
      [(not (char=? (string-ref s 0)
                    (string-ref s (- (string-length s) 1)))) #f]
      [else (check (substring s 1 (- (string-length s) 1)))]))
  (check clean))

;; ====================
;; 5. Binary Search
;; ====================

(define (binary-search vec target [low 0] [high (- (vector-length vec) 1)])
  ;; Binary search on sorted vector
  (if (> low high)
      -1  ; Not found
      (let ([mid (quotient (+ low high) 2)])
        (cond
          [(= (vector-ref vec mid) target) mid]
          [(> (vector-ref vec mid) target)
           (binary-search vec target low (- mid 1))]
          [else (binary-search vec target (+ mid 1) high)]))))

;; ====================
;; 6. Quicksort
;; ====================

(define (quicksort lst)
  ;; Sort list using quicksort
  (if (null? lst)
      '()
      (let ([pivot (car lst)]
            [rest (cdr lst)])
        (append
         (quicksort (filter (lambda (x) (< x pivot)) rest))
         (list pivot)
         (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

;; ====================
;; 7. Tower of Hanoi
;; ====================

(define (hanoi n source target auxiliary)
  ;; Solve Tower of Hanoi
  (if (= n 1)
      (list (format "Move disk 1 from ~a to ~a" source target))
      (append
       (hanoi (- n 1) source auxiliary target)
       (list (format "Move disk ~a from ~a to ~a" n source target))
       (hanoi (- n 1) auxiliary target source))))

;; ====================
;; 8. Tree Traversal
;; ====================

(struct tree-node (value left right) #:transparent)

(define (tree-height node)
  ;; Calculate height
  (if (null? node)
      0
      (+ 1 (max (tree-height (tree-node-left node))
                (tree-height (tree-node-right node))))))

(define (tree-sum node)
  ;; Sum all values
  (if (null? node)
      0
      (+ (tree-node-value node)
         (tree-sum (tree-node-left node))
         (tree-sum (tree-node-right node)))))

(define (inorder-traversal node)
  ;; Left, root, right
  (if (null? node)
      '()
      (append (inorder-traversal (tree-node-left node))
              (list (tree-node-value node))
              (inorder-traversal (tree-node-right node)))))

(define (preorder-traversal node)
  ;; Root, left, right
  (if (null? node)
      '()
      (cons (tree-node-value node)
            (append (preorder-traversal (tree-node-left node))
                    (preorder-traversal (tree-node-right node))))))

(define (postorder-traversal node)
  ;; Left, right, root
  (if (null? node)
      '()
      (append (postorder-traversal (tree-node-left node))
              (postorder-traversal (tree-node-right node))
              (list (tree-node-value node)))))

;; ====================
;; 9. Mutual Recursion
;; ====================

(define (is-even n)
  ;; Check if even using mutual recursion
  (if (= n 0)
      #t
      (is-odd (- n 1))))

(define (is-odd n)
  ;; Check if odd
  (if (= n 0)
      #f
      (is-even (- n 1))))

;; ====================
;; 10. Greatest Common Divisor
;; ====================

(define (gcd-rec a b)
  ;; Euclid's algorithm
  (if (= b 0)
      a
      (gcd-rec b (remainder a b))))

;; ====================
;; Higher-Order Recursive Functions
;; ====================

(define (map-recursive f lst)
  ;; Map using recursion
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map-recursive f (cdr lst)))))

(define (filter-recursive pred lst)
  ;; Filter using recursion
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter-recursive pred (cdr lst)))]
    [else (filter-recursive pred (cdr lst))]))

(define (fold-recursive f acc lst)
  ;; Fold (reduce) using recursion
  (if (null? lst)
      acc
      (fold-recursive f (f acc (car lst)) (cdr lst))))

;; ====================
;; Tests and Examples
;; ====================

(define (main)
  (displayln "=== Recursion Examples in Racket ===\n")

  ;; Factorial
  (displayln "1. Factorial:")
  (displayln (format "   factorial(5) = ~a" (factorial 5)))
  (displayln (format "   factorial-tail(5) = ~a" (factorial-tail 5)))

  ;; Fibonacci
  (displayln "\n2. Fibonacci:")
  (displayln (format "   fibonacci(10) = ~a" (fibonacci 10)))
  (displayln (format "   fibonacci-memo(30) = ~a" (fibonacci-memo 30)))

  ;; List operations
  (displayln "\n3. List Operations:")
  (define numbers '(1 2 3 4 5))
  (displayln (format "   sum-list ~a = ~a" numbers (sum-list numbers)))
  (displayln (format "   list-length ~a = ~a" numbers (list-length numbers)))
  (displayln (format "   reverse-list ~a = ~a" numbers (reverse-list numbers)))
  (displayln (format "   max-element ~a = ~a" numbers (max-element numbers)))

  ;; String operations
  (displayln "\n4. String Operations:")
  (displayln (format "   reverse-string 'hello' = ~a" (reverse-string "hello")))
  (displayln (format "   is-palindrome 'racecar' = ~a" (is-palindrome "racecar")))
  (displayln (format "   is-palindrome 'A man a plan a canal Panama' = ~a"
                     (is-palindrome "A man a plan a canal Panama")))

  ;; Binary search
  (displayln "\n5. Binary Search:")
  (define sorted-vec #(1 3 5 7 9 11 13 15))
  (displayln (format "   binary-search ~a 7 = ~a"
                     sorted-vec (binary-search sorted-vec 7)))
  (displayln (format "   binary-search ~a 4 = ~a"
                     sorted-vec (binary-search sorted-vec 4)))

  ;; Quicksort
  (displayln "\n6. Quicksort:")
  (define unsorted '(3 6 8 10 1 2 1))
  (displayln (format "   quicksort ~a = ~a" unsorted (quicksort unsorted)))

  ;; Tower of Hanoi
  (displayln "\n7. Tower of Hanoi (3 disks):")
  (define moves (hanoi 3 "A" "C" "B"))
  (for ([move moves])
    (displayln (format "   ~a" move)))

  ;; Tree operations
  (displayln "\n8. Binary Tree:")
  ;;       5
  ;;      / \
  ;;     3   8
  ;;    / \   \
  ;;   1   4   9
  (define example-tree
    (tree-node 5
               (tree-node 3
                          (tree-node 1 '() '())
                          (tree-node 4 '() '()))
               (tree-node 8
                          '()
                          (tree-node 9 '() '()))))

  (displayln (format "   tree-height = ~a" (tree-height example-tree)))
  (displayln (format "   tree-sum = ~a" (tree-sum example-tree)))
  (displayln (format "   inorder-traversal = ~a" (inorder-traversal example-tree)))
  (displayln (format "   preorder-traversal = ~a" (preorder-traversal example-tree)))
  (displayln (format "   postorder-traversal = ~a" (postorder-traversal example-tree)))

  ;; Mutual recursion
  (displayln "\n9. Mutual Recursion:")
  (displayln (format "   is-even(10) = ~a" (is-even 10)))
  (displayln (format "   is-odd(10) = ~a" (is-odd 10)))
  (displayln (format "   is-even(7) = ~a" (is-even 7)))
  (displayln (format "   is-odd(7) = ~a" (is-odd 7)))

  ;; GCD
  (displayln "\n10. Greatest Common Divisor:")
  (displayln (format "   gcd-rec(48, 18) = ~a" (gcd-rec 48 18)))
  (displayln (format "   gcd-rec(100, 35) = ~a" (gcd-rec 100 35)))

  ;; Higher-order recursion
  (displayln "\n11. Higher-Order Recursive Functions:")
  (displayln (format "   map-recursive (* 2) '(1 2 3 4 5) = ~a"
                     (map-recursive (lambda (x) (* x 2)) '(1 2 3 4 5))))
  (displayln (format "   filter-recursive even? '(1 2 3 4 5 6) = ~a"
                     (filter-recursive even? '(1 2 3 4 5 6))))
  (displayln (format "   fold-recursive + 0 '(1 2 3 4 5) = ~a"
                     (fold-recursive + 0 '(1 2 3 4 5)))))

;; Run main if this is the main module
(module+ main
  (main))
