#lang racket

;; Lesson 7: Object-Oriented Programming in Racket
;;
;; Racket is primarily functional but has excellent OOP support through:
;; - Structs (lightweight, functional)
;; - Classes (full OOP with inheritance, interfaces, mixins)
;; This shows both approaches!

(require racket/class)

;; ====================
;; 1. Structs (Functional approach)
;; ====================

(struct person (name age) #:mutable #:transparent)

;; "Constructor" is the struct itself
(define (make-person name age)
  (person name age))

;; "Methods" are functions
(define (introduce p)
  (format "Hi, I'm ~a, ~a years old" (person-name p) (person-age p)))

(define (have-birthday! p)
  (set-person-age! p (+ (person-age p) 1)))

;; ====================
;; 2. Classes (OOP approach)
;; ====================

(define person%
  (class object%
    (init-field name age)  ; Fields with constructor parameters

    (super-new)  ; Initialize parent class

    ;; Methods
    (define/public (introduce)
      (format "Hi, I'm ~a, ~a years old" name age))

    (define/public (have-birthday)
      (set! age (+ age 1)))

    (define/public (get-age)
      age)))

;; ====================
;; 3. Inheritance
;; ====================

(define animal%
  (class object%
    (init-field name species)

    (super-new)

    (define/public (speak)
      "Some generic animal sound")

    (define/public (sleep)
      (format "~a is sleeping... Zzz" name))))

;; Dog inherits from animal%
(define dog%
  (class animal%
    (init-field breed)
    (init name)  ; Required for parent

    (super-new [name name] [species "Canine"])

    ;; Override speak
    (define/override (speak)
      (format "~a says Woof!" name))

    ;; New method specific to dog
    (define/public (fetch)
      (format "~a is fetching the ball!" name))

    (define/public (get-breed)
      breed)))

;; Cat inherits from animal%
(define cat%
  (class animal%
    (init-field indoor)
    (init name)

    (super-new [name name] [species "Feline"])

    ;; Override speak
    (define/override (speak)
      (format "~a says Meow!" name))

    (define/public (scratch)
      (format "~a is scratching the furniture!" name))))

;; ====================
;; 4. Interfaces
;; ====================

(define shape-interface
  (interface ()
    area
    perimeter))

;; Abstract base class for shapes
(define shape%
  (class* object% (shape-interface)
    (super-new)

    ;; Abstract methods - must be overridden
    (define/public (area)
      (error "Must override area"))

    (define/public (perimeter)
      (error "Must override perimeter"))

    ;; Concrete method available to all shapes
    (define/public (describe)
      (format "~a: area=~a, perimeter=~a"
              (object-name this)
              (~r (area) #:precision 2)
              (~r (perimeter) #:precision 2)))))

(define circle%
  (class* shape% (shape-interface)
    (init-field radius)

    (super-new)

    (define/override (area)
      (* pi radius radius))

    (define/override (perimeter)
      (* 2 pi radius))))

(define rectangle%
  (class* shape% (shape-interface)
    (init-field width height)

    (super-new)

    (define/override (area)
      (* width height))

    (define/override (perimeter)
      (* 2 (+ width height)))))

(define triangle%
  (class* shape% (shape-interface)
    (init-field side-a side-b side-c)

    (super-new)

    (define/override (area)
      ;; Heron's formula
      (let ([s (/ (perimeter) 2)])
        (sqrt (* s (- s side-a) (- s side-b) (- s side-c)))))

    (define/override (perimeter)
      (+ side-a side-b side-c))))

;; ====================
;; 5. Encapsulation
;; ====================

(define bank-account%
  (class object%
    (init-field account-number initial-balance)

    ;; Private fields (not init-field, not public)
    (define balance initial-balance)
    (define transactions '())

    (super-new)

    ;; Public methods
    (define/public (deposit amount)
      (when (> amount 0)
        (set! balance (+ balance amount))
        (set! transactions (append transactions
                                   (list (format "Deposit: +$~a" amount))))
        #t))

    (define/public (withdraw amount)
      (cond
        [(and (> amount 0) (<= amount balance))
         (set! balance (- balance amount))
         (set! transactions (append transactions
                                    (list (format "Withdrawal: -$~a" amount))))
         #t]
        [else #f]))

    (define/public (get-balance)
      balance)

    (define/public (get-transactions)
      transactions)))

;; ====================
;; 6. Class Fields and Methods (like static)
;; ====================

(define temperature%
  (class object%
    (init-field celsius)

    ;; Class-level field (shared across all instances)
    (define conversion-count 0)

    (super-new)

    ;; Class method (doesn't use instance data)
    (define/public (is-freezing?)
      (<= celsius 0))

    (define/public (to-fahrenheit)
      (+ (* celsius 9/5) 32))

    (define/public (to-kelvin)
      (+ celsius 273.15))))

;; Factory methods as regular functions
(define (temperature-from-fahrenheit f)
  (new temperature% [celsius (* (- f 32) 5/9)]))

(define (temperature-from-kelvin k)
  (new temperature% [celsius (- k 273.15)]))

;; ====================
;; 7. Composition
;; ====================

(define engine%
  (class object%
    (init-field horsepower)
    (define running #f)

    (super-new)

    (define/public (start)
      (set! running #t)
      (format "Engine starting... ~ahp engine now running" horsepower))

    (define/public (stop)
      (set! running #f)
      "Engine stopped")))

(define car%
  (class object%
    (init-field brand model horsepower)

    ;; Composition - car HAS-AN engine
    (define engine (new engine% [horsepower horsepower]))

    (super-new)

    (define/public (start)
      (format "~a ~a: ~a" brand model (send engine start)))

    (define/public (stop)
      (format "~a ~a: ~a" brand model (send engine stop)))))

;; ====================
;; 8. Mixins (Multiple Inheritance Alternative)
;; ====================

;; Mixin adds flying capability
(define (flyable-mixin class%)
  (class class%
    (super-new)

    (define/public (fly)
      (format "~a is flying through the air" (get-field name this)))))

;; Mixin adds swimming capability
(define (swimmable-mixin class%)
  (class class%
    (super-new)

    (define/public (swim)
      (format "~a is swimming in water" (get-field name this)))))

;; Duck with mixins
(define duck%
  (flyable-mixin
   (swimmable-mixin
    (class animal%
      (init name)

      (super-new [name name] [species "Waterfowl"])

      (define/override (speak)
        (format "~a says Quack!" name))

      (define/public (quack)
        "Quack!")))))

;; ====================
;; 9. Design Pattern: Singleton
;; ====================

(define singleton%
  (class object%
    (super-new)
    (define data '())

    (define/public (add-data item)
      (set! data (append data (list item))))

    (define/public (get-data)
      data)))

;; Create single instance
(define singleton-instance (new singleton%))

(define (get-singleton)
  singleton-instance)

;; ====================
;; 10. Design Pattern: Factory
;; ====================

(define (create-animal type name)
  (cond
    [(equal? type "dog") (new dog% [name name] [breed "Mixed"])]
    [(equal? type "cat") (new cat% [name name] [indoor #t])]
    [else (new animal% [name name] [species "Unknown"])]))

;; ====================
;; 11. Struct Inheritance (Alternative to Classes)
;; ====================

(struct shape-struct () #:transparent)
(struct circle-struct shape-struct (radius) #:transparent)
(struct rect-struct shape-struct (width height) #:transparent)

;; Functions for struct-based polymorphism
(define (shape-area s)
  (cond
    [(circle-struct? s)
     (* pi (circle-struct-radius s) (circle-struct-radius s))]
    [(rect-struct? s)
     (* (rect-struct-width s) (rect-struct-height s))]
    [else (error "Unknown shape")]))

;; ====================
;; Helper Functions
;; ====================

(define (object-name obj)
  (let ([class-name (object-name% obj)])
    (regexp-replace #rx"%$" (symbol->string class-name) "")))

(define (object-name% obj)
  (object-interface obj))

;; ====================
;; Main Function
;; ====================

(define (main)
  (displayln "=== Object-Oriented Programming in Racket ===\n")

  ;; 1. Structs (functional approach)
  (displayln "1. Structs (Functional approach):")
  (define alice-struct (make-person "Alice" 30))
  (displayln (string-append "   " (introduce alice-struct)))
  (have-birthday! alice-struct)
  (displayln (format "   After birthday: age = ~a" (person-age alice-struct)))

  ;; 2. Classes (OOP approach)
  (displayln "\n2. Classes (OOP approach):")
  (define alice-class (new person% [name "Alice"] [age 30]))
  (displayln (string-append "   " (send alice-class introduce)))
  (send alice-class have-birthday)
  (displayln (format "   After birthday: age = ~a" (send alice-class get-age)))

  ;; 3. Inheritance and Polymorphism
  (displayln "\n3. Inheritance and Polymorphism:")
  (define buddy (new dog% [name "Buddy"] [breed "Golden Retriever"]))
  (define whiskers (new cat% [name "Whiskers"] [indoor #t]))
  (define max-dog (new dog% [name "Max"] [breed "German Shepherd"]))

  (displayln (string-append "   " (send buddy speak)))
  (displayln (string-append "   " (send whiskers speak)))
  (displayln (string-append "   " (send max-dog speak)))

  (displayln (string-append "   " (send buddy fetch)))
  (displayln (string-append "   " (send whiskers scratch)))

  ;; 4. Abstract-like classes and shapes
  (displayln "\n4. Interfaces and Shapes:")
  (define circle (new circle% [radius 5]))
  (define rectangle (new rectangle% [width 4] [height 6]))
  (define triangle (new triangle% [side-a 3] [side-b 4] [side-c 5]))

  (displayln (string-append "   " (send circle describe)))
  (displayln (string-append "   " (send rectangle describe)))
  (displayln (string-append "   " (send triangle describe)))

  ;; 5. Encapsulation
  (displayln "\n5. Encapsulation (Bank Account):")
  (define account (new bank-account% [account-number "ACC001"] [initial-balance 1000]))
  (displayln (format "   Initial balance: $~a" (send account get-balance)))
  (send account deposit 500)
  (displayln (format "   After deposit: $~a" (send account get-balance)))
  (send account withdraw 200)
  (displayln (format "   After withdrawal: $~a" (send account get-balance)))
  (displayln (format "   Transactions: ~a" (send account get-transactions)))

  ;; 6. Factory methods
  (displayln "\n6. Temperature with Factory Methods:")
  (define temp1 (new temperature% [celsius 0]))
  (define temp2 (temperature-from-fahrenheit 32))
  (define temp3 (temperature-from-kelvin 273.15))

  (displayln (format "   0°C = ~a°F" (~r (send temp1 to-fahrenheit) #:precision 1)))
  (displayln (format "   32°F = ~a°C" (~r (get-field celsius temp2) #:precision 1)))
  (displayln (format "   273.15K = ~a°C" (~r (get-field celsius temp3) #:precision 1)))
  (displayln (format "   Is 0°C freezing? ~a" (send temp1 is-freezing?)))

  ;; 7. Composition
  (displayln "\n7. Composition:")
  (define car (new car% [brand "Toyota"] [model "Camry"] [horsepower 200]))
  (displayln (string-append "   " (send car start)))
  (displayln (string-append "   " (send car stop)))

  ;; 8. Mixins
  (displayln "\n8. Mixins (Multiple Inheritance Alternative):")
  (define duck (new duck% [name "Donald"]))
  (displayln (string-append "   " (send duck quack)))
  (displayln (string-append "   " (send duck fly)))
  (displayln (string-append "   " (send duck swim)))

  ;; 9. Singleton
  (displayln "\n9. Singleton Pattern:")
  (define s1 (get-singleton))
  (define s2 (get-singleton))
  (displayln (format "   s1 eq? s2? ~a" (eq? s1 s2)))
  (send s1 add-data "item1")
  (displayln (format "   s1.data: ~a" (send s1 get-data)))
  (displayln (format "   s2.data: ~a" (send s2 get-data)))

  ;; 10. Factory
  (displayln "\n10. Factory Pattern:")
  (define dog (create-animal "dog" "Rover"))
  (define cat (create-animal "cat" "Mittens"))
  (displayln (string-append "   " (send dog speak)))
  (displayln (string-append "   " (send cat speak)))

  ;; 11. Struct inheritance
  (displayln "\n11. Struct Inheritance:")
  (define c (circle-struct 5))
  (define r (rect-struct 4 6))
  (displayln (format "   Circle area: ~a" (~r (shape-area c) #:precision 2)))
  (displayln (format "   Rectangle area: ~a" (~r (shape-area r) #:precision 2))))

;; Run main
(main)
