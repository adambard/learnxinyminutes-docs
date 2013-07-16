---

language: racket
filename: learnracket.rkt
contributors:
  - ["th3rac25", "https://github.com/voila"]

---

Racket is a general purpose, multi-paradigm programming language in the Lisp/Scheme family.

Feedback is appreciated! You can reach me at [@th3rac25](http://twitter.com/th3rac25) or th3rac25 [at] [google's email service]


```racket
#lang racket ; defines the language we are using

;;; Comments

;; Single line comments start with a semicolon

#| Block comments
   can span multiple lines and...
    #|
       they can be nested!
    |#
|#

;; S-expression comments discard the following expression
#; "this expression will be discarded" "2nd expression" ; => "2nd expression"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Primitive Datatypes and Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numbers
9999999999999999999999 ; integers
3.14                   ; reals
6.02e+23
1/2                    ; rationals
1+2i                   ; complex numbers

;; Function application is written (f x y z ...)
;; where f is a function and x, y, z, ... are operands
;; If you want to create a literal list of data, use ' to stop it from
;; being evaluated
'(+ 1 2) ; => (+ 1 2)
;; Now, some arithmetic operations
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(quotient 5 2) ; => 2
(remainder 5 2) ; => 1
(/ 35 5) ; => 7
(/ 1 3) ; => 1/3
(exact->inexact 1/3) ; => 0.3333333333333333
(+ 1+2i  2-3i) ; => 3-1i

;;; Booleans
#t ; for true
#f ; for false -- any value other than #f is true
(not #t) ; => #f
(and 0 #f (error "doesn't get here")) ; => #f
(or #f 0 (error "doesn't get here"))  ; => 0

;;; Characters
#\A ; => #\A
#\λ ; => #\λ
#\u03BB ; => #\λ

;;; Strings are fixed-length array of characters.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel" ; backslash is an escaping character
"λx:(μα.α→α).xx" ; any Unicode character can appear in a string constant

;; Strings can be added too!
(string-append "Hello " "world!") ; => "Hello world!"

;; A string can be treated like a list of characters
(string-ref "Apple" 0) ; => #\A

;; format can be used to format strings:
(format "~a can be ~a" "strings" "formatted")

;; Printing is pretty easy
(printf "I'm Racket. Nice to meet you!\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can create a variable using define
;; a variable name can use any character except: ()[]{}",'`;#|\
(define some-var 5)
some-var ; => 5

;; You can also use unicode characters
(define ⊆ subset?)
(⊆ (set 3 2) (set 1 2 3)); => #t

;; Accessing a previously unassigned variable is an exception
; x ; => x: undefined ...

;; Local binding: me is bound to "Bob" only within (let ...)
(let ([me "Bob"])
  "Alice"
  me) ; => "Bob"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Structs and Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Structs
(struct dog (name breed age))
(define my-pet
  (dog "lassie" "collie" 5))
my-pet ; => #<dog>
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"

;;; Pairs (immutable)
;; "cons" constructs pairs, "car" and "cdr" extract the first
;; and second elements
(cons 1 2) ; => '(1 . 2)
(car (cons 1 2)) ; => 1
(cdr (cons 1 2)) ; => 2

;;; Lists

;; Lists are linked-list data structures
(list 1 2 3) ; => '(1 2 3)

;; Use "cons" to add an item to the beginning of a list
(cons 4 '(1 2 3)) ; => (4 1 2 3)

;; Use "append" to add lists together
(append '(1 2) '(3 4)) ; => (1 2 3 4)

;;; Vectors

;; Vectors are fixed-length arrays
#(1 2 3) ; => '#(1 2 3)

;; Use "vector-append" to add vectors together
(vector-append #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; Sets

;; create a set from a list
(list->set '(1 2 3 1 2 3 3 2 1 3 2 1)) ; => (set 1 2 3)

;; Add a member with "set-add"
(set-add (set 1 2 3) 4); => (set 1 2 3 4)

;; Remove one with "set-remove"
(set-remove (set 1 2 3) 1) ; => (set 2 3)

;; Test for existence with "set-member?"
(set-member? (set 1 2 3) 1) ; => #t
(set-member? (set 1 2 3) 4) ; => #f

;;; Hashes

;; Create an immutable hash table (There are also mutables ones)
(define m (hash 'a 1 'b 2 'c 3))

;; Retrieve a value
(hash-ref m 'a) ; => 1

;; Retrieving a non-present value is an exception
; (hash-ref m 'd) => no value found

;; You can provide a default value for missing keys
(hash-ref m 'd 0) ; => 0

;; Use "hash-set" to extend a hash table
(define m2 (hash-set m 'd 4))
m2 ; => '#hash((b . 2) (a . 1) (d . 4) (c . 3))

;; Remember, these hashes are immutable!
m ; => '#hash((b . 2) (a . 1) (c . 3))

;; Use "hash-remove" to remove keys
(hash-remove m 'a) ; => '#hash((b . 2) (c . 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use lambda to create new functions.
;; A function always returns its last statement.
(lambda () "Hello World") ; => #<procedure>

;; (You need extra parens to call it)
((lambda () "Hello World")) ; => "Hello World"

;; Assign a function to a var
(define hello-world (lambda () "Hello World"))
(hello-world) ; => "Hello World"

;; You can shorten this to:
(define (hello-world2) "Hello World")

;; The () is the list of arguments for the function.
(define hello
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve") ; => "Hello Steve"

;; You can have multi-variadic functions, too
(define hello2
  (case-lambda
    [() "Hello World"]
    [(name) (string-append "Hello " name)]))
(hello2 "Jake") ; => "Hello Jake"
(hello2) ; => "Hello World"

;; Functions can pack extra arguments up in a list
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

;; You can mix regular and packed arguments
(define (hello-count name . args)
  (format "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for numbers use "="
(= 3 3.0) ; => #t
(= 2 1) ; => #f

;; for object identity use "eq?"
(eq? 3 3) ; => #t
(eq? 3 3.0) ; => #f
(eq? (list 3) (list 3)) ; => #f

;; for collections use "equal?"
(equal? (list 'a 'b) (list 'a 'b)) ; => #t
(equal? (list 'a 'b) (list 'b 'a)) ; => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Control Flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conditionals

(if #t               ; test expression
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;; In conditionals, all non-#f values are treated as true
(member "Groucho" '("Harpo" "Groucho" "Zeppo")) ; => '("Groucho" "Zeppo")
(if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
    'yep
    'nope)
; => 'yep

;; "cond" chains a series of tests to select a result
(cond [(> 2 2) (error "wrong!")]
      [(< 2 2) (error "wrong again!")]
      [else 'ok]) ; => 'ok

;;; Pattern Matching

(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizzbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_          #f]))

(fizzbuzz? 15) ; => 'fizzbuzz
(fizzbuzz? 37) ; => #f

;;; Loops

;; looping can be done through recursion
(define (loop i)
  (when (< i 10)
    (printf "i:~a~n" i)
    (loop (add1 i))))

(loop 5) ; => i:5 i:6 ...

;; similarly, with a named let
(let loop ((i 0))
  (when (< i 10)
    (printf "i:~a~n" i)
    (loop (add1 i)))) ; => i:0 i:1 ...

;;; Comprehensions

(for/list ([i '(1 2 3)])
  (add1 i)) ; => '(2 3 4)

(for/list ([i '(1 2 3)] #:when (even? i))
  i) ; => '(2)

(for/hash ([i '(1 2 3)])
  (values i (number->string i))) ; => '#hash((1 . "1") (2 . "2") (3 . "3"))

;; To combine iteration results, use "for/fold"
(for/fold ([sum 0]) ([i '(1 2 3 4)])
  (+ sum i)) ; => 10

;;; Sequences

;; "for" allows iteration over sequences:
;; lists, vectors, strings, sets, hash tables, etc...
(for ([i (in-list '(l i s t))])
  (displayln i))

(for ([i (in-vector #(v e c t o r))])
  (displayln i))

(for ([i (in-string "string")])
  (displayln i))

(for ([i (in-set (set 'x 'y 'z))])
  (displayln i))

(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3 ))])
  (printf "key:~a value:~a ~n" k v))

;;; Exceptions

;; To catch an exception, use the "with-handlers" form
;; To throw an exception use "raise"
(with-handlers
    ([(lambda (v) (equal? v "infinity"))
      (lambda (exn) +inf.0)])
  (raise "infinity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use set! to assign a new value to an existing variable
(define n 5)
(set! n 6)
n ; => 6

;; Many Racket datatypes can be immutable or mutable
;; (Pairs, Lists, Strings, Vectors, Hash Tables, etc...)

;; Use "vector" to create a mutable vector
(define vec (vector 2 2 3 4))
;; Use vector-set! to update a slot
(vector-set! vec 0 1)
vec ; => #(1 2 3 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modules let you organize code into multiple files and reusable libraries

(module cake racket/base ; define a new module 'cake' based on racket/base

  (provide print-cake) ; function exported by the module

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch) ; internal function
    (printf fmt (make-string n ch))
    (newline)))

;; Use "require" to import all functions from the module
(require 'cake)
(print-cake 3)
; (show "~a" 1 #\A) ; => error, "show" was not exported

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Classes and Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a class fish%
(define fish%
  (class object%
    (init size) ; initialization argument
    (super-new) ; superclass initialization
    ;; Field
    (define current-size size)
    ;; Public methods
    (define/public (get-size)  current-size)
    (define/public (grow amt) (set! current-size (+ amt current-size)))
    (define/public (eat other-fish) (grow (send other-fish get-size)))))

;; Create an instance of fish%
(define charlie
  (new fish% [size 10]))

;; Use "send" to call an object's methods
(send charlie grow 6)
(send charlie get-size) ; => 16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macros let you extend the syntax of the language

;; Let's add a while loop
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(let ([i 0])
  (while (< i  10)
    (displayln i)
    (set! i (add1 i))))

;; Macros are hygienic, you cannot clobber existing variables!
(define-syntax-rule (swap x y)
  (begin
    (define tmp x)
    (set! x y)
    (set! y tmp)))

(define tmp 1)
(define a 2)
(define b 3)
(swap a b)
(printf "tmp = ~a; a = ~a; b = ~a~n" tmp a b) ; tmp is unaffected by swap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. Contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contracts impose constraints on values exported from modules

(module bank-account racket
  (provide (contract-out
            [deposit (-> positive? any)] ; amount will always be a positive number
            [balance (-> positive?)]))

  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))
  (define (balance) amount)
  )

(require 'bank-account)
(deposit 5)

(balance) ; => 5

;; Any client that attempt to deposit a non-positive amount, will be blamed
;; (deposit -5) ; => deposit: contract violation
;; expected: positive?
;; given: -5
;; more details....
```

## Further Reading

Still up for more? Try [Quick: An Introduction to Racket with Pictures](http://docs.racket-lang.org/quick/)
