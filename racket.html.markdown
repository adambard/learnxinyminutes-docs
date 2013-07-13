---
language: racket
author: th3rac25
---

Racket is a general purpose, multi-paradigm programming language in the Lisp/Scheme family. 

Feedback is appreciated! You can reach me at [@th3rac25](http://twitter.com/th3rac25) or th3rac25 [at] [google's email service]


```racket
#lang racket ; defines the language we are using

; TODO: 
; quote
; structs
; control flow (pattern-matching, loops, sequences)


; Single line comments start with a semicolon
#| Multiline strings can be written
    using three "'s, and are often used
    as comments
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Primitive Datatypes and Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Numbers
9999999999999999999999 ; integers
3.14                   ; reals
6.02e+23
1/2                    ; rationals   
1+2i                   ; complex numbers   

; Function application is written (f x y z ...) 
; where f is a function and x, y, z, ... are operands

; Here are a few arithmetic operators
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(quotient 5 2) ; => 2
(remainder 5 2) ; => 1
(/ 35 5) ; => 7
(/ 1 3) ; => 1/3
(exact->inexact 1/3) ; => 0.3333333333333333
(+ 1+2i  2-3i) ; => 3-1i

; Booleans 
#t ; for true  
#f ; for false
(not #t) ; => #f

; Equality for numbers is =
(= 1 1.0) ; => #t
(= 2 1) ; => #f

; Characters 
#\A ; => #\A
#\λ ; => #\λ 
#\u03BB ; => #\λ

; Strings are fixed-length array of characters.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel" ; backslash is an escaping character
"λx:(μα.α→α).xx" ; any Unicode character can appear in a string constant

; Strings can be added too!
(string-append "Hello " "world!") ; => "Hello world!"

; A string can be treated like a list of characters
(string-ref "Apple" 0) ; => #\A

; format can be used to format strings:
(format "~a can be ~a" "strings" "formatted")

; Printing is pretty easy
(printf "I'm Racket. Nice to meet you!\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; You can create a variable using define
; a variable name can use any characters except: () [] {} " , ' ` ; # | \
(define some-var 5)
some-var ; => 5

; Use set! to assign a new value to an existing variable
(set! some-var 6) 
some-var ; => 6

; Accessing a previously unassigned variable is an exception
;x ; => x: undefined ...

; Local binding: me is bound to "Bob" only within (let ...)
(let ([me "Bob"])
    "Alice"
    me) ; => "Bob"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Lists are linked-list data structures
'(1 2 3) 

; Vectors are fixed-length arrays
#(1 2 3) 

; Use "cons" to add an item to the beginning of a list
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Use "append" to add lists together
(append '(1 2) '(3 4)) ; => (1 2 3 4)

; Use "filter", "map" to interact with collections
(map add1 '(1 2 3)) ; => (2 3 4)
(filter even?  '(1 2 3)) ; => (2)

; Use "fold" to reduce them
(foldl + 0 '(1 2 3 4))
; = (+ 1 (+ 2 (+ 3 (+ 4 0)))
; => 10

;;; Sets

; create a set from a list
(list->set '(1 2 3 1 2 3 3 2 1 3 2 1)) ; => (set 1 2 3)

; Add a member with "set-add"
(set-add (set 1 2 3) 4); => (set 1 2 3 4)

; Remove one with "set-remove"
(set-remove (set 1 2 3) 1) ; => (set 2 3)

; Test for existence with "set-member?"
(set-member? (set 1 2 3) 1) ; => #t
(set-member? (set 1 2 3) 4) ; => #f

;;; Hashes

; Create an immutable hash table (There are also mutables ones)
(define m (hash 'a 1 'b 2 'c 3))

; Retrieve a value
(hash-ref m 'a) ; => 1

; Retrieving a non-present value is an exception
; (hash-ref m 'd) => no value found

; You can provide a default value for missing keys
(hash-ref m 'd 0) ; => 0

; Use "hash-set" to extend a hash table
(define m2 (hash-set m 'd 4)) 
m2 ; => '#hash((b . 2) (a . 1) (d . 4) (c . 3))

; Remember, these hashes are immutable!
m ; => '#hash((b . 2) (a . 1) (c . 3))

; Use "hash-remove" to remove keys
(hash-remove m 'a) ; => '#hash((b . 2) (c . 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Use lambda to create new functions. A function always returns
; its last statement.
(lambda () "Hello World") ; => #<procedure>

; (You need extra parens to call it)
((lambda () "Hello World")) ; => "Hello World"

; Assign a function to a var
(define hello-world (lambda () "Hello World"))
(hello-world) ; => "Hello World"

; You can shorten this to:
(define (hello-world2) "Hello World")

; The () is the list of arguments for the function.
(define hello 
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve") ; => "Hello Steve"

; You can have multi-variadic functions, too
(define hello2
  (case-lambda 
    [() "Hello World"]
    [(name) (string-append "Hello " name)]))
(hello2 "Jake") ; => "Hello Jake"
(hello2) ; => "Hello World"

; Functions can pack extra arguments up in a list
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; You can mix regular and packed arguments
(define (hello-count name . args)
  (format "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Control Flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Conditionals
(if #t               ; test expression
    "this is true"   ; then expression
    "this is false"  ; else expression
    ) ; =>  "this is true"

; In conditionals, all non-#f values are treated as true
(member "Groucho" '("Harpo" "Groucho" "Zeppo")) ; => '("Groucho" "Zeppo")
(if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
      'yep
      'nope) ; => 'yep


; "cond" chains a series of tests to select a result
(cond
   [(> 2 2) (error "wrong!")]
   [(< 2 2) (error "wrong again!")]
   [else 'ok]) ; => 'ok

; Pattern matching

; Loops

; Sequences

; Exceptions
; To catch an exception, use the "with-handlers" form
; To throw an exception use "raise"
(with-handlers 
    ([(lambda (v) (equal? v "infinity"))   
      (lambda (exn) +inf.0)])
  (raise "infinity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modules let you organize code into multiple files and reusable libraries.

(module cake racket/base  ;; define a new module 'cake' based on racket/base
  
  (provide print-cake) ;; function exported by the module
  
  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
  
  (define (show fmt n ch) ;; internal function       
    (printf fmt (make-string n ch))
    (newline)))

;; Use "require" to import all functions from the module
(require 'cake) 
(print-cake 3)  
;(show "~a" 1 #\A) ; => error, "show" was not exported

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Classes and Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Create a class fish%
(define fish%
  (class object% 
    (init size) ; initialization argument
    (super-new) ; superclass initialization 
    
    (define current-size size) ; field
  
    ; Public methods
    (define/public (get-size)  
      current-size)
    
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))


; Create an instance of fish%
(define charlie 
  (new fish% [size 10]))

; Use "send" to call an object's methods
(send charlie grow 6)
(send charlie get-size) ; => 16
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros let you extend the syntax of the language
(define-syntax-rule (unless test then else) 
  (if test else then))

(unless (even? 10) "odd" "even") ; => "even"

; Macros are hygienic, there is no risk to clobber existing variables!   
(define-syntax-rule (swap x y)
  (begin
    (define tmp x) 
    (set! x y)
    (set! y tmp)))

(define tmp 1) 
(define a 2)
(define b 3)
(swap a b)
(printf "tmp = ~a; a = ~a; b = ~a" tmp a b) ; tmp is unaffected by swap
```

## Further Reading

Still up for more? Try [Quick: An Introduction to Racket with Pictures](http://docs.racket-lang.org/quick/)

