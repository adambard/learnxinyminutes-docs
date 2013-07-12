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
; collections (set, hash)
; structs
; control flow (pattern-matching, loops, sequences)
; objects


; Single line comments start with a semicolon
#| Multiline strings can be written
    using three "'s, and are often used
    as comments
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Primitive Datatypes and Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have numbers
9999999999999999999999 ; big integers
3.14                   ; reals
6.02e+23
1/2                    ; rationals   
1+2i                   ; complex numbers   

; Operators are functions, functions applications are written
; (f x y z ...) where f is a function and x, y, z, ... are operands
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

; Lists are linked-list data structures, vectors are fixed-length arrays.
'(1 2 3) ; a list
#(1 2 3) ; a vector

; Use cons to add an item to the beginning of a list
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Use append to add lists together
(append '(1 2) '(3 4)) ; => (1 2 3 4)

; Use filter, map to interact with collections
(map add1 '(1 2 3)) ; => (2 3 4)
(filter even?  '(1 2 3)) ; => (2)

; Use fold to reduce them
(foldl + 0 '(1 2 3 4))
; = (+ 1 (+ 2 (+ 3 (+ 4 0)))
; => 10

; Set


; Hash


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


; Cond chains a series of test to select a result
(cond
   [(> 2 2) (error "wrong!")]
   [(< 2 2) (error "wrong again!")]
   [else 'ok]) ; => 'ok

; Pattern matching

; Loops

; Sequences

; Exceptions
; To catch an exception, use the with-handlers form
; To throw an exception use raise
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

(require 'cake) ;; import all 'cake' functions
(print-cake 3)  
;(show "~a" 1 #\A) ; => this is an error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;; Further Reading

Still up for more? Try [Quick: An Introduction to Racket with Pictures](http://docs.racket-lang.org/quick/)

