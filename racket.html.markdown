TODO: sequences, control flow, macros, objects, modules

---
language: racket
author: Th3rac25
---

Racket is a general purpose, multi-paradigm programming language in the Lisp/Scheme family. 

Feedback would be highly appreciated! You can reach me at [@th3rac25](http://twitter.com/th3rac25) or th3rac25 [at] [google's email service]


```racket
; Single line comments start with a semicolon
#| Multiline strings can be written
    using three "'s, and are often used
    as comments
|#


#lang racket ; defines the language we are using

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Primitive Datatypes and Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have numbers
1       
9999999999999999999999 ; big integers
3.14
6.02e+23
1/2  ; rationals   
1+2i ; complex numbers   

; Math is what you would expect
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
; In conditionals, all non-#f values are treated as true

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
;; 2. Variables and Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You need to declare variables before assigning to them.
; a variable name can use any characters except: () [] {} " , ' ` ; # | \
(define some-var 5)
some-var ; => 5

; use set! to reassign
(set! some-var 6) 
some-var ; => 6

; Accessing a previously unassigned variable is an exception
x ; => x: undefined ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Collections and Sequences
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

; a sequence is an ordered collection of value
(sequence? '(1 2 3)) ; => #t
(sequence? #(1 2 3)) ; => #t

;; more sequence stuff here !


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Use fn to create new functions. A function always returns
; its last statement.
(lambda () "Hello World") ; => #<procedure>

; (You need extra parens to call it)
((lambda () "Hello World")) ; => "Hello World"

; You can create a variable using define
(define x 1)
x ; => 1

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


```

;; Further Reading

Still up for more? Try [Quick: An Introduction to Racket with Pictures](http://docs.racket-lang.org/quick/)

