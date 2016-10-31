---
language: "CHICKEN"
filename: CHICKEN.scm
contributors:
  - ["Diwakar Wagle", "https://github.com/deewakar"]
---


CHICKEN is an implementation of Scheme programming language that can
compile Scheme programs to C code as well as interpret them. CHICKEN
supports RSR5 and RSR7 (work in progress) standards and many extensions.


```scheme
;; #!/usr/bin/env csi -s

;; Run the CHICKEN REPL in the commandline as follows :
;; $ csi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 0. Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Single line comments start with a semicolon

#| Block comments
   can span multiple lines and...
   #| can be nested
   |#
|#

;; S-expression comments are used to comment out expressions
#; (display "nothing")    ; discard this expression 

;; CHICKEN has two fundamental pieces of syntax: Atoms and S-expressions
;; an atom is something that evaluates to itself
;; all builtin data types viz. numbers, chars, booleans, strings etc. are atoms
;; Furthermore an atom can be a symbol, an identifier, a keyword, a procedure
;; or the empty list (also called null)
'athing              ;; => athing 
'+                   ;; => + 
+                    ;; => <procedure C_plus>

;; S-expressions (short for symbolic expressions) consists of one or more atoms
(quote +)            ;; => + ; another way of writing '+
(+ 1 2 3)            ;; => 6 ; this S-expression evaluates to a function call
'(+ 1 2 3)           ;; => (+ 1 2 3) ; evaluates to a list 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Primitive Datatypes and Operators 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Numbers
99999999999999999999 ;; integers
#b1010               ;; binary ; => 10
#o10                 ;; octal  ; => 8
#x8ded               ;; hexadecimal ; => 36333
3.14                 ;; real
6.02e+23
3/4                  ;; rational

;;Characters and Strings
#\A                  ;; A char
"Hello, World!"      ;; strings are fixed-length arrays of characters

;; Booleans
#t                  ;; true
#f                  ;; false

;; Function call is written as (f x y z ...)
;; where f is a function and x,y,z, ... are arguments
(print "Hello, World!")    ;; => Hello, World!
;; formatted output
(printf "Hello, ~a.\n" "World")  ;; => Hello, World.

;; print commandline arguments
(map print (command-line-arguments)) 

(list 'foo 'bar 'baz)          ;; => (foo bar baz)
(string-append "pine" "apple") ;; => "pineapple"
(string-ref "tapioca" 3)       ;; => #\i;; character 'i' is at index 3
(string->list "CHICKEN")       ;; => (#\C #\H #\I #\C #\K #\E #\N)
(string->intersperse '("1" "2") ":") ;; => "1:2"
(string-split "1:2:3" ":")     ;; => ("1" "2" "3")


;; Predicates are special functions that return boolean values
(atom? #t)                ;; => #t

(symbol? #t)              ;; => #f

(symbol? '+)              ;; => #t

(procedure? +)            ;; => #t

(pair? '(1 2))            ;; => #t

(pair? '(1 2 . 3))        ;; => #t

(pair? '())               ;; => #f

(list? '())               ;; => #t


;; Some arithmetic operations

(+ 1 1)                   ;; => 2
(- 8 1)                   ;; => 7
(* 10 2)                  ;; => 20
(expt 2 3)                ;; => 8
(remainder 5 2)           ;; => 1
(/ 35 5)                  ;; => 7
(/ 1 3)                   ;; => 0.333333333333333

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You can create variables with define
;; A variable name can use any character except: ()[]{}",'`;#\
(define myvar 5)
myvar        ;; => 5

;; Alias to a procedure
(define ** expt)
(** 2 3)     ;; => 8

;; Accessing an undefined variable raises an exception
s            ;; => Error: unbound variable: s

;; Local binding
(let ((me "Bob"))
  (print me))     ;; => Bob

(print me)        ;; => Error: unbound variable: me

;; Assign a new value to previously defined variable
(set! myvar 10) 
myvar             ;; => 10


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pairs
;; 'cons' constructs pairs, 
;; 'car' extracts the first element, 'cdr' extracts the rest of the elements
(cons 'subject 'verb)       ;; => '(subject . verb)
(car (cons 'subject 'verb)) ;; => subject
(cdr (cons 'subject 'verb)) ;; => verb

;; Lists
;; cons creates a new list if the second item is a list
(cons 0 '())         ;; => (0)
(cons 1 (cons 2  (cons 3 '())))    ;; => (1 2 3)
;; 'list' is a convenience variadic constructor for lists
(list 1 2 3)    ;; => (1 2 3)


;; Use 'append' to append lists together
(append '(1 2) '(3 4)) ;; => (1 2 3 4)

;; Some basic operations on lists
(map add1 '(1 2 3))    ;; => (2 3 4)
(reverse '(1 3 4 7))   ;; => (7 4 3 1)
(sort '(11 22 33 44) >)   ;; => (44 33 22 11)

(define days '(SUN MON FRI))
(list-ref days 1)      ;; => MON
(set! (list-ref days 1) 'TUE)
days                   ;; => (SUN TUE FRI)

;; Vectors
;; Vectors are heterogeneous structures whose elements are indexed by integers
;; A Vector typically occupies less space than a list of the same length
;; Random access of an element in a vector is faster than in a list
#(1 2 3)                     ;; => #(1 2 3) ;; literal syntax
(vector 'a 'b 'c)            ;; => #(a b c) 
(vector? #(1 2 3))           ;; => #t
(vector-length #(1 (2) "a")) ;; => 3
(vector-ref #(1 (2) (3 3)) 2);; => (3 3)

(define vec #(1 2 3))
(vector-set! vec 2 4)
vec                         ;; => #(1 2 4)

;; Vectors can be created from lists and vice-verca
(vector->list #(1 2 4))     ;; => '(1 2 4)
(list->vector '(a b c))     ;; => #(a b c)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use 'lambda' to create functions.
;; A function always returns the value of its last expression
(lambda () "Hello World")   ;; => #<procedure (?)> 

;; Use extra parens around function definition to execute 
((lambda () "Hello World")) ;; => Hello World ;; argument list is empty

;; A function with an argument
((lambda (x) (* x x)) 3)           ;; => 9
;; A function with two arguments
((lambda (x y) (* x y)) 2 3)       ;; => 6

;; assign a function to a variable
(define sqr (lambda (x) (* x x)))
sqr                        ;; => #<procedure (sqr x)>
(sqr 3)                    ;; => 9

;; We can shorten this using the function definition syntactic sugar
(define (sqr x) (* x x))
(sqr 3)                    ;; => 9

;; We can redefine existing procedures
(foldl cons '() '(1 2 3 4 5)) ;; => (((((() . 1) . 2) . 3) . 4) . 5)
(define (foldl func accu alist)
  (if (null? alist)
    accu
    (foldl func (func (car alist) accu) (cdr alist))))

(foldl cons '() '(1 2 3 4 5))   ;; => (5 4 3 2 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5. Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For numbers use '='
(= 3 3.0)                  ;; => #t
(= 2 1)                    ;; => #f

;; 'eq?' returns #t if two arguments refer to the same object in memory
;; In other words, it's a simple pointer comparision.
(eq? '() '())              ;; => #t ;; there's only one empty list in memory
(eq? (list 3) (list 3))    ;; => #f ;; not the same object
(eq? 'yes 'yes)            ;; => #t
(eq? 3 3)                  ;; => #t ;; don't do this even if it works in this case
(eq? 3 3.0)                ;; => #f ;; it's better to use '=' for number comparisions
(eq? "Hello" "Hello")      ;; => #f

;; 'eqv?' is same as 'eq?' all datatypes except numbers and characters
(eqv? 3 3.0)               ;; => #f
(eqv? (expt 2 3) (expt 2 3)) ;; => #t
(eqv? 'yes 'yes)           ;; => #t

;; 'equal?' recursively compares the contents of pairs, vectors, and strings,
;; applying eqv? on other objects such as numbers and symbols. 
;; A rule of thumb is that objects are generally equal? if they print the same.

(equal? '(1 2 3) '(1 2 3)) ;; => #t
(equal? #(a b c) #(a b c)) ;; => #t
(equal? 'a 'a)             ;; => #t
(equal? "abc" "abc")       ;; => #f

;; In Summary:
;; eq? tests if objects are identical
;; eqv? tests if objects are operationally equivalent
;; equal? tests if objects have same structure and contents

;; Comparing strings for equality
(string=? "Hello" "Hello") ;; => #t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6. Control Flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Conditionals
(if #t                     ;; test expression
  "True"                   ;; then expression
  "False")                 ;; else expression
                           ;; => "True"

(if (> 3 2)
  "yes"
  "no")                    ;; => "yes"

;; In conditionals, all values that are not '#f' are treated as true.
;; 0, '(), #() "" , are all true values
(if 0
  "0 is not false"
  "0 is false")            ;; => "0 is not false"

;; 'cond' chains a series of tests and returns as soon as it encounters a true condition
;; 'cond' can be used to simulate 'if/elseif/else' statements
(cond ((> 2 2) "not true so don't return this")
      ((< 2 5) "true, so return this")
      (else "returning default"))    ;; => "true, so return this"


;; A case expression is evaluated as follows:
;; The key is evaluated and compared with each datum in sense of 'eqv?',
;; The corresponding clause in the matching datum is evaluated and returned as result
(case (* 2 3)              ;; the key is 6
  ((2 3 5 7) 'prime)       ;; datum 1
  ((1 4 6 8) 'composite))  ;; datum 2; matched!
                           ;; => composite

;; case with else clause
(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else 'consonant))       ;; =>  consonant

;; Boolean expressions
;; 'and' returns the first expression that evaluates to #f
;; otherwise, it returns the result of the last expression
(and #t #f (= 2 2.0))                ;; => #f
(and (< 2 5) (> 2 0) "0 < 2 < 5")    ;; => "0 < 2 < 5"

;; 'or' returns the first expression that evaluates to #t 
;; otherwise the result of the last expression is returned
(or #f #t #f)                        ;; => #t
(or #f #f #f)                        ;; => #f

;; 'when' is like 'if' without the else expression
(when (positive? 5) "I'm positive")  ;; => "I'm positive"

;; 'unless' is equivalent to (when (not <test>) <expr>)
(unless (null? '(1 2 3)) "not null") ;; => "not null"


;; Loops
;; loops can be created with the help of tail-recursions
(define (loop count)
  (unless (= count 0)
    (print "hello") 
    (loop (sub1 count))))
(loop 4)                             ;; => hello, hello ...

;; Or with a named let
(let loop ((i 0) (limit 5))
  (when (< i limit)
    (printf "i = ~a\n" i)
    (loop (add1 i) limit)))          ;; => i = 0, i = 1....

;; 'do' is another iteration construct
;; It initializes a set of variables and updates them in each iteration
;; A final expression is evaluated after the exit condition is met
(do ((x 0 (add1 x )))            ;; initialize x = 0 and add 1 in each iteration
  ((= x 10) (print "done"))      ;; exit condition and final expression
  (print x))                     ;; command to execute in each step
                                 ;; => 0,1,2,3....9,done

;; Iteration over lists 
(for-each (lambda (a) (print (* a a)))
          '(3 5 7))                  ;; => 9, 25, 49

;; 'map' is like for-each but returns a list
(map add1 '(11 22 33))               ;; => (12 23 34)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7. Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The CHICKEN core is very minimal, but additional features are provided by library extensions known as Eggs.
;; You can install Eggs with 'chicken-install <eggname>' command.

;; 'numbers' egg provides support for full numeric tower.
(require-extension numbers)
;; complex numbers
3+4i                               ;; => 3+2i
;; Supports fractions without falling back to inexact flonums
1/3                                ;; => 1/3
;; provides support for large integers through bignums
(expt 9 20)                        ;; => 12157665459056928801 
;; And other 'extended' functions
(log 10 (exp 1))                   ;; => 2.30258509299405
(numerator 2/3)                    ;; => 2

;; 'utf8' provides unicode support
(require-extension utf8)
"\u03BBx:(\u03BC\u0251.\u0251\u2192\u0251).xx" ;; => "λx:(μɑ.ɑ→ɑ).xx"

;; 'posix' provides file I/O and lots of other services for unix-like operating systems
;; Some of the functions are not available in Windows system,
;; See http://wiki.call-cc.org/man/4/Unit%20posix for more details

;; Open a file to append, open "write only" and create file if it does not exist
(define outfn (file-open "chicken-hen.txt" (+ open/append open/wronly open/creat)))
;; write some text to the file
(file-write outfn "Did chicken came before hen?") 
;; close the file
(file-close outfn)
;; Open the file "read only"
(define infn (file-open "chicken-hen.txt" open/rdonly))
;; read some text from the file
(file-read infn 30)         ;; => ("Did chicken came before hen?  ", 28)
(file-close infn)

;; CHICKEN also supports SRFI (Scheme Requests For Implementation) extensions
;; See 'http://srfi.schemers.org/srfi-implementers.html" to see srfi's supported by CHICKEN
(require-extension srfi-1)         ;; list library
(filter odd? '(1 2 3 4 5 6 7))     ;; => (1 3 5 7)
(count even? '(1 2 3 4 5))         ;; => 2
(take '(12 24 36 48 60) 3)         ;; => (12 24 36)
(drop '(12 24 36 48 60) 2)         ;; => (36 48 60)
(circular-list 'z 'q)              ;; => z q z q ...

(require-extension srfi-13)        ;; string library
(string-reverse "pan")             ;; => "nap"
(string-index "Turkey" #\k)        ;; => 3
(string-every char-upper-case? "CHICKEN") ;; => #t
(string-join '("foo" "bar" "baz") ":")    ;; => "foo:bar:baz"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A 'for .. in ..' iteration like python, for lists
(define-syntax for
  (syntax-rules (in)
                ((for elem in alist body ...)
                 (for-each (lambda (elem) body ...) alist))))

(for x in '(2 4 8 16)
     (print x))          ;; => 2, 4, 8, 16

(for chr in (string->list "PENCHANT")
     (print chr))        ;; => P, E, N, C, H, A, N, T

;; While loop
(define-syntax while
  (syntax-rules ()
                ((while cond body ...)
                 (let loop ()
                   (when cond
                     body ...
                     (loop))))))

(let ((str "PENCHANT") (i 0))
  (while (< i (string-length str))     ;; while (condition)
         (print (string-ref str i))    ;; body 
         (set! i (add1 i))))           
                                       ;; => P, E, N, C, H, A, N, T

;; Advanced Syntax-Rules Primer -> http://petrofsky.org/src/primer.txt
;; Macro system in chicken -> http://lists.gnu.org/archive/html/chicken-users/2008-04/msg00013.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 9. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Also See http://wiki.call-cc.org/man/4/Modules

;; The 'test' module exports a value named 'hello' and a macro named 'greet'
(module test (hello greet)
  (import scheme)

  (define-syntax greet
    (syntax-rules ()
      ((_ whom) 
       (begin
         (display "Hello, ")
         (display whom)
         (display " !\n") ) ) ) )

  (define (hello)
    (greet "world") )  )

;; we can define our modules in a separate file (say test.scm) and load them to the interpreter with
;;         (load "test.scm")

;; import the module
(import test)
(hello)                ;; => Hello, world !
(greet "schemers")     ;; => Hello, schemers !

;; We can compile the module files in to shared libraries by using following command,
;;         csc -s test.scm
;;         (load "test.so")

;; Functors
;; Functors are high level modules that can be parameterized by other modules
;; Following functor requires another module named 'M' that provides a function called 'multiply'
;; The functor itself exports a generic function 'square'
(functor (squaring-functor (M (multiply))) (square)
         (import scheme M) 
         (define (square x) (multiply x x)))

;; Module 'nums' can be passed as a parameter to 'squaring-functor'
(module nums (multiply) 
        (import scheme)     ;; predefined modules
        (define (multiply x y) (* x y))) 
;; the final module can be imported and used in our program
(module number-squarer = (squaring-functor nums)) 

(import number-squarer)
(square 3)              ;; => 9

;; We can instantiate the functor for other inputs
;; Here's another example module that can be passed to squaring-functor
(module stars (multiply)
        (import chicken scheme)  ;; chicken module for the 'use' keyword
        (use srfi-1)             ;; we can use external libraries in our module
        (define (multiply x y)
          (list-tabulate x (lambda _ (list-tabulate y (lambda _ '*))))))
(module star-squarer = (squaring-functor stars))

(import star-squarer)
(square 3)              ;; => ((* * *)(* * *)(* * *))

```
## Further Reading
* [CHICKEN User's Manual](http://wiki.call-cc.org/man/4/The%20User%27s%20Manual).
* [RSR5 standards](http://www.schemers.org/Documents/Standards/R5RS)


## Extra Info

* [For programmers of other languages](http://wiki.call-cc.org/chicken-for-programmers-of-other-languages)
* [Compare CHICKEN syntax with other languages](http://plr.sourceforge.net/cgi-bin/plr/launch.py)
