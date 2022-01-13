;; This gives an introduction to Scheme in 15 minutes
;;
;; First make sure you read this text by Peter Norvig:
;; http://norvig.com/21-days.html
;;
;; Then install GNU Guile
;; NOTE: I won't encourage using Racket but if you really want,
;;       I still give you some hints in this tutorial. But the
;;       tutorial will base on Guile and RnRs.
;;
;; openSUSE: zypper install guile
;; Debian: apt-get install guile-2.0 (or see your distro instructions)
;; MacOSX: Building Guile 2.0 on the Mac
;;         http://irrealblog.blogspot.hk/2011/03/building-guile-2.html
;; Windows try web: http://repl.it/languages/Scheme
;;
;; More general information can be found at:
;; http://www.gnu.org/software/guile

;; Important warning:
;;
;; Going through this tutorial won't damage your computer unless
;; you get so angry that you throw it on the floor.  In that case,
;; I hereby decline any responsability.  Have fun!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Fire up Scheme:
;; Type 'guile' for GNU Guile
;; Or just use the browser for web version
;;
;; Now look at the prompt:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Semi-colons start comments anywhere on a line.
;;
;; Scheme programs are made of symbolic expressions (s-exps):
(+ 2 2)

;; This symbolic expression reads as "Add 2 to 2".

;; Sexps are enclosed into parentheses, possibly nested:
(+ 2 (+ 1 1))

;; A symbolic expression contains atoms or other symbolic
;; expressions.  In the above examples, 1 and 2 are atoms,
;; (+ 2 (+ 1 1)) and (+ 1 1) are symbolic expressions.

(+ 3 (+ 1 2))
;; => 6

;; `set!' stores a value into a variable:
;; Please define my-name first, or you can't assign it.
(define my-name "unknown")
(set! my-name "NalaGinrut")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Primitive Datatypes and Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numbers
9999999999999999999999 ; integers
#b111                  ; binary => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
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
(expt 2 3) ; => 8
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
;; According to RnRs, characters only have two notations:
;; #\ and #\x
;; Racket support #\u, but it's never Scheme.
#\A ; => #\A
#\λ ; => #\λ
#\x03BB ; => #\λ

;;; Strings are fixed-length array of characters.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; backslash is an escaping character
"Foo\tbar\x21\a\r\n" ; includes C escapes (only support hex)
;; try to print the above string
;; Printing is pretty easy
(display "I'm Guile. Nice to meet you!\n")
;; and unicode escapes
"\u004B" ; => K

;; Strings can be added too!
(string-append "Hello " "world!") ; => "Hello world!"

;; A string can be treated like a list of characters
(string-ref "Apple" 0) ; => #\A

;; format can be used to format strings:
(format #t "~a can be ~a" "strings" "formatted")
;; ==> print "strings can be formatted" on screen
(define str (format #f "~a can be ~a" "strings" "formatted"))
;; str was assigned to "strings can be formatted"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can create a variable using define
;; a variable name can use any character except: ()[]{}",'`;#|\
(define some-var 5)
some-var ; => 5

;; Accessing a previously unassigned variable is an exception
; x ; => x: undefined ...

;; Local binding: `me' is bound to "Bob" only within the (let ...)
(let ((me "Bob"))
  "Alice"
  me) 
;; => "Bob"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Structs and Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Record Type (Skip this chapter if you're trying web version
(use-modules (srfi srfi-9))
(define-record-type dog 
  (make-dog name breed age)
  dog?
  (name dog-name)
  (breed dog-breed)
  (age dog-age))
(define my-pet
  (make-dog "lassie" "collie" 5))
my-pet ; => #<dog>
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"

;;; Pairs (immutable)
;; `cons' constructs pairs, `car' and `cdr' extract the first
;; and second elements
(cons 1 2) ; => '(1 . 2)
(car (cons 1 2)) ; => 1
(cdr (cons 1 2)) ; => 2

;;; Lists

;; Lists are linked-list data structures, made of `cons' pairs and end
;; with a '() to mark the end of the list
(cons 1 (cons 2 (cons 3 '()))) ; => '(1 2 3)
;; `list' is a convenience variadic constructor for lists
(list 1 2 3) ; => '(1 2 3)
;; and a quote can also be used for a literal list value
'(1 2 3) ; => '(1 2 3)

;; Can still use `cons' to add an item to the beginning of a list
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;; Use `append' to add lists together
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;; Lists are a very basic type, so there is a *lot* of functionality for
;; them, a few examples:
;; For Racket users:
(map add1 '(1 2 3))          ; => '(2 3 4)
;; For Guile users:
(map 1+ '(1 2 3))	     ; => '(2 3 4)
;; add1 or 1+ is not a standard primitive, so it depends on implementations.

(map + '(1 2 3) '(10 20 30)) ; => '(11 22 33)

;; filter/count/take/drop are dwell in SRFI-1, so you have to load it first.
;; For Racket users:
(require srfi/1)
;; For Guile users:
(use-modules (srfi srfi-1))

(filter even? '(1 2 3 4))    ; => '(2 4)
(count even? '(1 2 3 4))     ; => 2
(take '(1 2 3 4) 2)          ; => '(1 2)
(drop '(1 2 3 4) 2)          ; => '(3 4)

;;; Vectors

;; Vectors are fixed-length arrays
#(1 2 3) ; => '#(1 2 3)

;; Use `vector-append' to add vectors together
;; NOTE: vector-append is in SRFI-43 which is not supported in Guile-2.0.9
;;       or earlier. And it may not be added in Guile-2.0.10.
;;       But it's proposed in R7RS, and there's a r7rs branch in Guile upstream.
;;       If your Guile doesn't support vector-append, please skip this step.
(vector-append #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; Hashes

;; Create mutable hash table
;; For GNU Guile
(define m (make-hash-table))
(hash-set! m 'a 1)
(hash-set! m 'b 2)
(hash-set! m 'c 3)

;; Retrieve a value
(hash-ref m 'a) ; => 1

;; Retrieving a non-present value is an exception
(hash-ref m 'd) 
;; => #f 

;; You can provide a default value for missing keys
(hash-ref m 'd 0)
;; => 0

;; Use `hash-remove' to remove keys (functional too)
(hash-remove! m 'a) ; => ((b . 2) (c . 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use `lambda' to create functions.
;; A function always returns the value of its last expression
(lambda () "Hello World") ; => #<procedure>

;; Use parens to call all functions, including a lambda expression
((lambda () "Hello World")) ; => "Hello World"
((lambda (x) (+ x x)) 5)      ; => 10

;; Assign a function to a var
(define hello-world (lambda () "Hello World"))
(hello-world) ; => "Hello World"

;; You can shorten this using the function definition syntatcic sugar:
(define (hello-world2) "Hello World")
(hello-world2) ; => "Hello World"

;; The () in the above is the list of arguments for the function
(define hello
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve") ; => "Hello Steve"
;; ... or equivalently, using a sugared definition:
(define (hello2 name)
  (string-append "Hello " name))

;; You can have multi-variadic functions too, using `case-lambda'
(define hello3
  (case-lambda
    (() "Hello World")
    ((name) (string-append "Hello " name))))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"
;; ... or specify optional arguments with a default value expression
(define* (hello4 #:key (name "World"))
  (string-append "Hello " name))

;; Functions can pack extra arguments up in a list
(define (count-args . args)
  (format #t "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"
;; ... or with the unsugared `lambda' form:
(define count-args2
  (lambda args
    (format #t "You passed ~a args: ~a" (length args) args)))

;; You can mix regular and packed arguments
(define (hello-count name . args)
  (format #t "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"
;; ... unsugared:
(define hello-count2
  (lambda (name . args)
    (format #t "Hello ~a, you passed ~a extra args" name (length args))))

;; And with keywords
;; the keywords are those like this #:its-name, sometimes you may see
;; :its-name without '#' in certain Scheme implementation.
;; NOTE: keywords is not in any Scheme standards like RnRs.
;;       But mainstream Scheme implementation often contains it. 
;;	 This truth also means that different implementation may has
;;	 different result.
;;       The code below will follow Guile situation.
(define* (hello #:key (name "World") (greeting "Hello") . args)
  (format #t "~a ~a, ~a extra args~%" greeting name (length args)))
;; 'define*' is similar with 'define', but you may use it for defining
;; optional args or using keyword to specify the value to specified argument.
(hello)                 ; => "Hello World, 0 extra args"
(hello 1 2 3)           ; => "Hello World, 3 extra args"
(hello #:greeting "Hi") ; => "Hi World, 2 extra args"
;; NOTE: In Guile, all the keyword-value pairs are countered as the rest args.
;;       It means 'args' here will be (#:greeting "Hi"), so its length is 2.
;;       If you are in Racket, it's different, so the result will be 0.
;;       Because it's not defined in RnRs, so it's implementation specific.
(hello #:name "Finn" #:greeting "Hey") ; => "Hey Finn, 4 extra args"
(hello 1 2 3 #:greeting "Hi" #:name "Finn" 4 5 6) ; => "Hi Finn, 10 extra args"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for numbers use `='
(= 3 3.0) ; => #t
(= 2 1) ; => #f

;; for characters use `char=?'
(char=? #\c #\c) ; => #t

;; for object identity use `eq?'
;; WARNING: don't use `eq?' on numbers and characters, the reason is very simple:
;;          because RnRs treat this rule as undefined! 
;;(eq? 3 3) ; => Wrong! it's undefined! So it depends on implementation!
;; No matter if you got #t from above, it's a wrong usage!!!
;; Should use (= 3 3) or (eqv? 3 3)

(eq? (list 3) (list 3)) ; => #f
;; Why it's #f? Because comparison between objects depends on their head-pointers.
;; These two lists are different objects, and they have different head-pointers.

(eq? 'a 'a) ; => #t
;; Symbols are the typical objects to compare with their head-pointers

;; eqv?
;; The difference between `eqv?' and `eq':
;; You can compare numbers and characters with `eqv?'
;; When you use `eqv?' for any objects other than numbers&characters, it's the
;; same with `eq?'.
(eqv? 3 3)     ; => #t
(eqv? #\c #\c) ; => #t
(eqv? 'a 'a)   ; => #t

;; for collections use `equal?'
;; `equal?' will compare all the values in a collections type like record or list.
;; You can compare any objects with `equal?' safely, but inefficiency.
(equal? (list 'a 'b) (list 'a 'b)) ; => #t
(equal? (list 'a 'b) (list 'b 'a)) ; => #f

;; Choosing proper equal pred for you code, is one of the art in Scheme programming!

(eqv? "abcd" "abcd") ; => unspecified
;; Obviously, strings are collections type, so...it's your EXECISE now!

;; Another EXECISE, what's the proper 'equal-pred' for functions?
(what-to-use? (lambda (x) (1+ x)) (lambda (x) (1+ x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Control Flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conditionals

(if #t               ; test expression
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;; In conditionals, all non-#f values are treated as true
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(Groucho Zeppo)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'yep

;; `cond' chains a series of tests to select a result
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (else 'ok)) ; => 'ok

;;; Pattern Matching
;; For Racket users:
(require racket/match) ; use match module
(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    ((list 0 0) 'fizzbuzz)
    ((list 0 _) 'fizz)
    ((list _ 0) 'buzz)
    (else #f)))

;; For Guile users:
(use-modules (ice-9 match)) ; use match module
(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    ((0 0) 'fizzbuzz)
    ((0 _) 'fizz)
    ((_ 0) 'buzz)
    (else #f)))

(fizzbuzz? 15) ; => 'fizzbuzz
(fizzbuzz? 37) ; => #f

;;; Loops

;; Looping can be done through (tail-) recursion
(define (lp i)
  (when (< i 10)
    (format #t "i=~a\n" i)
    (lp (1+ i))))
(lp 5) ; => i=5, i=6, ...

;; Similarly, with a named let
(let lp ((i 0))
  (when (< i 10)
    (format #t "i=~a\n" i)
    (lp (1+ i)))) ; => i=0, i=1, ...

;; how to get a range? just like range(0, 9)?
;; the original 'iota' only accept one para
(iota 10) ; ==> (0 1 2 3 4 5 6 7 8 9)
;; the 'iota' in srfi-1 was extended 
(use-modules (srfi srfi-1))
;; #<procedure iota (count #:optional start step)>
(iota 5 10) ; => (10 11 12 13 14) 
;; means from 10 count 5 times, each step +1 (plus one is default)
(iota 5 10 2) ; => (10 12 14 16 18)
;; from 10 count 5 times, each step +2
;; If you need a Python like range(5, 10) ==> (5 6 7 8 9), try:
(define (range from to) (map (lambda (x) (+ from x)) (iota (- to from))))
;; EXECISE: you may find this 'range' implementation is not so good,
;;          please optimize it if you can.
(range 5 10) ; => (5 6 7 8 9)

;; how to do iteration?
(for-each display '(1 2 3 4 5))
;; => 12345
(for-each (lambda (i) (format #t "i=~a\n" i))
          (iota 10)) ; => i=0, i=1, ...
(for-each (lambda (i) (format #t "i=~a\n" i))
          (range 5 10)) ; => i=5, i=6, ...

;;; Iteration Over Other Sequences
;; `for' allows iteration over many other kinds of sequences:
;; lists, vectors, strings, sets, hash tables, etc...
(for-each display '(l i s t))
;; => list
(define vector-for-each (@ (rnrs) vector-for-each))
;; export vector-for-each from rnrs only
(vector-for-each display #(v e c t o r))
;; => vector
(string-for-each display "string")
;; => string
;;; More Iterations
(do ((i 10 (1+ i)) (j '(x y z) (cdr j))) 
    ((null? j)) ; if j is '(), just end the loop
  (format #t "~a:~a " i (car j)))
; => 0:x 1:y 2:z

;;; Exceptions

;; To catch exceptions, use the 'catch' form
(catch 'my-error 
  (lambda () (throw 'my-error))
  (lambda e (display "oh~my error!\n")))
; => oh~my error!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use `set!' to assign a new value to an existing variable
(define n 5)
(set! n (1+ n))
n ; => 6

;; Use fluid for explicitly mutable values
(define n* (make-fluid 5))
(fluid-set! n* (1+ (fluid-ref n*)))
(fluid-ref n*) ; => 6

;; Many Guile datatypes are immutable (pairs, lists, etc), some come in
;; both mutable and immutable flavors (strings, vectors, hash tables,
;; etc...)

;; Use `vector' or `make-vector' to create mutable vectors
(define vec (vector 2 2 3 4))
(define wall (make-vector 100 'bottle-of-beer))
;; Use vector-set! to update a slot
(vector-set! vec 0 1)
(vector-set! wall 99 'down)
vec ; => #(1 2 3 4)

;; Create an empty mutable hash table and manipulate it
(define m3 (make-hash-table))
(hash-set! m3 'a 1)
(hash-set! m3 'b 2)
(hash-set! m3 'c 3)
(hash-ref m3 'a)   ; => 1
(hash-ref m3 'd 0) ; => 0
(hash-remove! m3 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modules let you organize code into multiple files and reusable
;; libraries; Make sure put all the module code in one file, since 
;; the modules split as files. And the module name should be same 
;; with the filename, say, module named (my-cake) is 'my-cake.scm',
;; and module named (mods my-cake) is 'mods/my-cake.scm',
;; (mods submods my-cake) ==> 'mods/submods/my-cake.scm'.
;; ---begin my-cake.scm---
(define-module (my-cake) ; define a `cake' module based on racket/base
  #:use-module (ice-9 format) ; the pre-requisition of current module
  #:export (print-cake)) ; function exported by the module

  (define (show fmt n ch) ; internal function
    (format #t fmt (make-string n ch))
    (newline))

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
;; --end my-cake.scm---

;; Be sure that the path of 'my-cake.scm' is in your current 
;; %load-path list. Use `use-modules' to get all `provide'd names 
;; from a module.
(use-modules (my-cake)) ; the ' is for a local submodule
(print-cake 3)
; (show "~a" 1 #\A) ; => error, `show' was not exported

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Classes and Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
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

(let ((i 0))
  (while (< i  10)
    (display i)
    (set! i (1+ i))))

;; Macros are hygienic, you cannot clobber existing variables!
(define-syntax-rule (swap! x y) ; -! is idomatic for mutation
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))

(define tmp 1)
(define a 2)
(define b 3)
(swap! a b)
(format #t "tmp = ~a; a = ~a; b = ~a\n" tmp a b) ; tmp is unaffected

;; But they are still code transformations, for example:
(define-syntax-rule (bad-while condition body ...)
  (when condition
    body ...
    (bad-while condition body ...)))
;; this macro is broken: it generates infinite code, if you try to use
;; it, the compiler will get in an infinite loop
