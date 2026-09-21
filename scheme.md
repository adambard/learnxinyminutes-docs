---
name: "Scheme"
filename: scheme.scm
contributors:
- ["Bruno G. Ciccarino", "https://github.com/xenodesire"]
---

Scheme is a minimalist dialect of Lisp that is widely used in education, research, and industry. It emphasizes simplicity, powerful abstractions, and functional programming paradigms.

It is important to clarify that there is no single implementation of Scheme; rather, each implementation has its own characteristics and use cases, as I demonstrate in this table:

| Implementation  | Characteristic  |
|---|---|
| Racket  | large ecosystem, custom extensions  |
| GNU/Guile  | strong C/GNU integration  |
| Chez Scheme  | fast/compiled implementation  |
| Chicken  | compiles Scheme to C MIT/GNU Scheme	traditional/educational  |
| Gambit  | compilation and embedded systems  |

A classic resource to learn Scheme is [Structure and Interpretation of Computer Programs (SICP)](https://web.mit.edu/6.001/6.037/sicp.pdf). For a modern introduction, consider [The Scheme Programming Language](https://www.scheme.org/).

To follow this tutorial, I strongly recommend using Emacs as your text editor, although you can have a very pleasant experience using Neovim + Conjure. 

> When entering a relatively new field and just starting out, it is very easy to confuse the essence of what you are doing with the tools you are using. - Harold Abelson

I think this specific quote captures much of what I’m trying to convey here.


```scheme
;;;-----------------------------------------------------------------------------
;;; 0. Syntax
;;;-----------------------------------------------------------------------------

;;; General form

;;; Scheme has two fundamental elements of syntax: ATOM and S-EXPRESSION.
;;; S-expressions are used for both data and code.

10          ; a number atom; evaluates to itself
'symbol     ; a symbol atom; evaluates to itself when quoted
#t          ; boolean true
(+ 1 2 3)   ; an s-expression (function application)
'(4 'foo #t) ; quoted s-expression (a list)


;;; Comments

;;; Single-line comments start with a semicolon:
; This is a single-line comment

;;; Block comments use `#|` and `|#`:
#| This is a block comment.
   It spans multiple lines.
|#


;;; REPL and environment

;;; Scheme is typically developed interactively in a Read-Eval-Print Loop (REPL).
;;; Implementations such as Racket, Guile, or MIT Scheme provide REPLs for interactive exploration.
;;; Libraries and tools can be installed depending on the specific implementation.


;;;-----------------------------------------------------------------------------
;;; 1. Primitive datatypes and operators
;;;-----------------------------------------------------------------------------

;;; Numbers

42          ; integers
#b101       ; binary => 5
#o777       ; octal => 511
#xFF        ; hexadecimal => 255
3.14        ; floating-point numbers
1/2         ; fractions (exact rational numbers)
(make-rectangular 1 2) ; complex numbers


;;; Basic arithmetic

(+ 1 2)     ; => 3
(- 7 3)     ; => 4
(* 2 5)     ; => 10
(/ 10 3)    ; => 10/3
(sqrt 4)    ; => 2
(expt 2 3)  ; => 8


;;; Booleans

#t          ; true
#f          ; false
(and #t #f) ; => #f
(or #t #f)  ; => #t
(not #t)    ; => #f


;;; Strings

"Hello, World!"
(string-append "Hello, " "World!") ; => "Hello, World!"


;;; Lists

'(1 2 3)      ; a list
(cons 1 '(2 3)) ; => '(1 2 3)
(car '(1 2 3))  ; => 1
(cdr '(1 2 3))  ; => '(2 3)
(append '(1 2) '(3 4)) ; => '(1 2 3 4)


;;;-----------------------------------------------------------------------------
;;; 2. Variables
;;;-----------------------------------------------------------------------------

;;; Define a variable

(define x 10)
x           ; => 10


;;; Define a local variable

(let ((x 5)) (+ x 10)) ; => 15
x                      ; => 10 (unchanged globally)


;;;-----------------------------------------------------------------------------
;;; 3. Functions
;;;-----------------------------------------------------------------------------

;;; Define a named function

(define (square x)
  (* x x))

(square 4) ; => 16


;;; Define an anonymous (lambda) function

((lambda (x) (* x x)) 5) ; => 25


;;; Higher-order functions

(define (apply-twice f x)
  (f (f x)))

(apply-twice square 2) ; => 16


;;;-----------------------------------------------------------------------------
;;; 4. Conditionals and control flow
;;;-----------------------------------------------------------------------------

;;; If statements

(if (> 5 3)
    'yes
    'no)     ; => 'yes


;;; Cond expressions (multi-branch conditionals)

(cond
  [(< 5 3) 'less]
  [(> 5 3) 'greater]
  [else 'equal]) ; => 'greater


;;;-----------------------------------------------------------------------------
;;; 5. Structs and collections
;;;-----------------------------------------------------------------------------

;;; Define a structure

(define-struct dog (name breed age))

(define my-dog (make-dog "Fido" "Labrador" 5))

(dog-name my-dog)   ; => "Fido"
(dog-age my-dog)    ; => 5


;;;-----------------------------------------------------------------------------
;;; 6. Common patterns
;;;-----------------------------------------------------------------------------

;;; Recursive functions

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5) ; => 120

;;; Tail Recursion and Accumulators

;;; Scheme implementations are required to support proper tail calls,
;;; allowing recursive procedures to be used for iteration without
;;; growing the call stack when the recursive call is in tail position.

;;; An accumulator carries the partial result through each recursive call.

(define (factorial n)
  (define (iter n acc) ; => `n` represents the part of the problem still to process
    (if (= n 0) 
        acc ; => acc contains the result accumulated so far
        (iter (- n 1) (* n acc))))
  (iter n 1))

;;;-----------------------------------------------------------------------------
;;; 7. Libraries and modules
;;;-----------------------------------------------------------------------------

;;; Importing libraries/modules depends on the implementation.
;;; For example, in Racket:

(require racket/math)

(sqrt 16) ; => 4

;;;-----------------------------------------------------------------------------
;;; 8. Macros
;;;-----------------------------------------------------------------------------

;;; Macros allow you to create new syntactic constructs.

(define-syntax when
  (syntax-rules ()
    [(when test body ...)
     (if test
         (begin body ...))]))

(when #t
  (display "Condition is true!\n")) ; Output: Condition is true!


;;;-----------------------------------------------------------------------------
;;; 9. Input and Output (I/O)
;;;-----------------------------------------------------------------------------

;;; Printing to the console

(display "Hello, Scheme!") ; => prints "Hello, Scheme!"
(newline)                  ; => moves to the next line


;;; Reading input

(let ((user-input (read)))
  (display "You entered: ")
  (display user-input))


;;; File I/O

(define output-port (open-output-file "example.txt"))
(display "Writing to a file." output-port)
(close-output-port output-port)

(define input-port (open-input-file "example.txt"))
(let ((file-content (read input-port)))
  (display file-content))
(close-input-port input-port)


;;;-----------------------------------------------------------------------------
;;; 10. Iteration
;;;-----------------------------------------------------------------------------

;;; Iterating with `do`

(do ((i 0 (+ i 1)))        ; initialize i to 0, increment by 1
    ((>= i 5))             ; stop when i >= 5
  (display i)              ; print i
  (newline))


;;; Using recursion for iteration

(define (countdown n)
  (if (= n 0)
      (display "Blastoff!\n")
      (begin
        (display n)
        (newline)
        (countdown (- n 1)))))

(countdown 5) ; Output: 5 4 3 2 1 Blastoff!


;;;-----------------------------------------------------------------------------
;;; 11. Error handling
;;;-----------------------------------------------------------------------------

;;; Using `guard` for error handling (Racket example)

(guard [e (displayln (format "Error: ~a" e))]
  (/ 1 0)) ; Output: Error: division by zero


;;; Catching exceptions manually

(with-handlers ([exn:fail? (lambda (e) (displayln "Caught an error!"))])
  (error "Something went wrong!")) ; Output: Caught an error!


;;;-----------------------------------------------------------------------------
;;; 12. Advanced concepts
;;;-----------------------------------------------------------------------------

;;; Continuations with `call/cc`

(call/cc
 (lambda (cont)
   (display "Before continuation\n")
   (cont #f)
   (display "After continuation\n"))) ; Output: Before continuation


;;; Lazy evaluation (streams)

;;; Lazy evaluation is a form of computation in which values are not calculated until they are needed. in Elisp, you can implement something similar using these garbage collector (GC) settings:
;;;
;;; (setq gc-cons-threshold (* 512 1024 1024)
;;;      gc-cons-percentage 0.6)
;;;
;;; (add-hook 'emacs-startup-hook
;;;          (lambda ()
;;;            (setq gc-cons-threshold (* 100 100 8)
;;;                  gc-cons-percentage 0.1)))
;;;

(define lazyeval (delay (+ 1 2))) ; Value: lazyeval 
(promise? lazyeval) ; Value 11: #[promise 11] return #t 
(force lazyeval) ; Value: 3
(* 10 (force lazyeval)) ; Value: 30

;;;-----------------------------------------------------------------------------
;;; 13. Meta-programming
;;;-----------------------------------------------------------------------------

;;; Evaluate expressions dynamically

(eval '(+ 1 2)) ; => 3


;;; Quasiquoting for meta-programming

`(1 2 ,(+ 3 4)) ; => '(1 2 7)

```
