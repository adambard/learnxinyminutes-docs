---

language: "Common Lisp"
filename: commonlisp.lisp
contributors:
  - ["Paul Nathan", "https://github.com/pnathan"]
  - ["Rommel Martinez", "https://ebzzry.io"]
---

Common Lisp is a general-purpose, multi-paradigm programming language suited for a wide variety of
industry applications. It is frequently referred to as a programmable programming language.

The classic starting point is [Practical Common Lisp](http://www.gigamonkeys.com/book/). Another
popular and recent book is [Land of Lisp](http://landoflisp.com/). A new book about best practices,
[Common Lisp Recipes](http://weitz.de/cl-recipes/), was recently published.



```lisp
;;;-----------------------------------------------------------------------------
;;; 0. Syntax
;;;-----------------------------------------------------------------------------

;;; General form

;;; CL has two fundamental pieces of syntax: ATOM and S-EXPRESSION.
;;; Typically, grouped S-expressions are called `forms`.

10            ; an atom; it evaluates to itself
:thing        ; another atom; evaluating to the symbol :thing
t             ; another atom, denoting true
(+ 1 2 3 4)   ; an s-expression
'(4 :foo t)   ; another s-expression


;;; Comments

;;; Single-line comments start with a semicolon; use four for file-level
;;; comments, three for section descriptions, two inside definitions, and one
;;; for single lines. For example,

;;;; life.lisp

;;; Foo bar baz, because quu quux. Optimized for maximum krakaboom and umph.
;;; Needed by the function LINULUKO.

(defun meaning (life)
  "Return the computed meaning of LIFE"
  (let ((meh "abc"))
    ;; Invoke krakaboom
    (loop :for x :across meh
       :collect x)))                    ; store values into x, then return it

;;; Block comments, on the other hand, allow for free-form comments. They are
;;; delimited with #| and |#

#| This is a block comment which
   can span multiple lines and
    #|
       they can be nested!
    |#
|#


;;; Environment

;;; A variety of implementations exist; most are standards-conformant. SBCL
;;; is a good starting point. Third party libraries can be easily installed with
;;; Quicklisp

;;; CL is usually developed with a text editor and a Read Eval Print
;;; Loop (REPL) running at the same time. The REPL allows for interactive
;;; exploration of the program while it is running "live".


;;;-----------------------------------------------------------------------------
;;; 1. Primitive datatypes and operators
;;;-----------------------------------------------------------------------------

;;; Symbols

'foo ; => FOO  Notice that the symbol is upper-cased automatically.

;;; INTERN manually creates a symbol from a string.

(intern "AAAA")        ; => AAAA
(intern "aaa")         ; => |aaa|

;;; Numbers

9999999999999999999999 ; integers
#b111                  ; binary => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; ratios
#C(1 2)                ; complex numbers

;;; Function application are written as (f x y z ...) where f is a function and
;;; x, y, z, ... are the arguments.

(+ 1 2)                ; => 3

;;; If you want to create literal data, use QUOTE to prevent it from being
;;; evaluated

(quote (+ 1 2))        ; => (+ 1 2)
(quote a)              ; => A

;;; The shorthand for QUOTE is '

'(+ 1 2)               ; => (+ 1 2)
'a                     ; => A

;;; Basic arithmetic operations

(+ 1 1)                ; => 2
(- 8 1)                ; => 7
(* 10 2)               ; => 20
(expt 2 3)             ; => 8
(mod 5 2)              ; => 1
(/ 35 5)               ; => 7
(/ 1 3)                ; => 1/3
(+ #C(1 2) #C(6 -4))   ; => #C(7 -2)

;;; Booleans

t                      ; true; any non-NIL value is true
nil                    ; false; also, the empty list: ()
(not nil)              ; => T
(and 0 t)              ; => T
(or 0 nil)             ; => 0

;;; Characters

#\A                    ; => #\A
#\λ                    ; => #\GREEK_SMALL_LETTER_LAMDA
#\u03BB                ; => #\GREEK_SMALL_LETTER_LAMDA

;;; Strings are fixed-length arrays of characters

"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; backslash is an escaping character

;;; Strings can be concatenated

(concatenate 'string "Hello, " "world!") ; => "Hello, world!"

;;; A string can be treated like a sequence of characters

(elt "Apple" 0) ; => #\A

;;; FORMAT is used to create formatted output, which ranges from simple string
;;; interpolation to loops and conditionals. The first argument to FORMAT
;;; determines where will the formatted string go. If it is NIL, FORMAT
;;; simply returns the formatted string as a value; if it is T, FORMAT outputs
;;; to the standard output, usually the screen, then it returns NIL.

(format nil "~A, ~A!" "Hello" "world")   ; => "Hello, world!"
(format t "~A, ~A!" "Hello" "world")     ; => NIL


;;;-----------------------------------------------------------------------------
;;; 2. Variables
;;;-----------------------------------------------------------------------------

;;; You can create a global (dynamically scoped) variable using DEFVAR and
;;; DEFPARAMETER. The variable name can use any character except: ()",'`;#|\

;;; The difference between DEFVAR and DEFPARAMETER is that re-evaluating a
;;; DEFVAR expression doesn't change the value of the variable. DEFPARAMETER,
;;; on the other hand, does.

;;; By convention, dynamically scoped variables have earmuffs in their name.

(defparameter *some-var* 5)
*some-var* ; => 5

;;; You can also use unicode characters.
(defparameter *AΛB* nil)

;;; Accessing a previously unbound variable results in an UNBOUND-VARIABLE
;;; error, however it is defined behavior. Don't do it.

;;; You can create local bindings with LET. In the following snippet, `me` is
;;; bound to "dance with you" only within the (let ...). LET always returns
;;; the value of the last `form` in the LET form.

(let ((me "dance with you")) me) ; => "dance with you"


;;;-----------------------------------------------------------------------------;
;;; 3. Structs and collections
;;;-----------------------------------------------------------------------------;


;;; Structs

(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))
*rover*            ; => #S(DOG :NAME "rover" :BREED "collie" :AGE 5)
(dog-p *rover*)    ; => T
(dog-name *rover*) ; => "rover"

;;; DOG-P, MAKE-DOG, and DOG-NAME are all automatically created by DEFSTRUCT


;;; Pairs

;;; CONS constructs pairs. CAR and CDR return the head and tail of a CONS-pair.

(cons 'SUBJECT 'VERB)         ; => '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB))   ; => SUBJECT
(cdr (cons 'SUBJECT 'VERB))   ; => VERB


;;; Lists

;;; Lists are linked-list data structures, made of CONS pairs and end with a
;;; NIL (or '()) to mark the end of the list

(cons 1 (cons 2 (cons 3 nil)))     ; => '(1 2 3)

;;; LIST is a convenience variadic constructor for lists

(list 1 2 3)                       ; => '(1 2 3)

;;; When the first argument to CONS is an atom and the second argument is a
;;; list, CONS returns a new CONS-pair with the first argument as the first
;;; item and the second argument as the rest of the CONS-pair

(cons 4 '(1 2 3))                  ; => '(4 1 2 3)

;;; Use APPEND to join lists

(append '(1 2) '(3 4))             ; => '(1 2 3 4)

;;; Or CONCATENATE

(concatenate 'list '(1 2) '(3 4))  ; => '(1 2 3 4)

;;; Lists are a very central type, so there is a wide variety of functionality for
;;; them, a few examples:

(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => NIL
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)


;;; Vectors

;;; Vector's literals are fixed-length arrays

#(1 2 3) ; => #(1 2 3)

;;; Use CONCATENATE to add vectors together

(concatenate 'vector #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)


;;; Arrays

;;; Both vectors and strings are special-cases of arrays.

;;; 2D arrays

(make-array (list 2 2))         ; => #2A((0 0) (0 0))
(make-array '(2 2))             ; => #2A((0 0) (0 0))
(make-array (list 2 2 2))       ; => #3A(((0 0) (0 0)) ((0 0) (0 0)))

;;; Caution: the default initial values of MAKE-ARRAY are implementation-defined.
;;; To explicitly specify them:

(make-array '(2) :initial-element 'unset)  ; => #(UNSET UNSET)

;;; To access the element at 1, 1, 1:

(aref (make-array (list 2 2 2)) 1 1 1)     ;  => 0
;;; This value is implementation-defined:
;;; NIL on ECL, 0 on SBCL and CCL.

;;; Adjustable vectors

;;; Adjustable vectors have the same printed representation as
;;; fixed-length vector's literals.

(defparameter *adjvec* (make-array '(3) :initial-contents '(1 2 3)
                                   :adjustable t :fill-pointer t))
*adjvec* ; => #(1 2 3)

;;; Adding new elements

(vector-push-extend 4 *adjvec*)   ; => 3
*adjvec*                          ; => #(1 2 3 4)


;;; Sets, naively, are just lists:

(set-difference '(1 2 3 4) '(4 5 6 7))   ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7))     ; => 4
(union '(1 2 3 4) '(4 5 6 7))            ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))                    ; => (1 2 3 4)

;;; However, you'll need a better data structure than linked lists when working
;;; with larger data sets

;;; Dictionaries are implemented as hash tables.

;;; Create a hash table

(defparameter *m* (make-hash-table))

;;; Set value

(setf (gethash 'a *m*) 1)

;;; Retrieve value

(gethash 'a *m*) ; => 1, T

;;; CL expressions have the ability to return multiple values.

(values 1 2) ; => 1, 2

;;; which can be bound with MULTIPLE-VALUE-BIND

(multiple-value-bind (x y)
    (values 1 2)
  (list y x))

; => '(2 1)

;;; GETHASH is an example of a function that returns multiple values. The first
;;; value it return is the value of the key in the hash table; if the key is
;;; not found it returns NIL.

;;; The second value determines if that key is indeed present in the hash
;;; table. If a key is not found in the table it returns NIL. This behavior
;;; allows us to check if the value of a key is actually NIL.

;;; Retrieving a non-present value returns nil

(gethash 'd *m*) ;=> NIL, NIL

;;; You can provide a default value for missing keys

(gethash 'd *m* :not-found) ; => :NOT-FOUND

;;; Let's handle the multiple return values here in code.

(multiple-value-bind (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)


;;;-----------------------------------------------------------------------------
;;; 3. Functions
;;;-----------------------------------------------------------------------------

;;; Use LAMBDA to create anonymous functions. Functions always returns the
;;; value of the last expression. The exact printable representation of a
;;; function varies between implementations.

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;;; Use FUNCALL to call anonymous functions

(funcall (lambda () "Hello World"))   ; => "Hello World"
(funcall #'+ 1 2 3)                   ; => 6

;;; A call to FUNCALL is also implied when the lambda expression is the CAR of
;;; an unquoted list

((lambda () "Hello World"))           ; => "Hello World"
((lambda (val) val) "Hello World")    ; => "Hello World"

;;; FUNCALL is used when the arguments are known beforehand. Otherwise, use APPLY

(apply #'+ '(1 2 3))   ; => 6
(apply (lambda () "Hello World") nil) ; => "Hello World"

;;; To name a function, use DEFUN

(defun hello-world () "Hello World")
(hello-world) ; => "Hello World"

;;; The () in the definition above is the list of arguments

(defun hello (name) (format nil "Hello, ~A" name))
(hello "Steve") ; => "Hello, Steve"

;;; Functions can have optional arguments; they default to NIL

(defun hello (name &optional from)
  (if from
      (format t "Hello, ~A, from ~A" name from)
      (format t "Hello, ~A" name)))

(hello "Jim" "Alpacas")       ; => Hello, Jim, from Alpacas

;;; The default values can also be specified

(defun hello (name &optional (from "The world"))
   (format nil "Hello, ~A, from ~A" name from))

(hello "Steve")               ; => Hello, Steve, from The world
(hello "Steve" "the alpacas") ; => Hello, Steve, from the alpacas

;;; Functions also have keyword arguments to allow non-positional arguments

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
  (format t "Hello, ~A ~A, from ~A" honorific name from))

(generalized-greeter "Jim")
; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer


;;;-----------------------------------------------------------------------------
;;; 4. Equality
;;;-----------------------------------------------------------------------------

;;; CL has a sophisticated equality system. Some are covered here.

;;; For numbers, use `='
(= 3 3.0)               ; => T
(= 2 1)                 ; => NIL

;;; For object identity (approximately) use EQL
(eql 3 3)               ; => T
(eql 3 3.0)             ; => NIL
(eql (list 3) (list 3)) ; => NIL

;;; for lists, strings, and bit-vectors use EQUAL
(equal (list 'a 'b) (list 'a 'b)) ; => T
(equal (list 'a 'b) (list 'b 'a)) ; => NIL


;;;-----------------------------------------------------------------------------
;;; 5. Control Flow
;;;-----------------------------------------------------------------------------

;;; Conditionals

(if t                ; test expression
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;;; In conditionals, all non-NIL values are treated as true

(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;;; COND chains a series of tests to select a result
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;;; TYPECASE switches on the type of the value
(typecase 1
  (string :string)
  (integer :int))
; => :int


;;; Looping

;;; Recursion

(defun fact (n)
  (if (< n 2)
      1
    (* n (fact(- n 1)))))

(fact 5) ; => 120

;;; Iteration

(defun fact (n)
  (loop :for result = 1 :then (* result i)
     :for i :from 2 :to n
     :finally (return result)))

(fact 5) ; => 120

(loop :for x :across "abcd" :collect x)
; => (#\a #\b #\c #\d)

(dolist (i '(1 2 3 4))
  (format t "~A" i))
; => 1234


;;;-----------------------------------------------------------------------------
;;; 6. Mutation
;;;-----------------------------------------------------------------------------

;;; Use SETF to assign a new value to an existing variable. This was
;;; demonstrated earlier in the hash table example.

(let ((variable 10))
    (setf variable 2))
; => 2

;;; Good Lisp style is to minimize the use of destructive functions and to avoid
;;; mutation when reasonable.


;;;-----------------------------------------------------------------------------
;;; 7. Classes and objects
;;;-----------------------------------------------------------------------------

;;; No more animal classes. Let's have Human-Powered Mechanical
;;; Conveyances.

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;;; The arguments to DEFCLASS, in order are:
;;; 1. class name
;;; 2. superclass list
;;; 3. slot list
;;; 4. optional specifiers

;;; When no superclass list is set, the empty list defaults to the
;;; standard-object class. This *can* be changed, but not until you
;;; know what you're doing. Look up the Art of the Metaobject Protocol
;;; for more information.

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

;;; Calling DESCRIBE on the HUMAN-POWERED-CONVEYANCE class in the REPL gives:

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;;; Note the reflective behavior available. CL was designed to be an
;;; interactive system

;;; To define a method, let's find out what our circumference of the
;;; bike wheel turns out to be using the equation: C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;;; PI is defined as a built-in in CL

;;; Let's suppose we find out that the efficiency value of the number
;;; of rowers in a canoe is roughly logarithmic. This should probably be set
;;; in the constructor/initializer.

;;; To initialize your instance after CL gets done constructing it:

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;;; Then to construct an instance and check the average efficiency...

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887


;;;-----------------------------------------------------------------------------
;;; 8. Macros
;;;-----------------------------------------------------------------------------

;;; Macros let you extend the syntax of the language. CL doesn't come
;;; with a WHILE loop, however, it's trivial to write one. If we obey our
;;; assembler instincts, we wind up with:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)) (done (gensym)))
        `(tagbody
           ,block-name
           (unless ,condition
               (go ,done))
           (progn
           ,@body)
           (go ,block-name)
           ,done)))

;;; Let's look at the high-level version of this:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
  `(loop while ,condition
         do
         (progn
            ,@body)))

;;; However, with a modern compiler, this is not required; the LOOP form
;;; compiles equally well and is easier to read.

;;; Note that ``` is used, as well as `,` and `@`. ``` is a quote-type operator
;;; known as quasiquote; it allows the use of `,` . `,` allows "unquoting"
;;; variables. @ interpolates lists.

;;; GENSYM creates a unique symbol guaranteed to not exist elsewhere in
;;; the system. This is because macros are expanded at compile time and
;;; variables declared in the macro can collide with variables used in
;;; regular code.

;;; See Practical Common Lisp and On Lisp for more information on macros.
```


## Further reading

- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/book.pdf)


## Extra information

- [CLiki](http://www.cliki.net/)
- [common-lisp.net](https://common-lisp.net/)
- [Awesome Common Lisp](https://github.com/CodyReichert/awesome-cl)
- [Lisp Lang](http://lisp-lang.org/)


## Credits

Lots of thanks to the Scheme people for rolling up a great starting
point which could be easily moved to Common Lisp.

- [Paul Khuong](https://github.com/pkhuong) for some great reviewing.
