---

language: "Lisp Flavoured Erlang(LFE)"
filename: lispflavourederlang.lfe
contributors:
  - ["Pratik Karki", "https://github.com/prertik"]
---

Lisp Flavoured Erlang(LFE) is a functional, concurrent, general-purpose programming 
language and Lisp dialect(Lisp-2) built on top of Core Erlang and the Erlang Virtual Machine(BEAM). 

LFE can be obtained from [LFE](https://github.com/rvirding/lfe)

The classic starting point is [LFE DOCS.](http://docs.lfe.io)

Another new site is being built to replace it.[LFE DEV.](http://docs.lfe.io/dev)



```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General form.

;; Lisp comprises of two syntax called: the ATOM and the S-expression.
;; `forms` are known as grouped S-expressions.

8  ; an atom; it evaluates to itself

:ERLANG ;Atom; evaluates to the symbol :ERLANG.

t  ; another atom which denotes true.

(* 2 21) ; an S- expression

'(8 :foo t)  ;another one


;;; Comments

;; Single line comments start with a semicolon; use two for normal
;; comments, three for section comments, and four fo file-level
;; comments.

;; Block Comment

   #| comment text |#

;;; Environment

;; LFE is the de-facto standard.

;; Libraries can be used directly from the Erlang ecosystem. Rebar3 is the build tool.

;; LFE is usually developed with a text editor(preferably Emacs) and a REPL
;; (Read Evaluate Print Loop) running at the same time. The REPL 
;; allows for interactive exploration of the program as it is "live"
;; in the system.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Literals and Special Syntactic Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Integers

1234 -123	        ; Regular decimal notation
#b0 #b10101	        ; Binary notation
#0 #10101	        ; Binary notation (alternative form)
#o377 #o-111	    ; Octal notation
#d123456789 #d+123	; Explicitly decimal notation
#xc0ffe 0x-01	    ; Hexadecimal notation
#2r1010 #8r377 	    ;Notation with explicit base (up to 36)
#\a #$ #\ä #\🐭     ;Character notation (the value is the Unicode code point of the character)
#\x1f42d;	        ;Character notation with the value in hexadecimal

;;; Floating point numbers
1.0 +2.0 -1.5 1.0e10 1.111e-10     

;;; Strings

"any text between double quotes where \" and other special characters like \n can be escaped".
; List String
"Cat: \x1f639;" ; writing unicode in string for regular font ending with semicolon.

#"This is a binary string \n with some \"escaped\" and quoted (\x1f639;) characters"
; Binary strings are just strings but function different in the VM. 
; Other ways of writing it are:  #B("a"), #"a", and #B(97).


;;; Character escaping

\b	; => Backspace
\t	; => Tab
\n	; => Newline
\v	; => Vertical tab
\f	; => Form Feed
\r	; => Carriage Return
\e	; => Escape
\s	; => Space
\d	; => Delete

;;; Binaries
;; It is used to create binaries with any contents.
#B((#"a" binary) (#"b" binary))	               ; #"ab" (Evaluated form)

;;; Lists are: () or (foo bar baz)

;;; Tuples are written in: #(value1 value2 ...). Empty tuple #() is also valid.

;;; Maps are written as: #M(key1 value1 key2 value2 ...). Empty map #M() is also valid.

;;; Symbols: Things that cannot be parsed. Eg: foo, Foo, foo-bar, :foo
| foo | ; explicit construction of symbol by wrapping vertical bars.

;;; Evaluation 

;; #.(... some expression ...). E.g. '#.(+ 1 1) will evaluate the (+ 1 1) while it            ;; reads the expression and then be effectively '2.

;; List comprehension in LFE REPL

lfe> (list-comp
          ((<- x '(0 1 2 3)))
          (trunc (math:pow 3 x)))
       (1 3 9 27)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Core forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These forms are same as those found at Common Lisp and Scheme.

(quote e)
(cons head tail)
(car e)
(cdr e)
(list e ... )
(tuple e ... )
(binary seg ... )
(map key val ...), (map-get m k), (map-set m k v ...), (map-update m k v ...)

(lambda (arg ...) ...)
  (match-lambda
    ((arg ... ) {{(when e ...)}} ...) ; Matches clauses
    ... )
(let ((pat {{(when e ...)}} e)
      ...)
  ... )
(let-function ((name lambda|match-lambda) ; Only define local
               ... )                      ; functions
  ... )
(letrec-function ((name lambda|match-lambda) ; Only define local
                  ... )                      ; functions
  ... )
(let-macro ((name lambda-match-lambda) ; Only define local
            ...)                       ; macros
  ...)
(progn ... )
(if test true-expr {{false-expr}})
(case e
  (pat {{(when e ...)}} ...)
   ... ))
(receive
  (pat {{(when e ...)}} ... )
  ...
  (after timeout ... ))
(catch ... )
(try
  e
  {{(case ((pat {{(when e ...)}} ... )
          ... ))}}
  {{(catch
     ; Next must be tuple of length 3!
     (((tuple type value ignore) {{(when e ...)}}
      ... )
     ... )}}
  {{(after ... )}})

(funcall func arg ... )
(call mod func arg ... ) - Call to Erlang Mod:Func(Arg, ... )
(define-module name declaration ... )
(extend-module declaration ... ) - Define/extend module and declarations.
(define-function name lambda|match-lambda)
(define-macro name lambda|match-lambda) - Define functions/macros at top-level.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macros are part of the language to allow you to create abstractions 
;; on top of the core language and standard library that move you closer 
;; toward being able to directly express the things you want to express.

;; Top-level function

(defun name (arg ...) ...)

;; Adding comments in functions

(defun name
  "Toplevel function with pattern-matching arguments"
  ((argpat ...) ...)
  ...)

;; Top-level macro

(defmacro name (arg ...) ...)
(defmacro name arg ...)

;; Top-level macro with pattern matching arguments

(defmacro name
  ((argpat ...) ...)
  ...)

;; Top-level macro using Scheme inspired syntax-rules format 

(defsyntax name
  (pat exp)
  ...)

;;; Local macros in macro or syntax-rule format

(macrolet ((name (arg ... ) ... )
            ... )
    ... )
    
(syntaxlet ((name (pat exp) ...)
             ...)
 ...)

;; Like CLISP

(prog1 ...)
(prog2 ...)

;; Erlang LFE module

(defmodule name ...)

;; Erlang LFE record

(defrecord name ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Patterns and Guards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using patterns in LFE compared to that of Erlang

;; Erlang                     ;; LFE
;; {ok, X}                       (tuple 'ok x)
;; error                         'error
;; {yes, [X|Xs]}                 (tuple 'yes (cons x xs))
;; <<34,F/float>>                (binary 34 (f float))
;; [P|Ps]=All                    (= (cons p ps) all)

  _    ; => is don't care while pattern matching
  
  (= pattern1 pattern2)     ; => easier, better version of pattern matching
  
;; Guards

;; Whenever pattern occurs(let, case, receive, lc, etc) it can be followed by an optional
;; guard which has the form (when test ...).

(progn gtest ...)             ;; => Sequence of guard tests
(if gexpr gexpr gexpr)
(type-test e)
(guard-bif ...)               ;; => Guard BIFs, arithmetic, boolean and comparison operators

;;; REPL

lfe>(set (tuple len status msg) #(8 ok "Trillian"))
    #(8 ok "Trillian")
lfe>msg
    "Trillian"

;;; Program illustrating use of Guards

(defun right-number?
        ((x) (when (orelse (== x 42) (== x 276709)))
          'true)
        ((_) 'false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple function using if.

(defun max (x y)
  "The max function."
  (if (>= x y) x y))

;; Same function using more clause

(defun max
  "The max function."
  ((x y) (when (>= x y)) x)
  ((x y) y))

;; Same function using similar style but using local functions defined by flet or fletrec

(defun foo (x y)
  "The max function."
  (flet ((m (a b) "Local comment."
            (if (>= a b) a b)))
    (m x y)))

;; LFE being Lisp-2 has separate namespaces for variables and functions
;; Both variables and function/macros are lexically scoped.
;; Variables are bound by lambda, match-lambda and let.
;; Functions are bound by top-level defun, flet and fletrec.
;; Macros are bound by top-level defmacro/defsyntax and by macrolet/syntaxlet.

;; (funcall func arg ...) like CL to call lambdas/match-lambdas 
;; (funs) bound to variables are used.

;; separate bindings and special for apply.
apply _F (...), 
apply _F/3 ( a1, a2, a3 )
    
;; Cons'ing in function heads
(defun sum (l) (sum l 0))
  (defun sum
    (('() total) total)
    (((cons h t) total) (sum t (+ h total))))
    
;; ``cons`` literal instead of constructor form
      (defun sum (l) (sum l 0))
      (defun sum
        (('() total) total)
        ((`(,h . ,t) total) (sum t (+ h total))))

;; Matching records in function heads

(defun handle_info
  (('ping (= (match-state remote-pid 'undefined) state))
    (gen_server:cast (self) 'ping)
    `#(noreply ,state))
  (('ping state)
   `#(noreply ,state)))

;; Receiving Messages
      (defun universal-server ()
        (receive
          ((tuple 'become func)
           (funcall func))))
           
;; another way for receiving messages

 (defun universal-server ()
        (receive
          (`#(become ,func)
            (funcall func))))

;; Composing a complete function for specific tasks

(defun compose (f g)
  (lambda (x)
   (funcall f
     (funcall g x))))

(defun check ()
  (let* ((sin-asin (compose #'sin/1 #'asin/1))
         (expected (sin (asin 0.5)))
         (compose-result (funcall sin-asin 0.5)))
    (io:format "Expected answer: ~p~n" (list expected))
    (io:format "Answer with compose: ~p~n" (list compose-result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Concurrency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Message passing as done by Erlang's light-weight "processes".

(defmodule messenger-back
 (export (print-result 0) (send-message 2)))

(defun print-result ()
  (receive
    ((tuple pid msg)
      (io:format "Received message: '~s'~n" (list msg))
      (io:format "Sending message to process ~p ...~n" (list pid))
      (! pid (tuple msg))
      (print-result))))

(defun send-message (calling-pid msg)
  (let ((spawned-pid (spawn 'messenger-back 'print-result ())))
    (! spawned-pid (tuple calling-pid msg))))
    
;; Multiple simultaneous HTTP Requests:

(defun parse-args (flag)
  "Given one or more command-line arguments, extract the passed values.

  For example, if the following was passed via the command line:

    $ erl -my-flag my-value-1 -my-flag my-value-2

  One could then extract it in an LFE program by calling this function:

    (let ((args (parse-args 'my-flag)))
      ...
      )
  In this example, the value assigned to the arg variable would be a list
  containing the values my-value-1 and my-value-2."
  (let ((`#(ok ,data) (init:get_argument flag)))
    (lists:merge data)))

(defun get-pages ()
  "With no argument, assume 'url parameter was passed via command line."
  (let ((urls (parse-args 'url)))
    (get-pages urls)))

(defun get-pages (urls)
  "Start inets and make (potentially many) HTTP requests."
  (inets:start)
  (plists:map
    (lambda (x)
      (get-page x)) urls))

(defun get-page (url)
  "Make a single HTTP request."
  (let* ((method 'get)
         (headers '())
         (request-data `#(,url ,headers))
         (http-options ())
         (request-options '(#(sync false))))
    (httpc:request method request-data http-options request-options)
    (receive
      (`#(http #(,request-id #(error ,reason)))
       (io:format "Error: ~p~n" `(,reason)))
      (`#(http #(,request-id ,result))
       (io:format "Result: ~p~n" `(,result))))))


;; Check out Erlang's documentation for more concurrency and OTP docs.
```

## Further Reading

*    [LFE DOCS](http://docs.lfe.io)
*    [LFE GitBook](https://lfe.gitbooks.io/reference-guide/index.html)
*    [LFE Wiki](https://en.wikipedia.org/wiki/LFE_(programming_language))

## Extra Info
*    [LFE PDF](http://www.erlang-factory.com/upload/presentations/61/Robertvirding-LispFlavouredErlang.pdf)
*    [LFE mail](https://groups.google.com/d/msg/lisp-flavoured-erlang/XA5HeLbQQDk/TUHabZCHXB0J)

## Credits

Lots of thanks to Robert Virding for creating LFE, Duncan McGreggor for documenting it and other LFE contributors who made LFE awesome.

