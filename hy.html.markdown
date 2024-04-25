---
language: hy
filename: learnhy.hy
contributors:
    - ["Abhishek L", "http://twitter.com/abhishekl"]
    - ["Zirak", "http://zirak.me"]
---

Hy is a Lisp dialect built on top of Python. This is achieved by
converting Hy code to Python's abstract syntax tree (AST). This allows
Hy to call native Python code or Python to call native Hy code as well

```hylang
; Semicolon comments, like other Lisps

;; S-expression basics
; Lisp programs are made of symbolic expressions or sexps which
; resemble
(some-function args)
; now the quintessential hello world
(print "hello world")

;; Simple data types
; All simple data types are the same as their Python counterparts
42 ; => 42
3.14 ; => 3.14
True ; => True
4+10j ; => (4+10j) a complex number

; lets start with some simple arithmetic
(+ 4 1) ;=> 5
; the operator is applied to all arguments, like other Lisps
(+ 4 1 2 3) ;=> 10
(- 2 1) ;=> 1
(* 4 2) ;=> 8
(/ 4 1) ;=> 4
(% 4 2) ;=> 0 the modulo operator
; power is represented by the ** operator, like Python
(** 3 2) ;=> 9
; nesting forms will do the expected thing
(+ 2 (* 4 2)) ;=> 10
; also logical operators and or not and equal to etc. work as expected
(= 5 4) ;=> False
(not (= 5 4)) ;=> True

;; Variables
; variables are set using setv, variable names can use utf-8 except
; for ()[]{}",'`;#|
(setv a 42)
(setv π 3.14159)
(def *foo* 42)
;; Other container data types
; strings, lists, tuples & dicts
; these are exactly same as Python's container types
"hello world" ;=> "hello world"
; string operations work similar to Python
(+ "hello " "world") ;=> "hello world"
; lists are created using [], indexing starts at 0
(setv mylist [1 2 3 4])
; tuples are immutable data structures
(setv mytuple (, 1 2))
; dictionaries are key value pairs
(setv dict1 {"key1" 42 "key2" 21})
; :name can be used to define keywords in Hy which can be used for keys
(setv dict2 {:key1 41 :key2 20})
; use `get' to get the element at an index/key
(get mylist 1) ;=> 2
(get dict1 "key1") ;=> 42
; Alternatively if keywords were used they can be called directly
(:key1 dict2) ;=> 41

;; Functions and other program constructs
; functions are defined using defn, the last sexp is returned by default
(defn greet [name]
  "A simple greeting" ; an optional docstring
  (print "hello " name))

(greet "bilbo") ;=> "hello bilbo"

; functions can take optional arguments as well as keyword arguments
(defn foolists [arg1 &optional [arg2 2]]
  [arg1 arg2])

(foolists 3) ;=> [3 2]
(foolists 10 3) ;=> [10 3]

; you can use rest arguments and kwargs too:
(defn something-fancy [wow &rest descriptions &kwargs props]
  (print "Look at" wow)
  (print "It's" descriptions)
  (print "And it also has:" props))

(something-fancy "My horse" "amazing" :mane "spectacular")

; you use apply instead of the splat operators:
(apply something-fancy ["My horse" "amazing"] { "mane" "spectacular" })

; anonymous functions are created using `fn' or `lambda' constructs
; which are similar to `defn'
(map (fn [x] (* x x)) [1 2 3 4]) ;=> [1 4 9 16]

;; Sequence operations
; Hy has some builtin utils for sequence operations etc.
; retrieve the first element using `first' or `car'
(setv mylist [1 2 3 4])
(setv mydict {"a" 1 "b" 2})
(first mylist) ;=> 1

; slice lists using cut
(cut mylist 1 3) ;=> [2 3]

; get elements from a list or dict using `get'
(get mylist 1) ;=> 2
(get mydict "b") ;=> 2
; list indexing starts from 0, same as Python
; assoc can set elements at keys/indexes
(assoc mylist 2 10) ; makes mylist [1 2 10 4]
(assoc mydict "c" 3) ; makes mydict {"a" 1 "b" 2 "c" 3}
; there are a whole lot of other core functions which makes working with
; sequences fun

;; Python interop
;; import works just like in Python
(import datetime)
(import functools [partial reduce]) ; imports partial and reduce from functools
(import matplotlib.pyplot :as plt) ; imports foo as bar
; all builtin Python methods etc. are accessible from Hy
; a.foo(arg) is called as (.foo a arg)
(.split (.strip "hello world  ")) ;=> ["hello" "world"]

; there is a shortcut for executing multiple functions on a value called the
; "threading macro", denoted by an arrow:
(-> "hello world  " (.strip) (.split)) ;=> ["hello" "world]
; the arrow passes the value along the calls as the first argument, for instance:
(-> 4 (* 3) (+ 2))
; is the same as:
(+ (* 4 3) 2)

; there is also a "threading tail macro", which instead passes the value as the
; second argument. compare:
(-> 4 (- 2) (+ 1)) ;=> 3
(+ (- 4 2) 1) ;=> 3
; to:
(->> 4 (- 2) (+ 1)) ;=> -1
(+ 1 (- 2 4)) ;=> -1

;; Conditionals
; (if condition (body-if-true) (body-if-false)
(if (= passcode "moria")
  (print "welcome")
  (print "Speak friend, and Enter!"))

; nest multiple if else if clauses with cond
(cond
  (= someval 42) (print "Life, universe and everything else!")
  (> someval 42) (print "val too large")
  (< someval 42) (print "val too small"))

; group statements with do, these are executed sequentially
; forms like defn have an implicit do
(do
  (setv someval 10)
  (print "someval is set to " someval)) ;=> 10

; create lexical bindings with `let', all variables defined thusly
; have local scope
(let [nemesis {"superman" "lex luther"
                "sherlock" "moriarty"
                "seinfeld" "newman"}]
  (for [[h v] (.items nemesis)]
    (print (.format "{0}'s nemesis was {1}" h v))))

;; Classes
; classes are defined in the following way
(defclass Wizard [object]
  (defn __init__ [self spell]
    (setv self.spell spell))

  (defn get-spell [self]
    self.spell))
```

### Further Reading

This tutorial is just a basic introduction to Hy/Lisp/Python.

Hy docs are here: [https://hylang.org/hy/doc](https://hylang.org/hy/doc)

Hy's GitHub repo: [https://github.com/hylang/hy](https://github.com/hylang/hy)

On freenode IRC `#hy`, twitter hashtag #hylang
