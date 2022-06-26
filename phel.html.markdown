---
language: phel
filename: learnphel.phel
contributors:
    - ["Chemaclass", "https://github.com/Chemaclass"]
---

[Phel](https://phel-lang.org/) is a functional programming language that compiles to PHP.
It is a dialect of Lisp inspired by Clojure and Janet.

## Features
- Built on PHP's ecosystem
- Good error reporting
- Persistent Datastructures (Lists, Vectors, Maps and Sets)
- Macros
- Recursive functions
- Powerful but simple Syntax
- REPL

```newlisp
# Comments begin with a # character and continue until the end of the line. There are no multi-line comments.

# Phel is written in "forms", which are just
# lists of things inside parentheses, separated by whitespace.

# The first call in a file should be ns, to set the namespace
(ns learn-phel)

# More basic examples:

# str will create a string out of all its arguments
(str "Hello" " " "World") #=> "Hello World"

# Math is straightforward
(+ 1 1) #=> 2
(- 2 1) #=> 1
(* 1 2) #=> 2
(/ 2 1) #=> 2

# Equality is =
(= 1 1) #=> true
(= 2 1) #=> false

# You need not for logic, too
(not true) #=> false

# Nesting forms works as you expect
(+ 1 (- 3 2)) # = 1 + (3 - 2) => 2

# Phel inherits PHP under the hood, so it can use native PHP (functions and classes) without 
# any additional cost by using the `php/` prefix to all PHP native functions.

# Types
#############

# Booleans are similar as the native PHP ones

nil
true
false 

# Symbols are used to name functions and variables in Phel
# For example: symbol, snake_case_symbol, my-module/my-function

# Keywords are like symbols that begin with a colon character. However, they are used as constants rather than a name for something.

:keyword
:0x0x0x
::

# Numbers in Phel are equivalent to numbers in PHP

1337 # integer
+1337 # positive integer
-1337 # negative integer

1.234 # float
+1.234 # positive float
-1.234 # negative float
1.2e3 # float
7E-10 # float

# Strings are surrounded by double quotes. They almost work the same as PHP double quoted strings.
# A string can be written in multiple lines. The line break character is then ignored by the reader.

"hello world"

"this is\na\nstring"

"this
is
a
string."

"use backslack to escape \" string"

"the dollar must not be escaped: $ or $abc just works"


# Collections & Sequences
#############

# Lists are linked-list data structures, while vectors are array-backed
(type '(1 2 3)) #=> :list
(type [1 2 3])  #=> :vector

# A list would be written as just (1 2 3), but we have to quote
# it to stop the reader thinking it's a function.
# Also, (list 1 2 3) is the same as '(1 2 3)

# You can produce a (non-lazy) sequence between a range. 
(range 1 10 2) #=> (range from to step)
(take 4 (range 10))

# Use cons to add an item to the beginning of a list
(cons 4 '(1 2 3)) #=> (4 1 2 3)

# Use push to add, and put to replace an item in a vector 
(push [1 2 3] 4)  #=> (1 2 3 4)
(put [1 2 3] 1 4) #=> (1 4 3)

# Use concat to add lists or vectors together
(concat [1 2] '(3 4)) #=> [1 2 3 4]

# Use filter, map to interact with collections
(map inc [1 2 3])      #=> [2 3 4]
(filter even? [1 2 3]) #=> [2]

# Use reduce to reduce them. The initial-value is mandatory
(reduce + 0 [1 2 3 4])
#=> (+ (+ (+ 1 2) 3) 4)
#=> 10

(reduce push [] '(3 2 1))
#=> (push (push (push [] 3) 2) 1)
#=> [3 2 1]

# Functions
#############

# Use fn to create new functions
# A function always returns its last statement
(fn [] "Hello World") #=> <function>

# You need extra parens to call it
((fn [] "Hello World")) #=> "Hello World"

# You can bind a value to a symbol using def for definition
(def x 1)
x #=> 1

# Variables provide a way to manage mutable state
(def foo (var 10)) # Define a variable with value 10

# Assign a function to a definition
(def hello-world (fn [] "Hello World"))
(hello-world) #=> "Hello World"

# You can shorten this process by using defn
(defn hello-world [] "Hello World")

# The [] is the list of arguments for the function
(defn hello [name]
  (str "Hello " name))
(hello "Jens") #=> "Hello Jens"

# You can also use this shorthand to create functions
(def hello2 |(str "Hello " $1))
(hello2 "Anna") #=> "Hello Anna"

# Functions can pack extra arguments up in a seq for you
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) #=> "You passed 3 args: @[1 2 3]"

# You can mix regular and packed arguments
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Jesus" 1 2) #=> "Hello Jesus, you passed 2 extra args"


# Maps
#############

# Hash maps have faster lookups but don't retain key order
(type {:a 1 :b 2 :c 3})          #=> :hash-map
(type (hash-map :a 1 :b 2 :c 3)) #=> :hash-map

# Maps can use any hashable type as a key, but usually keywords are best
# Keywords are like strings with some efficiency bonuses and they start with `:`
(type :a) #=> :keyword

(def stringmap {"a" 1 "b" 2 "c" 3})
stringmap  #=> {"a" 1 "b" 2 "c" 3}

(def keymap {:a 1 :b 2 :c 3})
keymap  #=> {:a 1 :c 3 :b 2}

# Retrieve a value from a map by calling it as a function
(stringmap "a") #=> 1
(keymap :a)     #=> 1

# Keywords can be used to retrieve their value from a map, too!
(:b keymap) #=> 2

# Don't try this with strings
# ("a" stringmap)
# ...Exception: Call to undefined function a()

# Retrieving a non-present key returns nil
(stringmap "d") #=> nil

# Use put to add new keys to hash-maps
(def newkeymap (put keymap :d 4))
newkeymap #=> {:a 1 :b 2 :c 3 :d 4}

# But remember, phel types are immutable!
keymap #=> {:a 1 :b 2 :c 3}

# Use unset to remove keys
(unset keymap :a) #=> {:b 2 :c 3}

# Sets
#############

# A Set contains unique values in random order

(type (set 1 2 3)) #=> :set
(set 1 2 3 1 2 3 3 2 1 3 2 1) #=> (set 1 2 3)

# Add a member with push
(push (set 1 2 3) 4) #=> (set 1 2 3 4)

# Remove one with unset
(unset (set 1 2 3) 1) #=> (set 2 3)

# Test for existence by using the set as a function
((set 1 2 3) 1) #=> 1
((set 1 2 3) 4) #=> nil

# There are more functions like: count, union, intersection, difference, etc


# Useful forms
#############

# `If` conditionals in phel are special forms
(if false "a" "b") #=> "b"
(if false "a") #=> nil

# Use let to create temporary bindings
(let [a 1 b 2]
  (> a b)) #=> false

# Group statements together with do
(do
  (print "Hello")
  "World") #=> "World" (prints "Hello")

# Functions have an implicit do
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") #=> "Hello Jeff" (prints "Saying hello to Jeff")

# So does let
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) #=> "Hello Urkel" (prints "Saying hello to Urkel")

# Use the threading macros (-> and ->>) to express transformations of
# data more clearly.

# The "Thread-first" macro (->) inserts into each form the result of
# the previous, as the first argument (second item)
(->
   {:a 1 :b 2}
   (put :c 3)  #=> (put {:a 1 :b 2} :c 3)
   (unset :b)) #=> (unset (put {:a 1 :b 2} :c 3) :b)


# The double arrow does the same thing, but inserts the result of
# each line at the *end* of the form. This is useful for collection
# operations in particular:
(->>
   (range 10)
   (map inc)      #=> (map inc (range 10))
   (filter odd?)) #=> (filter odd? (map inc (range 10)))
                  # Result: [1 3 5 7 9]


# When you are in a situation where you want more freedom as where to
# put the result of previous data transformations in an
# expression, you can use the as-> macro. With it, you can assign a
# specific name to transformations' output and use it as a
# placeholder in your chained expressions:

(as-> [1 2 3] input
  (map inc input)     #=> You can use last transform's output at the last position
  (get input 2)       #=> and at the second position, in the same expression
  (push [4 5 6] input 8 9 10)) #=> or in the middle !
                               # Result: [4 5 6 4 8 9 10]

# PHP
#################

# PHP has a huge and useful standard library, and you're able to use
# all native functions with the prefix `php/`.
(php/+ 1 2 3)

# With :use you can use different namespaces. Similar as `use` in PHP
(ns my\module
  (:use \DateTimeImmutable))

# You can import functions from other phel files with :require
(ns my\module
  (:require phel\test :refer [deftest is]))

# Use the class name with a "php/new" to make a new instance
(php/new \DateTime) # <a date-time object>

# Use php/-> to call methods of an object
(def d (php/new \DateTime))
(php/-> d (getTimestamp)) # <a timestamp>

# you can do it in one line too
(php/-> (php/new \DateTime) (getTimestamp))

# Use php/:: to call static methods
(php/:: \DateTimeImmutable ATOM) # <a timestamp>
```

### Further Reading

This is far from exhaustive, but hopefully it's enough to get you on your feet.

Read the full documentation in the website: [https://phel-lang.org/](https://phel-lang.org/documentation/getting-started/)
