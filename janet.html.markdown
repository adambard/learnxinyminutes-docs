---
language: Janet
filename: learnJanet.janet
contributors:
    - ["John Gabriele", "http://www.unexpected-vortices.com/"]
---

[Janet](https://janet-lang.org/) is a Lisp-like (Clojure-like),
lexically-scoped, dynamically-typed, garbage-collected, C-based, high-level
language. The entire language (core library, interpreter, compiler, assembler,
PEG) is about 300-500 kB and should run on many constrained systems.

I encourage you to try out the code snippets below in the Janet
repl (either by [installing Janet](https://janet-lang.org/docs/index.html),
or else by using the repl embedded in the Janet homepage).

As we only have a scant *y* minutes, we'll survey the basics here and
leave the remaining details for the manual. So please, keep your arms and
legs inside the vehicle at all times, and on with the scenic tour!

```python
# A comment.

# Some literal values.
true
false
nil

# Typical style for symbols (identifiers-for / names-of things).
do-stuff
pants-on-fire!
foo->bar        # Evidently for converting foos to bars.
fully-charged?
_               # Usually used as a dummy variable.

# Keywords are like symbols that start with a colon, are treated like
# constants, and are typically used as map keys or pieces of syntax in
# macros.
:a
:some-val

# Numbers #####################################################################
5
1e3    # => 1000
1_000  # => 1000
2e-03  # => 0.002
0xff   # => 255

# You can specify a radix (base) like so:
16rff   # => 255 (same as 0xff)
2r1101  # =>  13

# Some numbers in the math library:
math/pi  # => 3.14159
math/e   # => 2.71828

# Strings #####################################################################
"hello"
"hey\tthere"  # contains a tab

# For multi-line strings, use one or more backticks. Backslash-escapes not
# recognized in these (bytes will be parsed literally).
``a long
multi-line
string``    # => "a long\nmulti-line\nstring"

# Strings and data structures in Janet come in two varieties: mutable and
# immutable. The literal for the mutable variety is written with a `@` in
# front of it.

# A mutable string (aka "buffer").
@"this"
@`a multi-line
one here`

(string "con" "cat" "enate")   # => "concatenate"

# To get a substring:
(string/slice "abcdefgh" 2 5)  # => "cde"
# To find a substring:
(string/find "de" "abcdefgh")  # => 3

# See the string library for more (splitting, replacement, etc.)

# Data Structures #############################################################
# Arrays and Tuples
# Arrays are mutable, tuples are immutable.

# Arrays (mutable)
@(4 5 6)
@[4 5 6]

# Tuples (immutable)
# Note that an open paren usually indicates a function call, so if you want a
# literal tuple with parens, you need to "quote" it (with a starting single
# quote mark)...
'(4 5 6)
[4 5 6]  # ... or just use square brackets.

# Tables and Structs (associative data structures)
@{:a 1 :b 2 :c 3}  # table  (mutable)
{:a 1 :b 2 :c 3}   # struct (immutable)

# To "pretty-print" these out, use `pp` instead of `print`.
# More about how to work with arrays/tuples and tables/structs below.

# Bindings ####################################################################
# Bind a value to a symbol.
(def x 4.7)  # Define a constant, `x`.
x            # => 4.7
(quote x)    # => x (the symbol x)
'x           # => x (the symbol x (shorthand))
(print x)    # prints 4.7

# Since we used `def`, can't change to what `x` refers:
(set x 5.6)  # Error, `x` is a constant.

(var y 10)
(set y 12)  # Works, since `y` was defined using `var`.

# Note that bindings are local to the scope they're called in. `let`
# creates a local scope and makes some bindings all in one shot:
(let [a 2
      b 3]
  (print "Hello from inside this local scope.")
  (* a b))  # => 6

# Destructuring is supported, both for arrays/tuples ...
(def a ["foos" "bars" "moos"])
(let [[s1 _ s2] a]
  (print s1 s2))  # foosmoos

# ... and for tables/structs.
(def t {:a "ayy" :b "bee" :c "sea"})
(let [{:a a :b b} t]
  (print a b))  # ayybee

# You can even destructure right in a `def`:
(def [aa1 aa2] a)
aa1  # => foos
aa2  # => bars

(def {:c body-of-water :b insect-friend} t)
body-of-water  # => sea
insect-friend  # => bee

# Note that keywords evaluate to themselves, whereas symbols evaluate
# to whatever value they're bound to (unless you quote them).

# Operators ###################################################################
# Janet supports the usual ensemble of operators.
# +, -, *, /, and so on. Note:
(/ 5 3)  # =>  1.66667
(% 5 3)  # =>  2 (remainder)
(- 5)    # => -5 (or you can just write `-5`)

(++ i)    # increments (modifies `i`)
(-- i)    # decrements
(+= i 3)  # add 3 to `i`
(*= i 3)  # triple `i`
# ... and so on for the other operations on numbers.

# If you don't want to mutate `i`, use `(inc i)` and `(dec i)`.

# Comparison
# =  <  >  not=  <=  >=
(< 2 7 12)  # => true

# Functions ###################################################################
# Call them:
(- 5 3)                   # => 2 (Operators and functions work the same way.)
(math/sin (/ math/pi 2))  # => 1
(range 5)                 # => @[0 1 2 3 4]

# Create them:
(defn mult-by-2
  ``First line of docstring.

  Some more of the docstring.``
  [x]
  (print "Hi.")
  (print "Will compute using: " x)
  (* 2 x))

(print (mult-by-2 6))  # => 12 (after printing "Hi" and so forth)

# If you have a function named "main" in your file, `janet` will automatically
# call it for you when you run the file.

# Interactively read a function's docs from within the repl:
(doc mult-by-2)

# Note, functions have to be defined before they can be used in a function,
# so if you design top-down, you'll need to write your functions from the
# bottom of the file up.

# You can make anonymous functions as well:
(fn [x] (+ x x))
(fn my-func [x] (+ x x))  # This one's less anonymous.

# Use `do` to make some side-effecting calls and then evaluate to
# the last form in the `do`:
(def n (do
         (print "hi")
         (do-some-side-effecting 42)
         3))
n  # => 3

# You might say that function bodies provide an "implicit do".

# Operations on data structures ###############################################
# (Making all of these mutable so we can ... mutate them.)
(def s @"Hello, World!")
(def a @[:a :b :c :d :e])
(def t @{:a 1 :b 2})

(length s)  # => 13
(length a)  # =>  5
(length t)  # =>  2

# Getting values:
(s 7)       # => 87 (which is the code point for "W")
(a 1)       # => :b
(t :a)      # => 1
(keys t)    # => @[:a :b]
(values t)  # => @[1 2]

# Changing values (for mutable data structures):
(put s 2 87)   # @"HeWlo, World!"
(put a 2 :x)   # @[:a :b :x :d :e]
(put t :b 42)  # @{:a 1 :b 42}

# Adding and removing values (again, for mutable data structures):
(buffer/push-string s "??")  # @"HeWlo, World!??"
(array/push a :f)  # @[:a :b :x :d :e :f]
(array/pop a)      # => :f, and it's also removed from `a`.
(put t :x 88)      # @{:a 1 :b 42 :x 88}

# See the manual for a wide variety of functions for working with
# buffers/strings, arrays/tuples, and tables/structs.

# Flow control ################################################################
(if some-condition
  42
  38)

# Only `nil` and `false` are falsey. Everything else is truthy.

(if got-it?
  71)  # No false-branch value. Returns `nil` if `got-it?` is falsey.

(var i 10)
(while (pos? i)
  (print "... " i)
  (-- i))
# Now `i` is 0.

# `case` compares the dispatch value to each of the options.
(var x 2)
(case x
  1 "won"
  2 "too"
  3 "tree"
  "unknown")  # => "too"

# `cond` evaluates conditions until it gets a `true`.
(set x 8)
(cond
  (= x 1) "won"
  (= x 2) "too"
  (< x 10) "tree"
  "oof!")  # => "tree"

(when (avoided-wipeout?)
  (do-side-effecty-thing 88)
  (smell-the-roses)
  (paint-fencepost-error))

# Pattern matching.
# `match` is like a high-powered switch expression. If you switch on a data
# structure, it can look inside to try and match on its contents. For example,
# matching on a table or struct:
(def t {:a 1 :b 2 :c 3})
(match t
  {:yar v} (print "matches key :yar! " v)
  {:moo v} (print "matches key :moo! " v)
  {:c   v} (print "matches key :c! "   v)
  _        (print "no match"))             # => prints "matches key :c! 3"

# Iterating ###################################################################
# Iterate over an integer range:
(for i 0 5
  (print i))  # prints 0, 1, 2, 3, 4

# There's also the more general `loop`:
(loop [i :range [0 10] :when (even? i)]
  (print i))

# Loop over an array/tuple:
(def words ["foo" "bar" "baz"])
(each word words
  (print word))

# Loop over a table/struct:
(def t {:a 1 :b 2})
(eachp [k v] t  # Loop over each pair in `t`.
  (print k " --> " v))

# Can also use `eachk` to loop over keys in a table or struct.

# Functional programming ######################################################
# You'll find many familiar old friends here.
(filter even?
        (map (fn [x]
               (* x x))
             (range 10)))  # => @[0 4 16 36 64]

(reduce + 0 (range 5))     # => 10

# ...and lots more (see the API docs).

# Errata ######################################################################
(type a)                # => the type of `a` (as a keyword)
(describe a)            # => a human-readable description of `a`
(string/format "%j" a)  # => Janet values, nicely-formatted
```

This tour didn't cover a number of other features such as modules, fibers,
PEGs, macros, etc., but should give you a taste of what Janet is like. See
the [Janet manual](https://janet-lang.org/docs/index.html) and the [Janet API
docs](https://janet-lang.org/api/index.html) for more info.
