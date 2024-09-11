---
language: fennel
filename: learnfennel.fnl
contributors:
    - ["Jesse Wattenbarger", "https://github.com/jjwatt"]
---

Fennel is a programming language that brings together the simplicity,
speed, and reach of Lua with the flexibility of a lisp syntax and
macro system.


```fennel
;; Comments start with semicolons.

;; Fennel is written in lists of things inside parentheses, separated
;; by whitespace.

;; The first thing in parentheses is a function or macro to call, and
;; the rest are the arguments.
                        
;; ------------------------- ;;
;; 1. Primitives & Operators ;;
;; ------------------------- ;;

;; (local ...) defines a var inside the whole file's scope.
(local s "walternate") ; Immutable strings like Python.

;; local supports destructuring and multiple value binding.
;; (covered later).

;; Strings are utf8 byte arrays
"λx:(μα.α→α).xx" ; can include Unicode characters

;; .. will create a string out of it's arguments.
;; It will coerce numbers but nothing else.
(.. "Hello" " " "World") ; => "Hello World"

;; (print ...) will print all arguments with tabs in between
(print "Hello" "World") ; "Hello World" printed to screen

(local num 42) ;; Numbers can be integer or floating point.

;; Equality is =
(= 1 1) ; => true
(= 2 1) ; => false

;; Nesting forms works as you expect
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

;; Comparisons
(> 1 2) ; => false
(< 1 2) ; => true
(>= 1 1) ; => true
(<= 1 2) ; => true
(not= 1 2) ; => true

;; TODO: find some bitwise operator examples
;; (lshift 1) ; => 2

;; -------- ;;
;; 2. Types ;;
;; -------- ;;

;; Fennel uses Lua's types for booleans, strings & numbers.
;; Use `type` to inspect them.
(type 1) ; => "number"
(type 1.0) ; => "number"
(type "")  ; => "string"
(type false) ; => "boolean"
(type nil) ; => "nil"

;; Booleans
true ; for true
false ; for false
(not true) ; => false
(and 0 false) ; => false
(or false 0) ; => 0

;; All values other than nil or false are treated as true.

;;,--------
;;| Binding
;;`--------

;; Use `let` to bind local vars to values.
;; Local binding: `me` is bound to "Bob" only within the (let ...)
(let [me "Bob"]
  (print "returning Bob")
  me) ; => "Bob"
;; Outside the body of the let, the bindings it introduces are no
;; longer visible. The last form in the body is used as the return
;; value. `set` does not work on `let` bound locals.

;; `local` introduces a new local inside an existing scope. Similar to
;; let but without a body argument. Recommended for use at the
;; top-level of a file for locals which will be used throughout the
;; file. `set` does not work on `locals`
(local tau-approx 6.28318)

;; `var` introduces a new local inside an existing scope which may have its
;; value changed. Identical to local apart from allowing set to work
;; on it. `set` works on `vars`
(var x 83)

;; `set` local variable or table field
;; Changes the value of a variable introduced with `var`. Will not work
;; on `globals` or `let`/`local`-bound locals.
(set x (+ x 91)) ; var

;; Can also be used to change a field of a table, even if the table is
;; bound with `let` or `local`. If the table field name is static, use
;; `tbl.field`; if the field name is dynamic, use `(. tbl field)`.
(let [t {:a 4 :b 8}] ; static table field
  (set t.a 2) t) ; => {:a 2 :b 8}

;; In any context where you can make a new binding, you can use
;; multiple value binding. Otherwise, you will only capture the first
;; value.
(let [x (values 1 2 3)]
  x) ; => 1

;; `global` set global variable
;; Sets a global variable to a new value. Note that there is no
;; distinction between introducing a new global and changing the value
;; of an existing one. This supports destructuring and multiple-value
;; binding.
(global tbl {:a 4 :b 8})
(global prettyprint (fn [x] (print (fennel.view x))))

;;,--------------------------
;;| Prettyprint and .fennelrc
;;`--------------------------
;; `prettyprint` is a good function to have in your repl

;; Return values in the repl will get pretty-printed, but calling
;; (print tbl) will emit output like table: 0x55a3a8749ef0. If you
;; don't already have one, it's recommended for debugging to define a
;; printer function which calls `fennel.view` on its argument before
;; printing it.
(local fennel (require :fennel))
(fn _G.pp [x] (print
               (fennel.view x)))
;; Notice that adding it to `_G` puts the function in the global
;; table, similar to what `global` would do.  The fennel documentation
;; says that using `_G` is the preferred method of defining and using
;; globals.
;; If you add this definition to your ~/.fennelrc
;; file it will be available in the standard repl.
(pp tbl)

;;,--------------------------------
;;| Collections & Sequences: Tables
;;`--------------------------------

;; Tables are the only compound data structure in Lua and fennel.
;; Similar to php arrays or js objects, they are
;; hash-lookup dicts that can also be used as lists.

;; tables can be treated as sequential or non-sequential: as hashmaps
;; or lists/arrays.

;; Using tables as dictionaries / maps:
(local t {:key1 "value1" :key2 false})

;; String keys can use dot notation:
(print t.key1)         ;; Prints "value1"

;; Setting table keys and values
(tset t :newKey {})    ;; Adds a new key/value pair.
(tset t :key2 nil)     ;; Removes key2 from the table.
;; Literal notation for any (non-nil) value as key

;; length string or table length
(+ (length [1 2 3 nil 8]) (length "abc")) ; => 8

;; . table lookup looks up a given key in a table. Multiple arguments
;; will perform nested lookup.
(. t :key1)

(let [t {:a [2 3 4]}] (. t :a 2)) ; => 3

;; If the field name is a string known at compile time, you don't need
;; this and can just use table.field (dot notation).

;; Nil-safe ?. table lookup
;; Looks up a given key in a table. Multiple arguments will perform
;; nested lookup. If any subsequent keys is not presnet, will
;; short-circuit to nil.
(?. t :key1) ; => "value"
(let [t {:a [2 3 4]}] (?. t :a 4 :b)) ; => nil
(let [t {:a [2 3 4 {:b 42}]}] (?. t :a 4 :b)) ; => 42

;; The table module
(let [t [1 2 3]]
  (table.insert t 2 "a") ; t is now [1 "a" 2 3]
  (table.insert t "last") ; now [1 "a" 2 3 "last"]
  (print (table.remove t)) ; prints "last"
  (table.remove t 1) ; t is now ["a" 2 3]
  (print (table.concat t ", "))) ; prints "a, 2, 3"

;; The table.sort function sorts a table in-place, as a
;; side-effect. It takes an optional comparator function which should
;; return true when its first argument is less than the second.

;; The table.unpack function returns all the elements in the table as
;; multiple values. Note that table.unpack is just unpack in Lua 5.1.
;; This will make `unpack` work in both.
(var unpack (or _G.unpack table.unpack))
;; See "prettyprint" section about printing tables


;; --------------------- ;;
;; 3. Basic Flow Control ;;
;; --------------------- ;;
;; `if` checks a condition and evaluates the corresponding body.
;; Accepts any number of condition/body pairs. If an odd number of
;; args is given, the last value is treated as a catch-all "else,"
;; similar to cond in other lisps.
(let [x (math.random 64)]
  (if (= 0 (% x 10))
      "multiple of ten"
      (= 0 (% x 2))
      "even"
      "I dunno, something else"))
;; All values other than nil or false are treated as true.

;; `when` takes a single condition and evalutes the rest as a body if
;; it's truthy. Intended for side-effects. The last form is the return
;; value.
(when launch-missiles?
  (power-on)
  (open-doors)
  (fire))

;;,------------------
;;| Loops & Iteration
;;`------------------

;; each: general iteration
;; `each` runs the body once for each value provided by the iterator.
(each [key value (pairs mytbl)]
  (print "executing key")
  (print (f value)))

;; Any loop can be terminated early by placing an &until clause at the
;; end of the bindings
(local out [])
(each [_ value (pairs tbl) &until (< max-len (length out))]
  (table.insert out value))

;; `for` is a numeric loop with start, stop and optional step.
(for [i 1 10 2]
  (log-number i)
  (print i)) ;; print odd numbers under 10

;; Like each, loops using for can also be terminated early with an
;; &until clause
(var x 0)
(for [i 1 128 &until (maxed-out? x)]
  (set x (+ x i)))

;; while loops over a body until a condition is met
;; Returns nil.
(var done? false)
(while (not done?)
  (print :not-done)
  (when (< 0.95 (math.random))
    (set done? true)))
;; while uses the native lua while loop

;; `do` evaluate multiple forms returning last value
;; Accepts any number of forms and evaluates all of them in order,
;; returning the last value. This is used for inserting side-effects
;; into a form which accepts only a single value, such as in a body of
;; an if when multiple clauses make it so you can't use when. Some
;; lisps call this begin or progn.
(if launch-missiles?
    (do
      (power-on)
      (open-doors)
      (fire))
    false-alarm?
    (promote lt-petrov))

;; Many functions and macros like fn & let have an implicit do at the
;; start, so you don't have to add it to use multiple forms.

;;,-----------------------
;;| `collect` & `icollect`
;;`-----------------------
;; icollect, collect: table comprehension macros

;; The icollect macro takes a "iterator binding table" in the same
;; format that `each` takes, and returns a sequential table containing
;; all the values produced by each iteration of the macro's body. This
;; is similar to how map works in several other languages, but it is a
;; macro, not a function.

;; If the value is nil, it is omitted from the return table. This is
;; analogous to filter in other languages.
(icollect [_ v (ipairs [1 2 3 4 5 6])]
  (if (< 2 v) (* v v)))
;; -> [9 16 25 36]
;; equivalent to:
(let [tbl []]
  (each [_ v (ipairs [1 2 3 4 5 6])]
    (tset tbl (+ (length tbl) 1) (if (< 2 v) (* v v))))
  tbl)

;; The `collect` macro is almost identical, except that the body should
;; return two things: a key and a value.
(collect [k v (pairs {:apple "red" :orange "orange" :lemon "yellow"})]
  (if (not= v "yellow")
      (values (.. "color-" v) k)))
;; -> {:color-orange "orange" :color-red "apple"}
;; equivalent to:
(let [tbl {}]
  (each [k v (pairs {:apple "red" :orange "orange"})]
    (if (not= v "yellow")
      (match (values (.. "color-" v) k)
        (key value) (tset tbl key value))))
  tbl)

;; If the key and value are given directly in the body of collect and
;; not nested in an outer form, then the `values` call can be omitted
;; for brevity
(collect [k v (pairs {:a 85 :b 52 :c 621 :d 44})]
  k (* v 5))
;; -> {:a 425 :b 260 :c 3105 :d 220}

;; If the index and value are given directly in the body of collect and
;; not nested in an outer form, then the values can be omitted for
;; brevity
(icollect [_ x (ipairs [2 3]) &into [9]]
  (* x 11))
;; -> [9 22 33]

;; accumulate
;; Runs through an iterator and performs accumulation, similar to fold
;; and reduce commonly used in functional programming languages. Like
;; collect and icollect, it takes an iterator binding table and an
;; expression as its arguments. The difference is that in accumulate,
;; the first two items in the binding table are used as an
;; "accumulator" variable and its initial value. For each iteration
;; step, it evaluates the given expression and its value becomes the
;; next accumulator variable. accumulate returns the final value of
;; the accumulator variable.
(accumulate [sum 0
             i n (ipairs [10 20 30 40])]
  (+ sum n)) ; -> 100

;; `faccumulate` range accumulation Identical to accumulate, but
;; instead of taking an iterator and the same bindings as `each`, it
;; accepts the same bindings as `for` and will iterate the numerical
;; range. Accepts `&until` just like `for` and `accumulate`.
(faccumulate [n 0 i 1 5] (+ n i)) ; => 15

;; `fcollect` range comprehension Similarly to `icollect`, `fcollect`
;; provides a way of building a sequential table. Unlike `icollect`,
;; instead of an iterator it traverses a range, as accepted by the
;; `for` special. The `&into` and `&until` clauses work the same as in
;; `icollect.`
(fcollect [i 0 10 2]
  (if (> i 2) (* i i)))
;; -> [16 36 64 100]
;; equivalent to:
(let [tbl {}]
  (for [i 0 10 2]
    (if (> i 2)
        (table.insert tbl (* i i))))
  tbl)

;; `values` Returns multiple values from a function. Usually used to
;; signal failure by returning nil followed by a message.
(fn [filename]
  (if (valid-file-name? filename)
      (open-file filename)
      (values nil (.. "Invalid filename: " filename))))

;; See the Destructuring and Matching sections for more advanced Flow
;; Control.

;; ------------ ;;
;; 4. Functions ;;
;; ------------ ;;

;; Use fn to create new functions. A function always returns its last
;; statement.
(fn [] "Hello World") ; => #<function: 0x55630f9d7f20>

; (You need extra parens to call it)
((fn [] "Hello World")) ; => "Hello World"

;; Assign a function to a local variable
(local hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

;; You can use fn and name a function.
(fn hello-world [] "Hello World") ; => "Hello World"

;; The [] is the list of arguments to the function.
(fn hello [name]
  (.. "Hello " name))
(hello "Steve") ; => "Hello Steve"

;; Will accept any number of arguments. ones in excess of the declared
;; ones are ignored, and if not enough arguments are supplied to cover
;; the declared ones, the remaining ones are given values of nil.

;; Providing a name that's a table field will cause it to be inserted
;; in a table instead of bound as a local
(local functions {})

(fn functions.p [x y z]
  (print (* x (+ y z))))

;; equivalent to:
(set functions.p (fn [x y z]
                   (print (* x (+ y z)))))

;; Like Lua, functions in Fennel support tail-call optimization,
;; allowing (among other things) functions to recurse indefinitely
;; without overflowing the stack, provided the call is in a tail
;; position.
(fn factorial [x acc]
  (if (= 0 x)
      acc
      (factorial (- x 1) (* x acc))))
(factorial 5 1) ;; -> 120
;; The final form in this and all other function forms is used as the
;; return value.

;; (lambda [...])
;; Creates a function like fn does, but throws an error at runtime if
;; any of the listed arguments are nil, unless its identifier begins
;; with ?.
(lambda [x ?y z]
  (print (- x (* (or ?y 1) z))))

;; Note that the Lua runtime will fill in missing arguments with nil
;; when they are not provided by the caller, so an explicit nil
;; argument is no different than omitting an argument.

;; Docstrings and metadata
;; The fn, lambda, λ and macro forms accept an optional docstring.
(fn pxy [x y]
  "Print the sum of x and y"
  (print (+ x y)))

;; Hash function literal shorthand

;; hashfn is a special function that you can abbreviate as #
;; #foo expands to (hashfn foo)

;; Hash functions are anonymous functions of one form, with implicitly
;; named arguments.

#(+ $1 $2) ;; same as
(hashfn (+ $1 $2)) ; implementation detail; don't use directly
;; same as
(fn [a b] (+a b))

;; A lone $ in a hash function is treated as ana alias for $1.
#(+ $ 1)

#$ ; same as (fn [x] x) (aka the identity function
#val ; same as (fn [] val)
#[$1 $2 $3] ; same as (fn [a b c] [a b c])

;; ---------------------------------------------;;
;; 5. Destructuring, Binding & Pattern Matching ;;
;; ---------------------------------------------;;
;; Any time you bind a local, you can destructure it if the value is a
;; table or a function call which returns multiple values
(let [(x y z) (unpack [10 9 8])]
  (+ x y z)) ; => 27

(let [[a b c] [1 2 3]]
  (+ a b c)) ; => 6
;; If a table key is a string with the same name as the local you want
;; to bind to, you can use shorthand of just : for the key name
;; followed by the local name. This works for both creating tables and
;; destructuring them.
(let [{:msg message : val} {:msg "hello there" :val 19}]
  (print message)
  val) ; prints "hello there" and returns 19

;; When destructuring a sequential table, you can capture all the
;; remainder of the table in a local by using &
(let [[a b & c] [1 2 3 4 5 6]]
  (table.concat c ",")) ; => "3,4,5,6"

;; When destructuring a non-sequential table, you can capture the
;; original table along with the destructuring by using &as
(let [{:a a :b b &as all} {:a 1 :b 2 :c 3 :d 4}]
  (+ a b all.c all.d)) ; => 10
              
;;,-----------------------
;;| Multiple value binding
;;`-----------------------                   
;; In most contexts where you can make a new binding, you can use
;; multiple value binding.
(let [x (values 1 2 3)] x) ; = > 1
(let [(file-handle message code) (io.open "fooblah.blah")]
  message) ; => "fooblah.blah: No such file or directory"
(do
  (local (_ _ z) (table.unpack [:a :b :c :d :e]))
  z) ; => c

;; tset sets the field of a given table to a new value.
(let [tbl {:d 32}
      field :d]
  (tset tbl field 19) tbl) ; => {:d 19}
;; You can provide multiple successive field names to perform
;; nested sets.
(let [tbl {:a
           {:b {}}}
      field :c]
  (tset tbl :a :b field "d") tbl) ; => .. .. {:a {:b {:c "d"}}}

;;,------------------------
;;| `case` pattern matching
;;`------------------------
;; Evaluates its first argument, then searches thru the subsequent
;; pattern/body clauses to find one where the pattern matches the
;; value, and evaluates the corresponding body. Pattern matching can
;; be thought of as a combination of destructuring and conditionals.
(case mytable
  59      :will-never-match-hopefully
  [9 q 5] (print :q q)
  [1 a b] (+ a b))q

;; Patterns can be tables, literal values, or symbols. Any symbol is
;; implicitly checked to be not nil. Symbols can be repeated in an
;; expression to check for the same value.
(case mytable
  ;; the first and second values of mytable are not nil and are the same value
  [a a] (* a 2)
  ;; the first and second values are not nil and are not the same value
  [a b] (+ a b))

;; It's important to note that expressions are checked in order! In
;; the above example, since [a a] is checked first

;; You may allow a symbol to optionally be nil by prefixing it with ?.
(case mytable
  ;; not-nil, maybe-nil
  [a ?b] :maybe-one-maybe-two-values
  ;; maybe-nil == maybe-nil, both are nil or both are the same value
  [?a ?a] :maybe-none-maybe-two-same-values
  ;; maybe-nil, maybe-nil
  [?a ?b] :maybe-none-maybe-one-maybe-two-values)

;; Symbols prefixed by an _ are ignored and may stand in as positional
;; placeholders or markers for "any" value - including a nil value. A
;; single _ is also often used at the end of a case expression to
;; define an "else" style fall-through value.
(case mytable
  ;; not-nil, anything
  [a _b] :maybe-one-maybe-two-values
  ;; anything, anything (different to the previous ?a example!)
  ;; note this is effectively the same as []
  [_a _a] :maybe-none-maybe-one-maybe-two-values
  ;; anything, anything
  ;; this is identical to [_a _a] and in this example would never actually match.
  [_a _b] :maybe-none-maybe-one-maybe-two-values
  ;; when no other clause matched, in this case any non-table value
  _ :no-match)

;; You can match with multiple return values with
;; parenthesis, like you can with destructuring `let`
(case (io.open "/some/file")
  (nil msg) (report-error msg)
  f (read-file f))

;;,--------------
;;| Guard Clauses
;;`--------------
;; If you need to match on something more general than
;; a structure, use guard clauses:
(case [91 12 53]
  (where [a b c] (= 5 a)) :will-not-match
  (where [a b c]
         (= 0 (math.fmod
               (+ a b c) 2))
         (= 91 a))
  c) ; -> 53
;; Each form after the pattern is a condition. All conditions must
;; evaluate to true for the pattern to match.

;; If several patterns share the same body & guards, such patterns can
;; be with the `or` special in the `where` clause.
(case [5 1 2]
  (where (or [a 3 9] [a 1 2]) (= 5 a))
  "Either [5 3 9] or [5 1 2]"
  _ "anything else")

;; Symbols bound inside a case pattern are independent from any
;; existing symbols in the current scope, Sometimes it may be
;; desirable to match against an existing value in the outer scope. To
;; do this we can "pin" a binding inside the pattern with an existing
;; outer binding with the unary (= binding-name) form. The unary (=
;; binding-name) form is only valid in a case pattern and must be
;; inside a (where) guard.
(let [x 1]
  (case [1]
    ;; 1 == 1
    (where [(= x)]) x
    _ :no-match)) ; -> 1
;; Pinning is only required inside the pattern. Outer bindings are
;; automatically available inside guards and bodies as long as the
;; name has not been rebound in the pattern.

;; Note: The case macro can be used in place of the if-let macro from
;; Clojure. The reason Fennel doesn't have if-let is that case makes
;; it redundant.

;;,-------------------------
;;| `match` pattern matching
;;`-------------------------
;; match is conceptually equivalent to case, except symbols in the
;; patterns are always pinned with outer-scope symbols if they exist.
(let [x 95]
  (match [52 85 95]
    [b a a] :no         ; because a=85 and a=95
    [x y z] :no         ; because x=95 and x=52
    [a b x] :yes)) ; a and b are fresh values while x=95 and x=95

;;,-----------------------------------------------------
;;| `case-try` & `match-try` for matching multiple steps
;;`-----------------------------------------------------
;; Evaluates a series of pattern matching steps. The value from the
;; first expression is matched against the first pattern. If it
;; matches, the first body is evaluated and its value is matched
;; against the second pattern, etc.
;; 
;; If there is a (catch pat1 body1 pat2 body2 ...) form at the end,
;; any mismatch from the steps will be tried against these patterns in
;; sequence as a fallback just like a normal case. If no catch pattern
;; matches, nil is returned.
;; 
;; If there is no catch, the mismatched value will be returned as the
;; value of the entire expression.
(fn handle [conn token]
  (case-try (conn:receive :*l)
    input (parse input)
    (command-name params (= token)) (commands.get command-name)
    command (pcall command (table.unpack params))
    (catch
     (_ :timeout) nil
     (_ :closed) (pcall disconnect conn "connection closed")
     (_ msg) (print "Error handling input" msg))))

;; This is useful when you want to perform a series of steps, any of
;; which could fail. The catch clause lets you keep all your error
;; handling in one place. Note that there are two ways to indicate
;; failure in Fennel and Lua: using the assert/error functions or
;; returning nil followed by some data representing the failure. This
;; form only works on the latter, but you can use pcall to transform
;; error calls into values.

;; `match-try` for matching multiple steps
;; Unlike case-try, match-try will pin values in a given catch block
;; with those in the original steps.
(fn handle [conn token]
  (match-try (conn:receive :*l)
    input (parse input)
    (command-name params token) (commands.get command-name)
    command (pcall command (table.unpack params))
    (catch
      (_ :timeout) nil
      (_ :closed) (pcall disconnect conn "connection closed")
      (_ msg) (print "Error handling input" msg))))


;; ---------;;
;; 6. Other ;;
;; ---------;;

;; The `:` method call
;; Looks up a function in a table and calls it with the table as its
;; first argument. This is a common idiom in many Lua APIs, including
;; some built-in ones. Just like Lua, you can perform a method call by
;; calling a function name where : separates the table variable and
;; method name.
(let [f (assert (io.open "hello" "w"))]
  (f:write "world")
  (f:close))

;; If the name of the method or the table containing it isn't fixed,
;; you can use : followed by the table and then the method's name to
;; allow it to be a dynamic string instead
(let [f (assert (io.open "hello" "w"))
      method1 :write
      method2 :close]
  (: f method1 "world")
  (: f method2))

;; Unlike Lua, there's nothing special about defining functions that
;; get called this way; typically it is given an extra argument called
;; self but this is just a convention; you can name it anything.
(local t {})
(fn t.enable [self]
  (set self.enabled? true))
(t:enable)


;; ->, ->>, -?> and -?>> threading macros The -> macro takes its first
;; value and splices it into the second form as the first
;; argument. The result of evaluating the second form gets spliced
;; into the first argument of the third form, and so on.
(-> 52
    (+ 91 2)                 ; (+ 52 91 2)
    (- 8)                    ; (- (+ 52 91 2) 8)
    (print "is the answer")) ; (print (- (+ 52 91 2) 8) "is the answer")

;; The ->> macro works the same, except it splices it into the last
;; position of each form instead of the first.  -?> and -?>>, the
;; thread maybe macros, are similar to -> & ->> but they also do
;; checking after the evaluation of each threaded form. If the result
;; is false or nil then the threading stops and the result is
;; returned. -?> splices the threaded value as the first argument,
;; like ->, and -?>> splices it into the last position, like ->>.
;; This example shows how to use them to avoid accidentally indexing a
;; nil value
(-?> {:a {:b {:c 42}}}
     (. :a)
     (. :missing)
     (. :c)) ; -> nil
(-?>> :a
      (. {:a :b})
      (. {:b :missing})
      (. {:c 42})) ; -> nil
;; While -> and ->> pass multiple values thru without any trouble, the
;; checks in -?> and -?>> prevent the same from happening there
;; without performance overhead, so these pipelines are limited to a
;; single value.

;; doto
;; Similarly, the doto macro splices the first value into subsequent
;; forms. However, it keeps the same value and continually splices the
;; same thing in rather than using the value from the previous form
;; for the next form.
(doto (io.open "/tmp/err.log")
  (: :write contents)
  (: :close))
;; equivalent to:
(let [x (io.open "/tmp/err.log")]
  (: x :write contents)
  (: x :close)
  x)

;; tail!
;; the tail! form asserts that its argument is called in a tail
;; position. You can use this when the code depends on tail call
;; optimization; that way if the code is changed so that the recursive
;; call is no longer in the tail position, it will cause a compile
;; error instead of overflowing the stack later on large data sets.
(fn process-all [data i]
  (case (process (. data i))
    :done (print "Process completed.")
    :next (process-all data (+ i 1))
    :skip (do (tail! (process-all data (+ i 2)))
;;             ^^^^^ Compile error: Must be in tail position
              (print "Skipped" (+ i 1)))))

```
### Further Reading

The [fennel website] (https://fennel-lang.org/) is the best resource
on fennel.  It links to the [fennel setup guide]
(https://fennel-lang.org/setup) and to the [fennel reference manual]
(https://fennel-lang.org/reference). This docuement borrows heavily in
parts from the fennel reference manual.
