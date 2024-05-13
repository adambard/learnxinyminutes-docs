---
language: edn
filename: learnedn.edn
contributors:
  - ["Jason Yeo", "https://github.com/jsyeo"]
  - ["Jonathan D Johnston", "https://github.com/jdjohnston"]
---

Extensible Data Notation (EDN) is a format for serializing data.

EDN is a subset of the syntax used by Clojure. Reading data defined by EDN is
safer than that defined by the full Clojure syntax, especially from untrusted
sources. EDN is restricted to data, no code. It is similar in intent to JSON.
Though it is more commonly used in Clojure, there are implementations of EDN
for many other languages.

The main benefit of EDN over JSON and YAML is that it is extensible. We
will see how it is extended later on.

```clojure
; Comments start with a semicolon.
; Anything after the semicolon is ignored.

;;;;;;;;;;;;;;;;;;;
;;; Basic Types ;;;
;;;;;;;;;;;;;;;;;;;

nil         ; also known in other languages as null

; Booleans
true
false

; Strings are enclosed in double quotes
"hungarian breakfast"
"farmer's cheesy omelette"

; Characters are preceded by backslashes
\g \r \a \c \e

; Keywords start with a colon. They behave like enums. Kind of
; like symbols in Ruby.
:eggs
:cheese
:olives

; Symbols are used to represent identifiers. 
; You can namespace symbols by using /. Whatever precedes / is
; the namespace of the symbol.
spoon
kitchen/spoon ; not the same as spoon
kitchen/fork
github/fork   ; you can't eat with this

; Integers and floats
42
3.14159

; Lists are sequences of values
(:bun :beef-patty 9 "yum!")

; Vectors allow random access
[:gelato 1 2 -2]

; Maps are associative data structures that associate the key with its value
{:eggs        2
 :lemon-juice 3.5
 :butter      1}

; You're not restricted to using keywords as keys
{[1 2 3 4] "tell the people what she wore",
 [5 6 7 8] "the more you see the more you hate"}

; You may use commas for readability. They are treated as whitespace.

; Sets are collections that contain unique elements.
#{:a :b 88 "huat"}

;;;;;;;;;;;;;;;;;;;;;;;
;;; Tagged Elements ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; EDN can be extended by tagging elements with # symbols.

#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}

; Let me explain this with a Clojure example. Suppose I want to transform that
; piece of EDN into a MenuItem record.

(defrecord MenuItem [name rating])

; defrecord defined, among other things, map->MenuItem which will take a map
; of field names (as keywords) to values and generate a user.MenuItem record

; To transform EDN to Clojure values, I will need to use the built-in EDN
; reader, clojure.edn/read-string

(clojure.edn/read-string "{:eggs 2 :butter 1 :flour 5}")
; -> {:eggs 2 :butter 1 :flour 5}

; To transform tagged elements, pass to clojure.edn/read-string an option map
; with a :readers map that maps tag symbols to data-reader functions, like so

(clojure.edn/read-string
    {:readers {'MyYelpClone/MenuItem map->MenuItem}}
    "#MyYelpClone/MenuItem {:name \"eggs-benedict\" :rating 10}")
; -> #user.MenuItem{:name "eggs-benedict", :rating 10}
```

# References

- [EDN spec](https://github.com/edn-format/edn)
- [Implementations](https://github.com/edn-format/edn/wiki/Implementations)
- [Tagged Elements](http://www.compoundtheory.com/clojure-edn-walkthrough/)
