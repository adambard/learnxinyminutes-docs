---
language: LB Stanza
filename: learn-stanza.stanza
contributors: 
  - ["Mike Hilgendorf", "https://github.com/m-hilgendorf"]
---

LB Stanza (or Stanza for short) is a new optionally-typed general purpose programming language from the University of California, Berkeley. Stanza was designed to help programmers tackle the complexity of architecting large programs and significantly increase the productivity of application programmers across the entire software development life cycle.


```
; this is a comment 
;<A>
This is a block comment 
    ;<B> 
        block comments can be nested with optional tags. 
    ;<B>
;<A>
defpackage learn-stanza-in-y: 
  import core 
  import collections 

;==============================================================================
; The basics, things you'd find in most programming languages
;==============================================================================


; Variables can be mutable (var) or immutable (val)
val immutable = "this string can't be changed"
var mutable = "this one can be" 
mutable = "like this"

; The basic data types (annotations are optional) 
val an-int: Int = 12345
val a-long: Long = 12345L
val a-float: Float = 1.2345f
val a-double: Double = 3.14159
val a-string: String = "this is a string"
val a-multiline-string = \<tag>
    this is a "raw" string literal 
\<tag>

; Print a formatted string with println and "..." % [...]
println("this is a formatted string %_ %_" % [mutable, immutable])

; Stanza is optionally typed, and has a ? (any) type. 
var anything:? = 0
anything = 3.14159
anything = "a string"

; Stanza has basic collections like Tuples, Arrays, Vectors and HashTables
val tuple: Tuple<?> = [mutable, immutable]

val array = Array<?>(3)
array[0] = "string"
array[1] = 1
array[2] = 1.23455
; array[3] = "out-of-bounds" ; arrays are bounds-checked 

val vector = Vector<?>()
vector[0] = "string"
vector[1] = 1
vector[2] = 3.14159

val hash-table = HashTable<String, ?>()
hash-table["0"] = 0
hash-table["1"] = 1 
hash-table["2"] = 1 


;==============================================================================
; Functions
;==============================================================================
; Functions are declared with the `defn` keyword 
defn my-function (arg:?) : ; note the space between identifier and arg list
  println("called my-function with %_" % [arg])

my-function("arg")  ; note the lack of a space to call the function

; Functions can be declared inside another function and capture variables from
; the surrounding environment.
defn outer (arg): 
  defn inner (): 
    println("outer had arg: %_" % [arg])
  inner()

outer("something")

; functions are "first-class" in stanza, meaning you can assign variables 
; to functions and pass functions as arguments to other functions. 
val a-function = outer 
defn do-n-times (arg, func, n:Int): 
  for i in 0 to n do : 
    func(arg)
do-n-times("argument", a-function, 3)

; sometimes you want to define a function inline, or use an anonymous function.
; for this you can use the syntax: 
;   fn (args): 
;       ... 
do-n-times("hello", fn (arg): println(arg), 2)

; there is a shorthand for writing anonymous functions
do-n-times("hello", { println(_) }, 2)

; the short hand works for multiple arguments as well. 
val multi-lambda = { println(_ + 2 * _) }
multi-lambda(1, 2)

;==============================================================================
; User defined types
;==============================================================================
; Structs are declared with the `defstruct` keyword
defstruct MyStruct: 
  field

; constructors are derived automatically
val my-struct = MyStruct("field:value")

; fields are accessed using function-call syntax
println(field(my-struct))

; Stanza supports subtyping with a "multimethod" system based on method 
; overloading.
deftype MyType
defmulti a-method (m:MyType)

defstruct Foo <: MyType
defstruct Bar <: MyType
defmethod a-method (a-foo: Foo):
  println("called a-method on a Foo")

defmethod a-method (a-foo: Bar):
  println("called a-method on a Bar")

;==============================================================================
; The Type System
;==============================================================================
; True and Falseare types with a single value. 
val a-true: True = true 
val a-false: False = false 

; You can declare a union type, or a value that is one of a set of types 
val a-boolean: True|False = true 
val another-boolean: True|False = false 

; You can pattern match on types 
match(a-boolean):
  (t:True): println("is true")
  (f:False): println("is false")

; You can match against a single possible type
match(a-boolean:True):
  println("is still true")
else: 
  println("is not true")

; You can compose program logic around the type of a variable
if anything is Float :
  println("anything is a float")
else if anything is-not String : 
  println("anything is not an int")
else : 
  println("I don't know what anything is")

;==============================================================================
; Control Flow 
;==============================================================================
; stanza has the standard basic control flow 
val condition = [false, false]
if condition[0] : 
  ; do something 
  false 
else if condition[1] : 
  ; do another thing
  false 
else :
  ; whatever else
  false

; there is also a switch statement, which can be used to pattern match
; on values (as opposed to types)
switch(anything):
  "this": false 
  "that": false 
  "the-other-thing": false 
  else: false 

; for and while loops are supported
while condition[0]: 
  println("do stuff")

for i in 0 to 10 do:  
  vector[i] = i

; stanza also supports named labels which can function as break or return 
; statements
defn another-fn (): 
  label<False> return:
    label<False> break:
      while true: 
        if condition[0] is False: 
            break(false) 
    return(false)

; For a comprehensive guide on Stanza's advanced control flow, check out 
; this page: http://lbstanza.org/chapter9.html from Stanza-by-Example

;==============================================================================
; Sequences 
;==============================================================================
; for "loops" are sugar for a more powerful syntax. 
val xs = [1, 2, 3] 
val ys = ['a', 'b', 'c']
val zs = ["foo", "bar", "baz"]

for (x in xs, y in ys, z in zs) do : 
  println("x:%_, y:%_, z:%_" % [x, y, z])


;xs, ys, and zs are all "Seqable" meaning they are Seq types (sequences). 
; the `do` identifier is a special function that just applies the body of
; the for loop to each element of the sequence.
; 
; A common sequence task is concatenating sequences. This is accomplished 
; using the `seq-cat` function. This is analogous to "flattening" iterateors
val concat = to-tuple $ 
  for sequence in [xs, ys, zs] seq-cat: 
    sequence

; we can also use a variation to interleave the elements of multiple sequences
val interleaved = to-tuple $ 
  for (x in xs, y in ys, z in zs) seq-cat : 
    [x, y, z]

println("[%,] [%,]" % [concat, interleaved])

; Another common task is mapping a sequence to another, for example multiplying
; all the elements of a list of numbers by a constant. To do this we use `seq`. 
var numbers = [1.0, 2.0, 3.0, 4.0] 
numbers = to-tuple $ 
  for n in numbers seq : 
    2.0 * n 
println("%," % [numbers])

if find({_ == 2.0}, numbers) is-not False : 
  println("found it!")

; or maybe we just want to know if there's something in a sequence 
var is-there = 
  for n in numbers any? :
    n == 2.0 

; since this is "syntactic sugar" we can write it explicitly using an 
; anonymous function
is-there = any?({_ == 2.0}, numbers)

; a detailed reference of the sequence library and various adaptors can 
; be found here: http://lbstanza.org/reference.html#anchor439


=========================================================================
; Documentation 
;=========================================================================
;
; Top level statements can be prefixed with the "doc" field which takes 
; a string value and is used to autogenerate documentation for the package. 
doc: \<doc>
    # Document Strings 

    ```
    val you-can = "include code snippets, too" 
    ```

    To render documentation as markdown (compatible with mdbook)

    ```bash 
    stanza doc source.stanza -o docs 
    ```
\<doc>
defn docfn () : false 
```
