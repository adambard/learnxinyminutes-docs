---
name: MakrellPy
contributors:
    - ["Hans-Christian Holm", "https://hch.no/"]
filename: learn-makrellpy.mrpy
---

MakrellPy is the Python-hosted member of the Makrell language family. It
compiles to Python AST and runs on CPython, so every Python library is
available. The syntax is compact and expression-oriented, built on a shared
structural foundation called MBF that also powers the family's data and
markup formats.

```makrell
####################################################
## 1. Primitives
####################################################

# Numbers
42              # => 42
3.14            # => 3.14
-5.7e-3         # => -0.0057

# Number suffixes — scale or convert inline
2.5M            # => 2500000        multiply by 10^6
100k            # => 100000         multiply by 10^3
2pi             # => 6.283185...    multiply by pi
90deg           # => 1.570796...    degrees to radians
3i              # => 3j             Python complex

# Booleans and null
true            # => True
false           # => False
null            # => None

# Strings with optional suffixes
"hello"                 # => "hello"
"2026-04-01"dt          # => datetime(2026, 4, 1)
"ff"hex                 # => 255
"[a-z]+"regex           # => re.compile("[a-z]+")

# E-strings interpolate {expr} segments
name = "world"
"Hello {name}!"e        # => "Hello world!"

# Collections
[1 2 3]         # list  => [1, 2, 3]
(1 2 3)         # tuple => (1, 2, 3)
[]              # => []
()              # => ()


####################################################
## 2. Operators
####################################################

# Arithmetic
2 + 3           # => 5
10 / 3          # => 3.333...
2 ** 8          # => 256

# Comparison and logic
3 == 3          # => True
3 != 4          # => True
true && false   # => False
true || false   # => True

# + coerces to string when either side is a string
"age: " + 25    # => "age: 25"

# Assignment (right-associative, lowest precedence)
x = y = 10

# @ indexes into collections (high precedence)
items = [10 20 30]
items @ 0       # => 10
items @ 2       # => 30

# . accesses members (highest precedence)
"hello".upper   # => <method>

# .. creates a range/slice
1 .. 5          # => slice(1, 5)


####################################################
## 3. Calling Functions
####################################################

# Curly brackets call functions: {f arg1 arg2}
{print "hello"}               # prints: hello
{len [1 2 3]}                 # => 3
{max 10 20}                   # => 20

# Nested calls work as you'd expect
{print {len "Makrell"}}       # prints: 7


####################################################
## 4. Defining Functions
####################################################

# Named function
{fun greet [who]
    "Hello, " + who + "!"}

{greet "Makrell"}   # => "Hello, Makrell!"

# Anonymous function
double = {fun [x] x * 2}
{double 5}          # => 10

# Lambda with the arrow operator ->
triple = x -> x * 3
{triple 4}          # => 12

# Multi-arg lambda
hyp = [x y] -> {Math.sqrt x ** 2 + y ** 2}

# No-arg lambda
dice = [] -> {random.randint 1 6}

# Partial application: _ becomes a parameter
add = {fun [a b] a + b}
add5 = {add 5 _}        # => a function that adds 5
{add5 10}                # => 15

inc = {add _ 1}
{inc 99}                 # => 100


####################################################
## 5. Pipes and Composition
####################################################

# | threads a value left-to-right:  a | f  =>  {f a}
5 | double               # => 10
5 | double | triple      # => 30

# Combine with partial application for expressive chains
[1 2 3 4 5]
    | {filter {< _ 4}}       # keep items < 4 => [1, 2, 3]
    | {map {* _ 2}}           # double them   => [2, 4, 6]
    | sum                     # sum           => 12

# |* maps a function over each element
[1 2 3] |* double        # => [2, 4, 6]

# \ is the reverse pipe:  f \ a  =>  {f a}
double \ 5              # => 10

# -> binds tighter than |, so lambdas compose naturally
[1 2 3] | {map x -> x + 10}   # => [11, 12, 13]


####################################################
## 6. Control Flow
####################################################

# if is an expression — it returns a value
x = 7
{if x > 0 "positive" "non-positive"}   # => "positive"

# Chained conditions
{if x > 100 "high"
    x > 50  "medium"
            "low"}                       # => "low"

# when — like if with null as the implicit else
{when x > 0 {print "positive"}}

# Loops
{while i < 3
    {print i}
    i = i + 1}

{for item [10 20 30]
    {print item}}

# do block — introduces a scope, returns the last expression
result = {do
    a = 10
    b = 20
    a + b}
# result => 30


####################################################
## 7. Pattern Matching
####################################################

# Match tests a value against pattern-result pairs
{match 42
    0     "zero"
    42    "the answer"
    _     "other"}
# => "the answer"

# _ is the wildcard — matches anything

# Alternatives with |
{match 5
    2 | 3 | 5   "small prime"
    _            "other"}
# => "small prime"

# List patterns
{match [1 2 3]
    [1 _ _]   "starts with 1"
    [_ _ _]   "three items"
    _         "other"}

# Binding with =
{match [10 20]
    [a=_ b=_]   a + b
    _            0}
# => 30

# Type patterns
{match 42
    _:str   "string"
    _:int   "integer"
    _       "other"}
# => "integer"

# Constructor / dataclass patterns
{dataclass Point
    x:float
    y:float}

{match {Point 3.0 4.0}
    {$type Point [0.0 0.0]}   "origin"
    {$type Point [x y]}       "({x}, {y})"e}
# => "(3.0, 4.0)"

# Regular sequence patterns — regex-like matching on lists
{match [1 2 2 2 3]
    [$r 1 (some 2) 3]   "one, twos, then three"
    _                    "other"}
# => "one, twos, then three"
# Quantifiers: maybe (0-1), some (1+), any (0+), 3* (exact), (2..5)* (range)

# Boolean pattern operators
[1 2 3] ~= [_ _ _]     # => True   (matches?)
[1 2] !~= [_ _ _]      # => True   (doesn't match?)


####################################################
## 8. Classes
####################################################

{class Animal
    {fun __init__ [self name sound]
        self.name = name
        self.sound = sound}
    {fun speak [self]
        self.name + " says " + self.sound}}

a = {Animal "Cat" "meow"}
{a.speak}       # => "Cat says meow"

# Inheritance
{class Dog [Animal]
    {fun __init__ [self name]
        {{super}.__init__ name "woof"}}
    {fun fetch [self item]
        self.name + " fetches " + item}}

d = {Dog "Rex"}
{d.speak}        # => "Rex says woof"
{d.fetch "ball"} # => "Rex fetches ball"

# Dataclasses — auto-generate __init__, __repr__, etc.
{dataclass User
    name:str
    age:int
    email:str}

u = {User "Alice" 30 "alice@example.com"}
u.name          # => "Alice"


####################################################
## 9. Exception Handling
####################################################

{try
    x = 1 / 0
    {catch e:ZeroDivisionError
        {print "caught:" e}}
    {finally
        {print "done"}}}
# prints: caught: division by zero
#         done

# {else} runs when no exception was raised
{try
    result = {int "42"}
    {catch ValueError {print "bad"}}
    {else {print "ok:" result}}}
# prints: ok: 42


####################################################
## 10. Python Interop
####################################################

# Import any Python module — interop is seamless because
# Makrell compiles to Python AST directly
{import json}
{json.loads "[1, 2, 3]"}    # => [1, 2, 3]

{import os.path}
{os.path.exists "/tmp"}     # => True or False

# Every pip package works.
# {import pandas}
# df = {pandas.read_csv "data.csv"}


####################################################
## 11. Async/Await
####################################################

{async fun fetch_data [url]
    response = {await {aiohttp.get url}}
    {await {response.text}}}

{async for item async_iter
    {await {process item}}}

{async with {aiohttp.ClientSession} session
    {await {session.get url}}}


####################################################
## 12. Metaprogramming
####################################################

# Quote captures syntax as data (AST nodes, not values)
q = {quote [a b c]}         # => a SquareBrackets node

# Unquote splices values into quoted forms
x = 42
q = {quote {unquote x}}     # splice x's value into the form

# Meta blocks run at compile time
{meta greeting = "hello from compile time"}

# Macros transform syntax before compilation
{def macro twice [ns]
    ns = {regular ns}
    expr = ns@0
    {quote [{unquote expr} {unquote expr}]}}

{twice 5}       # => [5, 5]

# User-defined suffixes
{def strsuffix upper [s] {s.upper}}
"hello"upper    # => "HELLO"

{def floatsuffix pct [s] {float s} / 100}
50pct           # => 0.5

# User-defined operators
{def operator <=> 50 left}


####################################################
## 13. A Complete Example
####################################################

{import collections}

{fun word_freq [text n]
    text
    | {str.lower _}
    | {str.split _}
    | {collections.Counter _}
    | {_.most_common n}}

{word_freq "one fish two fish red fish blue fish" 3}
# => [('fish', 4), ('one', 1), ('two', 1)]
```

## Want to know more?

- **[MakrellPy documentation](https://makrell.dev/makrellpy/)** — full guide, cookbook, and reference for the Python track.
- **[Makrell family site](https://makrell.dev/)** — documentation for all languages and formats in the Makrell family.
- **[MakrellTS playground](https://makrell.dev/playground/)** — try Makrell in the browser. Note: the playground runs MakrellTS, so some Python-specific features (dataclasses, Python interop, async for/with) are not available there.
- **[In-depth design article](https://makrell.dev/odds-and-ends/makrell-design-article.html)** — a technical article covering the architecture, macro system, and design philosophy behind the Makrell family.
- **[Source on GitHub](https://github.com/hcholm/makrell-omni)** — the monorepo for all Makrell implementations, specifications, and tooling.
