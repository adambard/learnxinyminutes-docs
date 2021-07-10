---
language: Julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
    - ["Pranit Bauva", "https://github.com/pranitbauva1997"]
    - ["Daniel YC Lin", "https://github.com/dlintw"]
filename: learnjulia.jl
---

Julia is a new homoiconic functional language focused on technical computing.
While having the full power of homoiconic macros, first-class functions,
and low-level control, Julia is as easy to learn and use as Python.

This is based on Julia version 1.0.0.

```julia
# Single line comments start with a hash (pound) symbol.
#= Multiline comments can be written
   by putting '#=' before the text  and '=#'
   after the text. They can also be nested.
=#

####################################################
## 1. Primitive Datatypes and Operators
####################################################

# Everything in Julia is an expression.

# There are several basic types of numbers.
typeof(3)       # => Int64
typeof(3.2)     # => Float64
typeof(2 + 1im) # => Complex{Int64}
typeof(2 // 3)  # => Rational{Int64}

# All of the normal infix operators are available.
1 + 1      # => 2
8 - 1      # => 7
10 * 2     # => 20
35 / 5     # => 7.0
10 / 2     # => 5.0  # dividing integers always results in a Float64
div(5, 2)  # => 2    # for a truncated result, use div
5 \ 35     # => 7.0
2^2        # => 4    # power, not bitwise xor
12 % 10    # => 2

# Enforce precedence with parentheses
(1 + 3) * 2  # => 8

# Julia (unlike Python for instance) has integer under/overflow
10^19      # => -8446744073709551616
# use bigint or floating point to avoid this
big(10)^19 # => 10000000000000000000
1e19       # => 1.0e19
10.0^19    # => 1.0e19

# Bitwise Operators
~2         # => -3 # bitwise not
3 & 5      # => 1  # bitwise and
2 | 4      # => 6  # bitwise or
xor(2, 4)  # => 6  # bitwise xor
2 >>> 1    # => 1  # logical shift right
2 >> 1     # => 1  # arithmetic shift right
2 << 1     # => 4  # logical/arithmetic shift left

# Use the bitstring function to see the binary representation of a number.
bitstring(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bitstring(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# Boolean values are primitives
true
false

# Boolean operators
!true   # => false
!false  # => true
1 == 1  # => true
2 == 1  # => false
1 != 1  # => false
2 != 1  # => true
1 < 10  # => true
1 > 10  # => false
2 <= 2  # => true
2 >= 2  # => true
# Comparisons can be chained, like in Python but unlike many other languages
1 < 2 < 3  # => true
2 < 3 < 2  # => false

# Strings are created with "
"This is a string."

# Character literals are written with '
'a'

# Strings are UTF8 encoded, so strings like "π" or "☃" are not directly equivalent
# to an array of single characters.
# Only if they contain only ASCII characters can they be safely indexed.
ascii("This is a string")[1]    # => 'T'
# => 'T': ASCII/Unicode U+0054 (category Lu: Letter, uppercase)
# Beware, Julia indexes everything from 1 (like MATLAB), not 0 (like most languages).
# Otherwise, iterating over strings is recommended (map, for loops, etc).

# String can be compared lexicographically, in dictionnary order:
"good" > "bye"   # => true
"good" == "good" # => true
"1 + 2 = 3" == "1 + 2 = $(1 + 2)" # => true

# $(..) can be used for string interpolation:
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# You can put any Julia expression inside the parentheses.

# Printing is easy
println("I'm Julia. Nice to meet you!")  # => I'm Julia. Nice to meet you!

# Another way to format strings is the printf macro from the stdlib Printf.
using Printf   # this is how you load (or import) a module
@printf "%d is less than %f\n" 4.5 5.3   # => 5 is less than 5.300000


####################################################
## 2. Variables and Collections
####################################################

# You don't declare variables before assigning to them.
someVar = 5  # => 5
someVar      # => 5

# Accessing a previously unassigned variable is an error
try
    someOtherVar  # => ERROR: UndefVarError: someOtherVar not defined
catch e
    println(e)
end

# Variable names start with a letter or underscore.
# After that, you can use letters, digits, underscores, and exclamation points.
SomeOtherVar123! = 6  # => 6

# You can also use certain unicode characters
# here ☃ is a Unicode 'snowman' characters, see http://emojipedia.org/%E2%98%83%EF%B8%8F if it displays wrongly here
☃ = 8  # => 8
# These are especially handy for mathematical notation, like the constant π
2 * π  # => 6.283185307179586

# A note on naming conventions in Julia:
#
# * Word separation can be indicated by underscores ('_'), but use of
#   underscores is discouraged unless the name would be hard to read
#   otherwise.
#
# * Names of Types begin with a capital letter and word separation is shown
#   with CamelCase instead of underscores.
#
# * Names of functions and macros are in lower case, without underscores.
#
# * Functions that modify their inputs have names that end in !. These
#   functions are sometimes called mutating functions or in-place functions.

# Arrays store a sequence of values indexed by integers 1 through n:
a = Int64[] # => 0-element Array{Int64,1}

# 1-dimensional array literals can be written with comma-separated values.
b = [4, 5, 6] # => 3-element Array{Int64,1}: [4, 5, 6]
b = [4; 5; 6] # => 3-element Array{Int64,1}: [4, 5, 6]
b[1]    # => 4
b[end]  # => 6

# 2-dimensional arrays use space-separated values and semicolon-separated rows.
matrix = [1 2; 3 4] # => 2×2 Array{Int64,2}: [1 2; 3 4]

# Arrays of a particular type
b = Int8[4, 5, 6] # => 3-element Array{Int8,1}: [4, 5, 6]

# Add stuff to the end of a list with push! and append!
# By convention, the exclamation mark '!' is appended to names of functions
# that modify their arguments
push!(a, 1)    # => [1]
push!(a, 2)    # => [1,2]
push!(a, 4)    # => [1,2,4]
push!(a, 3)    # => [1,2,4,3]
append!(a, b)  # => [1,2,4,3,4,5,6]

# Remove from the end with pop
pop!(b)  # => 6
b # => [4,5]

# Let's put it back
push!(b, 6)  # => [4,5,6]
b # => [4,5,6]

a[1]  # => 1  # remember that Julia indexes from 1, not 0!

# end is a shorthand for the last index. It can be used in any
# indexing expression
a[end]  # => 6

# we also have popfirst! and pushfirst!
popfirst!(a)  # => 1 
a # => [2,4,3,4,5,6]
pushfirst!(a, 7)  # => [7,2,4,3,4,5,6]
a # => [7,2,4,3,4,5,6]

# Function names that end in exclamations points indicate that they modify
# their argument.
arr = [5,4,6]  # => 3-element Array{Int64,1}: [5,4,6]
sort(arr)      # => [4,5,6]
arr            # => [5,4,6]
sort!(arr)     # => [4,5,6]
arr            # => [4,5,6]

# Looking out of bounds is a BoundsError
try
    a[0] 
    # => ERROR: BoundsError: attempt to access 7-element Array{Int64,1} at 
    # index [0]
    # => Stacktrace:
    # =>  [1] getindex(::Array{Int64,1}, ::Int64) at .\array.jl:731
    # =>  [2] top-level scope at none:0
    # =>  [3] ...
    # => in expression starting at ...\LearnJulia.jl:180
    a[end + 1] 
    # => ERROR: BoundsError: attempt to access 7-element Array{Int64,1} at 
    # index [8]
    # => Stacktrace:
    # =>  [1] getindex(::Array{Int64,1}, ::Int64) at .\array.jl:731
    # =>  [2] top-level scope at none:0
    # =>  [3] ...
    # => in expression starting at ...\LearnJulia.jl:188
catch e
    println(e)
end

# Errors list the line and file they came from, even if it's in the standard
# library. You can look in the folder share/julia inside the julia folder to
# find these files.

# You can initialize arrays from ranges
a = [1:5;]  # => 5-element Array{Int64,1}: [1,2,3,4,5]
a2 = [1:5]  # => 1-element Array{UnitRange{Int64},1}: [1:5]

# You can look at ranges with slice syntax.
a[1:3]    # => [1, 2, 3]
a[2:end]  # => [2, 3, 4, 5]

# Remove elements from an array by index with splice!
arr = [3,4,5]
splice!(arr, 2) # => 4 
arr # => [3,5]

# Concatenate lists with append!
b = [1,2,3]
append!(a, b) # => [1, 2, 3, 4, 5, 1, 2, 3]
a # => [1, 2, 3, 4, 5, 1, 2, 3]

# Check for existence in a list with in
in(1, a)  # => true

# Examine the length with length
length(a)  # => 8

# Tuples are immutable.
tup = (1, 2, 3)  # => (1,2,3)
typeof(tup) # => Tuple{Int64,Int64,Int64}
tup[1] # => 1
try
    tup[1] = 3  
    # => ERROR: MethodError: no method matching 
    # setindex!(::Tuple{Int64,Int64,Int64}, ::Int64, ::Int64)
catch e
    println(e)
end

# Many array functions also work on tuples
length(tup) # => 3
tup[1:2]    # => (1,2)
in(2, tup)  # => true

# You can unpack tuples into variables
a, b, c = (1, 2, 3)  # => (1,2,3)  
a  # => 1
b  # => 2
c  # => 3

# Tuples are created even if you leave out the parentheses
d, e, f = 4, 5, 6  # => (4,5,6)
d  # => 4
e  # => 5
f  # => 6

# A 1-element tuple is distinct from the value it contains
(1,) == 1 # => false
(1) == 1  # => true

# Look how easy it is to swap two values
e, d = d, e  # => (5,4) 
d  # => 5
e  # => 4

# Dictionaries store mappings
emptyDict = Dict()  # => Dict{Any,Any} with 0 entries

# You can create a dictionary using a literal
filledDict = Dict("one" => 1, "two" => 2, "three" => 3)
# => Dict{String,Int64} with 3 entries:
# =>  "two" => 2, "one" => 1, "three" => 3

# Look up values with []
filledDict["one"]  # => 1

# Get all keys
keys(filledDict)
# => Base.KeySet for a Dict{String,Int64} with 3 entries. Keys:
# =>  "two", "one", "three"
# Note - dictionary keys are not sorted or in the order you inserted them.

# Get all values
values(filledDict)
# => Base.ValueIterator for a Dict{String,Int64} with 3 entries. Values: 
# =>  2, 1, 3
# Note - Same as above regarding key ordering.

# Check for existence of keys in a dictionary with in, haskey
in(("one" => 1), filledDict)  # => true
in(("two" => 3), filledDict)  # => false
haskey(filledDict, "one")     # => true
haskey(filledDict, 1)         # => false

# Trying to look up a non-existent key will raise an error
try
    filledDict["four"]  # => ERROR: KeyError: key "four" not found
catch e
    println(e)
end

# Use the get method to avoid that error by providing a default value
# get(dictionary, key, defaultValue)
get(filledDict, "one", 4)   # => 1
get(filledDict, "four", 4)  # => 4

# Use Sets to represent collections of unordered, unique values
emptySet = Set()  # => Set(Any[])
# Initialize a set with values
filledSet = Set([1, 2, 2, 3, 4])  # => Set([4, 2, 3, 1])

# Add more values to a set
push!(filledSet, 5)  # => Set([4, 2, 3, 5, 1])

# Check if the values are in the set
in(2, filledSet)   # => true
in(10, filledSet)  # => false

# There are functions for set intersection, union, and difference.
otherSet = Set([3, 4, 5, 6])         # => Set([4, 3, 5, 6])
intersect(filledSet, otherSet)      # => Set([4, 3, 5])
union(filledSet, otherSet)          # => Set([4, 2, 3, 5, 6, 1])
setdiff(Set([1,2,3,4]), Set([2,3,5])) # => Set([4, 1])

# Assignment with `=` attaches a new label to the same value without copying
a = [1, 2, 3]
b = a
# Now `b` and `a` point to the same value, so changing one affects the other:
a[3] = 5
b[3]  # => 5

# The `copy()` function can create a shallow copy of an array, dictionary,
# or other container
a = [1, 2, 3]
c = copy(a)
a[3] = 5
c[3] # => 3

####################################################
## 3. Control Flow
####################################################

# Let's make a variable
someVar = 5

# Here is an if statement. Indentation is not meaningful in Julia.
if someVar > 10
    println("someVar is totally bigger than 10.")
elseif someVar < 10    # This elseif clause is optional.
    println("someVar is smaller than 10.")
else                    # The else clause is optional too.
    println("someVar is indeed 10.")
end
# => prints "some var is smaller than 10"

# For loops iterate over iterables.
# Iterable types include Range, Array, Set, Dict, and AbstractString.
for animal = ["dog", "cat", "mouse"]
    println("$animal is a mammal")
    # You can use $ to interpolate variables or expression into strings.
    # In this special case, no need for parenthesis: $animal and $(animal) give the same
end
# => dog is a mammal
# => cat is a mammal
# => mouse is a mammal

# You can use 'in' instead of '='.
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# => dog is a mammal
# => cat is a mammal
# => mouse is a mammal

for pair in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    from, to = pair
    println("$from is a $to")
end
# => mouse is a mammal
# => cat is a mammal
# => dog is a mammal

for (k, v) in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    println("$k is a $v")
end
# => mouse is a mammal
# => cat is a mammal
# => dog is a mammal

# While loops loop while a condition is true
let x = 0
    while x < 4
        println(x)
        x += 1  # Shorthand for in place increment: x = x + 1
    end
end
# => 0
# => 1
# => 2
# => 3

# Handle exceptions with a try/catch block
try
    error("help")
catch e
    println("caught it $e")
end
# => caught it ErrorException("help")

####################################################
## 4. Functions
####################################################

# The keyword 'function' creates new functions
# function name(arglist)
#   body...
# end
function add(x, y)
    println("x is $x and y is $y")

    # Functions return the value of their last statement
    x + y
end

add(5, 6)
# => x is 5 and y is 6
# => 11

# Compact assignment of functions
f_add(x, y) = x + y  # => f_add (generic function with 1 method)
f_add(3, 4)  # => 7

# Function can also return multiple values as tuple
fn(x, y) = x + y, x - y # => fn (generic function with 1 method)
fn(3, 4)  # => (7, -1)

# You can define functions that take a variable number of
# positional arguments
function varargs(args...)
    return args
    # use the keyword return to return anywhere in the function
end
# => varargs (generic function with 1 method)

varargs(1, 2, 3)  # => (1,2,3)

# The ... is called a splat.
# We just used it in a function definition.
# It can also be used in a function call,
# where it will splat an Array or Tuple's contents into the argument list.
add([5,6]...)  # this is equivalent to add(5,6)

x = (5, 6)  # => (5,6)
add(x...)  # this is equivalent to add(5,6)


# You can define functions with optional positional arguments
function defaults(a, b, x=5, y=6)
    return "$a $b and $x $y"
end
# => defaults (generic function with 3 methods)

defaults('h', 'g')  # => "h g and 5 6"
defaults('h', 'g', 'j')  # => "h g and j 6"
defaults('h', 'g', 'j', 'k')  # => "h g and j k"
try
    defaults('h')  # => ERROR: MethodError: no method matching defaults(::Char)
    defaults()  # => ERROR: MethodError: no method matching defaults()
catch e
    println(e)
end

# You can define functions that take keyword arguments
function keyword_args(;k1=4, name2="hello")  # note the ;
    return Dict("k1" => k1, "name2" => name2)
end
# => keyword_args (generic function with 1 method)

keyword_args(name2="ness")  # => ["name2"=>"ness", "k1"=>4]
keyword_args(k1="mine")     # => ["name2"=>"hello", "k1"=>"mine"]
keyword_args()              # => ["name2"=>"hello", "k1"=>4]

# You can combine all kinds of arguments in the same function
function all_the_args(normalArg, optionalPositionalArg=2; keywordArg="foo")
    println("normal arg: $normalArg")
    println("optional arg: $optionalPositionalArg")
    println("keyword arg: $keywordArg")
end
# => all_the_args (generic function with 2 methods)

all_the_args(1, 3, keywordArg=4)
# => normal arg: 1
# => optional arg: 3
# => keyword arg: 4

# Julia has first class functions
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end
# => create_adder (generic function with 1 method)

# This is "stabby lambda syntax" for creating anonymous functions
(x -> x > 2)(3)  # => true

# This function is identical to create_adder implementation above.
function create_adder(x)
    y -> x + y
end
# => create_adder (generic function with 1 method)

# You can also name the internal function, if you want
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end
# => create_adder (generic function with 1 method)

add_10 = create_adder(10) # => (::getfield(Main, Symbol("#adder#11")){Int64}) 
                          # (generic function with 1 method)
add_10(3) # => 13


# There are built-in higher order functions
map(add_10, [1,2,3])  # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7])  # => [6, 7]

# We can use list comprehensions
[add_10(i) for i = [1, 2, 3]]   # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] # => [6, 7]

####################################################
## 5. Types
####################################################

# Julia has a type system.
# Every value has a type; variables do not have types themselves.
# You can use the `typeof` function to get the type of a value.
typeof(5)  # => Int64

# Types are first-class values
typeof(Int64)     # => DataType
typeof(DataType)  # => DataType
# DataType is the type that represents types, including itself.

# Types are used for documentation, optimizations, and dispatch.
# They are not statically checked.

# Users can define types
# They are like records or structs in other languages.
# New types are defined using the `struct` keyword.

# struct Name
#   field::OptionalType
#   ...
# end
struct Tiger
    taillength::Float64
    coatcolor  # not including a type annotation is the same as `::Any`
end

# The default constructor's arguments are the properties
# of the type, in the order they are listed in the definition
tigger = Tiger(3.5, "orange")  # => Tiger(3.5,"orange")

# The type doubles as the constructor function for values of that type
sherekhan = typeof(tigger)(5.6, "fire")  # => Tiger(5.6,"fire")

# These struct-style types are called concrete types
# They can be instantiated, but cannot have subtypes.
# The other kind of types is abstract types.

# abstract Name
abstract type Cat end  # just a name and point in the type hierarchy

# Abstract types cannot be instantiated, but can have subtypes.
# For example, Number is an abstract type
subtypes(Number)  # => 2-element Array{Any,1}:
                  # =>  Complex
                  # =>  Real
subtypes(Cat)  # => 0-element Array{Any,1}

# AbstractString, as the name implies, is also an abstract type
subtypes(AbstractString)  # => 4-element Array{Any,1}:
                          # =>  String
                          # =>  SubString
                          # =>  SubstitutionString
                          # =>  Test.GenericString

# Every type has a super type; use the `supertype` function to get it.
typeof(5) # => Int64
supertype(Int64)    # => Signed
supertype(Signed)   # => Integer
supertype(Integer)  # => Real
supertype(Real)     # => Number
supertype(Number)   # => Any
supertype(supertype(Signed))  # => Real
supertype(Any)      # => Any
# All of these type, except for Int64, are abstract.
typeof("fire")      # => String
supertype(String)   # => AbstractString
# Likewise here with String
supertype(SubString)  # => AbstractString

# <: is the subtyping operator
struct Lion <: Cat  # Lion is a subtype of Cat
    maneColor
    roar::AbstractString
end

# You can define more constructors for your type
# Just define a function of the same name as the type
# and call an existing constructor to get a value of the correct type
Lion(roar::AbstractString) = Lion("green", roar)
# This is an outer constructor because it's outside the type definition

struct Panther <: Cat  # Panther is also a subtype of Cat
    eyeColor
    Panther() = new("green")
    # Panthers will only have this constructor, and no default constructor.
end
# Using inner constructors, like Panther does, gives you control
# over how values of the type can be created.
# When possible, you should use outer constructors rather than inner ones.

####################################################
## 6. Multiple-Dispatch
####################################################

# In Julia, all named functions are generic functions
# This means that they are built up from many small methods
# Each constructor for Lion is a method of the generic function Lion.

# For a non-constructor example, let's make a function meow:

# Definitions for Lion, Panther, Tiger
function meow(animal::Lion)
    animal.roar  # access type properties using dot notation
end

function meow(animal::Panther)
    "grrr"
end

function meow(animal::Tiger)
    "rawwwr"
end

# Testing the meow function
meow(tigger)  # => "rawwwr"
meow(Lion("brown", "ROAAR"))  # => "ROAAR"
meow(Panther()) # => "grrr"

# Review the local type hierarchy
Tiger   <: Cat  # => false
Lion    <: Cat  # => true
Panther <: Cat  # => true

# Defining a function that takes Cats
function pet_cat(cat::Cat)
    println("The cat says $(meow(cat))")
end
# => pet_cat (generic function with 1 method)

pet_cat(Lion("42")) # => The cat says 42
try
    pet_cat(tigger) # => ERROR: MethodError: no method matching pet_cat(::Tiger)
catch e
    println(e)
end

# In OO languages, single dispatch is common;
# this means that the method is picked based on the type of the first argument.
# In Julia, all of the argument types contribute to selecting the best method.

# Let's define a function with more arguments, so we can see the difference
function fight(t::Tiger, c::Cat)
    println("The $(t.coatcolor) tiger wins!")
end
# => fight (generic function with 1 method)

fight(tigger, Panther())  # => The orange tiger wins!
fight(tigger, Lion("ROAR")) # => The orange tiger wins!

# Let's change the behavior when the Cat is specifically a Lion
fight(t::Tiger, l::Lion) = println("The $(l.maneColor)-maned lion wins!")
# => fight (generic function with 2 methods)

fight(tigger, Panther())  # => The orange tiger wins!
fight(tigger, Lion("ROAR")) # => The green-maned lion wins!

# We don't need a Tiger in order to fight
fight(l::Lion, c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (generic function with 3 methods)

fight(Lion("balooga!"), Panther())  # => The victorious cat says grrr
try
    fight(Panther(), Lion("RAWR"))  
    # => ERROR: MethodError: no method matching fight(::Panther, ::Lion)
    # => Closest candidates are:
    # =>   fight(::Tiger, ::Lion) at ...
    # =>   fight(::Tiger, ::Cat) at ...
    # =>   fight(::Lion, ::Cat) at ...
    # => ...
catch e
    println(e)
end

# Also let the cat go first
fight(c::Cat, l::Lion) = println("The cat beats the Lion")
# => fight (generic function with 4 methods)

# This warning is because it's unclear which fight will be called in:
try
    fight(Lion("RAR"), Lion("brown", "rarrr"))
    # => ERROR: MethodError: fight(::Lion, ::Lion) is ambiguous. Candidates:
    # =>   fight(c::Cat, l::Lion) in Main at ...
    # =>   fight(l::Lion, c::Cat) in Main at ...
    # => Possible fix, define
    # =>   fight(::Lion, ::Lion)
    # => ...
catch e
    println(e)
end
# The result may be different in other versions of Julia

fight(l::Lion, l2::Lion) = println("The lions come to a tie") 
# => fight (generic function with 5 methods)
fight(Lion("RAR"), Lion("brown", "rarrr"))  # => The lions come to a tie


# Under the hood
# You can take a look at the llvm  and the assembly code generated.

square_area(l) = l * l  # square_area (generic function with 1 method)

square_area(5)  # => 25

# What happens when we feed square_area an integer?
code_native(square_area, (Int32,), syntax = :intel)
	#         .text
	# ; Function square_area {
	# ; Location: REPL[116]:1       # Prologue
	#         push    rbp
	#         mov     rbp, rsp
	# ; Function *; {
	# ; Location: int.jl:54
	#         imul    ecx, ecx      # Square l and store the result in ECX
	# ;}
	#         mov     eax, ecx
	#         pop     rbp           # Restore old base pointer
	#         ret                   # Result will still be in EAX
	#         nop     dword ptr [rax + rax]
	# ;}

code_native(square_area, (Float32,), syntax = :intel)
    #         .text
    # ; Function square_area {
    # ; Location: REPL[116]:1
    #         push    rbp
    #         mov     rbp, rsp
    # ; Function *; {
    # ; Location: float.jl:398
    #         vmulss  xmm0, xmm0, xmm0  # Scalar single precision multiply (AVX)
    # ;}
    #         pop     rbp
    #         ret
    #         nop     word ptr [rax + rax]
    # ;}

code_native(square_area, (Float64,), syntax = :intel)
    #         .text
    # ; Function square_area {
    # ; Location: REPL[116]:1
    #         push    rbp
    #         mov     rbp, rsp
    # ; Function *; {
    # ; Location: float.jl:399
    #         vmulsd  xmm0, xmm0, xmm0  # Scalar double precision multiply (AVX)
    # ;}
    #         pop     rbp
    #         ret
    #         nop     word ptr [rax + rax]
    # ;}

# Note that julia will use floating point instructions if any of the
# arguments are floats.
# Let's calculate the area of a circle
circle_area(r) = pi * r * r     # circle_area (generic function with 1 method)
circle_area(5)  # 78.53981633974483

code_native(circle_area, (Int32,), syntax = :intel)
    #         .text
    # ; Function circle_area {
    # ; Location: REPL[121]:1
    #         push    rbp
    #         mov     rbp, rsp
    # ; Function *; {
    # ; Location: operators.jl:502
    # ; Function *; {
    # ; Location: promotion.jl:314
    # ; Function promote; {
    # ; Location: promotion.jl:284
    # ; Function _promote; {
    # ; Location: promotion.jl:261
    # ; Function convert; {
    # ; Location: number.jl:7
    # ; Function Type; {
    # ; Location: float.jl:60
    #         vcvtsi2sd       xmm0, xmm0, ecx     # Load integer (r) from memory
    #         movabs  rax, 497710928              # Load pi
    # ;}}}}}
    # ; Function *; {
    # ; Location: float.jl:399
    #         vmulsd  xmm1, xmm0, qword ptr [rax] # pi * r
    #         vmulsd  xmm0, xmm1, xmm0            # (pi * r) * r
    # ;}}
    #         pop     rbp
    #         ret
    #         nop     dword ptr [rax]
    # ;}

code_native(circle_area, (Float64,), syntax = :intel)
    #         .text
    # ; Function circle_area {
    # ; Location: REPL[121]:1
    #         push    rbp
    #         mov     rbp, rsp
    #         movabs  rax, 497711048
    # ; Function *; {
    # ; Location: operators.jl:502
    # ; Function *; {
    # ; Location: promotion.jl:314
    # ; Function *; {
    # ; Location: float.jl:399
    #         vmulsd  xmm1, xmm0, qword ptr [rax]
    # ;}}}
    # ; Function *; {
    # ; Location: float.jl:399
    #         vmulsd  xmm0, xmm1, xmm0
    # ;}
    #         pop     rbp
    #         ret
    #         nop     dword ptr [rax + rax]
    # ;}
```

## Further Reading

You can get a lot more detail from the [Julia Documentation](https://docs.julialang.org/)

The best place to get help with Julia is the (very friendly) [Discourse forum](https://discourse.julialang.org/).
