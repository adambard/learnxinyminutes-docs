---
language: Julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
    - ["Pranit Bauva", "http://github.com/pranitbauva1997"]
    - ["Michał Rawlik", "https://github.com/rawlik"]
filename: learnjulia.jl
---

Julia is a new homoiconic functional language focused on technical computing.
While having the full power of homoiconic macros, first-class functions, and
low-level control, Julia is as easy to learn and use as Python.

This is based on Julia 0.6.

```julia
# Single line comments start with a hash (pound) symbol.
#= Multiline comments can be written
   by putting '#=' before the text  and '=#'
   after the text. They can also be nested.
=#

####################################################
## Primitive Datatypes and Operators
####################################################

# Everything in Julia is an expression.

# There are several basic types of numbers.
3 # => 3 (Int64)
3.2 # => 3.2 (Float64)
2 + 1im # => 2 + 1im (Complex{Int64})
2//3 # => 2//3 (Rational{Int64})

# There are mathematical constants.
pi # => π = 3.1415926535897... (Irrational{:π})
e # => e = 2.7182818284590... (Irrational{:e})
# They are printed in a special way, including their symbol, to clearly indicate
# that they are irrational. Their precision depends on context.

# All of the normal infix operators are available.
1 + 1 # => 2
8 - 1 # => 7
10 * 2 # => 20
# No operator implies multiplication, when not ambiguous
2pi # => 6.283185307179586
2(3+2) # => 10
1 / 2π == 1 / (2 * pi) # => true
1 / 2π == 1 / 2 * pi # => false
35 / 5 # => 7.0
5 / 2 # => 2.5 # dividing an Int by an Int always results in a Float
div(5, 2) # => 2 # for a truncated result, use div
5 ÷ 2 # => 2 # or the ÷ operator (\div[TAB] in julia REPL and juno)
5 \ 35 # => 7.0
2 ^ 2 # => 4 # power, not bitwise xor
12 % 10 # => 2
-1 % 10 # => -1 # modulus of negative numbers is different from python

1 / 0 # => Inf
-1 / 0 # => -Inf
0 / 0 # => NaN

# Enforce precedence with parentheses
(1 + 3) * 2 # => 8

# Bitwise Operators
~2 # => -3   # bitwise not
3 & 5 # => 1 # bitwise and
2 | 4 # => 6 # bitwise or
xor(2, 4) # => 6 # bitwise xor
2 ⊻ 4 # also bitwise xor, \veebar[TAB]
2 >>> 1 # => 1 # logical shift right
2 >> 1  # => 1 # arithmetic shift right
2 << 1  # => 4 # logical/arithmetic shift left

# You can use the bits function to see the binary representation of a number.
bits(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bits(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# Boolean values are primitives
true
false

# Boolean operators
!true # => false
!false # => true
1 == 1 # => true
2 == 1 # => false
1 != 1 # => false
2 != 1 # => true
2 ≠ 1 # => true # the utf character ≠ (\ne) works, too
1 < 10 # => true
1 > 10 # => false
2 <= 2 # => true
2 ≤ 2 # => true # \le
2 >= 2 # => true
2 ≥ 2 # => true # \ge
# Comparisons can be chained
1 < 2 < 3 # => true
2 < 3 < 2 # => false

# the negative and positive zeros are equal
-0.0 == 0.0 # => true
# but we can test they are not equivalent
-0.0 === 0.0 # => false
-0.0 ≡ 0.0 # => false # \equiv




#############
## Variables
#############

# You don't declare variables before assigning to them.
some_var = 5 # => 5
some_var # => 5

# Accessing a previously unassigned variable is an error
try
    some_other_var # => UndefVarError(:some_other_var)
catch e
    println(e)
end
# Errors list the line and file they came from, even if it's in the standard
# library. If you built Julia from source, you can look in the folder base
# inside the julia folder to find these files.

# Variable names start with a letter or underscore.
# After that, you can use letters, digits, underscores, and exclamation points.
SomeOtherVar123! = 6 # => 6

# You can also use certain unicode characters
☃ = 8 # => 8
# These are especially handy for mathematical notation
2 * π # => 6.283185307179586 # \pi[TAB] enters π in julia REPL and juno IDE

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
#   By convention, usually the first argument is modified.


# use const to define constants
const constant = 3

# changing the value of a constant is only a warining
constant = 4 # => WARNING: redefining constant constant

# changing the *type* of a constant in an ERROR
try
    constant = 3.14 # => invalid redefinition of constant constant
catch e
    println(e)
end




##########
## Arrays
##########

# 1-dimensional array (a column vector) literals can be written with
# comma-separated values in square brackets
a = [4, 5, 6] # => 3-element Int64 Array: [4, 5, 6]
a[1] # => 4
a[2] # => 5

# semicolon can be used, too
a = [4; 5; 6] # => 3-element Int64 Array: [4, 5, 6]

# use end to access the last element
a[end] # => 6

# you can perform operations on end
a[end - 1] # => 5

# end only works inside square brackets. This won't work
# ind = end # => syntax: unexpected end
# a[ind]

# no index defaults to 1, which is useful when working with
# single-element arrays
a[] # => 4

# Looking out of bounds is a BoundsError
try
    a[0] # => BoundsError: attempt to access 3-element Array{Int64,1}
         #    at index [0]
catch e
    println(e)
end

# You can index with an Array of integers
a[[1, 3]] # => [4, 6]

# Or with an Array of Boolean
a[[true, false, true]] # => [4, 6]

# You can initialize arrays from ranges
collect(1:5) # => 5-element Int64 Array: [1,2,3,4,5]

# Be careful, [1:5] is an array of ranges!
notwhatithought = [1:5] # => 1-element Array{UnitRange{Int64},1}: 1:4
notwhatithought[1] # => 1:5

# Curiously enough:
discouraged = [1:5;] # => Int64[1,2,3,4,5]
discouraged[1] # => 1
# but it is discouraged.
# It is because ; is a concatenation operator in square bracket context.
[1:4; 48; 100:100:300] # => Int64[1,2,3,4,48,100,200,300]
# In tripple slices, the middle is the step

# You can look at arrays with slice syntax.
a[1:3] # => [4,5,6]
a[2:end] # => [5,6]
a[end:-1:1] # => [6,5,4]

# Arrays are mutable
a[1] = 0
a # => [0, 5, 6]

# 2-dimensional arrays use space-separated values and semicolon-separated rows.
matrix = [1 2; 3 4] # => 2x2 Int64 Array: [1 2; 3 4]
# or Multiline
matrix = [1 2
          3 4] # => 2x2 Int64 Array: [1 2; 3 4]

# Arrays of a particular Type
b = Int8[4, 5, 6] # => 3-element Int8 Array: [4, 5, 6]

# You can multiply Arrays by a number
a * 10 # => [0, 50, 60]
10 * a # => [0, 50, 60]
10a # => [0, 50, 60]

# Julia is designed with linear algebra in mind. * is a matrix multiplication
# operator
matrix * [10, 20] # => Int64[50, 110]

# Julia distinguishes row and column vectors
# 1D Arrays (Vectors), are column
x = [1, 2] # => 2-element Array{Int64,1}:
           #     1
           #     2

y = [1 2] # => 1×2 Array{Int64,2}:
          #     1  2

y * x # => 5
x * y # 2×2 Array{Int64,2}:
      #  1  2
      #  2  4

# ' is Hermitian transpose
matrix' # => Int64[1 3; 2 4]

try
    [1, 2] * [3, 4] # => DimensionMismatch("Cannot multiply two vectors")
catch e
    println(e)
end

# Use the so-called dot operators to perform operations element-wise
[1, 2] .* [3, 4] # => Int64[3, 8]

# Dot operators support broadcasting - singleton dimensions are automatically
# expanded
[1 2; 3 4] .* [10, 20] # => 2×2 Array{Int64,2}:
                       #    10  20
                       #    60  80

# The above has the same efect as this
[1 2; 3 4] .* [10 10; 20 20] # => 2×2 Array{Int64,2}:
                             #    10  20
                             #    60  80

# Assignment can be dotted, too
a .= 7
a # => [7, 7, 7]

b = [1, 2, 3]
c = [10, 20, 30]

# dotted operations are internally fused into one loop and are very efficient
a .= b .* c # => [10, 40, 90]
a # => [10, 40, 90]

# @. adds a dot to every operator (and function, but more on that later)
@. a = b * c # => [10, 40, 90]
a # => [10, 40, 90]

a[a.>20] # => [40, 90]




###########
## Strings
###########

# Strings are created with "
s = "This is a string."

"""You can
have multiline strings, too."""

# Character literals are written with '
'a'

# Strings can be indexed like an array of characters
s[1] # => 'T', a Char
s[1:4] # => "This", a String

# Indexing a unicode string in a wrong place will throw an arror,
# rather than returning gibberish
utf8string = "Θεσσαλονίκη"
utf8string[1] # => 'Θ'
try
    utf8string[2] # => UnicodeError: invalid character index
catch e
    println(e)
end
utf8string[3] # => 'ε'

# Iterating over Strings works as expected, even for Strings with
# unicode characters
[ c for c in utf8string ] # => 11-element Array{Char,1}:
                          # ['Θ','ε','σ','σ','α','λ','ο','ν','ί','κ','η']


# Strings are immutable
try
    s[1] = "T" # => MethodError: no method matching
               #    setindex!(::String, ::String, ::Int64)
catch e
    println(e)
end


# $ can be used for string interpolation:
"pi is $pi" # => "pi is π = 3.1415926535897..."
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# You can put any Julia expression inside the parentheses.

# Another way to format strings is the printf macro.
@printf "%g is less than %f" 4.5 5.3 # prints "4.5 is less than 5.300000"

# and sprintf, which returns a String
@sprintf "%g is less than %f" 4.5 5.3 # => "4.5 is less than 5.300000"

# Printing is easy
print("Print without newline... ")
println("Print with newline at the end.")

# String can be compared lexicographically
"good" > "bye" # => true
"good" == "good" # => true
"1 + 2 = 3" == "1 + 2 = $(1+2)" # => true

# You can concatenate strings with * and repeat with ^
"one" * "two" # => "onetwo"
"one"^3 # => "oneoneone"

# r"" syntax crates a regular expression
ismatch(r"^\s*(?:#|$)", "not a comment") # => false
ismatch(r"^\s*(?:#|$)", "# a comment") # => true

# b"" is for raw byte arrays
b"abc" # => 3-element Array{UInt8,1}: [0x61, 0x62, 0x63]




##############
# Collections
##############

a = [] # => Any[]
# Any is the supertype of all types  (more on that later).
# This means, that this vector can hold anything.

# Add stuff to the end of a with push! (single element)
# and append! (another collection)
push!(a, 1)     # => Any[1]
push!(a, 2)     # => Any[1,2]
push!(a, 4)     # => Any[1,2,4]
push!(a, 3)     # => Any[1,2,4,3]
append!(a, ["six", π]) # => Any[1,2,4,3,"six",π = 3.14…]
# By convention, function names that end in exclamations points indicate that
# they modify their arguments. Usually it is the first argument being modified.

# Remove from the end with pop
pop!(a) # => π = 3.14…
a # => [1,2,4,3,"six"]

# we also have shift and unshift
shift!(a) # => 1
a # => [2,4,3,"six"]
unshift!(a, 7) # => [7,2,4,3,"six"]

# Remove elements from an array by index with splice!
splice!(a, 2) # => 2
a # => [7,4,3,"six"]

# Check for existence in a list with in
in(1, a) # => false
3 in a # => true

# Examine the length with length
length(a) # => 4

# Sorting
arr = [5,4,6] # => 3-element Int64 Array: [5,4,6]
sort(arr) # => [4,5,6]
arr # => [5,4,6]
sort!(arr) # => [4,5,6]
arr # => [4,5,6]


# Tuples are immutable.
tup = (1, 2, 3) # => (1,2,3) # an (Int64,Int64,Int64) tuple.
tup[1] # => 1
try:
    tup[1] = 3 # => ERROR: MethodError: no method matching
               #    setindex!(::Tuple{Int64,Int64,Int64}, ::Int64, ::Int64)
catch e
    println(e)
end

# Many list functions also work on tuples
length(tup) # => 3
tup[1:2] # => (1,2)
2 in tup # => true

# You can unpack tuples into variables
a, b, c = (1, 2, 3) # => (1,2,3)
a # => 1
b # => 2
c # => 3

# Tuples are created even if you leave out the parentheses
d, g, h = 4, 5, 6 # => (4,5,6)

# A 1-element tuple is distinct from the value it contains
(1,) == 1 # => false
(1) == 1 # => true

# Look how easy it is to swap two values
g, d = d, g  # => (4,5)
g # => 4
d # => 5


# Dictionaries store mappings
empty_dict = Dict() # => Dict{Any,Any} with 0 entries

# You can create a dictionary using a literal
filled_dict = Dict("one"=> 1, "two"=> 2, "three"=> 3)
# => Dict{String,Int64}

# Look up values with []
filled_dict["one"] # => 1

# Get all keys
keys(filled_dict) # => Base.KeyIterator for a Dict{String,Int64} with 3 entries.
                  #    Keys:
                  #     "two"
                  #     "one"
                  #     "three"
# Note - dictionary keys are not sorted or in the order you inserted them.

# Get all values
values(filled_dict) # => Base.ValueIterator for a Dict{String,Int64} with
                    #    3 entries. Values:
                    #     2
                    #     1
                    #     3
# Note - Same as above regarding key ordering.

# Check for existence of keys in a dictionary with in, haskey
in(("one" => 1), filled_dict) # => true
("two" => 3) in filled_dict # => false
haskey(filled_dict, "one") # => true
haskey(filled_dict, 1) # => false

# Trying to look up a non-existent key will raise an error
try
    filled_dict["four"] # => KeyError: key "four" not found
catch e
    println(e)
end

# Use the get method to avoid that error by providing a default value
# get(dictionary,key,default_value)
get(filled_dict, "one", 4) # => 1
get(filled_dict, "four", 4) # => 4

# Use Sets to represent collections of unordered, unique values
empty_set = Set() # => Set{Any}[]
# Initialize a set with values
filled_set = Set([1,2,2,3,4]) # => Set([4, 2, 3, 1])

# Add more values to a set
push!(filled_set, 5) # => Set{Int64}([4,2,3,5,1])
filled_set # => Set{Int64}([4,2,3,5,1])

# Check if the values are in the set
2 in filled_set # => true
10 in filled_set # => false

# There are functions for set intersection, union, and difference.
other_set = Set([3, 4, 5, 6]) # => Set{Int64}([4,3,5,6])
intersect(filled_set, other_set) # => Set{Int64}([4,3,5])
filled_set ∩ other_set # => Set{Int64}([4,3,5]) # The same with ∩ (\cap)
union(filled_set, other_set) # => Set{Int64}([4,2,3,5,6,1])
filled_set ∪ other_set # => Set{Int64}([4,2,3,5,6,1]) # The same ∪ (\cup)
setdiff(Set([1,2,3,4]), Set([2,3,5])) # => Set{Int64}([4,1])

# Set operations work on arrays, too
[1,2,3,4] ∩ [2,3,4] # => 3-element Array{Int64,1}: [2,3,4]




####################################################
## Control Flow
####################################################

# Let's make a variable
some_var = 5

# Here is an if statement. Indentation is not meaningful in Julia.
if some_var > 10
    println("some_var is totally bigger than 10.")
elseif some_var < 10    # This elseif clause is optional.
    println("some_var is smaller than 10.")
else                    # The else clause is optional too.
    println("some_var is indeed 10.")
end
# => prints "some var is smaller than 10"

# if statement can return variables
result = if some_var > 10
    "greater than 10"
else
    "smaller of equal 10"
end
result # => smaller of equal 10

# There is the ternary operator
result = some_var > 5 ? "greater than 5" : "smaller of equal 5"
result # => smaller of equal 5

# For loops iterate over iterables.
# Iterable types include Range, Array, Set, Dict, and AbstractString.
for animal = ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# You can use 'in' instead of '='.
for i in 1:3
    println("i is now $i")
end
# prints:
#    i is now 1
#    i is now 2
#    i is now 3

for a in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    println("$(a[1]) is a $(a[2])")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

for (k,v) in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    println("$k is a $v")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# While loops loop while a condition is true
x = 0
while x < 4
    println(x)
    x += 1  # Shorthand for x = x + 1
end
# prints:
#   0
#   1
#   2
#   3

# Handle exceptions with a try/catch block
try
   error("help")
catch e
   println("caught it $e")
end
# => caught it ErrorException("help")




####################################################
## Functions
####################################################

# The keyword 'function' creates new functions
#function name(arglist)
#  body...
#end
function add(x, y)
    println("x is $x and y is $y")

    # Functions return the value of their last statement
    x + y
end

add(5, 6) # => 11,
# prints out "x is 5 and y is 6"

# By the way, every binary operator is a function, too, so:
+(5, 6) # => 11

# Compact assignment of functions
add(x, y) = x + y # => "f (generic function with 1 method)"
add(3, 4) # => 7

# Function can also return multiple values as tuple
add_subtract(x, y) = x + y, x - y
add_subtract(3, 4) # => (7, -1)

# You can define functions that take a variable number of
# positional arguments
function varargs(args...)
    return args
    # use the keyword return to return anywhere in the function
end
# => varargs (generic function with 1 method)

varargs(1,2,3) # => (1,2,3)

# The ... is called a splat.
# We just used it in a function definition.
# It can also be used in a function call,
# where it will splat an Array or Tuple's contents into the argument list.
add([5,6]...) # this is equivalent to add(5,6)

x = (5,6)     # => (5,6)
add(x...)     # this is equivalent to add(5,6)

# You can define functions with optional positional arguments
function defaults(a, b, x = 5, y = 6)
    return "$a $b and $x $y"
end

defaults('h','g') # => "h g and 5 6"
defaults('h','g','j') # => "h g and j 6"
defaults('h','g','j','k') # => "h g and j k"
try
    defaults('h') # => ERROR: no method defaults(Char,)
    defaults() # => ERROR: no methods defaults()
catch e
    println(e)
end

# You can define functions that take keyword arguments,
# specified after a semicolon
function keyword_args(; k1 = 4, name2 = "hello") # note the ;
    return Dict("k1"=>k1, "name2"=>name2)
end

keyword_args(name2 = "ness") # => ["name2"=>"ness","k1"=>4]
keyword_args(k1 = "mine") # => ["k1"=>"mine","name2"=>"hello"]
keyword_args() # => ["name2"=>"hello","k1"=>4]

# You can combine all kinds of arguments in the same function
function all_the_args(normal_arg, optional_positional_arg=2; keyword_arg="foo")
    println("normal arg: $normal_arg")
    println("optional arg: $optional_positional_arg")
    println("keyword arg: $keyword_arg")
end

all_the_args(1, 3, keyword_arg=4)
# prints:
#   normal arg: 1
#   optional arg: 3
#   keyword arg: 4

# Julia has first class functions
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end

# This is "stabby lambda syntax" for creating anonymous functions
(x -> x > 2)(3) # => true

# This function is identical to create_adder implementation above.
function create_adder(x)
    y -> x + y
end

# You can also name the internal function, if you want
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end

add_10 = create_adder(10)
add_10(3) # => 13


# There are built-in higher order functions
map(iszero, [1, 0, 3, 0]) # => [false, true, false, true]
filter(x -> x > 5, [3, 4, 5, 6, 7]) # => [6, 7]

# The "do" block creates an anonymous function and passes is as the first
# argument.
result = map([1, 2, 3, 4, 5, 6]) do x
    x % 2 == 0
end
result # => [false, true, false, true, false, true]
result == map(x -> x % 2 == 0, [1, 2, 3, 4, 5, 6]) # => true

# Negating a function returns a function with its return value negated
map(!iszero, [1, 0, 3, 0]) # => [true, false, true, false]

# The binary operator ∘ (\circ) combines functions
map( (-) ∘ √, [1, 4, 16]) # => [-1.00, -2.00, -4.00]
# √, \sqrt[TAB], is the square root. It can also be used without parentheses
√2 # => 1.41…

# We can use list comprehensions for nicer maps
[add_10(i) for i=[1, 2, 3]] # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]] # => [11, 12, 13]

# List comprehensions work in more dimensions, too
[x+y for x in 1:2, y in 10:10:30] # => 2×3 Array{Int64,2}:
                                  #     11  21  31
                                  #     12  22  32


# Function followed by a dot acts element-wise
isfinite.([1, 1/0, NaN]) # => [true, false, false]

x = rand(3)
abs.(sin.(x).^2 .+ cos.(x).^2 .- 1) .≤ eps(Float64) # => [true, true, true]
# Many dot operations are automatically fused into a single loop.

# @. adds a dot to every function and operator
@. abs(sin(x)^2 + cos(x)^2 - 1) ≤ eps(Float64) # => [true, true, true]




####################################################
## Types
####################################################

# Julia has a type system.
# Every value has a type; variables do not have types themselves.
# You can use the `typeof` function to get the type of a value.
typeof(5) # => Int64

# Types are first-class values
typeof(Int64) # => DataType
typeof(DataType) # => DataType
# DataType is the type that represents types, including itself.

# Types are used for documentation, optimizations, and dispatch.
# They are not statically checked.

# Users can define types
struct Tiger
  taillength::Float64
  coatcolor # not including a type annotation is the same as `::Any`
end

# The default constructor's arguments are the properties
# of the type, in the order they are listed in the definition
tigger = Tiger(3.5, "orange") # => Tiger(3.5,"orange")

# The type doubles as the constructor function for values of that type
sherekhan = typeof(tigger)(5.6, "fire") # => Tiger(5.6,"fire")

# These struct-style types are called concrete types
# They can be instantiated, but cannot have subtypes.
# The other kind of types is abstract types.

# abstract type Name
abstract type Cat  # just a name and point in the type hierarchy
end

# Abstract types cannot be instantiated, but can have subtypes.
# For example, Number is an abstract type
subtypes(Number) # => 2-element Array{Union{DataType, UnionAll},1}:
                 #     Complex
                 #     Real
subtypes(Cat) # => 0-element Array{Union{DataType, UnionAll},1}

# AbstractString, as the name implies, is also an abstract type
subtypes(AbstractString) # => 6-element Array{Union{DataType, UnionAll},1}:
                         #     Base.SubstitutionString
                         #     Base.Test.GenericString
                         #     DirectIndexString
                         #     RevString
                         #     String
                         #     SubString
# Every type has a super type; use the `supertype` function to get it.
typeof(5) # => Int64
supertype(Int64) # => Signed
supertype(Signed) # => Integer
supertype(Integer) # => Real
supertype(Real) # => Number
supertype(Number) # => Any
supertype(supertype(Signed)) # => Real
supertype(Any) # => Any
# All of these type, except for Int64, are abstract.
typeof("fire") # => String
supertype(String) # => AbstractString
supertype(AbstractString) # => Any

# <: is the subtyping operator
type Lion <: Cat # Lion is a subtype of Cat
  mane_color
  roar::AbstractString
end

# You can define more constructors for your type
# Just define a function of the same name as the type
# and call an existing constructor to get a value of the correct type
Lion(roar::AbstractString) = Lion("green", roar)
# This is an outer constructor because it's outside the type definition

type Panther <: Cat # Panther is also a subtype of Cat
  eye_color
  Panther() = new("green")
  # Panthers will only have this constructor, and no default constructor.
  # new() is a spetial function available in inner constructors, that creates
  # objects of the block's type.
end
# Using inner constructors, like Panther does, gives you control
# over how values of the type can be created.
# When possible, you should use outer constructors rather than inner ones.




####################################################
## Multiple-Dispatch
####################################################

# In Julia, all named functions are generic functions
# This means that they are built up from many small methods
# Each constructor for Lion is a method of the generic function Lion.

# For a non-constructor example, let's make a function meow:

# Definitions for Lion, Panther, Tiger
function meow(animal::Lion)
    animal.roar # access type properties using dot notation
end

function meow(animal::Panther)
    "grrr"
end

function meow(animal::Tiger)
    "rawwwr"
end

# Testing the meow function
meow(tigger) # => "rawwr"
meow(Lion("brown","ROAAR")) # => "ROAAR"
meow(Panther()) # => "grrr"

# Review the local type hierarchy
issubtype(Tiger, Cat) # => false
issubtype(Lion, Cat) # => true
issubtype(Panther, Cat) # => true

# Defining a function that takes Cats
function pet_cat(cat::Cat)
    println("The cat says $(meow(cat))")
end

pet_cat(Lion("42")) # => prints "The cat says 42"
try
    pet_cat(tigger) # => ERROR: no method matching pet_cat(::Tiger)
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

fight(tigger, Panther()) # => prints The orange tiger wins!
fight(tigger, Lion("ROAR")) # => prints The orange tiger wins!

# Let's change the behavior when the Cat is specifically a Lion
fight(t::Tiger, l::Lion) = println("The $(l.mane_color)-maned lion wins!")
# => fight (generic function with 2 methods)

fight(tigger, Panther()) # => prints The orange tiger wins!
fight(tigger, Lion("ROAR")) # => prints The green-maned lion wins!

# We don't need a Tiger in order to fight
fight(l::Lion, c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (generic function with 3 methods)

fight(Lion("balooga!"), Panther()) # => prints The victorious cat says grrr

# review the methods we created
methods(fight) # => 3 methods for generic function "fight":
               #     fight(t::Tiger, l::Lion)
               #     fight(t::Tiger, c::Cat)
               #     fight(l::Lion, c::Cat)

try
    fight(Panther(), Lion("RAWR")) # => MethodError: no method matching
                                   #    fight(::Panther, ::Lion)
catch e
    println(e)
end

# Also let the cat go first
fight(c::Cat, l::Lion) = println("The cat beats the Lion")
try
    fight(Lion("RAR"), Lion("brown", "rarrr"))
    # => MethodError: fight(::Lion, ::Lion) is ambiguous. Candidates:
    #    fight(c::Cat, l::Lion)
    #    fight(l::Lion, c::Cat)
    #  Possible fix, define
    #    fight(::Lion, ::Lion)
catch e
    println(e)
end

fight(l1::Lion, l2::Lion) = println("The lions come to a tie")
fight(Lion("RAR"), Lion("brown", "rarrr")) # => prints The lions come to a tie




#########
# Macros
#########

v = 9

@show 10 * √v # => 30.0
# prints "10 * √v = 30.0"

@time inv(rand(100, 100)) # => 100x100 Array{Float64,2}...
# prints "  0.306575 seconds (76.98 k allocations: 4.521 MiB)"

@time inv(rand(100, 100)) # => 100x100 Array{Float64,2}...
# prints "    0.001164 seconds (19 allocations: 286.781 KiB)"
# The second time is faster. The first time the functions were compiled.

a = rand(10)
pointer(a) # => Ptr{Float64} @0x000000010d97fb10
# Slicing produces a new Array
pointer(a[1:10]) == pointer(a) # => false
# The @view macro produces a SubArray sharing memory with the original
pointer(@view a[1:10]) == pointer(a) # => true




#################
# Under the hood
#################
# You can take a look at the llvm  and the assembly code generated.

square_area(l) = l * l      # square_area (generic function with 1 method)

square_area(5) # => 25

# What happens when we feed square_area an integer?
code_native(square_area, (Int32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1              # Prologue
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    movsxd  RAX, EDI        # Fetch l from memory?
	#	    imul    RAX, RAX        # Square l and store the result in RAX
	#	    pop RBP                 # Restore old base pointer
	#	    ret                     # Result will still be in RAX

code_native(square_area, (Float32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulss  XMM0, XMM0, XMM0  # Scalar single precision multiply (AVX)
	#	    pop RBP
	#	    ret

code_native(square_area, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vmulsd  XMM0, XMM0, XMM0 # Scalar double precision multiply (AVX)
	#	    pop RBP
	#	    ret
	#
# Note that julia will use floating point instructions if any of the
# arguments are floats.
# Let's calculate the area of a circle
circle_area(r) = pi * r * r     # circle_area (generic function with 1 method)
circle_area(5)                  # 78.53981633974483

code_native(circle_area, (Int32,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	Source line: 1
	#	    vcvtsi2sd   XMM0, XMM0, EDI          # Load integer (r) from memory
	#	    movabs  RAX, 4593140240              # Load pi
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]  # pi * r
	#	    vmulsd  XMM0, XMM0, XMM1             # (pi * r) * r
	#	    pop RBP
	#	    ret
	#

code_native(circle_area, (Float64,))
	#	    .section    __TEXT,__text,regular,pure_instructions
	#	Filename: none
	#	Source line: 1
	#	    push    RBP
	#	    mov RBP, RSP
	#	    movabs  RAX, 4593140496
	#	Source line: 1
	#	    vmulsd  XMM1, XMM0, QWORD PTR [RAX]
	#	    vmulsd  XMM0, XMM1, XMM0
	#	    pop RBP
	#	    ret
	#
```

## Further Reading

You can get a lot more detail from [The Julia Manual](http://docs.julialang.org/en/latest/#Manual-1)

The best place to get help with Julia is the (very friendly) [Discourse forum](https://discourse.julialang.org/).
