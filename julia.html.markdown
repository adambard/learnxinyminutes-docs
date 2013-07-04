---
language: julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
filename: learnjulia.jl
---

Julia is a new homoiconic functional language focused on technical computing.
While having the full power of homoiconic macros, first-class functions, and low-level control, Julia is as easy to learn and use as Python.

This is based on the current development version of Julia, as of June 29th, 2013.

```ruby

# Single line comments start with a hash.

####################################################
## 1. Primitive Datatypes and Operators
####################################################

# Everything in Julia is a expression.

# You have numbers
3 #=> 3 (Int64)
3.2 #=> 3.2 (Float64)
2 + 1im #=> 2 + 1im (Complex{Int64})
2//3 #=> 2//3 (Rational{Int64})

# Math is what you would expect
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7.0
5 \ 35 #=> 7.0
5 / 2 #=> 2.5
div(5, 2) #=> 2
2 ^ 2 #=> 4 # power, not bitwise xor
12 % 10 #=> 2

# Enforce precedence with parentheses
(1 + 3) * 2 #=> 8

# Bitwise Operators
~2 #=> -3   # bitwise not
3 & 5 #=> 1 # bitwise and
2 | 4 #=> 6 # bitwise or
2 $ 4 #=> 6 # bitwise xor
2 >>> 1 #=> 1 # logical shift right
2 >> 1  #=> 1 # arithmetic shift right
2 << 1  #=> 4 # logical/arithmetic shift left

# You can use the bits function to see the binary representation of a number.
bits(12345)
#=> "0000000000000000000000000000000000000000000000000011000000111001"
bits(12345.0)
#=> "0100000011001000000111001000000000000000000000000000000000000000"

# Boolean values are primitives
true
false

# Boolean operators
!true #=> false
!false #=> true
1 == 1 #=> true
2 == 1 #=> false
1 != 1 #=> false
2 != 1 #=> true
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true
# Comparisons can be chained
1 < 2 < 3 #=> true
2 < 3 < 2 #=> false

# Strings are created with "
"This is a string."

# Character literals written with '
'a'

# A string can be treated like a list of characters
"This is a string"[1] #=> 'T' # Julia indexes from 1

# $ can be used for string interpolation:
"2 + 2 = $(2 + 2)" #=> "2 + 2 = 4"
# You can put any Julia expression inside the parenthesis.

# Another way to format strings is the printf macro.
@printf "%d is less than %f" 4.5 5.3 # 5 is less than 5.300000

####################################################
## 2. Variables and Collections
####################################################

# Printing is pretty easy
println("I'm Julia. Nice to meet you!")

# No need to declare variables before assigning to them.
some_var = 5 #=> 5 
some_var #=> 5

# Accessing a previously unassigned variable is an error
try
  some_other_var #=> ERROR: some_other_var not defined
catch e
    println(e)
end

# Variable name start with a letter. You can use uppercase letters, digits,
# and exclamation points as well after the initial alphabetic character.
SomeOtherVar123! = 6 #=> 6

# You can also use unicode characters
☃ = 8 #=> 8

# A note on naming conventions in Julia:
#
# * Names of variables are in lower case, with word separation indicated by
#   underscores ('\_').
#
# * Names of Types begin with a capital letter and word separation is shown
#   with CamelCase instead of underscores.
#
# * Names of functions and macros are in lower case, without underscores.
#
# * Functions that modify their inputs have names that end in !. These
#   functions are sometimes called mutating functions or in-place functions.

# Arrays store a sequence of values indexed by integers 1 through n:
a = Int64[] #=> 0-element Int64 Array

# 1-dimensional array literals can be written with comma-separated values.
b = [4, 5, 6] #=> 3-element Int64 Array: [4, 5, 6]
b[1] #=> 4
b[end] #=> 6

# 2-dimentional arrays use space-separated values and semicolon-separated rows.
matrix = [1 2; 3 4] #=> 2x2 Int64 Array: [1 2; 3 4]

# Add stuff to the end of a list with push! and append!
push!(a,1)     #=> [1]
push!(a,2)     #=> [1,2]
push!(a,4)     #=> [1,2,4]
push!(a,3)     #=> [1,2,4,3]
append!(a,b) #=> [1,2,4,3,4,5,6]

# Remove from the end with pop
pop!(a)        #=> 6 and b is now [4,5]

# Let's put it back
push!(b,6)   # b is now [4,5,6] again.

a[1] #=> 1 # remember that Julia indexes from 1, not 0!

# end is a shorthand for the last index. It can be used in any
# indexing expression
a[end] #=> 6

# Function names that end in exclamations points indicate that they modify
# their argument.
arr = [5,4,6] #=> 3-element Int64 Array: [5,4,6]
sort(arr) #=> [4,5,6]; arr is still [5,4,6]
sort!(arr) #=> [4,5,6]; arr is now [4,5,6]

# Looking out of bounds is a BoundsError
try
    a[0] #=> ERROR: BoundsError() in getindex at array.jl:270
    a[end+1] #=> ERROR: BoundsError() in getindex at array.jl:270
catch e
    println(e)
end

# Errors list the line and file they came from, even if it's in the standard
# library. If you built Julia from source, you can look in the folder base
# inside the julia folder to find these files.

# You can initialize arrays from ranges
a = [1:5] #=> 5-element Int64 Array: [1,2,3,4,5]

# You can look at ranges with slice syntax.
a[1:3] #=> [1, 2, 3]
a[2:] #=> [2, 3, 4, 5]

# Remove arbitrary elements from a list with splice!
arr = [3,4,5]
splice!(arr,2) #=> 4 ; arr is now [3,5]

# Concatenate lists with append!
b = [1,2,3]
append!(a,b) # Now a is [1, 3, 4, 5, 1, 2, 3]

# Check for existence in a list with contains
contains(a,1) #=> true

# Examine the length with length
length(a) #=> 7

# Tuples are immutable.
tup = (1, 2, 3) #=>(1,2,3) # an (Int64,Int64,Int64) tuple.
tup[1] #=> 1
try:
    tup[0] = 3 #=> ERROR: no method setindex!((Int64,Int64,Int64),Int64,Int64)
catch e
    println(e)
end

# Many list functions also work on tuples
length(tup) #=> 3
tup[1:2] #=> (1,2)
contains(tup,2) #=> true

# You can unpack tuples into variables
a, b, c = (1, 2, 3) #=> (1,2,3)  # a is now 1, b is now 2 and c is now 3

# Tuples are created by default if you leave out the parentheses
d, e, f = 4, 5, 6 #=> (4,5,6)

# Now look how easy it is to swap two values
e, d = d, e  #=> (5,4) # d is now 5 and e is now 4


# Dictionaries store mappings
empty_dict = Dict() #=> Dict{Any,Any}()

# Here is a prefilled dictionary
filled_dict = ["one"=> 1, "two"=> 2, "three"=> 3]
# => Dict{ASCIIString,Int64}

# Look up values with []
filled_dict["one"] #=> 1

# Get all keys
keys(filled_dict)
#=> KeyIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Note - dictionary keys are not sorted or in the order you inserted them.

# Get all values 
values(filled_dict)
#=> ValueIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Note - Same as above regarding key ordering.

# Check for existence of keys in a dictionary with contains, haskey
contains(filled_dict, ("one", 1)) #=> true
contains(filled_dict, ("two", 3)) #=> false
haskey(filled_dict, "one") #=> true
haskey(filled_dict, 1) #=> false

# Trying to look up a non-existing key will raise an error
try
    filled_dict["four"] #=> ERROR: key not found: four in getindex at dict.jl:489
catch e
    println(e)
end

# Use get method to avoid the error
# get(dictionary,key,default_value)
get(filled_dict,"one",4) #=> 1
get(filled_dict,"four",4) #=> 4

# Sets store sets
empty_set = Set() #=> Set{Any}()
# Initialize a set with a bunch of values
filled_set = Set(1,2,2,3,4) #=> Set{Int64}(1,2,3,4)

# Add more items to a set
add!(filled_set,5) #=> Set{Int64}(5,4,2,3,1)

# There are functions for set intersection, union, and difference.
other_set = Set(3, 4, 5, 6) #=> Set{Int64}(6,4,5,3)
intersect(filled_set, other_set) #=> Set{Int64}(3,4,5)
union(filled_set, other_set) #=> Set{Int64}(1,2,3,4,5,6)
setdiff(Set(1,2,3,4),Set(2,3,5)) #=> Set{Int64}(1,4)

# Check for existence in a set with contains 
contains(filled_set,2) #=> true
contains(filled_set,10) #=> false


####################################################
## 3. Control Flow
####################################################

# Let's make a variable
some_var = 5

# Here is an if statement. Indentation is NOT meaningful in Julia.
# prints "some var is smaller than 10"
if some_var > 10
    println("some_var is totally bigger than 10.")
elseif some_var < 10    # This elseif clause is optional.
    println("some_var is smaller than 10.")
else                    # The else clause is optional too.
    println("some_var is indeed 10.")
end


# For loops iterate over iterables, such as ranges, lists, sets, dicts, strings.

for animal=["dog", "cat", "mouse"]
    # You can use $ to interpolate into strings
    println("$animal is a mammal")
end
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# You can use in instead of =, if you want.
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end

for a in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$(a[1]) is $(a[2])")
end

for (k,v) in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$k is $v")
end


# While loops go until a condition is no longer met.
# prints:
#   0
#   1
#   2
#   3
x = 0
while x < 4
    println(x)
    x += 1  # Shorthand for x = x + 1
end

# Handle exceptions with a try/except block
try
   error("help")
catch e
   println("caught it $e")
end
#=> caught it ErrorException("help")


####################################################
## 4. Functions
####################################################

# Use the keyword function to create new functions
function add(x, y)
    println("x is $x and y is $y")

    # Functions implicitly return the value of their last statement
    x + y
end

add(5, 6) #=> 11 after printing out "x is 5 and y is 6"

# You can define functions that take a variable number of
# positional arguments
function varargs(args...)
    return args
end

varargs(1,2,3) #=> (1,2,3)

# The ... is called a splat.
# It can also be used in a fuction call
# to splat a list or tuple out to be the arguments
Set([1,2,3])    #=> Set{Array{Int64,1}}([1,2,3]) # produces a Set of Arrays
Set([1,2,3]...) #=> Set{Int64}(1,2,3) # this is equivalent to Set(1,2,3)

x = (1,2,3)     #=> (1,2,3)
Set(x)          #=> Set{(Int64,Int64,Int64)}((1,2,3)) # a Set of Tuples
Set(x...)       #=> Set{Int64}(2,3,1)


# You can define functions with optional positional arguments
function defaults(a,b,x=5,y=6)
    return "$a $b and $x $y"
end

defaults('h','g') #=> "h g and 5 6"
defaults('h','g','j') #=> "h g and j 6"
defaults('h','g','j','k') #=> "h g and j k"
try
    defaults('h') #=> ERROR: no method defaults(Char,)
    defaults() #=> ERROR: no methods defaults()
catch e
println(e)
end

# You can define functions that take keyword arguments
function keyword_args(;k1=4,name2="hello") # note the ;
    return ["k1"=>k1,"name2"=>name2]
end 

keyword_args(name2="ness") #=> ["name2"=>"ness","k1"=>4]
keyword_args(k1="mine") #=> ["k1"=>"mine","name2"=>"hello"]
keyword_args() #=> ["name2"=>"hello","k2"=>4]

# You can also do both at once
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

# or equivalently
function create_adder(x)
    y -> x + y
end

# you can also name the internal function, if you want
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end

add_10 = create_adder(10)
add_10(3) #=> 13

# The first two inner functions above are anonymous functions
(x -> x > 2)(3) #=> true

# There are built-in higher order functions
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# We can use list comprehensions for nice maps and filters
[add_10(i) for i=[1, 2, 3]] #=> [11, 12, 13]
[add_10(i) for i in [1, 2, 3]] #=> [11, 12, 13]

####################################################
## 5. Types and Multiple-Dispatch 
####################################################

# Type definition
type Tiger
  taillength::Float64
  coatcolor # no type annotation is implicitly Any
end
# default constructor is the properties in order
# so, Tiger(taillength,coatcolor)

# Type instantiation
tigger = Tiger(3.5,"orange") # the type doubles as the constructor function

# Abtract Types
abstract Cat # just a name and point in the type hierarchy

# * types defined with the type keyword are concrete types; they can be
#   instantiated
#
# * types defined with the abstract keyword are abstract types; they can
#   have subtypes.
#
# * each type has one supertype; a supertype can have zero or more subtypes.

type Lion <: Cat # Lion is a subtype of Cat
  mane_color
  roar::String
end

type Panther <: Cat # Panther is also a subtype of Cat
  eye_color
  Panther() = new("green")
  # Panthers will only have this constructor, and no default constructor.
end

# Multiple Dispatch

# In Julia, all named functions are generic functions
# This means that they are built up from many small methods
# For example, let's make a function meow:
function meow(cat::Lion)
  cat.roar # access properties using dot notation
end

function meow(cat::Panther)
  "grrr"
end

function meow(cat::Tiger)
  "rawwwr"
end

meow(tigger) #=> "rawwr"
meow(Lion("brown","ROAAR")) #=> "ROAAR"
meow(Panther()) #=> "grrr"

function pet_cat(cat::Cat)
  println("The cat says $(meow(cat))")
end

try
    pet_cat(tigger) #=> ERROR: no method pet_cat(Tiger,)
catch e
    println(e)
end

pet_cat(Lion(Panther(),"42")) #=> prints "The cat says 42"

```

## Further Reading

You can get a lot more detail from [The Julia Manual](http://docs.julialang.org/en/latest/manual/)

