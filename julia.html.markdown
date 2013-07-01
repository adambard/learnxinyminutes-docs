---
language: julia
author: Leah Hanson
author_url: http://leahhanson.us
---

Julia is a new homoiconic functional language focused on technical computing.
While having the full power of homoiconic macros, first-class functions, and low-level control, Julia is as easy to learn and use as Python.

This is based on the current development version of Julia, as of June 29th, 2013.

```julia
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
bits(2)   #=> "0000000000000000000000000000000000000000000000000000000000000010"
bits(2.0) #=> "0100000000000000000000000000000000000000000000000000000000000000"

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
"2 + 2 = $(2+2)" # => "2 + 2 = 4"
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
some_other_var #=> ERROR: some_other_var not defined

# Variable Names:
SomeOtherVar123! = 6 #=> 6 # You can use uppercase letters, digits, and exclamation points as well.
☃ = 8 #=> 8 # You can also use unicode characters

# A note on naming conventions in Julia:
# * Names of variables are in lower case, with word separation indicated by underscores ('\_').
# * Names of Types begin with a capital letter and word separation is shown with CamelCase instead of underscores.
# * Names of functions and macros are in lower case, without underscores.
# * Functions that modify their inputs have names that end in !. These functions are sometimes called mutating functions or in-place functions.

# Arrays store sequences
li = Int64[] #=> 0-element Int64 Array
# 1-dimensional array literals can be written with comma-separated values.
other_li = [4, 5, 6] #=> 3-element Int64 Array: [4, 5, 6]
# 2-dimentional arrays use space-separated values and semicolon-separated rows.
matrix = [1 2; 3 4] #=> 2x2 Int64 Array: [1 2; 3 4]

# Add stuff to the end of a list with push! and append!
push!(li,1)     #=> [1]
push!(li,2)     #=> [1,2]
push!(li,4)     #=> [1,2,4]
push!(li,3)     #=> [1,2,4,3]
append!(li,other_li) #=> [1,2,4,3,4,5,6]
# Remove from the end with pop
pop!(other_li)        #=> 6 and other_li is now [4,5]
# Let's put it back
push!(other_li,6)   # other_li is now [4,5,6] again.

li[1] #=> 1 # remember that Julia indexes from 1, not 0!
li[end] #=> 6 # end is a shorthand for the last index; it can be used in any indexing expression.

# Function names that end in exclamations points indicate that they modify their argument.
arr = [5,4,6] #=> 3-element Int64 Array: [5,4,6]
sort(arr) #=> [4,5,6]; arr is still [5,4,6]
sort!(arr) #=> [4,5,6]; arr is now [4,5,6]

# Looking out of bounds is a BoundsError
li[0] # ERROR: BoundsError() in getindex at array.jl:270
# Errors list the line and file they came from, even if it's in the standard library.
# If you built Julia from source, you can look in the folder base inside the julia folder to find these files.

# You can initialize arrays from ranges
li = [1:5] #=> 5-element Int64 Array: [1,2,3,4,5]

# You can look at ranges with slice syntax.
li[1:3] #=> [1, 2, 3]
# Omit the beginning
li[2:] #=> [2, 3, 4, 5]

# Remove arbitrary elements from a list with splice!
arr = [3,4,5]
splice!(arr,2) #=> 4 ; arr is now [3,5]

# Concatenate lists with append! 
other_li = [1,2,3]
append!(li,other_li) # Now li is [1, 3, 4, 5, 1, 2, 3]

# Check for existence in a list with contains 
contains(li,1) #=> true

# Examine the length with length
length(li) #=> 7

# Tuples are immutable.
tup = (1, 2, 3) #=>(1,2,3) # an (Int64,Int64,Int64) tuple.
tup[1] #=> 1
tup[0] = 3  # ERROR: no method setindex!((Int64,Int64,Int64),Int64,Int64)

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
filled_dict = ["one"=> 1, "two"=> 2, "three"=> 3] #=> ["one"=> 1, "two"=> 2, "three"=> 3] # Dict{ASCIIString,Int64}

# Look up values with []
filled_dict["one"] #=> 1

# Get all keys
keys(filled_dict) #=> KeyIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Note - Dictionary key ordering is not guaranteed.
# Your results might not match this exactly.

# Get all values 
values(d) #=> ValueIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Note - Same as above regarding key ordering.

# Check for existence of keys in a dictionary with contains, haskey
contains(filled_dict,("one",1)) #=> true
contains(filled_dict,("two",3)) #=> false
haskey(filled_dict,"one") #=> true
haskey(filled_dict,1) #=> false

# Trying to look up a non-existing key will raise an error
filled_dict["four"] #=> ERROR: key not found: four in getindex at dict.jl:489

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
else           # This is optional too.
    println("some_var is indeed 10.")
end



# For loops iterate over iterable things, such as ranges, lists, sets, dicts, strings. 
# prints:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

for animal=["dog", "cat", "mouse"]
    # You can use $ to interpolate into strings
    println("$animal is a mammal")
end

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

error("help") # ERROR: help in error at error.jl:21

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
    x + y    # or equivalently: return x + y
end

add(5, 6) #=> 11 and prints out "x is 5 and y is 6"

# You can define functions that take a variable number of
# positional arguments
function varargs(args...)
    return args
end

varargs(1, 2, 3) #=> (1,2,3)

# You can define functions with optional positional arguments
function defaults(a,b,x=5,y=6)
  return "$a $b and $x $y"
end

defaults('h','g') #=> "h g and 5 6"
defaults('h','g','j') #=> "h g and j 6"
defaults('h','g','j','k') #=> "h g and j k"
defaults('h') #=> ERROR: no method defaults(Char,)
defaults() #=> ERROR: no methods defaults()

# You can define functions that take keyword arguments
function keyword_args(;k1=4,name2="hello") # note the ;
    return ["k1"=>k1,"name2"=>name2]
end 

keyword_args(name2="ness") #=> ["name2"=>"ness","k1"=>4]
keyword_args(k1="mine") #=> ["k1"=>"mine","name2"=>"hello"]
keyword_args() #=> ["name2"=>"hello","k2"=>4]

####
#### In progress point
####

# You can do both at once, if you like
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) prints:
    [1, 2]
    {"a": 3, "b": 4}
"""

# You can also use * and ** when calling a function
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
foo(*args) # equivalent to foo(1, 2, 3, 4)
foo(**kwargs) # equivalent to foo(a=3, b=4)
foo(*args, **kwargs) # equivalent to foo(1, 2, 3, 4, a=3, b=4)

# Python has first class functions
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# There are also anonymous functions
(lambda x: x > 2)(3) #=> True

# There are built-in higher order functions
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# We can use list comprehensions for nice maps and filters
[add_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. Classes
####################################################

# We subclass from object to get a class.
class Human(object):

     # A class attribute. It is shared by all instances of this class
    species = "H. sapiens"

    # Basic initializer
    def __init__(self, name):
        # Assign the argument to the instance's name attribute
        self.name = name

    # An instance method. All methods take self as the first argument
    def say(self, msg):
       return "%s: %s" % (self.name, msg)

    # A class method is shared among all instances
    # They are called with the calling class as the first argument
    @classmethod
    def get_species(cls):
        return cls.species

    # A static method is called without a class or instance reference
    @staticmethod
    def grunt():
        return "*grunt*"


# Instantiate a class
i = Human(name="Ian")
print i.say("hi")     # prints out "Ian: hi"

j = Human("Joel")
print j.say("hello")  #prints out "Joel: hello"

# Call our class method
i.get_species() #=> "H. sapiens"

# Change the shared attribute
Human.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# Call the static method
Human.grunt() #=> "*grunt*"
```

## Further Reading

Still up for more? Try [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)

