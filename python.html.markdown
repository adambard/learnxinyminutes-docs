---
language: python
author: Louie Dinh
author_url: http://ldinh.ca
---

Python was created by Guido Van Rossum in the early 90's. It is now one of the most popular
languages in existence. I fell in love with Python for it's syntactic clarity. It's basically
executable pseudocode.

Note: This article applies to Python 2.7 specifically, but should be applicable
to Python 2.x. Look for another tour of Python 3 soon!

```python
# Single line comments start with a hash.
""" Multiline comments can we written
    using three "'s
"""

####################################################
## 1. Primitive Datatypes and Operators
####################################################

# You have numbers
3 #=> 3

# Math is what you would expect
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Division is a bit tricky. It is integer division and floors the results
# automatically.
11 / 4 #=> 2

# To fix division we need to learn about floats.
2.0     # This is a float
5.0 / 2.0 #=> 2.5 ahhh...much better

# Enforce precedence with parentheses
(1 + 3) * 2 #=> 8

# Boolean values are primitives
True
False

# negate with not
not True #=> False
not False #=> True

# Equality is ==
1 == 1 #=> True
2 == 1 #=> False

# Inequality is !=
1 != 1 #=> False
2 != 1 #=> True

# More comparisons
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# Comparisons can be chained  !
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Strings are created with " or '
"This is a string."
'This is also a string.'

# Strings can be added too!
"Hello " + "world!" #=> "Hello world!"

# A string can be treated like a list of characters
"This is a string"[0] #=> 'T'

# None is an object
None #=> None


####################################################
## 2. Variables and Collections
####################################################

# Printing is pretty easy
print "I'm Python. Nice to meet you!"


# No need to declare variables before assigning to them.
some_var = 5    # Convention is to use lower_case_with_underscores
some_var #=> 5

# Accessing a previously unassigned variable is an exception
try:
    some_other_var
except NameError:
    print "Raises a name error"

# Conditional Expressions can be used when assigning
some_var = a if a > b else b
# If a is greater than b, then a is assigned to some_var.
# Otherwise b is assigned to some_var.

# Lists store sequences
li = []
# You can start with a prefilled list
other_li = [4, 5, 6]

# Add stuff to the end of a list with append
li.append(1)    #li is now [1]
li.append(2)    #li is now [1, 2]
li.append(4)    #li is now [1, 2, 4]
li.append(3)    #li is now [1, 2, 4, 3]
# Remove from the end with pop
li.pop()        #=> 3 and li is now [1, 2, 4]
# Let's put it back
li.append(3)    # li is now [1, 2, 4, 3] again.

# Access a list like you would any array
li[0] #=> 1
# Look at the last element
li[-1] #=> 4

# Looking out of bounds is an IndexError
try:
    li[4] # Raises an IndexError
except IndexError:
    print "Raises an IndexError"

# You can look at ranges with slice syntax.
# (It's a closed/open range for you mathy types.)
li[1:3] #=> [2, 4]
# Omit the beginning
li[:3] #=> [1, 2, 4]
# Omit the end
li[2:] #=> [4, 3]

# Remove arbitrary elements from a list with del
del li[2] # li is now [1, 2, 3]

# You can add lists
li + other_li #=> [1, 2, 3, 4, 5, 6] - Note: li and other_li is left alone

# Concatenate lists with extend
li.extend(other_li) # Now li is [1, 2, 3, 4, 5, 6] 

# Check for existence in a list with in
1 in li #=> True

# Examine the length with len
len(li) #=> 6

# Tuples are like lists but are immutable.
tup = (1, 2, 3)
tup[0] #=> 1
try:
    tup[0] = 3  # Raises a TypeError
except TypeError:
    print "Tuples cannot be mutated."

# You can do all those list thingies on tuples too
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# However, you can unpack tuples into variables
a, b, c = (1, 2, 3)     # a is now 1, b is now 2 and c is now 3
# Tuples are created by default if you leave out the parentheses
d, e, f = 4, 5, 6
# Now look how easy it is to swap two values
e, d = d, e     # d is now 5 and e is now 4


# Dictionaries store mappings
empty_dict = {}
# Here is a prefilled dictionary
filled_dict = {"one": 1, "two": 2, "three": 3}

# Look up values with []
filled_dict["one"] #=> 1

# Get all keys as a list
filled_dict.keys() #=> ["three", "two", "one"]
# Note - Dictionary key ordering is not guaranteed.
# Your results might not match this exactly.

# Get all values as a list
filled_dict.values() #=> [3, 2, 1]
# Note - Same as above regarding key ordering.

# Check for existence of keys in a dictionary with in
"one" in filled_dict #=> True
1 in filled_dict #=> False

# Trying to look up a non-existing key will raise a KeyError
filled_dict["four"] #=> KeyError

# Use get method to avoid the KeyError
filled_dict.get("one", 4) #=> 1
filled_dict.get("four", 4) #=> 4

# Setdefault method is a safe way to add new key-value pair into dictionary
filled_dict.setdefault("five", 5) #filled_dict["five"] is set to 5
filled_dict.setdefault("five", 6) #filled_dict["five"] is still 5


# Sets store ... well sets
empty_set = set()
# Initialize a set with a bunch of values
filled_set = set([1,2,2,3,4]) # filled_set is now set([1, 2, 3, 4])

# Add more items to a set
filled_set.add(5) # filled_set is now set([1, 2, 3, 4, 5])

# Do set intersection with &
other_set = set([3, 4, 5 ,6])
filled_set & other_set #=> set([3, 4, 5])
# Do set union with |
filled_set | other_set #=> set([1, 2, 3, 4, 5, 6])
# Do set difference with -
set([1,2,3,4]) - set([2,3,5]) #=> set([1, 4])

# Check for existence in a set with in
2 in filled_set #=> True
10 in filled_set #=> False


####################################################
## 3. Control Flow
####################################################

# Let's just make a variable
some_var = 5

# Here is an if statement. INDENTATION IS SIGNIFICANT IN PYTHON!
# prints "some var is smaller than 10"
if some_var > 10:
    print "some_var is totally bigger than 10."
elif some_var < 10:    # This elif clause is optional.
    print "some_var is smaller than 10."
else:           # This is optional too.
    print "some_var is indeed 10."


"""
For loops iterate over lists
prints:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # You can use % to interpolate formatted strings
    print "%s is a mammal" % animal 

"""
While loops go until a condition is no longer met.
prints:
    0
    1
    2 
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Shorthand for x = x + 1

# Handle exceptions with a try/except block

# Works on Python 2.6 and up:
try:
    # Use raise to raise an error
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # Pass is just a no-op. Usually you would do recovery here.

# Works for Python 2.7 and down:
try:
    raise IndexError("This is an index error")
except IndexError, e: # No "as", comma instead
    pass


####################################################
## 4. Functions
####################################################

# Use def to create new functions
def add(x, y):
    print "x is %s and y is %s" % (x, y)
    return x + y    # Return values with a return statement

# Calling functions with parameters
add(5, 6) #=> 11 and prints out "x is 5 and y is 6"
# Another way to call functions is with keyword arguments
add(y=6, x=5)   # Keyword arguments can arrive in any order.

# You can define functions that take a variable number of
# positional arguments
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# You can define functions that take a variable number of
# keyword arguments, as well
def keyword_args(**kwargs):
    return kwargs

# Let's call it to see what happens
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

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

