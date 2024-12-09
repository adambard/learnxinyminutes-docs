---
language: citron
filename: learncitron.ctr
contributors:
    - ["AnotherTest", ""]
lang: en-us
---
```ruby
# Comments start with a '#'
# All comments encompass a single line

###########################################
## 1. Primitive Data types and Operators
###########################################

# You have numbers
3. # 3

# Numbers are all doubles in interpreted mode

# Mathematical operator precedence is not respected.
# binary 'operators' are evaluated in ltr order
1 + 1. # 2
8 - 4. # 4
10 + 2 * 3. # 36

# Division is always floating division
35 / 2 # 17.5.

# Integer division is non-trivial, you may use floor
(35 / 2) floor # 17.

# Booleans are primitives
True.
False.

# Boolean messages
True not. # False
False not. # True
1 = 1. # True
1 !=: 1. # False
1 < 10. # True

# Here, `not` is a unary message to the object `Boolean`
# Messages are comparable to instance method calls
# And they have three different forms:
#   1. Unary messages: Length > 1, and they take no arguments:
        False not.
#   2. Binary Messages: Length = 1, and they take a single argument:
        False & True.
#   3. Keyword messages: must have at least one ':', they take as many arguments
#      as they have `:` s
        False either: 1 or: 2. # 2

# Strings
'This is a string'.
'There are no character types exposed to the user'.
# "You cannot use double quotes for strings" <- Error

# Strins can be summed
'Hello, ' + 'World!'. # 'Hello, World!'

# Strings allow access to their characters
'This is a beautiful string' at: 0. # 'T'

###########################################
## intermission: Basic Assignment
###########################################

# You may assign values to the current scope:
var name is value. # assigns `value` into `name`

# You may also assign values into the current object's namespace
my name is value. # assigns `value` into the current object's `name` property

# Please note that these names are checked at compile (read parse if in interpreted mode) time
# but you may treat them as dynamic assignments anyway

###########################################
## 2. Lists(Arrays?) and Tuples
###########################################

# Arrays are allowed to have multiple types
Array new < 1 ; 2 ; 'string' ; Nil. # Array new < 1 ; 2 ; 'string' ; Nil

# Tuples act like arrays, but are immutable.
# Any shenanigans degrade them to arrays, however
[1, 2, 'string']. # [1, 2, 'string']

# They can interoperate with arrays
[1, 'string'] + (Array new < 'wat'). # Array new < 1 ; 'string' ; 'wat'

# Indexing into them
[1, 2, 3] at: 1. # 2

# Some array operations
var arr is Array new < 1 ; 2 ; 3.

arr head. # 1
arr tail. # Array new < 2 ; 3.
arr init. # Array new < 1 ; 2.
arr last. # 3
arr push: 4. # Array new < 1 ; 2 ; 3 ; 4.
arr pop. # 4
arr pop: 1. # 2, `arr` is rebound to Array new < 1 ; 3.

# List comprehensions
[x * 2 + y,, arr, arr + [4, 5],, x > 1]. # Array ← 7 ; 9 ; 10 ; 11
# fresh variable names are bound as they are encountered,
# so `x` is bound to the values in `arr`
# and `y` is bound to the values in `arr + [4, 5]`
#
# The general format is: [expr,, bindings*,, predicates*]


####################################
## 3. Functions
####################################

# A simple function that takes two variables
var add is {:a:b ^a + b.}.

# this function will resolve all its names except the formal arguments
# in the context it is called in.

# Using the function
add applyTo: 3 and: 5. # 8
add applyAll: [3, 5]. # 8

# Also a (customizable -- more on this later) pseudo-operator allows for a shorthand
# of function calls
# By default it is REF[args]

add[3, 5]. # 8

# To customize this behaviour, you may simply use a compiler pragma:
#:callShorthand ()

# And then you may use the specified operator.
# Note that the allowed 'operator' can only be made of any of these: []{}()
# And you may mix-and-match (why would anyone do that?)

add(3, 5). # 8

# You may also use functions as operators in the following way:

3 `add` 5. # 8
# This call binds as such: add[(3), 5]
# because the default fixity is left, and the default precedence is 1

# You may change the precedence/fixity of this operator with a pragma
#:declare infixr 1 add

3 `add` 5. # 8
# now this binds as such: add[3, (5)].

# There is another form of functions too
# So far, the functions were resolved in a dynamic fashion
# But a lexically scoped block is also possible
var sillyAdd is {\:x:y add[x,y].}.

# In these blocks, you are not allowed to declare new variables
# Except with the use of Object::'letEqual:in:`
# And the last expression is implicitly returned.

# You may also use a shorthand for lambda expressions
var mul is \:x:y x * y.

# These capture the named bindings that are not present in their
# formal parameters, and retain them. (by ref)

###########################################
## 5. Control Flow
###########################################

# inline conditional-expressions
var citron is 1 = 1 either: 'awesome' or: 'awful'. # citron is 'awesome'

# multiple lines is fine too
var citron is 1 = 1
    either: 'awesome'
    or:     'awful'.

# looping
10 times: {:x
    Pen writeln: x.
}. # 10. -- side effect: 10 lines in stdout, with numbers 0 through 9 in them

# Citron properly supports tail-call recursion in lexically scoped blocks
# So use those to your heart's desire

# mapping most data structures is as simple as `fmap:`
[1, 2, 3, 4] fmap: \:x x + 1. # [2, 3, 4, 5]

# You can use `foldl:accumulator:` to fold a list/tuple
[1, 2, 3, 4] foldl: (\:acc:x acc * 2 + x) accumulator: 4. # 90

# That expression is the same as
(2 * (2 * (2 * (2 * 4 + 1) + 2) + 3) + 4)

###################################
## 6. IO
###################################

# IO is quite simple
# With `Pen` being used for console output
# and Program::'input' and Program::'waitForInput' being used for console input

Pen writeln: 'Hello, ocean!' # prints 'Hello, ocean!\n' to the terminal

Pen writeln: Program waitForInput. # reads a line and prints it back
```
