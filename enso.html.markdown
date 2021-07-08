---
language: enso
filename: Main.enso
contributors:
    - ["Marcin Kostrzewa", "https://github.com/kustosz"]
    - ["Joe Nash", "https://github.com/joenash"]
---

Enso is a general-purpose programming language and environment for interactive data processing. It is a tool that spans the entire stack, going from high-level visualisation and communication to the nitty-gritty of backend services, all in a single language. Enso has two representations: a textual langauge, and a visual, node based language. This tutorial explains the textual syntax. 

```enso
# Single line comments start with a hash.

##  Documentation comments start with two hashes, and are block indented.
    The block ends when the block indentation returns to the baseline indentation. 

# Imports
# Most Enso files will want to start with importing the standard library.
# An unqualified import declaration, importing everything from the Base module.
from Base import all

# Unqualified imports can also be restricted
# from Base.Network.Http import Header

# Qualified imports bring only the module name into the scope
# import Base.Network.Http 

# Functions & Methods
# Declare functions with a name,
# followed by a space-separated list of parameters.
sum a b = a + b

# When declared in the top-level, functions are methods of the current module.
# Methods can be called with `.`, and `here` refers to the current module.
# `sum` is a method of the current module, so can be called on `here`.
result = here.sum 1 2
# Methods can also be declared on a type, see Type Definition below.

# Methods can be declared with default values for parameters.
product x=1 y=2 = x * y

# The entry point for an Enso program is a method `main` in `Main.enso`.
main =

    # Assignments
    # `=` for assignments, storing the result of the right-hand side under
    # the name on the left.
    x = 1
    y = 10

    # Enso has single assignments: you cannot modify existing variable.
    # x = 2 now will result in a compile error.

    # Methods can be called with `.`.
    # Methods can be passed additional arguments.
    # `up_to` creates a `Range` from 1 up to 10.
    range = x.up_to y

    # Operator sections:
    # (+ 5) => a function that takes x and returns x+5
    # (5 +) => a function that takes x and returns 5+x
    # [1, 2, 3] . map (+ 5)
    # [1, 2, 3] . map +5
    # .noise => a function that takes x and return x.noise

    # Operators, including `.`, have different precedence depending on spacing
    # No-spaces binds stronger than a spaced version
    # foo bar . baz ==> (foo bar).baz
    # foo bar.baz   ==> foo (bar.baz)
    result_2 = 2+2 * 2 # (2+2)*2 == 8
    result_3 = 2 + 2 * 2 # 6, conventional aritmetic operator
    mapped_list = range . to_vector . map .noise
    # equivalent to:
    # mapped_list = (range.to_vector).map .noise

    # Lambdas / anonymous functions
    # Created with `->`
    # Parameters on the left, results on the right:
    [1, 2, 3] . map (x -> x * 5)
    # Multiple parameters are achieved by multiple `->`:
    [1, 2, 3] . reduce (x -> y -> x + y)

    # Lambda shorthands
    my_lambda = (here.sum _ 5) # equivalent to (x-> here.sum x 5)

    # Calling a method
    this.sum 1 2 # `this` refers to whatever the current method is called on
    here.sum 1 2 # `here` ALWAYS refers to the current module

    # Defaulted parameters can be omitted when calling methods.
    result_5 = here.product x=20 y=100 # equivalent to product 20 100
    result_6 = here.product x=20 # equivalent to product 20 2
    result_7 = here.product 20 # equivalent to product 20 2
    result_8 = here.product 20 ... # returns a function expecting a parameter for y

    # The last line in a block of code is the return value
    here.sum 1 99 # main returns 100

# Built-in data types
data_types =
    # Numbers
    integer = 6
    decimal = 78.23

    # Text
    text_sample = "this \n is my text"
    text_sample_2 = 'this \n is my text'
    text_block = '''
        My text
        can have multiple
            lines
        now
    # `+` concatenates text
    concat = text_sample + text_block

    # Booleans
    my_true = True
    my_false = False

    # Boolean logic operators

    True && False
    True || False
    True.not

    my_bool = 1 == 2
    my_bool_2 = 2 > 0.3
    my_bool_3 = "foo" == "bar"

    # Vectors:
    # Create with []
    my_vec = [1, 2, 3]
    # Vectors can contain multiple types:
    my_vec_2 = [1, "Foo", False]
    # `characters` is a method that creates a vector of characters in text.
    text_sample.characters

    # Maps
    # key:value data structure.

    empty_map = Map.empty

    inserted = empty_map.insert 4 "foo"
    item = inserted.get 4 # returns "foo"
    item_2 = inserted.get 10 # returns Dataflow Error

# Type definition
# Types are defined with the `type` keyword,
# followed by a type name and fields.
type Geo_Point latitude longitude

# Types can also be defined with default values for fields.
type Geo_Point_2 latitude longitude shape='star'

# Methods can be defined as Extension Methods, on an existing type (an Atom).
Geo_Point.convert =
    # `this` refers to the `Geo_Point` object
    result = Geo_Point (this.latitude / 3600000) (this.longitude / 3600000)
    # `here` still refers to the current module
    here.my_method result.latitude result.longitude
    result

test_types =
    # Create a Geo_Point:
    my_point = Geo_Point 52.05 15.2
    # Fields can be used as getters:
    my_latitude = my_point.latitude
    my_longitude = my_point.longitude
    # Pattern matching on fields:
    my_number = case my_point of
        Geo_Point ltd lng -> ltd * lng
    # Calling a method on a value
    my_point.convert

# Types can be nested and combined with pattern matching to create sum types.
type One_Or_Another
    type One value
    type Another value_x value_y

    my_method v = case this of
        One False -> v || False
        One True -> v && True
        Another vx vy -> v * vx * vy

# Polyglot: language interoperation

# Java interoperability
polyglot java import java.lang.Long
polyglot java import java.lang.StringBuilder

poly_test =
    # Call a static method
    result = Long.add 1 2
    # Create an instance! (call a constructor)
    my_inst = StringBuilder.new "initial text"
    # Call an instance method
    my_inst.append "more text"

# Polyglot (JS | More languages, like Python and R, coming soon)

foreign r my_function arg1 arg2 = """
    library(lattice)
    print(histogram(arg1 ~ arg2))

foreign js mk_object arg1 arg2 = """
    {foo: arg1, bar: arg2}

poly_test_2 =
    # Call just like Enso, everything is curried.
    here.my_function 1 2
    here.mk_object 3 4

# Dataflow Errors 
errors =
    # Create a dataflow error:
    foo = Error.throw "foo" 

    # Returns "foo" unmodified, without calling `my_method`. 
    foo.my_method 

    # Match result is "foo" again:
    match_result = case foo of
        Nothing -> True
        True -> False
        False -> Nothing

    # We just can't escape "foo":
    foo + 5 

    # Errors can be caught with `.catch`:
    caught_error = foo.catch error_value->
        "Error: " + error_value

    # y is 10, as there is no error so `.catch` is a no-op:
    y = 10.catch x-> x + 5

    # Something very bad and probably irrecoverable has happened.
    Panic.throw "HEEEELP!"

    # A Panic can be recovered, resulting in a Dataflow error.
    Panic.recover (here.my_method)
```