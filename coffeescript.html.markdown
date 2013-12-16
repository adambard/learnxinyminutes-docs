---
language: coffeescript
contributors:
  - ["Adam Brenecki", "https://adam.brenecki.id.au/"]
filename: coffeescript.coffee
---

As the Web has grown more interactive, JavaScript has been used more not only as
a programming language in its own right, but also as a compilation target for
other languages. One of the more popular of these is CoffeeScript.

Of those languages, CoffeeScript is also one of the ones which stays closest to
its JavaScript roots, providing mostly syntactic sugar over JavaScript. Indeed,
the project website calls it "an attempt to expose the good parts of
JavaScript in a simple way" and says that "It's just JavaScript".

For that reason, if you don't already know JavaScript, it's probably worthwhile
skimming through the JavaScript tour first.

``` coffeescript
#################
# 1. Basic Syntax

# This is a single-line comment.
###
This is a multi-line comment.
While single-line comments are suppressed by the CoffeeScript compiler,
multiline comments are preserved in the output, so they're a good choice for
things like copyright notices.
###

# The first thing you'll notice about CS code is that semicolons aren't
# neccesary, and are in fact discouraged.
console.log("Hello, World", 52)

# Assignments and object literals are otherwise the same as JS, except you don't
# need to use 'var'.
myBoolean = true
myNumber = 42
myString = 'example string'
myList = [1, 2, 3, 4, 5]
myObject = {answer: 42, answerTo: ['life', 'universe', 'everything']}

# Lists can be wrapped across lines without trailing commas
bitlist = [
  1, 0, 1
  1, 1, 0
]
bitlist[2] # = 1

# Multiline objects are indented instead of curly-bracketed:
people =
  john:
    gender: 'm'
    age: 20
  jane:
    gender: 'f'
    age: 22

people.john.age # 20

# Functions have an unusual syntax. Here's an anonymous adder function:
(a, b) -> a + b

# To give functions names, simply assign them:
adder = (a, b) -> a + b
adder(2, 3) # = 5

# The brackets can be omitted for functions that take no arguments:
alwaysReturnsThree = -> 3 # '() -> 3' would also work

# When calling a function, the brackets are optional...
adder 2, 3 # = 5

# ...unless there's no arguments, in which case you'll get the function object.
alwaysReturnsThree() # = 3
alwaysReturnsThree # = [Function]

# Functions declared over multiple lines are also indented.
factorial = (n) ->
  if n == 0
    1
  else
    n * factorial(n-1)

factorial 5 # = 120


```

## Learn More

[CoffeeScript's homepage](http://coffeescript.org/) features a very in-depth
guide through the language, explaining all its features in detail and showing
exactly how they compile down to plain JavaScript.
