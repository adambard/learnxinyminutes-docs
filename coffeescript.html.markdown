---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
filename: coffeescript.coffee
---

``` coffeescript
# CoffeeScript is a hipster language.
# It goes with the trends of many modern languages.
# So comments are like Ruby and Python, they use hashes.

###
Block comments are like these, and they translate directly to '/ *'s and '* /'s
for the resulting JavaScript code.

You should understand most of JavaScript semantices
before continuing.
###

# Assignment:
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# Conditions:
number = -42 if opposite #=> if(opposite) { number = -42; }

# Functions:
square = (x) -> x * x #=> var square = function(x) { return x * x; }

# Ranges:
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# Objects:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#  "root": Math.sqrt,
#  "square": square,
#  "cube": function(x) { return x * square(x); }
#}

# Splats:
race = (winner, runners...) ->
  print winner, runners

# Existence:
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# Array comprehensions:
cubes = (math.cube num for num in list) #=> ...
```
