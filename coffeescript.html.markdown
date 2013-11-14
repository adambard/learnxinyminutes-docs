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

# Functions may also have default values for arguments.
fill = (container, liquid = "coffee") ->
  "Filling the #{container} with #{liquid}..."

# Objects and Arrays
# Objects may be created using indentation instead of explicit braces, similar to YAML.
song = ["do", "re", "mi", "fa", "so"]

bitlist = [
  1, 0, 1
  0, 0, 1
  1, 1, 0
]

kids =
  brother:
    name: "Max"
    age:  11
  sister:
    name: "Ida"
    age:  9

# If, Else, Unless, and Conditional Assignment
mood = greatlyImproved if singing

if happy and knowsIt
  clapsHands()
  chaChaCha()
else
  showIt()

date = if friday then sue else jill

# Splats...
awardMedals = (first, second, others...) ->
  gold   = first
  silver = second
  rest   = others

contenders = [
  "Michael Phelps"
  "Liu Xiang"
  "Yao Ming"
  "Allyson Felix"
  "Shawn Johnson"
  "Roman Sebrle"
  "Guo Jingjing"
  "Tyson Gay"
  "Asafa Powell"
  "Usain Bolt"
]

awardMedals contenders...

# Loops and Comprehensions
eat food for food in ['toast', 'cheese', 'wine']

courses = ['greens', 'caviar', 'truffles', 'roast', 'cake']
menu i + 1, dish for dish, i in courses

foods = ['broccoli', 'spinach', 'chocolate']
eat food for food in foods when food isnt 'chocolate'

# Comprehensions should be able to handle most places where you otherwise would use a loop
shortNames = (name for name in list when name.length < 5)
countdown = (num for num in [10..1])

# To step through a range comprehension in fixed-size chunks, use by
evens = (x for x in [0..10] by 2)

# Use of to signal comprehension over the properties of an object instead of the values in an array.
yearsOld = max: 10, ida: 9, tim: 11

ages = for child, age of yearsOld
  "#{child} is #{age}"

# The only low-level loop that CoffeeScript provides is the while loop.
if this.studyingEconomics
  buy()  while supply > demand
  sell() until supply > demand

num = 6
lyrics = while num -= 1
  "#{num} little monkeys, jumping on the bed.
    One fell out and bumped his head."

# CoffeeScript provides the do keyword, which immediately invokes a passed function, forwarding any arguments.
for filename in list
  do (filename) ->
    fs.readFile filename, (err, contents) ->
      compile filename, contents.toString()

# Array Slicing and Splicing with Ranges
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
start   = numbers[0..2]
middle  = numbers[3...6]
end     = numbers[6..]
copy    = numbers[..]

numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
numbers[3..6] = [-3, -4, -5, -6]

# Operators and Aliases

launch() if ignition is on

volume = 10 if band isnt SpinalTap

letTheWildRumpusBegin() unless answer is no

if car.speed < limit then accelerate()

winner = yes if pick in [47, 92, 13]

print inspect "My name is #{@name}"

# The Existential Operator
solipsism = true if mind? and not world?

speed = 0
speed ?= 15

footprints = yeti ? "bear"

# The accessor variant of the existential operator ?. can be used to soak up null references in a chain of properties
zip = lottery.drawWinner?().address?.zipcode

# Classes, Inheritance, and Super
# super() is converted into a call against the immediate ancestor's method of the same name.
class Animal
  constructor: (@name) ->

  move: (meters) ->
    alert @name + " moved #{meters}m."

class Snake extends Animal
  move: ->
    alert "Slithering..."
    super 5

class Horse extends Animal
  move: ->
    alert "Galloping..."
    super 45

sam = new Snake "Sammy the Python"
tom = new Horse "Tommy the Palomino"

sam.move()
tom.move()

# :: gives you quick access to an object's prototype;
String::dasherize = ->
  this.replace /_/g, "-"

# Destructuring Assignment
theBait   = 1000
theSwitch = 0

[theBait, theSwitch] = [theSwitch, theBait]

weatherReport = (location) ->
  # Make an Ajax request to fetch the weather...
  [location, 72, "Mostly Sunny"]

[city, temp, forecast] = weatherReport "Berkeley, CA"

# Destructuring assignment can be used with any depth of array and object nesting
futurists =
  sculptor: "Umberto Boccioni"
  painter:  "Vladimir Burliuk"
  poet:
    name:   "F.T. Marinetti"
    address: [
      "Via Roma 42R"
      "Bellagio, Italy 22021"
    ]

{poet: {name, address: [street, city]}} = futurists

# Destructuring assignment can even be combined with splats.
tag = "<impossible>"

[open, contents..., close] = tag.split("")

# Destructuring assignment is also useful when combined with class constructors to assign properties to your instance
# from an options object passed to the constructor.
class Person
  constructor: (options) -> 
    {@name, @age, @height} = options

# Function binding
# The fat arrow => can be used to both define a function, and to bind it to the current value of this, right on the spot.
Account = (customer, cart) ->
  @customer = customer
  @cart = cart

  $('.shopping_cart').bind 'click', (event) =>
    @customer.purchase @cart

# Switch/When/Else
switch day
  when "Mon" then go work
  when "Tue" then go relax
  when "Thu" then go iceFishing
  when "Fri", "Sat"
    if day is bingoDay
      go bingo
      go dancing
  when "Sun" then go church
  else go work

score = 76
grade = switch
  when score < 60 then 'F'
  when score < 70 then 'D'
  when score < 80 then 'C'
  when score < 90 then 'B'
  else 'A'

# Chained Comparisons
healthy = 200 > cholesterol > 60

# String Interpolation
quote  = "A picture is a fact. -- #{ author }"

sentence = "#{ 22 / 7 } is a decent approximation of π"

```