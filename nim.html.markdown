---
language: nim
filename: learnNim.nim
contributors:
    - ["Jason J. Ayala P.", "http://JasonAyala.com"]
---

Nim is a statically typed, imperative programming language that tries to give
the programmer ultimate power without compromises on runtime efficiency. This
means it focuses on compile-time mechanisms in all their various forms.

Nim is efficient, expressive, and elegant.

```nimrod

var x: int    # Declare a variable and its type
x = 1         # Assign it a value
var z = "Yep" # Declare and assign, with or without type annotations

var                       # Several, with or without type annotations
  letter: char = 'n'      # One byte character
  name         = "Nimrod" # string
  truth: bool  = false    # Common boolean operators: `and` `not` `or`
  seconds: int = 42
  thoughts     = """
A great programming language
that everyone can enjoy!
"""                       # Multiline raw strings
  boat: float

let            # Use let to declare and bind an variable *once*.
  legs = 400   # legs is immutable.
  arms = 2_000 # _ are ignored and are useful for long numbers.

const            # Constants are computed at compile time. This provides
  debug = true   # performance and is useful in compile time expressions.
  aboutPi = 3.15
  compileBadCode = false

when compileBadCode:            # `when` is a compile time `if`
  legs = legs + 1               # This error will never be compiled.
  const input = readline(stdin) # const values must be known at compile time.

discard 1 > 2 # The compiler will complain if the result of an expression
              # is unused. `discard` bypasses this.

discard """
This can work as a
multiline comment
"""

#
# Common Operations on Basic Types
#

var nim = "Nimrod is a progamming language"
name = nim[0..5]

# TODO More common operations?

#
# Data Structures
#

# Tuples

var
  child: tuple[name: string, age: int]   # Tuples have *both* field names
  today: tuple[sun: string, temp: float] # *and* order.

child = (name: "Rudiger", age: 2) # Assign all at once with literal ()
today.sun = "Overcast"            # or individual fields.
today.temp = 70.1

# Sequences

var
  drinks: seq[string]

drinks = @["Water", "Juice", "Chocolate"] # @[V1,..,Vn] is the sequence literal

#
# Defining Your Own Types
#

# Defining your own types puts the compiler to work for you. It's what makes
# static typing powerful and useful.

type
  Name = string # A type alias gives you a new type that is interchangable
  Age  = int    # with the old type but is more descriptive.
  Person = tuple[name: Name, age: Age] # Define data structures too.

var
  john: Person = ("John B.", 17)
  newage: int  = 18 # It would be better to use Age than int

john.age = newage # But still works because int and Age are synonyms

type
  Cash = distinct int    # `distinct` makes a new type incompatible with it's
  Desc = distinct string # base type.

var
  money: Cash = 100.Cash           # `.Cash` converts the int to our type
  desc: Desc  = "Interesting".Desc

when compileBadCode:
  john.age  = money # Error! age is of type int and money is Cash
  john.name = desc  # Compiler says: "No way!"

#
# More Types and Data Structures
#

# Enumerations allow a type to be one of a limited number of values

type
  Directions = enum north, west, east, south
  Colors     = enum red, blue, green
var
  orient = north # `orient` is of type Directions, with the value `north`
  pixel  = green # `pixel` is of type Colors, with the value `green`

discard north > east # Enums are usually an "ordinal" type

# Subranges specify a limited valid range

type
  DieFaces = range[1..20] # Only an int from 1 to 20 is a valid value
var
  my_roll: DieFaces = 13

when compileBadCode:
  my_roll = 23 # Error!

# Arrays

type
  RollCounter = array[DieFaces, int]      # Array's are fixed length and
  DirNames    = array[Directions, string] # indexed by any ordinal type.
  Truths      = array[42..44, bool]
var
  rollCounter: RollCounter
  directions: DirNames
  truths: Truths

truths = [false, false, false] # Literal arrays are created with [V1,..,Vn]
truths[42] = true

directions[north] = "Ahh. The Great White North!"
directions[west] = "No, don't go there."

my_roll = 13
rollCounter[my_roll] += 1
rollCounter[my_roll] += 1

var anotherArray = ["Default index", "starts at", "0"]

# TODO common operations

#
# IO and Control Flow
#

# `case`, `readLine()`

echo "Read any good books lately?"
case readLine(stdin)
of "no", "No":
  echo "Go to your local library."
of "yes", "Yes":
  echo "Carry on, then."
else:
  echo "That's great; I assume."

# `while`, `if`, `continue`, `break`

import strutils as str
echo "I'm thinking of a number between 41 and 43. Guess which!"
var
  answer: int = 42
  raw_guess: string
  guess: int
while guess != answer:
  raw_guess = readLine(stdin)
  if raw_guess == "":
    continue # `continue` restarts loop/block
  guess = str.parseInt(raw_guess)
  if guess == 1001:
    echo("AAAAAAGGG!")
    break
  elif guess > answer:
    echo("Too high.")
  elif guess < answer:
    echo("Too low")
  else:
    echo("Yeeeeeehaw!")

#
# Iteration
#

# Iterate with the `for` keyword
# TODO `for` examples for strings, arrays, etc

for elem in ["Yes", "No", "Maybe so"]:
  echo elem

# string iterators

let myString = """
an example
string to
play with
"""

for line in splitLines(myString):
  echo(line)

#
# Procedures
#

type Answer = enum yes, no

proc ask(question: string): Answer =
  echo(question, " (y/n)")
  while true:
    case readLine(stdin)
    of "y", "Y", "yes", "Yes":
      return Answer.yes  # Enums can be qualified
    of "n", "N", "no", "No":
      return Answer.no
    else: echo("Please be clear: yes or no")

proc addSugar(amount: int = 2) = # Default amount is 2, returns nothing
  for a in 1..amount:
    echo a, " sugar..."

case ask("Would you like sugar in your tea?")
of yes:
  addSugar(3)
of no:
  echo "Oh do take a little!"
  addSugar()
# No need for an `else` here. only `yes` and `no` are possible.

```

## Further Reading

* [Home Page](http://nimrod-lang.org)
* [Download](http://nimrod-lang.org/download.html)
* [Community](http://nimrod-lang.org/community.html)
* [FAQ](http://nimrod-lang.org/question.html)
* [Documentation](http://nimrod-lang.org/documentation.html)
* [Manual](http://nimrod-lang.org/manual.html)
* [Standard Libray](http://nimrod-lang.org/lib.html)
