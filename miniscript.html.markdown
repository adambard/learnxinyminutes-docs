---
language: MiniScript
contributors:
    - ["Joe Strout", "https://github.com/JoeStrout"]
filename: miniscript.ms
---

**MiniScript** is a simple scripting language designed to be easily embedded in games and other software. It can also be used on the command line, or as a cross-platform game development environment via [Soda](https://github.com/JoeStrout/soda) or [Mini Micro](https://miniscript.org/MiniMicro).

An easy way to get started with MiniScript is on the [Try-It! page](https://miniscript.org/tryit/), which runs MiniScript code on the server. Note however that the code on this page is limited to 2000 characters. (The tutorial scripts below are broken up into blocks 2048 characters or less so they will run on the Try-It! page.)

Once you are ready to go beyond the Try-It! page, your next stop should probably be to download [Mini Micro](https://miniscript.org/MiniMicro), a free virtual computer that uses MiniScript both on the command line and in programs. In that environment, enter **edit** at the prompt to edit code, then click the Run button in the editor to run it.

```
print "Hello world"

// MiniScript is very syntax-light. Notice that no parentheses are
// needed on the print statement above. Comments begin with //, and
// extend to the end of the line. MiniScript is case-sensitive.

// CONTROL FLOW
// Use if blocks to do different things depending on some condition.
// Include zero or more else if blocks, and one optional else block.
if 2+2 == 4 then
   print "math works!"
else if pi > 3 then
   print "pi is tasty"
else if "a" < "b" then
   print "I can sort"
else
   print "last chance"
end if

// LOOPING
// MiniScript has only two loop constructs: while loops and for loops.
// Use a while block to loop as long as a condition is true.
s = "Spam"
while s.len < 50
    s = s + ", spam"
end while
print s + " and spam!"

// A for loop can loop over any list, including ones easily created
// with the range function.
for i in range(10, 1)
    print i + "..."
end for
print "Liftoff!"

// Two additional keywords are useful inside loops. The break statement
// jumps out of the nearest while or for loop. The continue statement
// jumps to the top of the loop, skipping the rest of the current iteration.
for i in range(1,100)
    if i % 3 == 0 then continue  // skip multiples of 3
    if i^2 > 200 then break  // stop when i^2 is over 200
    print i + " squared is " + i^2
end for
```

### Numbers

```
// All numbers are stored in full-precision format. Numbers also
// represent true (1) and false (0), and there are built-in keywords
// (true and false) for those.
a = 7
b = 3
ultimateAnswer = 42
pi = 3.14159
n = true
m = false
print ultimateAnswer + ", " + pi + ", " + n + ", " + m

// Numbers support the following operators:
print "Basic math:"
print a + b     // addition
print a - b     // subtraction
print a * b     // multiplication
print a / b     // division
print a % b     // modulo (remainder)
print a ^ b     // power

print "Logic:"
print n and m   // logical "and"
print n or m    // logical "or"
print not n     // logical negation

print "Comparisons:"
print a == b    // equality test (note == rather than = here!)
print a != b    // inequality
print a > b     // greater than
print a >= b    // greater than or equal
print a < b     // less than
print a <= b    // less than or equal
```

### Strings

```
// Text is stored in strings of Unicode characters. Write strings
// by surrounding them with quotes. If you need to include a
// quotation mark in a string, type it twice.
print "Hello, ""Bob""."
a = "Hello"
b = "Spam"

// Strings support the following operators:
print "String ""math"":"
print a + b     // string concatenation
print b - "m"   // string subtraction (chop)
print b * 4     // string replication
print a / 2     // string division

print "Comparisons:"
print a == b    // equality test (note == rather than = here!)
print a != b    // inequality
print a > b     // greater than
print a >= b    // greater than or equal
print a < b     // less than
print a <= b    // less than or equal

// Indexing and slicing in a string is done with an index (or two)
// in square brackets. Use a 0-based index to count from the front,
// or a negative index to count from the end. Get a slice (substring)
// with two indices, separated by a colon. Either one may be omitted
// to extend the slice to the beginning or end of the string.
print "Indexing and slicing:"
print a[0]      // get a character, starting with 0 ("H")
print a[1]      // get second character ("e")
print a[-1]     // negative numbers count from the end ("o")
print a[1:4]    // get slice from 1 up to (but not including) 4 ("ell")
print a[1:-1]   // same as above, but using a negative index
print a[1:]     // get slice from 1 to the end ("ello")
print a[:2]     // get slice from beginning up to 2 ("He")

// Note that strings in MiniScript are immutable. You can't reach
// into a string and change what characters it contains (but you can
// always create a new string with different characters).
```

### Lists

```
// A list is an ordered sequence of values of any type. You can
// iterate over a list with a for loop, or iterate over the indexes
// using .indexes.
x = ["alpha", "beta", "gamma", "delta"]
for item in x
    print item
end for
for i in x.indexes
    print "x[" + i + "] is " + x[i]
end for

// Indexing and slicing in a list is exactly like a string: use a
// 0-based index to count from the front, or a negative number to
// count from the end. Get a slice (subset) of a list with two
// indices, separated by a colon. Either one may be omitted
// to extend the slice to the beginning or end of the list.
print x[0]      // alpha
print x[-1]     // delta
print x[1:3]    // [beta, gamma]
print x[2:]     // [gamma, delta]
print x[:-1]    // [alpha, beta, gamma]

// Lists support the following operators:
y = ["a", "be", "ce", "de"]
print "List ""math"":"
print x + y     // list concatenation
print y * 3     // list replication
print x / 2     // list division

print "Comparisons:"
print x == y    // equality test (note == rather than = here!)
print x != y    // inequality
```

### Maps

```
// A map is a set of values associated with unique keys. Maps
// are an extremely powerful and versatile data type, used to
// represent data records, objects, sparse arrays, and much more.
// Create a map with curly braces; get or set a single value
// with square brackets. Keys and values may be any type.
// ("Key" and "index" mean the same thing in the context of a map.)
m = {1:"one", 2:"two"}
print m[1]      // one
m[2] = "dos"    // change the value associated with index 2
print m[2]      // dos

// In the special case where the key (index) is a string that
// would be a valid variable name, there is an alternate to the
// square-bracket syntax called dot syntax. Just put the key,
// without quotes, after the map and a dot (period).
m.pi = 3.14     // equivalent to: m["pi"] = 3.14
print m["pi"]   // 3.14
m["e"] = 2.72   // equivalent to: m.e = 2.72
print m.e       // 2.72

// Maps support only the + operator, which combines all the key/value
// pairs from two maps into one.
m1 = {1:"one", 2:"two"}
m2 = {2:"dos", 3:"tres"}
print m1 + m2   // map concatenation

// You can iterate over the key/value pairs in a map with a for loop.
// On each iteration, the variable will be itself a little map with
// "key" and "value" indexes.
for kv in m1+m2
    print kv.key + " -> " + kv.value
end for

// Note that the order of key/value pairs in a map is undefined.
// You should never rely on them appearing in a particular order
// when you print or iterate over a map.
```

### Functions

```
// Create a function in miniscript with a function...end function
// block. In most cases you will assign the result to a variable
// so you can call it later. If a function needs to return a
// a result, do that with the return keyword.
rollDie = function
    return ceil(rnd * 6)  // return a random number from 1-6
end function
print rollDie
print rollDie

// If it needs parameters, put them after function keyword inside
// parentheses. Parameters may have default values.
roll = function(numberOfDice, sides=6)
    sum = 0
    for i in range(1, numberOfDice)
        sum = sum + ceil(rnd * sides)
    end for
    return sum
end function
print roll(2)     // roll two 6-sided dice
print roll(2,20)  // roll two 20-sided dice

// Variables are always local by default in MiniScript. The
// variables i and sum in the function above are not accessible
// outside the function, and disappear as soon as the function
// returns. (We'll talk more about variable scope later.)

// Parentheses are needed only if (1) you're passing arguments
// (parameter values) to the function, and (2) you're using the
// result as part of some larger statement. Notice how the first
// example above, rollDie did not need any parentheses because we
// weren't passing any arguments. Here's an example of a function
// that, like the built-in print function, is used as a statement
// by itself, and so does not need parentheses.
doRoll = function(numberOfDice, sides=6)
    print "Rolling " + numberOfDice + "d" + sides + "..."
    sum = 0
    for i in range(1, numberOfDice)
        roll = ceil(rnd * sides)
        print "You rolled a " + roll + "."
        sum = sum + roll
    end for
    print "Your total is: " + sum
end function
doRoll 3         // roll 3d6 -- note no parentheses needed
doRoll 3, 8      // same here, but rolling 3d6

// If you ever need to refer to a function without invoking it,
// you can do so with the @ operator.
f = @doRoll      // makes f refer to the same function as doRoll
f 2,4            // rolls 2d4
```

### Classes and Objects

```
// MiniScript uses prototype-based inheritance. A class or object
// is just a map with a special __isa entry that points to the
// parent class. This is set automatically when you use the new
// operator.
Shape = {}            // make a base class
Shape.sides = 0       // give it 0 sides by default

Square = new Shape    // make a subclass of Shape called Square
Square.sides = 4      // override the number of sides

x = new Square        // create an instance of the Square class
print x.sides         // 4, because x is a Square and Square.sides is 4

// A method is just a function stored in a class (map). These
// are inherited through the __isa chain, just like other values.
// Within a method, the keyword self refers to the object on which
// the method was invoked (using dot syntax). This is how you
// refer to data or methods on the object.
Shape.describe = function
    print
    print "This is a " + self.sides + "-sided shape."
end function
x.describe            // This is a 4-sided shape.

// Methods may be overridden (again just like values). In a
// subclass/instance method, you may use super to invoke the next
// version of the method up the inheritance chain, while still
// keeping self bound to the object this method was called on.
Square.describe = function
    super.describe    // first, do the standard description
    print "It looks very squarish."  // then add this
end function
x.describe
```

### More on Variable Scope

```
// Variables assignments in MiniScript always create or update
// a local variable, i.e., one that exists only within the function
// containing the assignment, unless dot syntax is used to specify
// some other scope.
x = 42      // here's a global variable called x
f = function
    x = 1234   // make a local variable, also called x
    print "Inside the function, x is now " + x
end function
f
print "Outside the function, x is " + x

// In the example above, the assignment to x inside the function
// has no effect on the global value of x, even though they happen
// to have the same name. (This is a Good Thing because it helps
// you avoid unintended side-effects in your code.)  Global variables
// are generally discouraged, but if you must update one inside
// a function, you can use a "globals." prefix to do so.
f = function
    print "Using the globals prefix..."
    globals.x = 1234   // update the global variable x
    print "Inside the function, x is now " + x
end function
f
print "Outside the function, x is " + x

// This is very similar to the "self." prefix used with
// class methods; in both cases, you are giving a more specific
// scope to a variable (which is really just specifying a map
// to index into with dot syntax).

// However there is an important difference: when READING (not
// assigning to) a variable, if the variable name is not found
// among the local variables, MiniScript will automatically look
// for a global variable of that name. Thus no "globals." prefix
// is needed when reading a variable, but only when assigning it.
count = 0
addToCount = function(amount=1)
    globals.count = count + amount
end function
addToCount
addToCount
print "count is now: " + count

// In the addToCount function above, note how we need the globals
// prefix on the left-hand side of the assignment, since otherwise
// it would create a local variable. But we don't need it on the
// right-hand side, where we are merely reading the global value.
```

### Handy Intrinsic Methods

```
// Intrinsic methods are ones that are built into MiniScript or its
// environment. Particular MiniScript environments (e.g. Mini Micro,
// Soda, command-line MiniScript, some game using MiniScript as an
// embedded language, etc.) will probably add additional intrinsics.
// But there is a core of about 50 intrinsics that should always be
// available.

// Here's a quick demo of some of the most commonly used ones.
print abs(-42)         // absolute value
print pi               // get value of pi (yep, this is built in!)
print cos(pi)          // cosine
print sqrt(100)        // square root
print round(pi, 2)     // round (to 2 decimal places)
print char(65)         // get Unicode character 65

print
s = "Hello world!"
print s.upper          // convert to upper case
print s.len            // get length (number of characters)
print s.replace("Hello", "Heya")  // string substitution
print s.split(" ")     // split on spaces to make a list
print s.remove("l")    // remove first occurrence of "l"

print
a = range(2,15,3)      // make a list: 2 through 10, in steps of 3
print "a: " + a
print "a.len:" + a.len // get length (number of values)
print "a.sum:" + a.sum // get sum of adding all values together
print a.pop            // pop off the last value
print a.pull           // pull off the first value
print "popped and pulled: " + a
a.push 99              // push a new item onto the end
a.insert 0, 101        // insert a new item at index 0
print "after push and insert: " + a
a.remove 2             // remove index 2 from the list
print "after remove 2: " + a
s = a.join("#")        // make a string by joining values with #
print s

print
m = {"one": "uno", "two": "dos", "three": "tres"}
print m.hasIndex("one") // check whether a key exists
print m.indexes         // get all the indexes
print m.values          // get all the values
m.remove "two"          // remove an index (and its value)
print m
```

## Further Reading

* [MiniScript.org website](https://miniscript.org/) — center of the MiniScript universe
* [MiniScript Quick Reference](https://miniscript.org/files/MiniScript-QuickRef.pdf) — this tutorial, in one page
* [MiniScript User's Manual](https://miniscript.org/files/MiniScript-Manual.pdf) — more in-depth documentation
* [MiniScript Wiki](https://miniscript.org/wiki/) — community-driven documentation
