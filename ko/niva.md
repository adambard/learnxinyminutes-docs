# niva.md (번역)

---
name: niva
filename: main.niva
contributors:
    - ["gavr", "https://github.com/gavr123456789"]
---

## Intro
Niva is a simple language that takes a lot of inspiration from Smalltalk.
But leaning towards the functional side and static typed.
Everything is still an object, but instead of classes, interfaces, inheritance, and abstract classes,
we have tagged unions, which is the only way to achieve polymorphism.


For example, everything except the declaration is sending messages to objects.
`1 + 2` is not a + operator, but a `... + Int` message for `Int` object.
(there are no extra costs for that)

C-like: `1.inc()`
Niva: `1 inc`

In essence, niva is highly minimalistic, like its ancestor Smalltalk.
It introduces types, unions, and associated methods.
There are no functions.

Install:
```bash
git clone https://github.com/gavr123456789/Niva.git
cd Niva
./gradlew buildJvmNiva
# LSP here https://github.com/gavr123456789/niva-vscode-bundle
```

## The Basics

#### Variable
Variables are immutable by default.
There is no special keyword for declaring a variable.

```Scala
// this is a comment
int = 1
str = "qwf"
boolean = true
char = 'a'
float = 3.14f
double = 3.14
mutltiLineStr = """
qwf ars zxc \n \t "qwf"
"""
explicit_type::Int = 5


list = {1 2 3}
set = #(1 2 3)
map = #{1 'a' 2 'b'}

// declare a mutable variable
mut x = 5
x <- 6 // mutate
```

#### Messages
```Scala
// hello world is a one liner
"Hello world" echo // echo is a message for String obj


// There are 3 types of messages
1 inc // 2         unary
1 + 2 // 3         binary
"abc" at: 0 // 'a' keyword


// they can be chained
1 inc inc inc dec // 3
1 + 1 + 2 - 3 // 1
1 to: 3 do: [it echo] // 1 2 3
// the last one here to:do: is a single message
// to chain 2 keyword messages you need to wrap it in parentheses
("123456" drop: 1) dropLast: 2 // "234"
the comma `,` is syntactic sugar for the same effect
"123456" drop: 1, dropLast: 2 // "234"

// you can mix them
1 inc + 3 dec - "abc" count // 2 + 2 - 3 -> 1
"123" + "456" drop: 1 + 1   // "123456" drop: 2 -> "3456"

// everything except type and message declarations are message sends in niva
// for example `if` is a message for Boolean object that takes a lambda
1 > 2 ifTrue: ["wow" echo]
// expression
base = 1 > 2 ifTrue: ["heh"] ifFalse: ["qwf"]
// same for while
mut q = 0
[q > 10] whileTrue: [q <- q inc]
// all of this is zero cost because of inlining at compile time
```

#### Type
New lines are not significant in niva
Type declarations look like keyword messages consisting of fields and types
```Scala
type Square side: Int

type Person
  name: String
  age: Int
```

#### Create instance
Creating an object is the same keyword message as when declaring it, but with values in place of types.
```Scala
square = Square side: 42
alice = Person name: "Alice" age: 24

// destruct fields by names
{age name} = alice
```

#### Access fields
Getting fields is the same as sending a unary message with its name to the object
```Scala
// get age, add 1 and print it
alice age inc echo // 25
```

#### Method for type:
Everything is an object, just like in Smalltalk, so everything can have a method declared.
Here, we add a `double` method to `Int` and then use it inside the `perimeter` method of `Square`.

```Scala
Int double = this + this
Square perimeter = side double

square = Square side: 42
square perimeter // call


// explicit return type
Int double2 -> Int = this * 2

// with body
Int double3 -> Int = [
    result = this * 2
    ^ result // ^ is return
]
```


#### Keyword message declaration
```Scala
type Range from: Int to: Int
// keyword message with one arg `to`
Int to::Int = Range from: this to: to

1 to: 2 // Range
```
#### Type constructor
A type constructor functions as a message for the type itself rather than to a specific instance.
```Scala
constructor Float pi = 3.14
x = Float pi // 3.14
```

It can be useful for initializing fields with default values.
```Scala
type Point x: Int y: Int
constructor Point atStart = Point x: 0 y: 0

p1 = Point x: 0 y: 0
p2 = Point atStart
// constructor is just a usual message, so it can have params
constructor Point y::Int = Point x: 0 y: y
p3 = Point y: 20 // x: 0 y: 20
```


#### Conditions
If, like everything else, is the usual sending of a message to a Boolean object.
It takes one or two lambda arguments.
```Scala
false ifTrue: ["yay" echo]
1 < 2 ifTrue: ["yay" echo]
1 > 2 ifTrue: ["yay" echo] ifFalse: ["oh no" echo]

// `ifTrue:ifFalse:` message can be used as expression
str = 42 % 2 == 0
    ifTrue: ["even"]
    ifFalse: ["odd"]
// str == "even"
```

#### Cycles
There is no special syntax for cycles.
It's just keyword messages that take codeblocks as parameters.
(it's zero cost thanks for inlining)
```Scala
{1 2 3} forEach: [ it echo ] // 1 2 3
1..10 forEach: [ it echo ]

mut c = 10
[c > 0] whileTrue: [ c <- c dec ]

c <- 3 // reset c
[c > 0] whileTrue: [
    c <- c dec
    c echo // 3 2 1
]
```
`whileTrue:` is a message for lambda object of the type:
`[ -> Boolean] whileTrue::[ -> Unit]`

#### Matching
there is special syntax for matching, since niva heavily utilize tagged unions
```Scala
x = "Alice"
// matching on x
| x
| "Bob" => "Hi Bob!"
| "Alice" => "Hi Alice!"
|=> "Hi guest"


// It can be used as expression as well
y = | "b"
    | "a" => 1
    | "b" => 2
    |=> 0
y echo // 2
```

#### Tagged unions

```Scala
union Color = Red | Blue | Green

// branches can have fields
union Shape =
    | Rectangle width: Int height: Int
    | Circle    radius: Double

constructor Double pi = 3.14
Double square = this * this

// match on this(Shape)
Shape getArea -> Double =
    | this
    | Rectangle => width * height, toDouble
    | Circle => Double pi * radius square

// There is exhaustiveness checking, so when you add a new branch
// all the matches will become errors until all cases processed

Shape getArea -> Double = | this
    | Rectangle => width * height, toDouble
// ERROR: Not all possible variants have been checked (Circle)
```

#### Collections
```Scala
// commas are optional
list = {1 2 3}
map = #{'a' 1 'b' 2}
map2 = #{'a' 1, 'b' 2, 'c' 3}
set = #(1 2 3)

// common collection operations
{1 2 3 4 5}
  map: [it inc],
  filter: [it % 2 == 0],
  forEach: [it echo] // 2 4 6

// iteration on map
map forEach: [key, value ->
  key echo
  value echo
]
```

#### Nullability
By default, variables cannot be assigned a null value.
To allow this, nullable types are used, indicated by a question mark at the end of the type.
You may already be familiar with this concept from TypeScript, Kotlin, or Swift.
```Scala
x::Int? = null
q = x unpackOrPANIC

// do something if it's not null
x unpack: [it echo]

// same but provide a backup value
w = x unpack: [it inc] or: -1

// just unpack or backup value
e = x unpackOrValue: -1
```

#### Handling the error
```Scala
// exit the program with stack trace
x = file read orPANIC
x = file read orValue: "no file"
```
Errors work like effects, look for more in [Error handling](https://gavr123456789.github.io/niva-site/error-handling.html)

## Misc

#### Local arg names
```Scala
Int from: x::Int to: y::Int = this + x + y
```

#### Syntax sugar for this
```Scala
Person foo = [
    .bar
    this bar // same thing
]
```

#### Compile time reflection
You can get string representation of any argument from a call site.
```Scala
Int bar::Int baz::String = [
    // getting string representation from call side
    a = Compiler getName: 0
    b = Compiler getName: 1
    c = Compiler getName: 2
    a echo // 1 + 1
    b echo // variable
    c echo // "str"
]

variable = 42
// call side
1 + 1
    bar: variable
    baz: "str"
```

Links:
- [Site](https://gavr123456789.github.io/niva-site/reference.html)
- [GitHub](https://github.com/gavr123456789/Niva)
- [LSP](https://github.com/gavr123456789/vaLSe)
- [VSC plugin](https://github.com/gavr123456789/niva-vscode-bundle)
