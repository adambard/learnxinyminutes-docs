---
name: niva
filename: learnniva.niva
contributors:
    - ["gavr", "https://github.com/gavr123456789"]
---

## Intro
Niva is a simple language that takes a lot of inspiration from Smalltalk. But leaning towards the functional side. Everything is still an object but instead of classes, interfaces, and abstract classes, we have tagged unions, which is the only way to achieve polymorphism.

So basically niva is types, unions and methods for them. 

On an imaginary graph of complexity, I would put it here:
Go < Niva < Java < Kotlin < Scala

Links:
- [Site](https://gavr123456789.github.io/niva-site/reference.html)
- [GitHub](https://github.com/gavr123456789/Niva)
- [LSP](https://github.com/gavr123456789/vaLSe)
- [VSC plugin](https://github.com/gavr123456789/niva-vscode-bundle)

## The Basics

#### Variable
Variables are immutable by default.
There is no keyword for declarating a variable.

```Scala
// this is a comment
int = 1 
str = "qwf"
boolean = true 
char = 'a'
float = 3.14f
double = 3.14
mutltilineStr = """
qwf ars zxc \n \t "qwf"
"""
explicit_type::Int = 5

mut x = 5
x <- 6 // mutate
```


#### Type

```Scala
type Square side: Int
type Person
    name: String
    age: Int
```

#### Create instance
```Scala
square = Square side: 42
alice = Person name: "Alice" age: 24

// destruct fields by names
{age name} = alice
```

#### Access fields
Getting fields is the same as sending message with its name to object 
```Scala
// get age, add 1 to it and print
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
```


#### Messages with many args
```Scala
type Range from: Int to: Int
Int to::Int = Range from: this to: to

1 to: 2 // Range
```
#### Type constructor
Its like a message for type itself instead of instance
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
=> is syntax sugar for ifTrue message, since conditions is pretty common
```Scala
// syntax sugar
1 < 2 => "yay" echo
// everything is message send(this is free because of lambda-inlining)
1 < 2 ifTrue: ["yay" echo]

// with else branch
1 > 2 => "yay" echo |=> "oh no" echo

1 > 2 ifTrue: ["yay" echo] ifFalse: ["oh no" echo]
```

#### Cycles
There is no special syntax for cycles, its just keyword messages that takes codeblocs as parameters.  
(its zero cost thanks for inlining)
```Scala
{1 2 3} forEach: [ it echo ]
1..10 forEach: [ it echo ]

mut c = 10
[c > 0] whileTrue: [ c <- c dec ]
```
`whileTrue` is a message for codeblock of type: 
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

#### Unions

```Scala
union Shape =
    | Rectangle width: Int height: Int
    | Circle    radius: Int
    
constructor Float pi = 3.14

// match on this(Shape)
Shape getArea -> Float = | this 
    | Rectangle => width * height |> toFloat
    | Circle => Float pi * radius * radius
    
// its exhaustive, so when u add new branch 
// all the matches will become errors until all cases processed
```

#### Collections 
```Scala
// commas are optional
list = {1 2 3}
map = #{'a' 1 'b' 2}
map2 = #{'a' 1, 'b' 2, 'c' 3}
set = #(1 2 3)

```

#### Nullability
```Scala
x::Int? = null
q = x unpackOrPANIC
// do something if its not null
x unpack: [it echo]
// same but expression with backup value
w = x unpack: [it inc] or: -1
// just unpack or backup value
e = x unpackOrValue: -1
```

#### Handling the error
```Scala
x = file read orPANIC
x = file read orValue: "no file"
```
Look for more in [Error handling](https://gavr123456789.github.io/niva-site/error-handling.html)

## Misc



#### Local arg names
```Scala
Int from: x::Int to: y::Int = this + x + y
```

#### Syntax sugar for this
```Scala
Person foo = [
    .bar
    this bar // same thign
]
```

#### Compile time reflection
```Scala
Foo bar::Int baz::String = [
    // getting string representation from call side
    a = Compiler getName: 0
    b = Compiler getName: 1
    c = Compiler getName: 2
    a echo
    b echo
    c echo
]
```