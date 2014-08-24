---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
filename: learnswift.swift
---

Swift is a programming language for iOS and OS X development created by Apple. Designed to coexist with Objective-C and to be more resilient against erroneous code, Swift was introduced in 2014 at Apple's developer conference WWDC. It is built with the LLVM compiler included in Xcode 6 beta.

See also Apple's [getting started guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/LandingPage/index.html), which has a complete tutorial on Swift.

```js
//
// MARK: Basics
//

// Xcode supports landmarks to annotate your code and lists them in the jump bar
// MARK: Section mark
// TODO: Do something soon
// FIXME Fix this code

println("Hello, world")

var myVariable = 42
//let fƒ∆ = "value" // unicode in variable names
let myConstant = 3.1415926
let convenience = "keyword" // contextual variable name
let weak = "keyword"; let override = "another keyword" // statements can be separated by a semi-colon
let `class` = "keyword" // backticks allow keywords to be used as variable names
let explicitDouble: Double = 70
let intValue = 0007 // 7
let largeIntValue = 77_000 // 77000
let label = "some text " + String(myVariable) // Casting
let piText = "Pi = \(myConstant), Pi 2 = \(myConstant * 2)" // String interpolation
var optionalString: String? = "optional" // Can be nil
optionalString = nil

/*
Comment here
    /*
        Nested comment here
    */
*/

//
// MARK: Collections
//

// Array
var shoppingList = ["catfish", "water", "lemons"]
shoppingList[1] = "bottle of water"
let emptyArray = [String]()

// Dictionary
var occupations = [
    "Malcolm": "Captain",
    "kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
let emptyDictionary = Dictionary<String, Float>()


//
// MARK: Control Flow
//

// for loop (array)
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        println("One!")
    } else {
        println("Not one!")
    }
}

// for loop (dictionary)
var dict = ["one": 1, "two": 2]
for (key, value) in dict {
    println("\(key): \(value)")
}

// for loop (range)
for i in -1...1 { // [-1, 0, 1]
    println(i)
}
// use ..< to exclude the last number

// while loop
var i = 1
while i < 1000 {
    i *= 2
}

// do-while loop
do {
    println("hello")
} while 1 == 2

// Switch
let vegetable = "red pepper"
switch vegetable {
case "celery":
    let vegetableComment = "Add some raisins and make ants on a log."
case "cucumber", "watercress":
    let vegetableComment = "That would make a good tea sandwich."
case let x where x.hasSuffix("pepper"):
    let vegetableComment = "Is it a spicy \(x)?"
default: // required (in order to cover all possible input)
    let vegetableComment = "Everything tastes good in soup."
}


//
// MARK: Functions
//

// Functions are a first-class type, meaning they can be nested
// in functions and can be passed around

// Function with Swift function docs
/**
    A greet operation

    - A bullet in docs
    - Another bullet in the docs

    :param: name A name
    :param: day A day
    :returns: A string containing the name and day value.
*/
func greet(name: String, day: String) -> String {
    return "Hello \(name), today is \(day)."
}
greet("Bob", "Tuesday")

// Function that returns multiple items in a tuple
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}

// Variadic Args
func setup(numbers: Int...) {}

// Passing and returning functions
func makeIncrementer() -> (Int -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)


//
// MARK: Closures
//
var numbers = [1, 2, 6]

// Functions are special case closures ({})

// Closure example.
// `->` separates the arguments and return type
// `in` separates the closure header from the closure body
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})

// When the type is known, like above, we can do this
numbers = numbers.map({ number in 3 * number })
// Or even this
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// Trailing closure
numbers = sorted(numbers) { $0 > $1 }

print(numbers) // [18, 6, 3]

// Super shorthand, since the < operator infers the types

numbers = sorted(numbers, < )

print(numbers) // [3, 6, 18]

//
// MARK: Classes
//

class Shape {
    func getArea() -> Int {
        return 0;
    }
}

// All methods and properties of a class are public.
// If you just need to store data in a
// structured object, you should use a `struct`

// A simple class `Square` extends `Shape`
class Rect: Shape {
    var sideLength: Int = 1
    
    // Custom getter and setter property
    var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            sideLength = newValue / 4
        }
    }
    
    // If you don't need a custom getter and setter,
    // but still want to run code before and after getting or setting
    // a property, you can use `willSet` and `didSet`
    var identifier: String = "defaultID" {
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }
    
    init(sideLength: Int) {
        super.init()
        self.sideLength = sideLength
    }
    
    func shrink() {
        if sideLength > 0 {
            --sideLength
        }
    }
    
    override func getArea() -> Int {
        return sideLength * sideLength
    }
}

class Square: Rect {
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// compare instances, not the same as == which compares objects (equal to)
if mySquare === mySquare {
    println("Yep its mySquare")
}


//
// MARK: Enums
//

// Enums can optionally be of a specific type or on their own.
// They can contain methods like classes.

enum Suit {
    case Spades, Hearts, Diamonds, Clubs
    func getIcon() -> String {
        switch self {
        case .Spades: return "♤"
        case .Hearts: return "♡"
        case .Diamonds: return "♢"
        case .Clubs: return "♧"
        }
    }
}


//
// MARK: Other
//

// `protocol`: Similar to Java interfaces.
protocol ShapeGenerator {
    func buildShape() -> Shape
}

// `extension`s: Add extra functionality to an already existing type
extension Square: Printable {
    var description: String {
        return "Area: \(self.getArea()) - ID: \(self.identifier)"
    }
}

println("Square: \(mySquare)")

// You can also extend built-in types
extension Int {
    var customProperty: String {
        return "This is \(self)"
    }

    func multiplyBy(num: Int) -> Int {
        return num * self
    }
}

println(7.customProperty) // "This is 7"
println(14.multiplyBy(2)) // 42

// Generics: Similar to Java. Use the `where` keyword to specify the
//   requirements of the generics.

func findIndex<T: Equatable>(array: [T], valueToFind: T) -> Int? {
    for (index, value) in enumerate(array) {
        if value == valueToFind {
            return index
        }
    }
    return nil
}


// Operators:
// Custom operators can start with the characters:
//      / = - + * % < > ! & | ^ . ~
// or
// Unicode math, symbol, arrow, dingbat, and line/box drawing characters.
prefix operator !!! {}

// An operator that triples the side length when used
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

let bigSquare = !!!mySquare
println(bigSquare.sideLength)

```
