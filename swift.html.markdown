---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
filename: learnswift.swift
---

Swift is a programming language for iOS and OS X development created by Apple. Designed to coexist with Objective-C and to be more resilient against erroneous code, Swift was introduced in 2014 at Apple's developer conference WWDC. It is built with the LLVM compiler included in Xcode 6 beta.

The official [Swift Programming Language](https://itunes.apple.com/us/book/swift-programming-language/id881256329) book from Apple is now available via iBooks.

See also Apple's [getting started guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/LandingPage/index.html), which has a complete tutorial on Swift.

```swift
//
// MARK: Basics
//

// Xcode supports landmarks to annotate your code and lists them in the jump bar
// MARK: Section mark
// TODO: Do something soon
// FIXME Fix this code

println("Hello, world")

var myVariable = 42
let øπΩ = "value" // unicode variable names
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
        Nested comments are also supported
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
let emptyDictionary = [String: Float]()


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

// Function with Swift header docs (format as reStructedText)
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
// MARK: Structures
//

// Structures and classes have very similar capabilites
struct NamesTable {
    let names: [String]

    // Custom subscript
    subscript(index: Int) -> String {
        return names[index]
    }
}

// Structures have an auto-generated (implicit) designated initializer
let namesTable = NamesTable(names: ["Me", "Them"])
//let name = namesTable[2]
//println("Name is \(name)") // Name is Them

//
// MARK: Classes
//

// Classes, structures and its members have three levels of access control
// They are: internal (default), public, private

public class Shape {
    public func getArea() -> Int {
        return 0;
    }
}

// All methods and properties of a class are public.
// If you just need to store data in a
// structured object, you should use a `struct`

internal class Rect: Shape {
    var sideLength: Int = 1
    
    // Custom getter and setter property
    private var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue` is an implicit variable available to setters
            sideLength = newValue / 4
        }
    }

    // Lazily load a property
    // subShape remains nil (uninitialized) until getter called
    lazy var subShape = Rect(sideLength: 4)
    
    // If you don't need a custom getter and setter,
    // but still want to run code before and after getting or setting
    // a property, you can use `willSet` and `didSet`
    var identifier: String = "defaultID" {
        // the `willSet` arg will be the variable name for the new value
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

// A simple class `Square` extends `Rect`
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
    println("Yep, it's mySquare")
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
// MARK: Protocols
//

// `protocol`s can require that conforming types have specific
// instance properties, instance methods, type methods, 
// operators, and subscripts.

protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

/*
// Protocols declared with @objc allow optional functions,
// which allow you to check for conformance
@objc protocol TransformShape {
    optional func reshaped()
    optional func canReshape() -> Bool
}

class MyShape: Rect {
    var delegate: TransformShape?

    func grow() {
        sideLength += 2

        if let allow = self.delegate?.canReshape?() {
            // test for delegate then for method
            self.delegate?.reshaped?()
        }
    }
}
*/

//
// MARK: Other
//

// `extension`s: Add extra functionality to an already existing type

// Square now "conforms" to the `Printable` protocol
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
let foundAtIndex = findIndex([1, 2, 3, 4], 3)
println(foundAtIndex == 2) // true

// Operators:
// Custom operators can start with the characters:
//      / = - + * % < > ! & | ^ . ~
// or
// Unicode math, symbol, arrow, dingbat, and line/box drawing characters.
prefix operator !!! {}

// A prefix operator that triples the side length when used
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// current value
println(mySquare.sideLength) // 4

// change side length using custom !!! operator, increases size by 3
!!!mySquare
println(mySquare.sideLength) // 12

```
