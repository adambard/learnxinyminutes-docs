---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
filename: learnswift.swift
---

Swift is a programming language for iOS and OS X development created by Apple. Designed to coexist with Objective-C and to be more resilient against erroneous code, Swift was introduced in 2014 at Apple's developer conference WWDC. It is built with the LLVM compiler included in Xcode 6 beta.

See also Apple's [getting started guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/LandingPage/index.html), which has a complete tutorial on Swift.

```js
//
// Basics
//

println("Hello, world")
var myVariable = 42
let myConstant = 3.1415926
let explicitDouble: Double = 70
let label = "some text " + String(myVariable)     // Casting
let piText = "Pi = \(myConstant)"                 // String interpolation
var optionalString: String? = "optional"          // Can be nil
optionalString = nil


//
// Arrays and Dictionaries
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
// Control Flow
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
// Functions
//

// Functions are a first-class type, meaning they can be nested
// in functions and can be passed around

// Function
func greet(name: String, day: String) -> String {
  return "Hello \(name), today is \(day)."
}
greet("Bob", "Tuesday")

// Function that returns multiple items in a tuple
func getGasPrices() -> (Double, Double, Double) {
  return (3.59, 3.69, 3.79)
}

// Args
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
// Closures
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
//Or even this
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]


//
// Classes
//

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
var mySquare = new Square(sideLength: 5)
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// If you don't need a custom getter and setter,
// but still want to run code before and after getting or setting
// a property, you can use `willSet` and `didSet`


//
// Enums
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
// Other
//

// `protocol`: Similar to Java interfaces.
// `extension`s: Add extra functionality to an already created type
// Generics: Similar to Java. Use the `where` keyword to specify the
//   requirements of the generics.

```
