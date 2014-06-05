---
language: swift
filename: learnswift-cn.swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
translators:
  - ["Xavier Yao", "http://github.com/xavieryao"]
lang: zh-cn
---

Swift 是Apple 开发的用于iOS 和OS X 开发的编程语言。Swift 于2014年Apple WWDC （全球开发者大会）中被引入，用以与Objective-C 共存，同时对错误代码更具弹性。Swift 由Xcode 6 beta 中包含的LLVM编译器编译。

参阅：Apple's [getting started guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/LandingPage/index.html) ——一个完整的Swift 教程

```js
//
// 基础
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
// 数组与字典（关联数组）
//

// 数组
var shoppingList = ["catfish", "water", "lemons"]
shoppingList[1] = "bottle of water"
let emptyArray = String[]()

// 字典
var occupations = [
  "Malcolm": "Captain",
  "kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
let emptyDictionary = Dictionary<String, Float>()


//
// 控制流
//

// 用于数组的for 循环
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
  if value == 1 {
    println("One!")
  } else {
    println("Not one!")
  }
}

// 用于字典的for 循环
for (key, value) in dict {
  println("\(key): \(value)")
}

// 用于区间的for 循环
for i in -1...1 { // [-1, 0, 1]
  println(i)
}
// 使用 .. 表示的区间不包含最后一个元素 [-1,0,1)

// while 循环
var i = 1
while i < 1000 {
  i *= 2
}

// do-while 循环
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
default: // 必须 (为了覆盖所有可能的输入)
  let vegetableComment = "Everything tastes good in soup."
}


//
// 函数
//

// 函数是一等类型，这意味着可以在函数中构建函数
// 并且可以被传递

// 函数
func greet(name: String, day: String) -> String {
  return "Hello \(name), today is \(day)."
}
greet("Bob", "Tuesday")

// 使用多元数组返回多返回值的函数
func getGasPrices() -> (Double, Double, Double) {
  return (3.59, 3.69, 3.79)
}

// 不定参数
func setup(numbers: Int...) {}

// 传递、返回函数
func makeIncrementer() -> (Int -> Int) {
  func addOne(number: Int) -> Int {
    return 1 + number
  }
  return addOne
}
var increment = makeIncrementer()
increment(7)


//
// 闭包
//

// 函数是特殊的闭包({})

// 闭包示例.
// `->` 分隔参数和返回类型
// `in` 分隔闭包头和闭包体
numbers.map({
  (number: Int) -> Int in
  let result = 3 * number
  return result
  })

// 当类型已知时，可以这样做：
var numbers = [1, 2, 6]
numbers = numbers.map({ number in 3 * number })
print(numbers) // [3, 6, 18]


//
// 类
//

// 类的全部方法和属性都是public 的
// 如果你在一个数据结构中只需储存数据，
// 应使用 `struct`

// 集成自`Shape` 类的简单的类`Square
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

// 如果你不需要自定义getter 和setter,
// 但仍希望在获取或设置一个属性之前或之后运行
// 一些代码，你可以使用`willSet` 和 `didSet`


//
// 枚举类型
//

// 枚举类型可以是某种指定的类型，抑或自成一种类型
// 像类一样，枚举类型可以包含方法

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
// 其它
//

// `协议(protocol)`: 与Java 的接口(Interface) 类似.
// `扩展(extension)`: 为现有类型添加额外特性
// 泛型: 与Java 相似。使用`where` 关键字指定
//   泛型的要求.

```
