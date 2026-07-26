---
contributors:
    - ["sam5440"]
translators:
    - ["sam5440"]
---

仓颉是一门静态类型的通用编程语言。它有接近 C 系语言的表层语法，
同时提供类型推断、表达式化控制流、代数枚举、模式匹配、类、接口、
泛型、属性、函数值和操作符重载等现代语言特性。

```text
// 源文件以包声明开始。
package learnxinyminutes

// 导入语句跟在包声明之后。
import std.convert.*
import std.math.*
import std.random.*

// 安装仓颉 SDK 后，单文件可这样编译：
//     cjc learncangjie.cj
// 较大的工程通常使用 `cjpm` 和 `cjpm.toml`。

// 单行注释以两个斜杠开始。
/*
多行注释是这个样子。
*/

////////////////////////////////////////////////////////////
// 1. 程序入口、输出、输入与变量创建
////////////////////////////////////////////////////////////

// `main` 是可执行程序的入口。
main(): Int64 {
    // `println` 向标准输出写一行。
    println("Hello, Cangjie!")

    // `print` 输出后不自动换行。
    print("这个提示保持在同一行: ")
    println("done")

    // `readln()` 从标准输入读取一整行，结果是 String。
    // 这里保持注释，避免导览程序运行时等待输入。
    // let rawInput = readln()
    // let parsedNumber = Int64.parse(rawInput)

    // 用 `let` 创建不可变绑定。
    let language: String = "Cangjie"

    // 用 `var` 创建可变变量。
    var releaseYear: Int64 = 2024
    releaseYear += 1

    // 局部变量通常可以省略类型标注。
    let inferredInt = 42
    let inferredFloat = 3.14159

    println("${language} ${releaseYear}")
    println("${inferredInt}, ${inferredFloat}")

    learnBindings()
    learnStringsAndNumbers()
    learnControlFlow()
    learnCollections()
    learnFunctions()
    learnStructsClassesAndInterfaces()
    learnEnumsAndOptions()
    learnStandardLibrary()

    return 0
}

////////////////////////////////////////////////////////////
// 2. 后文会用到的声明
////////////////////////////////////////////////////////////

// `struct` 常用于值语义记录。
struct Vec2 {
    // 主构造函数可以直接声明字段。
    public Vec2(var x: Float64, var y: Float64) {}

    public func length(): Float64 {
        sqrt(x * x + y * y)
    }

    // `mut` 表示这个 struct 方法会修改接收者。
    public mut func scale(k: Float64): Unit {
        x *= k
        y *= k
    }

    // 操作符也是函数。
    public operator func +(rhs: Vec2): Vec2 {
        Vec2(x + rhs.x, y + rhs.y)
    }

    public func toString(): String {
        "(${x.format(".1")}, ${y.format(".1")})"
    }
}

// 接口描述行为。实现接口使用 `<:`。
interface Named {
    prop label: String
}

// class 是引用类型，可以包含字段、构造函数、属性、实例方法、
// 静态方法和操作符函数。
class Person <: Named {
    let id: Int64
    private let rawName: String

    public init(id: Int64, name: String) {
        this.id = id
        this.rawName = name
    }

    // 属性使用 `get`，可变属性还可以提供 `set`。
    public prop label: String {
        get() {
            "${id}:${rawName}"
        }
    }

    public func greet(): Unit {
        println("Hello, I am ${rawName}.")
    }
}

// 类也可以使用紧凑构造函数，在参数列表中声明字段。
class Box<T> {
    public Box(var value: T) {}

    public func map<R>(f: (T) -> R): Box<R> {
        Box<R>(f(value))
    }
}

class Directory {
    private let people: Array<Person>

    public init(people: Array<Person>) {
        this.people = people
    }

    // 重载 `[]`，让这个小查询类型可以用下标访问。
    public operator func [](id: Int64): ?Person {
        for (person in people) {
            if (person.id == id) {
                return Some(person)
            }
        }
        return None
    }
}

// enum 是代数数据类型。构造器可以携带数据。
enum Command {
    Quit |
    Echo(String) |
    Move(Vec2) |
    Repeat(Int64, String)
}

// `Option<T>` 是包含 `Some(T)` 和 `None` 的 enum。
// `?T` 是 `Option<T>` 的简写。
func first<T>(items: Array<T>): ?T {
    if (items.size == 0) {
        return None
    }
    return Some(items[0])
}

func run(command: Command): Unit {
    match (command) {
        case Quit =>
            println("quit")
        case Echo(text) =>
            println(text)
        case Move(delta) =>
            println("move by ${delta.toString()}")
        case Repeat(times, text) =>
            for (_ in 0..times) {
                println(text)
            }
    }
}

func printNamed<T>(item: T): Unit where T <: Named {
    println(item.label)
}

// 函数可以按参数类型重载。
func twice(x: Int64): Int64 { x * 2 }
func twice(x: Float64): Float64 { x * 2.0 }

////////////////////////////////////////////////////////////
// 3. 绑定与基本类型
////////////////////////////////////////////////////////////

func learnBindings(): Unit {
    // 常见基本类型。
    let ok: Bool = true
    let signed: Int64 = -42
    let unsigned: UInt64 = 42
    let f32: Float32 = 1.25
    let f64: Float64 = 2.5

    println("${ok}, ${signed}, ${unsigned}, ${f32}, ${f64}")

    // 整数字面量支持常见进制前缀。
    let decimal: Int64 = 255
    let binary: Int64 = 0b11111111
    let octal: Int64 = 0o377
    let hex: Int64 = 0xFF
    println("${decimal}, ${binary}, ${octal}, ${hex}")

    // 数值转换是显式的。
    let count: Int64 = 3
    let price: Float64 = 19.95
    let total = Float64(count) * price
    println("total: ${total.format(".2")}")
}

////////////////////////////////////////////////////////////
// 4. 字符串、字符、元组与插值
////////////////////////////////////////////////////////////

func learnStringsAndNumbers(): Unit {
    let name = "Ada"
    let score = 99.5

    // 字符串插值使用 `${...}`。
    println("${name} scored ${score.format(".1")}")

    // `readln()` 读取字符串。做数值计算前要先解析。
    // let line = readln()
    // let n = Int64.parse(line)

    let csv = "red,green,blue"
    let parts = csv.split(",")
    println(parts[0])

    // `Rune` 表示 Unicode 字符。
    let letter: Rune = Rune(UInt32(65))
    let lineBreak: Rune = Rune(UInt32(10))
    let generated: Rune = Rune(UInt32(65))
    println("${letter}${lineBreak}${generated}")

    // 元组把固定数量的值组合在一起。
    let pair: (String, Int64) = ("answer", 42)
    let (word, value) = pair
    println("${word}: ${value}")
}

////////////////////////////////////////////////////////////
// 5. 控制流是表达式化的
////////////////////////////////////////////////////////////

func learnControlFlow(): Unit {
    let temperature = 22.5

    // `if` 可用于分支。
    if (temperature > 30.0) {
        println("hot")
    } else if (temperature < 10.0) {
        println("cold")
    } else {
        println("comfortable")
    }

    // `if` 也可以产生值。
    let status = if (temperature > 18.0) {
        "open"
    } else {
        "closed"
    }
    println(status)

    // `a..b` 是左闭右开区间。
    for (i in 0..3) {
        print("${i} ")
    }
    println()

    // `a..=b` 两端都包含。
    var sum: Int64 = 0
    for (i in 1..=100) {
        sum += i
    }
    println(sum)

    var x: Int64 = 4
    while (x > 0) {
        x -= 1
    }

    // `do-while` 至少执行一次循环体。
    do {
        x += 1
    } while (x < 2)

    while (true) {
        if (x < 2) {
            continue
        }
        break
    }
}

////////////////////////////////////////////////////////////
// 6. 数组与区间
////////////////////////////////////////////////////////////

func learnCollections(): Unit {
    // 数组字面量会推断元素类型。
    let primes = [2, 3, 5, 7, 11]
    println("first prime: ${primes[0]}")

    // 用重复初值创建固定长度 Array。
    var squares: Array<Int64> = Array(5, repeat: 0)
    for (i in 0..squares.size) {
        squares[i] = i * i
    }

    for (value in squares) {
        print("${value} ")
    }
    println()

    let maybeFirst = first(squares)
    if (maybeFirst.isSome()) {
        println("first square: ${maybeFirst.getOrThrow()}")
    }
}

////////////////////////////////////////////////////////////
// 7. 函数、闭包与重载
////////////////////////////////////////////////////////////

func learnFunctions(): Unit {
    // 函数类型使用箭头。
    var op: (Int64, Int64) -> Int64 = { a: Int64, b: Int64 => a + b }
    println(op(2, 3))

    op = { a: Int64, b: Int64 => a * b }
    println(op(2, 3))

    // 闭包可以捕获局部变量。
    let offset = 10
    let addOffset: (Int64) -> Int64 = { n: Int64 => n + offset }
    println(addOffset(5))

    // 函数本身可以作为值。
    let f: (Int64) -> Int64 = twice
    println(f(21))

    println(twice(21))
    println(twice(21.0).format(".1"))

    let boxed = Box<Int64>(21)
    let mapped = boxed.map({ n: Int64 => "value=${n}" })
    println(mapped.value)
}

////////////////////////////////////////////////////////////
// 8. struct、class、interface、属性与操作符
////////////////////////////////////////////////////////////

func learnStructsClassesAndInterfaces(): Unit {
    var a = Vec2(3.0, 4.0)
    let b = Vec2(1.0, 2.0)
    let c = a + b

    println(a.length().format(".1"))
    println(c.toString())

    a.scale(2.0)
    println(a.toString())

    let ada = Person(1, "Ada")
    ada.greet()

    // `Person` 实现了 `Named`，所以可以传给带 `where T <: Named`
    // 约束的泛型函数。
    printNamed<Person>(ada)

    let directory = Directory([
        Person(1, "Ada"),
        Person(2, "Grace"),
        Person(3, "Edsger")
    ])

    // 这里会调用 Directory.operator[]。
    let maybePerson = directory[2]
    if (maybePerson.isSome()) {
        let person = maybePerson.getOrThrow()
        println("found ${person.label}")
    }
}

////////////////////////////////////////////////////////////
// 9. enum、match 与 Option
////////////////////////////////////////////////////////////

func learnEnumsAndOptions(): Unit {
    let commands: Array<Command> = [
        Echo("ready"),
        Move(Vec2(5.0, -2.0)),
        Repeat(2, "again"),
        Quit
    ]

    for (command in commands) {
        run(command)
    }

    let directory = Directory([Person(1, "Ada")])
    let missing = directory[99]

    match (missing) {
        case Some(person) =>
            println(person.label)
        case None =>
            println("no person with that id")
    }

    // `if let` 风格条件适合只关心某个匹配成功分支的场景。
    if (directory[1].isSome()) {
        let person = directory[1].getOrThrow()
        println("hello ${person.label}")
    }
}

////////////////////////////////////////////////////////////
// 10. 标准库一瞥
////////////////////////////////////////////////////////////

func learnStandardLibrary(): Unit {
    // `std.math.*`
    println(sqrt(81.0))
    println(abs(-12))

    // `std.random.*`
    let rng = Random()
    let dice = rng.nextInt64(6) + 1
    println("dice: ${dice}")

    // `std.convert.*`
    let n = Int64.parse("123")
    let y = Float64.parse("3.5")
    println("${n}, ${y.format(".2")}")

    // 仓颉标准库还包含集合、I/O、时间、正则、并发、测试、
    // 反射、宏和 C 互操作等模块。
}
```

这份导览刻意保持紧凑，但展示了仓颉不同于普通 C 系语法的地方：
表达式有类型，`Option` 取代许多空值场景，枚举可以携带数据，接口可以
约束泛型代码，操作符和属性也是普通声明。
