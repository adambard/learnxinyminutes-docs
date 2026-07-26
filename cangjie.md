---
name: Cangjie
filename: learncangjie.cj
contributors:
    - ["sam5440"]
---

Cangjie is a statically typed, general-purpose programming language.
It combines a C-family surface syntax with type inference, expression-oriented
control flow, algebraic enums, pattern matching, classes, interfaces,
generics, properties, function values, and operator overloading.

```text
// Source files begin with a package declaration.
package learnxinyminutes

// Imports come after the package declaration.
import std.convert.*
import std.math.*
import std.random.*

// With the Cangjie SDK, a single file can be compiled with:
//     cjc learncangjie.cj
// Larger projects normally use `cjpm` and a `cjpm.toml` manifest.

// Single-line comments start with two slashes.
/*
Multi-line comments look like this.
*/

////////////////////////////////////////////////////////////
// 1. Program entry, output, input, and variables
////////////////////////////////////////////////////////////

// `main` is the entry point of an executable program.
main(): Int64 {
    // `println` writes a line to standard output.
    println("Hello, Cangjie!")

    // `print` writes without appending a newline.
    print("This prompt stays on the same line: ")
    println("done")

    // `readln()` reads one line from standard input as a String.
    // Keep this commented so the tour can run without waiting for input.
    // let rawInput = readln()
    // let parsedNumber = Int64.parse(rawInput)

    // Create an immutable binding with `let`.
    let language: String = "Cangjie"

    // Create a mutable variable with `var`.
    var releaseYear: Int64 = 2024
    releaseYear += 1

    // Type annotations are often optional for local variables.
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
// 2. Declarations used later in this tour
////////////////////////////////////////////////////////////

// Cangjie has `struct` for value-like records.
struct Vec2 {
    // A primary constructor can declare fields directly.
    public Vec2(var x: Float64, var y: Float64) {}

    public func length(): Float64 {
        sqrt(x * x + y * y)
    }

    // `mut` marks a struct method that changes the receiver.
    public mut func scale(k: Float64): Unit {
        x *= k
        y *= k
    }

    // Operators are functions too.
    public operator func +(rhs: Vec2): Vec2 {
        Vec2(x + rhs.x, y + rhs.y)
    }

    public func toString(): String {
        "(${x.format(".1")}, ${y.format(".1")})"
    }
}

// Interfaces describe behavior. Implementing types use `<:`.
interface Named {
    prop label: String
}

// Classes are reference types and can carry fields, constructors, properties,
// instance methods, static methods, and operator functions.
class Person <: Named {
    let id: Int64
    private let rawName: String

    public init(id: Int64, name: String) {
        this.id = id
        this.rawName = name
    }

    // Properties use `get` and, for mutable properties, `set`.
    public prop label: String {
        get() {
            "${id}:${rawName}"
        }
    }

    public func greet(): Unit {
        println("Hello, I am ${rawName}.")
    }
}

// A compact class constructor can declare fields in the parameter list.
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

    // Overload `[]` to make a small lookup type.
    public operator func [](id: Int64): ?Person {
        for (person in people) {
            if (person.id == id) {
                return Some(person)
            }
        }
        return None
    }
}

// Enums are algebraic data types. Constructors can hold data.
enum Command {
    Quit |
    Echo(String) |
    Move(Vec2) |
    Repeat(Int64, String)
}

// `Option<T>` is an enum with `Some(T)` and `None`.
// `?T` is shorthand for `Option<T>`.
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

// Overload functions by parameter types.
func twice(x: Int64): Int64 { x * 2 }
func twice(x: Float64): Float64 { x * 2.0 }

////////////////////////////////////////////////////////////
// 3. Bindings and basic types
////////////////////////////////////////////////////////////

func learnBindings(): Unit {
    // Common primitive types.
    let ok: Bool = true
    let signed: Int64 = -42
    let unsigned: UInt64 = 42
    let f32: Float32 = 1.25
    let f64: Float64 = 2.5

    println("${ok}, ${signed}, ${unsigned}, ${f32}, ${f64}")

    // Integer literals support familiar base prefixes.
    let decimal: Int64 = 255
    let binary: Int64 = 0b11111111
    let octal: Int64 = 0o377
    let hex: Int64 = 0xFF
    println("${decimal}, ${binary}, ${octal}, ${hex}")

    // Numeric conversions are explicit.
    let count: Int64 = 3
    let price: Float64 = 19.95
    let total = Float64(count) * price
    println("total: ${total.format(".2")}")
}

////////////////////////////////////////////////////////////
// 4. Strings, characters, tuples, and interpolation
////////////////////////////////////////////////////////////

func learnStringsAndNumbers(): Unit {
    let name = "Ada"
    let score = 99.5

    // String interpolation uses `${...}`.
    println("${name} scored ${score.format(".1")}")

    // `readln()` reads a String. Parse it before numeric work.
    // let line = readln()
    // let n = Int64.parse(line)

    let csv = "red,green,blue"
    let parts = csv.split(",")
    println(parts[0])

    // `Rune` represents a Unicode character.
    let letter: Rune = Rune(UInt32(65))
    let lineBreak: Rune = Rune(UInt32(10))
    let generated: Rune = Rune(UInt32(65))
    println("${letter}${lineBreak}${generated}")

    // Tuples group a fixed number of values.
    let pair: (String, Int64) = ("answer", 42)
    let (word, value) = pair
    println("${word}: ${value}")
}

////////////////////////////////////////////////////////////
// 5. Control flow is expression-oriented
////////////////////////////////////////////////////////////

func learnControlFlow(): Unit {
    let temperature = 22.5

    // `if` can be used for branching.
    if (temperature > 30.0) {
        println("hot")
    } else if (temperature < 10.0) {
        println("cold")
    } else {
        println("comfortable")
    }

    // `if` can also produce a value.
    let status = if (temperature > 18.0) {
        "open"
    } else {
        "closed"
    }
    println(status)

    // `a..b` is left-closed and right-open.
    for (i in 0..3) {
        print("${i} ")
    }
    println()

    // `a..=b` includes both ends.
    var sum: Int64 = 0
    for (i in 1..=100) {
        sum += i
    }
    println(sum)

    var x: Int64 = 4
    while (x > 0) {
        x -= 1
    }

    // `do-while` runs its body at least once.
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
// 6. Arrays and ranges
////////////////////////////////////////////////////////////

func learnCollections(): Unit {
    // Array literals infer their element type.
    let primes = [2, 3, 5, 7, 11]
    println("first prime: ${primes[0]}")

    // Create a fixed-size Array with a repeated initial value.
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
// 7. Functions, closures, and overloading
////////////////////////////////////////////////////////////

func learnFunctions(): Unit {
    // Function types use arrows.
    var op: (Int64, Int64) -> Int64 = { a: Int64, b: Int64 => a + b }
    println(op(2, 3))

    op = { a: Int64, b: Int64 => a * b }
    println(op(2, 3))

    // Closures can capture local variables.
    let offset = 10
    let addOffset: (Int64) -> Int64 = { n: Int64 => n + offset }
    println(addOffset(5))

    // Functions are values.
    let f: (Int64) -> Int64 = twice
    println(f(21))

    println(twice(21))
    println(twice(21.0).format(".1"))

    let boxed = Box<Int64>(21)
    let mapped = boxed.map({ n: Int64 => "value=${n}" })
    println(mapped.value)
}

////////////////////////////////////////////////////////////
// 8. Structs, classes, interfaces, properties, operators
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

    // `Person` implements `Named`, so it can be used by a generic function
    // constrained with `where T <: Named`.
    printNamed<Person>(ada)

    let directory = Directory([
        Person(1, "Ada"),
        Person(2, "Grace"),
        Person(3, "Edsger")
    ])

    // This calls Directory.operator[].
    let maybePerson = directory[2]
    if (maybePerson.isSome()) {
        let person = maybePerson.getOrThrow()
        println("found ${person.label}")
    }
}

////////////////////////////////////////////////////////////
// 9. Enums, match, and Option
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

    // `if let` style conditions are concise for one successful pattern.
    if (directory[1].isSome()) {
        let person = directory[1].getOrThrow()
        println("hello ${person.label}")
    }
}

////////////////////////////////////////////////////////////
// 10. A tiny bit of the standard library
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

    // Cangjie also has packages for collections, I/O, time, regex,
    // concurrency, testing, reflection, macros, and C interoperability.
}
```

The syntax above is intentionally compact, but it shows the parts that make
Cangjie feel different from a plain C-family language: expressions have types,
`Option` replaces many null-like cases, enums carry data, interfaces can
constrain generic code, and operators and properties are regular declarations.
