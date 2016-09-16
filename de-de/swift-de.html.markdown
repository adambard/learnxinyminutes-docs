---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
  - ["Joey Huang", "http://github.com/kamidox"]  
  - ["Anthony Nguyen", "http://github.com/anthonyn60"]
translators:
    - ["Jonas Wippermann", "http://vfuc.co"]
filename: learnswift-de.swift
---

Swift ist eine Programmiersprache von Apple für die Entwicklung von iOS und OS X Applikationen. Swift wurde 2014 zu Apples WWDC Entwicklerkonferenz vorgestellt und wurde mit dem Ziel entwickelt, fehlerträchtigen Code zu vermeiden sowie mit Objective-C zu koexistieren. Es wird mit dem LLVM Compiler gebaut und ist ab Xcode 6+ verfügbar.

Das offizielle [Swift Programming Language](https://itunes.apple.com/us/book/swift-programming-language/id881256329) Buch von Apple ist kostenlos via iBooks verfügbar.

Außerdem hilfreich ist Apples [Getting Started Guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/RoadMapiOS/index.html), ein guter Einstiegspunkt mit komplettem Swift-Tutorial.

```swift
// importiere ein Modul
import UIKit

//
// MARK: Grundlagen
//

// Xcode unterstützt "Landmarks" um Code zu gliedern, sie werden in der Jump Bar aufgelistet
// MARK: Abschnitts-Markierung
// TODO: Zu erledigen
// FIXME: Zu beheben

// In Swift 2 wurden println und print zusammengefasst in eine print-Methode. Es wird automatisch ein Zeilenumbruch angehängt.
print("Hello, world!") // println ist jetzt print
print("Hello, world!", appendNewLine: false) // printen ohne Zeilenumbruch am Ende

// Variablen (var) können nach der Initialisierung verändert werden 
// Konstanten (let) können nach der Initialisierung NICHT verändert werden 

var myVariable = 42
let øπΩ = "value" // Unicode-Variablennamen
let π = 3.1415926
let convenience = "keyword" // Kontext-abhängiger Variablenname
let weak = "keyword"; let override = "another keyword" // Instruktionen können durch ein Semikolon aufgeteilt werden
let `class` = "keyword" // Nutze "Backticks" um Schlüsselwörter als Variablennamen zu verwenden
let explicitDouble: Double = 70 // Typ explizit festgelegt
let intValue = 0007 // 7
let largeIntValue = 77_000 // 77000
let label = "some text " + String(myVariable) // Casting
let piText = "Pi = \(π), Pi 2 = \(π * 2)" // String Interpolation

// Build-spezifische Werte
// benutzt -D build configuration
#if false
    print("not printed")
    let buildValue = 3
#else
    let buildValue = 7
#endif
print("Build value: \(buildValue)") // Build value: 7

/*
    Optionals ist ein Swift-Feature, welches ermöglicht, dass eine Variable entweder einen (`Some`) oder keinen (`None`) Wert hat

    Da Swift von jeder property einen Wert erwartet, muss sogar nil explizit als Optional festgelegt werden.

    Optional<T> ist ein Enum.
*/
var someOptionalString: String? = "optional" // Kann nil sein
// Genau wie oben, aber ? ist ein postfix operator (Syntax Candy)
var someOptionalString2: Optional<String> = "optional"

if someOptionalString != nil {
    // Ich bin nicht nil
    if someOptionalString!.hasPrefix("opt") {
        print("has the prefix")
    }
    
    let empty = someOptionalString?.isEmpty
}
someOptionalString = nil

// Implizit entpackter Optionalwert
var unwrappedString: String! = "Value is expected."
// Genau wie oben, aber ! ist ein postfix operator (noch mehr Syntax Candy)
var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Value is expected."

if let someOptionalStringConstant = someOptionalString {
    // hat einen (`Some`) Wert, nicht nil
    if !someOptionalStringConstant.hasPrefix("ok") {
        // hat keinen "ok"-Prefix
    }
}

// Swift unterstützt das festlegen von Werten eines beliebigen Typens
// AnyObject == id
// Im Gegensatz zum Objective-C `id`, funktioniert AnyObject mit jeglichen Werten (Class, Int, struct, etc)
var anyObjectVar: AnyObject = 7
anyObjectVar = "Changed value to a string, not good practice, but possible."

/*
    Ein Kommentar
    
    /*
        Verschachtelte Kommentare sind ebenfalls unterstützt
    */
*/

//
// MARK: Collections
//

/*
    Array und Dictionary-Typen sind structs. 
    Deswegen implizieren `let` und `var` bei der Initialisierung auch ob sie änderbar (var) oder unveränderlich (let) sind.
*/

// Array
var shoppingList = ["catfish", "water", "lemons"]
shoppingList[1] = "bottle of water"
let emptyArray = [String]() // let == unveränderlich
let emptyArray2 = Array<String>() // genau wie oben
var emptyMutableArray = [String]() // var == änderbar


// Dictionary
var occupations = [
    "Malcolm": "Captain",
    "kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
let emptyDictionary = [String: Float]() // let == unveränderlich
let emptyDictionary2 = Dictionary<String, Float>() // genau wie oben
var emptyMutableDictionary = [String: Float]() // var == änderbar


//
// MARK: Kontrollstruktur
//

// for-Schleife (array)
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        print("One!")
    } else {
        print("Not one!")
    }
}

// for-Schleife mit Indizes (array)
for index in myArray.indices {
    print("Value with index \(index) is \(myArray[index])")
}

// for-Schleife (dictionary)
var dict = ["one": 1, "two": 2]
for (key, value) in dict {
    print("\(key): \(value)")
}

// for-Schleife (range)
for i in -1...shoppingList.count {
    print(i)
}
shoppingList[1...2] = ["steak", "peacons"]
// ..< schließt letzte Nummer aus

// while-Schleife
var i = 1
while i < 1000 {
    i *= 2
}

// do-while-Schleife
do {
    print("hello")
} while 1 == 2

// Switch
// Sehr mächtig, wie `if` statement mit Syntax Candy
// Unterstützt Strings, Objekt-Instanzen und primitive Typen (Int, Double, etc)
let vegetable = "red pepper"
switch vegetable {
case "celery":
    let vegetableComment = "Add some raisins and make ants on a log."
case "cucumber", "watercress":
    let vegetableComment = "That would make a good tea sandwich."
case let localScopeValue where localScopeValue.hasSuffix("pepper"):
    let vegetableComment = "Is it a spicy \(localScopeValue)?"
default: // notwendig (um alle möglichen Eingaben zu verarbeiten)
    let vegetableComment = "Everything tastes good in soup."
}


//
// MARK: Funktionen
//

// Funktionen sind ein sogenannter "first-class" Typ, was bedeutet, dass sie
// in Funktionen geschachtelt werden und "herumgereicht" werden können

// Funktion mit Swift header Dokumentation

/**
    Eine Grüß-Funktion

    - Ein Aufzählungspunkt
    - Ein weiterer Aufzählungspunkt in der Dokumentation

    :param: name Ein Name
    :param: day Ein Tag
    :returns: Ein String, der Name und Tag beinhält.
*/
func greet(name: String, day: String) -> String {
    return "Hello \(name), today is \(day)."
}
greet("Bob", "Tuesday")

// Ähnlich wie oben, bloß anderes Funktions-Parameter-Verhalten
func greet2(#requiredName: String, externalParamName localParamName: String) -> String {
    return "Hello \(requiredName), the day is \(localParamName)"
}
greet2(requiredName:"John", externalParamName: "Sunday")


// Funktion, welche mehrere Werte in einem Tupel zurückgibt
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// Ignoriere Tupel-(oder andere)Werte mit _ (Unterstrich)
let (_, price1, _) = pricesTuple // price1 == 3.69
print(price1 == pricesTuple.1) // true
print("Gas price: \(price)")

// Variierende Argumente..
func setup(numbers: Int...) {
    // .. liegen als Array vor
    let number = numbers[0]
    let argCount = numbers.count
}

// Funktionen übergeben und zurückgeben
func makeIncrementer() -> (Int -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

// Übergabe via Referenz ("Pass by reference")
func swapTwoInts(inout a: Int, inout b: Int) {
    let tempA = a
    a = b
    b = tempA
}
var someIntA = 7
var someIntB = 3
swapTwoInts(&someIntA, &someIntB)
print(someIntB) // 7


//
// MARK: Closures
//
var numbers = [1, 2, 6]

// Funktionen sind besondere Closures ({})

// Closure Beispiel
// `->` teilt Parameter und Rückgabe-Typ
// `in` teilt den Closure Header vom Body
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})


// Wenn der Typ bekannt ist, wie oben, kann folgendes getan werden
numbers = numbers.map({ number in 3 * number })
// oder sogar dies
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// "Schleppende Closure" (Trailing Closure)
numbers = sorted(numbers) { $0 > $1 }

print(numbers) // [18, 6, 3]

// Sehr verkürzt, da sich der Typ durch den < Operator ableiten lässt

numbers = sorted(numbers, < )

print(numbers) // [3, 6, 18]

//
// MARK: Strukturen
// (häufig einfach structs)
//

// Structures und Klassen haben sehr ähnliche Fähigkeiten
struct NamesTable {
    let names = [String]()
    
    // Eigendefiniertes subscript
    subscript(index: Int) -> String {
        return names[index]
    }
}


// Strukturen haben eine automatisch generierte, designierte Initialisierungsfunktion
let namesTable = NamesTable(names: ["Me", "Them"])
let name = namesTable[1]
print("Name is \(name)") // Name is Them

//
// MARK: Klassen
//

// Klassen, Strukturen und deren Member haben drei Ebenen der Zugriffskontrolle
// Es gibt: internal (default), public, private

public class Shape {
    public func getArea() -> Int {
        return 0;
    }
}

// Alle Methoden und Properties einer Klasse sind public
// Wenn das einfache Ziel ist, Daten in einem strukturierten Objekt zu halten,
// sollte ein `struct` verwendet werden

internal class Rect: Shape {
    var sideLength: Int = 1
    
    // Eigendefinierte Getter und Setter für die Property
    private var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue` ist eine implizite Variable, welche in Settern verfügbar ist
            sideLength = newValue / 4
        }
    }
    
    // "Lazy" (faules) Laden einer Property, sie bleibt uninitialisiert (nil),
    // bis sie aufgerufen wird
    lazy var subShape = Rect(sideLength: 4)
    
    // Wenn kein eigendefinierter Getter/Setter notwendig ist,
    // aber trotzdem Code vor und nach dem Setzen eines Variablenwertes laufen soll,
    // kann "willSet" und "didSet" benutzt werden
    var identifier: String = "defaultID" {
        // der `willSet` Parameter wird der Variablenname für den neuen Wert sein 
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }
    
    init(sideLength: Int) {
        self.sideLength = sideLength
        // super.init muss immer aufgerufen werden, wenn eigene Properties initialisiert werden
        super.init()
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

// Eine simple `Square`-Klasse erbt von/erweitert `Rect`
class Square: Rect {
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// Casten der Instanz
let aShape = mySquare as Shape

// Vergleiche Instanzen, nicht äquivalent zum == , welches Objekte vergleicht ("equal to") 
if mySquare === mySquare {
    print("Yep, it's mySquare")
}

// Optionale Initialisierung
class Circle: Shape {
    var radius: Int
    override func getArea() -> Int {
        return 3 * radius * radius
    }
    
    // Ein Fragezeichen nach `init` ist eine optionale Initialisierung,
    // welche nil zurückgeben kann
    init?(radius: Int) {
        self.radius = radius
        super.init()
        
        if radius <= 0 {
            return nil
        }
    }
}

var myCircle = Circle(radius: 1)
print(myCircle?.getArea())    // Optional(3)
print(myCircle!.getArea())    // 3
var myEmptyCircle = Circle(radius: -1)
print(myEmptyCircle?.getArea())    // "nil"
if let circle = myEmptyCircle {
    // wird nicht ausgeführt, da myEmptyCircle nil ist
    print("circle is not nil")
}


//
// MARK: Enums
//

// Enums können optional einen eigenen Typen haben
// Wie Klassen auch können sie Methoden haben

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


// Enum-Werte können vereinfacht geschrieben werden, es muss nicht der Enum-Typ
// genannt werden, wenn die Variable explizit deklariert wurde

var suitValue: Suit = .Hearts

// Nicht-Integer-Enums brauchen direkt zugewiesene "Rohwerte"
enum BookName: String {
    case John = "John"
    case Luke = "Luke"
}
print("Name: \(BookName.John.rawValue)")

// Enum mit assoziierten Werten
enum Furniture {
    // mit Int assoziiert
    case Desk(height: Int)
    // mit String und Int assoziiert
    case Chair(String, Int)
    
    func description() -> String {
        switch self {
        case .Desk(let height):
            return "Desk with \(height) cm"
        case .Chair(let brand, let height):
            return "Chair of \(brand) with \(height) cm"
        }
    }
}

var desk: Furniture = .Desk(height: 80)
print(desk.description())     // "Desk with 80 cm"
var chair = Furniture.Chair("Foo", 40)
print(chair.description())    // "Chair of Foo with 40 cm"


//
// MARK: Protokolle
//

// Protokolle (`protocol`s) können verlangen, dass entsprechende
// Typen spezifische Instanz-Properties, Instanz/Klassen-Methoden,
// Operatoren oder Subscripts implementieren/haben

protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

// Protocols mit @objc deklariert ermöglichen optionale Funktionen,
// welche es ermöglichen, abzufragen ob ein Typ einem Protokoll entspricht
@objc protocol TransformShape {
    optional func reshaped()
    optional func canReshape() -> Bool
}

class MyShape: Rect {
    var delegate: TransformShape?
    
    func grow() {
        sideLength += 2

        // Ein Fragezeichen nach einer optionalen Property, Methode oder Subscript
        // ignoriert elegant Nil-Werte und geben nil zurück, anstatt einen Laufzeitfehler zu werfen
        // Dies wird "optional Chaining" (optionale Verkettung) genannt
        if let allow = self.delegate?.canReshape?() {
            // frage erst nach delegate, dann nach Methode
            self.delegate?.reshaped?()
        }
    }
}


//
// MARK: Sonstiges
//

// `extension`s: (Erweiterungen), erweitere Typen um zusätzliche Funktionalität

// Square entspricht jetzt dem `Printable` Protokoll
extension Square: Printable {
    var description: String {
        return "Area: \(self.getArea()) - ID: \(self.identifier)"
    }
}

print("Square: \(mySquare)")

// Standardtypen können ebenfalls erweitert werden
extension Int {
    var customProperty: String {
        return "This is \(self)"
    }
    
    func multiplyBy(num: Int) -> Int {
        return num * self
    }
}

print(7.customProperty) // "This is 7"
print(14.multiplyBy(3)) // 42


//Generics: Ähnlich zu Java und C#. Nutze das `where` keyword um die Bedingung
// des Generics festzulegen

func findIndex<T: Equatable>(array: [T], valueToFind: T) -> Int? {
    for (index, value) in enumerate(array) {
        if value == valueToFind {
            return index
        }
    }
    return nil
}
let foundAtIndex = findIndex([1, 2, 3, 4], 3)
print(foundAtIndex == 2) // true

// Operatoren:
// Eigendefinierte Operatoren können mit diesen Zeichen beginnen:
//      / = - + * % < > ! & | ^ . ~
// oder
// Unicode Mathematik, Symbole, Pfeile, Dingbat, und Linien/Box - Zeichen
prefix operator !!! {}


// Ein Prefix-Operator, welcher die Seitenlänge verdreifacht
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// Aktueller Wert
print(mySquare.sideLength) // 4

// Wert nach verwendung des eigenen Operators
!!!mySquare
print(mySquare.sideLength) // 12
```
