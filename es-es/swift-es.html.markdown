---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
  - ["Joey Huang", "http://github.com/kamidox"]  
  - ["Anthony Nguyen", "http://github.com/anthonyn60"]
translators:
    - ["David Hsieh", "http://github.com/deivuh"]
lang: es-es
filename: learnswift-es.swift
---

Swift es un lenguaje de programación para el desarrollo en iOS y OS X creado
por Apple. Diseñado para coexistir con Objective-C y ser más resistente contra
el código erroneo, Swift fue introducido en el 2014 en el WWDC, la conferencia
de desarrolladores de Apple.

Véase también la guía oficial de Apple, [getting started guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/DevelopiOSAppsSwift/), el cual tiene un completo tutorial de Swift.


```swift
// Importar un módulo
import UIKit

//
// MARK: Básicos
//

// XCode soporta referencias para anotar tu código y agregarlos a lista de la
// barra de saltos.
// MARK: Marca de sección
// TODO: Hacer algo pronto
// FIXME: Arreglar este código

// En Swift 2, println y print fueron combinados en un solo método print.
// Print añade una nueva línea automáticamente.
print("Hola, mundo") // println ahora es print
print("Hola, mundo", appendNewLine: false) // print sin agregar nueva línea

// Valores de variables (var) pueden cambiar después de ser asignados
// Valores de constrantes (let) no pueden cambiarse después de ser asignados

var myVariable = 42
let øπΩ = "value" // nombres de variable unicode
let π = 3.1415926
let convenience = "keyword" // nombre de variable contextual
// Las declaraciones pueden ser separadas por punto y coma (;)
let weak = "keyword"; let override = "another keyword"
// Los acentos abiertos (``) permiten utilizar palabras clave como nombres de
// variable
let `class` = "keyword"
let explicitDouble: Double = 70
let intValue = 0007 // 7
let largeIntValue = 77_000 // 77000
let label = "some text " + String(myVariable) // Conversión (casting)
let piText = "Pi = \(π), Pi 2 = \(π * 2)" // Interpolación de string

// Valores específicos de la compilación (build)
// utiliza la configuración -D
#if false
    print("No impreso")
    let buildValue = 3
#else
    let buildValue = 7
#endif
print("Build value: \(buildValue)") // Build value: 7

/*
    Las opcionales son un aspecto del lenguaje Swift que permite el
    almacenamiento de un valor `Some` (algo) o `None` (nada).

    Debido a que Swift requiere que cada propiedad tenga un valor,
    hasta un valor 'nil' debe de ser explicitamente almacenado como un
    valor opcional.

    Optional<T> es un enum.
*/
var someOptionalString: String? = "opcional" // Puede ser nil
// Al igual que lo anterior, pero ? es un operador postfix (sufijo)
var someOptionalString2: Optional<String> = "opcional"

if someOptionalString != nil {
    // No soy nil
    if someOptionalString!.hasPrefix("opt") {
        print("Tiene el prefijo")
    }

    let empty = someOptionalString?.isEmpty
}
someOptionalString = nil

// Opcional implícitamente desenvuelto
var unwrappedString: String! = "Un valor esperado."
// Al igual que lo anterior, pero ! es un operador postfix (sufijo)
var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Un valor esperado."

if let someOptionalStringConstant = someOptionalString {
    // tiene valor `Some` (algo), no nil
    if !someOptionalStringConstant.hasPrefix("ok") {
        // No tiene el prefijo
    }
}

// Swift tiene soporte de almacenamiento para cualquier tipo de valor.
// AnyObject == id
// A diferencia de Objective-C `id`, AnyObject funciona con cualquier
// valor (Class, Int, struct, etc)
var anyObjectVar: AnyObject = 7
anyObjectVar = "Cambiado a un valor string, no es buena práctica, pero posible."

/*
    Comentar aquí

    /*
        Comentarios anidados también son soportados
    */
*/

//
// MARK: Colecciones
//

/*
    Tipos Array (arreglo) y Dictionary (diccionario) son structs (estructuras).
    Así que `let` y `var` también indican si son mudables (var) o
    inmutables (let) durante la declaración de sus tipos.    
*/

// Array (arreglo)
var shoppingList = ["catfish", "water", "lemons"]
shoppingList[1] = "bottle of water"
let emptyArray = [String]() // let == inmutable
let emptyArray2 = Array<String>() // igual que lo anterior
var emptyMutableArray = [String]() // var == mudable


// Dictionary (diccionario)
var occupations = [
    "Malcolm": "Captain",
    "kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
let emptyDictionary = [String: Float]() // let == inmutable
let emptyDictionary2 = Dictionary<String, Float>() // igual que lo anterior
var emptyMutableDictionary = [String: Float]() // var == mudable


//
// MARK: Flujo de control
//

// Ciclo for (array)
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        print("Uno!")
    } else {
        print("No es uno!")
    }
}

// Ciclo for (dictionary)
var dict = ["uno": 1, "dos": 2]
for (key, value) in dict {
    print("\(key): \(value)")
}

// Ciclo for (range)
for i in -1...shoppingList.count {
    print(i)
}
shoppingList[1...2] = ["steak", "peacons"]
// Utilizar ..< para excluir el último valor

// Ciclo while
var i = 1
while i < 1000 {
    i *= 2
}

// Ciclo do-while
do {
    print("Hola")
} while 1 == 2

// Switch
// Muy potente, se puede pensar como declaraciones `if` con _azúcar sintáctico_
// Soportan String, instancias de objetos, y primitivos (Int, Double, etc)
let vegetable = "red pepper"
switch vegetable {
case "celery":
    let vegetableComment = "Add some raisins and make ants on a log."
case "cucumber", "watercress":
    let vegetableComment = "That would make a good tea sandwich."
case let localScopeValue where localScopeValue.hasSuffix("pepper"):
    let vegetableComment = "Is it a spicy \(localScopeValue)?"
default: // obligatorio (se debe cumplir con todos los posibles valores de entrada)
    let vegetableComment = "Everything tastes good in soup."
}


//
// MARK: Funciones
//

// Funciones son un tipo de primera-clase, quiere decir que pueden ser anidados
// en funciones y pueden ser pasados como parámetros

// Función en documentación de cabeceras Swift (formato reStructedText)

/**
    Una operación de saludo

    - Una viñeta en la documentación
    - Otra viñeta en la documentación

    :param: name Un nombre
    :param: day Un día
    :returns: Un string que contiene el valor de name y day
*/
func greet(name: String, day: String) -> String {
    return "Hola \(name), hoy es \(day)."
}
greet("Bob", "Martes")

// Similar a lo anterior, a excepción del compartamiento de los parámetros
// de la función
func greet2(requiredName: String, externalParamName localParamName: String) -> String {
    return "Hola \(requiredName), hoy es el día \(localParamName)"
}
greet2(requiredName:"John", externalParamName: "Domingo")

// Función que devuelve múltiples valores en una tupla
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// Ignorar tupla (u otros) valores utilizando _ (guión bajo)
let (_, price1, _) = pricesTuple // price1 == 3.69
print(price1 == pricesTuple.1) // true
print("Gas price: \(price)")

// Cantidad variable de argumentos
func setup(numbers: Int...) {
    // Es un arreglo
    let number = numbers[0]
    let argCount = numbers.count
}

// Pasando y devolviendo funciones
func makeIncrementer() -> (Int -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

// Pasando como referencia
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
// MARK: Closures (Clausuras)
//
var numbers = [1, 2, 6]

// Las funciones son un caso especial de closure ({})

// Ejemplo de closure.
// `->` Separa los argumentos del tipo de retorno
// `in` Separa la cabecera del cuerpo del closure
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})

// Cuando se conoce el tipo, como en lo anterior, se puede hacer esto
numbers = numbers.map({ number in 3 * number })
// o esto
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// Closure restante
numbers = sorted(numbers) { $0 > $1 }

print(numbers) // [18, 6, 3]

// Bastante corto, debido a que el operador < infiere los tipos

numbers = sorted(numbers, < )

print(numbers) // [3, 6, 18]

//
// MARK: Estructuras
//

// Las estructuras y las clases tienen capacidades similares
struct NamesTable {
    let names = [String]()

    // Subscript personalizado
    subscript(index: Int) -> String {
        return names[index]
    }
}

// Las estructuras tienen un inicializador designado autogenerado (implícitamente)
let namesTable = NamesTable(names: ["Me", "Them"])
let name = namesTable[1]
print("Name is \(name)") // Name is Them

//
// MARK: Clases
//

// Las clases, las estructuras y sus miembros tienen tres niveles de control de acceso
// Éstos son: internal (predeterminado), public, private

public class Shape {
    public func getArea() -> Int {
        return 0;
    }
}

// Todos los métodos y las propiedades de una clase son public (públicas)
// Si solo necesitas almacenar datos en un objecto estructurado,
// debes de utilizar `struct`

internal class Rect: Shape {
    var sideLength: Int = 1

    // Getter y setter personalizado
    private var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue` es una variable implícita disponible para los setters
            sideLength = newValue / 4
        }
    }

    // Lazily loading (inicialización bajo demanda) a una propiedad
    // subShape queda como nil (sin inicializar) hasta que getter es llamado
    lazy var subShape = Rect(sideLength: 4)

    // Si no necesitas un getter y setter personalizado
    // pero aún quieres ejecutar código antes y después de hacer get o set
    // a una propiedad, puedes utilizar `willSet` y `didSet`    
    var identifier: String = "defaultID" {
        // El argumento `willSet` será el nombre de variable para el nuevo valor
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }

    init(sideLength: Int) {
        self.sideLength = sideLength
        // Siempre poner super.init de último al momento de inicializar propiedades
        // personalizadas
        super.init()
    }

    func shrink() {
        if sideLength > 0 {
            sideLength -= 1
        }
    }

    override func getArea() -> Int {
        return sideLength * sideLength
    }
}

// Una clase simple `Square` que extiende de `Rect`
class Square: Rect {
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// Conversión de tipo de instancia
let aShape = mySquare as Shape

// Comparar instancias, no es igual a == que compara objetos (equal to)
if mySquare === mySquare {
    print("Yep, it's mySquare")
}

// Inicialización (init) opcional
class Circle: Shape {
    var radius: Int
    override func getArea() -> Int {
        return 3 * radius * radius
    }

    // Un signo de interrogación como sufijo después de `init` es un init opcional
    // que puede devolver nil    
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
    // no será ejecutado debido a que myEmptyCircle es nil
    print("circle is not nil")
}


//
// MARK: Enums
//


// Los enums pueden ser opcionalmente de un tipo específico o de su propio tipo
// Al igual que las clases, pueden contener métodos

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

// Los valores de enum permite la sintaxis corta, sin necesidad de poner
// el tipo del enum cuando la variable es declarada de manera explícita
var suitValue: Suit = .Hearts

// Enums de tipo no-entero requiere asignaciones de valores crudas directas
enum BookName: String {
    case John = "John"
    case Luke = "Luke"
}
print("Name: \(BookName.John.rawValue)")

// Enum con valores asociados
enum Furniture {
    // Asociación con Int
    case Desk(height: Int)
    // Asociación con String e Int
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
// MARK: Protocolos
//

// `protocol` puede requerir que los tipos tengan propiedades
// de instancia específicas, métodos de instancia, métodos de tipo,
// operadores, y subscripts


protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

// Protocolos declarados con @objc permiten funciones opcionales,
// que te permite evaluar conformidad
@objc protocol TransformShape {
    optional func reshaped()
    optional func canReshape() -> Bool
}

class MyShape: Rect {
    var delegate: TransformShape?

    func grow() {
        sideLength += 2

        // Pon un signo de interrogación después de la propiedad opcional,
        // método, o subscript para ignorar un valor nil y devolver nil
        // en lugar de  tirar un error de tiempo de ejecución
        // ("optional chaining")        
        if let allow = self.delegate?.canReshape?() {
            // test for delegate then for method
            self.delegate?.reshaped?()
        }
    }
}


//
// MARK: Otros
//

// `extension`: Agrega funcionalidades a tipos existentes

// Square ahora se "conforma" al protocolo `Printable`
extension Square: Printable {
    var description: String {
        return "Area: \(self.getArea()) - ID: \(self.identifier)"
    }
}

print("Square: \(mySquare)")

// También puedes hacer extend a tipos prefabricados (built-in)
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

// Generics: Similar Java y C#. Utiliza la palabra clave `where` para
// especificar los requerimientos de los genéricos.

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

// Operadores:
// Operadores personalizados puede empezar con los siguientes caracteres:
//      / = - + * % < > ! & | ^ . ~
// o
// Caracteres unicode: math, symbol, arrow, dingbat, y line/box.
prefix operator !!! {}

// Un operador prefix que triplica la longitud del lado cuando es utilizado
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// Valor actual
print(mySquare.sideLength) // 4

// Cambiar la longitud del lado utilizando el operador !!!,
// incrementa el tamaño por 3
!!!mySquare
print(mySquare.sideLength) // 12
```
