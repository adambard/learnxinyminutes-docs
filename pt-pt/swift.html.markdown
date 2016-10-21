---
language: swift
filename: learnswift-pt.swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
  - ["Joey Huang", "http://github.com/kamidox"]
  - ["Anthony Nguyen", "http://github.com/anthonyn60"]
  - ["Clayton Walker", "https://github.com/cwalk"]
translators:
  - ["João Costa", "https://github.com/joaofcosta"]
lang: pt-pt
---

Swift é uma linguagem de programação criada pela Apple para o desenvolvimento em iOS e OS X.
Desenhada de forma a coexistir com Objective-C e ser mais resiliente contra código errôneo, a linguagem Swift foi introduzida em 2014 na conferência para desenvolvedores WWDC da Apple.
Swift usa o compilador LLVM incluido no XCode 6+.

O livro oficial [Swift Programming Language](https://itunes.apple.com/us/book/swift-programming-language/id881256329) da Apple está agora disponivel via iBooks.

Consulta também o [guia de iniciação](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/DevelopiOSAppsSwift/) da Apple, que contêm um tutorial completo em Swift.

```swift
// importar um módulo
import UIKit

//
// MARK: Básico
//

// O Xcode suporta landmarks para anotação de código e lista-as na jump bar
// MARK: Marco de secção (MARK)
// TODO: Algo a fazer em breve
// FIXME: Reparar este código

// Em Swift 2, println e print foram unidos num só método print. O print automaticamente acrescenta uma nova linha.
print("Hello, world") // println mudou para print
print("Hello, world", appendNewLine: false) // imprimir sem acrescentar uma nova linha

// variáveis (var) podem ser modificadas depois de inicializadas
// constantes (let) NÂO podem ser modificadas depois de inicializadas

var myVariable = 42
let øπΩ = "value" // nomes de variáveis em unicode
let π = 3.1415926
let convenience = "keyword" // nome de variável contextual
let weak = "keyword"; let override = "another keyword" // expressões podem ser separadas com ';'
let `class` = "keyword" // plicals permitem que keywords sejam usadas como nomes de vartiáveis
let explicitDouble: Double = 70
let intValue = 0007 // 7
let largeIntValue = 77_000 // 77000
let label = "some text " + String(myVariable) // Casting
let piText = "Pi = \(π), Pi 2 = \(π * 2)" // interpolação de Strings

// Valores especificos à build
// usam a configuração de build -D
#if false
    print("Not printed")
    let buildValue = 3
#else
    let buildValue = 7
#endif
print("Build value: \(buildValue)") // Build value: 7

/*
    Optionals são um dos recursos de Swift, Optionals tanto podem conter
    um valor ou conter nil (sem valor) que indica que não existe um valor.
    Adicionar um ponto de exclamção (?) após definir o tipo declara
    esse valor como um Optional.

    Como Swift requere que todas as propriedades tenham um valor, até nil
    tem que ser explicitamente guardado como um valor Optional.

    Optional<T> é uma enumeração.
*/
var someOptionalString: String? = "optional" // Pode assumir o valor nil
// Igual ao de cima, mas ? é um operando pósfixo (açúcar sintático)
var someOptionalString2: Optional<String> = "optional"

if someOptionalString != nil {
    // Não sou nil
    if someOptionalString!.hasPrefix("opt") {
        print("has the prefix")
    }

    let empty = someOptionalString?.isEmpty
}
someOptionalString = nil

/*
    Tentar usar ! para aceder a Optional com valor não existente, ou seja, nil,
    causa em erro de execução.
    É necessário ter sempre a certeza que um Optional não tem valor nil
    antes de usar ! para fazer 'force-unwrap' ao seu valor.
*/

// Optional implicitamente desembrulhado
var unwrappedString: String! = "Value is expected."
// O mesmo de cima, mas ! é um operando pósfixo (mais açúcar sintático)
var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Value is expected."

if let someOptionalStringConstant = someOptionalString {
    // Tem um valor diferente de nil
    if !someOptionalStringConstant.hasPrefix("ok") {
        // Não tem o prefixo
    }
}

// Swift tem suporte para guardar valores de qualquer tipo.
// AnyObject == id
// Ao contrátio do `id` de Objective-C, AnyObject funciona com qualquer valor (Class, Int, struct, etc.)
var anyObjectVar: AnyObject = 7
anyObjectVar = "Changed value to a string, not good practice, but possible."

/*
    Comentar aqui

    /*
        Também é possível fazer comentários aninhados
    */
*/

//
// MARK: Coleções (Collections)
//

/*
    Os tipos Array e Dictionary são structs e, portanto, `let` e `var`
    também indicam se eles são mutáveis (var) or imutáveis (let)
    na altura em que se declaram estes tipos.
*/

// Array
var shoppingList = ["catfish", "water", "lemons"]
shoppingList[1] = "bottle of water"
let emptyArray = [String]() // let == imutável
let emptyArray2 = Array<String>() // mesmo de cima
var emptyMutableArray = [String]() // var == mutável


// Dictionary
var occupations = [
    "Malcolm": "Captain",
    "kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
let emptyDictionary = [String: Float]() // let == imutável
let emptyDictionary2 = Dictionary<String, Float>() // mesmo de cima
var emptyMutableDictionary = [String: Float]() // var == mutável


//
// MARK: Controlo de Fluxo (Control Flow)
//

// for loop (array)
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        print("One!")
    } else {
        print("Not one!")
    }
}

// for loop (dictionary)
var dict = ["one": 1, "two": 2]
for (key, value) in dict {
    print("\(key): \(value)")
}

// ciclo for (limite)
for i in -1...shoppingList.count {
    print(i)
}
shoppingList[1...2] = ["steak", "peacons"]
// usar ..< para excluir o último número

// ciclo while
var i = 1
while i < 1000 {
    i *= 2
}

// ciclo do-whie
do {
    print("hello")
} while 1 == 2

// Switch
// Muito poderoso, imagine `if`s com açúcar sintático
// Funciona para String, instâncias de objectos e primitivas (Int, Double, etc.)
let vegetable = "red pepper"
switch vegetable {
case "celery":
    let vegetableComment = "Add some raisins and make ants on a log."
case "cucumber", "watercress":
    let vegetableComment = "That would make a good tea sandwich."
case let localScopeValue where localScopeValue.hasSuffix("pepper"):
    let vegetableComment = "Is it a spicy \(localScopeValue)?"
default: // obrigatório (de forma a cobrir todos os possíveis inputs)
    let vegetableComment = "Everything tastes good in soup."
}


//
// MARK: Funções (Functions)
//

// Funções são tipos de primeira classe, o que significa que podem ser
// aninhadas dentro de outras funções e passadas como argumento

// Função em Swift com documentação no header

/**
    Função de cumprimento.

    - Um ponto em documentação
    - Outro ponto na documentação

    :param: nome Um nome
    :param: dia Um dia
    :returns: Uma string com um cumprimento contendo o nome e o dia.
*/
func greet(nome: String, dia: String) -> String {
    return "Hello \(nome), today is \(dia)."
}
greet("Bob", "Tuesday")

// Semelhante ao método de cima excepto ao comportamento dos argumentos
func greet2(#nomeObrigatório: String, nomeArgumentoExterno nomeArgumentoLocal: String) -> String {
    return "Hello \(nomeObrigatório), the day is \(nomeArgumentoLocal)"
}
greet2(nomeObrigatório:"John", nomeArgumentoExterno: "Sunday")

// Função que devolve vários itens num tuplo
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// Ignorar tuplos ou outros valores usando _ (underscore)
let (_, price1, _) = pricesTuple // price1 == 3.69
print(price1 == pricesTuple.1) // true
print("Gas price: \(price)")

// Argumentos variáveis
func setup(numbers: Int...) {
    // é um array
    let number = numbers[0]
    let argCount = numbers.count
}

// Passar e devolver funções
func makeIncrementer() -> (Int -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

// Passar por referência (inout)
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

// Funções são casos especiais de closures ({})

// Exemplo de um Closure.
// `->` separa o argumento e o tipo de retorno.
// `in` separa o cabeçalho do closure do corpo do closure.
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})

// Quando o tipo é conhecido, como em cima, podemos fazer o seguinte
numbers = numbers.map({ number in 3 * number })
// Ou até mesmo isto
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// Closure à direita (Trailing closure)
numbers = sorted(numbers) { $0 > $1 }

print(numbers) // [18, 6, 3]

// Super curto, pois o operador < consegue inferir o tipo

numbers = sorted(numbers, < )

print(numbers) // [3, 6, 18]

//
// MARK: Estruturas (Structures)
//

// Estruturas (struct) e classes (class) têm capacidades muito semelhantes
struct NamesTable {
    let names = [String]()

    // Custom subscript
    subscript(index: Int) -> String {
        return names[index]
    }
}

// Estruturas têm um inicializador implicito que é automaticamente gerado
let namesTable = NamesTable(names: ["Me", "Them"])
let name = namesTable[1]
print("Name is \(name)") // Name is Them

//
// MARK: Classes
//

// Classes, estruturas e os seus membros têm três níveis de controlo de acesso
// Nomeadamente: interno (predefinição)(internal) , público (public), privado (private)

public class Shape {
    public func getArea() -> Int {
        return 0;
    }
}

// Todos os métodos e propriedades de uma classe são públicos.
// Se só for necessário guarda dados num
// objecto estruturado, então é melhor usar uma `struct`

internal class Rect: Shape {
    var sideLength: Int = 1

    // Propriedade getter e setter personalizado
    private var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue` é uma variável implicita disponível aos setters
            sideLength = newValue / 4
        }
    }

    // Carregar preguiçosamente uma propriedade
    // subShape permanece a nil (unintialized) até o getter ser invocado
    lazy var subShape = Rect(sideLength: 4)

    // Se não for necessário um getter e setter personalizado,
    // mas se quiser correr o código antes e depois de modificar ou aceder
    // uma propriedade, é possível usar `willSet` e `didSet`
    var identifier: String = "defaultID" {
        // o argumento de `willSet` é o nome da variável para o novo valor
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }

    init(sideLength: Int) {
        self.sideLength = sideLength
        // invocar super.init no final do método de inicialização
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

// A class `Square` estende (extends) a classe `Rect` (hierarquia)
class Square: Rect {
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// Cast de uma instância de `Square` para `Shape`
let aShape = mySquare as Shape

// Compara instâncias, não é igual a == , visto que == compara objects (igual a)
if mySquare === mySquare {
    print("Yep, it's mySquare")
}

// Inicializador (init) com Optional
class Circle: Shape {
    var radius: Int
    override func getArea() -> Int {
        return 3 * radius * radius
    }

    // Colocar um ponto de interrpgação depois de `init` cria um inicializador
    // Optional, o qual pode retornar nil
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
    // Não vai executar pois a variável myEmptyCircle é igual a nil
    print("circle is not nil")
}


//
// MARK: Enumerações (Enums)
//

// Enums pode opcionalmente ser um tipo especifico ou não.
// Enums podem conter métodos tal como as classes.

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

// Os valores de Enum permitem syntax reduzida, não é preciso escrever o tipo do enum
// quando a variável é explicitamente definida.
var suitValue: Suit = .Hearts

// Enums que não sejam inteiros obrigam a atribuições valor bruto (raw value) diretas
enum BookName: String {
    case John = "John"
    case Luke = "Luke"
}
print("Name: \(BookName.John.rawValue)")

// Enum com valores associados
enum Furniture {
    // Associar com um inteiro (Int)
    case Desk(height: Int)
    // Associar com uma String e um Int
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
// MARK: Protocolos (Protocols)
//

// Protocolos (`protcol`s) obrigam a que os tipos tenham
// propriedades de instância, métodos de instância, métodos de tipo,
// operadores e subscripts específicos.

protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

// Protocolos definidos com @objc permitem funções com optional
// que permitem verificar se existem conformidade
@objc protocol TransformShape {
    optional func reshaped()
    optional func canReshape() -> Bool
}

class MyShape: Rect {
    var delegate: TransformShape?

    func grow() {
        sideLength += 2

        // Coloca um ponto de interrogação após uma propriedade opcional, método
        // ou subscript para graciosamente ignorar um valor nil e retornar nil
        // em vez de provoar um erro em tempo de execução ("optional chaining").
        if let allow = self.delegate?.canReshape?() {
            // testar o delegate e depois o método
            self.delegate?.reshaped?()
        }
    }
}


//
// MARK: Outro
//

// extensões (`extension`s): Adiciona funcionalidade extra a um tipo já existente.

// Square agora "conforma" com o protocolo `Printable`
extension Square: Printable {
    var description: String {
        return "Area: \(self.getArea()) - ID: \(self.identifier)"
    }
}

print("Square: \(mySquare)")

// Também é possível extender tipos já embutidos
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

// Generics: Semelhante a Java e C#. Usa a palavra-chave `where` para
// especificar requisitos do `generics`.

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
// Operadores personalizados podem começar com caracteres:
//      / = - + * % < > ! & | ^ . ~
// ou
// Caracteres Unicode matemáticos, símbolos, setas, dingbat e
// caracteres de desenho linha/caixa.
operador prefixo !!! {}

// Um operador prefixo que triplica o comprimento do lado quando usado
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// valor atual
print(mySquare.sideLength) // 4

// muda o comprimento deste lado usando o operador personalizado !!!, aumenta
// o comprimento 3x
!!!mySquare
print(mySquare.sideLength) // 12

// Operadores também podem ser generics
infix operator <-> {}
func <-><T: Equatable> (inout a: T, inout b: T) {
    let c = a
    a = b
    b = c
}

var foo: Float = 10
var bar: Float = 20

foo <-> bar
print("foo is \(foo), bar is \(bar)") // "foo is 20.0, bar is 10.0"
```
