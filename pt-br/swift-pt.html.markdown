---
language: swift
contributors:
    - ["Grant Timmerman", "http://github.com/grant"]
    - ["Christopher Bess", "http://github.com/cbess"]
translators:
    - ["Mariane Siqueira Machado", "https://twitter.com/mariane_sm"]
lang: pt-br
filename: learnswift.swift
---

Swift é uma linguagem de programação para desenvolvimento de aplicações no iOS e OS X criada pela Apple. Criada para
coexistir com Objective-C e para ser mais resiliente a código com erros, Swift foi apresentada em 2014 na Apple's
developer conference WWDC. Foi construída com o compilador LLVM já incluído no Xcode 6 beta.

O livro oficial [Swift Programming Language] (https://itunes.apple.com/us/book/swift-programming-language/id881256329) da
Apple já está disponível via IBooks (apenas em inglês).

Confira também o tutorial completo de Swift da Apple [getting started guide](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/RoadMapiOS/index.html), também disponível apenas em inglês.

```swift
// importa um módulo
import UIKit

//
// MARK: Noções básicas
//

// Xcode supporta anotações para seu código e lista elas na barra de atalhos
// MARK: Marca uma sessão
// TODO: Faça algo logo
// FIXME: Conserte esse código

println("Hello, world")

// Valores em variáveis (var) podem ter seu valor alterado depois de declarados.
// Valores em constantes (let) NÃO podem ser alterados depois de declarados.

var myVariable = 42
let øπΩ = "value" // nomes de variáveis em unicode
let π = 3.1415926
let convenience = "keyword" // nome de variável contextual
let weak = "keyword"; let override = "another keyword" // comandos podem ser separados por uma ponto e vírgula
let `class` = "keyword" // Crases permitem que palavras-chave seja usadas como nome de variáveis
let explicitDouble: Double = 70
let intValue = 0007 // 7
let largeIntValue = 77_000 // 77000
let label = "some text " + String(myVariable) // Coerção
let piText = "Pi = \(π), Pi 2 = \(π * 2)" // Interpolação de strings

// Constrói valores específicos
// Utiliza configuração de build -D
#if false
    println("Not printed")
    let buildValue = 3
#else
    let buildValue = 7
#endif
println("Build value: \(buildValue)") // Build value: 7

/*
    Optionals fazem parte da linguagem e permitem que você armazene um
    valor `Some` (algo) ou `None` (nada).

    Como Swift requer que todas as propriedades tenham valores, até mesmo nil deve
    ser explicitamente armazenado como um valor Optional.

    Optional<T> é uma enum.
*/
var someOptionalString: String? = "optional" // Pode ser nil
// o mesmo acima, mas ? é um operador pós-fixado (açúcar sintático)
var someOptionalString2: Optional<String> = "optional"

if someOptionalString != nil {
    // Eu não sou nil
    if someOptionalString!.hasPrefix("opt") {
        println("has the prefix")
    }

    let empty = someOptionalString?.isEmpty
}
someOptionalString = nil

// Optional implicitamente desempacotado (unwrapped)
var unwrappedString: String! = "Valor é esperado."
// o mesmo acima, mas ? é um operador pósfixado (açúcar sintático)
var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Valor é esperado."

if let someOptionalStringConstant = someOptionalString {
    // Tem `Some` (algum) valor, não nil
    if !someOptionalStringConstant.hasPrefix("ok") {
        // não possui o prefixo
    }
}

// Swift tem suporte para armazenar um valor de qualquer tipo.
// AnyObject == id
// Ao contrário de Objective-C `id`, AnyObject funciona com qualquer valor (Class, Int, struct, etc)
var anyObjectVar: AnyObject = 7
anyObjectVar = "Mudou o valor para string, não é uma boa prática, mas é possível."

/*
Comentário aqui
    /*
        Comentários aninhados também são suportados
    */
*/

//
// MARK: Coleções
//

/*
    Tipos Array e Dicionário são structs. Portanto `let` e `var`
    também indicam se são mutáveis (var) ou imutáveis (let) quando declarados
    com esses tipos.
*/

// Array
var shoppingList = ["catfish", "water", "lemons"]
shoppingList[1] = "bottle of water"
let emptyArray = [String]() // imutável
var emptyMutableArray = [String]() // mutável


// Dicionário
var occupations = [
    "Malcolm": "Captain",
    "kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
let emptyDictionary = [String: Float]() // imutável
var emptyMutableDictionary = [String: Float]() // mutável


//
// MARK: Controle de fluxo
//

// laço for (array)
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        println("One!")
    } else {
        println("Not one!")
    }
}

// laço for (dicionário)
var dict = ["one": 1, "two": 2]
for (key, value) in dict {
    println("\(key): \(value)")
}

// laço for (alcance)
for i in -1...shoppingList.count {
    println(i)
}
shoppingList[1...2] = ["steak", "peacons"]
// use ..< para excluir o último número

// laço while (enquanto)
var i = 1
while i < 1000 {
    i *= 2
}

// laço do-while
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
// MARK: Funções
//

// Funções são tipos de primeira classe, o que significa que eles podem ser aninhados
// em funções e podem ser passados como parâmetros

// Funções Swift com cabeçalhos doc (formato como reStructedText)
/**
Uma operação de saudação

- Um bullet em documentos
- Outro bullet

:param: nome A nome
:param: dia A dia
:returns: Uma string contendo o nome e o dia.
*/
func greet(name: String, day: String) -> String {
    return "Hello \(name), today is \(day)."
}
greet("Bob", "Tuesday")

// Função que retorna múltiplos items em uma tupla
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// Ignore valores de Tuplas (ou outros valores) usando _ (underscore)
let (_, price1, _) = pricesTuple // price1 == 3.69
println(price1 == pricesTuple.1) // true
println("Gas price: \(price)")

// Número variável de argumentos
func setup(numbers: Int...) {
    // é um array
    let number = numbers[0]
    let argCount = numbers.count
}

// Passando e retornando funções
func makeIncrementer() -> (Int -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

// passagem por referência
func swapTwoInts(inout a: Int, inout b: Int) {
    let tempA = a
    a = b
    b = tempA
}
var someIntA = 7
var someIntB = 3
swapTwoInts(&someIntA, &someIntB)
println(someIntB) // 7


//
// MARK: Closures
//
var numbers = [1, 2, 6]

// Funções são casos especiais de closures ({})

// Exemplo de closure.
// `->` separa argumentos e tipo de retorno
// `in` separa o cabeçalho do closure do seu corpo
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})

// Quando o tipo é conhecido, como abaixo, nós podemos fazer o seguinte
numbers = numbers.map({ number in 3 * number })
// Ou até mesmo isso
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// Closure restante
numbers = sorted(numbers) { $0 > $1 }

print(numbers) // [18, 6, 3]

// Super atalho, já que o operador < infere os tipos

numbers = sorted(numbers, < )

print(numbers) // [3, 6, 18]

//
// MARK: Estruturas
//

// Estruturas e classes tem funcionalidades muito similares
struct NamesTable {
    let names: [String]

    // Custom subscript
    subscript(index: Int) -> String {
        return names[index]
    }
}

// Estruturas possuem um inicializador auto-gerado automático (implícito)
let namesTable = NamesTable(names: ["Me", "Them"])
//let name = namesTable[2]
//println("Name is \(name)") // Name is Them

//
// MARK: Classes
//

// Classes, Estruturas e seus membros possuem três níveis de modificadores de acesso
// Eles são: internal (default), public, private

public class Shape {
    public func getArea() -> Int {
        return 0;
    }
}

// Todos os métodos e propriedades de uma classe são públicos.
// Se você só precisa armazenar dados em um objeto estruturado, use `struct`

internal class Rect: Shape {
    var sideLength: Int = 1

    // Getter e setter personalizado
    private var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue` é uma variável implicita disponível para os setters
            sideLength = newValue / 4
        }
    }

    // Carregue uma propriedade sob demanda (lazy)
    // subShape permanece nil (não inicializado) até seu getter ser chamado
    lazy var subShape = Rect(sideLength: 4)

    // Se você não precisa de um getter e setter personalizado,
    // mas ainda quer roda código antes e depois de configurar
    // uma propriedade, você  pode usar `willSet` e `didSet`
    var identifier: String = "defaultID" {
        // o argumento `willSet` será o nome da variável para o novo valor
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }

    init(sideLength: Int) {
        self.sideLength = sideLength
         // sempre chame super.init por último quand inicializar propriedades personalizadas (custom)
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

// Uma classe básica `Square` que estende `Rect`
class Square: Rect {
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// Compara instâncias, não é o mesmo que == o qual compara objetos
if mySquare === mySquare {
    println("Yep, it's mySquare")
}


//
// MARK: Enums
//

// Enums podem opcionalmente ser de um tipo específico ou não.
// Podem conter métodos do mesmo jeito que classes.

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
// MARK: Protocolos
//

// `protocol` pode requerer que os tipos que se adequam tenham
// propriedades de instância, métodos, operadores e subscripts.
protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

// Protocolos declarados com @objc permitem funções opcionais,
// que permitem verificar a confomidade
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
            // testa por delegação e então por método
            self.delegate?.reshaped?()
        }
    }
}


//
// MARK: Outros
//

// `extension`s: Adicionam uma funcionalidade extra para um tipo já existente.

// Square agora "segue" o protocolo `Printable`
extension Square: Printable {
    var description: String {
        return "Area: \(self.getArea()) - ID: \(self.identifier)"
    }
}

println("Square: \(mySquare)")

// Você pode também estender tipos embutidos (built-in)
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

// Generics: Similar com Java e C#. Use a palavra-chave `where` para
// especificar os requisitos do generics.

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

// Operadores:
// Operadores personalizados (custom) podem começar com os seguintes caracteres:
//      / = - + * % < > ! & | ^ . ~
// ou
// Unicode math, símbolo, seta, e caracteres tipográficos ou de desenho.
prefix operator !!! {}

// Um operador de prefixo que triplica o comprimento do lado do quadrado
// quando usado
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// valor atual
println(mySquare.sideLength) // 4

// Troca o comprimento do lado usando um operador personalizado !!!, aumenta o lado por 3
!!!mySquare
println(mySquare.sideLength) // 12

```
