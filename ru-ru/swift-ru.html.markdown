---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
  - ["Joey Huang", "http://github.com/kamidox"]
  - ["Alexey Nazaroff", "http://github.com/rogaven"]
filename: learnswift-ru.swift
translators:
  - ["Dmitry Bessonov", "https://github.com/TheDmitry"]
  - ["Alexey Nazaroff", "https://github.com/rogaven"]
lang: ru-ru
---

Swift - это язык программирования, созданный компанией Apple, для приложений
под iOS и OS X. Разработанный, чтобы сосуществовать с Objective-C и
быть более устойчивым к ошибочному коду, Swift был представлен в 2014 году на
конференции разработчиков Apple, WWDC. Приложения на Swift собираются
с помощью LLVM-компилятора, включенного в Xcode 6+.

Официальная книга по [языку программирования Swift](https://itunes.apple.com/us/book/swift-programming-language/id881256329) от Apple доступна в iBooks.

Смотрите еще [начальное руководство](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/RoadMapiOS/index.html) Apple, которое содержит полное учебное пособие по Swift.

```swift
// Версия Swift: 3.0

// импорт модуля
import UIKit

//
// MARK: Основы
//

// Xcode поддерживает маркеры, чтобы давать примечания своему коду
// и вносить их в список обозревателя (Jump Bar)
// MARK: Метка раздела
// MARK: - Метка с разделителем
// TODO: Сделайте что-нибудь вскоре
// FIXME: Исправьте этот код

// Начиная со второй версии Swift, println и print объединены в методе print.
// Перенос строки теперь добавляется в конец автоматически.
print("Привет, мир!") // println – теперь просто print
print("Привет, мир!", terminator: "") // вывод текста без переноса строки

// переменные (var), значение которых можно изменить после инициализации
// константы (let), значение которых нельзя изменить после инициализации

var myVariable = 42
let øπΩ = "значение" // именование переменной символами unicode
let π = 3.1415926
let convenience = "Ключевое слово" // контекстное имя переменной
let weak = "Ключевое слово"; let override = "еще ключевое слово" // операторы
                                      // могут быть отделены точкой с запятой
let `class` = "Ключевое слово" // обратные апострофы позволяют использовать
                               // ключевые слова в именовании переменных
let explicitDouble: Double = 70
let intValue = 0007 // 7
let largeIntValue = 77_000 // 77000
let label = "некоторый текст " + String(myVariable) // Приведение типа
let piText = "Pi = \(π), Pi 2 = \(π * 2)" // Вставка переменных в строку

// Сборка особых значений
// используя ключ -D сборки конфигурации
#if false
    print("Не печатается")
    let buildValue = 3
#else
    let buildValue = 7
#endif
print("Значение сборки: \(buildValue)") // Значение сборки: 7

/*
    Опционалы - это особенность языка Swift, которая допускает вам сохранять
    `некоторое` или `никакое` значения.

    Язык Swift требует, чтобы каждое свойство имело значение, поэтому даже nil
    должен быть явно сохранен как опциональное значение.

    Optional<T> является перечислением.
*/
var someOptionalString: String? = "опционал" // Может быть nil
// как и выше, только ? - это постфиксный оператор (синтаксический сахар)
var someOptionalString2: Optional<String> = "опционал"

if someOptionalString != nil {
    // я не nil
    if someOptionalString!.hasPrefix("opt") {
        print("содержит префикс")
    }

    let empty = someOptionalString?.isEmpty
}
someOptionalString = nil

/*
Использование ! для доступа к несуществующему опциональному значению генерирует
рантайм ошибку. Всегда проверяйте, что опционал содержит не пустое значение,
перед тем как раскрывать его через !.
*/

// неявная развертка опциональной переменной
var unwrappedString: String! = "Ожидаемое значение."
// как и выше, только ! - постфиксный оператор (с еще одним синтаксическим сахаром)
var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Ожидаемое значение."

// If let конструкции -
// If let это специальная конструкция в Swift, которая позволяет проверить Optional
// справа от `=` непустой, и если это так - разворачивает его и присваивает левой части.
if let someOptionalStringConstant = someOptionalString {
    // имеется некоторое (`Some`) значение, не `nil`
    if !someOptionalStringConstant.hasPrefix("ok") {
        // нет такого префикса
    }
}

// Swift поддерживает сохранение значения любого типа
// Для этих целей есть два ключевых слова `Any` и `AnyObject`
// AnyObject == id
// `Any` же, в отличие от `id` в Objective-C, `Any` работает с любым значением (Class, Int, struct и т.д.)
var anyVar: Any = 7
anyVar = "Изменять значение на строку не является хорошей практикой, но возможно."
let anyObjectVar: AnyObject = Int(1) as NSNumber

/*
    Комментируйте здесь

    /*
        Вложенные комментарии тоже поддерживаются
    */
*/

//
// MARK: Коллекции
//

/*
    Массив (Array) и словарь (Dictionary) являются структурами (struct). Так
    `let` и `var` также означают, что они изменяются (var) или не изменяются (let)
    при объявлении переменных этих типов.
*/

// Массив
var shoppingList = ["сом", "вода", "лимоны"]
shoppingList[1] = "бутылка воды"
let emptyArray = [String]() // let == неизменный
let emptyArray2 = Array<String>() // как и выше
var emptyMutableArray = [String]() // var == изменяемый
var explicitEmptyMutableStringArray: [String] = [] // так же как и выше


// Словарь
var occupations = [
    "Malcolm": "Капитан",
    "kaylee": "Техник"
]
occupations["Jayne"] = "Связи с общественностью"
let emptyDictionary = [String: Float]() // let == неизменный
let emptyDictionary2 = Dictionary<String, Float>() // как и выше
var emptyMutableDictionary = [String: Float]() // var == изменяемый
var explicitEmptyMutableDictionary: [String: Float] = [:] // то же


//
// MARK: Поток управления
//

// С помощью "," можно указать дополнительные условия для раскрытия
// опциональных значений.
let someNumber = Optional<Int>(7)
if let num = someNumber, num > 3 {
    print("Больше 3х")
}

// цикл for для массива
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        print("Один!")
    } else {
        print("Не один!")
    }
}

// цикл for для словаря
var dict = ["один": 1, "два": 2]
for (key, value) in dict {
    print("\(key): \(value)")
}

// цикл for для диапазона чисел
for i in -1...shoppingList.count {
    print(i)
}
shoppingList[1...2] = ["бифштекс", "орехи пекан"]
// используйте ..< для исключения последнего числа

// цикл while
var i = 1
while i < 1000 {
    i *= 2
}

// цикл do-while
repeat {
    print("привет")
} while 1 == 2

// Переключатель
// Очень мощный оператор, представляйте себе операторы `if` с синтаксическим
// сахаром
// Они поддерживают строки, объекты и примитивы (Int, Double, etc)
let vegetable = "красный перец"
switch vegetable {
case "сельдерей":
    let vegetableComment = "Добавьте немного изюма, имитируя муравьев на бревнышке."
case "огурец", "кресс-салат":
    let vegetableComment = "Было бы неплохо сделать бутерброд с чаем."
case let localScopeValue where localScopeValue.hasSuffix("перец"):
    let vegetableComment = "Это острый \(localScopeValue)?"
default: // обязательный (чтобы предусмотреть все возможные вхождения)
    let vegetableComment = "В супе все овощи вкусные."
}


//
// MARK: Функции
//

// Функции являются типом первого класса, т.е. они могут быть вложены в функциях
// и могут передаваться между собой

// Функция с документированным заголовком Swift (формат Swift-модифицированный Markdown)

/**
    Операция приветствия

    - Маркер в документировании
    - Еще один маркер в документации

    - Parameter name	: Это имя
    - Parameter day	: Это день
    - Returns : Строка, содержащая значения name и day.
*/
func greet(name: String, day: String) -> String {
    return "Привет \(name), сегодня \(day)."
}
greet(name: "Боб", day: "вторник")

// как и выше, кроме обращения параметров функции
func greet2(name: String, externalParamName localParamName: String) -> String {
    return "Привет \(name), сегодня \(localParamName)"
}
greet2(name: "Иван", externalParamName: "Воскресенье")

// Функция, которая возвращает множество элементов в кортеже
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// Пропускайте значения кортежей с помощью подчеркивания _
let (_, price1, _) = pricesTuple // price1 == 3.69
print(price1 == pricesTuple.1) // вывод: true
print("Цена газа: \(price)")

// Именованные параметры кортежа
func getGasPrices2() -> (lowestPrice: Double, highestPrice: Double, midPrice: Double) {
    return (1.77, 37.70, 7.37)
}
let pricesTuple2 = getGasPrices2()
let price2 = pricesTuple2.lowestPrice
let (_, price3, _) = pricesTuple2
print(pricesTuple2.highestPrice == pricesTuple2.1) // вывод: true
print("Самая высокая цена за газ: \(pricesTuple2.highestPrice)")

// guard утверждения
func testGuard() {
    // guards обеспечивают прерывание дальнейшего выполнения функции,
    // позволяя держать обработчики ошибок рядом с проверкой условия
    // Объявляемая переменная находится в той же области видимости, что и guard.
    guard let aNumber = Optional<Int>(7) else {
        return
    }

    print("число равно \(aNumber)")
}
testGuard()

// Переменное число аргументов
func setup(numbers: Int...) {
    // это массив
    let number = numbers[0]
    let argCount = numbers.count
}

// Передача и возврат функций
func makeIncrementer() -> ((Int) -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

// передача по ссылке
func swapTwoInts(a: inout Int, b: inout Int) {
    let tempA = a
    a = b
    b = tempA
}
var someIntA = 7
var someIntB = 3
swapTwoInts(a: &someIntA, b: &someIntB)
print(someIntB) // 7


//
// MARK: Замыкания
//
var numbers = [1, 2, 6]

// Функции - это частный случай замыканий ({})

// Пример замыкания.
// `->` отделяет аргументы и возвращаемый тип
// `in` отделяет заголовок замыкания от тела замыкания
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})

// Когда тип известен, как и выше, мы можем сделать так
numbers = numbers.map({ number in 3 * number })
// Или даже так
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// Хвостовое замыкание
numbers = numbers.sorted { $0 > $1 }

print(numbers) // [18, 6, 3]

// Суперсокращение, поскольку оператор < выполняет логический вывод типов

numbers = numbers.sorted(by: <)

print(numbers) // [3, 6, 18]

//
// MARK: Структуры
//

// Структуры и классы имеют очень похожие характеристики
struct NamesTable {
    let names: [String]

    // Пользовательский индекс
    subscript(index: Int) -> String {
        return names[index]
    }
}

// У структур автогенерируемый (неявно) инициализатор
let namesTable = NamesTable(names: ["Иван", "Яков"])
let name = namesTable[1]
print("Имя :\(name)") // Имя: Яков

//
// MARK: Обработка ошибок
//

// Протокол `Error` используется для перехвата выбрасываемых ошибок
enum MyError: Error {
    case BadValue(msg: String)
    case ReallyBadValue(msg: String)
}

// фунции помеченные словом `throws` должны вызываться с помощью `try`
func fakeFetch(value: Int) throws -> String {
    guard 7 == value else {
        throw MyError.ReallyBadValue(msg: "Действительно плохое значение")
    }

    return "тест"
}

func testTryStuff() {
    // предполагается, что не будет выброшено никаких ошибок,
    // в противном случае мы получим рантайм исключение
    let _ = try! fakeFetch(value: 7)

    // Если возникает ошибка, то выполнение продолжится. Но если значение равно nil,
    // то результат будет опционалом
    let _ = try? fakeFetch(value: 7)

    do {
        // обычно try оператор, позволяющий обработать ошибку в `catch` блоке
        try fakeFetch(value: 1)
    } catch MyError.BadValue(let msg) {
        print("Ошибка: \(msg)")
    } catch {
        // все остальное
    }
}
testTryStuff()

//
// MARK: Классы
//

// Классы, структуры и их члены имеют трехуровневый контроль доступа
// Уровни: internal (по умолчанию), public, private

public class Shape {
    public func getArea() -> Int {
        return 0
    }
}

// Все методы и свойства класса являются открытыми (public).
// Если вам необходимо содержать только данные
// в структурированном объекте, вы должны использовать `struct`

internal class Rect: Shape {
    var sideLength: Int = 1

    // Пользовательский сеттер и геттер
    private var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue` - неявная переменная, доступная в сеттере
            sideLength = newValue / 4
        }
    }

    // Вычисляемые свойства должны быть объявлены с помощью `var`, ведь они могут меняться
    var smallestSideLength: Int {
        return self.sideLength - 1
    }

    // Ленивая загрузка свойства
    // свойство subShape остается равным nil (неинициализированным),
    // пока не вызовется геттер
    lazy var subShape = Rect(sideLength: 4)

    // Если вам не нужны пользовательские геттеры и сеттеры,
    // но все же хотите запустить код перед и после вызовов геттера или сеттера
    // свойств, вы можете использовать `willSet` и `didSet`
    var identifier: String = "defaultID" {
        // аргумент у `willSet` будет именем переменной для нового значения
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }

    init(sideLength: Int) {
        self.sideLength = sideLength
        // последним всегда вызывается super.init, когда init с параметрами
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

// Простой класс `Square` наследует `Rect`
class Square: Rect {
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// преобразование объектов
let aShape = mySquare as Shape

// сравнение экземпляров, в отличие от ==, которая проверяет эквивалентность
if mySquare === mySquare {
    print("Ага, это mySquare")
}

// Опциональная инициализация (init)
class Circle: Shape {
    var radius: Int
    override func getArea() -> Int {
        return 3 * radius * radius
    }

    // Поместите постфиксный знак вопроса после `init` - это и будет опциональная инициализация,
    // которая может вернуть nil
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
    // не будет выполняться, поскольку myEmptyCircle равен nil
    print("circle не nil")
}


//
// MARK: Перечисления
//

// Перечисления могут быть определенного или своего типа.
// Они могут содержать методы подобно классам.

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

// Значения перечислений допускают сокращенный синтаксис, нет необходимости
// указывать тип перечисления, когда переменная объявляется явно
var suitValue: Suit = .Hearts

// Значения нецелочисленных перечислений должны быть указаны явно
// или могут выводится с помощью функции `rawValue` из имени
enum BookName: String {
    case John
    case Luke = "Лука"
}
print("Имя: \(BookName.John.rawValue)")

// Перечисление (enum) со связанными значениями
enum Furniture {
    // Связать с типом Int
    case Desk(height: Int)
    // Связать с типами String и Int
    case Chair(String, Int)

    func description() -> String {
        switch self {
        case .Desk(let height):
            return "Письменный стол высотой \(height) см."
        case .Chair(let brand, let height):
            return "Стул марки \(brand) высотой \(height) см."
        }
    }
}

var desk: Furniture = .Desk(height: 80)
print(desk.description())     // "Письменный стол высотой 80 см."
var chair = Furniture.Chair("Foo", 40)
print(chair.description())    // "Стул марки Foo высотой 40 см."


//
// MARK: Протоколы
//

// `protocol` может потребовать, чтобы у соответствующих типов
// были определенные свойства экземпляра, методы экземпляра, тип методов,
// операторы и индексы.

protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

// Протоколы, объявленные с @objc, допускают необязательные функции,
// которые позволяют вам проверять на соответствие. Для функций также необходимо указать @objc
@objc protocol TransformShape {
    @objc optional func reshape()
    @objc optional func canReshape() -> Bool
}

class MyShape: Rect {
    var delegate: TransformShape?

    func grow() {
        sideLength += 2

        // Размещайте знак вопроса перед опционным свойством, методом
        // или индексом, чтобы не учитывать nil-значение и возвратить nil
        // вместо выбрасывания ошибки выполнения (т.н. "опционная цепочка")
        if let reshape = self.delegate?.canReshape?(), reshape {
            // проверка делегата на выполнение метода
            self.delegate?.reshape?()
        }
    }
}


//
// MARK: Прочее
//

// `extension`s: Добавляет расширенный функционал к существующему типу

// Класс Square теперь "соответствует" протоколу `CustomStringConvertible`
extension Square: CustomStringConvertible {
    var description: String {
        return "Площадь: \(self.getArea()) - ID: \(self.identifier)"
    }
}

print("Объект Square: \(mySquare)")

// Вы также можете расширить встроенные типы
extension Int {
    var customProperty: String {
        return "Это \(self)"
    }

    func multiplyBy(num: Int) -> Int {
        return num * self
    }
}

print(7.customProperty) // "Это 7"
print(14.multiplyBy(num: 3)) // 42

// Обобщения: Подобно языкам Java и C#. Используйте ключевое слово `where`,
// чтобы определить условия обобщений.

func findIndex<T: Equatable>(array: [T], valueToFind: T) -> Int? {
    for (index, value) in array.enumerated() {
        if value == valueToFind {
            return index
        }
    }
    return nil
}
let foundAtIndex = findIndex(array: [1, 2, 3, 4], valueToFind: 3)
print(foundAtIndex == 2) // вывод: true

// Операторы:
// Пользовательские операторы могут начинаться с символов:
//      / = - + * % < > ! & | ^ . ~
// или
// Unicode- знаков математики, символов, стрелок, декорации и линий/кубов,
// нарисованных символов.
prefix operator !!!

// Префиксный оператор, который утраивает длину стороны, когда используется
prefix func !!! (shape: inout Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// текущее значение
print(mySquare.sideLength) // 4

// Используя пользовательский оператор !!!, изменится длина стороны
// путем увеличения размера в 3 раза
!!!mySquare
print(mySquare.sideLength) // 12

// Операторы также могут быть обобщенными
infix operator <->
func <-><T: Equatable> (a: inout T, b: inout T) {
    let c = a
    a = b
    b = c
}

var foo: Float = 10
var bar: Float = 20

foo <-> bar
print("foo это \(foo), bar это \(bar)") // "foo = 20.0, bar = 10.0"
```
