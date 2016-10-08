---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
  - ["Joey Huang", "http://github.com/kamidox"]
filename: learnswift-ru.swift
translators:
  - ["Dmitry Bessonov", "https://github.com/TheDmitry"]
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
// импорт модуля
import UIKit

//
// MARK: Основы
//

// Xcode поддерживает маркеры, чтобы давать примечания своему коду
// и вносить их в список обозревателя (Jump Bar)
// MARK: Метка раздела
// TODO: Сделайте что-нибудь вскоре
// FIXME: Исправьте этот код

println("Привет, мир")

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
    println("Не печатается")
    let buildValue = 3
#else
    let buildValue = 7
#endif
println("Значение сборки: \(buildValue)") // Значение сборки: 7

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
        println("содержит префикс")
    }

    let empty = someOptionalString?.isEmpty
}
someOptionalString = nil

// неявная развертка опциональной переменной
var unwrappedString: String! = "Ожидаемое значение."
// как и выше, только ! - постфиксный оператор (с еще одним синтаксическим сахаром)
var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Ожидаемое значение."

if let someOptionalStringConstant = someOptionalString {
    // имеется некоторое значение, не nil
    if !someOptionalStringConstant.hasPrefix("ok") {
        // нет такого префикса
    }
}

// Swift поддерживает сохранение значения любого типа
// AnyObject == id
// В отличие от `id` в Objective-C, AnyObject работает с любым значением (Class,
// Int, struct и т.д.)
var anyObjectVar: AnyObject = 7
anyObjectVar = "Изменять значение на строку не является хорошей практикой, но возможно."

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


// Словарь
var occupations = [
    "Malcolm": "Капитан",
    "kaylee": "Техник"
]
occupations["Jayne"] = "Связи с общественностью"
let emptyDictionary = [String: Float]() // let == неизменный
let emptyDictionary2 = Dictionary<String, Float>() // как и выше
var emptyMutableDictionary = [String: Float]() // var == изменяемый


//
// MARK: Поток управления
//

// цикл for для массива
let myArray = [1, 1, 2, 3, 5]
for value in myArray {
    if value == 1 {
        println("Один!")
    } else {
        println("Не один!")
    }
}

// цикл for для словаря
var dict = ["один": 1, "два": 2]
for (key, value) in dict {
    println("\(key): \(value)")
}

// цикл for для диапазона чисел
for i in -1...shoppingList.count {
    println(i)
}
shoppingList[1...2] = ["бифштекс", "орехи пекан"]
// используйте ..< для исключения последнего числа

// цикл while
var i = 1
while i < 1000 {
    i *= 2
}

// цикл do-while
do {
    println("привет")
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

// Функция с документированным заголовком Swift (формат reStructedText)

/**
    Операция приветствия

    - Маркер в документировании
    - Еще один маркер в документации

    :param: name - это имя
    :param: day - это день
    :returns: Строка, содержащая значения name и day.
*/
func greet(name: String, day: String) -> String {
    return "Привет \(name), сегодня \(day)."
}
greet("Боб", "вторник")

// как и выше, кроме обращения параметров функции
func greet2(#requiredName: String, externalParamName localParamName: String) -> String {
    return "Привет \(requiredName), сегодня \(localParamName)"
}
greet2(requiredName:"Иван", externalParamName: "воскресенье")

// Функция, которая возвращает множество элементов в кортеже
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// Пропускайте значения кортежей с помощью подчеркивания _
let (_, price1, _) = pricesTuple // price1 == 3.69
println(price1 == pricesTuple.1) // вывод: true
println("Цена газа: \(price)")

// Переменное число аргументов
func setup(numbers: Int...) {
    // это массив
    let number = numbers[0]
    let argCount = numbers.count
}

// Передача и возврат функций
func makeIncrementer() -> (Int -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

// передача по ссылке
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
numbers = sorted(numbers) { $0 > $1 }

print(numbers) // [18, 6, 3]

// Суперсокращение, поскольку оператор < выполняет логический вывод типов

numbers = sorted(numbers, < )

print(numbers) // [3, 6, 18]

//
// MARK: Структуры
//

// Структуры и классы имеют очень похожие характеристики
struct NamesTable {
    let names = [String]()

    // Пользовательский индекс
    subscript(index: Int) -> String {
        return names[index]
    }
}

// У структур автогенерируемый (неявно) инициализатор
let namesTable = NamesTable(names: ["Me", "Them"])
let name = namesTable[1]
println("Name is \(name)") // Name is Them

//
// MARK: Классы
//

// Классы, структуры и их члены имеют трехуровневый контроль доступа
// Уровни: internal (по умолчанию), public, private

public class Shape {
    public func getArea() -> Int {
        return 0;
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
    println("Ага, это mySquare")
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
println(myCircle?.getArea())    // Optional(3)
println(myCircle!.getArea())    // 3
var myEmptyCircle = Circle(radius: -1)
println(myEmptyCircle?.getArea())    // "nil"
if let circle = myEmptyCircle {
    // не будет выполняться, поскольку myEmptyCircle равен nil
    println("circle не nil")
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

// Нецелочисленные перечисления требуют прямого указания значений
enum BookName: String {
    case John = "Иоанн"
    case Luke = "Лука"
}
println("Имя: \(BookName.John.rawValue)")

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
println(desk.description())     // "Письменный стол высотой 80 см."
var chair = Furniture.Chair("Foo", 40)
println(chair.description())    // "Стул марки Foo высотой 40 см."


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
// которые позволяют вам проверять на соответствие
@objc protocol TransformShape {
    optional func reshaped()
    optional func canReshape() -> Bool
}

class MyShape: Rect {
    var delegate: TransformShape?

    func grow() {
        sideLength += 2
        // Размещайте знак вопроса перед опционным свойством, методом
        // или индексом, чтобы не учитывать nil-значение и возвратить nil
        // вместо выбрасывания ошибки выполнения (т.н. "опционная цепочка")
        if let allow = self.delegate?.canReshape?() {
            // проверка делегата на выполнение метода
            self.delegate?.reshaped?()
        }
    }
}


//
// MARK: Прочее
//

// `extension`s: Добавляет расширенный функционал к существующему типу

// Класс Square теперь "соответствует" протоколу `Printable`
extension Square: Printable {
    var description: String {
        return "Площадь: \(self.getArea()) - ID: \(self.identifier)"
    }
}

println("Объект Square: \(mySquare)")

// Вы также можете расширить встроенные типы
extension Int {
    var customProperty: String {
        return "Это \(self)"
    }

    func multiplyBy(num: Int) -> Int {
        return num * self
    }
}

println(7.customProperty) // "Это 7"
println(14.multiplyBy(3)) // 42

// Обобщения: Подобно языкам Java и C#. Используйте ключевое слово `where`,
// чтобы определить условия обобщений.

func findIndex<T: Equatable>(array: [T], valueToFind: T) -> Int? {
    for (index, value) in enumerate(array) {
        if value == valueToFind {
            return index
        }
    }
    return nil
}
let foundAtIndex = findIndex([1, 2, 3, 4], 3)
println(foundAtIndex == 2) // вывод: true

// Операторы:
// Пользовательские операторы могут начинаться с символов:
//      / = - + * % < > ! & | ^ . ~
// или
// Unicode- знаков математики, символов, стрелок, декорации и линий/кубов,
// нарисованных символов.
prefix operator !!! {}

// Префиксный оператор, который утраивает длину стороны, когда используется
prefix func !!! (inout shape: Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// текущее значение
println(mySquare.sideLength) // 4

// Используя пользовательский оператор !!!, изменится длина стороны
// путем увеличения размера в 3 раза
!!!mySquare
println(mySquare.sideLength) // 12
```
