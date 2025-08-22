---
name: Swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
  - ["Christopher Bess", "http://github.com/cbess"]
  - ["Joey Huang", "http://github.com/kamidox"]
  - ["Anthony Nguyen", "http://github.com/anthonyn60"]
  - ["Clayton Walker", "https://github.com/cwalk"]
  - ["Fernando Valverde", "http://visualcosita.xyz"]
  - ["Alexey Nazaroff", "https://github.com/rogaven"]
  - ["@Samasaur1", "https://github.com/Samasaur1"]
filename: learnswift.swift
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Swift는 모든 Apple 운영 체제에서 개발하기 위해 Apple이 만든 프로그래밍 언어입니다. 오류가 있는 코드에 대해 더 탄력적이면서 Objective-C와 공존하도록 설계된 Swift는 2014년 Apple의 개발자 컨퍼런스 WWDC에서 소개되었습니다. 자동 메모리 관리, 타입 안전성 및 코드를 더 읽기 쉽고 오류가 적게 만드는 현대적인 구문을 특징으로 합니다.

Swift는 오픈 소스이며 Linux 및 Windows에서도 실행됩니다. 이 언어는 LLVM 컴파일러로 빌드되었으며 Xcode에 포함되어 있습니다.

공식 참조는 [docs.swift.org](https://docs.swift.org/swift-book/documentation/the-swift-programming-language)에서 제공되는 *The Swift Programming Language* 가이드입니다. 이 포괄적인 가이드는 최신 Swift 기능으로 정기적으로 업데이트되며 언어를 배우기 위한 권장 시작점입니다.

```swift
// 모듈 가져오기
import Foundation

// 한 줄 주석은 //로 시작합니다.
// 여러 줄 주석은 /*로 시작하여 */로 끝납니다.
/* 중첩된 여러 줄 주석
 /* 은 */
 허용됩니다.
 */

// Xcode는 코드에 주석을 달고 점프 바에 나열하는 랜드마크를 지원합니다.
// MARK: 섹션 마크
// MARK: - 구분선이 있는 섹션 마크
// TODO: 곧 무언가 해야 함
// FIXME: 이 코드 수정

//MARK: Hello, World
// Swift 3부터는 `print` 메서드를 사용하여 출력합니다.
// 자동으로 새 줄을 추가합니다.
print("Hello, world")

//
// MARK: - 변수
//


//상수를 선언하려면 `let`을, 변수를 선언하려면 `var`를 사용합니다.
let theAnswer = 42
var theQuestion = "What is the Answer?"
theQuestion = "How many roads must a man walk down?"
theQuestion = "What is six by nine?"
// 상수를 재할당하려고 하면 컴파일 타임 오류가 발생합니다.
//theAnswer = 54

// 변수와 상수 모두 값을 할당하기 전에 선언할 수 있지만,
// 사용하기 전에 값을 할당해야 합니다.
let someConstant: Int
var someVariable: String
// 이 줄은 오류를 발생시킵니다.
//print(someConstant)
//print(someVariable)
someConstant = 0
someVariable = "0"
// 이 줄은 이제 유효합니다.
print(someConstant)
print(someVariable)

// 위에서 볼 수 있듯이 변수 타입은 자동으로 추론됩니다.
// 타입을 명시적으로 선언하려면 변수 이름 뒤에
// 콜론으로 구분하여 작성합니다.
let aString: String = "A string"
let aDouble: Double = 0

// 값은 다른 타입으로 암시적으로 변환되지 않습니다.
// 원하는 타입의 인스턴스를 명시적으로 만듭니다.
let stringWithDouble = aString + String(aDouble)
let intFromDouble = Int(aDouble)

// 문자열의 경우 문자열 보간을 사용합니다.
let descriptionString = "The value of aDouble is \(aDouble)"
// 문자열 보간 안에 모든 표현식을 넣을 수 있습니다.
let equation = "Six by nine is \(6 * 9), not 42!"
// 큰따옴표와 백슬래시 이스케이프를 피하려면 문자열 구분 기호를 변경하십시오.
let explanationString = #"The string I used was "The value of aDouble is \(aDouble)" and the result was \#(descriptionString)"#
// 여는 따옴표 앞에 원하는 만큼 숫자 기호를 넣을 수 있습니다.
// 닫는 따옴표에서 일치시키기만 하면 됩니다. 또한 이스케이프 문자를
// 백슬래시 뒤에 동일한 수의 숫자 기호로 변경합니다.

let multiLineString = """
    This is a multi-line string.
    It's called that because it takes up multiple lines (wow!)
        Any indentation beyond the closing quotation marks is kept, the rest is discarded.
    You can include " or "" in multi-line strings because the delimiter is three "s.
    """

// 배열
let shoppingList = ["catfish", "water", "tulips",] //마지막 요소 뒤에 쉼표를 허용합니다.
let secondElement = shoppingList[1] // 배열은 0부터 인덱싱됩니다.

// let으로 선언된 배열은 불변입니다. 다음 줄은 컴파일 타임 오류를 발생시킵니다.
//shoppingList[2] = "mango"

// 배열은 구조체이므로(나중에 자세히 설명), 동일한 객체를 참조하는 대신 복사본을 만듭니다.
var mutableShoppingList = shoppingList
mutableShoppingList[2] = "mango"

// ==는 동등성입니다.
shoppingList == mutableShoppingList // false

// let으로 선언된 딕셔너리도 불변입니다.
var occupations = [
    "Malcolm": "Captain",
    "Kaylee": "Mechanic"
]
occupations["Jayne"] = "Public Relations"
// 딕셔너리도 구조체이므로 이것도 복사본을 만듭니다.
let immutableOccupations = occupations

immutableOccupations == occupations // true

// 배열과 딕셔너리는 모두 요소를 추가하면 자동으로 커집니다.
mutableShoppingList.append("blue paint")
occupations["Tim"] = "CEO"

// 둘 다 비울 수 있습니다.
mutableShoppingList = []
occupations = [:]

let emptyArray = [String]()
let emptyArray2 = Array<String>() // 위와 동일
// [T]는 Array<T>의 약어입니다.
let emptyArray3: [String] = [] // 타입을 명시적으로 선언하면 빈 배열로 설정할 수 있습니다.
let emptyArray4: Array<String> = [] // 위와 동일

// [Key: Value]는 Dictionary<Key, Value>의 약어입니다.
let emptyDictionary = [String: Double]()
let emptyDictionary2 = Dictionary<String, Double>() // 위와 동일
var emptyMutableDictionary: [String: Double] = [:]
var explicitEmptyMutableDictionary: Dictionary<String, Double> = [:] // 위와 동일

// MARK: 기타 변수
let øπΩ = "value" // 유니코드 변수 이름
let 🤯 = "wow" // 이모티콘 변수 이름

// 키워드를 변수 이름으로 사용할 수 있습니다.
// 이것들은 지금 사용되지 않는 문맥 키워드이므로 허용됩니다.
let convenience = "keyword"
let weak = "another keyword"
let override = "another keyword"

// 백틱을 사용하면 일반적으로 허용되지 않는 경우에도 키워드를 변수 이름으로 사용할 수 있습니다.
let `class` = "keyword"

// MARK: - 옵셔널

/*
 옵셔널은 값이 있거나 값이 없음을 나타내는 nil(값 없음)을
 포함하는 Swift 언어 기능입니다.
 Nil은 다른 언어의 `null`과 거의 동일합니다.
 타입 뒤의 물음표(?)는 해당 타입의 옵셔널 값으로 표시합니다.

 타입이 옵셔널이 아닌 경우 값이 보장됩니다.

 Swift는 모든 속성에 타입이 있어야 하므로 nil조차도
 명시적으로 옵셔널 값으로 저장해야 합니다.

 Optional<T>는 .none (nil)과 .some(T) (값) 케이스가 있는 열거형입니다.
 */

var someOptionalString: String? = "optional" // nil일 수 있음
// T?는 Optional<T>의 약어입니다. — ?는 후위 연산자입니다 (구문 설탕)
let someOptionalString2: Optional<String> = nil
let someOptionalString3 = String?.some("optional") // 첫 번째와 동일
let someOptionalString4 = String?.none //nil

/*
 값이 있는 옵셔널의 값에 액세스하려면 후위 연산자 !를
 사용하여 강제 언래핑합니다. 강제 언래핑은 "이 옵셔널에
 확실히 값이 있다는 것을 알고 있으니 주세요"라고 말하는 것과 같습니다.

 !를 사용하여 존재하지 않는 옵셔널 값에 액세스하려고 하면
 런타임 오류가 발생합니다. !를 사용하여 값을 강제 언래핑하기 전에
 항상 옵셔널에 nil이 아닌 값이 포함되어 있는지 확인하십시오.
 */

if someOptionalString != nil {
    // nil이 아님
    if someOptionalString!.hasPrefix("opt") {
        print("has the prefix")
    }
}

// Swift는 "옵셔널 체이닝"을 지원합니다. 즉, 옵셔널 값의 함수를
// 호출하거나 속성을 가져올 수 있으며 적절한 타입의 옵셔널이 됩니다.
// 이 작업을 여러 번 수행할 수 있으므로 "체이닝"이라는 이름이 붙었습니다.

let empty = someOptionalString?.isEmpty // Bool?

// if-let 구조 -
// if-let은 Swift의 특별한 구조로, 옵셔널 rhs에
// 값이 있는지 확인하고, 있으면 언래핑하여
// lhs에 할당할 수 있습니다.
if let someNonOptionalStringConstant = someOptionalString {
    // `Some` 값이 있음, nil이 아님
    // someOptionalStringConstant는 String? 타입이 아닌 String 타입입니다.
    if !someNonOptionalStringConstant.hasPrefix("ok") {
        // 접두사가 없음
    }
}

//if-var도 허용됩니다!
if var someNonOptionalString = someOptionalString {
    someNonOptionalString = "Non optional AND mutable"
    print(someNonOptionalString)
}

// 하나의 if-let 문에서 여러 옵셔널 값을 바인딩할 수 있습니다.
// 바인딩된 값 중 하나라도 nil이면 if 문이 실행되지 않습니다.
if let first = someOptionalString, let second = someOptionalString2,
    let third = someOptionalString3, let fourth = someOptionalString4 {
    print("\(first), \(second), \(third), and \(fourth) are all not nil")
}

//if-let은 ","(쉼표) 절을 지원하며, 이는 새로 바인딩된
// 옵셔널 값에 대한 조건을 강제하는 데 사용할 수 있습니다.
// 할당과 "," 절 모두 통과해야 합니다.
let someNumber: Int? = 7
if let num = someNumber, num > 3 {
    print("num is not nil and is greater than 3")
}

// 암시적으로 언래핑된 옵셔널 — 언래핑할 필요가 없는 옵셔널 값
let unwrappedString: String! = "Value is expected."

// 차이점은 다음과 같습니다.
let forcedString = someOptionalString! // 느낌표 필요
let implicitString = unwrappedString // 느낌표 필요 없음

/*
 암시적으로 언래핑된 옵셔널은 사용될 때마다 자동으로
 언래핑되도록 허용하는 것으로 생각할 수 있습니다.
 사용할 때마다 옵셔널 이름 뒤에 느낌표를 붙이는 대신,
 선언할 때 옵셔널 타입 뒤에 느낌표를 붙입니다.
 */

// 그렇지 않으면 암시적으로 언래핑된 옵셔널을 일반 옵셔널과 동일하게
// 취급할 수 있습니다 (예: if-let, != nil 등).

// Swift 5 이전에는 T!가 ImplicitlyUnwrappedOptional<T>의 약어였습니다.
// Swift 5 이상에서는 ImplicitlyUnwrappedOptional을 사용하면 컴파일 타임 오류가 발생합니다.
//var unwrappedString2: ImplicitlyUnwrappedOptional<String> = "Value is expected." //오류

// nil 병합 연산자 ??는 옵셔널이 nil이 아닌 값을 포함하는 경우 언래핑하거나 기본값을 반환합니다.
someOptionalString = nil
let someString = someOptionalString ?? "abc"
print(someString) // abc
// a ?? b는 a != nil ? a! : b의 약어입니다.

// MARK: - 제어 흐름

let condition = true
if condition { print("condition is true") } // 중괄호를 생략할 수 없음

if theAnswer > 50 {
    print("theAnswer > 50")
} else if condition {
    print("condition is true")
} else {
    print("Neither are true")
}

// `if` 문의 조건은 `Bool`이어야 하므로 다음 코드는 오류이며, 0과 암시적으로 비교되지 않습니다.
//if 5 {
//    print("5 is not zero")
//}

// Switch
// 철저해야 함
// 암시적으로 fall through하지 않음, fallthrough 키워드 사용
// 매우 강력함, `if` 문에 구문 설탕을 더한 것으로 생각
// String, 객체 인스턴스 및 기본 타입(Int, Double 등) 지원
let vegetable = "red pepper"
let vegetableComment: String
switch vegetable {
case "celery":
    vegetableComment = "Add some raisins and make ants on a log."
case "cucumber", "watercress": // 여러 값 일치
    vegetableComment = "That would make a good tea sandwich."
case let localScopeValue where localScopeValue.hasSuffix("pepper"):
    vegetableComment = "Is it a spicy \(localScopeValue)?"
default: // 필수 (모든 가능한 입력을 처리하기 위해)
    vegetableComment = "Everything tastes good in soup."
}
print(vegetableComment)

// `for-in` 루프를 사용하여 배열, 딕셔너리, 범위 등과 같은 시퀀스를 반복합니다.
for element in shoppingList {
    print(element) // shoppingList는 `[String]` 타입이므로 element는 `String` 타입입니다.
}
//딕셔너리를 반복해도 특정 순서가 보장되지 않습니다.
for (person, job) in immutableOccupations {
    print("\(person)'s job is \(job)")
}
for i in 1...5 {
    print(i, terminator: " ") // "1 2 3 4 5" 출력
}
for i in 0..<5 {
    print(i, terminator: " ") // "0 1 2 3 4" 출력
}
//for index in range는 C 스타일 for 루프를 대체할 수 있습니다.
//    for (int i = 0; i < 10; i++) {
//        //코드
//    }
//는 다음과 같습니다.
//    for i in 0..<10 {
//        //코드
//    }
//1보다 큰 단계로 이동하려면 stride(from:to:by:) 또는 stride(from:through:by) 함수를 사용합니다.
//`for i in stride(from: 0, to: 10, by: 2)`는 `for (int i = 0; i < 10; i += 2)`와 동일합니다.
//`for i in stride(from: 0, through: 10, by: 2)`는 `for (int i = 0; i <= 10; i += 2)`와 동일합니다.

// while 루프는 대부분의 언어와 같습니다.
var i = 0
while i < 5 {
    i += Bool.random() ? 1 : 0
    print(i)
}

// 이것은 다른 언어의 do-while 루프와 같습니다. — 루프의 본문은 최소 한 번 실행됩니다.
repeat {
    i -= 1
    i += Int.random(in: 0...3)
} while i < 5

// continue 문은 다음 반복에서 루프를 계속 실행합니다.
// break 문은 루프를 즉시 종료합니다.

// MARK: - 함수

// 함수는 일급 타입이므로 함수에 중첩될 수 있으며 전달될 수 있습니다.

// Swift 헤더 문서가 있는 함수 (Swift 수정 마크다운 구문으로 서식 지정)

/// 인사 작업입니다.
///
/// - 매개변수:
///   - name: 이름입니다.
///   - day: 요일입니다.
/// - 반환값: 이름과 요일 값을 포함하는 문자열입니다.
func greet(name: String, day: String) -> String {
    return "Hello \(name), today is \(day)."
}
greet(name: "Bob", day: "Tuesday")

// 이상적으로는 함수 이름과 매개변수 레이블이 결합되어 문장과 유사한 함수 호출을 만듭니다.
func sayHello(to name: String, onDay day: String) -> String {
    return "Hello \(name), the day is \(day)"
}
sayHello(to: "John", onDay: "Sunday")

//아무것도 반환하지 않는 함수는 반환 화살표를 생략할 수 있습니다. Void를 반환한다고 말할 필요가 없습니다(물론 가능합니다).
func helloWorld() {
    print("Hello, World!")
}

// 인수 레이블은 비워 둘 수 있습니다.
func say(_ message: String) {
    print(#"I say "\#(message)""#)
}
say("Hello")

// 기본 매개변수는 함수를 호출할 때 생략할 수 있습니다.
func printParameters(requiredParameter r: Int, optionalParameter o: Int = 10) {
    print("The required parameter was \(r) and the optional parameter was \(o)")
}
printParameters(requiredParameter: 3)
printParameters(requiredParameter: 3, optionalParameter: 6)

// 가변 인자 — 함수당 한 세트만.
func setup(numbers: Int...) {
    // 배열입니다.
    let _ = numbers[0]
    let _ = numbers.count
}

// 참조에 의한 전달
func swapTwoInts(a: inout Int, b: inout Int) {
    let tempA = a
    a = b
    b = tempA
}
var someIntA = 7
var someIntB = 3
swapTwoInts(a: &someIntA, b: &someIntB) //변수 이름 앞에 &를 붙여 호출해야 합니다.
print(someIntB) // 7

type(of: greet) // (String, String) -> String
type(of: helloWorld) // () -> Void

// 함수 전달 및 반환
func makeIncrementer() -> ((Int) -> Int) {
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne
}
var increment = makeIncrementer()
increment(7)

func performFunction(_ function: (String, String) -> String, on string1: String, and string2: String) {
    let result = function(string1, string2)
    print("The result of calling the function on \(string1) and \(string2) was \(result)")
}

// 튜플로 여러 항목을 반환하는 함수
func getGasPrices() -> (Double, Double, Double) {
    return (3.59, 3.69, 3.79)
}
let pricesTuple = getGasPrices()
let price = pricesTuple.2 // 3.79
// _ (밑줄)을 사용하여 튜플(또는 다른) 값 무시
let (_, price1, _) = pricesTuple // price1 == 3.69
print(price1 == pricesTuple.1) // true
print("Gas price: \(price)")

// 레이블/이름이 있는 튜플 매개변수
func getGasPrices2() -> (lowestPrice: Double, highestPrice: Double, midPrice: Double) {
    return (1.77, 37.70, 7.37)
}
let pricesTuple2 = getGasPrices2()
let price2 = pricesTuple2.lowestPrice
let (_, price3, _) = pricesTuple2
print(pricesTuple2.highestPrice == pricesTuple2.1) // true
print("Highest gas price: \(pricesTuple2.highestPrice)")

// guard 문
func testGuard() {
    // guard는 조기 종료 또는 중단을 제공하여 오류 처리 코드를 조건에 가깝게 배치합니다.
    // 선언하는 변수를 guard 문과 동일한 범위에 배치합니다.
    // "파멸의 피라미드"를 피하기 쉽게 만듭니다.
    guard let aNumber = Optional<Int>(7) else {
        return // guard 문은 반드시 있는 범위를 종료해야 합니다.
        // 일반적으로 `return` 또는 `throw`를 사용합니다.
    }

    print("number is \(aNumber)")
}
testGuard()

// print 함수는 다음과 같이 선언됩니다.
//     func print(_ input: Any..., separator: String = " ", terminator: String = "\n")
// 줄 바꿈 없이 출력하려면:
print("No newline", terminator: "")
print("!")

// MARK: - 클로저

var numbers = [1, 2, 6]

// 함수는 특수한 경우의 클로저({})입니다.

// 클로저 예제.
// `->`는 인수와 반환 타입을 구분합니다.
// `in`은 클로저 헤더와 클로저 본문을 구분합니다.
numbers.map({
    (number: Int) -> Int in
    let result = 3 * number
    return result
})

// 위와 같이 타입이 알려진 경우 다음과 같이 할 수 있습니다.
numbers = numbers.map({ number in 3 * number })
// 또는 이렇게도 할 수 있습니다.
//numbers = numbers.map({ $0 * 3 })

print(numbers) // [3, 6, 18]

// 후행 클로저
numbers = numbers.sorted { $0 > $1 }

print(numbers) // [18, 6, 3]

// MARK: - 열거형

// 열거형은 선택적으로 특정 타입이거나 자체적으로 있을 수 있습니다.
// 클래스처럼 메서드를 포함할 수 있습니다.

enum Suit {
    case spades, hearts, diamonds, clubs
    var icon: Character {
        switch self {
        case .spades:
            return "♤"
        case .hearts:
            return "♡"
        case .diamonds:
            return "♢"
        case .clubs:
            return "♧"
        }
    }
}

// 열거형 값은 약식 구문을 허용하며, 변수가 명시적으로
// 선언된 경우 열거형 타입을 입력할 필요가 없습니다.
var suitValue: Suit = .hearts

// CaseIterable 프로토콜을 준수하면 모든 값을 포함하는
// allCases 속성이 자동으로 합성됩니다.
// 연관된 값이나 @available 속성이 없는 열거형에서 작동합니다.
enum Rank: CaseIterable {
    case ace
    case two, three, four, five, six, seven, eight, nine, ten
    case jack, queen, king
    var icon: String {
        switch self {
        case .ace:
            return "A"
        case .two:
            return "2"
        case .three:
            return "3"
        case .four:
            return "4"
        case .five:
            return "5"
        case .six:
            return "6"
        case .seven:
            return "7"
        case .eight:
            return "8"
        case .nine:
            return "9"
        case .ten:
            return "10"
        case .jack:
            return "J"
        case .queen:
            return "Q"
        case .king:
            return "K"
        }
    }
}

for suit in [Suit.clubs, .diamonds, .hearts, .spades] {
    for rank in Rank.allCases {
        print("\(rank.icon)\(suit.icon)")
    }
}

// 문자열 열거형은 직접적인 원시 값 할당을 가질 수 있거나
// 원시 값은 열거형 필드에서 파생됩니다.
enum BookName: String {
    case john
    case luke = "Luke"
}
print("Name: \(BookName.john.rawValue)")

// 연관된 값이 있는 열거형
enum Furniture {
    // Int와 연관
    case desk(height: Int)
    // String 및 Int와 연관
    case chair(String, Int)

    func description() -> String {
        // let의 두 위치 모두 허용됩니다.
        switch self {
        case .desk(let height):
            return "Desk with \(height) cm"
        case let .chair(brand, height):
            return "Chair of \(brand) with \(height) cm"
        }
    }
}

var desk: Furniture = .desk(height: 80)
print(desk.description())     // "Desk with 80 cm"
var chair = Furniture.chair("Foo", 40)
print(chair.description())    // "Chair of Foo with 40 cm"

// MARK: - 구조체 및 클래스

/*
 Swift의 구조체와 클래스는 많은 공통점을 가지고 있습니다. 둘 다 다음을 할 수 있습니다.
 - 값을 저장하기 위한 속성 정의
 - 기능을 제공하기 위한 메서드 정의
 - 첨자 구문을 사용하여 값에 액세스하기 위한 첨자 정의
 - 초기 상태를 설정하기 위한 초기화자 정의
 - 기본 구현을 넘어 기능을 확장하기 위해 확장
 - 특정 종류의 표준 기능을 제공하기 위해 프로토콜 준수

 클래스는 구조체에 없는 추가 기능을 가지고 있습니다.
 - 상속을 통해 한 클래스가 다른 클래스의 특성을 상속할 수 있습니다.
 - 타입 캐스팅을 통해 런타임에 클래스 인스턴스의 타입을 확인하고 해석할 수 있습니다.
 - 소멸자를 통해 클래스 인스턴스가 할당한 모든 리소스를 해제할 수 있습니다.
 - 참조 계산을 통해 클래스 인스턴스에 대한 참조가 둘 이상 있을 수 있습니다.

 이러한 이유 중 하나로 클래스를 사용해야 하는 경우가 아니라면 구조체를 사용하십시오.

 구조체는 값 타입이고 클래스는 참조 타입입니다.
 */

// MARK: 구조체

struct NamesTable {
    let names: [String]

    // 사용자 정의 첨자
    subscript(index: Int) -> String {
        return names[index]
    }
}

// 구조체에는 자동 생성된(암시적) 지정된 "멤버별" 초기화자가 있습니다.
let namesTable = NamesTable(names: ["Me", "Them"])
let name = namesTable[1]
print("Name is \(name)") // Name is Them

// MARK: 클래스

class Shape {
    func getArea() -> Int {
        return 0
    }
}

class Rect: Shape {
    var sideLength: Int = 1

    // 사용자 정의 getter 및 setter 속성
    var perimeter: Int {
        get {
            return 4 * sideLength
        }
        set {
            // `newValue`는 setter에서 사용할 수 있는 암시적 변수입니다.
            sideLength = newValue / 4
        }
    }

    // 계산된 속성은 변경될 수 있으므로 `var`로 선언해야 합니다.
    var smallestSideLength: Int {
        return self.sideLength - 1
    }

    // 속성을 지연 로드합니다.
    // subShape는 getter가 호출될 때까지 nil(초기화되지 않음)으로 유지됩니다.
    lazy var subShape = Rect(sideLength: 4)

    // 사용자 정의 getter 및 setter가 필요하지 않지만
    // 속성을 가져오거나 설정하기 전후에 코드를 실행하려면
    // `willSet` 및 `didSet`을 사용할 수 있습니다.
    var identifier: String = "defaultID" {
        // `someIdentifier` 인수는 새 값의 변수 이름이 됩니다.
        willSet(someIdentifier) {
            print(someIdentifier)
        }
    }

    init(sideLength: Int) {
        self.sideLength = sideLength
        // 사용자 정의 속성을 초기화할 때 항상 super.init을 마지막에 호출합니다.
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

// 간단한 클래스 `Square`는 `Rect`를 확장합니다.
class Square: Rect {
    // 편의 초기화자를 사용하여 지정된 초기화자를 더 빠르고 "편리하게" 호출합니다.
    // 편의 초기화자는 동일한 클래스의 다른 초기화자를 호출하고 하나 이상의 매개변수에 기본값을 전달합니다.
    // 편의 초기화자도 매개변수를 가질 수 있으며, 이는 호출된 초기화자 매개변수를 사용자 정의하거나 전달된 값에 따라 적절한 초기화자를 선택하는 데 유용합니다.
    convenience init() {
        self.init(sideLength: 5)
    }
}

var mySquare = Square()
print(mySquare.getArea()) // 25
mySquare.shrink()
print(mySquare.sideLength) // 4

// 인스턴스 캐스팅
let aShape = mySquare as Shape

// 인스턴스 다운캐스팅:
// 다운캐스팅은 실패할 수 있으므로 결과는 옵셔널(as?) 또는 암시적으로 언래핑된 옵셔널(as!)일 수 있습니다.
let anOptionalSquare = aShape as? Square // aShape가 Square가 아니면 nil을 반환합니다.
let aSquare = aShape as! Square // aShape가 Square가 아니면 런타임 오류가 발생합니다.

// 객체를 비교하는 ==와 달리 인스턴스를 비교합니다.
if mySquare === mySquare {
    print("Yep, it's mySquare")
}

// 옵셔널 초기화
class Circle: Shape {
    var radius: Int
    override func getArea() -> Int {
        return 3 * radius * radius
    }

    // `init` 뒤에 물음표 접미사는 옵셔널 초기화입니다.
    // nil을 반환할 수 있습니다.
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
    // myEmptyCircle이 nil이므로 실행되지 않습니다.
    print("circle is not nil")
}

// MARK: - 프로토콜

// 프로토콜은 다른 언어의 인터페이스라고도 합니다.

// `protocol`은 준수하는 타입이 특정
// 인스턴스 속성, 인스턴스 메서드, 타입 메서드,
// 연산자 및 첨자를 갖도록 요구할 수 있습니다.

protocol ShapeGenerator {
    var enabled: Bool { get set }
    func buildShape() -> Shape
}

// MARK: - 기타

// MARK: 타입 별칭

// 타입 별칭을 사용하면 한 타입(또는 타입의 구성)을 다른 이름으로 참조할 수 있습니다.
typealias Integer = Int
let myInteger: Integer = 0

// MARK: = 연산자

// 할당은 값을 반환하지 않습니다. 즉, 조건문에서 사용할 수 없으며,
// 다음 문장도 불법입니다.
//    let multipleAssignment = theQuestion = "No questions asked"
//하지만 이렇게 할 수 있습니다.
let multipleAssignment = "No questions asked", secondConstant = "No answers given"

// MARK: 범위

// ..< 및 ... 연산자는 범위를 생성합니다.

// ...는 양쪽 끝을 포함합니다("닫힌 범위") — 수학적으로 [0, 10]
let _0to10 = 0...10
// ..<는 왼쪽은 포함하고 오른쪽은 제외합니다("범위") — 수학적으로 [0, 10)
let singleDigitNumbers = 0..<10
// 한쪽 끝을 생략할 수 있습니다("PartialRangeFrom") — 수학적으로 [0, ∞)
let toInfinityAndBeyond = 0...
// 또는 다른 쪽 끝("PartialRangeTo") — 수학적으로 (-∞, 0)
let negativeInfinityToZero = ..<0
// ("PartialRangeThrough") — 수학적으로 (-∞, 0]
let negativeInfinityThroughZero = ...0

// MARK: 와일드카드 연산자

// Swift에서 _(밑줄)은 와일드카드 연산자로, 값을 무시할 수 있습니다.

// 인수 레이블 없이 함수를 선언할 수 있습니다.
func function(_ labelLessParameter: Int, label labeledParameter: Int, labelAndParameterName: Int) {
    print(labelLessParameter, labeledParameter, labelAndParameterName)
}
function(0, label: 0, labelAndParameterName: 0)

// 함수의 반환 값을 무시할 수 있습니다.
func printAndReturn(_ str: String) -> String {
    print(str)
    return str
}
let _ = printAndReturn("Some String")

// 튜플의 일부를 무시하고 일부를 유지할 수 있습니다.
func returnsTuple() -> (Int, Int) {
    return (1, 2)
}
let (_, two) = returnsTuple()

// 클로저 매개변수를 무시할 수 있습니다.
let closure: (Int, Int) -> String = { someInt, _ in
    return "\(someInt)"
}
closure(1, 2) // 1 반환

// for 루프에서 값을 무시할 수 있습니다.
for _ in 0..<10 {
    // 10번 실행할 코드
}

// MARK: 접근 제어

/*
 Swift에는 5가지 수준의 접근 제어가 있습니다.
 - Open: 가져오는 모든 모듈에서 액세스 가능하고 하위 클래스화 가능합니다.
 - Public: 가져오는 모든 모듈에서 액세스 가능하며, 선언된 모듈에서 하위 클래스화 가능합니다.
 - Internal: 선언된 모듈에서 액세스 가능하고 하위 클래스화 가능합니다.
 - Fileprivate: 선언된 파일에서 액세스 가능하고 하위 클래스화 가능합니다.
 - Private: 둘러싸는 선언에서 액세스 가능하고 하위 클래스화 가능합니다(내부 클래스/구조체/열거형 생각).

 자세한 내용은 여기를 참조하십시오: https://docs.swift.org/swift-book/LanguageGuide/AccessControl.html
 */

// MARK: 재정의 방지

// 클래스나 인스턴스 메서드, 또는 속성 앞에 `final` 키워드를 추가하여 재정의를 방지할 수 있습니다.
class Shape {
    final var finalInteger = 10
}

// 클래스가 하위 클래스화되는 것을 방지
final class ViewManager {
}

// MARK: 조건부 컴파일, 컴파일 타임 진단 및 가용성 조건

// 조건부 컴파일
#if false
print("This code will not be compiled")
#else
print("This code will be compiled")
#endif
/*
 옵션은 다음과 같습니다.
 os()                   macOS, iOS, watchOS, tvOS, Linux
 arch()                 i386, x86_64, arm, arm64
 swift()                >= 또는 < 뒤에 버전 번호
 compiler()             >= 또는 < 뒤에 버전 번호
 canImport()            모듈 이름
 targetEnvironment()    simulator
 */
#if swift(<3)
println()
#endif

// 컴파일 타임 진단
// #warning(message) 및 #error(message)를 사용하여 컴파일러가 경고 및/또는 오류를 내도록 할 수 있습니다.
#warning("This will be a compile-time warning")
//  #error("This would be a compile-time error")

//가용성 조건
if #available(iOSMac 10.15, *) {
    // macOS 10.15를 사용할 수 있으므로 여기에서 사용할 수 있습니다.
} else {
    // macOS 10.15를 사용할 수 없으므로 대체 API를 사용합니다.
}

// MARK: Any 및 AnyObject

// Swift는 모든 타입의 값을 저장하는 것을 지원합니다.
// 이를 위해 `Any`와 `AnyObject`라는 두 가지 키워드가 있습니다.
// `AnyObject` == Objective-C의 `id`
// `Any`는 모든 값(클래스, Int, 구조체 등)과 함께 작동합니다.
var anyVar: Any = 7
anyVar = "Changed value to a string, not good practice, but possible."
let anyObjectVar: AnyObject = Int(1) as NSNumber

// MARK: 확장

// 확장을 사용하면 이미 선언된 타입에 추가 기능을 추가할 수 있으며, 소스 코드가 없는 타입에도 추가할 수 있습니다.

// Square는 이제 `CustomStringConvertible` 프로토콜을 "준수"합니다.
extension Square: CustomStringConvertible {
    var description: String {
        return "Area: \(self.getArea()) - ID: \(self.identifier)"
    }
}

print("Square: \(mySquare)")

// 내장 타입을 확장할 수도 있습니다.
extension Int {
    var doubled: Int {
        return self * 2
    }

    func multipliedBy(num: Int) -> Int {
        return num * self
    }

    mutating func multiplyBy(num: Int) {
        self *= num
    }
}

print(7.doubled) // 14
print(7.doubled.multipliedBy(num: 3)) // 42

// MARK: 제네릭

// 제네릭: Java 및 C#과 유사합니다. `where` 키워드를 사용하여
// 제네릭의 요구 사항을 지정합니다.

func findIndex<T: Equatable>(array: [T], valueToFind: T) -> Int? {
    for (index, value) in array.enumerated() {
        if value == valueToFind {
            return index
        }
    }
    return nil
}
findIndex(array: [1, 2, 3, 4], valueToFind: 3) // Optional(2)

// 제네릭으로 타입을 확장할 수도 있습니다.
extension Array where Array.Element == Int {
    var sum: Int {
        var total = 0
        for el in self {
            total += el
        }
        return total
    }
}

// MARK: 연산자

// 사용자 정의 연산자는 다음 문자로 시작할 수 있습니다.
//      / = - + * % < > ! & | ^ . ~
// 또는
// 유니코드 수학, 기호, 화살표, 딩뱃 및 선/상자 그리기 문자.
prefix operator !!!

// 사용 시 측면 길이를 세 배로 늘리는 접두사 연산자
prefix func !!! (shape: inout Square) -> Square {
    shape.sideLength *= 3
    return shape
}

// 현재 값
print(mySquare.sideLength) // 4

// 사용자 정의 !!! 연산자를 사용하여 측면 길이 변경, 크기를 3배 증가
!!!mySquare
print(mySquare.sideLength) // 12

// 연산자는 제네릭일 수도 있습니다.
infix operator <->
func <-><T: Equatable> (a: inout T, b: inout T) {
    let c = a
    a = b
    b = c
}

var foo: Float = 10
var bar: Float = 20

foo <-> bar
print("foo is \(foo), bar is \(bar)") // "foo is 20.0, bar is 10.0"

// MARK: - 오류 처리

// `Error` 프로토콜은 오류를 throw하고 catch할 때 사용됩니다.
enum MyError: Error {
    case badValue(msg: String)
    case reallyBadValue(msg: String)
}

// `throws`로 표시된 함수는 `try`를 사용하여 호출해야 합니다.
func fakeFetch(value: Int) throws -> String {
    guard 7 == value else {
        throw MyError.reallyBadValue(msg: "Some really bad value")
    }

    return "test"
}

func testTryStuff() {
    // 오류가 발생하지 않을 것으로 가정하고, 그렇지 않으면 런타임 예외가 발생합니다.
    let _ = try! fakeFetch(value: 7)

    // 오류가 발생하면 계속 진행하지만, 값이 nil이면
    // 이미 옵셔널인 경우에도 모든 반환 값을 옵셔널로 래핑합니다.
    let _ = try? fakeFetch(value: 7)

    do {
        // `catch` 블록을 통해 오류 처리를 제공하는 일반적인 try 작업
        try fakeFetch(value: 1)
    } catch MyError.badValue(let msg) {
        print("Error message: \(msg)")
    } catch {
        // 철저해야 함
    }
}
testTryStuff()
```
