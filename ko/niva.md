---
name: niva
filename: main.niva
contributors:
    - ["gavr", "https://github.com/gavr123456789"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

## 소개
Niva는 Smalltalk에서 많은 영감을 받은 간단한 언어입니다.
그러나 함수형 측면과 정적 타입에 더 가깝습니다.
모든 것은 여전히 객체이지만 클래스, 인터페이스, 상속 및 추상 클래스 대신
다형성을 달성하는 유일한 방법인 태그된 유니온이 있습니다.


예를 들어, 선언을 제외한 모든 것은 객체에 메시지를 보내는 것입니다.
`1 + 2`는 + 연산자가 아니라 `Int` 객체에 대한 `... + Int` 메시지입니다.
(추가 비용은 없습니다)

C와 유사: `1.inc()`
Niva: `1 inc`

본질적으로 niva는 조상인 Smalltalk처럼 매우 미니멀리즘적입니다.
유형, 유니온 및 관련 메서드를 도입합니다.
함수는 없습니다.

설치:
```bash
git clone https://github.com/gavr123456789/Niva.git
cd Niva
./gradlew buildJvmNiva
# LSP 여기 https://github.com/gavr123456789/niva-vscode-bundle
```

## 기본

#### 변수
변수는 기본적으로 불변입니다.
변수를 선언하는 특별한 키워드는 없습니다.

```Scala
// 이것은 주석입니다
int = 1
str = "qwf"
boolean = true
char = 'a'
float = 3.14f
double = 3.14
mutltiLineStr = """
qwf ars zxc \n \t "qwf"
"""
explicit_type::Int = 5


list = {1 2 3}
set = #(1 2 3)
map = #{1 'a' 2 'b'}

// 가변 변수 선언
mut x = 5
x <- 6 // 변경
```

#### 메시지
```Scala
// hello world는 한 줄입니다
"Hello world" echo // echo는 String 객체에 대한 메시지입니다


// 3가지 유형의 메시지가 있습니다
1 inc // 2         단항
1 + 2 // 3         이항
"abc" at: 0 // 'a' 키워드


// 연결할 수 있습니다
1 inc inc inc dec // 3
1 + 1 + 2 - 3 // 1
1 to: 3 do: [it echo] // 1 2 3
// 여기 마지막 to:do:는 단일 메시지입니다
// 2개의 키워드 메시지를 연결하려면 괄호로 묶어야 합니다
("123456" drop: 1) dropLast: 2 // "234"
쉼표 ,는 동일한 효과를 위한 구문 설탕입니다
"123456" drop: 1, dropLast: 2 // "234"

// 혼합할 수 있습니다
1 inc + 3 dec - "abc" count // 2 + 2 - 3 -> 1
"123" + "456" drop: 1 + 1   // "123456" drop: 2 -> "3456"

// niva에서 타입 및 메시지 선언을 제외한 모든 것은 메시지 전송입니다
// 예를 들어 `if`는 람다를 받는 Boolean 객체에 대한 메시지입니다
1 > 2 ifTrue: ["wow" echo]
// 표현식
base = 1 > 2 ifTrue: ["heh"] ifFalse: ["qwf"]
// while도 마찬가지
mut q = 0
[q > 10] whileTrue: [q <- q inc]
// 이 모든 것은 컴파일 타임에 인라이닝되기 때문에 비용이 없습니다
```

#### 타입
niva에서 새 줄은 중요하지 않습니다
타입 선언은 필드와 타입으로 구성된 키워드 메시지처럼 보입니다
```Scala
type Square side: Int

type Person
  name: String
  age: Int
```

#### 인스턴스 생성
객체를 생성하는 것은 선언할 때와 동일한 키워드 메시지이지만 타입 대신 값이 있습니다.
```Scala
square = Square side: 42
alice = Person name: "Alice" age: 24

// 이름으로 필드 분해
{age name} = alice
```

#### 필드 액세스
필드를 가져오는 것은 객체에 이름으로 단항 메시지를 보내는 것과 같습니다
```Scala
// 나이를 가져와 1을 더하고 출력합니다
alice age inc echo // 25
```

#### 타입에 대한 메서드:
Smalltalk에서처럼 모든 것이 객체이므로 모든 것에 메서드를 선언할 수 있습니다.
여기에서는 Int에 `double` 메서드를 추가한 다음 Square의 `perimeter` 메서드 내에서 사용합니다.

```Scala
Int double = this + this
Square perimeter = side double

square = Square side: 42
square perimeter // 호출


// 명시적 반환 타입
Int double2 -> Int = this * 2

// 본문 포함
Int double3 -> Int = [
    result = this * 2
    ^ result // ^는 반환입니다
]
```


#### 키워드 메시지 선언
```Scala
type Range from: Int to: Int
// `to`라는 하나의 인수가 있는 키워드 메시지
Int to::Int = Range from: this to: to

1 to: 2 // Range
```
#### 타입 생성자
타입 생성자는 특정 인스턴스가 아닌 타입 자체에 대한 메시지로 작동합니다.
```Scala
constructor Float pi = 3.14
x = Float pi // 3.14
```

기본값으로 필드를 초기화하는 데 유용할 수 있습니다.
```Scala
type Point x: Int y: Int
constructor Point atStart = Point x: 0 y: 0

p1 = Point x: 0 y: 0
p2 = Point atStart
// 생성자는 일반적인 메시지이므로 매개변수를 가질 수 있습니다
constructor Point y::Int = Point x: 0 y: y
p3 = Point y: 20 // x: 0 y: 20
```


#### 조건
If는 다른 모든 것과 마찬가지로 Boolean 객체에 메시지를 보내는 일반적인 방법입니다.
하나 또는 두 개의 람다 인수를 받습니다.
```Scala
false ifTrue: ["yay" echo]
1 < 2 ifTrue: ["yay" echo]
1 > 2 ifTrue: ["yay" echo] ifFalse: ["oh no" echo]

// `ifTrue:ifFalse:` 메시지는 표현식으로 사용할 수 있습니다
str = 42 % 2 == 0
    ifTrue: ["even"]
    ifFalse: ["odd"]
// str == "even"
```

#### 순환
순환에 대한 특별한 구문은 없습니다.
코드 블록을 매개변수로 받는 키워드 메시지일 뿐입니다.
(인라이닝 덕분에 비용이 없습니다)
```Scala
{1 2 3} forEach: [ it echo ] // 1 2 3
1..10 forEach: [ it echo ]

mut c = 10
[c > 0] whileTrue: [ c <- c dec ]

c <- 3 // c 재설정
[c > 0] whileTrue: [
    c <- c dec
    c echo // 3 2 1
]
```
`whileTrue:`는 다음 타입의 람다 객체에 대한 메시지입니다:
`[ -> Boolean] whileTrue::[ -> Unit]`

#### 매칭
niva는 태그된 유니온을 많이 활용하므로 매칭에 대한 특별한 구문이 있습니다
```Scala
x = "Alice"
// x에 대한 매칭
| x
| "Bob" => "Hi Bob!"
| "Alice" => "Hi Alice!"
|=> "Hi guest"


// 표현식으로도 사용할 수 있습니다
y = | "b"
    | "a" => 1
    | "b" => 2
    |=> 0
y echo // 2
```

#### 태그된 유니온

```Scala
union Color = Red | Blue | Green

// 브랜치는 필드를 가질 수 있습니다
union Shape =
    | Rectangle width: Int height: Int
    | Circle    radius: Double

constructor Double pi = 3.14
Double square = this * this

// this(Shape)에 대한 매칭
Shape getArea -> Double =
    | this
    | Rectangle => width * height, toDouble
    | Circle => Double pi * radius square

// 완전성 검사가 있으므로 새 브랜치를 추가하면
// 모든 케이스가 처리될 때까지 모든 매치가 오류가 됩니다

Shape getArea -> Double = | this
    | Rectangle => width * height, toDouble
// 오류: 모든 가능한 변형이 확인되지 않았습니다 (Circle)
```

#### 컬렉션
```Scala
// 쉼표는 선택 사항입니다
list = {1 2 3}
map = #{'a' 1 'b' 2}
map2 = #{'a' 1, 'b' 2, 'c' 3}
set = #(1 2 3)

// 일반적인 컬렉션 작업
{1 2 3 4 5}
  map: [it inc],
  filter: [it % 2 == 0],
  forEach: [it echo] // 2 4 6

// 맵 반복
map forEach: [key, value ->
  key echo
  value echo
]
```

#### Null 가능성
기본적으로 변수에는 null 값을 할당할 수 없습니다.
이를 허용하려면 타입 끝에 물음표로 표시되는 nullable 타입을 사용합니다.
TypeScript, Kotlin 또는 Swift에서 이 개념에 익숙할 수 있습니다.
```Scala
x::Int? = null
q = x unpackOrPANIC

// null이 아닌 경우 무언가 수행
x unpack: [it echo]

// 동일하지만 백업 값 제공
w = x unpack: [it inc] or: -1

// 그냥 언패킹하거나 백업 값
e = x unpackOrValue: -1
```

#### 오류 처리
```Scala
// 스택 추적으로 프로그램 종료
x = file read orPANIC
x = file read orValue: "no file"
```
오류는 효과처럼 작동합니다. 자세한 내용은 [오류 처리](https://gavr123456789.github.io/niva-site/error-handling.html)를 참조하십시오.

## 기타

#### 로컬 인수 이름
```Scala
Int from: x::Int to: y::Int = this + x + y
```

#### this에 대한 구문 설탕
```Scala
Person foo = [
    .bar
    this bar // 같은 것
]
```

#### 컴파일 타임 리플렉션
호출 사이트에서 모든 인수의 문자열 표현을 가져올 수 있습니다.
```Scala
Int bar::Int baz::String = [
    // 호출 사이드에서 문자열 표현 가져오기
    a = Compiler getName: 0
    b = Compiler getName: 1
    c = Compiler getName: 2
    a echo // 1 + 1
    b echo // variable
    c echo // "str"
]

variable = 42
// 호출 사이드
1 + 1
    bar: variable
    baz: "str"
```

링크:
- [사이트](https://gavr123456789.github.io/niva-site/reference.html)
- [GitHub](https://github.com/gavr123456789/Niva)
- [LSP](https://github.com/gavr123456789/vaLSe)
- [VSC 플러그인](https://github.com/gavr123456789/niva-vscode-bundle)
