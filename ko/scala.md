---
name: Scala
filename: learnscala.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Ha-Duong Nguyen", "http://reference-error.org"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---
Scala - 확장 가능한 언어

```scala
/////////////////////////////////////////////////
// 0. 기본
/////////////////////////////////////////////////
/*
  Scala 설정:

  1) Scala 다운로드 - http://www.scala-lang.org/downloads
  2) 원하는 위치에 압축을 풀고 bin 하위 디렉토리를 `PATH` 환경 변수에 추가
*/

/*
  REPL 사용해보기

  Scala에는 REPL(Read-Eval-Print Loop)이라는 도구가 있으며, 이는 다른 많은 언어의
  명령줄 인터프리터와 유사합니다. 모든 Scala 표현식을 입력하면
  결과가 평가되어 출력됩니다.

  REPL은 코드를 테스트하고 확인하는 데 매우 유용한 도구입니다. 이 튜토리얼을
  읽으면서 사용하여 개념을 직접 빠르게 탐색하십시오.
*/

// `scala`를 실행하여 Scala REPL을 시작합니다. 다음과 같은 프롬프트를 볼 수 있습니다:
$ scala
scala>

// 기본적으로 입력하는 각 표현식은 새로운 번호가 매겨진 값으로 저장됩니다
scala> 2 + 2
res0: Int = 4

// 기본값을 재사용할 수 있습니다. 결과에 표시되는 값 유형에 유의하십시오..
scala> res0 + 2
res1: Int = 6

// Scala는 강력한 타입 언어입니다. REPL을 사용하여 표현식을 평가하지 않고
// 타입을 확인할 수 있습니다.
scala> :type (true, 2.0)
(Boolean, Double)

// REPL 세션을 저장할 수 있습니다
scala> :save /sites/repl-test.scala

// 파일을 REPL로 로드할 수 있습니다
scala> :load /sites/repl-test.scala
Loading /sites/repl-test.scala...
res2: Int = 4
res3: Int = 6

// 최근 기록을 검색할 수 있습니다
scala> :h?
1 2 + 2
2 res0 + 2
3 :save /sites/repl-test.scala
4 :load /sites/repl-test.scala
5 :h?

// 이제 어떻게 사용하는지 알았으니, 스칼라를 조금 배워봅시다...

/////////////////////////////////////////////////
// 1. 기본
/////////////////////////////////////////////////

// 한 줄 주석은 두 개의 슬래시로 시작합니다

/*
  위에서 이미 볼 수 있듯이 여러 줄 주석은 이렇습니다.
*/

// 출력하고, 다음 출력에서 새 줄을 강제합니다
println("Hello world!")
println(10)
// Hello world!
// 10

// 출력하고, 다음 출력에서 새 줄을 강제하지 않습니다
print("Hello world")
print(10)
// Hello world10

// 값 선언은 var 또는 val을 사용하여 수행됩니다.
// val 선언은 불변인 반면, var는 가변입니다. 불변성은
// 좋은 것입니다.
val x = 10 // x는 이제 10입니다
x = 20     // 오류: val에 재할당
var y = 10
y = 20     // y는 이제 20입니다

/*
  Scala는 정적 타입 언어이지만, 위 선언에서
  타입을 지정하지 않았다는 점에 유의하십시오. 이는 타입 추론이라는
  언어 기능 때문입니다. 대부분의 경우 Scala 컴파일러는 변수의
  타입을 추측할 수 있으므로 매번 입력할 필요가 없습니다.
  다음과 같이 변수의 타입을 명시적으로 선언할 수 있습니다:
*/
val z: Int = 10
val a: Double = 1.0

// Int에서 Double로의 자동 변환에 유의하십시오. 결과는 10이 아닌 10.0입니다.
val b: Double = 10

// 불리언 값
true
false

// 불리언 연산
!true         // false
!false        // true
true == false // false
10 > 5        // true

// 수학은 평소와 같습니다
1 + 1   // 2
2 - 1   // 1
5 * 3   // 15
6 / 2   // 3
6 / 4   // 1
6.0 / 4 // 1.5
6 / 4.0 // 1.5


// REPL에서 표현식을 평가하면 결과의 타입과 값이 제공됩니다

1 + 7

/* 위 줄의 결과는 다음과 같습니다:

  scala> 1 + 7
  res29: Int = 8

  이는 1 + 7을 평가한 결과가 값이 8인 Int 타입의 객체임을 의미합니다.

  "res29"는 입력한 표현식의 결과를 저장하기 위해 순차적으로 생성된
  변수 이름이며, 출력은 다를 수 있습니다.
*/

"Scala 문자열은 큰따옴표로 묶입니다"
'a' // Scala Char
// '작은따옴표 문자열은 존재하지 않습니다' <= 이로 인해 오류가 발생합니다

// 문자열에는 일반적인 Java 메서드가 정의되어 있습니다
"hello world".length
"hello world".substring(2, 6)
"hello world".replace("C", "3")

// 추가 Scala 메서드도 있습니다. scala.collection.immutable.StringOps도 참조하십시오.
"hello world".take(5)
"hello world".drop(5)

// 문자열 보간: 접두사 "s"에 유의하십시오
val n = 45
s"We have $n apples" // => "We have 45 apples"

// 보간된 문자열 내의 표현식도 가능합니다
val a = Array(11, 9, 6)
s"My second daughter is ${a(0) - a(2)} years old."    // => "My second daughter is 5 years old."
s"We have double the amount of ${n / 2.0} in apples." // => "We have double the amount of 22.5 in apples."
s"Power of 2: ${math.pow(2, 2)}"                      // => "Power of 2: 4"

// 접두사 "f"를 사용하여 보간된 문자열로 서식 지정
f"Power of 5: ${math.pow(5, 2)}%1.0f"         // "Power of 5: 25"
f"Square root of 122: ${math.sqrt(122)}%1.4f" // "Square root of 122: 11.0454"

// 원시 문자열, 특수 문자 무시.
raw"New line feed: \n. Carriage return: \r." // => "New line feed: \n. Carriage return: \r."

// 일부 문자는 "이스케이프"해야 합니다. 예를 들어 문자열 내의 큰따옴표:
"They stood outside the \"Rose and Crown\"" // => "They stood outside the "Rose and Crown""

// 세 개의 큰따옴표는 문자열이 여러 줄에 걸쳐 있고 따옴표를 포함할 수 있도록 합니다
val html = """<form id="daform">
                <p>Press belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. 함수
/////////////////////////////////////////////////

// 함수는 다음과 같이 정의됩니다:
//
//   def functionName(args...): ReturnType = { body... }
//
// 더 전통적인 언어에서 온 경우 return 키워드의 생략에 유의하십시오.
// Scala에서는 함수 블록의 마지막 표현식이 반환 값입니다.
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// 함수 본문이 단일 표현식인 경우 { }를 생략할 수 있습니다:
def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y

// 함수 호출 구문은 익숙합니다:
sumOfSquares(3, 4)  // => 25

// 매개변수 이름을 사용하여 다른 순서로 지정할 수 있습니다
def subtract(x: Int, y: Int): Int = x - y

subtract(10, 3)     // => 7
subtract(y=10, x=3) // => -7

// 대부분의 경우(재귀 함수가 가장 주목할 만한 예외) 함수
// 반환 타입을 생략할 수 있으며, 변수에서 보았던 것과 동일한 타입 추론이
// 함수 반환 값에서도 작동합니다:
def sq(x: Int) = x * x  // 컴파일러는 반환 타입이 Int라고 추측할 수 있습니다

// 함수에는 기본 매개변수가 있을 수 있습니다:
def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2) // => 3
addWithDefault(1)    // => 6


// 익명 함수는 다음과 같습니다:
(x: Int) => x * x

// def와 달리, 컨텍스트가 명확하면 익명 함수의 입력 타입도 생략할 수 있습니다.
// "Int => Int" 타입은 Int를 받아 Int를 반환하는 함수를 의미합니다.
val sq: Int => Int = x => x * x

// 익명 함수는 평소와 같이 호출할 수 있습니다:
sq(10)   // => 100

// 익명 함수의 각 인수가 한 번만 사용되는 경우,
// Scala는 더 짧은 방법으로 정의할 수 있습니다. 이러한
// 익명 함수는 데이터 구조 섹션에서 명백해지겠지만 매우 일반적입니다.
val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)

addOne(5)      // => 6
weirdSum(2, 4) // => 16


// return 키워드는 Scala에 존재하지만, 가장 안쪽의
// def에서만 반환합니다.
// 경고: Scala에서 return을 사용하는 것은 오류가 발생하기 쉬우므로 피해야 합니다.
// 익명 함수에는 영향을 미치지 않습니다. 예를 들어 여기서 foo(7)이 17을 반환할 것으로 예상할 수 있지만 7을 반환합니다:
def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5)
      return z // 이 줄은 z를 foo의 반환 값으로 만듭니다!
    else
      z + 2    // 이 줄은 anonFunc의 반환 값입니다
  }
  anonFunc(x) + 10  // 이 줄은 foo의 반환 값입니다
}

foo(7) // => 7

/////////////////////////////////////////////////
// 3. 제어 흐름
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach(println)

r foreach println
// 참고: Scala는 점과 괄호에 대해 상당히 관대합니다 -
// 규칙을 별도로 연구하십시오. 이는 영어처럼 읽히는 DSL 및 API를 작성하는 데 도움이 됩니다.

// 왜 여기서 `println`에 매개변수가 필요하지 않을까요?
// 아래 함수형 프로그래밍 섹션의 일급 함수를 기대하십시오!
(5 to 1 by -1) foreach (println)

// while 루프
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }   // 네, 다시. 무슨 일이 있었나요? 왜요?

i    // i의 값을 표시합니다. while은 고전적인 의미의 루프입니다 -
     // 루프 변수를 변경하면서 순차적으로 실행됩니다. while은 매우
     // 빠르지만, 위의 조합기 및 내포를 사용하는 것이 이해하기 쉽고
     // 병렬화하기 쉽습니다.

// do-while 루프
i = 0
do {
  println("i is still less than 10")
  i += 1
} while (i < 10)

// 재귀는 Scala에서 (대부분의 다른 함수형 언어에서와 같이)
// 작업을 반복하는 관용적인 방법입니다.
// 재귀 함수는 명시적인 반환 타입이 필요하며, 컴파일러는 이를 추론할 수 없습니다.
// 여기서 Unit은 Java의 `void` 반환 타입과 유사합니다.
def showNumbersInRange(a: Int, b: Int): Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}
showNumbersInRange(1, 14)


// 조건문

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


/////////////////////////////////////////////////
// 4. 데이터 구조
/////////////////////////////////////////////////

val a = Array(1, 2, 3, 5, 8, 13)
a(0)     // Int = 1
a(3)     // Int = 5
a(21)    // 예외 발생

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")         // java.lang.String = tenedor
m("spoon")        // java.lang.String = cuchara
m("bottle")       // 예외 발생

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")   // java.lang.String = no lo se

val s = Set(1, 3, 7)
s(0)      // Boolean = false
s(1)      // Boolean = true

/* 여기서 맵 설명서를 찾아보십시오 -
 * https://www.scala-lang.org/api/current/scala/collection/immutable/Map.html
 * 그리고 읽을 수 있는지 확인하십시오.
 */


// 튜플

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// 왜 이것이 있을까요?
val divideInts = (x: Int, y: Int) => (x / y, x % y)

// divideInts 함수는 결과와 나머지를 제공합니다
divideInts(10, 3)    // (Int, Int) = (3,1)

// 튜플의 요소에 액세스하려면 _._n을 사용하십시오. 여기서 n은 요소의 1-기반 인덱스입니다.
val d = divideInts(10, 3)    // (Int, Int) = (3,1)

d._1    // Int = 3
d._2    // Int = 1

// 또는 튜플에 다중 변수 할당을 할 수 있으며, 이는 많은 경우에 더 편리하고
// 읽기 쉽습니다.
val (div, mod) = divideInts(10, 3)

div     // Int = 3
mod     // Int = 1


/////////////////////////////////////////////////
// 5. 객체 지향 프로그래밍
/////////////////////////////////////////////////

/*
  참고: 이 튜토리얼에서 지금까지 수행한 모든 작업은 간단한
  표현식(값, 함수 등)이었습니다. 이러한 표현식은 빠른 테스트를 위해
  명령줄 인터프리터에 입력하기에 좋지만, Scala 파일에
  단독으로 존재할 수는 없습니다. 예를 들어 Scala 파일에 "val x = 5"만 있을 수는 없습니다.
  대신 Scala에서 허용되는 유일한 최상위 구문은 다음과 같습니다:

  - objects
  - classes
  - case classes
  - traits

  이제 이것들이 무엇인지 설명하겠습니다.
*/

// 클래스는 다른 언어의 클래스와 유사합니다. 생성자 인수는
// 클래스 이름 뒤에 선언되고, 초기화는 클래스 본문에서 수행됩니다.
class Dog(br: String) {
  // 생성자 코드 여기
  var breed: String = br

  // bark라는 메서드를 정의하고 String을 반환합니다
  def bark = "Woof, woof!"

  // 값과 메서드는 public으로 간주됩니다. "protected" 및 "private" 키워드도
  // 사용할 수 있습니다.
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  // 추상 메서드는 단순히 본문이 없는 메서드입니다. 아래 def 줄의 주석을
  // 해제하면 Dog 클래스를 다음과 같이 abstract로 선언해야 합니다:
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.breed) // => "greyhound"
println(mydog.bark)  // => "Woof, woof!"


// "object" 키워드는 타입과 해당 타입의 싱글턴 인스턴스를 생성합니다.
// Scala 클래스가 "컴패니언 객체"를 갖는 것이 일반적이며, 여기서 인스턴스별
// 동작은 클래스 자체에 캡처되지만, 해당 클래스의 모든
// 인스턴스와 관련된 동작은 객체로 이동합니다. 차이점은 다른 언어의
// 클래스 메서드와 정적 메서드와 유사합니다. 객체와 클래스는
// 동일한 이름을 가질 수 있습니다.
object Dog {
  def allKnownBreeds = List("pitbull", "shepherd", "retriever")
  def createDog(breed: String) = new Dog(breed)
}


// 케이스 클래스는 추가 기능이 내장된 클래스입니다. Scala 초보자를 위한
// 일반적인 질문은 언제 클래스를 사용하고 언제 케이스 클래스를 사용해야 하는지입니다.
// 경계가 모호하지만, 일반적으로 클래스는 캡슐화, 다형성 및
// 동작에 중점을 둡니다. 이러한 클래스의 값은 private인 경향이 있으며,
// 메서드만 노출됩니다. 케이스 클래스의 주요 목적은
// 불변 데이터를 보유하는 것입니다. 메서드가 거의 없으며,
// 메서드에는 부작용이 거의 없습니다.
case class Person(name: String, phoneNumber: String)

// 새 인스턴스를 만듭니다. 케이스 클래스는 "new"가 필요하지 않습니다.
val george = Person("George", "1234")
val kate = Person("Kate", "4567")

// 케이스 클래스를 사용하면 다음과 같은 몇 가지 특전을 무료로 얻을 수 있습니다.
george.phoneNumber  // => "1234"

// 필드별 동등성 (.equals를 재정의할 필요 없음)
Person("George", "1234") == Person("Kate", "1236")  // => false

// 쉬운 복사 방법
// otherGeorge == Person("George", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// 그리고 다른 많은 것들. 케이스 클래스는 패턴 매칭도 무료로 얻습니다. 아래 참조.

// 트레이트
// Java 인터페이스와 유사하게, 트레이트는 객체 타입과 메서드
// 시그니처를 정의합니다. Scala는 이러한 메서드의 부분 구현을 허용합니다.
// 생성자 매개변수는 허용되지 않습니다. 트레이트는 매개변수 없이 다른
// 트레이트나 클래스에서 상속할 수 있습니다.

trait Dog {
	def breed: String
	def color: String
	def bark: Boolean = true
	def bite: Boolean
}
class SaintBernard extends Dog {
	val breed = "Saint Bernard"
	val color = "brown"
	def bite = false
}

scala> val b = new SaintBernard
res0: SaintBernard = SaintBernard@3e57cd70
scala> b.breed
res1: String = Saint Bernard
scala> b.bark
res2: Boolean = true
scala> b.bite
res3: Boolean = false

// 트레이트는 믹스인으로도 사용할 수 있습니다. 클래스는 첫 번째 트레이트를 "extends"하지만,
// "with" 키워드는 추가 트레이트를 추가할 수 있습니다.

trait Bark {
	def bark: String = "Woof"
}
trait Dog {
	def breed: String
	def color: String
}
class SaintBernard extends Dog with Bark {
	val breed = "Saint Bernard"
	val color = "brown"
}

scala> val b = new SaintBernard
b: SaintBernard = SaintBernard@7b69c6ba
scala> b.bark
res0: String = Woof


/////////////////////////////////////////////////
// 6. 패턴 매칭
/////////////////////////////////////////////////

// 패턴 매칭은 Scala에서 강력하고 일반적으로 사용되는 기능입니다. 다음은
// 케이스 클래스를 패턴 매칭하는 방법입니다. 참고: 다른 언어와 달리 Scala 케이스는
// break가 필요 없으며, fall-through가 발생하지 않습니다.

def matchPerson(person: Person): String = person match {
  // 그런 다음 패턴을 지정합니다:
  case Person("George", number) => "We found George! His number is " + number
  case Person("Kate", number)   => "We found Kate! Her number is " + number
  case Person(name, number)     => "We matched someone : " + name + ", phone : " + number
}

// 정규식도 내장되어 있습니다.
// 문자열에서 `r` 메서드를 사용하여 정규식을 만듭니다:
val email = "(.*)@(.*)".r

// 패턴 매칭은 C 계열 언어의 switch 문과 비슷해 보일 수 있지만,
// 훨씬 더 강력합니다. Scala에서는 훨씬 더 많은 것을 매칭할 수 있습니다:
def matchEverything(obj: Any): String = obj match {
  // 값을 매칭할 수 있습니다:
  case "Hello world" => "Got the string Hello world"

  // 타입별로 매칭할 수 있습니다:
  case x: Double => "Got a Double: " + x

  // 조건을 지정할 수 있습니다:
  case x: Int if x > 10000 => "Got a pretty big number!"

  // 이전과 같이 케이스 클래스를 매칭할 수 있습니다:
  case Person(name, number) => s"Got contact info for $name!"

  // 정규식을 매칭할 수 있습니다:
  case email(name, domain) => s"Got email address $name@$domain"

  // 튜플을 매칭할 수 있습니다:
  case (a: Int, b: Double, c: String) => s"Got a tuple: $a, $b, $c"

  // 데이터 구조를 매칭할 수 있습니다:
  case List(1, b, c) => s"Got a list with three elements and starts with 1: 1, $b, $c"

  // 패턴을 중첩할 수 있습니다:
  case List(List((1, 2, "YAY"))) => "Got a list of list of tuple"

  // 이전 모든 것이 일치하지 않으면 모든 경우를 매칭합니다 (기본값)
  case _ => "Got unknown object"
}

// 사실, "unapply" 메서드가 있는 모든 객체를 패턴 매칭할 수 있습니다. 이
// 기능은 너무 강력해서 Scala는 전체 함수를 패턴으로 정의할 수 있습니다:
val patternFunc: Person => String = {
  case Person("George", number) => s"George's number: $number"
  case Person(name, number) => s"Random person's number: $number"
}


/////////////////////////////////////////////////
// 7. 함수형 프로그래밍
/////////////////////////////////////////////////

// Scala는 메서드와 함수가 다른 함수나 메서드를 반환하거나
// 매개변수로 받을 수 있도록 허용합니다.

val add10: Int => Int = _ + 10 // Int를 받아 Int를 반환하는 함수
List(1, 2, 3) map add10 // List(11, 12, 13) - add10이 각 요소에 적용됩니다

// 명명된 함수 대신 익명 함수를 사용할 수 있습니다:
List(1, 2, 3) map (x => x + 10)

// 그리고 익명 함수에 인수가 하나만 있는 경우 밑줄 기호를 사용할 수 있습니다.
// 변수로 바인딩됩니다.
List(1, 2, 3) map (_ + 10)

// 익명 블록과 적용하는 함수가 모두 인수를 하나만 받는 경우
// 밑줄을 생략할 수도 있습니다.
List("Dom", "Bob", "Natalia") foreach println


// 조합기
// 위에서 `s` 사용:
// val s = Set(1, 3, 7)

s.map(sq)

val sSquared = s.map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// filter 함수는 술어(A -> Boolean 함수)를 받아
// 술어를 만족하는 모든 요소를 선택합니다
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name: String, age: Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Scala의 특정 컬렉션(예: List)에는 `foreach` 메서드가 있으며,
// Unit을 반환하는 타입을 인수로 받습니다. 즉, void 메서드입니다.
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// For 내포

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* 참고: 이것들은 for 루프가 아니었습니다. for 루프의 의미는 '반복'인 반면,
   for-내포는 두 데이터 집합 간의 관계를 정의합니다. */


/////////////////////////////////////////////////
// 8. 암시
/////////////////////////////////////////////////

/* 경고 경고: 암시는 Scala의 강력한 기능 집합이므로
 * 남용하기 쉽습니다. Scala 초보자는 어떻게 작동하는지뿐만 아니라
 * 그 주변의 모범 사례를 이해할 때까지 사용하려는 유혹을 참아야 합니다.
 * 이 튜토리얼에 이 섹션을 포함하는 이유는 Scala 라이브러리에서 너무 흔해서
 * 암시가 있는 라이브러리를 사용하지 않고는 의미 있는 작업을 수행하는 것이
 * 불가능하기 때문입니다. 이것은 여러분이 암시를 이해하고 작업하기 위한 것이며,
 * 직접 선언하기 위한 것이 아닙니다.
 */

// 모든 값(val, 함수, 객체 등)은 "implicit" 키워드를 사용하여
// 암시적으로 선언될 수 있습니다. 이 예제에서는 5절의 Dog 클래스를 사용합니다.
implicit val myImplicitInt = 100
implicit def myImplicitFunction(breed: String) = new Dog("Golden " + breed)

// 그 자체로 implicit 키워드는 값의 동작을 변경하지 않으므로
// 위 값은 평소와 같이 사용할 수 있습니다.
myImplicitInt + 2                   // => 102
myImplicitFunction("Pitbull").breed // => "Golden Pitbull"

// 차이점은 이러한 값이 이제 다른 코드 조각이
// "암시적 값"을 "필요"할 때 사용될 수 있다는 것입니다. 한 가지 그러한 상황은
// 암시적 함수 인수입니다:
def sendGreetings(toWhom: String)(implicit howMany: Int) =
  s"Hello $toWhom, $howMany blessings to you and yours!"

// "howMany"에 대한 값을 제공하면 함수는 평소와 같이 동작합니다
sendGreetings("John")(1000)  // => "Hello John, 1000 blessings to you and yours!"

// 그러나 암시적 매개변수를 생략하면 동일한 타입의 암시적 값이
// 사용됩니다. 이 경우 "myImplicitInt"입니다:
sendGreetings("Jane")  // => "Hello Jane, 100 blessings to you and yours!"

// 암시적 함수 매개변수를 사용하면 다른
// 함수형 언어의 타입 클래스를 시뮬레이션할 수 있습니다. 너무 자주 사용되어
// 자체 약어가 있습니다. 다음 두 줄은 동일한 의미입니다:
// def foo[T](implicit c: C[T]) = ...
// def foo[T : C] = ...


// 컴파일러가 암시를 찾는 또 다른 상황은
//   obj.method(...)
// 이지만 "obj"에 "method"라는 메서드가 없는 경우입니다. 이 경우
// A => B 타입의 암시적 변환이 있고, 여기서 A는 obj의 타입이고 B에
// "method"라는 메서드가 있으면 해당 변환이 적용됩니다. 따라서
// 위의 myImplicitFunction이 범위에 있으면 다음과 같이 말할 수 있습니다:
"Retriever".breed // => "Golden Retriever"
"Sheperd".bark    // => "Woof, woof!"

// 여기서 String은 먼저 위 함수를 사용하여 Dog로 변환된 다음
// 적절한 메서드가 호출됩니다. 이것은 매우 강력한 기능이지만,
// 다시 말하지만, 가볍게 사용해서는 안 됩니다. 사실, 위의 암시적 함수를
// 정의했을 때 컴파일러는 정말로 무엇을 하는지 모르는 한
// 이렇게 해서는 안 된다는 경고를 했을 것입니다.


/////////////////////////////////////////////////
// 9. 기타
/////////////////////////////////////////////////

// 항목 가져오기
import scala.collection.immutable.List

// 모든 "하위 패키지" 가져오기
import scala.collection.immutable._

// 한 문장으로 여러 클래스 가져오기
import scala.collection.immutable.{List, Map}

// '=>'를 사용하여 가져오기 이름 바꾸기
import scala.collection.immutable.{List => ImmutableList}

// 일부를 제외하고 모든 클래스 가져오기. 다음은 Map과 Set을 제외합니다:
import scala.collection.immutable.{Map => _, Set => _, _}

// Java 클래스도 가져올 수 있습니다. Scala 구문을 사용할 수 있습니다
import java.swing.{JFrame, JWindow}

// 프로그램의 진입점은 Scala 파일에서 단일 메서드 main이 있는 객체를 사용하여
// 정의됩니다:
object Application {
  def main(args: Array[String]): Unit = {
    // 여기에 내용이 들어갑니다.
  }
}

// 파일에는 여러 클래스와 객체가 포함될 수 있습니다. scalac으로 컴파일


// 입출력

// 파일을 한 줄씩 읽으려면
import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)

// 파일을 쓰려면 Java의 PrintWriter를 사용하십시오
val writer = new PrintWriter("myfile.txt")
writer.write("Writing line for line" + util.Properties.lineSeparator)
writer.write("Another line here" + util.Properties.lineSeparator)
writer.close()
```

## 추가 자료

* [성급한 사람들을 위한 스칼라](http://horstmann.com/scala/)
* [트위터 스칼라 스쿨](http://twitter.github.io/scala_school/)
* [스칼라 문서](http://docs.scala-lang.org/)
* [브라우저에서 스칼라 사용해보기](http://scalatutorials.com/tour/)
* [스칼라 사용자 그룹 가입](https://groups.google.com/forum/#!forum/scala-user)
