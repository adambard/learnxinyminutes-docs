---
language: kotlin
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators: 
    - ["Alan Jeon", "https://github.com/skyisle"]
lang: ko-kr
filename: LearnKotlin-kr.kt
---

Kotlin 은 정적 타입 프로그래밍 언어로 JVM, 안드로이드, 브라우져를 지원하며 Java 와 100% 상호 운용이 가능합니다.
[자세한 내용은 다음을 참고하세요.](https://kotlinlang.org/)

```kotlin
// 한 줄짜리 주석은 // 로 시작합니다.
/*
여러 줄 주석은 이와 같이 표시합니다. 
*/

// "package" 예약어는 자바와 동일하게 사용됩니다.
package com.learnxinyminutes.kotlin

/*
Kotlin 프로그램의 진입점은 main 이라는 함수명으로 지정됩니다. 
이 함수에 명령행 인수가 배열로 전달됩니다.
*/
fun main(args: Array<String>) {
    /*
    값을 선언할때는 "var" 또는 "val"이 사용됩니다.
    "var"와는 다르게 "val"로 선언된 변수에는 값을 재할당 할 수 없습니다.
    */
    val fooVal = 10 // fooVal 에 다른 값을 다시 할당 할 수 없습니다.
    var fooVar = 10
    fooVar = 20 // fooVar 에는 선언 이후에도 다른 값을 할당 할 수 있습니다

    /*
    대부분의 경우, Kotlin 에서는 변수 타입을 판단할 수 있기때문에 명시적으로 지정해 주지 않을 수 있습니다.
    다음과 같이 변수의 타입을 명시적으로 지정할 수 있습니다.
    */
    val foo : Int = 7

    /*
    문자형은 Java와 비슷하게 표시될 수 있습니다.
    이스케이핑에는 백슬래시를 사용합니다.
    */
    val fooString = "My String Is Here!"
    val barString = "Printing on a new line?\nNo Problem!"
    val bazString = "Do you want to add a tab?\tNo Problem!"
    println(fooString)
    println(barString)
    println(bazString)

    /*
    Raw 문자열은 쌍따옴표 3개(""")로 표기합니다.
    Raw 문자열에는 줄바꿈이나 모든 다른 문자들을 사용할 수 있습니다. 
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Hello, world!")
}
"""
    println(fooRawString)

    /*
    문자열은 템플릿 표현식을 포함할 수 있습니다.
    템플릿은 달러 기호($)로 시작합니다. 
    */
    val fooTemplateString = "$fooString has ${fooString.length} characters"
    println(fooTemplateString)

    /*
    변수가 null 값을 가지려면 이를 명시적으로 선언해야 합니다.
    변수 선언시 타입에 ? 표시를 붙여 nullable 을 표시합니다.
    ?. 연산자를 사용해 nullable 변수에 접근합니다.
    ?: 연산자를 이용해서 변수 값이 null 일때 사용할 값을 지정합니다.
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    함수는 "fun" 예약어를 사용해 선언합니다.
    함수명 이후 괄호 안에 인자를 기술합니다.
    함수 인자에 기본 값을 지정할 수도 있습니다.
    함수에 리턴값이 있을 때, 필요한 경우 인자 뒤에 타입을 명시합니다.
    */
    fun hello(name: String = "world"): String {
        return "Hello, $name!"
    }
    println(hello("foo")) // => Hello, foo!
    println(hello(name = "bar")) // => Hello, bar!
    println(hello()) // => Hello, world!

    /*
    함수에 가변 인자를 넘기려면 인자에 "vararg" 예약어를 사용합니다. 
    */
    fun varargExample(vararg names: Int) {
        println("Argument has ${names.size} elements")
    }
    varargExample() // => 인자가 0개 인 경우
    varargExample(1) // => 인자가 1개인 경우
    varargExample(1, 2, 3) // => 인자가 3개인 경우

    /*
    함수가 단일 표현식으로 이루어진 경우에 중괄호를 생략할 수 있습니다.
    이때 함수 구현부는 = 기호 이후에 기술합니다.
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // 리턴 타입이 유추 가능한 경우 이를 명시하지 않아도 됩니다. 
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // 함수는 함수를 인자를 받을 수 있고 함수를 리턴할 수 있습니다. 
    fun not(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // 함수는 :: 연산자를 사용해서 다른 함수에 인자로 넘길 수 있습니다. 
    val notOdd = not(::odd)
    val notEven = not(::even)
    // 람다식을 인자로 사용할 수 있습니다. 
    val notZero = not {n -> n == 0}
    /*
    하나의 인자를 가지는 람다식의 선언부와 -> 연산자는 생략될 수 있습니다.
    이때 그 인자명은 it로 지정됩니다.
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // "class" 예약어는 클래스를 선언할 때 사용됩니다. 
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int): Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int): Int {
            return x * y
        }
    }
    /*
    새로운 객체를 생성하기 위해서는 생성자를 바로 호출합니다.
    Kotlin 에서는 new 예약어가 없다는 걸 기억하세요.
    */
    val fooExampleClass = ExampleClass(7)
    // 맴버 함수는 dot 표기로 호출할 수 있습니다. 
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    함수 선언에 "infix" 예약어를 사용하면 이 함수를 중위 표현식(infix notation)으로 호출할 수 있습니다 
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    데이터 클래스로 데이터만을 가지고 있는 클래스를 손쉽게 선언할 수 있습니다.
    "hashCode"/"equals" 와 "toString" 는 자동으로 생성됩니다.
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // 데이터 클래스는 copy 함수를 가지고 있습니다. 
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // 객체를 여러 변수로 분리할 수 있습니다. 
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4
    
    // "for" 루프에서 변수 분리 하기 
    for ((a, b, c) in listOf(fooData)) {
        println("$a $b $c") // => 1 100 4
    }
    
    val mapData = mapOf("a" to 1, "b" to 2)
    // Map.Entry 또한 키와 값으로 분리가 가능합니다. 
    for ((key, value) in mapData) {
        println("$key -> $value")
    }

    // "with" 함수는 JavaScript 의 "with" 구문과 비슷하게 사용됩니다. 
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableDate = MutableDataClassExample(7, 4, 9)
    with (fooMutableDate) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableDate) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    "listOf" 함수로 리스트를 만들 수 있습니다.
    리스트는 변경 불가능(immutable)하게 만들어져 항목의 추가 삭제가 불가능합니다.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // 각 항목은 인덱스로 접근이 가능합니다. 
    println(fooList[1]) // => b

    // 변경가능한(mutable) 리스트는 "mutableListOf" 함수로 만들 수 있습니다. 
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // 집합(set)은 "setOf" 함수로 만들 수 있습니다.
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // 맵은 "mapOf" 함수로 만들 수 있습니다.
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // 맵은 키를 통해 그 값에 접근할 수 있습니다. Map values can be accessed by their key.
    println(fooMap["a"]) // => 8

    /*
    시퀀스는 지연 평가되는 컬랙션을 말합니다. Sequences represent lazily-evaluated collections.
    "generateSequence" 를 사용해 시퀀스를 만들 수 있습니다. We can create a sequence using the "generateSequence" function.
    */
    val fooSequence = generateSequence(1, { it + 1 })
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // 다음은 시퀀스를 사용해서 피보나치 수열을 생성하는 예입니다.
    fun fibonacciSequence(): Sequence<Long> {
        var a = 0L
        var b = 1L

        fun next(): Long {
            val result = a + b
            a = b
            b = result
            return a
        }

        return generateSequence(::next)
    }
    val y = fibonacciSequence().take(10).toList()
    println(y) // => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

    // Kotlin 은 컬랙션에서 사용할 수 있는 고차(higher-order)함수를 제공합니다.
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "even" else "odd"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // "for" 루프는 이터레이터를 제공하는 어떤 것과도 함께 사용할 수 있습니다.
    for (c in "hello") {
        println(c)
    }

    // "while" 루프는 다른 언어들과 동일하게 사용됩니다.
    var ctr = 0
    while (ctr < 5) {
        println(ctr)
        ctr++
    }
    do {
        println(ctr)
        ctr++
    } while (ctr < 10)

    /*
    "if"는 값을 리턴하는 표현으로 사용될 수 있습니다.
    그래서 Kotlin 에서는 삼항 ?: 연산자가 필요하지 않습니다.
    */
    val num = 5
    val message = if (num % 2 == 0) "even" else "odd"
    println("$num is $message") // => 5 is odd

    // "when"은 "if-else if" 를 대체할때 사용할 수 있습니다.
    val i = 10
    when {
        i < 7 -> println("first block")
        fooString.startsWith("hello") -> println("second block")
        else -> println("else block")
    }

    // "when"은 인수와 함께 사용될 수 있습니다.
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // "when"은 값을 리턴하는 함수처럼 사용될 수 있습니다.
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)

    /*
    객체가 어떤 타입인지를 확인하기 위해 "is" 연산자를 사용할 수 있습니다.
    타입 체크를 통과하면 객체의 명시적인 형변환 없이도 그 타입 값으로 사용될 수 있습니다.
    이를 스마트 변환(Smartcast)이라 부릅니다.
    */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x is automatically cast to Boolean
            return x
        } else if (x is Int) {
            // x is automatically cast to Int
            return x > 0
        } else if (x is String) {
            // x is automatically cast to String
            return x.isNotEmpty()
        } else {
            return false
        }
    }
    println(smartCastExample("Hello, world!")) // => true
    println(smartCastExample("")) // => false
    println(smartCastExample(5)) // => true
    println(smartCastExample(0)) // => false
    println(smartCastExample(true)) // => true

    // 스마트 변환은 when 블럭과도 함께 사용됩니다.
    fun smartCastWhenExample(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    확장(Extensions)을 이용해 클래스에 새로운 기능을 추가할 수 있습니다.
    C#에서의 확장 매서드와 유사합니다.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hello, world!".remove('l')) // => Heo, word!

    println(EnumExample.A) // => A
    println(ObjectExample.hello()) // => hello
}

// Enum 클래스는 자바의 enum 타입과 유사합니다.
enum class EnumExample {
    A, B, C
}

/*
"object" 예약어는 싱클톤 객체를 생성할 때 사용됩니다. 
객체를 새로 생성할 수는 없지만 이름을 가지고 접근해 사용할 수 있습니다.
이는 스칼라의 싱글톤 객체와 유사합니다.
*/
object ObjectExample {
    fun hello(): String {
        return "hello"
    }
}

fun useObject() {
    ObjectExample.hello()
    val someRef: Any = ObjectExample // 객체의 이름을 그대로 사용합니다.
}

```

### 더 알아보기

* [Kotlin tutorials (EN)](https://kotlinlang.org/docs/tutorials/)
* [Try Kotlin in your browser (EN)](http://try.kotlinlang.org/)
* [A list of Kotlin resources (EN)](http://kotlin.link/)
