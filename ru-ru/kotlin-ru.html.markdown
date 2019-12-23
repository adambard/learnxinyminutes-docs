---
language: kotlin
filename: LearnKotlin-ru.kt
lang: ru-ru
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["Vadim Toptunov", "https://github.com/VadimToptunov"]
---

Kotlin - статически типизированный язык для JVM, Android и браузера. Язык полностью совместим c Java. 
[Более детальная информация здесь.](https://kotlinlang.org/)

```kotlin
// Однострочные комментарии начинаются с //
/*
А вот так выглядят многострочные комментарии.
*/

// Ключевое слово "package" действует и используется // абсолютно также, как и в Java.
package com.learnxinyminutes.kotlin

/*
Точкой входа в программу на языке Kotlin является функция "main".
Приведенная ниже функция передает массив, содержащий любые аргументы из командной строки.
*/
fun main(args: Array<String>) {
    /*
    Объявление значений производится с помощью или "var", или "val".
    Значения объявленные с помощью "val" не могут быть изменены или перезаписаны, в то время как объявленные с помощью "var" - могут.
    */
    val fooVal = 10 // мы не можем потом изменить значение fooVal на какое-либо иное
    var fooVar = 10
    fooVar = 20 // значение fooVar затем может быть изменено.

    /*
    В большинстве случаев Kotlin самостоятельно может определить тип переменной, поэтому нам не нужно явно указывать его каждый раз.
    Мы можем явно объявить тип переменной следующим образом:
    */
    val foo: Int = 7

    /*
    Строки могут быть представлены тем же образом, что и в Java.
    Для экранирования используется обратный слэш.
    */
    val fooString = "My String Is Here!"
    val barString = "Printing on a new line?\nNo Problem!"
    val bazString = "Do you want to add a tab?\tNo Problem!"
    println(fooString)
    println(barString)
    println(bazString)

    /*
    Необработанная строка разделяется тройной кавычкой (""").
    Необработанные строки могут содержать символы новой строки и любые другие символы.
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Hello, world!")
}
"""
    println(fooRawString)

    /*
    Строки могут содержать в себе шаблонные выражения.
    Шаблонные выражения начинаются со знака доллара ($).
    */
    val fooTemplateString = "$fooString has ${fooString.length} characters"
    println(fooTemplateString)

    /*
    Переменная, которая содержит null должна быть явно обозначена как nullable.
    Переменная может быть обозначена как nullable с помощью добавления знака вопроса(?) к ее типу.
    Мы можем получить доступ к nullable переменной используя оператор ?. .
    Для того, чтобы указать иное значение, если переменная является null, мы используем оператор ?: .
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    Функции могут быть объявлены с помощью ключевого слова "fun".
    Аргументы функции указываются в скобках после имени функции.
    Аргументы функции также могу иметь и значение по умолчанию.
    Если требуется, то тип возвращаемого функцией значения, может быть указан после аргументов.
    */
    fun hello(name: String = "world"): String {
        return "Hello, $name!"
    }
    println(hello("foo")) // => Hello, foo!
    println(hello(name = "bar")) // => Hello, bar!
    println(hello()) // => Hello, world!

    /*
    Параметр функции может быть отмечен с помощью ключевого слова "vararg", для того чтобы позволить аргументам попасть в функцию.
    */
    fun varargExample(vararg names: Int) {
        println("Argument has ${names.size} elements")
    }
    varargExample() // => Argument has 0 elements
    varargExample(1) // => Argument has 1 elements
    varargExample(1, 2, 3) // => Argument has 3 elements

    /*
    Если функция состоит из одиночного выражения, фигурные скобки могут быть опущены. Тело функции указывается после знака = .
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // Если возвращаемый тип может быть выведен, то нам не нужно его дополнительно указывать.
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // Функции могут брать другие функции в качестве аргументов, а также могут возвращать функции. 
    fun not(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // Именованные функции могут быть определены в качестве аргументов с помощью оператора :: .
    val notOdd = not(::odd)
    val notEven = not(::even)
    // Lambda-выражения могут быть определены в качестве аргументов.
    val notZero = not {n -> n == 0}
    /*
    Если lambda-выражение имеет только один параметр, то ее определение может быть опущено (вместе с ->).
    Имя этого единственного параметра будет "it".
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // Ключевое слово "class" используется для 
    // объявления классов.
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int): Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int): Int {
            return x * y
        }
    }
    /*
    Чтобы создать новый экземпляр класса, нужно вызвать конструктор.
    Обратите внимание, что в Kotlin нет ключевого слова "new".
    */
    val fooExampleClass = ExampleClass(7)
    // Функции-члены могут быть вызваны с использованием точечной нотации.
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    В случае, если функция была помечена ключевым словом "infix", она может быть вызвана с помощью инфиксной нотации. 
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    Data-классы - это компактный способ создать классы, которые лишь хранят данные.
    Методы "hashCode"/"equals" и "toString" генерируютсяч автоматически. 
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // Data-классы обладают функцией "copy".
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // Объекты могут быть деструктурированы на множество переменных.
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4
    
    // Деструктурирование в цикле "for"
    for ((a, b, c) in listOf(fooData)) {
        println("$a $b $c") // => 1 100 4
    }
    
    val mapData = mapOf("a" to 1, "b" to 2)
    // Map.Entry также может быть дествуктурирован
    for ((key, value) in mapData) {
        println("$key -> $value")
    }

    // Функция "with" аналогична оператору "with" в JavaScript.
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableData = MutableDataClassExample(7, 4, 9)
    with (fooMutableData) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableData) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    Можно создать список с помощью функции "ListOf".
    Этот список будет неизменяемым, т.е. элементы не могут быть удалены или добавлены в него.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // Элементы списка доступны по их индексу в нем. 
    println(fooList[1]) // => b

    // Изменяемый список может быть создан спомощью функции "mutableListOf".
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // Мы можем создать набор, используя функцию "setOf". 
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // Мы можем создать отображение (map), используя функцию "mapOf".
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // Получить доступ к значениям отображения (map) можно с помощью их ключа. 
    println(fooMap["a"]) // => 8

    /*
    Последовательности представляют собой коллекции с ленивой оценкой.
    Мы можем создать последовательность, используя функцию "generateSequence".
    */
    val fooSequence = generateSequence(1, { it + 1 })
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Пример использования последовательности для генерации чисел Фибоначчи:
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

    // Kotlin предоставляет функции высшего порядка для работы с коллекциями.
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "even" else "odd"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // Цикл "for" может использоваться со всем, что предоставляет  итератор.
    for (c in "hello") {
        println(c)
    }

    // Циклы "while" работают также, как и в других языках.
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
    "if" может быть использован в качестве выражения, которое возвращает значение.
    По этой причине в Kotlin тернарный оператор ?: не нужен.
    */
    val num = 5
    val message = if (num % 2 == 0) "even" else "odd"
    println("$num is $message") // => 5 is odd

    // "when" может быть использован как альтернатива цепочке "if-else if".
    val i = 10
    when {
        i < 7 -> println("first block")
        fooString.startsWith("hello") -> println("second block")
        else -> println("else block")
    }

    // "when" может быть использован с аргументами.
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // "when" также может быть использовано как функция, возвращающая значение.
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)

    /*
    Мы можем проверить, что объект принадлежит к определенному типу, используя оператор "is".
    Если объект проходит проверку типа, то он может использоваться как этот тип без явной его  передачи.
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

    // Smartcast также работает с блоком "when"
    fun smartCastWhenExample(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    Расширения - это способ добавить новый функционал к классу. 
    Это то же самое, что методы расширений в C#.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hello, world!".remove('l')) // => Heo, word!

    println(EnumExample.A) // => A
    println(ObjectExample.hello()) // => hello
}

// Enum-классы схожи с типами enum в Java.
enum class EnumExample {
    A, B, C
}

/*
Ключевое слово "object" может использоваться для создания одноэлементных объектов.
Мы не можем его инстанцировать, но можем вызывать его уникальный экземпляр по имени.
Это похоже на одиночные объекты Scala.
*/
object ObjectExample {
    fun hello(): String {
        return "hello"
    }
}

fun useObject() {
    ObjectExample.hello()
    val someRef: Any = ObjectExample // we use objects name just as is
}

```

### Дальнейшее чтение:

* [Учебные материалы по Kotlin](https://kotlinlang.org/docs/tutorials/)
* [Попробуй Kotlin в своем браузере](http://try.kotlinlang.org/)
* [Список ресурсов по языку Kotlin](http://kotlin.link/)
