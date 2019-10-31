---
language: kotlin
filename: LearnKotlin-uk.kt
lang: uk-ua
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["AstiaSun", "https://github.com/AstiaSun"]
---

Kotlin - це мова програмування зі статичною типізацією для JVM, Android та браузера. 
Вона має 100% сумісність із Java.

[Детальніше](https://kotlinlang.org/)

```kotlin
// Однорядкові коментарі починаються з //
/*
Такий вигляд мають багаторядкові коментарі
*/

// Ключове слово package працює так само, як і в Java.
package com.learnxinyminutes.kotlin

/*
Точкою входу для програм на Kotlin є функція під назвою main.
Вона приймає масив із аргументів, що були передані через командний рядок.
Починаючи з Kotlin 1.3, функція main може бути оголошена без параметрів взагалі.
*/
fun main(args: Array<String>) {
    /*
    Оголошення змінних відбувається за допомогою ключових слів var або val.
    Відмінність між ними полягає в тому, що значення змінних, оголошених через 
    val, не можна змінювати. Водночас, змінній "var" можна переприсвоїти нове 
    значення в подальшому.
    */
    val fooVal = 10 // більше ми не можемо змінити значення fooVal на інше
    var fooVar = 10
    fooVar = 20 // fooVar може змінювати значення

    /*
    В більшості випадків Kotlin може визначати, якого типу змінна, тому не 
    потрібно щоразу точно вказувати її тип.
    Тип змінної вказується наступним чином:
    */
    val foo: Int = 7

    /*
    Рядки мають аналогічне з Java представлення. Спеціальні символи 
    позначаються за допомогою зворотнього слеша.
    */
    val fooString = "My String Is Here!"
    val barString = "Printing on a new line?\nNo Problem!"
    val bazString = "Do you want to add a tab?\tNo Problem!"
    println(fooString)
    println(barString)
    println(bazString)

    /*
    Необроблений рядок розмежовується за допомогою потрійних лапок (""").
    Необроблені рядки можуть містити переніс рядка (не спеціальний символ \n) та 
    будь-які інші символи.
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Hello, world!")
}
"""
    println(fooRawString)

    /*
    Рядки можуть містити шаблонні вирази.
    Шаблонний вираз починається із символа доллара "$".
    */
    val fooTemplateString = "$fooString has ${fooString.length} characters"
    println(fooTemplateString) // => My String Is Here! has 18 characters

    /*
    Щоб змінна могла мати значення null, потрібно це додатково вказати. 
    Для цього після оголошеного типу змінної додається спеціальний символ "?".
    Отримати значення такої змінної можна використавши оператор "?.".
    Оператор "?:" застосовується, щоб оголосити альтернативне значення змінної 
    у випадку, якщо вона буде рівна null.
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    Функції оголошуються з використанням ключового слова fun.
    Аргументи функції перелічуються у круглих дужках після назви функції.
    Аргументи можуть мати значення за замовчуванням. Тип значення, що повертатиметься
    функцією, вказується після оголошення аргументів за необхідністю.
    */
    fun hello(name: String = "world"): String {
        return "Hello, $name!"
    }
    println(hello("foo")) // => Hello, foo!
    println(hello(name = "bar")) // => Hello, bar!
    println(hello()) // => Hello, world!

    /*
    Аргументи функції можуть бути помічені ключовим словом vararg. Це дозволяє 
    приймати довільну кількість аргументів функції зазначеного типу.
    */
    fun varargExample(vararg names: Int) {
        println("Argument has ${names.size} elements")
    }
    varargExample() // => Argument has 0 elements
    varargExample(1) // => Argument has 1 elements
    varargExample(1, 2, 3) // => Argument has 3 elements

    /*
    Коли функція складається з одного виразу, фігурні дужки не є обов'язковими. 
    Тіло функції вказується після оператора "=".
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // Якщо тип значення, що повертається функцією, може бути однозначно визначено, 
    // його непотрібно вказувати.
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // Функції можуть приймати інші функції як аргументи, а також повертати інші функції.
    fun not(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // Іменовані функції можуть бути вказані як аргументи за допомогою оператора "::".
    val notOdd = not(::odd)
    val notEven = not(::even)
    // Лямбда-вирази також можуть бути аргументами функції.
    val notZero = not {n -> n == 0}
    /*
    Якщо лямбда-вираз приймає лише один параметр, його оголошення може бути пропущене
    (разом із ->). Всередині виразу до цього параметра можна звернутись через 
    змінну "it".
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // Ключове слово class використовується для оголошення класів.
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int): Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int): Int {
            return x * y
        }
    }
    /*
    Щоб створити новий об'єкт, потрібно викликати конструктор класу.
    Зазначте, що в Kotlin немає ключового слова new.
    */
    val fooExampleClass = ExampleClass(7)
    // Методи класу викликаються через крапку.
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    Якщо функція була позначена ключовим словом infix, тоді її можна викликати через 
    інфіксну нотацію.
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    Класи даних - це лаконічний спосіб створювати класи, що містимуть тільки дані.
    Методи "hashCode"/"equals" та "toString" автоматично генеруються.
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // Класи даних також мають функцію "copy".
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // Об'єкти можуть бути деструктурувані кількома способами. 
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4

    // деструктурування у циклі for
    for ((a, b, c) in listOf(fooData)) {
        println("$a $b $c") // => 1 100 4
    }

    val mapData = mapOf("a" to 1, "b" to 2)
    // Map.Entry також деструктурувуються
    for ((key, value) in mapData) {
        println("$key -> $value")
    }

    // Функція із "with" працює майже так само як це ж твердження у JavaScript.
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableData = MutableDataClassExample(7, 4, 9)
    with (fooMutableData) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableData) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    Список можна створити використовуючи функцію listOf.
    Список буде незмінним, тобто елементи не можна буде додавати або видаляти.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // доступ до елементів здійснюється через їхні порядковий номер.
    println(fooList[1]) // => b

    // Змінні списки можна створити використовуючи функцію mutableListOf.
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // Функція setOf  створює об'єкт типу множина.
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // mapOf створює асоціативний масив.
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // Доступ до значень в асоціативних масивах здійснюється через їхні ключі.
    println(fooMap["a"]) // => 8

    /*
    Послідовності представлені як колекції лінивих обчислень. Функція generateSequence
    створює послідовність.
    */
    val fooSequence = generateSequence(1, { it + 1 })
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Приклад використання послідовностей, генерація чисел Фібоначчі:
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

    // Kotlin має функції вищого порядку для роботи з колекціями.
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "even" else "odd"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // Цикл for може використовуватись з будь-чим, що має ітератор.
    for (c in "hello") {
        println(c)
    }

    // Принцип роботи циклів "while" не відрізняється від інших мов програмування.
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
    if може бути використаний як вираз, що повертає значення. Тому тернарний 
    оператор ?: не потрібний в Kotlin.
    */
    val num = 5
    val message = if (num % 2 == 0) "even" else "odd"
    println("$num is $message") // => 5 is odd

    // "when" використовується як альтернатива ланцюгам "if-else if".
    val i = 10
    when {
        i < 7 -> println("first block")
        fooString.startsWith("hello") -> println("second block")
        else -> println("else block")
    }

    // "when" може приймати аргумент.
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // "when" також може використовуватись як функція, що повертає значення.
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)

    /*
    Тип об'єкта можна перевірити використавши оператор is. Якщо перевірка проходить 
    успішно, тоді можна використовувати об'єкт як данний тип не приводячи до нього 
    додатково.
    */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x тепер має тип Boolean
            return x
        } else if (x is Int) {
            // x тепер має тип Int
            return x > 0
        } else if (x is String) {
            // x тепер має тип String
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

    // Smartcast (розумне приведення) також працює з блоком when
    fun smartCastWhenExample(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    Розширення - це ще один спосіб розширити функціонал класу.
    Подібні методи розширення реалізовані у С#.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hello, world!".remove('l')) // => Heo, word!
}

// Класи перелічення також подібні до тих типів, що і в Java.
enum class EnumExample {
    A, B, C // Константи перелічення розділені комами.
}
fun printEnum() = println(EnumExample.A) // => A

// Оскільки кожне перелічення - це об'єкт класу enum, воно може бути 
// проініціалізоване наступним чином:
enum class EnumExample(val value: Int) {
    A(value = 1),
    B(value = 2),
    C(value = 3)
}
fun printProperty() = println(EnumExample.A.value) // => 1

// Кожне перелічення має властивості, які дозволяють отримати його ім'я 
// та порядок (позицію) в класі enum:
fun printName() = println(EnumExample.A.name) // => A
fun printPosition() = println(EnumExample.A.ordinal) // => 0

/*
Ключове слово object можна використати для створення об'єкту сінглтону. Об'єкт не 
можна інстанціювати, проте на його унікальний екземпляр можна посилатись за іменем.
Подібна можливість є в сінглтон об'єктах у Scala. 
*/
object ObjectExample {
    fun hello(): String {
        return "hello"
    }

    override fun toString(): String {
        return "Hello, it's me, ${ObjectExample::class.simpleName}"
    }
}


fun useSingletonObject() {
    println(ObjectExample.hello()) // => hello
    // В Kotlin, "Any" - це корінь ієрархії класів, так само, як і "Object" у Java.
    val someRef: Any = ObjectExample
    println(someRef) // => Hello, it's me, ObjectExample
}


/* 
Оператор перевірки на те, що об'єкт не рівний null, (!!) перетворює будь-яке значення в ненульовий тип і кидає виняток, якщо значення рівне null.
*/
var b: String? = "abc"
val l = b!!.length

// Далі - приклади перевизначення методів класу Any в класі-насліднику
data class Counter(var value: Int) {
    // перевизначити Counter += Int
    operator fun plusAssign(increment: Int) {
        this.value += increment
    }

    // перевизначити Counter++ та ++Counter
    operator fun inc() = Counter(value + 1)

    // перевизначити Counter + Counter
    operator fun plus(other: Counter) = Counter(this.value + other.value)

    // перевизначити Counter * Counter
    operator fun times(other: Counter) = Counter(this.value * other.value)

    // перевизначити Counter * Int
    operator fun times(value: Int) = Counter(this.value * value)

    // перевизначити Counter in Counter
    operator fun contains(other: Counter) = other.value == this.value

    // перевизначити Counter[Int] = Int
    operator fun set(index: Int, value: Int) {
        this.value = index + value
    }

    // перевизначити виклик екземпляру Counter
    operator fun invoke() = println("The value of the counter is $value")

}
// Можна також перевизначити оператори через методи розширення.
// перевизначити -Counter
operator fun Counter.unaryMinus() = Counter(-this.value)

fun operatorOverloadingDemo() {
    var counter1 = Counter(0)
    var counter2 = Counter(5)
    counter1 += 7
    println(counter1) // => Counter(value=7)
    println(counter1 + counter2) // => Counter(value=12)
    println(counter1 * counter2) // => Counter(value=35)
    println(counter2 * 2) // => Counter(value=10)
    println(counter1 in Counter(5)) // => false
    println(counter1 in Counter(7)) // => true
    counter1[26] = 10
    println(counter1) // => Counter(value=36)
    counter1() // => The value of the counter is 36
    println(-counter2) // => Counter(value=-5)
}
```

### Подальше вивчення

* [Уроки Kotlin](https://kotlinlang.org/docs/tutorials/)
* [Спробувати попрацювати з Kotlin в браузері](https://play.kotlinlang.org/)
* [Список корисних посилань](http://kotlin.link/)
