---
language: kotlin
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["Jimin Lu", "https://github.com/lujimin"]
filename: LearnKotlin-cn.kt
---

Kotlin是一门适用于JVM、Android和浏览器的静态类型编程语言。它100%兼容Java。
[了解更多。](https://kotlinlang.org/)

```java
// 单行注释从 // 开始
/*
多行注释看起来像这样。
*/

// "package" 关键字的工作方式与Java相同。
package com.learnxinyminutes.kotlin

/*
Kotlin程序的入口点是一个"main"函数
该函数传递一个包含任何命令行参数的数组。
*/
fun main(args: Array<String>) {
    /*
    使用"var"或"val"来声明一个值。
    "val"声明的值不能被重新赋值，而"vars"声明的值可以。
    */
    val fooVal = 10 // 以后我们不能再次给fooVal赋值
    var fooVar = 10
    fooVar = 20 // fooVar可以被再次赋值

    /*
    在大多数情况下，Kotlin可以确定变量的类型是什么，
    所以我们不必要每次都去明确指定它。
    我们可以像这样明确地声明一个变量的类型：
    */
    val foo : Int = 7

    /*
    可以采取和Java类似的方法来表示一个字符串。
    用反斜杠来转义字符。
    */
    val fooString = "My String Is Here!";
    val barString = "Printing on a new line?\nNo Problem!";
    val bazString = "Do you want to add a tab?\tNo Problem!";
    println(fooString);
    println(barString);
    println(bazString);

    /*
    原始字符串用三重引号(""")来定义。
    原始字符串可以包含新的行以及其他任何字符。
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Hello, world!")
}
"""
    println(fooRawString)

    /*
    字符串可以包含模板表达式。
    模板表达式从一个美元符号($)开始。
    */
    val fooTemplateString = "$fooString has ${fooString.length} characters"
    println(fooTemplateString)

    /*
    当某个变量的值可以为 null 的时候，我们必须被明确指定它是可为空的。
    在变量声明处的类型后面加上?来标识它是可为空的。
    我们可以用?.操作符来访问可为空的变量。
    我们可以用?:操作符来指定一个替代值在变量为空的时候使用。
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    使用"fun"关键字来声明一个函数。
    函数的参数在函数名后面的括号内指定。
    函数的参数可以设定一个默认值。
    如果需要的话，函数的返回值类型可以在参数后面指定。
    */
    fun hello(name: String = "world") : String {
        return "Hello, $name!"
    }
    println(hello("foo")) // => Hello, foo!
    println(hello(name = "bar")) // => Hello, bar!
    println(hello()) // => Hello, world!

    /*
    用"vararg"关键字来修饰一个函数的参数
    来允许可变参数传递给该函数
    */
    fun varargExample(vararg names: Int) {
        println("Argument has ${names.size} elements")
    }
    varargExample() // => Argument has 0 elements
    varargExample(1) // => Argument has 1 elements
    varargExample(1, 2, 3) // => Argument has 3 elements

    /*
    当函数只包含一个单独的表达式时，大括号可以被省略。
    函数体可以被指定在一个=符号后面。
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // 如果返回值类型可以被推断，那么我们不需要指定它。
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // 函数可以用函数作为参数并且可以返回函数。
    fun not(f: (Int) -> Boolean) : (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // 命名函数可以被指定为参数使用::操作符。
    val notOdd = not(::odd)
    val notEven = not(::even)
    // 匿名函数可以被指定为参数。
    val notZero = not {n -> n == 0}
    /*
    如果一个匿名函数只有一个参数
    那么它的声明可以被省略（连同->）。
    这个参数的名字是"it"。
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // "class"关键字用来声明类。
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int) : Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int) : Int {
            return x * y
        }
    }
    /*
    我们调用构造方法来创建一个新的实例。
    注意，Kotlin没有"new"关键字。
    */
    val fooExampleClass = ExampleClass(7)
    // 可以使用一个点号来调用成员函数。
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    如果使用"infix"关键字来标记一个函数
    那么可以使用中缀表示法来调用该函数。
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    数据类是创建只包含数据的类的一个简洁的方法。
    "hashCode"、"equals"和"toString"方法将被自动生成。
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // 数据类有一个"copy"函数
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // 对象可以被解构成为多个变量
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4

    // "with"函数类似于JavaScript中的"with"用法。
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableDate = MutableDataClassExample(7, 4, 9)
    with (fooMutableDate) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableDate) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    我们可以使用"listOf"函数来创建一个list。
    这个list是不可变的 - 元素不可以被添加或删除。
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // 可以通过索引来访问list中的元素。
    println(fooList[1]) // => b

    // 可以使用"mutableListOf"函数来创建一个可变的list。
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // 我们可以使用"setOf"函数来创建一个set。
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // 我们可以使用"mapOf"函数来创建一个map。
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // 可以通过键来访问map中的值。
    println(fooMap["a"]) // => 8

    /*
    序列表示惰性求值集合。
    我们可以使用"generateSequence"函数来创建一个序列。
    */
    val fooSequence = generateSequence(1, {it + 1})
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // 一个用序列来生成斐波那契数列的例子。
    fun fibonacciSequence() : Sequence<Long> {
        var a = 0L
        var b = 1L

        fun next() : Long {
            val result = a + b
            a = b
            b = result
            return a
        }

        return generateSequence(::next)
    }
    val y = fibonacciSequence().take(10).toList()
    println(y) // => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

    // Kotlin为集合提供高阶函数。
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "even" else "odd"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // 任何提供迭代器的都可以使用"for"循环。
    for (c in "hello") {
        println(c)
    }

    // "while"循环的用法和其他语言一样。
    var ctr = 0
    while (ctr < 5) {
        println(ctr)
        ctr++
    }
    do {
        println(ctr)
        ctr++
    } while (ctr < 10)

    // "when"可以用来替代"if-else if"链。
    val i = 10
    when {
        i < 7 -> println("first block")
        fooString.startsWith("hello") -> println("second block")
        else -> println("else block")
    }

    // "when"可以带参数。
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // "when"可以作为一个函数，提供返回值。
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)

    /*
    我们可以通过使用"is"操作符来检查一个对象是否是特定类型的。
    如果对象通过了类型检查那么它可以作为该类型使用而不需要强制转换它。
    */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x自动转换为Boolean
            return x
        } else if (x is Int) {
            // x自动转换为Int
            return x > 0
        } else if (x is String) {
            // x自动转换为String
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

    /*
    扩展是用来添加新的功能到一个类的。
    它类似于C#的扩展方法。
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hello, world!".remove('l')) // => Heo, word!

    println(EnumExample.A) // => A
    println(ObjectExample.hello()) // => hello
}

// 枚举类和Java的枚举类型类似。
enum class EnumExample {
    A, B, C
}

/*
"object"关键字用来创建单例对象。
我们不能把它赋给一个变量，但我们可以通过它的名字引用它。
这类似于Scala的单例对象。
*/
object ObjectExample {
    fun hello() : String {
        return "hello"
    }
}

```

### 进一步阅读

* [Kotlin教程](https://kotlinlang.org/docs/tutorials/)
* [在您的浏览器中使用Kotlin](http://try.kotlinlang.org/)
* [Kotlin资源列表](http://kotlin.link/)
