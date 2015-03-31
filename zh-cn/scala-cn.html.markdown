---
language: Scala
filename: learnscala-zh.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
translators:
    - ["Peiyong Lin", ""]
    - ["Guodong Qu", "https://github.com/jasonqu"]
lang: zh-cn
---

Scala - 一门可扩展的语言

```scala

/*
  自行设置:

  1) 下载 Scala - http://www.scala-lang.org/downloads
  2) unzip/untar 到你喜欢的地方，并将 bin 目录放在路径中
  3) 在终端输入 scala，开启 Scala 的 REPL，你会看到提示符：

  scala>

  这就是所谓的 REPL (Read-Eval-Print Loop)。
  你现在可以在其中运行任何有效的Scala表达式并打印结果。
  我们将在教程后面介绍Scala文件，不过首先让我们从基础开始。
*/

/////////////////////////////////////////////////
// 1. 基础
/////////////////////////////////////////////////

// 单行注释由双斜杠开始

/*
  多行注释看起来像这样。
*/

// 打印并强制换行
println("Hello world!")
println(10)

// 没有强制换行的打印
print("Hello world")

// 可以通过 var 或者 val 来声明变量。
// val 声明是不可变的，而 var 声明是可修改的。不可变性是好事。
val x = 10 // x 现在是 10
x = 20 // 错误: 对 val 声明的变量重新赋值
var x = 10 
x = 20  // x 现在是 20

/*
  Scala 是一个静态类型语言，然而，在上述声明中，我们并没有指明类型。
  这是源于名为类型推理的功能。在大多数情况下，Scala 编译器能猜出变量
  的类型是什么，所以你不需要每次都键入它。
  我们可以像这样为变量显式声明类型：
*/
val z: Int = 10
val a: Double = 1.0

// 注意从 Int 到 Double 的自动转换，结果是10.0，而不是10
val b: Double = 10

// 布尔值
true
false

// 布尔运算
!true // false
!false // true
true == false // false
10 > 5 // true

// 数学运算像平常一样
1 + 1 // 2
2 - 1 // 1
5 * 3 // 15
6 / 2 // 3
6 / 4 // 1
6.0 / 4 // 1.5


// 在 REPL 计算一个表达式会返回给你结果的类型和值

1 + 7

/* 上行的结果是：

  scala> 1 + 7
  res29: Int = 8

  这意味着计算 1 + 7 的结果是一个 Int 类型的对象，其值为 8

  注意"res29"是一个增序生成的变量名，来存储表达式的结果，
  你的输出可能不一样。
*/

"Scala 字符串由双引号包括"
'a' // 一个Scala Char
// '不存在单引号的字符串' <= 该行将报错

// String 包含 Java 字符串的方法
"hello world".length
"hello world".substring(2, 6)
"hello world".replace("C", "3")

// 它们还具有一些额外的 Scala 方法。见： scala.collection.immutable.StringOps
"hello world".take(5)
"hello world".drop(5)

// String 插值：注意前缀"s"
val n = 45
s"We have $n apples" // => "We have 45 apples"

// 也可以在插值字符串中使用表达式
val a = Array(11, 9, 6)
s"My second daughter is ${a(0) - a(2)} years old." // => "My second daughter is 5 years old."
s"We have double the amount of ${n / 2.0} in apples." // => "We have double the amount of 22.5 in apples."
s"Power of 2: ${math.pow(2, 2)}" // => "Power of 2: 4"

// 格式化插值字符串可以使用前缀"f"
f"Power of 5: ${math.pow(5, 2)}%1.0f" // "Power of 5: 25"
f"Square root of 122: ${math.sqrt(122)}%1.4f" // "Square root of 122: 11.0454"

// 原始字符串，忽略特殊字符。
raw"New line feed: \n. Carriage return: \r." // => "New line feed: \n. Carriage return: \r."

// 一些字符需要被“转义”，例如字符串中的双引号：
"They stood outside the \"Rose and Crown\"" // => "They stood outside the "Rose and Crown""

// 三引号使字符串可以占有多行并包含引号
val html = """<form id="daform">
                <p>Press belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. 函数
/////////////////////////////////////////////////

// 函数像这样定义：
//
//   def functionName(args...): ReturnType = { body... }
//
// 如果你是从更加传统的语言出身，请注意return关键字的省略。
// 在Scala中，函数块的最后一个表达式就是返回值。
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// 如果函数体只有一个表达式，则花括号{ }可以被省略：
def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y

// 调用函数的语法很熟悉：
sumOfSquares(3, 4)  // => 25

// 在大多数情况下（经常地例外情况是递归函数），函数返回值
// 类型可以被忽略，而我们在变量声明中看到的类型推断也将应用于
// 函数的返回值：
def sq(x: Int) = x * x  // 编译器可以推断出返回值类型为Int

// 函数可以有默认参数：
def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2)  // => 3
addWithDefault(1)  // => 6


// 可以像这样写匿名函数：
(x:Int) => x * x

// 与def定义的函数不同，如果上下文足够清晰，甚至可以省略匿名
// 函数的参数类型。注意类型 "Int => Int" 指代一个传入 Int 
// 并返回 Int 的函数。
val sq: Int => Int = x => x * x

// 匿名函数也像往常一样被调用：
sq(10)   // => 100

// 如果匿名函数中，参数只被使用一次，则Scala提供了更简便的语法
// 来定义它们。我们将在数据结构一节中看到，这类匿名函数非常常见。
val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)

addOne(5)  // => 6
weirdSum(2, 4)  // => 16


// Scala中有return关键字，但它只从包围它的最里层定义中返回。
// 警告：在 Scala 中使用 return 容易出现错误，应该避免。
// 它在匿名函数中无效。例如：
def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5) {
      return z  // 这句使得 z 成为foo的返回值！
    }
    else
      z + 2  // 这是 anonFunc 的返回值
  }
  anonFunc(x)  // 这是foo的返回值
}



/////////////////////////////////////////////////
// 3. 控制流
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach( println )

r foreach println
// 附注：Scala 对待点和括号是相当宽松的 —— 注意其规则是不同的。
// 这有助于编写读起来像英文的 DSL 和 API

(5 to 1 by -1) foreach ( println )

// while 循环
var i = 0
while (i < 10) {  println("i " + i); i+=1  }

while (i < 10) {  println("i " + i); i+=1  }   // 是的，又一次。发生了什么？为什么？

i    // 显示 i 的值。注意while是一个传统意义上的循环 ——
     // 它按顺序执行并改变循环变量。while 非常快，比java循环块，
     // 但使用下面将要提到的选择器和包含更容易理解和并行。

// do while 循环
do {
  println("x is still less than 10");
  x += 1
} while (x < 10)

// 在 Scala中，尾递归是一种惯用的执行循环的方式。
// 递归函数需要显式地指明返回值类型，编译器不能推断出它。
// 这里它是 Unit。
def showNumbersInRange(a:Int, b:Int):Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}
showNumbersInRange(1,14)


// 条件语句

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println ("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


/////////////////////////////////////////////////
// 4. 数据结构
/////////////////////////////////////////////////

val a = Array(1, 2, 3, 5, 8, 13)
a(0)
a(3)
a(21)    // 这会抛出一个异常

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")
m("spoon")
m("bottle")       // 这会抛出一个异常

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")

val s = Set(1, 3, 7)
s(0)
s(1)

/* 查看 map 的文档
 * 点击[这里](http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map)
 * 确保你可以读它
 */


// 元组

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// 为什么有这个？
val divideInts = (x:Int, y:Int) => (x / y, x % y)

divideInts(10,3) // 函数 divideInts 返回你结果和余数

// 要读取元组的元素，使用 _._n，其中n是从1开始的元素索引
val d = divideInts(10,3)

d._1

d._2


/////////////////////////////////////////////////
// 5. Object Oriented Programming
/////////////////////////////////////////////////

/*
  Aside: Everything we've done so far in this tutorial has been simple
  expressions (values, functions, etc). These expressions are fine to type into
  the command-line interpreter for quick tests, but they cannot exist by
  themselves in a Scala file. For example, you cannot have just "val x = 5" in
  a Scala file. Instead, the only top-level constructs allowed in Scala are:

  - objects
  - classes
  - case classes
  - traits

  And now we will explain what these are.
*/

// classes are similar to classes in other languages. Constructor arguments are
// declared after the class name, and initialization is done in the class body.
class Dog(br: String) {
  // Constructor code here
  var breed: String = br

  // Define a method called bark, returning a String
  def bark = "Woof, woof!"

  // Values and methods are assumed public. "protected" and "private" keywords
  // are also available.
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  // Abstract methods are simply methods with no body. If we uncomment the next
  // line, class Dog would need to be declared abstract
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.breed) // => "greyhound"
println(mydog.bark) // => "Woof, woof!"


// The "object" keyword creates a type AND a singleton instance of it. It is
// common for Scala classes to have a "companion object", where the per-instance
// behavior is captured in the classes themselves, but behavior related to all
// instance of that class go in objects. The difference is similar to class
// methods vs static methods in other languages. Note that objects and classes
// can have the same name.
object Dog {
  def allKnownBreeds = List("pitbull", "shepherd", "retriever")
  def createDog(breed: String) = new Dog(breed)
}


// Case classes are classes that have extra functionality built in. A common
// question for Scala beginners is when to use classes and when to use case
// classes. The line is quite fuzzy, but in general, classes tend to focus on
// encapsulation, polymorphism, and behavior. The values in these classes tend
// to be private, and only methods are exposed. The primary purpose of case
// classes is to hold immutable data. They often have few methods, and the
// methods rarely have side-effects.
case class Person(name: String, phoneNumber: String)

// Create a new instance. Note cases classes don't need "new"
val george = Person("George", "1234")
val kate = Person("Kate", "4567")

// With case classes, you get a few perks for free, like getters:
george.phoneNumber  // => "1234"

// Per field equality (no need to override .equals)
Person("George", "1234") == Person("Kate", "1236")  // => false

// Easy way to copy
// otherGeorge == Person("george", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// And many others. Case classes also get pattern matching for free, see below.


// Traits coming soon!












// 包括函数在内，每一个事物都是对象。在 REPL 中输入：

7 // 结果 res30: Int = 7 (res30 是一个生成的结果的 var 命名)

// 下一行给你一个接收一个 Int 类型并返回该数的平方的函数
(x:Int) => x * x    

// 你可以分配给函数一个标识符，像这样：
val sq = (x:Int) => x * x

/* 上面的例子说明
   
   sq: Int => Int = <function1>	

   意味着这次我们给予了 sq 这样一个显式的名字给一个接受一个 Int 类型值并返回 一个 Int 类型值的函数

   sq 可以像下面那样被执行：
*/

sq(10)   // 返回给你：res33: Int = 100.

// Scala 允许方法和函数返回或者接受其它的函数或者方法作为参数。

val add10: Int => Int = _ + 10 // 一个接受一个 Int 类型参数并返回一个 Int 类型值的函数
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 被应用到每一个元素

// 匿名函数可以被使用来代替有命名的函数：
List(1, 2, 3) map (x => x + 10)

// 下划线标志，如果匿名函数只有一个参数可以被使用来表示该参数变量
List(1, 2, 3) map (_ + 10)

// 如果你所应用的匿名块和匿名函数都接受一个参数，那么你甚至可以省略下划线
List("Dom", "Bob", "Natalia") foreach println





// 选择器

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// filter 函数接受一个预测(一个函数，形式为 A -> Boolean) 并选择出所有的元素满足这个预测

List(1, 2, 3) filter (_ > 2) // List(3)
List(
  Person(name = "Dom", age = 23), 
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Scala 的 foreach 方法定义在特定的接受一个类型的集合上
// 返回 Unit(一个 void 方法)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println




// For 包含

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* 注意：这些不是 for 循环. 一个 for 循环的语义是 '重复'('repeat')，
  然而，一个 for-包含 定义了一个两个数据结合间的关系 */






// 面向对象特性

// 类名是 Dog
class Dog {
  //bark 方法，返回字符串
  def bark: String = {
    // the body of the method
    "Woof, woof!"
  }
}

// 类可以包含几乎其它的构造，包括其它的类，
// 函数，方法，对象，case 类，特性等等。



// Case 类

case class Person(name:String, phoneNumber:String)

Person("George", "1234") == Person("Kate", "1236")




// 模式匹配

val me = Person("George", "1234")

me match { case Person(name, number) => {
            "We matched someone : " + name + ", phone : " + number }}

me match { case Person(name, number) => "Match : " + name; case _ => "Hm..." }

me match { case Person("George", number) => "Match"; case _ => "Hm..." }

me match { case Person("Kate", number) => "Match"; case _ => "Hm..." }

me match { case Person("Kate", _) => "Girl"; case Person("George", _) => "Boy" }

val kate = Person("Kate", "1234")

kate match { case Person("Kate", _) => "Girl"; case Person("George", _) => "Boy" }



// 正则表达式

val email = "(.*)@(.*)".r  // 在字符串上调用 r 会使它变成一个正则表达式

val email(user, domain) = "henry@zkpr.com"

"mrbean@pyahoo.com" match {
  case email(name, domain) => "I know your name, " + name
}



// 字符串

"Scala 字符串被双引号包围" //
'a' // Scala 字符
'单引号的字符串不存在' // 错误
"字符串拥有通常的 Java 方法定义在其上".length
"字符串也有额外的 Scala 方法".reverse

// 参见:  scala.collection.immutable.StringOps

println("ABCDEF".length)
println("ABCDEF".substring(2, 6))
println("ABCDEF".replace("C", "3"))

val n = 45
println(s"We have $n apples")

val a = Array(11, 9, 6)
println(s"My second daughter is ${a(2-1)} years old")

// 一些字符需要被转义，举例来说，字符串中的双引号：
val a = "They stood outside the \"Rose and Crown\""

// 三个双引号使得字符串可以跨行并且可以包含引号(无需转义)

val html = """<form id="daform">
                <p>Press belo', Joe</p>
             |  <input type="submit">
              </form>"""



// 应用结果和组织

// import
import scala.collection.immutable.List

// Import 所有的子包
import scala.collection.immutable._

// 在一条语句中 Import 多个类
import scala.collection.immutable.{List, Map}

// 使用 '=>' 来重命名一个 import
import scala.collection.immutable.{ List => ImmutableList }

// import 除了一些类的其它所有的类。下面的例子除去了 Map 类和 Set 类：
import scala.collection.immutable.{Map => _, Set => _, _}

// 在 scala 源文件中，你的程序入口点使用一个拥有单一方法 main 的对象来定义：

object Application {
  def main(args: Array[String]): Unit = {
    // stuff goes here.
  }
}

// 文件可以包含多个类和对象。由 scalac 来编译




// 输入和输出

// 一行一行读取文件
import scala.io.Source
for(line <- Source.fromPath("myfile.txt").getLines())
  println(line)

// 使用 Java 的 PrintWriter 来写文件


```

## 更多的资源

[为没耐心的人准备的 Scala](http://horstmann.com/scala/)

[Twitter Scala school](http://twitter.github.io/scala_school/)

[The Scala documentation](http://www.scala-lang.org/documentation/)

[在浏览器尝试 Scala](http://scalatutorials.com/tour/)

加入 [Scala 用户组](https://groups.google.com/forum/#!forum/scala-user)
