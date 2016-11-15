---
language: Scala
filename: learnscala-zh.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
translators:
    - ["Peiyong Lin", ""]
    - ["Jinchang Ye", "http://github.com/alwayswithme"]
    - ["Guodong Qu", "https://github.com/jasonqu"]
lang: zh-cn
---

Scala - 一门可拓展的语言

```scala

/*
  自行设置:

  1) 下载 Scala - http://www.scala-lang.org/downloads
  2) unzip/untar 到您喜欢的地方，并把 bin 子目录添加到 path 环境变量
  3) 在终端输入 scala，启动 Scala 的 REPL，您会看到提示符：

  scala>

  这就是所谓的 REPL (读取-求值-输出循环，英语： Read-Eval-Print Loop)，
  您可以在其中输入合法的表达式，结果会被打印。
  在教程中我们会进一步解释 Scala 文件是怎样的，但现在先了解一点基础。
*/


/////////////////////////////////////////////////
// 1. 基础
/////////////////////////////////////////////////

//  单行注释开始于两个斜杠

/*
  多行注释，如您之前所见，看起来像这样
*/

// 打印并强制换行
println("Hello world!")
println(10)

// 没有强制换行的打印
print("Hello world")

// 通过 var 或者 val 来声明变量
// val 声明是不可变的，var 声明是可修改的。不可变性是好事。
val x = 10 // x 现在是 10
x = 20 // 错误: 对 val 声明的变量重新赋值
var y = 10 
y = 20  // y 现在是 20

/*
  Scala 是静态语言，但注意上面的声明方式，我们没有指定类型。
  这是因为类型推导的语言特性。大多数情况， Scala 编译器可以推测变量的类型，
  所以您不需要每次都输入。可以像这样明确声明变量类型：
*/
val z: Int = 10
val a: Double = 1.0

// 注意从 Int 到 Double 的自动转型，结果是 10.0, 不是 10
val b: Double = 10

// 布尔值
true
false

// 布尔操作
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


// 在 REPL 计算一个表达式会返回给您结果的类型和值

1 + 7

/* 上行的结果是：

  scala> 1 + 7
  res29: Int = 8

  这意味着计算 1 + 7 的结果是一个 Int 类型的对象，其值为 8

  注意 "res29" 是一个连续生成的变量名，用以存储您输入的表达式结果，
  您看到的输出可能不一样。
*/

"Scala strings are surrounded by double quotes"
'a' // Scala 的字符
// '不存在单引号字符串' <= 这会导致错误

// String 有常见的 Java 字符串方法
"hello world".length
"hello world".substring(2, 6)
"hello world".replace("C", "3")

// 也有一些额外的 Scala 方法，另请参见：scala.collection.immutable.StringOps
"hello world".take(5)
"hello world".drop(5)

// 字符串改写：留意前缀 "s"
val n = 45
s"We have $n apples" // => "We have 45 apples"

// 在要改写的字符串中使用表达式也是可以的
val a = Array(11, 9, 6)
s"My second daughter is ${a(0) - a(2)} years old." // => "My second daughter is 5 years old."
s"We have double the amount of ${n / 2.0} in apples." // => "We have double the amount of 22.5 in apples."
s"Power of 2: ${math.pow(2, 2)}" // => "Power of 2: 4"

// 添加 "f" 前缀对要改写的字符串进行格式化
f"Power of 5: ${math.pow(5, 2)}%1.0f" // "Power of 5: 25"
f"Square root of 122: ${math.sqrt(122)}%1.4f" // "Square root of 122: 11.0454"

// 未处理的字符串，忽略特殊字符。
raw"New line feed: \n. Carriage return: \r." // => "New line feed: \n. Carriage return: \r."

// 一些字符需要转义，比如字符串中的双引号
"They stood outside the \"Rose and Crown\"" // => "They stood outside the "Rose and Crown""

// 三个双引号可以使字符串跨越多行，并包含引号
val html = """<form id="daform">
                <p>Press belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. 函数
/////////////////////////////////////////////////

// 函数可以这样定义:
//
//   def functionName(args...): ReturnType = { body... }
//
// 如果您以前学习过传统的编程语言，注意 return 关键字的省略。
// 在 Scala 中， 函数代码块最后一条表达式就是返回值。
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// 如果函数体是单行表达式，{ } 可以省略：
def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y

// 函数调用的语法是熟知的：
sumOfSquares(3, 4)  // => 25

// 在多数情况下 (递归函数是需要注意的例外), 函数返回值可以省略，
// 变量所用的类型推导一样会应用到函数返回值中：
def sq(x: Int) = x * x  // 编译器会推断得知返回值是 Int

// 函数可以有默认参数
def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2)  // => 3
addWithDefault(1)  // => 6


// 匿名函数是这样的：
(x:Int) => x * x

// 和 def 不同，如果语义清晰，匿名函数的参数类型也可以省略。
// 类型 "Int => Int" 意味着这个函数接收一个 Int 并返回一个 Int。
val sq: Int => Int = x => x * x

// 匿名函数的调用也是类似的：
sq(10)   // => 100

// 如果您的匿名函数中每个参数仅使用一次，
// Scala 提供一个更简洁的方式来定义他们。这样的匿名函数极为常见，
// 在数据结构部分会明显可见。
val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)

addOne(5)  // => 6
weirdSum(2, 4)  // => 16


// return 关键字是存在的，但它只从最里面包裹了 return 的 def 函数中返回。
// 警告： 在 Scala 中使用 return 容易出错，应该避免使用。
// 在匿名函数中没有效果，例如：
def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5)
      return z  // 这一行令 z 成为 foo 函数的返回值！
    else
      z + 2  // 这一行是 anonFunc 函数的返回值
  }
  anonFunc(x)  // 这一行是 foo 函数的返回值
}

/*
 * 译者注：此处是指匿名函数中的 return z 成为最后执行的语句，
 *    在 anonFunc(x) 下面的表达式（假设存在）不再执行。如果 anonFunc
 *    是用 def 定义的函数， return z 仅返回到 anonFunc(x) ，
 *    在 anonFunc(x) 下面的表达式（假设存在）会继续执行。
 */


/////////////////////////////////////////////////
// 3. 控制语句
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach( println )

r foreach println
// 附注： Scala 对点和括号的要求想当宽松，注意其规则是不同的。
// 这有助于写出读起来像英语的 DSL(领域特定语言) 和 API(应用编程接口)。

(5 to 1 by -1) foreach ( println )

// while 循环
var i = 0
while (i < 10) {  println("i " + i); i+=1  }

while (i < 10) {  println("i " + i); i+=1  }   // 没错，再执行一次，发生了什么？为什么？

i    // 显示 i 的值。注意 while 是经典的循环方式，它连续执行并改变循环中的变量。
     // while 执行很快，比 Java 的循环快，但像上面所看到的那样用组合子和推导式
     // 更易于理解和并行化。

// do while 循环
do {
  println("x is still less than 10");
  x += 1
} while (x < 10)

// Scala 中尾递归是一种符合语言习惯的递归方式。
// 递归函数需要清晰的返回类型，编译器不能推断得知。
// 这是一个 Unit。
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
a(21)    // 抛出异常

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")
m("spoon")
m("bottle")       // 抛出异常

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")

val s = Set(1, 3, 7)
s(0)
s(1)

/* 这里查看 map 的文档 -
 * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map
 * 并确保你会阅读
 */


// 元组

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// 为什么有这个？
val divideInts = (x:Int, y:Int) => (x / y, x % y)

divideInts(10,3) // 函数 divideInts 同时返回结果和余数

// 要读取元组的元素，使用 _._n，n是从1开始的元素索引
val d = divideInts(10,3)

d._1

d._2


/////////////////////////////////////////////////
// 5. 面向对象编程
/////////////////////////////////////////////////

/*
  旁白: 教程中到现在为止我们所做的一切只是简单的表达式（值，函数等）。
  这些表达式可以输入到命令行解释器中作为快速测试，但它们不能独立存在于 Scala 
  文件。举个例子，您不能在 Scala 文件上简单的写上 "val x = 5"。相反 Scala 文件
  允许的顶级结构是：

  - objects
  - classes
  - case classes
  - traits

  现在来解释这些是什么。
*/

// 类和其他语言的类相似，构造器参数在类名后声明，初始化在类结构体中完成。
class Dog(br: String) {
  // 构造器代码在此
  var breed: String = br

  // 定义名为 bark 的方法，返回字符串
  def bark = "Woof, woof!"

  // 值和方法作用域假定为 public。"protected" 和 "private" 关键字也是可用的。
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  // 抽象方法是没有方法体的方法。如果取消下面那行注释，Dog 类必须被声明为 abstract
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.breed) // => "greyhound"
println(mydog.bark) // => "Woof, woof!"


// "object" 关键字创造一种类型和该类型的单例。
// Scala 的 class 常常也含有一个 “伴生对象”，class 中包含每个实例的行为，所有实例
// 共用的行为则放入 object 中。两者的区别和其他语言中类方法和静态方法类似。
// 请注意 object 和 class 可以同名。
object Dog {
  def allKnownBreeds = List("pitbull", "shepherd", "retriever")
  def createDog(breed: String) = new Dog(breed)
}


// Case 类是有额外内建功能的类。Scala 初学者常遇到的问题之一便是何时用类
// 和何时用 case 类。界线比较模糊，但通常类倾向于封装，多态和行为。类中的值
// 的作用域一般为 private ， 只有方法是暴露的。case 类的主要目的是放置不可变
// 数据。它们通常只有几个方法，且方法几乎没有副作用。
case class Person(name: String, phoneNumber: String)

// 创造新实例，注意 case 类不需要使用 "new" 关键字
val george = Person("George", "1234")
val kate = Person("Kate", "4567")

// 使用 case 类，您可以轻松得到一些功能，像 getters:
george.phoneNumber  // => "1234"

// 每个字段的相等性比较（无需覆盖 .equals）
Person("George", "1234") == Person("Kate", "1236")  // => false

// 简单的拷贝方式
// otherGeorge == Person("george", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// 还有很多。case 类同时可以用于模式匹配，接下来会看到。


// 敬请期待 Traits ！


/////////////////////////////////////////////////
// 6. 模式匹配
/////////////////////////////////////////////////

// 模式匹配是一个强大和常用的 Scala 特性。这是用模式匹配一个 case 类的例子。
// 附注：不像其他语言， Scala 的 case 不需要 break， 其他语言中 switch 语句的
// fall-through 现象不会发生。

def matchPerson(person: Person): String = person match {
  // Then you specify the patterns:
  case Person("George", number) => "We found George! His number is " + number
  case Person("Kate", number) => "We found Kate! Her number is " + number
  case Person(name, number) => "We matched someone : " + name + ", phone : " + number
}

val email = "(.*)@(.*)".r  // 定义下一个例子会用到的正则

// 模式匹配看起来和 C语言家族的 switch 语句相似，但更为强大。
// Scala 中您可以匹配很多东西：
def matchEverything(obj: Any): String = obj match {
  // 匹配值：
  case "Hello world" => "Got the string Hello world"

  // 匹配类型：
  case x: Double => "Got a Double: " + x

  // 匹配时指定条件
  case x: Int if x > 10000 => "Got a pretty big number!"

  // 像之前一样匹配 case 类：
  case Person(name, number) => s"Got contact info for $name!"

  // 匹配正则表达式：
  case email(name, domain) => s"Got email address $name@$domain"

  // 匹配元组：
  case (a: Int, b: Double, c: String) => s"Got a tuple: $a, $b, $c"

  // 匹配数据结构：
  case List(1, b, c) => s"Got a list with three elements and starts with 1: 1, $b, $c"

  // 模式可以嵌套
  case List(List((1, 2,"YAY"))) => "Got a list of list of tuple"
}

// 事实上，你可以对任何有 "unapply" 方法的对象进行模式匹配。
// 这个特性如此强大以致于 Scala 允许定义一个函数作为模式匹配：
val patternFunc: Person => String = {
  case Person("George", number) => s"George's number: $number"
  case Person(name, number) => s"Random person's number: $number"
}


/////////////////////////////////////////////////
// 7. 函数式编程
/////////////////////////////////////////////////

// Scala 允许方法和函数作为其他方法和函数的参数和返回值。

val add10: Int => Int = _ + 10 // 一个接受一个 Int 类型参数并返回一个 Int 类型值的函数
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 被应用到每一个元素

// 匿名函数可以被使用来代替有命名的函数：
List(1, 2, 3) map (x => x + 10)

// 如果匿名函数只有一个参数可以用下划线作为变量
List(1, 2, 3) map (_ + 10)

// 如果您所应用的匿名块和匿名函数都接受一个参数，那么你甚至可以省略下划线
List("Dom", "Bob", "Natalia") foreach println


// 组合子

// 译注: val sq: Int => Int = x => x * x
s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// filter 函数接受一个 predicate （函数根据条件 A 返回 Boolean）并选择
// 所有满足 predicate 的元素
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name:String, age:Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Scala 的 foreach 方法定义在某些集合中，接受一个函数并返回 Unit （void 方法）
// 另请参见：
// http://www.scala-lang.org/api/current/index.html#scala.collection.IterableLike@foreach(f:A=>Unit):Unit
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// For 推导式

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* 注意，这些不是 for 循环，for 循环的语义是‘重复’，然而 for 推导式定义
   两个数据集合的关系。 */


/////////////////////////////////////////////////
// 8. 隐式转换
/////////////////////////////////////////////////

/* 警告 警告: 隐式转换是 Scala 中一套强大的特性，因此容易被滥用。
 * Scala 初学者在理解它们的工作原理和最佳实践之前，应抵制使用它的诱惑。
 * 我们加入这一章节仅因为它们在 Scala 的库中太过常见，导致没有用隐式转换的库
 * 就不可能做有意义的事情。这章节主要让你理解和使用隐式转换，而不是自己声明。
 */

// 可以通过 "implicit" 声明任何值（val, 函数，对象等）为隐式值，
// 请注意这些例子中，我们用到第5部分的 Dog 类。
implicit val myImplicitInt = 100
implicit def myImplicitFunction(breed: String) = new Dog("Golden " + breed)

// implicit 关键字本身不改变值的行为，所以上面的值可以照常使用。
myImplicitInt + 2  // => 102
myImplicitFunction("Pitbull").breed  // => "Golden Pitbull"

// 区别在于，当另一段代码“需要”隐式值时，这些值现在有资格作为隐式值。
// 一种情况是隐式函数参数。
def sendGreetings(toWhom: String)(implicit howMany: Int) =
  s"Hello $toWhom, $howMany blessings to you and yours!"

// 如果提供值给 “howMany”，函数正常运行
sendGreetings("John")(1000)  // => "Hello John, 1000 blessings to you and yours!"

// 如果省略隐式参数，会传一个和参数类型相同的隐式值，
// 在这个例子中， 是 “myImplicitInt":
sendGreetings("Jane")  // => "Hello Jane, 100 blessings to you and yours!"

// 隐式的函数参数使我们可以模拟其他函数式语言的 type 类（type classes）。
// 它经常被用到所以有特定的简写。这两行代码是一样的：
def foo[T](implicit c: C[T]) = ...
def foo[T : C] = ...

// 编译器寻找隐式值另一种情况是你调用方法时
//   obj.method(...)
// 但 "obj" 没有一个名为 "method" 的方法。这样的话，如果有一个参数类型为 A
// 返回值类型为 B 的隐式转换，obj 的类型是 A，B 有一个方法叫 "method" ，这样
// 转换就会被应用。所以作用域里有上面的 myImplicitFunction, 我们可以这样做：
"Retriever".breed  // => "Golden Retriever"
"Sheperd".bark  // => "Woof, woof!"

// 这里字符串先被上面的函数转换为 Dog 对象，然后调用相应的方法。
// 这是相当强大的特性，但再次提醒，请勿轻率使用。
// 事实上，当你定义上面的隐式函数时，编译器会作出警告，除非你真的了解
// 你正在做什么否则不要使用。


/////////////////////////////////////////////////
// 9. 杂项
/////////////////////////////////////////////////

// 导入类
import scala.collection.immutable.List

// 导入所有子包
import scala.collection.immutable._

// 一条语句导入多个类
import scala.collection.immutable.{List, Map}

// 使用 ‘=>’ 对导入进行重命名
import scala.collection.immutable.{ List => ImmutableList }

// 导入所有类，排除其中一些。下面的语句排除了 Map 和 Set：
import scala.collection.immutable.{Map => _, Set => _, _}

// 在 Scala 文件用 object 和单一的 main 方法定义程序入口：
object Application {
  def main(args: Array[String]): Unit = {
    // stuff goes here.
  }
}

// 文件可以包含多个 class 和 object，用 scalac 编译源文件




// 输入和输出

// 按行读文件
import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)

// 用 Java 的 PrintWriter 写文件
val writer = new PrintWriter("myfile.txt")
writer.write("Writing line for line" + util.Properties.lineSeparator)
writer.write("Another line here" + util.Properties.lineSeparator)
writer.close()

```

## 更多的资源

[为没耐心的人准备的 Scala](http://horstmann.com/scala/)

[Twitter Scala school](http://twitter.github.io/scala_school/)

[The Scala documentation](http://www.scala-lang.org/documentation/)

[在浏览器尝试 Scala](http://scalatutorials.com/tour/)

加入 [Scala 用户组](https://groups.google.com/forum/#!forum/scala-user)
