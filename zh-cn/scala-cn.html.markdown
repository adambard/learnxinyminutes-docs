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
// 5. 面向对象编程
/////////////////////////////////////////////////

/*
  旁白： 到目前为止我们在本教程中所做的一切不过是简单表达式 （值、 函数等）。
  这些表达式都能很方便地键入到命令行解释器中进行快速测试，但他们不能独立存在于 Scala 文件中。
  例如，你不能在一个 Scala 文件只写 "val x = 5"。
  相反，只有顶级结构允许在 Scala 文件中，它们是：

  - 对象 (objects)
  - 类 (classes)
  - 样本类 (case classes)
  - 特质 (traits)

  现在将解释它们分别是什么。
*/

// 类的概念与其他语言类似。构造函数的参数声明在类名后，并在类体中进行初始化。
class Dog(br: String) {
  // 从这里开始就是构造函数
  var breed: String = br

  // 定义一个名为bark的方法，返回一个String
  def bark = "Woof, woof!"

  // 字段和方法默认是 public 的。也可以使用"protected"和"private"关键字。
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  // 抽象方法就是没有方法体的声明。 If we uncomment the next
  // 如果我们去掉下面语句的注释，Dog类将必须声明为抽象(abstract)。 
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.breed) // => "greyhound"
println(mydog.bark) // => "Woof, woof!"

// "object"关键字创建一个类型和它的一个单例实例。
// Scala 类经常具有一个"伴生对象"，在类本身中定义每个实例的行为，
// 而与所有该类的实例有关的行为定义在对象中。
// 其区别类似于其他语言（如Java）中的类方法 vs 静态方法。
// 需要注意的是对象和类可以具有相同的名称。
object Dog {
  def allKnownBreeds = List("pitbull", "shepherd", "retriever")
  def createDog(breed: String) = new Dog(breed)
}


// 样本类是内建了额外功能的类。
// Scala 初学者的一个常见问题是何时使用类和何时使用样本类。
// 这个界限是相当模糊的，但一般情况下，类往往聚焦于封装、 多态性和行为。
// 这些类中的字段倾向是私有，并且只有方法是公开的。
// 样本类的主要目的是持有不可变的数据。
// 它们一般很少有方法，并且方法很少有副作用。
case class Person(name: String, phoneNumber: String)

// 创建一个实例。注意样本类不需要"new"
val george = Person("George", "1234")
val kate = Person("Kate", "4567")

// 样本类可以给你一些方便的方法，如获取器：
george.phoneNumber  // => "1234"

// 字段比较 (不需要重写 ".equals")
Person("George", "1234") == Person("Kate", "1236")  // => false

// 方便地复制方法
// otherGeorge == Person("george", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// 以及其他许多。样本类也能免费具有模式匹配能力，见下文。


// 特质 敬请期待!


/////////////////////////////////////////////////
// 6. 模式匹配
/////////////////////////////////////////////////

// 模式匹配是 Scala 中一种强大而常用的功能。
// 这里介绍如何模式匹配一个样本类。
// 注： 与其他语言不同，Scala的case不需要break，错误的case串联不会发生。

def matchPerson(person: Person): String = person match {
  // Then you specify the patterns:
  case Person("George", number) => "We found George! His number is " + number
  case Person("Kate", number) => "We found Kate! Her number is " + number
  case Person(name, number) => "We matched someone : " + name + ", phone : " + number
}

val email = "(.*)@(.*)".r  // 为下一个例子定义一个正则表达式。

// 模式匹配可能看起来很像 C 语言家族中的 switch 语句 ，但模式匹配要强大得多。
// 在 Scala 中，您可以匹配更多：
def matchEverything(obj: Any): String = obj match {
  // 可以匹配值:
  case "Hello world" => "Got the string Hello world"

  // 可以通过类型匹配:
  case x: Double => "Got a Double: " + x

  // 可以指定条件:
  case x: Int if x > 10000 => "Got a pretty big number!"

  // 可以像之前那样匹配样本类:
  case Person(name, number) => s"Got contact info for $name!"

  // 可以匹配正则表达式:
  case email(name, domain) => s"Got email address $name@$domain"

  // 可以匹配元组:
  case (a: Int, b: Double, c: String) => s"Got a tuple: $a, $b, $c"

  // 可以匹配数据结构:
  case List(1, b, c) => s"Got a list with three elements and starts with 1: 1, $b, $c"

  // 可以嵌套模式:
  case List(List((1, 2,"YAY"))) => "Got a list of list of tuple"
}

// 事实上，你可以模式匹配任何有"unapply"方法的对象。
// 此功能是如此强大，以至于 Scala 允许你将整个函数定义为模式：
val patternFunc: Person => String = {
  case Person("George", number) => s"George's number: $number"
  case Person(name, number) => s"Random person's number: $number"
}


/////////////////////////////////////////////////
// 7. 函数式编程
/////////////////////////////////////////////////

// Scala 中方法和函数可以被其他方法或函数返回，或当做参数。

val add10: Int => Int = _ + 10 // 一个接受一个 Int 类型参数并返回一个 Int 类型值的函数
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 被应用到每一个元素

// 匿名函数可以被使用来代替有命名的函数：
List(1, 2, 3) map (x => x + 10)

// 如果匿名函数只有一个参数，可以使用下划线来表示该参数变量
List(1, 2, 3) map (_ + 10)

// 如果你所应用的匿名块和匿名函数都接受一个参数，那么你甚至可以省略下划线
List("Dom", "Bob", "Natalia") foreach println


// 组合子

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// filter 函数接受一个预测函数(一个形式为 A -> Boolean的函数) 并选择出所有满足这个预测的元素
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name:String, age:Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Scala 的 foreach 方法定义在集合上
// 接受一个特定类型的参数并返回 Unit(一个 void 方法)
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// For 包含

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* 注意：这些不是 for 循环。 一个 for 循环的语义是 '重复'('repeat')，
  而一个 for-包含 定义了两个数据集合之间的某种关系 */


/////////////////////////////////////////////////
// 8. 隐式转换
/////////////////////////////////////////////////

/* 警告：隐式转换是 Scala一套强大的特性，并因此很容易遭到滥用。
 * Scala 初学者应该抵制使用他们的诱惑，直到他们不仅明白它们是如何工作的，而且知道其相关的最佳实践。
 * 本教程中之所以包含这一节，是因为隐式转换在 Scala 库中应用如此广泛，以至于不用它已不可能做有意义的事情。
 * 这意味着你需要理解和使用 implicts，而不是声明自己的。
 */

// 可以通过"implicit"关键字声明任何值(vals、函数、对象等)为隐式值。
// 注意这些示例中用到了第5节的Dog类。
implicit val myImplicitInt = 100
implicit def myImplicitFunction(breed: String) = new Dog("Golden " + breed)

// implicit 关键字本身并不会更改值的行为，因此上面的值可以像往常一样被使用。
myImplicitInt + 2  // => 102
myImplicitFunction("Pitbull").breed  // => "Golden Pitbull"

// 差异是这些值现在有资格在另一段代码"需要"隐式值时被用到。
// 一种场景是隐式函数参数：
def sendGreetings(toWhom: String)(implicit howMany: Int) =
  s"Hello $toWhom, $howMany blessings to you and yours!"

// 如果我们提供"howMany"的值，函数的行为像往常一样
sendGreetings("John")(1000)  // => "Hello John, 1000 blessings to you and yours!"

// 但如果我们省略隐式参数，一个同类型的隐式值将被使用，在本例中是"myImplicitInt"：
sendGreetings("Jane")  // => "Hello Jane, 100 blessings to you and yours!"

// 隐式函数参数使我们可以模拟其他函数式语言中的类型类。
// 由于经常这样使用，因而有自己的简写。
// 以下两行意思是相同的：
def foo[T](implicit c: C[T]) = ...
def foo[T : C] = ...

// 编译器寻找隐式转换的另一种场景是，如果你有
//   obj.method(...)
// 但是"obj"没有"method"这个方法。
// 在这种情况下，如果存在 "A => B" 类型的隐式转换，其中 A 的类型是 obj，
// 并且 B 有"method"方法，这种转换将被应用。
// 所以如果作用域中有 myImplicitFunction ，则可以这样写：
"Retriever".breed  // => "Golden Retriever"
"Sheperd".bark  // => "Woof, woof!"

// 这里字符串首先使用我们的函数被转换为Dog，然后调用适当的方法。
// 这是一个非常强大的功能，但重申一次，它不能随意使用。
// 事实上，当你定义上面的隐式函数，编译器应该会给你一个警告：
// 你不应该这样做，除非你确实知道你在做什么。


/////////////////////////////////////////////////
// 9. 杂项
/////////////////////////////////////////////////

// 引入类
import scala.collection.immutable.List

// 引入所有子包
import scala.collection.immutable._

// 在一条语句中引入多个类
import scala.collection.immutable.{List, Map}

// 使用 '=>' 重命名
import scala.collection.immutable.{ List => ImmutableList }

// 引入除了某些类外，其它所有的类。下面的例子排除了 Map 和 Set ：
import scala.collection.immutable.{Map => _, Set => _, _}

// 在 scala 源文件中，你的程序入口点使用一个拥有 main 方法的对象来定义：
object Application {
  def main(args: Array[String]): Unit = {
    // stuff goes here.
  }
}

// 文件可以包含多个类和对象。由 scalac 来编译




// 输入和输出

// 一行行读取文件
import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)

// 使用 Java 的 PrintWriter 来写文件
val writer = new PrintWriter("myfile.txt")
writer.write("Writing line for line" + util.Properties.lineSeparator)
writer.write("Another line here" + util.Properties.lineSeparator)
writer.close()

```

## 更多的资源

[快学 Scala](http://horstmann.com/scala/)

[Twitter Scala school](http://twitter.github.io/scala_school/)

[The Scala documentation](http://www.scala-lang.org/documentation/)

[在浏览器尝试 Scala](http://scalatutorials.com/tour/)

加入 [Scala 用户组](https://groups.google.com/forum/#!forum/scala-user)
