---
language: Scala
filename: learnscala-zh.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
translators:
    - ["Peiyong Lin", ""]
filename: learn.scala
lang: zh-cn
---

Scala - 一门可拓展性的语言

```cpp

/*
  自行设置:

  1) 下载 Scala - http://www.scala-lang.org/downloads
  2) unzip/untar 到你喜欢的地方，放在路径中的 bin 目录下
  3) 在终端输入 scala，开启 Scala 的 REPL，你会看到提示符：

  scala>

  这就是所谓的 REPL，你现在可以在其中运行命令，让我们做到这一点：
*/

println(10) // 打印整数 10

println("Boo!") // 打印字符串 "BOO!"


// 一些基础

// 打印并强制换行
println("Hello world!")
// 没有强制换行的打印
print("Hello world")

// 通过 var 或者 val 来声明变量
// val 声明是不可变的，var 声明是可修改的。不可变性是好事。
val x = 10 // x 现在是 10
x = 20 // 错误: 对 val 声明的变量重新赋值
var x = 10 
x = 20  // x 现在是 20

//  单行注释开始于两个斜杠
/* 
多行注释看起来像这样。
*/

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


// 在 REPL 计算一个命令会返回给你结果的类型和值

1 + 7

/* 上行的结果是：

  scala> 1 + 7
  res29: Int = 8

  这意味着计算 1 + 7 的结果是一个 Int 类型的对象，其值为 8

  1+7 的结果是一样的
*/


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



// 数据结构

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

// 要读取元组的元素，使用 _._n，n是从1开始的元素索引

val d = divideInts(10,3)

d._1

d._2



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



// 循环和迭代

1 to 5
val r = 1 to 5
r.foreach( println )

r foreach println     
// 注意：Scala 是相当宽容的当它遇到点和括号 - 分别地学习这些规则。
// 这帮助你编写读起来像英语的 DSLs 和 APIs

(5 to 1 by -1) foreach ( println )

// while 循环
var i = 0
while (i < 10) {  println("i " + i); i+=1  }

while (i < 10) {  println("i " + i); i+=1  }   // 发生了什么？为什么？

i    // 展示 i 的值。注意到 while 是一个传统意义上的循环
     // 它顺序地执行并且改变循环变量的值。while 非常快，比 Java // 循环快，
     // 但是在其上使用选择器和包含更容易理解和并行。

// do while 循环
do {
  println("x is still less then 10"); 
  x += 1
} while (x < 10)

// 在 Scala中，尾递归是一种惯用的执行循环的方式。
// 递归函数需要显示的返回类型，编译器不能推断出类型。
// 这里它是 Unit。
def showNumbersInRange(a:Int, b:Int):Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}



// 条件语句

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println ("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"

var i = 0
while (i < 10) { println("i " + i); i+=1  }



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
import scala.collection.immutable{ List => ImmutableList }

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
