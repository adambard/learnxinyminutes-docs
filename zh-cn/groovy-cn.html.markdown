---
language: Groovy
filename: learngroovy-cn.groovy
contributors:
    - ["Roberto Pérez Alcolea", "http://github.com/rpalcolea"]
translators:
    - ["Todd Gao", "http://github.com/7c00"]
lang: zh-cn
---

Groovy - Java平台的动态语言。[了解更多。](http://www.groovy-lang.org/)

```groovy

/*
  安装：

  1) 安装 GVM - http://gvmtool.net/
  2) 安装 Groovy： gvm install groovy
  3) 启动 groovy 控制台，键入： groovyConsole

*/

//  双斜线开始的是单行注释
/*
像这样的是多行注释
*/

// Hello World
println "Hello world!"

/*
  变量：

  可以给变量赋值，以便稍后使用
*/

def x = 1
println x

x = new java.util.Date()
println x

x = -3.1499392
println x

x = false
println x

x = "Groovy!"
println x

/*
  集合和映射
*/

//创建一个空的列表
def technologies = []

/*** 往列表中增加一个元素 ***/

// 和Java一样
technologies.add("Grails")

// 左移添加，返回该列表
technologies << "Groovy"

// 增加多个元素
technologies.addAll(["Gradle","Griffon"])

/*** 从列表中删除元素 ***/

// 和Java一样
technologies.remove("Griffon")

// 减号也行
technologies = technologies - 'Grails'

/*** 遍历列表 ***/

// 遍历列表中的元素
technologies.each { println "Technology: $it"}
technologies.eachWithIndex { it, i -> println "$i: $it"}

/*** 检查列表内容 ***/

//判断列表是否包含某元素，返回boolean
contained = technologies.contains( 'Groovy' )

// 或
contained = 'Groovy' in technologies

// 检查多个元素
technologies.containsAll(['Groovy','Grails'])

/*** 列表排序 ***/

// 排序列表（修改原列表）
technologies.sort()

// 要想不修改原列表，可以这样：
sortedTechnologies = technologies.sort( false )

/*** 列表操作 ***/

//替换列表元素
Collections.replaceAll(technologies, 'Gradle', 'gradle')

//打乱列表
Collections.shuffle(technologies, new Random())

//清空列表
technologies.clear()

//创建空的映射
def devMap = [:]

//增加值
devMap = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
devMap.put('lastName','Perez')

//遍历映射元素
devMap.each { println "$it.key: $it.value" }
devMap.eachWithIndex { it, i -> println "$i: $it"}

//判断映射是否包含某键
assert devMap.containsKey('name')

//判断映射是否包含某值
assert devMap.containsValue('Roberto')

//取得映射所有的键
println devMap.keySet()

//取得映射所有的值
println devMap.values()

/*
  Groovy Beans

  GroovyBeans 是 JavaBeans，但使用了更简单的语法

  Groovy 被编译为字节码时，遵循下列规则。

    * 如果一个名字声明时带有访问修饰符（public, private, 或者 protected），
      则会生成一个字段（field）。

    * 名字声明时没有访问修饰符，则会生成一个带有public getter和setter的
      private字段，即属性（property）。

    * 如果一个属性声明为final，则会创建一个final的private字段，但不会生成setter。

    * 可以声明一个属性的同时定义自己的getter和setter。

    * 可以声明具有相同名字的属性和字段，该属性会使用该字段。

    * 如果要定义private或protected属性，必须提供声明为private或protected的getter
      和setter。

    * 如果使用显式或隐式的 this（例如 this.foo, 或者 foo）访问类的在编译时定义的属性，
      Groovy会直接访问对应字段，而不是使用getter或者setter

    * 如果使用显式或隐式的 foo 访问一个不存在的属性，Groovy会通过元类（meta class）
      访问它，这可能导致运行时错误。

*/

class Foo {
    // 只读属性
    final String name = "Roberto"

    // 只读属性，有public getter和protected setter
    String language
    protected void setLanguage(String language) { this.language = language }

    // 动态类型属性
    def lastName
}

/*
  逻辑分支和循环
*/

//Groovy支持常见的if - else语法
def x = 3

if(x==1) {
    println "One"
} else if(x==2) {
    println "Two"
} else {
    println "X greater than Two"
}

//Groovy也支持三元运算符
def y = 10
def x = (y > 1) ? "worked" : "failed"
assert x == "worked"

//for循环
//使用区间（range）遍历
def x = 0
for (i in 0 .. 30) {
    x += i
}

//遍历列表
x = 0
for( i in [5,3,2,1] ) {
    x += i
}

//遍历数组
array = (0..20).toArray()
x = 0
for (i in array) {
    x += i
}

//遍历映射
def map = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
x = ""
for ( e in map ) {
    x += e.value
    x += " "
}
assert x.equals("Roberto Grails Groovy ")

/*
  运算符

  在Groovy中以下常用运算符支持重载：
  http://www.groovy-lang.org/operators.html#Operator-Overloading

  实用的groovy运算符
*/
//展开（spread）运算符：对聚合对象的所有元素施加操作
def technologies = ['Groovy','Grails','Gradle']
technologies*.toUpperCase() // 相当于 technologies.collect { it?.toUpperCase() }

//安全导航（safe navigation）运算符：用来避免NullPointerException
def user = User.get(1)
def username = user?.username


/*
  闭包
  Groovy闭包好比代码块或者方法指针，它是一段代码定义，可以以后执行。

  更多信息见：http://www.groovy-lang.org/closures.html
*/
//例子：
def clos = { println "Hello World!" }

println "Executing the Closure:"
clos()

//传参数给闭包
def sum = { a, b -> println a+b }
sum(2,4)

//闭包可以引用参数列表以外的变量
def x = 5
def multiplyBy = { num -> num * x }
println multiplyBy(10)

// 只有一个参数的闭包可以省略参数的定义
def clos = { print it }
clos( "hi" )

/*
  Groovy可以记忆闭包结果 [1][2][3]
*/
def cl = {a, b ->
    sleep(3000) // 模拟费时操作
    a + b
}

mem = cl.memoize()

def callClosure(a, b) {
    def start = System.currentTimeMillis()
    mem(a, b)
    println "Inputs(a = $a, b = $b) - took ${System.currentTimeMillis() - start} msecs."
}

callClosure(1, 2)
callClosure(1, 2)
callClosure(2, 3)
callClosure(2, 3)
callClosure(3, 4)
callClosure(3, 4)
callClosure(1, 2)
callClosure(2, 3)
callClosure(3, 4)

/*
  Expando

  Expando类是一种动态bean类，可以给它的实例添加属性和添加闭包作为方法

  http://mrhaki.blogspot.mx/2009/10/groovy-goodness-expando-as-dynamic-bean.html
*/
  def user = new Expando(name:"Roberto")
  assert 'Roberto' == user.name

  user.lastName = 'Pérez'
  assert 'Pérez' == user.lastName

  user.showInfo = { out ->
      out << "Name: $name"
      out << ", Last name: $lastName"
  }

  def sw = new StringWriter()
  println user.showInfo(sw)


/*
  元编程(MOP)
*/

//使用ExpandoMetaClass增加行为
String.metaClass.testAdd = {
    println "we added this"
}

String x = "test"
x?.testAdd()

//拦截方法调用
class Test implements GroovyInterceptable {
    def sum(Integer x, Integer y) { x + y }

    def invokeMethod(String name, args) {
        System.out.println "Invoke method $name with args: $args"
    }
}

def test = new Test()
test?.sum(2,3)
test?.multiply(2,3)

//Groovy支持propertyMissing，来处理属性解析尝试
class Foo {
   def propertyMissing(String name) { name }
}
def f = new Foo()

assertEquals "boo", f.boo

/*
  类型检查和静态编译
  Groovy天生是并将永远是一门动态语言，但也支持类型检查和静态编译

  更多： http://www.infoq.com/articles/new-groovy-20
*/
//类型检查
import groovy.transform.TypeChecked

void testMethod() {}

@TypeChecked
void test() {
    testMeethod()

    def name = "Roberto"

    println naameee

}

//另一例子
import groovy.transform.TypeChecked

@TypeChecked
Integer test() {
    Integer num = "1"

    Integer[] numbers = [1,2,3,4]

    Date date = numbers[1]

    return "Test"

}

//静态编译例子
import groovy.transform.CompileStatic

@CompileStatic
int sum(int x, int y) {
    x + y
}

assert sum(2,5) == 7


```

## 进阶资源

[Groovy文档](http://www.groovy-lang.org/documentation.html)

[Groovy web console](http://groovyconsole.appspot.com/)

加入[Groovy用户组](http://www.groovy-lang.org/usergroups.html)

## 图书

* [Groovy Goodness] (https://leanpub.com/groovy-goodness-notebook)

* [Groovy in Action] (http://manning.com/koenig2/)

* [Programming Groovy 2: Dynamic Productivity for the Java Developer] (http://shop.oreilly.com/product/9781937785307.do)

[1] http://roshandawrani.wordpress.com/2010/10/18/groovy-new-feature-closures-can-now-memorize-their-results/
[2] http://www.solutionsiq.com/resources/agileiq-blog/bid/72880/Programming-with-Groovy-Trampoline-and-Memoize
[3] http://mrhaki.blogspot.mx/2011/05/groovy-goodness-cache-closure-results.html



