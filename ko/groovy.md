---
name: Groovy
contributors:
    - ["Roberto Pérez Alcolea", "http://github.com/rpalcolea"]
filename: learngroovy.groovy
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Groovy](http://www.groovy-lang.org/)는 Java 플랫폼을 위한 동적 언어입니다.

```groovy
/*
  설정:

  1) SDKMAN 설치 - http://sdkman.io/
  2) Groovy 설치: sdk install groovy
  3) groovyConsole을 입력하여 groovy 콘솔 시작

*/

//  한 줄 주석은 두 개의 슬래시로 시작합니다.
/*
여러 줄 주석은 이렇습니다.
*/

// Hello World
println "Hello world!"

/*
  변수:

  나중에 사용하기 위해 변수에 값을 할당할 수 있습니다.
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
  컬렉션 및 맵
*/

//빈 목록 만들기
def technologies = []

// 또는 데이터로 목록 만들기
technologies = ["Kotlin", "Swift"]

/*** 목록에 요소 추가 ***/

// Java와 마찬가지로
technologies.add("Grails")

// 왼쪽 시프트는 추가하고 목록을 반환합니다.
technologies << "Groovy"

// 여러 요소 추가
technologies.addAll(["Gradle","Griffon"])

/*** 목록에서 요소 제거 ***/

// Java와 마찬가지로
technologies.remove("Griffon")

// 빼기도 작동합니다.
technologies = technologies - 'Grails'

/*** 목록 반복 ***/

// 목록의 요소를 반복합니다.
technologies.each { println "Technology: $it"}
technologies.eachWithIndex { it, i -> println "$i: $it"}

/*** 목록 내용 확인 ***/

//목록에 요소가 포함되어 있는지 평가합니다(부울).
contained = technologies.contains( 'Groovy' )

// 또는
contained = 'Groovy' in technologies

// 여러 내용 확인
technologies.containsAll(['Groovy','Grails'])

/*** 목록 정렬 ***/

// 목록 정렬 (원본 목록 변경)
technologies.sort()

// 원본을 변경하지 않고 정렬하려면 다음을 수행할 수 있습니다:
sortedTechnologies = technologies.sort( false )

/*** 목록 조작 ***/

//목록의 모든 요소 바꾸기
Collections.replaceAll(technologies, 'Gradle', 'gradle')

//목록 섞기
Collections.shuffle(technologies, new Random())

//목록 지우기
technologies.clear()

//빈 맵 만들기
def devMap = [:]

//값 추가
devMap = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
devMap.put('lastName','Perez')

//맵의 요소를 반복합니다.
devMap.each { println "$it.key: $it.value" }
devMap.eachWithIndex { it, i -> println "$i: $it"}

//맵에 키가 포함되어 있는지 평가합니다.
assert devMap.containsKey('name')

//맵에 값이 포함되어 있는지 평가합니다.
assert devMap.containsValue('Roberto')

//맵의 키 가져오기
println devMap.keySet()

//맵의 값 가져오기
println devMap.values()

/*
  Groovy Beans

  GroovyBeans는 JavaBeans이지만 훨씬 간단한 구문을 사용합니다.

  Groovy가 바이트코드로 컴파일될 때 다음 규칙이 사용됩니다.

    * 이름이 액세스 수정자(public, private 또는 protected)로 선언되면 필드가 생성됩니다.

    * 액세스 수정자 없이 선언된 이름은 public getter 및 setter가 있는 private 필드를 생성합니다(즉, 속성).

    * 속성이 final로 선언되면 private 필드가 final로 생성되고 setter가 생성되지 않습니다.

    * 속성을 선언하고 자신의 getter 또는 setter를 선언할 수도 있습니다.

    * 속성과 동일한 이름의 필드를 선언할 수 있으며, 속성은 해당 필드를 사용합니다.

    * private 또는 protected 속성을 원하면 자신의 getter 또는 setter를 제공해야 하며, 이는 private 또는 protected로 선언되어야 합니다.

    * 클래스 내에서 속성에 액세스하면 속성이 정의된 클래스 내에서 컴파일 타임에 암시적 또는 명시적 this(예: this.foo 또는 단순히 foo)로 액세스하면 Groovy는 getter 및 setter를 거치지 않고 필드에 직접 액세스합니다.

    * 명시적 또는 암시적 foo를 사용하여 존재하지 않는 속성에 액세스하면 Groovy는 메타 클래스를 통해 속성에 액세스하며, 이는 런타임에 실패할 수 있습니다.

*/

class Foo {
    // 읽기 전용 속성
    final String name = "Roberto"

    // public getter 및 protected setter가 있는 읽기 전용 속성
    String language
    protected void setLanguage(String language) { this.language = language }

    // 동적으로 유형이 지정된 속성
    def lastName
}

/*
  선택적 매개변수가 있는 메서드
*/

// 메서드는 매개변수에 대한 기본값을 가질 수 있습니다.
def say(msg = 'Hello', name = 'world') {
    "$msg $name!"
}

// 3가지 다른 방식으로 호출할 수 있습니다.
assert 'Hello world!' == say()
// 기본값이 있는 가장 오른쪽 매개변수가 먼저 제거됩니다.
assert 'Hi world!' == say('Hi')
assert 'learn groovy!' == say('learn', 'groovy')

/*
  논리적 분기 및 반복
*/

//Groovy는 일반적인 if - else 구문을 지원합니다.
def x = 3

if(x==1) {
    println "One"
} else if(x==2) {
    println "Two"
} else {
    println "X greater than Two"
}

//Groovy는 삼항 연산자도 지원합니다:
def y = 10
def x = (y > 1) ? "worked" : "failed"
assert x == "worked"

//Groovy는 'Elvis 연산자'도 지원합니다!
//삼항 연산자를 사용하는 대신:

displayName = user.name ? user.name : 'Anonymous'

//다음과 같이 작성할 수 있습니다:
displayName = user.name ?: 'Anonymous'

//For 루프
//범위를 반복합니다.
def x = 0
for (i in 0 .. 30) {
    x += i
}

//목록을 반복합니다.
x = 0
for( i in [5,3,2,1] ) {
    x += i
}

//배열을 반복합니다.
array = (0..20).toArray()
x = 0
for (i in array) {
    x += i
}

//맵을 반복합니다.
def map = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
x = ""
for ( e in map ) {
    x += e.value
    x += " "
}
assert x.equals("Roberto Grails Groovy ")

/*
  연산자

  Groovy가 지원하는 일반적인 연산자 목록에 대한 연산자 오버로딩:
  http://www.groovy-lang.org/operators.html#Operator-Overloading

  유용한 groovy 연산자
*/
//확산 연산자:  집계 개체의 모든 항목에 대해 작업을 호출합니다.
def technologies = ['Groovy','Grails','Gradle']
technologies*.toUpperCase() // = to technologies.collect { it?.toUpperCase() }

//안전 탐색 연산자: NullPointerException을 피하는 데 사용됩니다.
def user = User.get(1)
def username = user?.username


/*
  클로저
  Groovy 클로저는 "코드 블록" 또는 메서드 포인터와 같습니다. 정의된 다음 나중에 실행되는 코드 조각입니다.

  자세한 정보: http://www.groovy-lang.org/closures.html
*/
//예:
def clos = { println "Hello World!" }

println "Executing the Closure:"
clos()

//클로저에 매개변수 전달
def sum = { a, b -> println a+b }
sum(2,4)

//클로저는 매개변수 목록에 나열되지 않은 변수를 참조할 수 있습니다.
def x = 5
def multiplyBy = { num -> num * x }
println multiplyBy(10)

// 단일 인수를 사용하는 클로저가 있는 경우 클로저의 매개변수 정의를 생략할 수 있습니다.
def clos = { print it }
clos( "hi" )

/*
  Groovy는 클로저 결과를 메모할 수 있습니다.
*/
def cl = {a, b ->
    sleep(3000) // 시간 소모적인 처리 시뮬레이션
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

  Expando 클래스는 동적 빈이므로 속성을 추가하고 이 클래스의 인스턴스에 클로저를 메서드로 추가할 수 있습니다.

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
  메타프로그래밍 (MOP)
*/

//ExpandoMetaClass를 사용하여 동작 추가
String.metaClass.testAdd = {
    println "we added this"
}

String x = "test"
x?.testAdd()

//메서드 호출 가로채기
class Test implements GroovyInterceptable {
    def sum(Integer x, Integer y) { x + y }

    def invokeMethod(String name, args) {
        System.out.println "Invoke method $name with args: $args"
    }
}

def test = new Test()
test?.sum(2,3)
test?.multiply(2,3)

//Groovy는 속성 해결 시도를 처리하기 위해 propertyMissing을 지원합니다.
class Foo {
   def propertyMissing(String name) { name }
}
def f = new Foo()

assertEquals "boo", f.boo

/*
  TypeChecked 및 CompileStatic
  Groovy는 본질적으로 동적 언어이며 항상 그럴 것이지만 TypeChecked 및 CompileStatic을 지원합니다.

  자세한 정보: http://www.infoq.com/articles/new-groovy-20
*/
//TypeChecked
import groovy.transform.TypeChecked

void testMethod() {}

@TypeChecked
void test() {
    testMeethod()

    def name = "Roberto"

    println naameee

}

//또 다른 예:
import groovy.transform.TypeChecked

@TypeChecked
Integer test() {
    Integer num = "1"

    Integer[] numbers = [1,2,3,4]

    Date date = numbers[1]

    return "Test"

}

//CompileStatic 예:
import groovy.transform.CompileStatic

@CompileStatic
int sum(int x, int y) {
    x + y
}

assert sum(2,5) == 7
```

## 추가 자료

[Groovy 문서](http://www.groovy-lang.org/documentation.html)

[Groovy 웹 콘솔](http://groovyconsole.appspot.com/)

[Groovy 사용자 그룹 가입](http://www.groovy-lang.org/usergroups.html)

## 도서

* [Groovy Goodness](https://leanpub.com/groovy-goodness-notebook)
* [Groovy in Action](http://manning.com/koenig2/)
* [Programming Groovy 2: Dynamic Productivity for the Java Developer](http://shop.oreilly.com/product/9781937785307.do)