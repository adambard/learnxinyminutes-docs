---
language: Scala
filename: learnscala-es.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Ha-Duong Nguyen", "http://reference-error.org"]
translators:
    - ["Pablo Arranz Ropero", "http://arranzropablo.com"]
lang: es-es
---

Scala - El lenguaje escalable

```scala

/////////////////////////////////////////////////
// 0. Básicos
/////////////////////////////////////////////////
/*
  Configurar Scala:

  1) Descarga Scala - http://www.scala-lang.org/downloads
  2) Unzip/untar a tu carpeta elegida y pon la subcarpeta bin en tu variable de entorno `PATH`
*/

/*
  Prueba REPL

  Scala tiene una herramienta llamada REPL (Read-Eval-Print Loop, en español: Bucle de lectura-evaluación-impresión) que es analogo a interpretes de la linea de comandos en muchos otros lenguajes. 
  Puedes escribir cualquier expresión en Scala y el resultado será evaluado e impreso.  

  REPL es una herramienta muy práctica para testear y verificar código.
  Puedes usarla mientras lees este tutorial para explorar conceptos por tu cuenta.
*/

// Inicia Scala REPL ejecutando `scala` en tu terminal. Deberías ver:
$ scala
scala>

// Por defecto cada expresión que escribes es guardada como un nuevo valor numerado:
scala> 2 + 2
res0: Int = 4

// Los valores por defecto pueden ser reusados. Fíjate en el tipo del valor mostrado en el resultado...
scala> res0 + 2
res1: Int = 6

// Scala es un lenguaje fuertemente tipado. Puedes usar REPL para comprobar el tipo sin evaluar una expresión.
scala> :type (true, 2.0)
(Boolean, Double)

// Las sesiones REPL pueden ser guardadas
scala> :save /sites/repl-test.scala

// Se pueden cargar archivos en REPL
scala> :load /sites/repl-test.scala
Loading /sites/repl-test.scala...
res2: Int = 4
res3: Int = 6

// Puedes buscar en tu historial reciente
scala> :h?
1 2 + 2
2 res0 + 2
3 :save /sites/repl-test.scala
4 :load /sites/repl-test.scala
5 :h?

// Ahora que sabes como jugar, aprendamos un poco de Scala...

/////////////////////////////////////////////////
// 1. Básicos
/////////////////////////////////////////////////

// Los comentarios de una linea comienzan con dos barras inclinadas

/*
  Los comentarios de varias lineas, como ya has visto arriba, se hacen de esta manera.
*/

// Así imprimimos forzando una nueva linea en la siguiente impresión
println("Hola mundo!")
println(10)
// Hola mundo!
// 10

// Así imprimimos sin forzar una nueva linea en la siguiente impresión
print("Hola mundo")
print(10)
// Hola mundo10

// Para declarar valores usamos var o val.
// Valores decalrados con val son inmutables, mientras que los declarados con var son mutables. 
// La inmutabilidad es algo bueno.
val x = 10 // x es 10
x = 20     // error: reassignment to val
var y = 10
y = 20     // y es 20

/*
  Scala es un lenguaje tipado estáticamente, aunque se puede ver en las expresiones anteriores que no hemos especificado un tipo. 
  Esto es debido a una funcionalidad del lenguaje llamada inferencia. En la mayoría de los casos, el compilador de Scala puede adivinar cual es el tipo de una variable, así que no hace falta escribirlo siempre.
  Podemos declarar explicitamente el tipo de una variable de la siguiente manera:
*/
val z: Int = 10
val a: Double = 1.0

// Observa la conversión automática de Int a Double, el resultado será 10.0, no 10
val b: Double = 10

// Valores Booleanos
true
false

// Operaciones Booleanas
!true         // false
!false        // true
true == false // false
10 > 5        // true

// Las operaciones matemáticas se realizan como siempre
1 + 1   // 2
2 - 1   // 1
5 * 3   // 15
6 / 2   // 3
6 / 4   // 1
6.0 / 4 // 1.5
6 / 4.0 // 1.5


// Evaluar una expresión en REPL te da el tipo y valor del resultado

1 + 7

/* La linea superior tienen como resultado:

  scala> 1 + 7
  res29: Int = 8

  Esto quiere decir que el resultado de evaluar 1 + 7 es un objeto de tipo Int con valor 8

  Observa que "res29" es un nombre de variable secuencialmente generado para almacenar los resultados de las expresiones escritas, la salida que observes puede diferir en este sentido.
*/

"Las cadenas en Scala están rodeadas por comillas dobles"
'a' // Un caracter en Scala
// 'Las cadenas con comillas simples no existen' <= Esto causa un error

// Las cadenas tienen los los típicos metodos de Java definidos
"hello world".length
"hello world".substring(2, 6)
"hello world".replace("C", "3")

// También tienen algunos métodos extra de Scala. Ver: scala.collection.immutable.StringOps
"hello world".take(5)
"hello world".drop(5)

// Interpolación de cadenas: Observa el prefijo "s"
val n = 45
s"Tengo $n manzanas" // => "Tengo 45 manzanas"

// Es posible colocar expresiones dentro de cadenas interpoladas
val a = Array(11, 9, 6)
s"Mi segunda hija tiene ${a(0) - a(2)} años."         // => "Mi segunda hija tiene 5 años."
s"Hemos doblado la cantidad de ${n / 2.0} manzanas."  // => "Hemos doblado la cantidad de 22.5 manzanas."
s"Potencia de 2: ${math.pow(2, 2)}"                   // => "Potencia de 2: 4"

// Podemos formatear cadenas interpoladas con el prefijo "f"
f"Potencia de 5: ${math.pow(5, 2)}%1.0f"         // "Potencia de 5: 25"
f"Raiz cuadrada de 122: ${math.sqrt(122)}%1.4f" // "Raiz cuadrada de 122: 11.0454"

// Las cadenas puras ignoran caracteres especiales.
raw"Nueva linea: \n. Retorno: \r." // => "Nueva linea: \n. Retorno: \r."

// Algunos caracteres necesitn ser escapados, por ejemplo unas comillas dobles dentro de una cadena:
"Se quedaron fuera de \"Rose and Crown\"" // => "Se quedaron fuera de "Rose and Crown""

// Las triples comillas dobles dejan que las cadenas se expandan por multiples filas y contengan comillas dobles o simples
val html = """<form id="daform">
                <p>Press belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. Funciones
/////////////////////////////////////////////////

// Functions are defined like so:
//
//   def functionName(args...): ReturnType = { body... }
//
// If you come from more traditional languages, notice the omission of the
// return keyword. In Scala, the last expression in the function block is the
// return value.
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// The { } can be omitted if the function body is a single expression:
def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y

// Syntax for calling functions is familiar:
sumOfSquares(3, 4)  // => 25

// You can use parameters names to specify them in different order
def subtract(x: Int, y: Int): Int = x - y

subtract(10, 3)     // => 7
subtract(y=10, x=3) // => -7

// In most cases (with recursive functions the most notable exception), function
// return type can be omitted, and the same type inference we saw with variables
// will work with function return values:
def sq(x: Int) = x * x  // Compiler can guess return type is Int

// Functions can have default parameters:
def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2) // => 3
addWithDefault(1)    // => 6


// Anonymous functions look like this:
(x: Int) => x * x

// Unlike defs, even the input type of anonymous functions can be omitted if the
// context makes it clear. Notice the type "Int => Int" which means a function
// that takes Int and returns Int.
val sq: Int => Int = x => x * x

// Anonymous functions can be called as usual:
sq(10)   // => 100

// If each argument in your anonymous function is
// used only once, Scala gives you an even shorter way to define them. These
// anonymous functions turn out to be extremely common, as will be obvious in
// the data structure section.
val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)

addOne(5)      // => 6
weirdSum(2, 4) // => 16


// The return keyword exists in Scala, but it only returns from the inner-most
// def that surrounds it.
// WARNING: Using return in Scala is error-prone and should be avoided.
// It has no effect on anonymous functions. For example:
def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5)
      return z // This line makes z the return value of foo!
    else
      z + 2    // This line is the return value of anonFunc
  }
  anonFunc(x)  // This line is the return value of foo
}


/////////////////////////////////////////////////
// 3. Flow Control
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach(println)

r foreach println
// NB: Scala is quite lenient when it comes to dots and brackets - study the
// rules separately. This helps write DSLs and APIs that read like English

// Why doesn't `println` need any parameters here?
// Stay tuned for first-class functions in the Functional Programming section below!
(5 to 1 by -1) foreach (println)

// A while loop
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }   // Yes, again. What happened? Why?

i    // Show the value of i. Note that while is a loop in the classical sense -
     // it executes sequentially while changing the loop variable. while is very
     // fast, but using the combinators and comprehensions above is easier
     // to understand and parallelize

// A do-while loop
i = 0
do {
  println("i is still less than 10")
  i += 1
} while (i < 10)

// Recursion is the idiomatic way of repeating an action in Scala (as in most
// other functional languages).
// Recursive functions need an explicit return type, the compiler can't infer it.
// Here it's Unit, which is analagous to a `void` return type in Java
def showNumbersInRange(a: Int, b: Int): Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}
showNumbersInRange(1, 14)


// Conditionals

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


/////////////////////////////////////////////////
// 4. Data Structures
/////////////////////////////////////////////////

val a = Array(1, 2, 3, 5, 8, 13)
a(0)     // Int = 1
a(3)     // Int = 5
a(21)    // Throws an exception

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")         // java.lang.String = tenedor
m("spoon")        // java.lang.String = cuchara
m("bottle")       // Throws an exception

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")   // java.lang.String = no lo se

val s = Set(1, 3, 7)
s(0)      // Boolean = false
s(1)      // Boolean = true

/* Look up the documentation of map here -
 * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map
 * and make sure you can read it
 */


// Tuples

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// Why have this?
val divideInts = (x: Int, y: Int) => (x / y, x % y)

// The function divideInts gives you the result and the remainder
divideInts(10, 3)    // (Int, Int) = (3,1)

// To access the elements of a tuple, use _._n where n is the 1-based index of
// the element
val d = divideInts(10, 3)    // (Int, Int) = (3,1)

d._1    // Int = 3
d._2    // Int = 1

// Alternatively you can do multiple-variable assignment to tuple, which is more
// convenient and readable in many cases
val (div, mod) = divideInts(10, 3)

div     // Int = 3
mod     // Int = 1


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

  // Abstract methods are simply methods with no body. If we uncomment the
  // def line below, class Dog would need to be declared abstract like so:
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.breed) // => "greyhound"
println(mydog.bark)  // => "Woof, woof!"


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
// otherGeorge == Person("George", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// And many others. Case classes also get pattern matching for free, see below.

// Traits
// Similar to Java interfaces, traits define an object type and method
// signatures. Scala allows partial implementation of those methods.
// Constructor parameters are not allowed. Traits can inherit from other
// traits or classes without parameters.

trait Dog {
	def breed: String
	def color: String
	def bark: Boolean = true
	def bite: Boolean
}
class SaintBernard extends Dog {
	val breed = "Saint Bernard"
	val color = "brown"
	def bite = false
}  

scala> b  
res0: SaintBernard = SaintBernard@3e57cd70  
scala> b.breed  
res1: String = Saint Bernard  
scala> b.bark  
res2: Boolean = true  
scala> b.bite  
res3: Boolean = false  

// A trait can also be used as Mixin. The class "extends" the first trait,
// but the keyword "with" can add additional traits.

trait Bark {
	def bark: String = "Woof"
}
trait Dog {
	def breed: String
	def color: String
}
class SaintBernard extends Dog with Bark {
	val breed = "Saint Bernard"
	val color = "brown"
}

scala> val b = new SaintBernard
b: SaintBernard = SaintBernard@7b69c6ba
scala> b.bark
res0: String = Woof


/////////////////////////////////////////////////
// 6. Pattern Matching
/////////////////////////////////////////////////

// Pattern matching is a powerful and commonly used feature in Scala. Here's how
// you pattern match a case class. NB: Unlike other languages, Scala cases do
// not need breaks, fall-through does not happen.

def matchPerson(person: Person): String = person match {
  // Then you specify the patterns:
  case Person("George", number) => "We found George! His number is " + number
  case Person("Kate", number)   => "We found Kate! Her number is " + number
  case Person(name, number)     => "We matched someone : " + name + ", phone : " + number
}

// Regular expressions are also built in.
// Create a regex with the `r` method on a string:
val email = "(.*)@(.*)".r

// Pattern matching might look familiar to the switch statements in the C family
// of languages, but this is much more powerful. In Scala, you can match much
// more:
def matchEverything(obj: Any): String = obj match {
  // You can match values:
  case "Hello world" => "Got the string Hello world"

  // You can match by type:
  case x: Double => "Got a Double: " + x

  // You can specify conditions:
  case x: Int if x > 10000 => "Got a pretty big number!"

  // You can match case classes as before:
  case Person(name, number) => s"Got contact info for $name!"

  // You can match regular expressions:
  case email(name, domain) => s"Got email address $name@$domain"

  // You can match tuples:
  case (a: Int, b: Double, c: String) => s"Got a tuple: $a, $b, $c"

  // You can match data structures:
  case List(1, b, c) => s"Got a list with three elements and starts with 1: 1, $b, $c"

  // You can nest patterns:
  case List(List((1, 2, "YAY"))) => "Got a list of list of tuple"

  // Match any case (default) if all previous haven't matched
  case _ => "Got unknown object"
}

// In fact, you can pattern match any object with an "unapply" method. This
// feature is so powerful that Scala lets you define whole functions as
// patterns:
val patternFunc: Person => String = {
  case Person("George", number) => s"George's number: $number"
  case Person(name, number) => s"Random person's number: $number"
}


/////////////////////////////////////////////////
// 7. Functional Programming
/////////////////////////////////////////////////

// Scala allows methods and functions to return, or take as parameters, other
// functions or methods.

val add10: Int => Int = _ + 10 // A function taking an Int and returning an Int
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 is applied to each element

// Anonymous functions can be used instead of named functions:
List(1, 2, 3) map (x => x + 10)

// And the underscore symbol, can be used if there is just one argument to the
// anonymous function. It gets bound as the variable
List(1, 2, 3) map (_ + 10)

// If the anonymous block AND the function you are applying both take one
// argument, you can even omit the underscore
List("Dom", "Bob", "Natalia") foreach println


// Combinators
// Using `s` from above:
// val s = Set(1, 3, 7)

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// The filter function takes a predicate (a function from A -> Boolean) and
// selects all elements which satisfy the predicate
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name: String, age: Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Certain collections (such as List) in Scala have a `foreach` method,
// which takes as an argument a type returning Unit - that is, a void method
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// For comprehensions

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* NB Those were not for loops. The semantics of a for loop is 'repeat', whereas
   a for-comprehension defines a relationship between two sets of data. */


/////////////////////////////////////////////////
// 8. Implicits
/////////////////////////////////////////////////

/* WARNING WARNING: Implicits are a set of powerful features of Scala, and
 * therefore it is easy to abuse them. Beginners to Scala should resist the
 * temptation to use them until they understand not only how they work, but also
 * best practices around them. We only include this section in the tutorial
 * because they are so commonplace in Scala libraries that it is impossible to
 * do anything meaningful without using a library that has implicits. This is
 * meant for you to understand and work with implicits, not declare your own.
 */

// Any value (vals, functions, objects, etc) can be declared to be implicit by
// using the, you guessed it, "implicit" keyword. Note we are using the Dog
// class from section 5 in these examples.
implicit val myImplicitInt = 100
implicit def myImplicitFunction(breed: String) = new Dog("Golden " + breed)

// By itself, implicit keyword doesn't change the behavior of the value, so
// above values can be used as usual.
myImplicitInt + 2                   // => 102
myImplicitFunction("Pitbull").breed // => "Golden Pitbull"

// The difference is that these values are now eligible to be used when another
// piece of code "needs" an implicit value. One such situation is implicit
// function arguments:
def sendGreetings(toWhom: String)(implicit howMany: Int) =
  s"Hello $toWhom, $howMany blessings to you and yours!"

// If we supply a value for "howMany", the function behaves as usual
sendGreetings("John")(1000)  // => "Hello John, 1000 blessings to you and yours!"

// But if we omit the implicit parameter, an implicit value of the same type is
// used, in this case, "myImplicitInt":
sendGreetings("Jane")  // => "Hello Jane, 100 blessings to you and yours!"

// Implicit function parameters enable us to simulate type classes in other
// functional languages. It is so often used that it gets its own shorthand. The
// following two lines mean the same thing:
// def foo[T](implicit c: C[T]) = ...
// def foo[T : C] = ...


// Another situation in which the compiler looks for an implicit is if you have
//   obj.method(...)
// but "obj" doesn't have "method" as a method. In this case, if there is an
// implicit conversion of type A => B, where A is the type of obj, and B has a
// method called "method", that conversion is applied. So having
// myImplicitFunction above in scope, we can say:
"Retriever".breed // => "Golden Retriever"
"Sheperd".bark    // => "Woof, woof!"

// Here the String is first converted to Dog using our function above, and then
// the appropriate method is called. This is an extremely powerful feature, but
// again, it is not to be used lightly. In fact, when you defined the implicit
// function above, your compiler should have given you a warning, that you
// shouldn't do this unless you really know what you're doing.


/////////////////////////////////////////////////
// 9. Misc
/////////////////////////////////////////////////

// Importing things
import scala.collection.immutable.List

// Import all "sub packages"
import scala.collection.immutable._

// Import multiple classes in one statement
import scala.collection.immutable.{List, Map}

// Rename an import using '=>'
import scala.collection.immutable.{List => ImmutableList}

// Import all classes, except some. The following excludes Map and Set:
import scala.collection.immutable.{Map => _, Set => _, _}

// Java classes can also be imported. Scala syntax can be used
import java.swing.{JFrame, JWindow}

// Your programs entry point is defined in a scala file using an object, with a
// single method, main:
object Application {
  def main(args: Array[String]): Unit = {
    // stuff goes here.
  }
}

// Files can contain multiple classes and objects. Compile with scalac




// Input and output

// To read a file line by line
import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)

// To write a file use Java's PrintWriter
val writer = new PrintWriter("myfile.txt")
writer.write("Writing line for line" + util.Properties.lineSeparator)
writer.write("Another line here" + util.Properties.lineSeparator)
writer.close()

```

## Further resources

* [Scala for the impatient](http://horstmann.com/scala/)
* [Twitter Scala school](http://twitter.github.io/scala_school/)
* [The scala documentation](http://docs.scala-lang.org/)
* [Try Scala in your browser](http://scalatutorials.com/tour/)
* Join the [Scala user group](https://groups.google.com/forum/#!forum/scala-user)
