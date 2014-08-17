---
language: Scala
filename: learnscala.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
filename: learn.scala
---

Scala - the scalable language

```cpp

/*
  Set yourself up:

  1) Download Scala - http://www.scala-lang.org/downloads
  2) unzip/untar in your favourite location and put the bin subdir on the path
  3) Start a scala REPL by typing scala. You should see the prompt:

  scala>

  This is the so called REPL. You can run commands in the REPL. Let's do just
  that:
*/

println(10) // prints the integer 10

println("Boo!") // printlns the string Boo!


// Some basics

// Printing, and forcing a new line on the next print
println("Hello world!")
// Printing, without forcing a new line on next print
print("Hello world")

// Declaring values is done using either var or val
// val declarations are immutable, whereas var's are mutable. Immutability is 
// a good thing.
val x = 10 // x is now 10
x = 20 // error: reassignment to val
var x = 10 
x = 20  // x is now 20

//  Single line comments start with two forward slashes
/* 
Multi line comments look like this.
*/

// Boolean values
true
false

// Boolean operations
!true // false
!false // true
true == false // false
10 > 5 // true

// Math is as per usual
1 + 1 // 2
2 - 1 // 1
5 * 3 // 15
6 / 2 // 3


// Evaluating a command in the REPL gives you the type and value of the result

1 + 7

/* The above line results in:

  scala> 1 + 7
  res29: Int = 8

  This means the result of evaluating 1 + 7 is an object of type Int with a
  value of 8

  1+7 will give you the same result
*/


// Everything is an object, including a function. Type these in the REPL:

7 // results in res30: Int = 7 (res30 is just a generated var name for the result)

// The next line gives you a function that takes an Int and returns it squared
(x:Int) => x * x    

// You can assign this function to an identifier, like this:
val sq = (x:Int) => x * x

/* The above says this
   
   sq: Int => Int = <function1>	

   Which means that this time we gave an explicit name to the value - sq is a
   function that take an Int and returns Int.

   sq can be executed as follows:
*/

sq(10)   // Gives you this: res33: Int = 100.

// The colon explicitly defines the type of a value, in this case a function
// taking an Int and returning an Int. 
val add10: Int => Int = _ + 10 

// Scala allows methods and functions to return, or take as parameters, other
// functions or methods.

List(1, 2, 3) map add10 // List(11, 12, 13) - add10 is applied to each element

// Anonymous functions can be used instead of named functions:
List(1, 2, 3) map (x => x + 10)

// And the underscore symbol, can be used if there is just one argument to the
// anonymous function. It gets bound as the variable
List(1, 2, 3) map (_ + 10)

// If the anonymous block AND the function you are applying both take one
// argument, you can even omit the underscore
List("Dom", "Bob", "Natalia") foreach println



// Data structures

val a = Array(1, 2, 3, 5, 8, 13)
a(0)
a(3)
a(21)    // Throws an exception

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")
m("spoon")
m("bottle")       // Throws an exception

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")

val s = Set(1, 3, 7)
s(0)
s(1)

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
val divideInts = (x:Int, y:Int) => (x / y, x % y)

divideInts(10,3) // The function divideInts gives you the result and the remainder

// To access the elements of a tuple, use _._n where n is the 1-based index of
// the element
val d = divideInts(10,3)

d._1

d._2



// Combinators

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// The filter function takes a predicate (a function from A -> Boolean) and
// selects all elements which satisfy the predicate
List(1, 2, 3) filter (_ > 2) // List(3)
List(
  Person(name = "Dom", age = 23), 
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Scala a foreach method defined on certain collections that takes a type
// returning Unit (a void method)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println




// For comprehensions

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* NB Those were not for loops. The semantics of a for loop is 'repeat', whereas
   a for-comprehension defines a relationship between two sets of data. */



// Loops and iteration

1 to 5
val r = 1 to 5
r.foreach( println )

r foreach println     
// NB: Scala is quite lenient when it comes to dots and brackets - study the
// rules separately. This helps write DSLs and APIs that read like English

(5 to 1 by -1) foreach ( println )

// A while loops
var i = 0
while (i < 10) {  println("i " + i); i+=1  }

while (i < 10) {  println("i " + i); i+=1  }   // Yes, again. What happened? Why?

i    // Show the value of i. Note that while is a loop in the classical sense -
     // it executes sequentially while changing the loop variable. while is very
     // fast, faster that Java // loops, but using the combinators and
     // comprehensions above is easier to understand and parallelize

// A do while loop
do {
  println("x is still less then 10"); 
  x += 1
} while (x < 10)

// Tail recursion is an idiomatic way of doing recurring things in Scala.
// Recursive functions need an explicit return type, the compiler can't infer it.
// Here it's Unit.
def showNumbersInRange(a:Int, b:Int):Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}



// Conditionals

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println ("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"

var i = 0
while (i < 10) { println("i " + i); i+=1  }



// Object oriented features

// Classname is Dog
class Dog {
  //A method called bark, returning a String
  def bark: String = {
    // the body of the method
    "Woof, woof!"
  }
}

// Classes can contain nearly any other construct, including other classes,
// functions, methods, objects, case classes, traits etc.



// Case classes

case class Person(name:String, phoneNumber:String)

Person("George", "1234") == Person("Kate", "1236")



// Pattern matching

val me = Person("George", "1234")

me match { case Person(name, number) => {
            "We matched someone : " + name + ", phone : " + number }}

me match { case Person(name, number) => "Match : " + name; case _ => "Hm..." }

me match { case Person("George", number) => "Match"; case _ => "Hm..." }

me match { case Person("Kate", number) => "Match"; case _ => "Hm..." }

me match { case Person("Kate", _) => "Girl"; case Person("George", _) => "Boy" }

val kate = Person("Kate", "1234")

kate match { case Person("Kate", _) => "Girl"; case Person("George", _) => "Boy" }



// Regular expressions
val email = "(.*)@(.*)".r  // Invoking r on String makes it a Regex
val serialKey = """(\d{5})-(\d{5})-(\d{5})-(\d{5})""".r // Using verbatim (multiline) syntax

val matcher = (value: String) => {
  println(value match {
	case email(name, domain) => s"It was an email: $name"
	case serialKey(p1, p2, p3, p4) => s"Serial key: $p1, $p2, $p3, $p4"
	case _ => s"No match on '$value'" // default if no match found
  })
}

matcher("mrbean@pyahoo.com") // => "It was an email: mrbean"
matcher("nope..") // => "No match on 'nope..'"
matcher("52917") // => "No match on '52917'"
matcher("52752-16432-22178-47917") // => "Serial key: 52752, 16432, 22178, 47917"


// Strings

"Scala strings are surrounded by double quotes" //
'a' // A Scala Char
'Single quote strings don't exist' // Error
"Strings have the usual Java methods defined on them".length
"They also have some extra Scala methods.".reverse

// Seealso:  scala.collection.immutable.StringOps

println("ABCDEF".length)
println("ABCDEF".substring(2, 6))
println("ABCDEF".replace("C", "3"))

// String interpolation
val n = 45
println(s"We have $n apples") // => "We have 45 apples"

// Expressions inside interpolated strings are also possible
val a = Array(11, 9, 6)
println(s"My second daughter is ${a(0) - a(2)} years old.") // => "My second daughter is 5 years old."
println(s"We have double the amount of ${n / 2.0} in apples.") // => "We have double the amount of 22.5 in apples."
println(s"Power of 2: ${math.pow(2, 2)}") // => "Power of 2: 4"

// Formatting with interpolated strings (note the prefixed f)
println(f"Power of 5: ${math.pow(5, 2)}%1.0f") // "Power of 5: 25"
println(f"Square root of 122: ${math.sqrt(122)}%1.4f") // "Square root of 122"

// Ignoring special characters.
println(raw"New line feed: \n. Carriage return: \r.") // => "New line feed: \n. Carriage return: \r."

// Some characters need to be 'escaped', e.g. a double quote inside a string:
val a = "They stood outside the \"Rose and Crown\"" // => "They stood outside the "Rose and Crown""

// Triple double-quotes let strings span multiple rows and contain quotes
val html = """<form id="daform">
                <p>Press belo', Joe</p>
             |  <input type="submit">
              </form>"""



// Application structure and organization

// Importing things
import scala.collection.immutable.List

// Import all "sub packages"
import scala.collection.immutable._

// Import multiple classes in one statement
import scala.collection.immutable.{List, Map}

// Rename an import using '=>'
import scala.collection.immutable.{ List => ImmutableList }

// Import all classes, except some. The following excludes Map and Set:
import scala.collection.immutable.{Map => _, Set => _, _}

// Your programs entry point is defined in an scala file using an object, with a
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

[Scala for the impatient](http://horstmann.com/scala/)

[Twitter Scala school](http://twitter.github.io/scala_school/)

[The scala documentation](http://docs.scala-lang.org/)

[Try Scala in your browser](http://scalatutorials.com/tour/)

Join the [Scala user group](https://groups.google.com/forum/#!forum/scala-user)

