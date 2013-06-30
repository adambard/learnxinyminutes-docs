---
language: scala
author: Dominic Bou-Samra
author_url: http://dbousamra.github.com
filename: learnscala.scala
---

Scala is a <insert something nice here>

```scala

///////////////////////////////////////
// Basic syntax
///////////////////////////////////////

//  Single line comments start with two forward slashes
/* 
Multi line comments look like this.
*/

// Import packages
import scala.collection.immutable.List
// Import all "sub packages"
import scala.collection.immutable._
// Import multiple classes in one statement
import scala.collection.immutable.{List, Map}
// Rename an import using '=>'
import scala.collection.immutable{ List => ImmutableList }
// Import all classes, except some. The following excludes Map and Set:
import scala.collection.immutable.{Map => _, Set => _, _}

// Your programs entry point is defined in an scala file using an object, with a single method, main:
object Application {
  def main(args: Array[String]): Unit = {
    // stuff goes here.
  }
}

// Printing, and forcing a new line on the next print
println("Hello world!")
// Printing, without forcing a new line on next print
print("Hello world")

// Declaring values is done using either var or val
// val declarations are immutable, whereas var's are mutable. Immutablility is a good thing.
val x = 10 // x is now 10
x = 20 // error: reassignment to val
var x = 10 
x = 20  // x is now 20

///////////////////////////////////////
// Types
///////////////////////////////////////

// Almost all types are objects.

// You have numbers
3 //3

// Math is as per usual
1 + 1 // 2
2 - 1 // 1
5 * 3 // 15
6 / 2 // 3

// Boolean values
true
false

// Boolean operations
!true // false
!false // true
true == false // false
10 > 5 // true

// Strings and characters
"Scala strings are surrounded by double quotes" //
'a' // A Scala Char
'Single quote strings don't exist' // Error
"Strings have the usual Java methods defined on them".length
"They also have some extra Scala methods.".reverse // See scala.collection.immutable.StringOps

///////////////////////////////////////
// Basic control constructs
///////////////////////////////////////

// if statements (else statements are optional)
if (10 > 5) println("10 is greater than 5")
// an else
if (x > 5) println("x is greater than 5")
else println("No it's not.")

// Iteration

// A while loop
while (x < 10) {
  println("x is still less then 10")
  x += 1
}

// A do while loop
do { 
  println("x is still less then 10"); 
  x += 1
} while (x < 10)

// A for loop
for (x <- 0 until 10) {
  println(x)
}

// Any object implementing the map/filter/flatMap methods allows the use of a for loop:
val aListOfNumbers: List[Int] = List(1, 2, 3)
for (x <- aListOfNumbers) {
  println(x)
}

// Pattern matching (see respective section)
x match {
  case 5 => println("x is 5")
  case 10 => println("x is 10")
  case _ => println("default case")
}

///////////////////////////////////////
// Functions, methods and classes
///////////////////////////////////////

// Scala has classes

// classname is Dog
class Dog {
  //A method called bark, returning a String
  def bark: String = {
    // the body of the method
    "Woof, woof!"
  }
}

// They can contain nearly any other construct, including other classes, functions, methods, objects, case classes, traits etc.

///////////////////////////////////////
// Higher-order functions
///////////////////////////////////////

// Scala allows methods and functions to return, or take as parameters, other functions or methods.

val add10: Int => Int = _ + 10 // A function taking an Int and returning an Int
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 is applied to each element

// Anonymous functions can be used instead of named functions:
List(1, 2, 3) map (x => x + 10)

// And the underscore symbol, can be used if there is just one argument to the anonymous function. It gets bound as the variable
List(1, 2, 3) map (_ + 10)

TODO // If the anonymous block AND the function you are applying both take one argument, you can even omit the underscore
List("Dom", "Bob", "Natalia") foreach println


// Scala collections have rich higher-order functions defined on them. Some examples:

// The map function takes a function/method, and applies it to each element in the structure
List(1, 2, 3) map (number => number.toString)

// The filter function takes a predicate (a function from A -> Boolean) and selects all elements which satisfy the predicate
List(1, 2, 3) filter (_ > 2) // List(3)
List(
  Person(name = "Dom", age = 23), 
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// Scala a foreach method defined on certain collections that takes a type returning Unit (a void method)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println


