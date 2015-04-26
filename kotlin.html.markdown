---

language: kotlin
filename: LearnKotlin.kotlin

---

Kotlin is a statically-typed programming language that runs on the Java Virtual 
Machine and also can be compiled to JavaScript source code.
[Read more here.](http://kotlinlang.org/docs/reference/faq.html)

```java
// Single-line comments start with //
/*
Multi-line comments look like this.
*/

// Package specification should be at the top of the source file, but need not
// to match the directory structure.
package my.demo

// Directly importing from java packages, a single class or all classes.
import java.util.ArrayList;
import java.security.*;


// ____________________________________________________________________ BASICS

fun basicsDemo(name: String) {
  // String interpolation.
  println("Name $name")

  // Ranges
  for (i in 1..100) { 
    println(i)
  }
  
  // Read-only list.
  val list = listOf("a", "b", "c")

  // Read-only map
  val map = mapOf("a" to 1, "b" to 2, "c" to 3)
  println(map["s"])

}

// _________________________________________________________________ FUNCTIONS

// A function with no args, and return type not specified.
fun funDemo() {

  // Local read-only variable.
  val a: Int = 1;
  val b = 1
  val c: Int // Type required when no initializer is provided
  c = 1

  // Local mutable value, it's type is inferred.
  var x = 5
  x += 1

  return x + a;
}

// Function with both parameter and return type Int.
fun sum(a: Int, b: Int): Int {
  return a + b
}

// Return type automatically inferred. But such fuction is not visible outside 
// the module.
fun max(a: Int, b: Int) = if (a > b) a else b

// Returning no meaningfull value, Unit keyword can be ommited.
fun printSum(a: Int, b: Int): Unit {
  print(a + b)
}
public fun printSum(a: Int, b: Int) {
  print(a + b)
}

// Default value
fun foo(a: Int = 0, b: String = "") { 
  // ... 
}

// ______________________________________________________________ CONDITIONALS

fun conditionalRange(x: Int) {
  if (x in 1..y-1)
    print("OK")
}

fun conditionalIf(x: Int) {
  if (x > 0)
    return x
  else
    return 0
}

fun  conditionalWhen(obj: Any) {
  when (obj) {
    1          -> print("One")
    "Hello"    -> print("Greeting")
    is Long    -> print("Long")
    !is String -> print("Not a string")
    else       -> print("Unknown")
  }
}

// _____________________________________________________________________ LOOPS

fun loopDemo(args: Array<String>) {
  print("for-each")
  for (arg in args)
    print(arg)

  print("for i")
  for (i in args.indices)
    print(args[i])

  print("while")
  var i = 0
  while (i < args.size())
    print(args[i++])
}

// ____________________________________ NULL SAFETY AND AUTOMATIC TYPE CASTING

// References are non-null by default and if otherwise, must be marked.
// Type checking and automatic casting. Notice the `Any` type.
fun getStringLength(obj: Any): Int? { // Because of `?`, null may be returned.
  if (obj is String) {
    // `obj` is automatically cast to `String` in this branch
    return obj.length
  }
  
  return;

  // ---------  OR

  if (obj !is String)
    return null

  // Automatic cast to `String`!   
  return obj.length

  // ---------  OR

  // Automatic cast for right-hand side of `&&`
  if (obj is String && obj.length > 0)
    return obj.length

  // If not null shorthand 
  println(object?.length)
  // With else.
  println(object?.size ?: "empty")

  object?.let {
    ... // execute this block if not null
  }

 
  return null
}

// _______________________________________________________________ COLLECTIONS

fun collectionIter() {
  // init names as collection.
  for (name in names)
    println(name)

  if (text in names) // names.contains(text) is called
    print("Yes")

  // Function literals.
  names filter { it.startsWith("A") } 
    sortBy { it } 
    map { it.toUpperCase() } 
    forEach { print(it) }
}

// ___________________________________________________________________ CLASSES

// An empty class, no curely braces needed.
class Empty

// A class with one primary constructors and more secondary constructors. A 
// primary constructor is defined inline.
// Both primary and secondary constructors are optional, but secondaries must 
// directly or indirectly (through other constructors) call the primary.
public class LearnKotlin(firstName: String) {

    // The code for primary constructor should be defined in init block. If it
    // has any parameters, they will be available.
    init {
      logger.info("class initialized with value ${firstName}");
    }

    // firstName from primary constructor available in initializer.
    val myName = firstName.toUpperCase();

    // Secondary constructors are prefixed with *constructor*.
    // Calling other constructors with *this* keyword.
    class Person(val firstName: String, val lastName) this(firstName) {
      // ...
    }
}

// DTO's (POJO’s/POCO’s) or Data classes.
// `equals()`, `hashCode()`, `toString()`, `copy()`, `component1()`
// `component2()`, ... are all provided automatically by Kotlin. 
// [more about data classes](http://kotlinlang.org/docs/reference/data-classes.html)
data class Customer(val name: String, val email: String)

// Singleton
object Resource {
    val name = "Name"
}

