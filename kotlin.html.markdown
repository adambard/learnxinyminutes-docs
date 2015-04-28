---

language: kotlin
contributors:

filename: LearnKotlin.py

---

Kotlin is a staticaly-typed programming language that runs on the Java Virtual 
Machine and also can be compiled to JavaScript source code.
[Read more here.](http://kotlinlang.org/docs/reference/faq.html)

```java

// Single-line comments start with //

/*
Multi-line comments look like this.
*/

// Package specification should be at the top of the source file, but need not
// match the directory structure.
package my.demo

// Importing packages from java
import java.util.ArrayList
import java.security.*

// import foo.Bar // Bar is accessible
// import bar.Bar as bBar // bBar stands for 'bar.Bar'

// _____________________________________________________________________ BASICS

fun kotlinBasics(name: String) {

  // ---- Literals ----
  
  println("Decimal:\t" + 1000
          + "\nHex 0x100:\t" + 0x100
          + "\nLong value, denoted with capital L 100L:\t" + 100L
          + "\nBinary 0b00000100:\t" + 0b00000100
          + "\nOctal literal not supported!"
          + "\nFloating-point, Double by default 123.5:\t" + 123.5
          + "\nFloating-point, Double by default 123.5e10:\t" + 123.5e10
          + "\nFloat tagged with F 123.5f:\t" + 123.5f
  )

  // String literal, denoted by double quotes. Strings are iterable
  for (c in "Hello, world!\n")
      println(c)

  // Characters are not numbers, the following produces error:
  // val c: Char = 'c'
  // c == 1 // ERROR

  // Boolean
  val b0: Boolean = true
  val b1: Boolean = false
  println(
          "Boolean And (true && false):\t" + (b0 && b1)
          + "\nBoolean Or (true || false):\t" + (b0 || b1)
  )

  // Arrays
  val x: IntArray = intArray(1, 2, 3)
  // Translates to x.set(0, x.get(1) + x.get(2))
  x[0] = x[1] + x[2]
  // arrays have size(), get(), set(), iterator() by default.
  // [more about array class](http://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-array/index.html)
  println(
          "array size():\t" + x.size()
          + "\nget(0):\t" + x.get(0)
  )

  // Templates in strings. i.e. pieces of code that are evaluated and whose
  // results are concatenated into the string.
  val i = 10
  val sDemo = "abc"
  println("i = $i") // evaluates to "i = 10"
  println("$sDemo.length is ${sDemo.length}") // "abc.length is 3"


  // ---- Variables Definition ----

  
  // Local read-only variable.
  val a: Int = 1
  val b = 1
  val c: Int // Type required when no initializer is provided
  c = 1
  
  // The following line generates compiler error
  // a += 1

  // Local mutable value, its type is computed.
  var x = 5
  x += 1

  // Ranges
  for (i in 1..100) { 
    println(i)
  }
  
  // Read-only list.
  val list = listOf("a", "b", "c")
  println(list[0])

  // Read-only map
  val map = mapOf("a" to 1, "b" to 2, "c" to 3)
  println(map["s"])
}

// _________________________________________________________________ FUNCTIONS

// A function with no args, and return type not specified.
fun funDemo() {
  return 42
}

// Function with both parameter and return type Int.
fun funDemoSum(a: Int, b: Int): Int {
  return a + b
}

// Return type automatically inferred. But such a fuction is not visible 
// outside the module.
fun funDemoMax(a: Int, b: Int) = if (a > b) a else b

// Returning no meaningful value, Unit keyword can be omitted.
fun funDemoPrintSum0(a: Int, b: Int): Unit {
  print(a + b)
}
fun funDemoPrintSum1(a: Int, b: Int) {
  print(a + b)
}

// Default value
fun funDemoDefaultVal(a: Int = 0, b: String = "") { 
  println(a)
  println(b)
}

// ______________________________________________________________ CONDITIONALS
// Testing if value is in range
fun conditionalRange(x: Int, y: Int) {
  if (x in 1..y-1)
    print("OK")
}

fun conditionalIf(x: Int) {
  // if branches can be blocks, and the last expression is the value of a block:
  a = 9
  b = 2
  val max = if (a > b) { 
    print("Choose a") 
    a 
  } 
  else { 
    print("Choose b") 
    b 
  }
  
  if (x > 0)
    return x
  else
    return 0
}

fun conditionalWhen(obj: Any) {
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
    
  print("do while")
  i = args.size() - 1
  do {
    println(args[i])
  } while(i >= 0)
  
  // Break and continue are supported
  var loopCount = 0
  for (j in 0..20)
    if (j.mod(2) == 0)
      continue
    else if(j == 11)
      break
    else
      loopCount++
  println("From 0 to 20, skipping even numbers and breaking at 11," +
      " the loop counted $loopCount times.")
  
}

// _________________________________________________________ RETURNS AND jUMPS

/**
 * Any expression in Kotlin may be marked with a label. Labels have the form 
 * of the @ sign followed by an identifier, for example: @abc, @fooBar
 */
fun breakWitLabels() {
  @loop for (i in 1..100) {
    for (j in 1..100) {
      if ((i % j) == 0)
        // jumps to the execution point right after the loop marked with the 
        // label. A continue would instead proceed to the next iteration of 
        // the loop
        break@loop
    }
  }
  
}

/**
 * @TODO [Return at Labels](http://kotlinlang.org/docs/reference/returns.html)
 */

// ____________________________________ NULL SAFETY AND AUTOMATIC TYPE CASTING

// References are non-null by default. If otherwise, they must be marked.

// Type checking and automatic casting. Notice the `Any` type.
fun nullChecking0(obj: Any): Int? { // Because of `?`, null may be returned.
  if (obj is String) {
    // `obj` is automatically cast to `String` in this branch
    return obj.length
  }
  
  // else returns null
}

// Type checking and automatic casting, version2
fun nullChecking1(obj: Any): Int? {
  if (obj !is String)
    return null
    
  // Automatic cast to `String`! we may use obj.length now.
  return obj.length
}

// Automatic cast for right-hand side of `&&`
fun nullChecking2(obj: Any): Int? {
  if (obj is String && obj.length > 0)
    return obj.length
}

// *If not null* shorthand 
fun nullChecking3(obj: Any?): Int? {
  println(obj?.length)
}

// *If not null* shorthand with else
fun nullChecking4(obj: Any?): Int? {
  println(obj?.size ?: "empty")
}

// The block inside let is executed only of obj is not null. No casting.
fun nullChecking5(obj: Any): Int? {
  object?.let {
    return 42
  }
}

// _______________________________________________________________ COLLECTIONS

fun collectionIter() {
  names = listOf("foo", "bar", "baz")
  
  // Iterating over collection items.
  for (name in names)
    println(name)

  // Checking if collection contains an item.
  // names.contains(text) is called automatically.
  if (text in names) 
    print("Yes")

  // Function literals.
  names filter { it.startsWith("A") } 
    sortBy { it } 
    map { it.toUpperCase() } 
    forEach { print(it) }
}

// ___________________________________________________________________ CLASSES

/**
 * General class declaration syntax
 *
 * Note that *EVERYTHING* except class keyword and class name is optional!
 *
 * // @TODO other than private?
 *
 * class <NAME> [private] (PrimaryConstructorArgs) : someSuperType(ArgsIfAny) {
 *   // init blocks, like so:
 *   // secondary constructors.
 *   // functions
 *   // properties
 *   // Nested and Inner Classes
 *   // Object Declarations
 * }
 *
 * Classes with no super type implicitly inherit from `Any`, but `Any` is not
 * same as `java.lang.object`.
 *
 * [More about classes](http://kotlinlang.org/docs/reference/classes.html)
 */

// An empty class, no curly braces needed.
class Empty

/**
 * A class with one primary constructors and more secondary constructors. A 
 * primary constructor is defined inline.
 * Both primary and secondary constructors are optional, but secondaries must 
 * directly or indirectly (through other constructors) call the primary.
 */
public class LearnKotlin(firstName: String) {

  // The code for primary constructor should be defined in init block. If it
  // has any parameters, they will be available.
  init {
    logger.info("class initialized with value ${firstName}")
  }
  
  // firstName from primary constructor available in initializer.
  val myName = firstName.toUpperCase()
  
  // Secondary constructors are prefixed with *constructor*.
  // Calling other constructors with *this* keyword.
  class constructor(val firstName: String, val lastName) this(firstName) {
    // ...
  }
}

/**
 * DTO's (POJO’s/POCO’s) or Data classes.
 * `equals()`, `hashCode()`, `toString()`, `copy()`, `component1()`
 * `component2()`, ... are all provided automatically by Kotlin. 
 * [more about data classes](http://kotlinlang.org/docs/reference/data-classes.html)
 */
data class Customer(val name: String, val email: String)

// Singleton
object Resource {
    val name = "Name"
}

// Private primary constructor
class Customer private (name: String) { 
  //... 
}


/**
 * To create an instance of a class, we call the constructor as if it were a 
 * regular function:
 */
fun classInstantiationDemo() {
  val cus = Customer("their name", "someone@somewhere.tld")
  var learner = LearnKotlin("A nice person")
}

// _________________________________________________________________INHERITANCE

/**
 * By default classes are `final`, meaning they can not be inherited from.
 * `open` keyword is the opposite of `final`, which allows inheritance.
 */
open class Base(p: Int) {
  open fun v()
  fun nv() {}
}

// Explicit super declared after colon. If super primary constructor has args, 
// it must be called right here.
class Derived(p: Int) : Base(p) {
  override fun v() {} // The override annotation is required!
}

