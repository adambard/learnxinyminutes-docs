---

language: kotlin
contributors:
  - ["Koosha Hosseiny", "http://koosha.cc/"]
filename: LearnKotlin.kt

---

Kotlin is a staticaly-typed programming language that runs on the Java Virtual 
Machine and also can be compiled to JavaScript source code.
[Read more here.](http://kotlinlang.org/docs/reference/faq.html)

##TODO
  - Traits, implementing them, states.
  - Class properties, getters, setters, default values.
  - Companion objects, factory method convention in kotlin
  - Object Expressions
  - Extension functions
  - Generics
  - Nested classes
  - Enums
  - Delegations
  - Lazy property


```kotlin
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

fun kotlinBasics() {

  // ---- Literals ----
 
  // println is available for printing lines. No additional imports required.
  println("Decimal:\t" + 1000
          + "\nHex 0x100:\t" + 0x100
          + "\nLong value, denoted with capital L 100L:\t" + 100L
          + "\nBinary 0b00000100:\t" + 0b00000100
          + "\nOctal literal not supported!"
          + "\nFloating-point, Double by default 123.5:\t" + 123.5
          + "\nFloating-point, Double by default 123.5e10:\t" + 123.5e10
          + "\nFloating-point, as Float tagged with F 123.5f:\t" + 123.5f
  )

  // String literal, denoted by double quotes. Strings are iterable
  for (c in "Hello World!\n")
      println(c)

  // \n is an escaped character that starts a new line, \t adds a tab.
  println("Printing on a new line?\nNo Problem!"
          + "\nDo you want to add a tab?\tNo  Problem!")
  
  // Boolean
  println("Boolean operation (true && false):\t" + (true && false))

  // Ranges
  for (i in 1..100)
    println(i)
  
  
  // ---- Variables Definition ----
  
  // Local read-only variable defined with `val` keyword. 
  // General variable defination syntax is like:
  // val ARBITRARY_NAME : <TYPE> = <INIT_VALUE>
  // If INIT_VALUE is present, Type may be omitted.
  val variA: Int = 1
  val variB = 1
  val variC: Int // Type required when no initializer is provided
  variC = 1
  
  // The following line generates compiler error
  // variA += 1

  // Use `var` keyword to define local mutable variables. 
  // If its type is omitted, it will be computed for you.
  var someVar = 5
  someVar += 1

  
  // ---- Data Types ----
  
  val doubleValue: Double = 123.4   // 64 bits
  val foatValue:   Float  = 123.4f  // 32 bits
  val longValue:   Long   = 123L   // 64 bits
  val intValue:    Int    = 123    // 32 bits
  val shortValue:  Short  = 123    // 16 bits
  val byteValue:   Byte   = 123     // 08 bits
  
  
  // ---- Numbers ----

  // Smaller types are NOT implicitly widened.
  val numberA: Byte = 1               // OK, literals are checked statically
  // val numberB: Int = b             // ERROR!!!
  val numberB: Int = numberA.toInt()  // OK: explicitly widened
  
  
  // Bitwise operations are available as infix functions (for Int and Long only)
  // shl(bits) – signed shift left (Java’s <<)
  // shr(bits) – signed shift right (Java’s >>)
  // ushr(bits) – unsigned shift right (Java’s >>>)
  // and(bits) – bitwise and
  // or(bits) – bitwise or
  // xor(bits) – bitwise xor
  // inv() – bitwise inversion
  val x = 1 shl 2
  
  
  // ---- Characters ----
  
  // Character literals go in single quotes.
  val aChar: Char = 'c'
 
  // Characters may not be treated as numbers 
  // c == 1 // ERROR
  val charArrayDemo = charArray('1', '\n', '\uFF00')
  val digit = '8'
  
  // A char's numeric value is obtainedc with an explicit call to toInt()
  println('a'.toInt())
  
  
  // ---- Arrays ----

  // Arrays are represented by the `Array` class
  // [more about array class](http://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-array/index.html)
  val sampleArray: IntArray = intArray(1, 2, 3)
  
  // arrays have size(), get(), set(), iterator() by default.
  println(
          "array size():\t" + sampleArray.size()
          + "\nget(0):\t" + sampleArray.get(0)
  )
  
  // The following translates to x.set(0, x.get(1) + x.get(2)) by kotlin
  sampleArray[0] = sampleArray[1] + sampleArray[2]
 
  // Built-in array utility functions for primitive types, without boxing 
  // overhead.
  val sampleInts = intArray(1, 2, 3)
  val sampleChars = charArray('a', '\t', '\uFF00')
  // and so on.
  
  
  // ---- Various other stuff ----
  
  // Templates in strings. i.e. pieces of code that are evaluated and whose
  // results are concatenated into the string.
  val i = 10
  val strDemo = "abc"
  println("i = $i") // evaluates to "i = 10"
  println("$strDemo.length is ${strDemo.length}") // "abc.length is 3"

  // Read-only list.
  val list = listOf("a", "b", "c")
  for (i in list)
    println(i)

  // Read-only map
  val map = mapOf("a" to 1, "b" to 2, "c" to 3)
  for (i in map.keySet())
    println(map[i])
  
}

// _________________________________________________________________ FUNCTIONS

// A function with no args,
fun funDemo(): Int {
  return 42
}

// Function with both parameter and return type `Int`. b has default value 42
fun funDemoSum(a: Int, b: Int = 42): Int {
  return a + b
}

fun funDemoMax(a: Int, b: Int) = if (a > b) a else b

// Returning no meaningful value. Unit keyword can be omitted.
fun funDemoPrintSum0(a: Int, b: Int): Unit {
  print(a + b)
}

/**
 * Prefixing a function name with a `receiver type`, will add the function as
 * an extension to that type. the following declaration adds `swap` to the type
 * `MutableList<T>` (a generic type) which otherwise does not exist.
 * The this keyword inside an extension function corresponds to the receiver 
 * object (the one that is passed before the dot). 
 *
 * @TODO
 * [more about extensions](http://kotlinlang.org/docs/reference/extensions.html)
 */
 fun <T> MutableList<T>.swap(x: Int, y: Int) {
   val tmp = this[x] // 'this' corresponds to the list
   this[x] = this[y]
   this[y] = tmp
 }
 
 extensionFunSwapDemo() {
   val l0 = mutableListOf(1, 2, 3)
   val l1 = mutableListOf("a", "bb", "ccc")
   l0.swap(0, 2)
   l1.swap(1, 2)
 }

 // Extensions are resolved statically
 class C {
     fun foo() { println("member") }
 }
 
 fun C.foo() { println("extension") }
 fun extensionResolvingDemo() {
   C().foo() // prints "member", not "extension"
 }
// ______________________________________________________________ CONDITIONALS

// Testing if value is in range, using `in` operator.
fun conditionalRange(x: Int, y: Int) {
  if (x in 1..y-1) // If number is in range
    print("OK")
    
  val digit = '8'
  if (digit in '0'..'9') // If character is in range
    println("$digit is a valid digit.")
    
  if (x !in 0..100) // `not in` operator, !in
    println("x is not in range 0..100")
   
}

fun conditionalIf(x: Int): Int {
  // if branches can be blocks, and the last expression is the value of a block
  val a = 9
  val b = 2
  val max = if (a > b) { 
    print("Choose a") 
    a 
  } 
  else { 
    print("Choose b") 
    b 
  }
  
  // if as an expression 
  val maxFromExpression = if (a > b) a else b
  
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

fun conditionalInstanceCheck(x: Any) {
  when (x) {
    is Char -> println("$x is a character")
    is Int -> println($x is an integer")
    else -> println("$x is neither integer nor a character")
  }
}

// _____________________________________________________________________ LOOPS

fun loopDemo(args: Array<String>) {
  println("for-each")
  for (arg in args)
    print(arg)

  println("for i")
  for (i in args.indices)
    print(args[i])
    
  println("for i in range")
  for (i in 1..10)
    print(i)
  

  println("while")
  var i = 0
  while (i < args.size())
    print(args[i++])
    
  println("do while")
  i = args.size() - 1
  do {
    print(args[i])
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

// _________________________________________________________ RETURNS AND JUMPS

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
        return obj.length()
    }
}

// Type checking and automatic casting, version2
fun nullChecking1(obj: Any) {
    if (obj !is String) {
        println("Not an string,")
    }
    // Automatic cast to `String`! we may use obj.length now.
    else
        println(obj.length())
}

// Automatic cast for right-hand side of `&&`
fun nullChecking2(obj: Any) {
    if (obj is String && obj.length() > 0)
        println(obj.length())
}

// *If not null* shorthand with else
fun nullChecking4(obj: Any?) {
  // @TODO fix
  //println(obj?.size ?: "empty")
}

// @TODO: fixit
// The block inside let is executed only of obj is not null. No casting.
/*fun nullChecking5(obj: Any): Int? {
  object?.let {
    return 42
  }
}*/

// _______________________________________________________________ COLLECTIONS

fun collections() {
  val names = listOf("foo", "bar", "baz")
  
  // Iterating over collection items.
  for (name in names)
    println(name)

  // names.contains(text) is called automatically.
  if ("foo" in names) 
    print("Yes")
   
  // Traversing a map/list of pairs.
  for ((k, v) in map) {
     println("$k -> $v")
  }
}

fun collectionFiltering() {
  val list = listOf(1, 2, 55, 101, -101, -1098, 42)
  
  // Two methods to filter list with a condition.
  val positives0 = list.filter { x -> x > 0 }
  val positives1 = list.filter { it > 0 }
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
    // logger.info("class initialized with value ${firstName}")
  }
  
  // firstName from primary constructor available in initializer.
  val myName = firstName.toUpperCase()
  
  // @TODO more on properties, getters and setters, default values.
  
  // Secondary constructors are prefixed with *constructor*.
  // Calling other constructors with *this* keyword.
  constructor(firstName: String, lastName: String) : this(firstName) {
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
class Seller private (name: String) { 
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
  open fun v() {}
  fun nv() {}
}

// Explicit super type declared after colon. If super primary constructor has 
// args, it must be called right here.
class Derived(p: Int) : Base(p) {
  override fun v() {} // The override annotation is required!
}

// Multiple Inheritance is allowed, but if multiple implementations are 
// available to inherit, sub-class must override that member and provide it's 
// own implementation (and perhaps explicitly use one of the inherited ones).

open class InheritanceDemoA {
  open fun f() { print("Inheritance Demo A") }
  fun a() { print("a") }
}

trait InheritanceDemoB {
  fun f() { print("Inheritance Demo B") } // trait members are 'open' by default
  fun b() { print("b") }
}

class InheritanceDemoC() : InheritanceDemoA(), InheritanceDemoB {
  // The compiler requires f() to be overridden:
  override fun f() {
    super<A>.f() // call to A.f()
    super<B>.f() // call to B.f()
    super<A>.f() // call to A.f() again
  }
}

// b is markes as abstract so if its name clashes while inheriting from 
// multiple super types, there is no need to override it as it has no 
// implementation of its own
abstract class InheritanceDemoC {
  abstract fun b()
}

