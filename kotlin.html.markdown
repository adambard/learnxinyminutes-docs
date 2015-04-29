---

language: kotlin
contributors:
  - ["Koosha Hosseiny", "http://koosha.cc/"]
filename: LearnKotlin.kt

---

Kotlin is a staticaly-typed programming language that runs on the Java Virtual 
Machine and also can be compiled to JavaScript source code.
[Read more here.](http://kotlinlang.org/docs/reference/faq.html)

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
  var x = 5
  x += 1

  
  // ---- Data Types ----
  
  val doubleValue: Double = 123.4   // 64 bits
  val foatValue:   Float  = 123.4f  // 32 bits
  val longValue:   Long   = 1234L   // 64 bits
  val intValue:    Int    = 1234    // 32 bits
  val shortValue:  Short  = 1234    // 16 bits
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
  val charArrayDemo = CharArray('1', '\n', '\uFF00')
  val digit = '8'
  
  // Checking character range
  if (digit in '0'..'9')
    println("$digit is a valid digit.")
   
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

// A function with no args, and return type not specified. 
// Return type is computed by kotlin, however such a function is not visible 
// outside the module
fun funDemo() {
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

// ______________________________________________________________ CONDITIONALS

// Testing if value is in range
fun conditionalRange(x: Int, y: Int) {
  if (x in 1..y-1)
    print("OK")
}

fun conditionalIf(x: Int) {
  // if branches can be blocks, and the last expression is the value of a block
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
  constructor(val firstName: String, val lastName) this(firstName) {
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

