---

language: kotlin
contributors:
  - ["Koosha Hosseiny", "http://koosha.cc/"]
filename: LearnKotlin.kt

---

Kotlin is a staticaly-typed programming language that runs on the Java Virtual
Machine and also can be compiled to JavaScript source code.
[Read more here.](http://kotlinlang.org/docs/reference/faq.html)

##TODO: to add
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

##TODO: making it better
  - remove unnecessary parts, so the whole thing is not too long to read in x
    minutes.

```kotlin

// _______________________________________________Getting Started: Basic Syntax

/**
 * Package specification should be at the top of the source file. It is not
 * required to match directories and packages.
 */
package my.demo

// Importing packages
import java.util.* // direct import from java
import kotlin.Annotation as kat // Annotation is accessible as `kat`

fun kotlinBasics() {

  // ---- Variables Definition ----

  // Local read-only variable defined with `val` keyword.
  // val ARBITRARY_NAME : <TYPE> = <INIT_VALUE>
  val variA: Int = 1
  val variB = 1
  val variC: Int // Type required when no initializer is provided
  variC = 1
  // variA += 1 // ERROR! variA is immutable.

  // Use `var` keyword to define local mutable variables.
  // If its type is omitted, it will be computed for you.
  var someVar = 5
  someVar += 1


  // ---- Data Types and Literals ----

  // println is available for printing lines. No additional imports required.
  println(  "Decimal:\t"                                         + 1000
          + "\nHex 0x100:\t"                                     + 0x100
          + "\nLong value, denoted with capital L 100L:\t"       + 100L
          + "\nBinary 0b00000100:\t"                             + 0b00000100
          + "\nOctal literal not supported!\t"                   + "N/A"
          + "\nFloating-point, Double by default 123.5:\t"       + 123.5
          + "\nFloating-point, Double by default 123.5e10:\t"    + 123.5e10
//          + "\nFloating-point, as Float tagged with F 123.5f:\t" + 123.5f
  )

  // String literal, denoted by double quotes. Strings are iterable. \n is an
  // escaped character
  for (c in "Hello World!\nHow are you?")
      println(c)

  // Boolean
  println("Boolean operation (true && false):\t" + (true && false))

  // Ranges
  for (i in 1..10)
    println(i)

  val doubleValue: Double = 123.4  // 64 bits
  val foatValue:   Float  = 123.4f // 32 bits
  val longValue:   Long   = 123L   // 64 bits
  val intValue:    Int    = 123    // 32 bits
  val shortValue:  Short  = 123    // 16 bits
  val byteValue:   Byte   = 123    // 08 bits

  // Smaller types are NOT implicitly widened.
  val numberA: Byte = 1               // OK, literals are checked statically
  // val numberB: Int = b             // ERROR!!!
  val numberB: Int = numberA.toInt()  // OK: explicitly widened

  // Bitwise operations, available as infix functions (for Int and Long only)
  println(  "\nsigned shift left (Java’s <<):\t"     + (1 shl 2)
          + "\nsigned shift right (Java’s >>):\t"    + (1 shr 2)
          + "\nunsigned shift right (Java’s >>>):\t" + (1 ushr 2)
          + "\nbitwise and:\t"                       + (1 and 2)
          + "\nbitwise or:\t"                        + (1 or 2)
          + "\nbitwise xor:\t"                       + (1 xor 2)
          + "\nbitwise inversion:\t"                 + inv(1)
  )

  // Character literals go in single quotes.
  val aChar: Char = 'c'

  // Characters may not be treated as numbers
  // c == 1 // ERROR
  val charArrayDemo = charArray('1', '\n', '\uFF00')
  val digit = '8'

  // A char's numeric value is obtained with an explicit call to toInt()
  println('a'.toInt())


  // ---- Arrays ----

  // Arrays are represented by the `Array` class, and have size(), get(), set()
  // and iterator() by default
  val sampleArray: IntArray = intArray(1, 2, 3)
  println(  "array size():\t" + sampleArray.size()
          + "\nget(0):\t" + sampleArray.get(0))

  // The following translates to x.set(0, x.get(1) + x.get(2)) by kotlin
  sampleArray[0] = sampleArray[1] + sampleArray[2]

  // Built-in array utility functions for primitive types, without boxing
  // overhead.
  val sampleInts = intArray(1, 2, 3)
  val sampleChars = charArray('a', '\t', '\uFF00')
  // and so on for other basic types.


  // ---- Various other stuff ----

  // Templates in strings.
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

  // Calling functions on literals
  println(1.toString())
}

// ___________________________________________________________________ Function

// Defining function having two Int parameters with Int return type
fun simpleSum(a: Int, b: Int): Int {
    return a + b
}

// Function visible from outside of a module should have return type explicitly
// specified:
public fun sumExpression(a: Int, b: Int): Int = a + b

// Function returning no meaningful value. `Unit` can be omitted.
fun printSum(a: Int, b: Int): Unit {
    print(a + b)
}

// Function body as an expression
fun maxExpression(a: Int, b: Int) = if (a > b) a else b

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
fun  extensionFunSwapDemo() {
   // @TODO
   // val l0 = mutableListOf(1, 2, 3)
   // val l1 = mutableListOf("a", "bb", "ccc")
   // l0.swap(0, 2)
   // l1.swap(1, 2)
 }

 // Extensions are resolved statically
 class C {
     fun foo() { println("member") }
 }
 fun C.foo() { println("extension") }
 fun extensionResolvingDemo() {
   C().foo() // prints "member", not "extension"
 }


// _____________________________________________________ Control Flow and Loops

fun mLoopDemo(args: Array<String>) {
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

fun mLoopDemoWithLabel(args: Array<String>) {
    // Any expression may be marked with a label. Labels have the form of the
    // @ sign followed by an identifier: @abc, @foo, @fooBar
    @loop for (i in 1..20)
        for (j in 1..20)
            if (i*j > 50) break@loop

    // println("i = $i, j = $j") // i == 3, j == 17

    // @TODO qualified returns.
}

fun max(a: Int, b: Int): Int {
    // If expression
    val max_from_expression = if (a > b) a else b

    // if branches as blocks. The last expression is the value of a block
    val max = if (a > b) {
        print("Choose a")
        a
    }
    else {
        print("Choose b")
        b
    }

    return max
}

// loops
fun mLoop(args: Array<String>) {
    for (arg in args)
        print(arg)

    for (i in args.indices)
        print(args[i])

    var i = 0
    while (i < args.size())
        print(args[i++])
}

// `when` is the `switch` equivalent in other languages.
fun cases(obj: Any) {
    // Note the `is Long` branch, if evalutes to true, then obj is automatically
    // cast to Long, and obj.mod may be used. (mod() is available on Long).
    when (obj) {
        1, 2       -> print("One or Two")   // Combining conditions
        "Hello"    -> print("Greeting")
        is Long    -> print(obj.mod(2))     // `is` operator
        !is String -> print("Not a string") // `!is` operator
        else       -> { // Note the block
            print("Unknown")
            print("So... not really knowing about it.")
        }
    }

    // Arbitrary expression as condition
    val s = "abc"
//   val s_encodes = when (s) {
//        // @TODO fix
//       // parseLong(s) -> true
//       else -> false
//   }

    // `when` without argument as a replacement for if-else if.
    val i = 10
    when {
        i in 1..10        -> print("i is in the range")
        i !in 10..20      -> print("x is outside the range")
        else            -> print("none of the above")
    }
}

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

// __________________________________________________ Null Checking and Casting

// A reference must be explicitly marked as nullable when null value is possible.
fun parseLong(str: String): Long? {
    try {
        return java.lang.Long.parseLong("")
    }
    catch ( e: java.lang.NumberFormatException) { }
}

// Use a function returning nullable value, 1#2
fun nullChecking_One(strX: String, strY: String) {
  val x = parseLong(strX)
  val y = parseLong(strY)

  // Using `x * y` yields error because they may hold nulls.
  if (x != null && y != null) {
      // x and y are automatically cast to non-nullable after null check
      print(x * y)
  }
}

// Use a function returning nullable value, 2#2
fun nullChecking_Two(strX: String, strY: String) {
    val x = parseLong(strX)
    val y = parseLong(strY)

    if (x == null || y == null)
        print("Wrong number format in x/y")
    else
        // x and y are automatically cast to non-nullable after null check
        print(x * y)
}

// Automatic cast with `is` operator, Demo 1#3
fun getStringLength_One(obj: Any): Int? {
    if (obj is String)
        // `obj` is automatically cast to `String` in this branch
        return obj.length

    // `obj` is still of type `Any` outside of the type-checked branch
    return null
}

// Automatic cast with `is` operator, Demo 2#3
fun getStringLength_Two(obj: Any): Int? {
    if (obj !is String)
        return null

    // `obj` is automatically cast to `String` in this branch
    return obj.length
}

// Automatic cast with `is` operator, Demo 3#3
fun getStringLength_Three(obj: Any): Int? {
    // `obj` is automatically cast to `String` on the right-hand side of `&&`
    if (obj is String && obj.length > 0)
        return obj.length

    return null
}


// ________________________________________________________________ Collections

fun collections() {
    val names = listOf("foo", "bar", "baz")

    // Iterating over collection
    for (name in names)
        println(name)

    // Checking if collection contains item
    if("foo" in names) // names.contains("foo") is called by kotlin
        print("foo in names")

    // Using function literals to filter and map collections
    names filter { it.startsWith("b") } sortBy { it } map { it.toUpperCase() } forEach {print(it) }

    // Filtering a list with condition
    val list = listOf(1, 2, 55, 101, -101, -1098, 42)
    val positives0 = list.filter { x -> x > 0 }
    val positives1 = list.filter { it > 0 } // or simply like this.

    // Traversing a map/list of pairs.
    val map = mapOf("a" to 1, "b" to 2, "c" to 3)
    for ((k, v) in map) {
        println("$k -> $v")
    }
}


// ___________________________________________________________________ Classes

/**
 * General class declaration syntax
 *
 * Note that *EVERYTHING* except class keyword and class name is optional,
 * including braces.
 *
 * // @TODO other than private?
 *
 * class <NAME> [VISIBILITY] (PrimaryConstructorArgs) : SuperType(Args) {
 *   // init blocks
 *   // secondary constructors
 *   // functions
 *   // properties
 *   // Nested and Inner Classes
 *   // Object Declarations
 * }
 */

// An empty class, no curly braces needed. Implicitly inherits from `Any`.
// `Any` is not `java.lang.Object`, in particular, it does not have any members
// other than equals() , hashCode() and toString().
class Empty

/**
 * A class with one primary constructors and more secondary constructors. A
 * primary constructor is defined inline.
 * Both primary and secondary constructors are optional, but secondaries must
 * directly or indirectly (through other constructors) call the primary.
 */
public class Address(street: String) {

    // The code for primary ctor should be defined in init block. If ctor has
    // any parameters, they will be available.
    init {
        println("${street} passed as primary constructor parameter.")
    }
    init {
        println("Multiple init blocks allowed.")
    }

    // Property declaration full syntax.
    // <var/val> <propertyName>: <PropertyType> [= <property_initializer>]
    // <getter>
    // <setter> // Not allowed for val! (read-only property).

    // street from primary constructor arg available in property initializer.
    // Read-only property. Setters are not allowed. (Default getter and setter
    // are used).
    var myStreet = street.toUpperCase()

    // Mutable property (Default getter and setter are used).
    public var changeMe : Int = 42

    // Custom getter and setter
    var PrefixedAndUpercased: String
    get() = this.myStreet.toUpperCase()
    set(value) { // Convention is to use `value` as arg name for setter.
        this.myStreet = "Street: " + value
    }

    // ERROR! Type is *required* for public properties.
    // public var whatAmIReally = 1010

    // ERROR! explicit initializer is required for var.
    // var somwthingWithDefault = Int?

    // Custom accessor
    var y: Int = 1000;

    val isBig: Boolean
      get() = this.y > 100

    var xAsStr: String
      get() = this.y.toString()
      set(value) {
//         this.y =
      }


    // Secondary constructors are prefixed with `constructor`
    // Calling other constructors with `this` keyword.
    constructor(street: String, city: String) : this(street) {
        println("The secondary constructor which takes two args was called, "
            + "value '$street' passed to primary constructor, and '$city' used"
            + " just right here.")
    }
}

/**
 * DTO's (POJO’s/POCO’s) or Data classes.
 *
 * equals(), hashCode(), toString(), copy(), component1()
 * component2(), ... are all provided automatically by Kotlin.
 *
 * [more about data classes](http://kotlinlang.org/docs/reference/data-classes.html)
 */
data class DataCustomer(val name: String, val email: String)

// Singleton
object Resource {
    val name = "Name"
}

// Private primary constructor
class DontCreateMe private (name: String) { }


fun classDemo() {
    // To create an instance of a class, we call the constructor as if it were
    // a regular function:
    val cus = Customer("their name", "someone@somewhere.tld")
    var where = Address("somewhere nice")

    // Accessing properties as if they are a field in Java. getters and setters
    // are called automatically for us.
    val mail = cus.email
}

// _________________________________________________________________INHERITANCE


// Unlike java, classes are `final` by default, meaning they can not be
// inherited from. `open` keyword is the opposite of `final`.
open class Base(p: Int) {
    init {
        println("$p was passed as p.")
    }

    open fun v() {} // like classes, methods are final by default.
    fun nv() {} // can not be overridden.
}

// Explicit super types are declared after colon. If super primary constructor
// has args, it must be called right here.
class Derived : Base {
    override fun v() {} // Unlike java, he override annotation is required!

    // If a derived class has no primary constructor, each secondary constructor
    // must call super().
    constructor(p: Int, q: Int) : super(p) { }

    // Call to super() is implicit (indirectly through other constructors).
    constructor(p: Int, q: Int, r: Int) : this(p, q) { }
}

/**
 * Multiple Inheritance is allowed, but if multiple implementations are
 * available to inherit, sub-class must override that member and provide it's
 * own implementation (and perhaps use one of the inherited ones, explicitly).
 */

open class InheritanceDemoA {
    open fun f() { print("Inheritance Demo A") }
    open fun dummyA() { }
    fun a() { print("a") }
}

// trait members are 'open' by default
trait InheritanceDemoB {
    fun f() { print("Inheritance Demo B") }
    fun b() { print("b") }
}

class InheritanceDemoC() : InheritanceDemoA(), InheritanceDemoB {
    override fun f() {
        super<InheritanceDemoA>.f() // call to A.f()
        super<InheritanceDemoB>.f() // call to B.f()
        super<InheritanceDemoA>.f() // call to A.f() again
    }

    // An overridden method is `open` by default, except if it is made final.
    final override fun dummyA() { }
}


// ---- Abstract classes ----

// An abstract member does not have an implementation. Thus, when some
// class inherits an abstract member, it does not count as an implementation.
// Abstract classes and abstract functions are open by default.
abstract class InheritanceDemoAbstractClass {
    abstract fun b()
}

// We can override a non-abstract open member with an abstract one.
abstract class MakeAMethodAbstract : InheritanceDemoA() {
   override abstract fun f()
}


// ---- Companion Objects ----

/**
 * Kotlin has no static methods. Companion objects are used for cases like
 * static factory method, so it has access to class members while not needing
 * an available instance.
 */
class ClassWithCompanion {
   companion object { } // will be called "Companion"
}

fun ClassWithCompanion.Companion.foo() {
   println("Hei I'm foo and I act like an static method.")
}

fun CompanionObjectDemo() {
   // foo() can be called with class name only (no instance needed)
   ClassWithCompanion.foo();
}

class derivedFromAbstract: InheritanceDemoAbstractClass(), InheritanceDemoB {
    // We are not required to override b()
}

data class Customer(val name: String, val email: String)


/**
  val digit = '8'
  if (digit in '0'..'9') // If character is in range
    println("$digit is a valid digit.")

fun nullChecking4(obj: Any?) {
  println(obj?.size ?: "empty")
}

 The block inside let is executed only of obj is not null. No casting.
fun nullChecking5(obj: Any): Int? {
  object?.let {
    return 42
  }
}

*/
