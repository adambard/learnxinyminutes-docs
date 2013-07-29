---
language: scala
author: George Petrov
author_url: http://www.georgepetrov.com
---

/*
  Set yourself up:

  1) Download Scala - http://www.scala-lang.org/downloads
  2) unzip/untar in your favourite location and put the bin subdir on the path
  3) Start a scala REPL by typing scala. You should see the prompt:

  scala>

  This is the so called REPL. You can run commands in the REPL. Let do just that:
*/

println(10) // prints the integer 10

println("Boo!") // printlns the string Boo!


// Evaluating a command gives you the type and value of the result

1 + 7

/* The above line results in:

  scala> 1 + 7
  res29: Int = 8

  This means the result of evaluating 1 + 7 is an object of type Int with a value of 8

  1+7 will give you the same result
*/


// Everything is an object, including a function type these in the repl:

7 // results in res30: Int = 7 (res30 is just a generated var name for the result)

// The next line gives you a function that takes an Int and returns it squared
(x:Int) => x * x    

// You can assign this function to an identifier, like this:
val sq = (x:Int) => x * x

/* The above says this
   
   sq: Int => Int = <function1>	

   Which means that this time we gave an explicit name to the value - sq is a function that take an Int and returns Int.

   sq can be executed as follows:
*/

sq(10)   // Gives you this: res33: Int = 100. The result is the Int with a value 100



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


// Tuples


// Combinators

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)


// For comprehensions

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared



// Conditionals

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println ("yeah") else println("nope")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"



// Object oriented features

class Person 



// Case classes

case class Person(name:String, phoneNumber:String)

Person("George", "1234") == Person("Kate", "1236")



// Pattern matching


// Regular expressions


// Strings


// Input and output