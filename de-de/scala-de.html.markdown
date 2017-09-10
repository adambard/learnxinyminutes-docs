---
language: Scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Ha-Duong Nguyen", "http://reference-error.org"]
    - ["Dennis Keller", "github.com/denniskeller"]
translators:
  - ["Christian Albrecht", "https://github.com/coastalchief"]
  - ["Jonas Grote", "https://github.com/exic"]
filename: learnscala-de.scala
lang: de-de
---

Scala ist eine funktionale und objektorientierte Programmiersprache
für die Java Virtual Machine (JVM), um allgemeine Programmieraufgaben
zu erledigen. Scala hat einen akademischen Hintergrund und wurde an
der EPFL (Lausanne / Schweiz) unter der Leitung von Martin Odersky entwickelt.

```scala
/*
Scala-Umgebung einrichten:

1. Scala binaries herunterladen- http://www.scala-lang.org/downloads
2. Unzip/untar in ein Verzeichnis
3. das Unterverzeichnis `bin` der `PATH`-Umgebungsvariable hinzufügen
4. Mit dem Kommando `scala` wird die REPL gestartet und zeigt als Prompt:

scala>

Die REPL (Read-Eval-Print Loop) ist der interaktive Scala Interpreter.
Hier kann man jeden Scala Ausdruck verwenden und das Ergebnis wird direkt
ausgegeben.
Als nächstes beschäftigen wir uns mit ein paar Scala-Grundlagen.
*/


/////////////////////////////////////////////////
// 1. Grundlagen
/////////////////////////////////////////////////

// Einzeilige Kommentare beginnen mit zwei Schrägstrichen.

/*
  Mehrzeilige Kommentare starten mit Schrägstrich und Stern
  und enden mit Stern und Schrägstrich.
*/

// Einen Wert und eine zusätzliche neue Zeile ausgeben:

println("Hallo Welt!")
println(10)


// Einen Wert ohne eine zusätzliche neue Zeile ausgeben:

print("Hallo Welt")

/*
  Variablen werden entweder mit var oder val deklariert.
  Deklarationen mit val sind immutable, also unveränderlich.
  Deklarationen mit var sind mutable, also veränderlich.
  Immutability ist gut.
*/
val x = 10 // x ist 10
x = 20     // Error: reassignment to val (Fehler: neue Zuweisung zu einem unveränderlichen Wert)
var y = 10
y = 20     // y ist jetzt 20

/*
Scala ist eine statisch getypte Sprache, auch wenn wir in dem o.g. Beispiel
keine Typen an x und y geschrieben haben.
In Scala ist etwas eingebaut, was sich Type Inference nennt. Das heißt, dass der
Scala Compiler in den meisten Fällen erraten kann, von welchem Typ eine Variable ist,
so dass der Typ nicht jedes mal angegeben werden muss.
Einen Typ gibt man bei einer Variablendeklaration wie folgt an:
*/
val z: Int = 10
val a: Double = 1.0


// Bei automatischer Umwandlung von Int auf Double wird aus 10 eine 10.0:

val b: Double = 10


// Boolean-Werte:

true
false


// Boolean-Operationen:

!true         // false
!false        // true
true == false // false
10 > 5        // true


// Mathematische Operationen sind wie gewohnt:

1 + 1   // 2
2 - 1   // 1
5 * 3   // 15
6 / 2   // 3
6 / 4   // 1
6.0 / 4 // 1.5


// Die Auswertung eines Ausdrucks in der REPL gibt den Typ
// und das Ergebnis zurück:

  scala> 1 + 7
  res29: Int = 8

/*
Das bedeutet, dass das Resultat der Auswertung von 1 + 7 ein Objekt
von Typ Int ist und einen Wert 8 hat.
"res29" ist ein sequentiell generierter Name, um das Ergebnis des
Ausdrucks zu speichern. Dieser Wert kann bei Dir anders sein...
*/

"Scala strings werden in doppelten Anführungszeichen eingeschlossen"
'a' // Ein Scala Char
// 'Einzeln ge-quotete strings gibt es nicht!' <= Das erzeugt einen Fehler!

// Für Strings gibt es die üblichen Java-Methoden:

"Hallo Welt".length
"Hallo Welt".substring(2, 6)
"Hallo Welt".replace("C", "3")


// Zusätzlich gibt es noch extra Scala-Methoden
// siehe: scala.collection.immutable.StringOps

"Hallo Welt".take(5)
"Hallo Welt".drop(5)


// String-Interpolation: prefix "s":

val n = 45
s"Wir haben $n Äpfel" // => "Wir haben 45 Äpfel"


// Ausdrücke im Innern von interpolierten Strings gibt es auch:

val a = Array(11, 9, 6)
s"Meine zweite Tochter ist ${a(0) - a(2)} Jahre alt."    // => "Meine zweite Tochter ist 5 Jahre alt."
s"Wir haben das Doppelte von ${n / 2.0} an Äpfeln." // => "Wir haben das Doppelte von 22.5 an Äpfeln."
s"2 im Quadrat: ${math.pow(2, 2)}"                      // => "2 im Quadrat: 4"


// Formatierung der interpolierten Strings mit dem prefix "f":

f"5 im Quadrat: ${math.pow(5, 2)}%1.0f"         // "5 im Quadrat: 25"
f"Quadratwurzel von 122: ${math.sqrt(122)}%1.4f" // "Quadratwurzel von 122: 11.0454"


// Raw Strings ignorieren Sonderzeichen:

raw"New line feed: \n. Carriage return: \r." // => "New line feed: \n. Carriage return: \r."


// Manche Zeichen müssen "escaped" werden, z.B.
// ein doppeltes Anführungszeichen im Innern eines Strings:

"Sie standen vor der \"Rose and Crown\"" // => "Sie standen vor der "Rose and Crown""


// Dreifache Anführungszeichen erlauben es, dass ein String über mehrere Zeilen geht
// und Anführungszeichen enthalten kann:

val html = """<form id="dieform">
                <p>Drück belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. Funktionen
/////////////////////////////////////////////////

// Funktionen werden so definiert:
//
//   def functionName(args...): ReturnType = { body... }
//
// Beachte: Es wird hier kein Schlüsselwort "return" verwendet.
// In Scala ist der letzte Ausdruck in einer Funktion der Rückgabewert.

def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}


// Die geschweiften Klammern können weggelassen werden, wenn
// die Funktion nur aus einem einzigen Ausdruck besteht:

def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y


// Syntax für Funktionsaufrufe:

sumOfSquares(3, 4)  // => 25


// In den meisten Fällen (mit Ausnahme von rekursiven Funktionen) können
// Rückgabetypen auch weggelassen werden, da dieselbe Typ-Inferenz, wie bei
// Variablen, auch bei Funktionen greift:

def sq(x: Int) = x * x  // Compiler errät, dass der return type Int ist


// Funktionen können Default-Parameter haben:

def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2) // => 3
addWithDefault(1)    // => 6


// Anonyme Funktionen sehen so aus:

(x: Int) => x * x


// Im Gegensatz zu def bei normalen Funktionen, kann bei anonymen Funktionen
// sogar der Eingabetyp weggelassen werden, wenn der Kontext klar ist.
// Beachte den Typ "Int => Int", dies beschreibt eine Funktion,
// welche Int als Parameter erwartet und Int zurückgibt.

val sq: Int => Int = x => x * x


// Anonyme Funktionen benutzt man ganz normal:

sq(10)   // => 100


// Wenn ein Parameter einer anonymen Funktion nur einmal verwendet wird,
// bietet Scala einen sehr kurzen Weg diesen Parameter zu benutzen,
// indem die Parameter als Unterstrich "_" in der Parameterreihenfolge
// verwendet werden. Diese anonymen Funktionen werden sehr häufig
// verwendet.

val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)
addOne(5)      // => 6
weirdSum(2, 4) // => 16


// Es gibt ein Schlüsselwort "return" in Scala. Allerdings ist seine Verwendung
// nicht immer ratsam und kann fehlerbehaftet sein. "return" gibt nur aus
// dem innersten def, welches den return Ausdruck umgibt, zurück.
// "return" hat keinen Effekt in anonymen Funktionen:

def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5)
      return z // Zeile macht z zum return Wert von foo
    else
      z + 2    // Zeile ist der return Wert von anonFunc
  }
  anonFunc(x)  // Zeile ist der return Wert von foo
}


/////////////////////////////////////////////////
// 3. Flusskontrolle
/////////////////////////////////////////////////

// Wertebereiche und Schleifen:

1 to 5
val r = 1 to 5
r.foreach(println)
r foreach println
(5 to 1 by -1) foreach (println)

// Scala ist syntaktisch sehr großzügig; Semikolons am Zeilenende
// sind optional, beim Aufruf von Methoden können die Punkte
// und Klammern entfallen und Operatoren sind im Grunde austauschbare Methoden.

// while Schleife:

var i = 0
while (i < 10) { println("i " + i); i += 1 }
i    // i ausgeben, res3: Int = 10


// Beachte: while ist eine Schleife im klassischen Sinne -
// Sie läuft sequentiell ab und verändert die loop-Variable.
// "while" in Scala läuft schneller ab als in Java und die o.g.
// Kombinatoren und Zusammenlegungen sind einfacher zu verstehen
// und zu parallelisieren.

// Ein do while Schleife

do {
  println("x ist immer noch weniger als 10")
  x += 1
} while (x < 10)


// Endrekursionen sind idiomatisch um sich wiederholende
// Aufgaben in Scala zu lösen. Rekursive Funtionen benötigen explizit einen
// Rückgabe-Typ, der Compiler kann ihn nicht erraten.
// Der Rückgabe-Typ in diesem Beispiel ist Unit:

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
if (x == 11) println ("yeah") else println("nay")
println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


/////////////////////////////////////////////////
// 4. Datenstrukturen (Array, Map, Set, Tupel)
/////////////////////////////////////////////////

// Array

val a = Array(1, 2, 3, 5, 8, 13)
a(0)
a(3)
a(21)    // Exception


// Map - Speichert Key-Value-Paare

val m = Map("fork" -> "Gabel", "spoon" -> "Löffel", "knife" -> "Messer")
m("fork")
m("spoon")
m("bottle")       // Exception
val safeM = m.withDefaultValue("unbekannt")
safeM("bottle")

// Set - Speichert Unikate, unsortiert (sortiert -> SortedSet)

val s = Set(1, 3, 7)
s(0) //false
s(1) //true
val s = Set(1,1,3,3,7)
s: scala.collection.immutable.Set[Int] = Set(1, 3, 7)

// Tupel - Speichert beliebige Daten und "verbindet" sie miteinander
// Ein Tupel ist keine Collection.

(1, 2)
(4, 3, 2)
(1, 2, "three")
(a, 2, "three")


// Hier ist der Rückgabewert der Funktion ein Tupel
// Die Funktion gibt das Ergebnis sowie den Rest zurück.

val divideInts = (x: Int, y: Int) => (x / y, x % y)
divideInts(10, 3)


// Um die Elemente eines Tupels anzusprechen, benutzt man diese
// Notation: _._n wobei n der index des Elements ist (Index startet bei 1)

val d = divideInts(10, 3)
d._1
d._2


/////////////////////////////////////////////////
// 5. Objektorientierte Programmierung
/////////////////////////////////////////////////

/*
  Bislang waren alle gezeigten Sprachelemente einfache Ausdrücke, welche zwar
  zum Ausprobieren und Lernen in der REPL gut geeignet sind, jedoch in
  einer Scala-Datei selten alleine zu finden sind.
  Die einzigen Top-Level-Konstrukte in Scala sind nämlich:

  - Klassen (classes)
  - Objekte (objects)
  - case classes
  - traits

  Diesen Sprachelemente wenden wir uns jetzt zu.
*/

// Klassen

// Zum Erstellen von Objekten benötigt man eine Klasse, wie in vielen
// anderen Sprachen auch.

// erzeugt Klasse mit default Konstruktor:

class Hund
scala> val t = new Hund
t: Hund = Hund@7103745


// Der Konstruktor wird direkt hinter dem Klassennamen deklariert.

class Hund(sorte: String)
scala> val t = new Hund("Dackel")
t: Hund = Hund@14be750c
scala> t.sorte //error: value sorte is not a member of Hund


// Per val wird aus dem Attribut ein unveränderliches Feld der Klasse
// Per var wird aus dem Attribut ein veränderliches Feld der Klasse

class Hund(val sorte: String)
scala> val t = new Hund("Dackel")
t: Hund = Hund@74a85515
scala> t.sorte
res18: String = Dackel


// Methoden werden mit def geschrieben

def bark = "Wuff, wuff!"


// Felder und Methoden können public, protected und private sein
// default ist public
// private ist nur innerhalb des deklarierten Bereichs sichtbar

class Hund {
  private def x = ...
  def y = ...
}


// protected ist nur innerhalb des deklarierten und aller
// erbenden Bereiche sichtbar

class Hund {
  protected def x = ...
}
class Dackel extends Hund {
  // x ist sichtbar
}

// Object
// Wird ein Objekt ohne das Schlüsselwort "new" instanziert, wird das sog.
// "companion object" aufgerufen. Mit dem "object" Schlüsselwort wird so
// ein Objekt (Typ UND Singleton) erstellt. Damit kann man dann eine Klasse
// verwenden, ohne ein Objekt instanziieren zu müssen.
// Ein gültiges companion Objekt einer Klasse ist es aber erst dann, wenn
// es genauso heißt und in derselben Datei wie die Klasse definiert wurde.

object Hund {
  def alleSorten = List("Pitbull", "Dackel", "Retriever")
  def createHund(sorte: String) = new Hund(sorte)
}

// Case classes
// Fallklassen bzw. Case classes sind Klassen, die normale Klassen um
// zusätzliche Funktionalität erweitern.
// Mit Case-Klassen bekommt man ein paar
// Dinge einfach dazu, ohne sich darum kümmern zu müssen. Z.B.
// ein companion object mit den entsprechenden Methoden,
// Hilfsmethoden wie toString(), equals() und hashCode() und auch noch
// Getter für unsere Attribute (das Angeben von val entfällt dadurch)

class Person(val name: String)
class Hund(val sorte: String, val farbe: String, val halter: Person)


// Es genügt, das Schlüsselwort case vor die Klasse zu schreiben:

case class Person(name: String)
case class Hund(sorte: String, farbe: String, halter: Person)


// Für neue Instanzen braucht man kein "new":

val dackel = Hund("dackel", "grau", Person("peter"))
val dogge = Hund("dogge", "grau", Person("peter"))


// getter

dackel.halter  // => Person = Person(peter)


// equals

dogge == dackel  // => false


// copy
// otherGeorge == Person("george", "9876")

val otherGeorge = george.copy(phoneNumber = "9876")

// Traits
// Ähnlich wie Java interfaces, definiert man mit traits einen Objekttyp
// und Methodensignaturen. Scala erlaubt allerdings das teilweise
// Implementieren dieser Methoden. Konstruktorparameter sind nicht erlaubt.
// Traits können von anderen Traits oder Klassen erben, aber nur von
// parameterlosen.

trait Hund {
	def sorte: String
	def farbe: String
	def bellen: Boolean = true
	def beissen: Boolean
}
class Bernhardiner extends Hund{
	val sorte = "Bernhardiner"
	val farbe = "braun"
	def beissen = false
}



scala> b
res0: Bernhardiner = Bernhardiner@3e57cd70
scala> b.sorte
res1: String = Bernhardiner
scala> b.bellen
res2: Boolean = true
scala> b.beissen
res3: Boolean = false

// Ein Trait kann auch als Mixin eingebunden werden. Die Klasse erbt vom
// ersten Trait mit dem Schlüsselwort "extends", während weitere Traits
// mit "with" verwendet werden können.

trait Bellen {
	def bellen: String = "Wuff"
}
trait Hund {
	def sorte: String
	def farbe: String
}
class Bernhardiner extends Hund with Bellen{
	val sorte = "Bernhardiner"
	val farbe = "braun"
}
scala> val b = new Bernhardiner
b: Bernhardiner = Bernhardiner@7b69c6ba
scala> b.bellen
res0: String = Wuff

/////////////////////////////////////////////////
// 6. Mustervergleich (Pattern Matching)
/////////////////////////////////////////////////

// Pattern matching in Scala ist ein sehr nützliches und wesentlich
// mächtigeres Feature als Vergleichsfunktionen in Java. In Scala
// benötigt ein case Statement kein "break", ein fall-through gibt es nicht.
// Mehrere Überprüfungen können mit einem Statement gemacht werden.
// Pattern matching wird mit dem Schlüsselwort "match" gemacht.

val x = ...
x match {
  case 2 =>
  case 3 =>
  case _ =>
}


// Pattern Matching kann auf beliebige Typen prüfen

val any: Any = ...
val gleicht = any match {
  case 2 | 3 | 5 => "Zahl"
  case "wuff" => "String"
  case true | false => "Boolean"
  case 45.35 => "Double"
  case _ => "Unbekannt"
}


// und auf Objektgleichheit

def matchPerson(person: Person): String = person match {
  case Person("George", nummer) => "George! Die Nummer ist " + number
  case Person("Kate", nummer)   => "Kate! Die Nummer ist " + nummer
  case Person(name, nummer)     => "Irgendjemand: " + name + ", Telefon: " + nummer
}


// Und viele mehr...

val email = "(.*)@(.*)".r  // regex
def matchEverything(obj: Any): String = obj match {
  // Werte:
  case "Hallo Welt" => "string Hallo Welt gefunden"
  // Typen:
  case x: Double => "Double gefunden: " + x
  // Bedingungen:
  case x: Int if x > 10000 => "Ziemlich große Zahl gefunden!"
  // Case-Klassen:
  case Person(name, number) => s"Kontaktinformationen für $name gefunden!"
  // RegEx:
  case email(name, domain) => s"E-Mail-Adresse $name@$domain gefunden"
  // Tupel:
  case (a: Int, b: Double, c: String) => s"Tupel gefunden: $a, $b, $c"
  // Strukturen:
  case List(1, b, c) => s"Liste aus drei Elementen gefunden, startend mit 1: 1, $b, $c"
  // Pattern kann man ineinander schachteln:
  case List(List((1, 2, "YAY"))) => "Liste von Tupeln gefunden"
}


// Jedes Objekt mit einer "unapply" Methode kann per Pattern geprüft werden.
// Ganze Funktionen können Patterns sein:

val patternFunc: Person => String = {
  case Person("George", number) => s"George's number: $number"
  case Person(name, number) => s"Random person's number: $number"
}


/////////////////////////////////////////////////
// 7. "Higher-order"-Funktionen
/////////////////////////////////////////////////

// Scala erlaubt, dass Methoden und Funktionen wiederum Funtionen und Methoden
// als Aufrufparameter oder Rückgabewert verwenden. Diese Methoden heißen
// higher-order functions.
// Es gibt zahlreiche higher-order-Funtionen nicht nur für Listen, auch für
// die meisten anderen Collection-Typen, sowie andere Klassen in Scala.
// Nennenswerte sind:
// "filter", "map", "reduce", "foldLeft"/"foldRight", "exists", "forall"

// List

def istGleichVier(a:Int) = a == 4
val list = List(1, 2, 3, 4)
val resultExists4 = list.exists(isEqualToFour)


// map
// map nimmt eine Funktion und führt sie auf jedem Element aus und erzeugt
// eine neue Liste

// Funktion erwartet einen Int und gibt einen Int zurück:

val add10: Int => Int = _ + 10


// add10 wird auf jedes Element angewendet:

List(1, 2, 3) map add10 // => List(11, 12, 13)


// Anonyme Funktionen können anstatt definierter Funktionen verwendet werden:

List(1, 2, 3) map (x => x + 10)


// Der Unterstrich wird anstelle eines Parameters einer anonymen Funktion
// verwendet. Er wird an die Variable gebunden:

List(1, 2, 3) map (_ + 10)


// Wenn der anonyme Block und die Funktion beide EIN Argument erwarten,
// kann sogar der Unterstrich weggelassen werden.

List("Dom", "Bob", "Natalia") foreach println


// filter
// filter nimmt ein Prädikat (eine Funktion von A -> Boolean) und findet
// alle Elemente, die auf das Prädikat passen:

List(1, 2, 3) filter (_ > 2) // => List(3)
case class Person(name: String, age: Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))


// reduce
// reduce nimmt zwei Elemente und kombiniert sie zu einem Element,
// und zwar so lange, bis nur noch ein Element da ist.

// foreach
// foreach gibt es für einige Collections

val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// For comprehensions
// Eine for-comprehension definiert eine Beziehung zwischen zwei Datensets.
// Dies ist keine for-Schleife.

for { n <- s } yield sq(n)
val nSquared2 = for { n <- s } yield sq(n)
for { n <- nSquared2 if n < 10 } yield n
for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared


/////////////////////////////////////////////////
// 8. Implicits
/////////////////////////////////////////////////

// **ACHTUNG:**
// Implicits sind ein sehr mächtiges Sprachfeature von Scala.
// Es sehr einfach,
// sie falsch zu benutzen und Anfänger sollten sie mit Vorsicht oder am
// besten erst dann benutzen, wenn sie verstehen, wie sie funktionieren.
// Dieses Tutorial enthält Implicits, da sie in Scala an jeder Stelle
// vorkommen und man auch mit einer Bibliothek, die Implicits benutzt, sonst
// nichts sinnvolles machen kann.
// Hier soll ein Grundverständnis geschaffen werden, wie sie funktionieren.

// Mit dem Schlüsselwort implicit können Methoden, Werte, Funktion, Objekte
// zu "implicit Methods" werden.

implicit val myImplicitInt = 100
implicit def myImplicitFunction(sorte: String) = new Hund("Golden " + sorte)


// implicit ändert nicht das Verhalten eines Wertes oder einer Funktion

myImplicitInt + 2                   // => 102
myImplicitFunction("Pitbull").sorte // => "Golden Pitbull"


// Der Unterschied ist, dass diese Werte ausgewählt werden können, wenn ein
// anderer Codeteil einen implicit Wert benötigt, zum Beispiel innerhalb von
// implicit Funktionsparametern

// Diese Funktion hat zwei Parameter: einen normalen und einen implicit

def sendGreetings(toWhom: String)(implicit howMany: Int) =
  s"Hallo $toWhom, $howMany Segenswünsche für Sie und Ihre Angehörigen!"


// Werden beide Parameter gefüllt, verhält sich die Funktion wie erwartet

sendGreetings("John")(1000)  // => "Hallo John, 1000 Segenswünsche für Sie und Ihre Angehörigen!"


// Wird der implicit-Parameter jedoch weggelassen, wird ein anderer
// implicit-Wert vom gleichen Typ genommen. Der Compiler sucht im
// lexikalischen Scope und im companion object nach einem implicit-Wert,
// der vom Typ passt, oder nach einer implicit-Methode, mit der er in den
// geforderten Typ konvertieren kann.

// Hier also: "myImplicitInt", da ein Int gesucht wird

sendGreetings("Jane")  // => "Hallo Jane, 100 Segenswünsche für Sie und Ihre Angehörigen!"


// bzw. "myImplicitFunction"
// Der String wird erst mit Hilfe der Funktion in Hund konvertiert,
// dann wird die Methode aufgerufen:

"Retriever".sorte // => "Golden Retriever"


/////////////////////////////////////////////////
// 9. Sonstiges
/////////////////////////////////////////////////
// Importe

import scala.collection.immutable.List


// Importiere alle Unterpackages

import scala.collection.immutable._


// Importiere verschiedene Klassen mit einem Statement

import scala.collection.immutable.{List, Map}


// Einen Import kann man mit '=>' umbenennen

import scala.collection.immutable.{List => ImmutableList}


// Importiere alle Klasses, mit Ausnahem von....
// Hier ohne: Map and Set:

import scala.collection.immutable.{Map => _, Set => _, _}

// Main

object Application {
  def main(args: Array[String]): Unit = {
    // Zeugs hier rein.
  }
}


// I/O
// Eine Datei Zeile für Zeile lesen

import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)


// Eine Datei schreiben

val writer = new PrintWriter("myfile.txt")
writer.write("Schreibe Zeile" + util.Properties.lineSeparator)
writer.write("Und noch eine Zeile" + util.Properties.lineSeparator)
writer.close()

```

## Weiterführende Hinweise

### DE
* [Scala Tutorial](https://scalatutorial.wordpress.com)
* [Scala Tutorial](http://scalatutorial.de)

### EN
* [Scala for the impatient](http://horstmann.com/scala/)
* [Twitter Scala school](http://twitter.github.io/scala_school/)
* [The scala documentation](http://docs.scala-lang.org/)
* [Try Scala in your browser](http://scalatutorials.com/tour/)
* [Neophytes Guide to Scala](http://danielwestheide.com/scala/neophytes.html)
* Join the [Scala user group](https://groups.google.com/forum/#!forum/scala-user)
