---
language: kotlin
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["Xavier Sala", "https://github.com/utrescu"] 
lang: ca-es
filename: LearnKotlin-ca.kt
---

Kotlin és un llenguatge estàtic tipat per la JVM, Android i el navegador.
És interoperable al 100% amb Java.
[Llegir-ne més aquí.](https://kotlinlang.org/)

```kotlin
// Els comentaris d'una línia comencen amb //
/*
Els comentaris multilínia són com aquest
*/

// La paraula clau "package" funciona de la mateixa forma que en Java

package com.learnxinyminutes.kotlin

/*
El punt d'entrada dels programes en Kotlin és una funció anomenada "main".
La funció rep un array que té els arguments fets servir al executar-lo.
*/
fun main(args: Array<String>) {
    /*
    La declaració de variables es pot fer tant amb "var" com amb "val".
    A les declarades amb "val" no se'ls hi pot canviar el valor
    en canvi a les declarades amb "var" si.
    */
    val fooVal = 10 // no es podrà canviar el valor de fooVal
    var fooVar = 10
    fooVar = 20 // fooVar si que es pot canviar

    /*
    Gairebé sempre, Kotlin pot determinar el tipus d'una variable,
    de manera que no caldrà definir-lo cada vegada.
    Però es pot definir el tipus d'una variable explícitament d'aquesta forma:
    */
    val foo: Int = 7

    /*
    Els "strings" es poden representar igual que com es fa en Java.
    Es poden escapar caràcters amb la barra inversa.
    */
    val fooString = "Aquí està la meva cadena!"
    val barString = "Imprimir en dues línies?\nCap problema!"
    val bazString = "Es poden posar tabuladors?\tI tant!"
    println(fooString)
    println(barString)
    println(bazString)

    /*
    Es poden definir strings literals envoltant-los amb les triples cometes 
    (""").
    Dins hi poden haver tant salts de línies com d'altres caràcters.
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Hola món!")
}
"""
    println(fooRawString)

    /*
    Els strings poden contenir expressions de plantilla.
    Les expressions de plantilla comencen amb el símbol ($).
    */
    val fooTemplateString = "$fooString té ${fooString.length} caràcters"
    println(fooTemplateString)

    /*
    Perquè una variable pugui contenir null ha de ser declarada específicament
    com a nullable afengint-li ? al seu tipus.
    Es pot accedir a una variable nulable fent servir l'operador ?.
    L'operador ?: permet especificar un valor alternatiu en cas de que la
    variable sigui null.
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    Les funcions es declaren amb la paraula "fun".
    Els arguments s'especifiquen entre corxets després del nom de la funció.
    Els arguments poden tenir un valor per defecte.
    El retorn de les funcions, si cal, es posa després de l'argument.
    */
    fun hello(name: String = "món"): String {
        return "Hola, $name!"
    }
    println(hello("foo")) // => Hola, foo!
    println(hello(name = "bar")) // => Hola, bar!
    println(hello()) // => Hola, món!

    /*
    Un dels paràmetres d'una funció pot ser marcat amb la paraula clau
    "vararg" que permet que una funció accepti un número variable
    d'arguments.
    */
    fun varargExample(vararg names: Int) {
        println("S'han rebut ${names.size} arguments")
    }
    varargExample() // => S'han rebut 0 elements
    varargExample(1) // => S'ha rebut 1 element
    varargExample(1, 2, 3) // => S'han rebut 3 elements

    /*
    Quan una funció consisteix en una sola expressió no calen els corxets
    El cos de la funció es posa rere el símbol =.
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // Si el tipus retornat es pot determinar no cal especificar-lo.
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // Les funcions poden tenir altres funcions com arguments i
    // fins i tot retornar-ne.
    fun not(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // Les funcions amb nom es poden especificar quan fan d'arguments amb ::
    val notOdd = not(::odd)
    val notEven = not(::even)
    // Les expressions lambda es poden posar com arguments.
    val notZero = not {n -> n == 0}
    /*
    Si la lambda només té un paràmetre es pot ometre la seva declaració.
    El seu valor serà "it".
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // Les classes es defineixen amb "class".
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int): Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int): Int {
            return x * y
        }
    }
    /*
    Per crear una nova instància es crida al constructor.
    Tingueu en compte que Kotlin no té l'operador "new".
    */
    val fooExampleClass = ExampleClass(7)
    // Els mètodes es poden cridar amb la notació .
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    Si una funció ha estat marcada amb "infix" es pot cridar amb la
    notació infix.
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    Les classes "data" són classes que només contenen dades.
    Es creen automàticament els mètodes "hashCode","equals" i "toString"
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // Les classes data tenen un mètode "copy".
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // Els objectes es poden desestructurar amb múltiples variables
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4

    // desestructurat en un bucle "for"
    for ((a, b, c) in listOf(fooData)) {
        println("$a $b $c") // => 1 100 4
    }

    val mapData = mapOf("a" to 1, "b" to 2)
    // Els mapes també
    for ((key, value) in mapData) {
        println("$key -> $value")
    }

    // La funció "with" és similar a la de JavaScript.
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableData = MutableDataClassExample(7, 4, 9)
    with (fooMutableData) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableData) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    Es pot crear una llista amb la funció "listOf".
    La llista serà immutable - no s'hi poden afegir o treure elements.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // Es pot accedir als elements a partir del seu índex.
    println(fooList[1]) // => b

    // Es poden crear llistes mutables amb la funció "mutableListOf".
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // Es poden crear conjunts amb la funció "setOf".
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // Es poden crear mapes amb la funció "mapOf".
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // S'accedeix als valors del mapa a partir del seu índex.
    println(fooMap["a"]) // => 8

    /*
    Les sequències representen col·leccions evaluades quan fan falta.
    Podem crear una seqüencia amb la funció "generateSequence".
    */
    val fooSequence = generateSequence(1, { it + 1 })
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Per exemple amb aquesta seqüència es creen els números de Fibonacci:
    fun fibonacciSequence(): Sequence<Long> {
        var a = 0L
        var b = 1L

        fun next(): Long {
            val result = a + b
            a = b
            b = result
            return a
        }

        return generateSequence(::next)
    }
    val y = fibonacciSequence().take(10).toList()
    println(y) // => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

    // Kotlin proporciona funcions de primer ordre per treballar amb
    // col·leccions.
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "parell" else "senar"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // Es pot fer servir el bucle "for" amb qualsevol cosa que proporcioni
    // un iterador.
    for (c in "hello") {
        println(c)
    }

    // els bucles "while" funcionen com en altres llenguatges.
    var ctr = 0
    while (ctr < 5) {
        println(ctr)
        ctr++
    }
    do {
        println(ctr)
        ctr++
    } while (ctr < 10)

    /*
    "if" es pot fer servir com una expressió que retorna un valor.
    Per això no cal l'operador ternari ?: en Kotlin.
    */
    val num = 5
    val message = if (num % 2 == 0) "parell" else "senar"
    println("$num is $message") // => 5 is odd

    // "when" es pot fer servir com alternativa a les cadenes "if-else if".
    val i = 10
    when {
        i < 7 -> println("primer bloc")
        fooString.startsWith("hola") -> println("segon bloc")
        else -> println("bloc else")
    }

    // "when" es pot fer servir amb un argument.
    when (i) {
        0, 21 -> println("0 o 21")
        in 1..20 -> println("en el rang 1 a 20")
        else -> println("cap dels anteriors")
    }

    // "when" es pot fer servir com una funció que retorna valors.
    var result = when (i) {
        0, 21 -> "0 o 21"
        in 1..20 -> "en el rang 1 a 20"
        else -> "cap dels anteriors"
    }
    println(result)

    /*
    Es pot comprovar el tipus d'un objecte fent servir l'operador "is".
    Si un objecte passa una comprovació es pot fer servir sense posar-hi
    cap casting.
    */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x es converteix automàticament a Booleà
            return x
        } else if (x is Int) {
            // x es converteix automàticament a int
            return x > 0
        } else if (x is String) {
            // x es converteix a string automàticament
            return x.isNotEmpty()
        } else {
            return false
        }
    }
    println(smartCastExample("Hola món!")) // => true
    println(smartCastExample("")) // => false
    println(smartCastExample(5)) // => true
    println(smartCastExample(0)) // => false
    println(smartCastExample(true)) // => true

    // També es pot cridar smarcast en un bloc when
    fun smartCastWhenExample(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    Les extensions són una forma d'afegir noves funcionalitats a una classe.
    És semblant a les extensions de C#.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hola món!".remove('l')) // => Hoa, món!

    println(EnumExample.A) // => A
    println(ObjectExample.hello()) // => hola
}

// Les classes enumerades són semblants a les de Java
enum class EnumExample {
    A, B, C
}

/*
El paràmetre "object" es pot fer servir per crear objectes singleton.
No es poden instanciar però es pot fer referència a la seva única instància
amb el seu nom.
Són similars als singletons d'Scala.
*/
object ObjectExample {
    fun hello(): String {
        return "hola"
    }
}

fun useObject() {
    ObjectExample.hello()
    val someRef: Any = ObjectExample // podem fer servir el nom de l'objecte
}

```

### Per llegir més

* [tutorials de Kotlin](https://kotlinlang.org/docs/tutorials/)
* [Provar Kotlin en el navegador](http://try.kotlinlang.org/)
* [Llista de recursos de Kotlin](http://kotlin.link/)
