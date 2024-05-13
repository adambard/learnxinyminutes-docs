---
language: kotlin
filename: LearnKotlin-fr.kt
lang: fr-fr
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["Eric Ampire", "https://github.com/eric-ampire"]
---

Kotlin est un langage de programmation à typage statique pour la JVM, Android et le
navigateur. Il est 100% interopérable avec Java.
[Pour en savoir plus, cliquez ici](https://kotlinlang.org/)

```kotlin
// Les commentaires d'une seule ligne commencent par //
/*
Les commentaires de plusieurs lignes ressemblent à ceci.
*/

// Le mot-clé "package" fonctionne de la même manière qu'en Java.
   package com.learnxinyminutes.kotlin

/*
Le point d'entrée d'un programme Kotlin est une fonction appelée "main".
La fonction reçoit un tableau contenant tous les arguments de la ligne de commande.
Depuis Kotlin 1.3, la fonction "main" peut également être définie sans
tout paramètre.
*/
fun main(args: Array<String>) {
    /*
    La déclaration des valeurs se fait en utilisant soit "var" soit "val".
     Les déclarations "val" ne peuvent pas être réaffectées, alors que les déclarations "vars" le peuvent.
    */
    val fooVal = 10 // nous ne pouvons pas plus tard réaffecter fooVal à autre chose
    var fooVar = 10
    fooVar = 20 // fooVar peut être réaffecté

    /*
    Dans la plupart des cas, Kotlin peut déterminer quel est le type de variable,
    afin de ne pas avoir à le préciser explicitement à chaque fois.
    Nous pouvons déclarer explicitement le type d'une variable de cette manière :
    */
    val foo: Int = 7

    /*
    Les chaînes de caractères peuvent être représentées de la même manière qu'en Java.
    L'échappement se fait avec une barre oblique inversée.
    */
    val fooString = "Ma chaine est là !"
    val barString = "Imprimer sur une nouvelle ligne ? \nPas de problème !"
    val bazString = "Vous voulez ajouter une tabulation ? \tPas de problème !"
    println(fooString)
    println(barString)
    println(bazString)

    /*
    Une chaîne brute est délimitée par une triple citation (""").
    Les chaînes de caractères brutes peuvent contenir des nouvelles lignes et tout autre caractère.
    */
    val fooRawString = """
fun helloWorld(val name : String) {
   println("Bonjour, le monde !")
}
"""
    println(fooRawString)

    /*
    Les chaînes de caractères peuvent contenir des expressions modèles.
    Une expression modèle commence par le signe du dollar ($).
    */
    val fooTemplateString = "$fooString as ${fooString.length} caractères"
    println(fooTemplateString) // => Ma chaine est là ! comporte 18 caractères

    /*
    Pour qu'une variable soit considérée comme nulle, elle doit être explicitement spécifiée comme nulable.
    Une variable peut être spécifiée comme nulle en ajoutant un ? à son type.
    On peut accéder à une variable nulable en utilisant l'opérateur ?
    Nous pouvons utiliser l'opérateur ?: pour spécifier une valeur alternative à utiliser
    si une variable est nulle.
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1

    /*
    Les fonctions peuvent être déclarées en utilisant le mot-clé "fun".
    Les arguments des fonctions sont spécifiés entre parenthèses après le nom de la fonction.
    Les arguments de fonction peuvent éventuellement avoir une valeur par défaut.
    Le type de retour de la fonction, si nécessaire, est spécifié après les arguments.
    */
    fun hello(name: String = "world"): String {
        return "Bonjour, $name!"
    }
    println(hello("foo")) // => Bonjour, foo!
    println(hello(name = "bar")) // => Bonjour, bar!
    println(hello()) // => Bonjour, le monde!

    /*
    Un paramètre de fonction peut être marqué avec le mot-clé "vararg
    pour permettre de passer un nombre variable d'arguments à la fonction.
    */
    fun varargExample(vararg names: Int) {
        println("L'argument comporte ${names.size} éléments.")
    }
    varargExample() // => L'argument a 0 éléments
    varargExample(1) // => L'argument a 1 éléments
    varargExample(1, 2, 3) // => L'argument comporte 3 éléments

    /*
    Lorsqu'une fonction est constituée d'une seule expression, les parenthèses bouclées peuvent
    être omis. Le corps est spécifié après le symbole =.
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // Si le type de retour peut être déduit, alors nous n'avons pas besoin de le préciser.
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // Les fonctions peuvent prendre des fonctions d'arguments et des fonctions de retour.
    fun not(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // Les fonctions nommées peuvent être spécifiées comme arguments en utilisant l'opérateur :: .
    val notOdd = not(::odd)
    val notEven = not(::even)
    // Les expressions lambda peuvent être spécifiées en tant qu'arguments.
    val notZero = not {n -> n == 0}
    /*
    Si un lambda n'a qu'un seul paramètre
    alors sa déclaration peut être omise (ainsi que le ->).
    Le nom du paramètre unique sera "it".
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // Le mot-clé "class" est utilisé pour déclarer les classes.
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int): Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int): Int {
            return x * y
        }
    }
    /*
    Pour créer une nouvelle instance, nous appelons le constructeur.
    Notez que Kotlin n'a pas de mot-clé "new" .
    */
    val fooExampleClass = ExampleClass(7)
    // Les fonctions des membres peuvent être appelées en utilisant la notation par points.
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    Si une fonction a été marquée avec le mot-clé "infix", elle peut être
    appelé en utilisant la notation infixe.
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    Les classes de données sont une façon concise de créer des classes qui ne contiennent que des données.
    Les méthodes "hashCode"/"equals" et "toString" sont générées automatiquement.
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // Les classes de données ont une methode "copy".
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // Les objets peuvent être déstructurés en plusieurs variables.
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4

    // La déstructuration en boucle "for"
    for ((a, b, c) in listOf(fooData)) {
        println("$a $b $c") // => 1 100 4
    }

    val mapData = mapOf("a" to 1, "b" to 2)
    // Map.Entry est également déstructurable
    for ((key, value) in mapData) {
        println("$key -> $value")
    }

    // La fonction "with" est similaire à la déclaration "with" de JavaScript.
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableData = MutableDataClassExample(7, 4, 9)
    with (fooMutableData) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableData) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    Nous pouvons créer une liste en utilisant la fonction "listOf".
    La liste sera immuable - les éléments ne peuvent être ajoutés ou supprimés.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // Les éléments d'une liste sont accessibles par leur index.
    println(fooList[1]) // => b

    // Une liste mutable peut être créée en utilisant la fonction "mutableListOf".
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // Nous pouvons créer un ensemble en utilisant la fonction "setOf".
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // Nous pouvons créer un map en utilisant la fonction "mapOf".
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // Map values can be accessed by their key.
    println(fooMap["a"]) // => 8

    /*
    Les séquences représentent des collections évaluées paresseusement.
    Nous pouvons créer une séquence en utilisant la fonction "generateSequence".
    */
    val fooSequence = generateSequence(1, { it + 1 })
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Un exemple d'utilisation d'une séquence pour générer des nombres de Fibonacci :
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

    // Kotlin offre des fonctions d'ordre supérieur pour le travail avec les collections.
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "even" else "odd"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // Une boucle "for" peut être utilisée avec tout ce qui fournit un itérateur.
    for (c in "hello") {
        println(c)
    }

    // Les boucles "while" fonctionnent de la même manière que les autres langues.
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
    "if" peut être utilisé comme une expression qui renvoie une valeur.
    Pour cette raison, l'opérateur ternaire ?: n'est pas nécessaire dans Kotlin.
    */
    val num = 5
    val message = if (num % 2 == 0) "even" else "odd"
    println("$num is $message") // => 5 is odd

    // Le terme "when" peut être utilisé comme alternative aux chaînes "if-else if".
    val i = 10
    when {
        i < 7 -> println("first block")
        fooString.startsWith("hello") -> println("second block")
        else -> println("else block")
    }

    // "when" peut être utilisé avec un argument.
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // "when" peut être utilisé comme une fonction qui renvoie une valeur.
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)

    /*
    Nous pouvons vérifier si un objet est d'un type particulier en utilisant l'opérateur "is".
    Si un objet passe avec succès une vérification de type, il peut être utilisé comme ce type sans
    en le diffusant explicitement.
    */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x est automatiquement converti en booléen
            return x
        } else if (x is Int) {
            // x est automatiquement converti en Int
            return x > 0
        } else if (x is String) {
            // x est automatiquement converti en String
            return x.isNotEmpty()
        } else {
            return false
        }
    }
    println(smartCastExample("Bonjour, le monde !")) // => true
    println(smartCastExample("")) // => false
    println(smartCastExample(5)) // => true
    println(smartCastExample(0)) // => false
    println(smartCastExample(true)) // => true

    // Le Smartcast fonctionne également avec le bloc when
    fun smartCastWhenExample(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    Les extensions sont un moyen d'ajouter de nouvelles fonctionnalités à une classe.
    C'est similaire aux méthodes d'extension C#.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hello, world!".remove('l')) // => Heo, word!
}

// Les classes Enum sont similaires aux types Java enum.
enum class EnumExample {
    A, B, C // Les constantes Enum sont séparées par des virgules.
}
fun printEnum() = println(EnumExample.A) // => A

// Puisque chaque enum est une instance de la classe enum, ils peuvent être initialisés comme :
enum class EnumExample(val value: Int) {
    A(value = 1),
    B(value = 2),
    C(value = 3)
}
fun printProperty() = println(EnumExample.A.value) // => 1

// Chaque énum a des propriétés pour obtenir son nom et son ordinal (position) dans la déclaration de classe de l'énum :
fun printName() = println(EnumExample.A.name) // => A
fun printPosition() = println(EnumExample.A.ordinal) // => 0

/*
Le mot-clé "objet" peut être utilisé pour créer des objets singleton.
On ne peut pas l'instancier mais on peut se référer à son instance unique par son nom.
Cela est similaire aux objets singleton de Scala.
*/
object ObjectExample {
    fun hello(): String {
        return "Bonjour"
    }

    override fun toString(): String {
        return "Bonjour, c'est moi, ${ObjectExample::class.simpleName}"
    }
}


fun useSingletonObject() {
    println(ObjectExample.hello()) // => hello
    // Dans Kotlin, "Any" est la racine de la hiérarchie des classes, tout comme "Object" l'est dans Java
    val someRef: Any = ObjectExample
    println(someRef) // => Bonjour, c'est moi, ObjectExample
}


/* L'opérateur d'assertion non nulle ( !!) convertit toute valeur en un type non nul et
   lance une exception si la valeur est nulle.
*/
var b: String? = "abc"
val l = b!!.length

data class Counter(var value: Int) {
    // surcharge Counter += Int
    operator fun plusAssign(increment: Int) {
        this.value += increment
    }

    // surcharge Counter++ et ++Counter
    operator fun inc() = Counter(value + 1)

    // surcharge Counter + Counter
    operator fun plus(other: Counter) = Counter(this.value + other.value)

    // surcharge Counter * Counter
    operator fun times(other: Counter) = Counter(this.value * other.value)

    // surcharge Counter * Int
    operator fun times(value: Int) = Counter(this.value * value)

    // surcharge Counter dans Counter
    operator fun contains(other: Counter) = other.value == this.value

    // surcharge Counter[Int] = Int
    operator fun set(index: Int, value: Int) {
        this.value = index + value
    }

    // surcharge Counter instance invocation
    operator fun invoke() = println("The value of the counter is $value")

}
/* Vous pouvez également surcharger les opérateurs par des méthodes d'extension */
// surcharge -Counter
operator fun Counter.unaryMinus() = Counter(-this.value)

fun operatorOverloadingDemo() {
    var counter1 = Counter(0)
    var counter2 = Counter(5)
    counter1 += 7
    println(counter1) // => Counter(value=7)
    println(counter1 + counter2) // => Counter(value=12)
    println(counter1 * counter2) // => Counter(value=35)
    println(counter2 * 2) // => Counter(value=10)
    println(counter1 in Counter(5)) // => false
    println(counter1 in Counter(7)) // => true
    counter1[26] = 10
    println(counter1) // => Counter(value=36)
    counter1() // => La valeur est 36
    println(-counter2) // => Counter(value=-5)
}
```

### Lectures complémentaires

* [Kotlin tutorials](https://kotlinlang.org/docs/tutorials/)
* [Try Kotlin in your browser](https://play.kotlinlang.org/)
* [A list of Kotlin resources](http://kotlin.link/)
