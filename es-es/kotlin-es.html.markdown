---
language: kotlin
contributors:
- ["S Webber", "https://github.com/s-webber"]
translators:
- ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
filename: LearnKotlin-es.kt
---

Kotlin es un lenguaje estático tipado para la JVM, Android y el navegador. Es
100% interoperable con Java.
[Leer mas aqui.](https://kotlinlang.org/)

```java
// Los comentarios de una sóla línea comienzan con //

/*
 Los comentarios multilínea lucen así
*/

// La palabra clave "package" funciona de la misma manera que Java.

/*
El punto de entrada para un programa de Kotlin es una función llamada "main".
A dicha función se le pasa un arreglo que contiene los argumentos de la linea de comando.
*/
fun main(args: Array<String>) {
    /*
    La declaración de valores se realiza utilizando tanto "var" como "val".
    Las declaraciones "val" no pueden ser reasignadas, mientras que "var" sí.
    */
    val fooVal = 10 // más adelante no podremos reasignar fooVal con un valor distinto.
    var fooVar = 10
    fooVar = 20 // fooVar puede ser reasignado

    /*
    En la mayoría de los casos, Kotlin puede determinar cuál es el tipo de una variable,
    de tal manera que no tenemos que especificarlo explícitamente cada vez.
    Podemos declarar explícitamente el tipo de una variable así:
    */
    val foo : Int = 7

    /*
    Las cadenas pueden ser representadas de la misma manera que Java.
    El escape de caracteres se realiza con una barra invertida.
    */
    val fooString = "Mi Cadena está aquí!";
    val barString = "¿Imprimiendo en una nueva línea?\nNo hay problema!";
    val bazString = "¿Quíeres agregar una tabulación?\tNo hay problema!";
    println(fooString);
    println(barString);
    println(bazString);

    /*
    Una cadena está delimitada por comillas triple (""").
    Estas cadenas pueden contener saltos de línea y otros caracteres.
    */
    val fooRawString = """
    fun helloWorld(val name : String) {
       println("Hola, mundo!")
    }
    """
    println(fooRawString)

    /*
    Las cadenas pueden contener interpolación de cadenas.
    La interpolación de cadenas comienza con un signo de dólar ($).
    */
    val fooTemplateString = "$fooString tiene ${fooString.length} caracteres"
    println(fooTemplateString)

    /*
    Para que una variable pueda aceptar valor nulo se debe especificar
    explícitamente como anulable añadiendole ? a su tipo.
    Podemos acceder a una variable anulable mediante el uso del operador ?.
    Podemos utilizar el operador ?: para especificar un valor alternativo
    a usar si una variable es nula.
    */
    var fooNullable: String? = "abc"
    println(fooNullable?.length) // => 3
    println(fooNullable?.length ?: -1) // => 3
    fooNullable = null
    println(fooNullable?.length) // => null
    println(fooNullable?.length ?: -1) // => -1


    /*
    Las funciones pueden ser declaras usando la palabra clave "fun".
    Los argumentos de las funciones son especificados entre corchetes despues del nombre de la función.
    Los argumentos de las funciones pueden tener opcionalmente un valor por defecto.
    El tipo de retorno de las funciones, de ser requerido, es especificado despues del argumento.
    */
    fun hello(name: String = "mundo") : String {
        return "Hola, $name!"
    }
    println(hello("foo")) // => Hola, foo!
    println(hello(name = "bar")) // => Hola, bar!
    println(hello()) // => Hola, mundo!

    /*
    Un parametro de la función puede ser marcado con la palabra clave "vararg"
    que permite que una función acepte un numero variable de argumentos.
    */
    fun varargExample(vararg names: Int) {
        println("Argument tiene ${names.size} elementos")
    }
    varargExample() // => Argument tiene 0 elementos
    varargExample(1) // => Argument tiene 1 elementos
    varargExample(1, 2, 3) // => Argument tiene 3 elementos

    /*
    Cuando una función consiste de una sola expresión entonces las llaves
    pueden ser omitidas. El cuerpo es especificado despues del símbolo =
    */
    fun odd(x: Int): Boolean = x % 2 == 1
    println(odd(6)) // => false
    println(odd(7)) // => true

    // Si el tipo de retorno puede ser inferido entonces no se necesita
    // especificarlo.
    fun even(x: Int) = x % 2 == 0
    println(even(6)) // => true
    println(even(7)) // => false

    // Las funciones pueden tomar funciones como argumentos y 
    // retornar funciones.
    fun not(f: (Int) -> Boolean) : (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }

    // Las funciones con nombre pueden ser especificadas como argumentos
    // utilizando el operador ::.
    val notOdd = not(::odd)
    val notEven = not(::even)
    // Las funciones anónimas pueden ser especificadas como argumentos.
    val notZero = not {n -> n == 0}
    /*
    Si una función anónima tiene un solo parametro entonces la declaración
    puede ser omitida (junto con ->). El nombre del único parametro será "it".
    */
    val notPositive = not {it > 0}
    for (i in 0..4) {
        println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
    }

    // La palabra clave "class" es usada para declarar clases.
    class ExampleClass(val x: Int) {
        fun memberFunction(y: Int) : Int {
            return x + y
        }

        infix fun infixMemberFunction(y: Int) : Int {
            return x * y
        }
    }
    /*
    Para crear una nueva instancia llamamos al constructor.
    Nótese que Kotlin no usa la palabra clave "new".
    */
    val fooExampleClass = ExampleClass(7)
    // Las funciones miembros pueden ser llamadas usando la notación de punto (.)
    println(fooExampleClass.memberFunction(4)) // => 11
    /*
    Si una función ha sido marcada con la palabra clave "infix" entonces
    esta puede ser invocada usando la notación infija.
    */
    println(fooExampleClass infixMemberFunction 4) // => 28

    /*
    Las clases "data" son una manera concisa de crear clases que solo contengan datos.
    Los metodos "hashCode"/"equals" y "toString" son generados automáticamente.
    */
    data class DataClassExample (val x: Int, val y: Int, val z: Int)
    val fooData = DataClassExample(1, 2, 4)
    println(fooData) // => DataClassExample(x=1, y=2, z=4)

    // las clases de datos tienen una función "copy".
    val fooCopy = fooData.copy(y = 100)
    println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

    // Los objetos pueden ser estructurados en múltiples variables.
    val (a, b, c) = fooCopy
    println("$a $b $c") // => 1 100 4

    // La función "with" es similar a la expresión de JavaScript "with".
    data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
    val fooMutableDate = MutableDataClassExample(7, 4, 9)
    with (fooMutableDate) {
        x -= 2
        y += 2
        z--
    }
    println(fooMutableDate) // => MutableDataClassExample(x=5, y=6, z=8)

    /*
    Podemos crear una lista utilizando la función "listOf".
    La lista será inmutable - los elementos no pueden ser añadidos o eliminados.
    */
    val fooList = listOf("a", "b", "c")
    println(fooList.size) // => 3
    println(fooList.first()) // => a
    println(fooList.last()) // => c
    // Los elementos de una lista se pueden acceder a través de su índice.
    println(fooList[1]) // => b

    // Una lista mutable puede ser creada usando la función "mutableListOf".
    val fooMutableList = mutableListOf("a", "b", "c")
    fooMutableList.add("d")
    println(fooMutableList.last()) // => d
    println(fooMutableList.size) // => 4

    // Podemos crear un set usando la función "setOf".
    val fooSet = setOf("a", "b", "c")
    println(fooSet.contains("a")) // => true
    println(fooSet.contains("z")) // => false

    // Podemos crear un mapa usando la función "mapOf".
    val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
    // Se puede acceder a los valores del mapa por su llave.
    println(fooMap["a"]) // => 8

    /*
    Las secuencias representan colecciones evaluadas diferidamente.
    Podemos crear una secuencia con la función "generateSequence".
    */
    val fooSequence = generateSequence(1, {it + 1})
    val x = fooSequence.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Un ejemplo usando las secuencias para generar los números de Fibonacci:
    fun fibonacciSequence() : Sequence<Long> {
        var a = 0L
        var b = 1L

        fun next() : Long {
            val result = a + b
            a = b
            b = result
            return a
        }

        return generateSequence(::next)
    }
    val y = fibonacciSequence().take(10).toList()
    println(y) // => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

    // Kotlin provee funciones de Orden-Mayor para trabajar con colecciones.
    val z = (1..9).map {it * 3}
            .filter {it < 20}
            .groupBy {it % 2 == 0}
            .mapKeys {if (it.key) "even" else "odd"}
    println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

    // Un bucle "for" puede ser usado con cualquier cosa que provea un iterador.
    for (c in "hello") {
        println(c)
    }

    // El bucle "while" funciona de la misma manera que en los demás lenguajes.
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
    "if" puede ser usado como una expresión que retorna un valor.
    Por esta razón el operador ternario ?: no es necesario en Kotlin.
    */
    val num = 5
    val message = if (num % 2 == 0) "even" else "odd"
    println("$num is $message") // => 5 is odd

    // "when" puede ser usado como alternativa a cadenas de "if-else if".
    val i = 10
    when {
        i < 7 -> println("primer bloque")
        fooString.startsWith("hello") -> println("segundo bloque")
        else -> println("else bloque")
    }

    // "when" puede ser usado con argumentos.
    when (i) {
        0, 21 -> println("0 or 21")
        in 1..20 -> println("in the range 1 to 20")
        else -> println("none of the above")
    }

    // "when" puede ser usado como una función que retorna un valor.
    var result = when (i) {
        0, 21 -> "0 or 21"
        in 1..20 -> "in the range 1 to 20"
        else -> "none of the above"
    }
    println(result)

    /*
    Podemos analizar si un objeto es de un tipo particular usando el operador "is".
    Si un objeto pasa un chequeo de tipo entonces éste se puede utilizar como
    ese tipo sin convertido de forma explícita.
     */
    fun smartCastExample(x: Any) : Boolean {
        if (x is Boolean) {
            // x es automaticamente convertido a Boolean
            return x
        } else if (x is Int) {
            // x es automaticamente convertido a Int
            return x > 0
        } else if (x is String) {
            // x  es automaticamente convertido a String
            return x.isNotEmpty()
        } else {
            return false
        }
    }
    println(smartCastExample("Hola, mundo!")) // => true
    println(smartCastExample("")) // => false
    println(smartCastExample(5)) // => true
    println(smartCastExample(0)) // => false
    println(smartCastExample(true)) // => true

    /*
    Las extensiones son una manera de añadir nuevas funcionalidades a una clase.
    Estas son similares a la extensión de métodos en C#.
     */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("Hola, mundo!".remove('l')) // => Hoa, mundo!

    println(EnumExample.A) // => A
    println(ObjectExample.hello()) // => hola
}

// Las clases "enum" son similares a los tipos "enum" de Java.
enum class EnumExample {
    A, B, C
}

/*
La palabra clave "object" se puede utilizar para crear objetos únicos.
No podemos asignarlo a una variable, pero podemos hacer referencia a ella por su nombre.
Esto es similar a los objetos únicos de Scala
*/
object ObjectExample {
    fun hello() : String {
        return "hola"
    }
}
```

### Lectura Adicional

* [Kotlin tutorials (EN)](https://kotlinlang.org/docs/tutorials/)
* [Try Kotlin in your browser (EN)](http://try.kotlinlang.org/)
* [A list of Kotlin resources (EN)](http://kotlin.link/)
