---
language: kotlin
filename: LearnKotlin-pt.kt
lang: pt-pt
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["André Martins", "https://github.com/chriptus13"]
---

Kotlin é uma linguagem de programação de tipificação estática para a JVM, Android e browser. Ela é 100% interoperável com Java.
[Lê mais aqui.](https://kotlinlang.org/)

```kotlin
// Comentários de linha começam com //
/*
Comentários de múltiplas linhas são assim.
*/

// A palavra-chave "package" funciona da mesma forma que em Java.
package com.learnxinyminutes.kotlin

/*
O ponto de entrada de um programa em Kotlin é a função chamada "main".
Esta função tem como único parâmetro um array contendo todos os argumentos passados na linha de comandos.
Desde a versão 1.3 que esta pode também ser definida sem parâmetros.
*/
fun main(args: Array<String>) {
	/*
	A declaração de variáveis é feita usando "var" ou "val".
	Variáveis declaradas com "val" não podem ser redefinidas, já as declaradas com "var" podem.
	*/
	val fooVal = 10 // não podemos redefinir mais tarde o valor de fooVal para algo diferente
	var fooVar = 10
	fooVar = 20 // fooVar pode ser redefinida

	/*
	Na maioria dos casos, o Kotlin pode determinar (inferir) o tipo de uma variável,
	assim não precisamos de o dizer explicitamente sempre.
	Para especificar o tipo explicitamente fazemos assim:
	*/
	val foo: Int = 7

	/*
	As Strings são representadas de uma forma semelhante ao Java.
	O escape é feito com barras invertidas.
	*/
	val fooString = "A minha String está aqui!"
	val barString = "Imprimir numa nova linha?\nSem problemas!"
	val bazString = "Adicionar um tab?\tSem problemas!"
	println(fooString)
	println(barString)
	println(bazString)

	/*
	Uma raw string é delimitada por aspas triplas (""").
	Raw strings podem conter caracteres de nova linha ou qualquer outro.
	*/
	val fooRawString = """
fun helloWorld(val name : String) {
	println("Hello, world!")
}
"""
	println(fooRawString)

	/*
	As strings podem também conter template expressions.
	Uma template expression começa com o símbolo do dollar ($).
	*/
	val fooTemplateString = "$fooString tem ${fooString.length} caracteres"
	println(fooTemplateString) // => A minha String está aqui! tem 25 caracteres

	/*
	Para que uma variável possa ter o valor de null esta tem de ser 
	especificada explicitamente como nullable.
	Uma variável pode ser marcada como nullable adicionando um ? ao seu tipo.
	A variable can be specified as nullable by appending a ? to its type.
	Usando o operador ?. podemos facilmente aceder a propriedades de 
	uma variável nullable, se esta for null o resultado da expressão será também ele null.
	Podemos também usar o operador ?: para especificar um valor alternativo
	no caso da variavél ser null.
	*/
	var fooNullable: String? = "abc"
	println(fooNullable?.length) // => 3
	println(fooNullable?.length ?: -1) // => 3
	fooNullable = null
	println(fooNullable?.length) // => null
	println(fooNullable?.length ?: -1) // => -1

	/*
	As funções são declaradas usando a palavra-chave "fun".
	Os parâmetros da função são especificados entre parênteses a seguir ao nome da função.
	Estes parâmetros podem opcionalmente ter um valor por omissão.
	O tipo de retorno da função, se necessário, é especificado após os parâmetros.
	*/
	fun hello(name: String = "world"): String {
		return "Hello, $name!"
	}
	println(hello("foo")) // => Hello, foo!
	println(hello(name = "bar")) // => Hello, bar!
	println(hello()) // => Hello, world!

	/*
	Para que uma função receba um número variável de parâmetros podemos
	marcar um, e apenas um, parâmetro com a palavra-chave "vararg".
	*/
	fun varargExample(vararg names: Int) {
		println("Argument has ${names.size} elements")
	}
	varargExample() // => A chamada à função tem 0 argumentos
	varargExample(1) // => A chamada à função tem 1 argumentos
	varargExample(1, 2, 3) // => A chamada à função tem 3 argumentos

	/*
	Quando uma função consiste em apenas uma expressão as chavetas podem ser omitidas
	O corpo da mesma é especificado após o símbolo de igual (=).
	*/
	fun odd(x: Int): Boolean = x % 2 == 1
	println(odd(6)) // => false
	println(odd(7)) // => true

	// Se o tipo de retorno da função pode ser inferido então não é necessário especificá-lo.
	fun even(x: Int) = x % 2 == 0
	println(even(6)) // => true
	println(even(7)) // => false

	// As funções podem ter outras funções como parâmetros e/ou como retorno.
	fun not(f: (Int) -> Boolean): (Int) -> Boolean {
		return {n -> !f.invoke(n)}
	}
	// O operador :: pode ser usado para referênciar funções existentes.
	val notOdd = not(::odd)
	val notEven = not(::even)
	/*
	Expressões lambda podem ser usadas da seguinte forma.
	Os lambdas quando passados a outras funções podem estar 
	fora dos parênteses da chamada, caso sejam o último parâmetro.
	*/
	val notZero = not {n -> n == 0}
	/*
	Se o lambda apenas tiver um parâmetro então a sua
	declaração pode ser omitida (em conjunto com "->").
	O nome por omissão do parâmetro será "it".
	*/
	val notPositive = not {it > 0}
	for (i in 0..4) {
		println("${notOdd(i)} ${notEven(i)} ${notZero(i)} ${notPositive(i)}")
	}

	// Para declararmos classes usa-se a palavra-chave "class".
	class ExampleClass(val x: Int) {
		fun aMethod(y: Int): Int {
			return x + y
		}

		infix fun infixMemberFunction(y: Int): Int {
			return x * y
		}
	}
	/*
	Para se instanciar uma classe usamos o constructor.
	De notar que em Kotlin não existe a palavra-chave "new" como no Java.
	*/
	val fooExampleClass = ExampleClass(7)
	// Os métodos da classe podem então ser chamados usando o ponto.
	println(fooExampleClass.aMethod(4)) // => 11
	/*
	Uma função marcada com a palavra-chave "infix" pode ser chamada
	usando a notação infixa.
	*/
	println(fooExampleClass infixMemberFunction 4) // => 28

	/*
	Data classes são uma forma concisa de criar classes que apenas contêm dados.
	Neste tipo de classes os métodos "hashCode"/"equals" e "toString" são gerados
	automáticamente.
	*/
	data class DataClassExample (val x: Int, val y: Int, val z: Int)
	val fooData = DataClassExample(1, 2, 4)
	println(fooData) // => DataClassExample(x=1, y=2, z=4)

	// Instâncias deste tipo de classes têm acesso ao método "copy".
	val fooCopy = fooData.copy(y = 100)
	println(fooCopy) // => DataClassExample(x=1, y=100, z=4)

	// Os objectos podem ser desconstruídos para variáveis.
	val (a, b, c) = fooCopy
	println("$a $b $c") // => 1 100 4

	// desconstrucção dentro de um ciclo "for"
	for ((a, b, c) in listOf(fooData)) {
		println("$a $b $c") // => 1 2 4
	}

	val mapData = mapOf("a" to 1, "b" to 2)
	// Instâncias de Map.Entry podem também ser desconstruídas.
	for ((key, value) in mapData) {
		println("$key -> $value")
	}

	// A função "with" é semelhante ao bloco "with" do JavaScript.
	data class MutableDataClassExample (var x: Int, var y: Int, var z: Int)
	val fooMutableData = MutableDataClassExample(7, 4, 9)
	with (fooMutableData) {
		x -= 2
		y += 2
		z--
	}
	println(fooMutableData) // => MutableDataClassExample(x=5, y=6, z=8)

	/*
	Podemos criar listas usando a função "listOf".
	No Kotlin, por padrão, as listas são imútaveis - não podendo 
	assim adicionar ou remover elementos das mesmas.
	*/
	val fooList = listOf("a", "b", "c")
	println(fooList.size) // => 3
	println(fooList.first()) // => a
	println(fooList.last()) // => c
	// Os elementos de uma lista podem ser acedidos usando o seu índice.
	println(fooList[1]) // => b

	// Listas mútaveis podem ser criadas usando a função "mutableListOf".
	val fooMutableList = mutableListOf("a", "b", "c")
	fooMutableList.add("d")
	println(fooMutableList.last()) // => d
	println(fooMutableList.size) // => 4

	// Podemos criar conjuntos usando a função "setOf".
	val fooSet = setOf("a", "b", "c")
	println(fooSet.contains("a")) // => true
	println(fooSet.contains("z")) // => false

	// Podemos criar mapas usando a função "mapOf" e através da função infixa "to".
	val fooMap = mapOf("a" to 8, "b" to 7, "c" to 9)
	// Os valores do mapa podem ser acedidos usando a sua chave.
	println(fooMap["a"]) // => 8

	/*
	No Kotlin as sequências representam collecções de dados avaliadas de forma lazy.
	Podemos cirar uma sequência usando a função "generateSequence".
	*/
	val fooSequence = generateSequence(1, { it + 1 })
	val x = fooSequence.take(10).toList()
	println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

	// Um exemplo de uso das sequências para gerar os números de Fibonacci:
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

	// O Kotlin fornece funções de ordem superior convenientes para a manipulação de colecções.
	val z = (1..9).map {it * 3}
				  .filter {it < 20}
				  .groupBy {it % 2 == 0}
				  .mapKeys {if (it.key) "even" else "odd"}
	println(z) // => {odd=[3, 9, 15], even=[6, 12, 18]}

	// Um ciclo "for" pode ser usado com qualquer coisa que forneça um iterador.
	for (c in "hello") {
		println(c)
	}

	// Um ciclo "while" funciona da mesma forma que em outras linguagens.
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
	Um "if" pode ser usado como uma expressão que produz um valor.
	Por esta razão o operador ternário não é necessário no Kotlin.
	*/
	val num = 5
	val message = if (num % 2 == 0) "even" else "odd"
	println("$num is $message") // => 5 is odd

	// O bloco "when" pode ser usado como alternativa para cadeias de "if-else if".
	val i = 10
	when {
		i < 7 -> println("first block")
		fooString.startsWith("hello") -> println("second block")
		else -> println("else block")
	}

	// O "when" pode ser usado como um "switch" do Java.
	when (i) {
		0, 21 -> println("0 or 21")
		in 1..20 -> println("in the range 1 to 20")
		else -> println("none of the above")
	}

	// O "when" pode também ser usado como expressão para produzir um valor.
	var result = when (i) {
		0, 21 -> "0 or 21"
		in 1..20 -> "in the range 1 to 20"
		else -> "none of the above"
	}
	println(result)

	/*
	Podemos utilizar o operador "is" para verificar se um objecto é de um certo tipo.
	Se um objecto passar a verificação do tipo pode ser usado como sendo desse tipo
	sem conversão explicita, sendo isto chamado de smart cast.
	*/
	fun smartCastExample(x: Any) : Boolean {
		if (x is Boolean) {
			// x is automatically cast to Boolean
			return x
		} else if (x is Int) {
			// x is automatically cast to Int
			return x > 0
		} else if (x is String) {
			// x is automatically cast to String
			return x.isNotEmpty()
		} else {
			return false
		}
	}
	println(smartCastExample("Hello, world!")) // => true
	println(smartCastExample("")) // => false
	println(smartCastExample(5)) // => true
	println(smartCastExample(0)) // => false
	println(smartCastExample(true)) // => true

	// Os smart casts funcionam também com o bloco "when".
	fun smartCastWhenExample(x: Any) = when (x) {
		is Boolean -> x
		is Int -> x > 0
		is String -> x.isNotEmpty()
		else -> false
	}

	/*
	Extensões são uma forma de adicionar funcionalidade a classes existentes.
	Isto é semelhante aos métodos de extensão do C#.
	*/
	fun String.remove(c: Char): String {
		return this.filter {it != c}
	}
	println("Hello, world!".remove('l')) // => Hello, world!
}

// Enum classes são o equivalente aos tipos enum do Java.
enum class EnumExample {
	A, B, C // As constantes da enumeração são separadas por vírgula.
}
fun printEnum() = println(EnumExample.A) // => A

/*
Como cada constante é uma instância da classe enum, 
estas podem ser inicializadas da seguinte forma:
*/
enum class EnumExample(val value: Int) {
	A(value = 1),
	B(value = 2),
	C(value = 3)
}
fun printProperty() = println(EnumExample.A.value) // => 1

/*
Cada constante de enumerações tem propriedades para 
obter o nome e o ordinal (posição) na respectiva classe.
*/
fun printName() = println(EnumExample.A.name) // => A
fun printPosition() = println(EnumExample.A.ordinal) // => 0

/*
A palavra-chave "object" pode ser usada para criar objectos singleton.
Estes não podem ser instânciados, porém podem ser referênciados como uma
única instância através do seu nome.
São semelhantes aos objectos singleton do Scala.
*/
object ObjectExample {
	fun hello(): String {
		return "hello"
	}

	override fun toString(): String {
		return "Hello, it's me, ${ObjectExample::class.simpleName}"
	}
}


fun useSingletonObject() {
	println(ObjectExample.hello()) // => hello
	// Em Kotlin o tipo "Any" é a raíz da hierárquia de classes, tal como o tipo "Object" em Java.
	val someRef: Any = ObjectExample
	println(someRef) // => Hello, it's me, ObjectExample
}


/* 
O operador !! serve para realizar um assert de not-null. Este converte qualquer 
valor nullable para non-null ou lança exceção se o mesmo for null.
*/
var b: String? = "abc"
val l = b!!.length	// lançaria exceção caso "b" fosse null

// O modificador "operator" permite fazer overload dos operadores 
// [Ver lista de operadores](https://kotlinlang.org/docs/operator-overloading.html)
data class Counter(var value: Int) {
	// overload para Counter += Int
	operator fun plusAssign(increment: Int) {
		this.value += increment
	}

	// overload para Counter++ e ++Counter
	operator fun inc() = Counter(value + 1)

	// overload para Counter + Counter
	operator fun plus(other: Counter) = Counter(this.value + other.value)

	// overload para Counter * Counter
	operator fun times(other: Counter) = Counter(this.value * other.value)

	// overload para Counter * Int
	operator fun times(value: Int) = Counter(this.value * value)

	// overload para Counter in Counter
	operator fun contains(other: Counter) = other.value == this.value

	// overload para Counter[Int] = Int
	operator fun set(index: Int, value: Int) {
		this.value = index + value
	}

	// overload para invocação da instância Counter
	operator fun invoke() = println("The value of the counter is $value")
}
/* Podemos também dar overload dos operadores através de métodos de extensão */
// overload para -Counter
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
	counter1() // => The value of the counter is 36
	println(-counter2) // => Counter(value=-5)
}
```

### Leituras Adicionais

* [Tutoriais de Kotlin](https://kotlinlang.org/docs/tutorials/)
* [Experimenta Kotlin no browser](https://play.kotlinlang.org/)
* [Recursos adicionais](http://kotlin.link/)
