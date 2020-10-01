---
language: kotlin
filename: LearnKotlin-pt.kt
contributors:
    - ["S Webber", "https://github.com/s-webber"]
translators:
    - ["Márcio Torres", "https://github.com/marciojrtorres"]
lang: pt-br
---

Kotlin é uma linguagem de programação estaticamente tipada para a JVM, Android e navegadores web. Ela é 100% interoperável com Java.
[Leia mais aqui.](https://kotlinlang.org/)

```kotlin
// Comentários de uma linha iniciam com //
/*
Comentários multilinha se parecem com este.
*/

// A palavra-chave "package" funciona do mesmo modo que no Java.
package com.learnxinyminutes.kotlin

/*
O ponto de entrada para um programa em Kotlin é uma função chamada "main"
Esta função recebe um vetor contendo quaisquer argumentos da linha de comando
*/
fun main(args: Array<String>) {
    /*
    A declaração de valores pode ser feita tanto com "var" como "val"
    Declarações com "val" não podem ser reatribuídas, enquanto com "var" podem.
    */
    val umVal = 10 // não se poderá reatribuir qualquer coisa a umVal
    var umVar = 10
    umVar = 20 // umVar pode ser reatribuída, mas respeitando o tipo

    /*
    Na maioria dos casos Kotlin pode inferir o tipo, então não é preciso sempre
    especificar o tipo explicitamente, mas quando o fazemos é assim:
    */
    val umInteiro: Int = 7

    /*
    Strings podem ser representadas de forma semelhante a Java.
    A contrabarra realiza o "escape", da mesma forma.
    */
    val umaString = "Minha String está aqui!"
    val outraString = "Imprimir na outra linha?\nSem problema!"
    val maisString = "Você quer adicionar um tab?\tSem problema!"
    println(umaString)
    println(outraString)
    println(maisString)

    /*
    Uma string bruta é delimitada com três aspas (""").
    Strings brutas podem conter novas linhas e outros caracteres.
    */
    val umaStringBruta = """
fun olaMundo(val nome : String) {
   println("Olá, mundo!")
}
"""
    println(umaStringBruta)

    /*
    As strings podem conter expressões modelo (template).
    Uma expressão modelo começa com um cifrão ($).
    É semelhante à interpolação de Strings em Ruby.
    */
    val umaStringModelo = "$umaString tem ${umaString.length} caracteres"
    println(umaStringModelo)

    /*
    Para uma variável receber null deve-se explicitamente declará-la
    como anulável.
    A declaração de anulável é realizada incluindo uma "?" ao fim do tipo.
    Pode-se acessar uma variável anulável usando o operador "?."
    Usa-se o operador "?:" (também conhecido como operador Elvis) para
    atribuir um valor alternativo para quando uma variável é nula.
    */
    var umaVariavelAnulavel: String? = "abc"
    println(umaVariavelAnulavel?.length) // => 3
    println(umaVariavelAnulavel?.length ?: -1) // => 3
    umaVariavelAnulavel = null
    println(umaVariavelAnulavel?.length) // => null
    println(umaVariavelAnulavel?.length ?: -1) // => -1

    /*
    Funções podem ser declaradas usando a palavra-chave "fun"
    Os parâmetros da função são declarados entre parênteses logo
    após o nome da função.
    Os parâmetros da função podem ter opcionalmente um valor padrão.
    O tipo de retorno da função, se necessário, é especificado após os argumentos.
    */
    fun ola(nome: String = "mundo"): String {
        return "Olá, $nome!"
    }
    println(ola("você")) // => Olá, você!
    println(ola(nome = "tu")) // => Olá, tu!
    println(ola()) // => Olá, mundo!

    /*
    Um parâmetro pode ser declarado com a palavra-chave "vararg" para
    permitir que seja passado um número variável de argumentos.
    */
    fun exemploVarArg(vararg numeros: Int) {
        println("Foram recebidos ${numeros.size} argumentos")
    }
    exemploVarArg() // => Passando nenhum argumento (0 argumentos)
    exemploVarArg(1) // => Passando 1 argumento
    exemploVarArg(1, 2, 3) // => Passando 3 argumentos

    /*
    Quando uma função consiste numa única expressão as chaves
    podem ser omitidas e o corpo declarado após o símbolo de "="
    */
    fun impar(x: Int): Boolean = x % 2 == 1
    println(impar(6)) // => false
    println(impar(7)) // => true

    // O tipo de retorno não precisa ser declarado se pode ser inferido.
    fun impar(x: Int) = x % 2 == 0
    println(impar(6)) // => true
    println(impar(7)) // => false

    // Funções podem receber e retornar outras funções
    fun nao(f: (Int) -> Boolean): (Int) -> Boolean {
        return {n -> !f.invoke(n)}
    }
    // Funções nomeadas podem ser passadas como argumento usando o operador "::"
    val naoImpar = nao(::impar)
    val naoPar = nao(::par)
    // Expressões Lambda podem ser usadas como argumentos
    val naoZero = nao {n -> n == 0}
    /*
    Se uma lambda têm apenas um parâmetro sua declaração pode ser omitida,
    incluindo o símbolo "->".
    Neste caso o nome do único parâmetro deve ser "it".
    */
    val naoPositivo = nao {it > 0}
    for (i in 0..4) {
        println("${naoImpar(i)} ${naoPar(i)} ${naoZero(i)} ${naoPositivo(i)}")
    }

    // A palavra-chave "class" é usada para declarar classes
    class ClasseExemplo(val x: Int) {
        fun funcaoMembro(y: Int): Int { // ou "método"
            return x + y
        }

        infix fun funcaoMembroInfixa(y: Int): Int {
            return x * y
        }
    }
    /*
    Para criar uma nova instância chama-se o construtor.
    Note que Kotlin não tem a palavra-chave "new".
    */
    val umaInstanciaDaClasseExemplo = ClasseExemplo(7)
    // Funções membro (métodos) podem ser chamados usando a notação ponto "."
    println(umaInstanciaDaClasseExemplo.funcaoMembro(4)) // => 11
    /*
    Se uma função foi declarada com a palavra-chave "infix" então
    ela pode ser invocada com a notação infixa.
    */
    println(umaInstanciaDaClasseExemplo funcaoMembroInfixa 4) // => 28

    /*
    Classes de dados são um modo sucinto de criar classes que servem apenas
    para guardar informações.
    Os métodos "hashCode", "equals" e "toString" são gerados automaticamente.
    */
    data class ExemploClasseDados (val x: Int, val y: Int, val z: Int)
    val objetoDados = ExemploClasseDados(1, 2, 4)
    println(objetoDados) // => ExemploClasseDados(x=1, y=2, z=4)

    // Classes de dados têm uma função "copy"
    val dadosCopia = objetoDados.copy(y = 100)
    println(dadosCopia) // => ExemploClasseDados(x=1, y=100, z=4)

    // Objetos podem ser desestruturados em múltiplas variáveis.
    val (a, b, c) = dadosCopia
    println("$a $b $c") // => 1 100 4

    // desestruturando em um laço "for"
    for ((a, b, c) in listOf(objetoDados)) {
        println("$a $b $c") // => 1 100 4
    }

    val mapaDados = mapOf("a" to 1, "b" to 2)
    // Map.Entry também é desestruturável
    for ((chave, valor) in mapaDados) {
        println("$chave -> $valor")
    }

    // A função "with" é semelhante à declaração "with" do JavaScript
    data class ExemploClasseDadosMutaveis (var x: Int, var y: Int, var z: Int)
    val objDadosMutaveis = ExemploClasseDadosMutaveis(7, 4, 9)
    with (objDadosMutaveis) {
        x -= 2
        y += 2
        z--
    }
    println(objDadosMutaveis) // => ExemploClasseDadosMutaveis(x=5, y=6, z=8)

    /*
    Pode-se criar uma lista usando a função "listOf".
    A lista é imutável, isto é, elementos não podem ser adicionados ou removidos.
    */
    val umaLista = listOf("a", "b", "c")
    println(umaLista.size) // => 3
    println(umaLista.first()) // => a
    println(umaLista.last()) // => c
    // Elementos de uma lista podem ser acessados pelo índice
    println(umaLista[1]) // => b

    // Uma lista mutável pode ser criada com a função "mutableListOf".
    val umaListaMutavel = mutableListOf("a", "b", "c")
    umaListaMutavel.add("d")
    println(umaListaMutavel.last()) // => d
    println(umaListaMutavel.size) // => 4

    // Similarmente, pode-se criar um conjunto com a função "setOf".
    val umConjunto = setOf("a", "b", "c")
    println(umConjunto.contains("a")) // => true
    println(umConjunto.contains("z")) // => false

    // Da mesma forma que um mapa com a função "mapOf".
    val umMapa = mapOf("a" to 8, "b" to 7, "c" to 9)
    // Os valores contidos no mapa podem ser acessados pela sua chave.
    println(umMapa["a"]) // => 8

    /*
    Sequências representam coleções avaliadas "preguiçosamente" (sob demanda).
    Pode-se criar uma sequência usando a função "generateSequence".
    */
    val umaSequencia = generateSequence(1, { it + 1 })
    val x = umaSequencia.take(10).toList()
    println(x) // => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    // Um exemplo de uma sequência usada para gerar Números de Fibonacci:
    fun sequenciaFibonacci(): Sequence<Long> {
        var a = 0L
        var b = 1L

        fun proximo(): Long {
            val resultado = a + b
            a = b
            b = resultado
            return a
        }

        return generateSequence(::proximo)
    }
    val y = sequenciaFibonacci().take(10).toList()
    println(y) // => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

    // Kotlin oferece funções de alta-ordem para trabalhar com coleções.
    val z = (1..9).map {it * 3}
                  .filter {it < 20}
                  .groupBy {it % 2 == 0}
                  .mapKeys {if (it.key) "par" else "impar"}
    println(z) // => {impar=[3, 9, 15], par=[6, 12, 18]}

    // Um "for" pode ser usado com qualquer coisa que ofereça um "iterator"
    for (c in "salve") {
        println(c)
    }

    // O "while" funciona da mesma forma que em outras linguagens.
    var contador = 0
    while (contador < 5) {
        println(contador)
        contador++
    }
    do {
        println(contador)
        contador++
    } while (contador < 10)

    /*
    "if" pode ser usado como uma expressão que retorna um valor.
    Por este motivo o operador ternário "? :" não é necessário em Kotlin.
    */
    val numero = 5
    val mensagem = if (numero % 2 == 0) "par" else "impar"
    println("$numero é $mensagem") // => 5 é impar

    // "when" pode ser usado como alternativa às correntes de "if-else if".
    val i = 10
    when {
        i < 7 -> println("primeiro block")
        umaString.startsWith("oi") -> println("segundo block")
        else -> println("bloco else")
    }

    // "when" pode ser usado com um argumento.
    when (i) {
        0, 21 -> println("0 ou 21")
        in 1..20 -> println("entre 1 e 20")
        else -> println("nenhum dos anteriores")
    }

    // "when" pode ser usada como uma função que retorna um valor.
    var resultado = when (i) {
        0, 21 -> "0 ou 21"
        in 1..20 -> "entre 1 e 20"
        else -> "nenhum dos anteriores"
    }
    println(resultado)

    /*
    Pode-se verificar se um objeto é de um certo tipo usando o operador "is".
    Se o objeto passar pela verificação então ele pode ser usado como
    este tipo, sem a necessidade de uma coerção (cast) explícita (SmartCast).
    */
    fun exemploSmartCast(x: Any) : Boolean {
        if (x is Boolean) {
            // x é automaticamente coagido para Boolean
            return x
        } else if (x is Int) {
            // x é automaticamente coagido para Int
            return x > 0
        } else if (x is String) {
            // x é automaticamente coagido para String
            return x.isNotEmpty()
        } else {
            return false
        }
    }
    println(exemploSmartCast("Olá, mundo!")) // => true
    println(exemploSmartCast("")) // => false
    println(exemploSmartCast(5)) // => true
    println(exemploSmartCast(0)) // => false
    println(exemploSmartCast(true)) // => true

    // O Smartcast também funciona com blocos "when"
    fun exemploSmartCastComWhen(x: Any) = when (x) {
        is Boolean -> x
        is Int -> x > 0
        is String -> x.isNotEmpty()
        else -> false
    }

    /*
    As extensões são uma maneira nova de adicionar funcionalidades a classes.
    Elas são similares aos "extension methods" da linguagem C#.
    */
    fun String.remove(c: Char): String {
        return this.filter {it != c}
    }
    println("olá, mundo!".remove('o')) // => lá, mund!

    println(ExemploEnum.A) // => A
    println(ExemploObjeto.ola()) // => olá
}

// Classes Enum são similares aos "enum types" do Java.
enum class ExemploEnum {
    A, B, C
}

/*
A palavra-chave "object" pode ser usar para criar Singletons.
Eles não são instanciados, mas podem referenciar sua instância única pelo nome.
É semelhante aos "singleton objects" da linguagem Scala.
*/
object ExemploObjeto {
    fun ola(): String {
        return "olá"
    }
}

fun usaObjeto() {
    ExemploObjeto.ola()
    val algumaReferencia: Any = ExemploObjeto // usa-se o nome diretamente
}

```

### Leitura Adicional

* [Tutoriais de Kotlin](https://kotlinlang.org/docs/tutorials/)(EN)
* [Experimente Kotlin no seu navegador](http://try.kotlinlang.org/)(EN)
* [Uma lista de material sobre Kotlin](http://kotlin.link/)(EN)
