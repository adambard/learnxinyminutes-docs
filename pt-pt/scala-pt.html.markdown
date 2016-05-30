---
language: Scala
filename: learnscala-pt.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Ha-Duong Nguyen", "http://reference-error.org"]
translators:
    - ["João Costa", "http://joaocosta.eu"]
lang: pt-pt
---

Scala - a linguagem escalável

```scala

/*
  Prepare tudo:

  1) Faça Download do Scala - http://www.scala-lang.org/downloads
  2) Faça unzip/untar para onde preferir e coloque o subdirectório `bin` na
     variável de ambiente `PATH`
  3) Inicie a REPL de Scala correndo o comando `scala`. Deve aparecer:

  scala>

  Isto é chamado de REPL (Read-Eval-Print Loop / Lê-Avalia-Imprime Repete).
  Pode escrever qualquer expressão de Scala e o resultado será imprimido.
  Vamos mostrar ficheiros de Scala mais à frente neste tutorial mas, para já,
  vamos começar com os básicos.

*/


/////////////////////////////////////////////////
// 1. Basicos
/////////////////////////////////////////////////

// Uma linha de comentários é marcada com duas barras

/*
  Comentários de multiplas linhas, como se pode ver neste exemplo, são assim.
*/

// Imprimir, forçando uma nova linha no final
println("Hello world!")
println(10)

// Imprimir, sem forçar uma nova linha no final
print("Hello world")

// Valores são declarados com var ou val.
// As declarações val são imutáveis, enquanto que vars são mutáveis.
// A immutabilidade é uma propriedade geralmente vantajosa.
val x = 10 // x é agora 10
x = 20     // erro: reatribuição de um val
var y = 10
y = 20     // y é agora 12

/*
  Scala é uma linguagem estaticamente tipada, no entanto, nas declarações acima
  não especificamos um tipo. Isto é devido a uma funcionalidade chamada
  inferência de tipos. Na maior parte dos casos, o compilador de scala consegue
  inferir qual o tipo de uma variável, pelo que não o temos de o declarar sempre.
  Podemos declarar o tipo de uma variável da seguinte forma:
*/
val z: Int = 10
val a: Double = 1.0

// Note a conversão automática de Int para Double: o resultado é 10.0, não 10
val b: Double = 10

// Valores booleanos
true
false

// Operações booleanas
!true         // false
!false        // true
true == false // false
10 > 5        // true

// A matemática funciona da maneira habitual
1 + 1   // 2
2 - 1   // 1
5 * 3   // 15
6 / 2   // 3
6 / 4   // 1
6.0 / 4 // 1.5


// Avaliar expressões na REPL dá o tipo e valor do resultado

1 + 7

/* A linha acima resulta em:

  scala> 1 + 7
  res29: Int = 8

  Isto significa que o resultado de avaliar 1 + 7 é um objecto do tipo Int com
  o valor 8.

  Note que "res29" é um nome de uma variavel gerado sequencialmente para
  armazenar os resultados das expressões que escreveu, por isso o resultado
  pode ser ligeiramente diferente.
*/

"Strings em scala são rodeadas por aspas"
'a' // Um caracter de Scala
// 'Strings entre plicas não existem' <= Isto causa um erro

// Strings tem os métodos de Java habituais definidos
"olá mundo".length
"olá mundo".substring(2, 6)
"olá mundo".replace("á", "é")

// Para além disso, também possuem métodos de Scala.
// Ver: scala.collection.immutable.StringOps
"olá mundo".take(5)
"olá mundo".drop(5)

// Interpolação de Strings: repare no prefixo "s"
val n = 45
s"Temos $n maçãs" // => "Temos 45 maçãs"

// Expressões dentro de Strings interpoladas também são possíveis
val a = Array(11, 9, 6)
s"A minha segunda filha tem ${a(0) - a(2)} anos." // => "A minha segunda filha tem 5 anos."
s"Temos o dobro de ${n / 2.0} em maçãs."          // => "Temos o dobro de 22.5 em maçãs."
s"Potência de 2: ${math.pow(2, 2)}"               // => "Potência de 2: 4"

// Strings interpoladas são formatadas com o prefixo "f"
f"Potência de 5: ${math.pow(5, 2)}%1.0f"     // "Potência de 5: 25"
f"Raíz quadrada 122: ${math.sqrt(122)}%1.4f" // "Raíz quadrada de 122: 11.0454"

// Strings prefixadas com "raw" ignoram caracteres especiais
raw"Nova linha: \n. Retorno: \r." // => "Nova Linha: \n. Retorno: \r."

// Alguns caracteres tem de ser "escapados", e.g. uma aspa dentro de uma string:
"Esperaram fora do  \"Rose and Crown\"" // => "Esperaram fora do "Rose and Crown""

// Strings rodeadas por três aspas podem-se estender por varias linhas e conter aspas
val html = """<form id="daform">
                <p>Carrega aqui, Zé</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. Funções
/////////////////////////////////////////////////

// Funções são definidas como:
//
//   def nomeDaFuncao(args...): TipoDeRetorno = { corpo... }
//
// Se vem de linugagens mais tradicionais, repare na omissão da palavra
// return keyword. Em Scala, a ultima expressão de um bloco é o seu
// valor de retorno
def somaQuadrados(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// As { } podem ser omitidas se o corpo da função for apenas uma expressão:
def somaQuadradosCurto(x: Int, y: Int): Int = x * x + y * y

// A sintaxe para chamar funções deve ser familiar:
somaQuadrados(3, 4)  // => 25

// Na maior parte dos casos (sendo funções recursivas a principal excepção), o
// tipo de retorno da função pode ser omitido, sendo que a inferencia de tipos
// é aplicada aos valores de retorno
def quadrado(x: Int) = x * x  // O compilador infere o tipo de retorno Int

// Funções podem ter parâmetros por omissão:
def somaComOmissão(x: Int, y: Int = 5) = x + y
somaComOmissão(1, 2) // => 3
somaComOmissão(1)    // => 6


// Funções anónimas são definidas da seguinte forma:
(x: Int) => x * x

// Ao contrário de defs, o tipo de input de funções anónimas pode ser omitido
// se o contexto o tornar óbvio. Note que o tipo "Int => Int" representa uma
// funão que recebe Int e retorna Int.
val quadrado: Int => Int = x => x * x

// Funcões anónimas são chamadas como funções normais:
quadrado(10)   // => 100

// Se cada argumento de uma função anónima for usado apenas uma vez, existe
// uma forma ainda mais curta de os definir. Estas funções anónumas são
// extremamente comuns, como será visto na secção sobre estruturas de dados.
val somaUm: Int => Int = _ + 1
val somaEstranha: (Int, Int) => Int = (_ * 2 + _ * 3)

somaUm(5)          // => 6
somaEstranha(2, 4) // => 16


// O código return existe em Scala, mas apenas retorna do def mais interior
// que o rodeia.
// AVISO: Usar return em Scala deve ser evitado, pois facilmente leva a erros.
// Não tem qualquer efeito em funções anónimas, por exemplo:
def foo(x: Int): Int = {
  val funcAnon: Int => Int = { z =>
    if (z > 5)
      return z // Esta linha faz com que z seja o retorno de foo!
    else
      z + 2    // Esta linha define o retorno de funcAnon
  }
  funcAnon(x)  // Esta linha define o valor de retorno de foo
}


/////////////////////////////////////////////////
// 3. Controlo de fluxo
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach(println)

r foreach println
// NB: Scala é bastante brando no que toca a pontos e parentisis - estude as
// regras separadamente. Isto permite escrever APIs e DSLs bastante legiveis

(5 to 1 by -1) foreach (println)

// Ciclos while
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }   // Sim, outra vez. O que aconteceu? Porquê?

i    // Mostra o valor de i. Note que o while é um ciclo no sentido clássico -
     // executa sequencialmente enquanto muda uma variável. Ciclos while são
     // rápidos, por vezes até mais que ciclos de Java, mas combinadores e
     // compreensões (usados anteriormente) são mais fáceis de entender e
     // paralelizar

// Um ciclo do while
i = 0
do {
  println("i ainda é menor que 10")
  i += 1
} while (i < 10)

// A forma idiomática em Scala de definir acções recorrentes é através de
// recursão em cauda.
// Funções recursivas necessitam de um tipo de retorno definido explicitamente.
// Neste caso, é Unit.
def mostraNumerosEntre(a: Int, b: Int): Unit = {
  print(a)
  if (a < b)
    mostraNumerosEntre(a + 1, b)
}
mostraNumerosEntre(1, 14)


// Condicionais

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println ("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


/////////////////////////////////////////////////
// 4. Estruturas de dados
/////////////////////////////////////////////////

val a = Array(1, 2, 3, 5, 8, 13)
a(0)
a(3)
a(21)    // Lança uma excepção

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")
m("spoon")
m("bottle")       // Lança uma excepção

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")

val s = Set(1, 3, 7)
s(0)
s(1)

/* Veja a documentação de mapas de scala em -
 * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map
 * e verifique que a consegue aceder
 */


// Tuplos

(1, 2)

(4, 3, 2)

(1, 2, "três")

(a, 2, "três")

// Porquê ter isto?
val divideInts = (x: Int, y: Int) => (x / y, x % y)

divideInts(10, 3) // A função divideInts returna o resultado e o resto

// Para aceder aos elementos de um tuplo, pode-se usar _._n, onde n é o indice
// (começado em 1) do elemento
val d = divideInts(10, 3)

d._1

d._2


/////////////////////////////////////////////////
// 5. Programação Orientada a Objectos
/////////////////////////////////////////////////

/*
  Aparte: Até agora tudo o que fizemos neste tutorial foram expressões simples
  (valores, funções, etc). Estas expressões são suficientes para executar no
  interpretador da linha de comandos para testes rápidos, mas não podem existir
  isoladas num ficheiro de Scala. Por exemplo, não é possivel correr um
  ficheiro scala que apenas contenha "val x = 5". Em vez disso, as únicas
  construções de topo permitidas são:

  - object
  - class
  - case class
  - trait

  Vamos agora explicar o que são:
*/

// Classes são semelhantes a classes noutras linguagens. Os argumentos do
// construtor são declarados após o nome da classe, sendo a inicialização feita
// no corpo da classe.
class Cão(rc: String) {
  // Código de construção
  var raça: String = rc

  // Define um método chamado "ladra", que retorna uma String
  def ladra = "Woof, woof!"

  // Valores e métodos são assumidos como públicos, mas é possivel usar
  // os códigos "protected" and "private".
  private def dormir(horas: Int) =
    println(s"Vou dormir por $horas horas")

  // Métodos abstractos são métodos sem corpo. Se descomentarmos a próxima
  // linha, a classe Cão é declarada como abstracta
  //   abstract class Cão(...) { ... }
  // def persegue(oQue: String): String
}

val oMeuCão = new Cão("greyhound")
println(oMeuCão.raça)  // => "greyhound"
println(oMeuCão.ladra) // => "Woof, woof!"


// O termo "object" cria um tipo e uma instancia singleton desse tipo. É comum
// que classes de Scala possuam um "objecto companheiro", onde o comportamento
// por instância é capturado nas classes, equanto que o comportamento
// relacionado com todas as instancias dessa classe ficam no objecto.
// A diferença é semelhante a métodos de classes e métodos estáticos noutras
// linguagens. Note que objectos e classes podem ter o mesmo nome.
object Cão {
  def raçasConhecidas = List("pitbull", "shepherd", "retriever")
  def criarCão(raça: String) = new Cão(raça)
}


// Case classes são classes com funcionalidades extra incluidas. Uma questão
// comum de iniciantes de scala é quando devem usar classes e quando devem usar
// case classes. A linha é difusa mas, em geral, classes tendem a concentrar-se
// em encapsulamento, polimorfismo e comportamento. Os valores nestas classes
// tendem a ser privados, sendo apenas exposotos métodos. O propósito principal
// das case classes é armazenarem dados imutáveis. Geralmente possuem poucos
// métods, sendo que estes raramente possuem efeitos secundários.
case class Pessoa(nome: String, telefone: String)

// Cria uma nova instancia. De notar que case classes não precisam de "new"
val jorge = Pessoa("Jorge", "1234")
val cátia = Pessoa("Cátia", "4567")

// Case classes trazem algumas vantagens de borla, como acessores:
jorge.telefone  // => "1234"

// Igualdade por campo (não é preciso fazer override do .equals)
Pessoa("Jorge", "1234") == Pessoa("Cátia", "1236")  // => false

// Cópia simples
// outroJorge == Person("jorge", "9876")
val outroJorge = jorge.copy(telefone = "9876")

// Entre outras. Case classes também suportam correspondência de padrões de
// borla, como pode ser visto de seguida.


// Traits em breve!


/////////////////////////////////////////////////
// 6. Correspondência de Padrões
/////////////////////////////////////////////////

// A correspondência de padrões é uma funcionalidade poderosa e bastante
// utilizada em Scala. Eis como fazer correspondência de padrões numa case class:
// Nota: Ao contrário de outras linguagens, cases em scala não necessitam de
// breaks, a computação termina no primeiro sucesso.

def reconhecePessoa(pessoa: Pessoa): String = pessoa match {
  // Agora, especifique os padrões:
  case Pessoa("Jorge", tel) => "Encontramos o Jorge! O seu número é " + tel
  case Pessoa("Cátia", tel) => "Encontramos a Cátia! O seu número é " + tel
  case Pessoa(nome, tel)    => "Econtramos alguém : " + nome + ", telefone : " + tel
}

val email = "(.*)@(.*)".r  // Define uma regex para o próximo exemplo.

// A correspondência de padrões pode parecer familiar aos switches em linguagens
// derivadas de C, mas é muto mais poderoso. Em Scala, é possível fazer
// correspondências com muito mais:
def correspondeTudo(obj: Any): String = obj match {
  // Pode-se corresponder valores:
  case "Olá mundo" => "Recebi uma string Olá mundo."

  // Corresponder por tipo:
  case x: Double => "Recebi um Double: " + x

  // Corresponder tendo em conta condições especificas:
  case x: Int if x > 10000 => "Recebi um número bem grande!"

  // Fazer correspondências com case classes (visto anteriormente):
  case Pessoa(nome, tel) => s"Recebi o contacto para $nome!"

  // Fazer correspondência com expressões regulares:
  case email(nome, dominio) => s"Recebi o endereço de email $nome@$dominio"

  // Corresponder tuplos:
  case (a: Int, b: Double, c: String) => s"Recebi o tuplo: $a, $b, $c"

  // Corresponder estruturas de dados:
  case List(1, b, c) => s"Recebi uma lista de 3 elementos começada em 1: 1, $b, $c"

  // Combinar padrões:
  case List(List((1, 2, "YAY"))) => "Recebi uma lista de lista de triplo"
}

// Na realidade, é possível fazer correspondência com qualquer objecto que
// defina o método "unapply". Esta funcionalidade é tão poderosa que permite
// definir funções sob a forma de padrões:
val funcPaddrao: Pessoa => String = {
  case Pessoa("Jorge", tel) => s"Número do Jorge: $tel"
  case Pessoa(nome, tel)    => s"Número de alguém: $tel"
}


/////////////////////////////////////////////////
// 7. Programação Funcional
/////////////////////////////////////////////////

// Scala permite que funções e métodos retornem, ou recebam como parámetros,
// outras funções ou métodos

val soma10: Int => Int = _ + 10 // Função que recebe um Int e retorna um Int
List(1, 2, 3) map soma10 // List(11, 12, 13) - soma10 é aplicado a cada elemento

// Funções anónimas também podem ser usadas
List(1, 2, 3) map (x => x + 10)

// Sendo que o símbolo _ também pode ser usado se a função anónima só receber
// um argumento. Este fica com o valor da variável
List(1, 2, 3) map (_ + 10)

// Se tanto o bloco como a função apenas receberem um argumento, o próprio
// _ pode ser omitido
List("Dom", "Bob", "Natalia") foreach println


// Combinadores

s.map(quadrado)

val sQuadrado = s.map(quadrado)

sQuadrado.filter(_ < 10)

sQuadrado.reduce (_+_)

// O método filter recebe um predicado (uma função de A => Boolean) e escolhe
// todos os elementos que satisfazem o predicado
List(1, 2, 3) filter (_ > 2) // List(3)
case class Pessoa(nome: String, idade: Int)
List(
  Pessoa(nome = "Dom", idade = 23),
  Pessoa(nome = "Bob", idade = 30)
).filter(_.idade > 25) // List(Pessoa("Bob", 30))


// O método foreach recebe uma função de A => Unit, executando essa função em
// cada elemento da colecção
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

// Compreensões For

for { n <- s } yield quadrado(n)

val nQuadrado2 = for { n <- s } yield quadrado(n)

for { n <- nQuadrado2 if n < 10 } yield n

for { n <- s; nQuadrado = n * n if nQuadrado < 10} yield nQuadrado

/* Nota: isto não são ciclos for: A semântica de um ciclo é 'repetir', enquanto
   que uma compreensão define a relação entre dois conjuntos de dados. */


/////////////////////////////////////////////////
// 8. Implicitos
/////////////////////////////////////////////////

/* AVISO IMPORTANTE: Implicitos são um conjunto de funcionalidades muito
 * poderosas em Scala, que podem ser fácilmente abusadas. Iniciantes devem
 * resistir a tentação de usá-los até que compreendam não só como funcionam,
 * mas também as melhores práticas. Apenas incluimos esta secção no tutorial
 * devido a estes serem tão comuns em bibliotecas de Scala que muitas delas
 * se tornam impossíveis de usar sem conhecer implicitos. Este capítulo serve
 * para compreender como trabalhar com implicitos, não como declará-los.
*/

// Qualquer valor (vals, funções, objectos, etc) pode ser declarado como
// implicito usando a palavra "implicit". Vamos usar a classe Cão da secção 5
// nestes exemplos

implicit val oMeuIntImplicito = 100
implicit def aMinhaFunçãoImplicita(raça: String) = new Cão("Golden " + raça)

// Por si só, a palavra implicit não altera o comportamento de um valor, sendo
// que estes podem ser usados da forma habitual.
oMeuIntImplicito + 2                   // => 102
aMinhaFunçãoImplicita("Pitbull").raça // => "Golden Pitbull"

// A diferença é que estes valores podem ser utilizados quando outro pedaço de
// código "necessite" de uma valor implicito. Um exemplo são argumentos
// implicitos de funções:
def enviaCumprimentos(aQuem: String)(implicit quantos: Int) =
  s"Olá $aQuem, $quantos cumprimentos para ti e para os teus!"

// Se dermos um valor a "quantos", a função comporta-se normalmente
enviaCumprimentos("João")(1000)  // => "Olá João, 1000 cumprimentos para ti e para os teus!"

// Mas, se omitirmos o parâmetro implicito, um valor implicito do mesmo tipo é
// usado, neste caso, "oMeuInteiroImplicito"
enviaCumprimentos("Joana")  // => "Olá Joana, 100 cumprimentos para ti e para os teus!"

// Parâmentros implicitos de funções permitem-nos simular classes de tipos de
// outras linguagens funcionais. Isto é tão comum que tem a sua própria notação.
// As seguintes linhas representam a mesma coisa
// def foo[T](implicit c: C[T]) = ...
// def foo[T : C] = ...


// Outra situação em que o compilador prouca um implicito é se encontrar uma
// expressão
//    obj.método(...)
// mas "obj" não possuir um método chamado "método". Neste cso, se houver uma
// conversão implicita A => B, onde A é o tipo de obj, e B possui um método
// chamado "método", a conversão é aplicada. Ou seja, tendo
// aMinhaFunçãoImplicita definida, podemos dizer
"Retriever".raça // => "Golden Retriever"
"Sheperd".ladra  // => "Woof, woof!"

// Neste caso, a String é primeiro convertida para Cão usando a nossa funão,
// sendo depois chamado o método apropriado. Esta é uma funcionalidade
// incrivelmente poderosa, sendo que deve ser usada com cautela. Na verdade,
// ao definir a função implicita, o compilador deve lançar um aviso a insisitir
// que só deve definir a função se souber o que está a fazer.


/////////////////////////////////////////////////
// 9. Misc
/////////////////////////////////////////////////

// Importar coisas
import scala.collection.immutable.List

// Importar todos os "sub pacotes"
import scala.collection.immutable._

// Importar multiplas classes numa linha
import scala.collection.immutable.{List, Map}

// Renomear uma classe importada usando '=>'
import scala.collection.immutable.{List => ImmutableList}

// Importar todas as classes excepto algumas. Set e Map são excluidos:
import scala.collection.immutable.{Map => _, Set => _, _}

// O ponto de entrada de um programa em Scala é definido por un ficheiro .scala
// com um método main:
object Aplicação {
  def main(args: Array[String]): Unit = {
    // código aqui.
  }
}

// Ficheiros podem conter várias classes o objectos. Compilar com scalac




// Input e output

// Ler um ficheiro linha a linha
import scala.io.Source
for(linha <- Source.fromFile("ficheiro.txt").getLines())
  println(linha)

// Escrever um ficheiro usando o PrintWriter de Java
val writer = new PrintWriter("ficheiro.txt")
writer.write("Escrevendo linha por linha" + util.Properties.lineSeparator)
writer.write("Outra linha aqui" + util.Properties.lineSeparator)
writer.close()

```

## Mais recursos

* [Scala for the impatient](http://horstmann.com/scala/)
* [Twitter Scala school](http://twitter.github.io/scala_school/)
* [The scala documentation](http://docs.scala-lang.org/)
* [Try Scala in your browser](http://scalatutorials.com/tour/)
* Join the [Scala user group](https://groups.google.com/forum/#!forum/scala-user)
