---
language: Scala
filename: learnscala-pt.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Ha-Duong Nguyen", "http://reference-error.org"]
translators:
    - ["Francieli Viane", "https://github.com/FFrancieli"]
lang: pt-br
---

Scala - a linguagem escalável

```scala

/////////////////////////////////////////////////
// 0. O básico
/////////////////////////////////////////////////
/*
  Configurando o Scala:

  1) Baixe o instalador do Scala - http://www.scala-lang.org/downloads
  2) Extraia (unzip ou tar) para sua localização favorita e coloque o subdiretório
  bin na variável de ambiente `PATH`
*/

/*
  Tente o REPL

  Scala tem uma ferramenta chamada REPL (Read-Eval-Print Loop) que é análogo a
  interpretadores de linha de comando de outras linguagens. Você pode digitar
  qualquer expressão de Scala e o resultado será calculado e impresso.

  O REPL é uma ferramenta muito conveniente para testar e verificar código. Use-o
  enquanto você lê o tutorial para explorar os conceitos rapidamente por conta própria.
*/

//Inicialize um REPL de Scala executando o comando scala no terminal. Você deve ver o prompt:
$ scala
scala>

//Por padrão, cada expressão que você executa é salva como um novo valor enumerado:
scala> 2 + 2
res0: Int = 4

// Valores padrões podem ser reutilizados. Observe o tipo do valor exibido no resultado...
scala> res0 + 2
res1: Int = 6

// Scala é uma linguagem fortemente tipada. Você pode usar o REPL para verfificar o tipo
// sem avaliar uma expressão.

scala> :type (true, 2.0)
(Boolean, Double)

// As sessões do REPL podem ser salvas
scala> :save /sites/repl-test.scala

//Arquivos podem ser carregados no REPL
scala> :load /sites/repl-test.scala
Loading /sites/repl-test.scala...
res2: Int = 4
res3: Int = 6

// Você pode pesquisar em seu histórico recente
scala> :h?
1 2 + 2
2 res0 + 2
3 :save /sites/repl-test.scala
4 :load /sites/repl-test.scala
5 :h?

// Agora que você já sabe brincar, vamos aprender um pouco de Scala...

/////////////////////////////////////////////////
// 1. Introdução
/////////////////////////////////////////////////

// Comentários de uma linha começam com duas barras

/*
  Comentários com múltiplas linhas, como você já pode ver, são assim.
*/

// Imprimir e forçar uma linha na próxima impressão
println("Hello world!")
println(10)
// Hello world!
// 10

//Imprimir sem forçar uma nova linha na próxima impressão
print("Hello world")
print(10)
// Hello world10

//A declaração de valores pode ser feita usando tanto o var quanto o val.
// Declarações feitas com `val` são imutáveis, enquanto que declarações feitas
// com var são mutáveis. Imutabilidade é uma coisa boa.
val x = 10 // x is now 10
x = 20     // error: reassignment to val
var y = 10
 = 20     // y agora é 20

/*
  Scala é uma linguagem estaticamente tipada. Observe ainda que nas declarações
  acima nós não especificamos um tipo. Isso se deve a um recurso da linguagem
  chamado de inferência. Na maioria dos casos, o compilador do Scala consegue
  adivinhar qual tipo é, de forma que você não precisa digitar sempre. Nós
  podemos declarar o tipo da variável de maneira explícita asim:
*/

val z: Int = 10
val a: Double = 1.0

// Note que a conversão automática de Int para Double, o resultado é 10.0, não 10
val b: Double = 10

//Valores booleanos
true
false

//Operações booleanas
!true         // false
!false        // true
true == false // false
10 > 5        // true

// Matemática é como o de costume
1 + 1   // 2
2 - 1   // 1
5 * 3   // 15
6 / 2   // 3
6 / 4   // 1
6.0 / 4 // 1.5
6 / 4.0 // 1.5

// Calcular uma expressão no REPL te dá o tipo e o valor do resultado
1 + 7

/* A linha acima resulta em:
  scala> 1 + 7
  res29: Int = 8

  Isso significa que o resultado ao culcular  1 + 7 é um objeto do tipo Int com
  valor 8.

  Note que "res29" é o nome de uma variável gerada sequencialmente para guardar
  os resultados das expressões que você executa, logo seu nome pode ser
  diferente.
*/

"Strings em Scala são delimitadas por aspas duplas"
'a' // Um caractere em Scala
// 'Strings com aspas simples não existem em Scala.' <= isso causa um erro.

// Strings possuem os métodos comuns de Java definidos
"hello world".length
"hello world".substring(2, 6)
"hello world".replace("C", "3")

// Elas também possuem alguns métodos extras do Scala. Veja também:
scala.collection.immutable.StringOps
"hello world".take(5)
"hello world".drop(5)

//Interpolação de string: observe o prefixo "s"
val n = 45
s"We have $n apples" // => "We have 45 apples"

// Também é possível ter expressões dentro de interpolação de strings
val a = Array(11, 9, 6)
s"My second daughter is ${a(0) - a(2)} years old."    // => "My second daughter is 5 years old."
s"We have double the amount of ${n / 2.0} in apples." // => "We have double the amount of 22.5 in apples."
s"Power of 2: ${math.pow(2, 2)}"                      // => "Power of 2: 4"

// Formatação de strings interpoladas com o prefixo "f"
f"Power of 5: ${math.pow(5, 2)}%1.0f"         // "Power of 5: 25"
f"Square root of 122: ${math.sqrt(122)}%1.4f" // "Square root of 122: 11.0454"


// Strings cruas, ignorando caracteres especiais
raw"New line feed: \n. Carriage return: \r." // => "New line feed: \n. Carriage return: \r."

//Alguns caracteres precisam ser "escapados", ex. uma aspa dupla dentro de uma string

"They stood outside the \"Rose and Crown\"" // => "They stood outside the "Rose and Crown""

// Aspas triplas permitem strings a abrangerem múltiplas linhas e conter Aspas
val html = """<form id="daform">
                <p>Press belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. Funções
/////////////////////////////////////////////////

// Funções são definidas da seguinte maneira:
//
//   def nomeDaFuncao(args ...): TipoDeRetorno = {body ...}
//
// Se você vem de linguagens mais tradicionais, note a omissão da palavra chave
//return. Em Scala a última expressão no bloco da função é o valor de retorno
def sumOfSquares(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// As { } podem ser omitidas se o corpo da função possui apenas uma expressão:
def sumOfSquaresShort(x: Int, y: Int): Int = x * x + y * y

// A sintaxe para chamar funções é familiar:
sumOfSquares(3, 4)  // => 25

// Você poode usar o nome dos parâmetros para especificá-los numa ordem diferente
def subtract(x: Int, y: Int): Int = x - y

subtract(10, 3)     // => 7
subtract(y=10, x=3) // => -7

// Na maioria dos casos (sendo funções recursivas a a exceção mais notável) o
// tipo de retorno da função pode ser omitido, e o mesmo tipo de inferência que
// vimos nas variáveis funcionará com o valor de retorno da função:
def sq(x: Int) = x * x  // O compilador consegue adivinhar que o tipo de retorno é Int

// Funções podem ter parâmetros padrão:
def addWithDefault(x: Int, y: Int = 5) = x + y
addWithDefault(1, 2) // => 3
addWithDefault(1)    // => 6

// Funções anônimas são semelhantes a essa:
(x: Int) => x * x

// Diferente de defs, até mesmo a entrada de funções anônimas podem ser omitidas
// se o contexto deixar isso claro. Observe o tipo "Int => Int", que significa
// uma função que recebe umn Int e retorna um Int.
val sq: Int => Int = x => x * x

// Se cada argumento na sua função anônima é usado apenas uma vez, Scala te fornece
// uma maneira ainda mais curta de definí-lo. Estas funções anônimas acabam por
// ser muito comuns, como será mostrado na sessão de estrutura de dados.
val addOne: Int => Int = _ + 1
val weirdSum: (Int, Int) => Int = (_ * 2 + _ * 3)

addOne(5)      // => 6
weirdSum(2, 4) // => 16

// A palavra chave return existe em Scala, mas só retorna do def mais profundo que o cerca.
//AVISO: O uso do return em Scala é propenso a erros e deve ser evitado.
//Não há efeito em funções anônimas. Per exemplo:
def foo(x: Int): Int = {
  val anonFunc: Int => Int = { z =>
    if (z > 5)
      return z // Esta linha faz Z retornar o valor de foo!
    else
      z + 2    // Esta linha retorna o valor de anonFunc
  }
  anonFunc(x) // Esta linha retorna o valor de foo
}

/////////////////////////////////////////////////
// 3. Controle de Fluxo
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach(println)

r foreach println
///N.B.: Scala é bem flexível quando se fala de pontos e parêntesis - estude as regras
//separadamente. Isso ajuda a escrever DSLs e APIs que são lidas como inglês.

(5 to 1 by -1) foreach (println)

// Um loop while
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }   // Sim, de novo. O que aconteceu? Por quê?

i    // Exibe o valor de i. Note que o while é um loop no senso clássico -
     // executa sequencialmente enquanto muda a variável do loop. While é muito
     // rápido, mas usar os combinadores e compreenões acima é mais fácil
     // para entender e paralizar

// Um loop do-while
i = 0
do {
  println("i ainda é menor que 10")
  i += 1
} while (i < 10)


// Recursão é a forma mais idiomática de repetir uma ação em Scala (assim como na
// maioria das linguagens de programação funcional)
// Funções recursivas precisam de um tipo de retorno explícito, o compilador não
// consegue inferir;
// Aqui está o Unit
def showNumbersInRange(a: Int, b: Int): Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}
showNumbersInRange(1, 14)

// Condicionais

al x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"

/////////////////////////////////////////////////
// 4. Estrutura de Dados
/////////////////////////////////////////////////

val a = Array(1, 2, 3, 5, 8, 13)
a(0)     // Int = 1
a(3)     // Int = 5
a(21)    // Lança uma exceção

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")   // java.lang.String = no lo se

val s = Set(1, 3, 7)
s(0)      // Boolean = false
s(1)      // Boolean = true

/* Veja a documantação do map aqui -
 * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map
 * e garanta que você leia
 */

 // Tuplas

 (1, 2)

 (4, 3, 2)

 (1, 2, "three")

 (a, 2, "three")

 //Por que ter isso?
 val divideInts = (x: Int, y: Int) => (x / y, x % y)

//A função divideInts te dá o resultado e o resultado
divideInts(10, 3)    // (Int, Int) = (3,1)

//Para acessar os elementos de uma tupla, use _._n onde n é o índex do elemento

val d = divideInts(10, 3)    // (Int, Int) = (3,1)

d._1    // Int = 3
d._2    // Int = 1

// Alternativamente, você pode atribuir múltiplas variáveis para uma tupla, o
// que é mais conveniente e legível em muitos casos
val (div, mod) = divideInts(10, 3)

div     // Int = 3
mod     // Int = 1

/////////////////////////////////////////////////
// 5. Object Oriented Programming
/////////////////////////////////////////////////

/*
    Tudo o que vimos até agora neste tutorial foram expressões simples (valores, funções, etc).
    Essas expressões são boas para digitar no interpretador da linha de comando para
    testes rápidos, mas elas não podem existir por si só em um arquivo Scala. Por exemplo,
    você não pode ter simplesmente "val x = 5" em um arquivo Scala. Ao invés disso, os únicos
    construtores de alto nível permitidos em Scala são:

    - objects
    - classes
    - case classes
    - traits

    E agora vamos explicar o que é cada um deles.
*/

//classes são similares a classes em outras linguagens. Os argumentos do construtor
// são declarados logo depois do nome da classe e a inicialização é feita no corpo da classe.

class Dog(br: String) {
  // codigo do construtor aqui
  var breed: String = br

  // Define um método chamado bark que retorna uma String
  def bark = "Woof, woof!"

  // Assume-se que os métodos e valores são públicos. As palavras chave "protected"
  // e "private" também estão disponíveis.
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  // Métodos abstratos são simplesmente métodos sem corpo. Se a gente remover o
  // comentário da próxima linha a classe Dog teria que ser declarada como abstrata

  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

// A palavra chave "object" cria um tipo e uma instância singlenton desse tipo.
// É comum para classes em Scala ter um "companion object" (objeto companheiro),
// onde, por exemlo, o comportamento é capturado pelas classes em si, mas o comportamento
// relacionado a toda instância da classe vai em objects. A diferença é semelhante
// a métodos versus métodos estáticos em outras linguagens. Note que objects e
// classes podem ter o mesmo nome.

object Dog {
  def allKnownBreeds = List("pitbull", "shepherd", "retriever")
  def createDog(breed: String) = new Dog(breed)
}

// Case classes são classes que possuem uma funcionalidade extra incorporada.
// Uma dúvida comum para iniciantes em Scala é quando usar classes e quando usar
// case classes. A linha é bem tênue, mas em geral classes tendem a focar em encapsulamento,
// polimorfismo e comportamento. Os valores nestas classes tendem a ser privados e
// apenas métodos ficam expostos. O propósito primário de uma case class é guardar
// dados imutáveis. Às vezes as case classes possuem alguns poucos métodos, os quais
// raramente possuem efeitos colaterais (side effects).
case class Person(name: String, phoneNumber: String)

// Cria uma nova instância. Observe que case classes não precisam de usar "new" ao serem instanciadas
val george = Person("George", "1234")
val kate = Person("Kate", "4567")

// Com case classes você ganha algumas regalias, como getters:
// With case classes, you get a few perks for free, like getters:
george.phoneNumber  // => "1234"

// Verificação de igualdade por campo (sem a necessidade de sobrescrever o método equals)
Person("George", "1234") == Person("Kate", "1236")  // => false

// Uma maneira fácil de copiar
// otherGeorge == Person("george", "9876")
val otherGeorge = george.copy(phoneNumber = "9876")

// E muitas outras. Case classes também possuem pattern matching de graça. Veja no próximo tópico.

// Traits a caminho.


/////////////////////////////////////////////////
// 6. Pattern Matching
/////////////////////////////////////////////////

// Pattern matching é um recurso muito poderoso e muito usado em Scala. Aqui
// mostramos como o seu pattern se adequa a uma case class.
// NB: Diferente de outras linguagens, Scala não precisa de quebras. Entrar em
// todas as condições do pattern matching simples não acontece.

def matchPerson(person: Person): String = person match {
  // Enrão você especifica os padrões
  case Person("George", number) => "We found George! His number is " + number
  case Person("Kate", number)   => "We found Kate! Her number is " + number
  case Person(name, number)     => "We matched someone : " + name + ", phone : " + number
}

val email = "(.*)@(.*)".r  // Define uma regex para o próximo exemplo.

// Pattern matching pode parecer com o comando switch nas liguagens da família C,
// mas é muito mais poderoso. Em Scala você pode encontrar mais correpondências:

def matchEverything(obj: Any): String = obj match {
  // Você pode encontrar valores correspondentes:
  case "Hello world" => "Got the string Hello world"

  // Você pode fazer correspondência por tipo:
  case x: Double => "Got a Double: " + x

  // Você pode especificar condições:
  case x: Int if x > 10000 => "Got a pretty big number!"

  // Você pode encontrar correspondência com case classes, como fizemos antes:
  case Person(name, number) => s"Got contact info for $name!"

  // Você pode encontrar correspondências por regex:
  case email(name, domain) => s"Got email address $name@$domain"

  // Você pode encontrar correspondencias por tuplas:
  case (a: Int, b: Double, c: String) => s"Got a tuple: $a, $b, $c"

  // Você pode encontrar corresponências por estruturas de dados:
  case List(1, b, c) => s"Got a list with three elements and starts with 1: 1, $b, $c"

  // Você pode aninhar padrões:
  case List(List((1, 2, "YAY"))) => "Got a list of list of tuple"

  // Retornar qualquer valor (padrão - default) caso nenhuma das possibilidades é correspondente.
  case _ => "Got unknown object"

  // Na verdade, você pode fazer correspondência de padrão de qualquer objeto que
  // tenha o método "unnaply". Este recurso é tão poderoso que o Scala te deixa
  // criar funções inteiras como patterns:

  val patternFunc: Person => String = {
    case Person("George", number) => s"George's number: $number"
    case Person(name, number) => s"Random person's number: $number"
  }
}

/////////////////////////////////////////////////
// 7. Programação Funcional
/////////////////////////////////////////////////

// Scala permite que métodos e funções recebam ou retornem outras funções ou métodos.

val add10: Int => Int = _ + 10 // A function taking an Int and returning an Int
List(1, 2, 3) map add10 // List(11, 12, 13) - add10 is applied to each element

// Funções anônimas podem ser usadas ao invés de funções com nomes:
List(1, 2, 3) map (x => x + 10)

// E o símbolo underline ("_") pode ser usado quando há apenas um argumento para a função anônima.
List(1, 2, 3) map (_ + 10)

// Se tanto o bloco animo quanto a função que você estiver usando receberem apenas
// um argumento, você pode inclusive omitir o símbolo _
List(1, 2, 3) map (_ + 10)

// Combinadores

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// A função filter recebe um predicado (uma função do tipo A -> Boolean) e seleciona
// todos os elementos que satisfazem o predicado.
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name: String, age: Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))

// Scala tem o método foreach definido em algumas collections em específico, o qual
// recebe um tipo e retorna Unit (um método void)
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println

/* NB Ests não são laços for. A semântica dos laços for é 'repetir' enquanto um
  for-comprehension define um relacionamento entre dois conjuntos de dados */


/////////////////////////////////////////////////
// 8. Implicits
/////////////////////////////////////////////////

/* ALERTA ALERTA:
  Implicits são um conjunto de recursos poderosos de Scala e consequentemente é
  fácil abusar deles. Iniciantes em Scala deveriam resistir a tentação de usá-los
  até que eles entendam não apenas como eles funcionam mas também as melhores práticas
  deles. Incluimos uma sessão neste tutorial sobre isso porque implicits são tão
  corriqueiros em bibliotecas do Scala que é impossível fazer qualqeuer coisa expressiva
  sem utilizar uma biblioteca que usa implicits. Isto é para você entender e trabalhar
  com implicits. Não declare seus próprios implicits por conta própria.
*/

// qualquer valor (val, function, objects, etc) pode ser declarado para ser implícito
// usando a, você adivinhou, palavra chave "implicit". Usaremos a classe Dog definida
// na sessão 5 para os próximos exemplos.
implicit val myImplicitInt = 100
implicit def myImplicitFunction(breed: String) = new Dog("Golden " + breed)

// A palavra chave implicit não muda o comportamento do valor por si só, então
// os valores acima podem ser usados como de costume.
myImplicitInt + 2                   // => 102
myImplicitFunction("Pitbull").breed // => "Golden Pitbull"

A diferença é que agora esses valores são elegíveis para serem usados quando outra
// parte do código "precisa" de um valor implícito. Uma situação é uma função
// com argumentos implícitos:
def sendGreetings(toWhom: String)(implicit howMany: Int) =
  s"Hello $toWhom, $howMany blessings to you and yours!"

// Se fornecermos um valor para "howMany" a função se comporta como sempre
sendGreetings("John")(1000)  // => "Hello John, 1000 blessings to you and yours!"

// Mas se omitirmos o parâmetro implícito um valor implícito de mesmo tipo é usado,
// neste caso, "myImplicitInt":
sendGreetings("Jane")  // => "Hello Jane, 100 blessings to you and yours!"

// Parâmetros implícitos de funções nos permitem simular type classes em outras
//linguagens funcionais. As linhas abaixo são a mesma coisa:
// def foo[T](implicit c: C[T]) = ...
// def foo[T : C] = ...

// Outro caso no qual o compilador procura por um implicit é quando você tem obj.method(...)
// mas "obj" não possui "method" como um método. Neste caso, se houver uma conversão
// de implicit do tipo A => B, onde A é o tipo do "obj" e B tem um método chamado
// "method", a conversão é aplicada. Então, tendo myImplicitFunction acima em escopo, podemos dizer:
"Retriever".breed // => "Golden Retriever"
"Sheperd".bark    // => "Woof, woof!"

// Aqui, a String é convertida para Dog usando nossa função acima, então o método
// apropriado é chamado. Isso é um recurso extremamente poderoso, mas de novo, não
// é para ser usado de maneira leviana. Na verdade, quando você define a função
// implícita, o seu compilador deve exibir um aviso de que você não deveria fazer isso,
// a menos que você realmente saiba o que você está fazendo.

/////////////////////////////////////////////////
// 9. Misc
/////////////////////////////////////////////////

// Importando coisas
import scala.collection.immutable.List

// Importando todos os sub pacotes
import scala.collection.immutable._

// Importando várias classes em um único comando
import scala.collection.immutable.{List, Map}

// Renomeando um import usando '=>'
import scala.collection.immutable.{List => ImmutableList}

// Importa todas as classes, com exceção de algumas. O import abaixo importa todas as classes excluindo Map e Set:
import scala.collection.immutable.{Map => _, Set => _, _}

// Classes Java também podem ser importadas. A syntaxe de Scala pode ser usada:
import java.swing.{JFrame, JWindow}

// O ponto de entrada do seu programa é definido em um arquivo Scala usando um object com um único método main:
object Application {
  def main(args: Array[String]): Unit = {
    // o código fica aqui
  }
}

// Arquivos podem ter múltiplas classes e objects. Compile com scalac

// Entrada e saída

// Para ler um arquivo linha a linha
import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)

// Para escrever um arquivo use o PrintWriter do Javaval writer = new PrintWriter("myfile.txt")
writer.write("Writing line for line" + util.Properties.lineSeparator)
writer.write("Another line here" + util.Properties.lineSeparator)
writer.close()

##  Recursos adicionais

* [Scala for the impatient](http://horstmann.com/scala/)
* [Twitter Scala school](http://twitter.github.io/scala_school/)
* [Documentação de Scala](http://docs.scala-lang.org/)
* [Tente Scala no seu navegador](http://scalatutorials.com/tour/)
* Junte [Scala user group](https://groups.google.com/forum/#!forum/scala-user)
```
