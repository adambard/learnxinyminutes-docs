---
language: Groovy
category: language
filename: learngroovy.groovy
contributors:
    - ["Roberto Pérez Alcolea", "http://github.com/rpalcolea"]
translators:
    - ["João Farias", "https://github.com/JoaoGFarias"]
lang: pt-br
---

Groovy - Uma linguagem dinâmica para a plataforma Java. [Leia mais aqui.](http://www.groovy-lang.org/)

```groovy

/*
  Prepara-se:

  1) Instale a máquina virtual de Groovy - http://gvmtool.net/
  2) Intalse o Groovy: gvm install groovy
  3) Inicie o console groovy digitando: groovyConsole

*/

// Comentário de uma linha inicia-se com duas barras
/*
Comentário de múltiplas linhas são assim.
*/

// Olá Mundo!
println "Olá mundo!"

/*
  Variáveis:

  Você pode atribuir valores a variáveis para uso posterior
*/

def x = 1
println x

x = new java.util.Date()
println x

x = -3.1499392
println x

x = false
println x

x = "Groovy!"
println x

/*
  Coleções e mapeamentos
*/

//Criando uma lista vazia
def tecnologias = []

/*** Adicionando elementos à lista ***/

// Assim como Java
tecnologias.add("Grails")

// Shift para esquerda adiciona e retorna a lista
tecnologias << "Groovy"

// Adição de múltiplos elementos
tecnologias.addAll(["Gradle","Griffon"])

/*** Removendo elementos da lista ***/

// Assim como Java
tecnologias.remove("Griffon")

// Subtração também funciona
tecnologias = technologies - 'Grails'

/*** Iterando sobre listas ***/

// Itera sobre os elementos da lista
tecnologias.each { println "Tecnologias: $it"}
tecnologias.eachWithIndex { it, i -> println "$i: $it"}

/*** Checando os elementos da lista ***/

//Avalia se a lista contém o elemento 'Groovy'
contem = tecnologias.contains( 'Groovy' )

// Ou
contem = 'Groovy' in tecnologias

// Checagem por múltiplos elementos
tecnologias.containsAll(['Groovy','Grails'])

/*** Ordenando listas ***/

// Ordena a lista (altera a lista in-place)
tecnologias.sort()

// Para ordenar a lista sem alterar a original
tecnologiasOrdenadas = tecnologias.sort( false )

/*** Manipulando listas ***/

//Substitue todos os elementos da lista
Collections.replaceAll(tecnologias, 'Gradle', 'gradle')

//Desorganiza a lista
Collections.shuffle(tecnologias, new Random())

//Limpa a lista
technologies.clear()

//Criando um mapeamento vazio
def devMap = [:]

//Adicionando valores
devMap = ['nome':'Roberto', 'framework':'Grails', 'linguagem':'Groovy']
devMap.put('ultimoNome','Perez')

//Iterando sobre os elementos do mapeamento
devMap.each { println "$it.key: $it.value" }
devMap.eachWithIndex { it, i -> println "$i: $it"}

//Avalia se um mapeamento contém uma chave
assert devMap.containsKey('nome')

//Avalia se um mapeamento contém um valor
assert devMap.containsValue('Roberto')

//Pega as chaves de um mapeamento
println devMap.keySet()

//Pega os valores de um mapeamento
println devMap.values()

/*
  Groovy Beans

  GroovyBeans são JavaBeans com uma sintaxe muito mais simples.

  Quando Groovy é compilado para bytecode, as seguintes regras são usadas:

    * Se o nome é declarado com um modificador de acesso(public, private or
      protected) então um atributo é gerado.

    * Um nome declarado sem modificador de acesso gera um campo privado com
      getter e setter públicos (ou seja, uma propriedade).

    * Se uma propriedade é declarada como final, um campo private final é criado
      e o setter não é gerado.

    * Você pode declarar uma propriedade e também declarar seus próprios getters
      e setters.

    * Você pode declarar uma propriedade e um campo com o mesmo nome, a propriedade
      usará este campo.

    * Se você quer uma propriedade private ou protected, você deve prover seus
      próprios getters e setter, que devem ser declarados como private ou protected.

    * Se você acessar uma propriedade dentro da classe e esta propriedade é definida
      em tempo de compilação com 'this', implícito ou explícito (por exemplo,
      this.foo, ou simplesmente foo), Groovy acessará este campo diretamente, sem
      passar pelo getter ou setter.

    * Se você acessar uma propriedade que não existe usando foo, explicitamente ou
      implicitamente, então Groovy irá acessar esta propriedade através da meta
      classe, o que pode falhar em tempo de execução.

*/

class Foo {
    // propriedade de leitura, apenas
    final String nome = "Roberto"

    // propriedade de leitura, apenas, com getter e setter públicos
    String linguagem
    protected void setLinguagem(String linguagem) { this.linguagem = linguagem }

    // propriedade tipada dinamicamente
    def ultimoNome
}

/*
  Condicionais e loops
*/

//Groovy suporta a sintaxe if-else
def x = 3

if(x==1) {
    println "Um"
} else if(x==2) {
    println "Dois"
} else {
    println "X é maior que Dois"
}

//Groovy também suporta o operador ternário
def y = 10
def x = (y > 1) ? "functionou" : "falhou"
assert x == "functionou"

//Loop 'for'
//Itera sobre um intervalo (range)
def x = 0
for (i in 0 .. 30) {
    x += i
}

//Itera sobre uma lista
x = 0
for( i in [5,3,2,1] ) {
    x += i
}

//Itera sobre um array
array = (0..20).toArray()
x = 0
for (i in array) {
    x += i
}

//Itera sobre um mapa
def map = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
x = 0
for ( e in map ) {
    x += e.value
}

/*
  Operadores

  Sobrecarregamento de Operadores para uma lsita dos operadores comuns que
  Grooby suporta:
  http://www.groovy-lang.org/operators.html#Operator-Overloading

  Operadores Groovy úteis
*/
//Operador de espalhamento: invoca uma ação sobre todos os itens de um
//objeto agregador.
def tecnologias = ['Groovy','Grails','Gradle']
tecnologias*.toUpperCase() // = to tecnologias.collect { it?.toUpperCase() }

//Operador de navegação segura: usado para evitar NullPointerException.
def usuario = User.get(1)
def nomeUsuario = usuario?.nomeUsuario


/*
  Closures
  Um closure, em Grooby, é como um "bloco de código" ou um ponteiro para método.
  É um pedação de código que é definido e executado em um momento posterior.

  Mais informação em: http://www.groovy-lang.org/closures.html
*/
//Exemplo:
def clos = { println "Hello World!" }

println "Executando o closure:"
clos()

//Passando parêmetros para um closure
def soma = { a, b -> println a+b }
soma(2,4)

//Closdures por referir-se a variáveis que não estão listadas em sua
//lista de parêmetros.
def x = 5
def multiplicarPor = { num -> num * x }
println multiplicarPor(10)

// Se você tiver um closure que tem apenas um argumento, você pode omitir
// o parâmetro na definição do closure
def clos = { print it }
clos( "oi" )

/*
  Groovy pode memorizar resultados de closures [1][2][3]
*/
def cl = {a, b ->
    sleep(3000) //  simula processamento
    a + b
}

mem = cl.memoize()

def chamaClosure(a, b) {
    def inicio = System.currentTimeMillis()
    mem(a, b)
    println "Os inputs(a = $a, b = $b) - tomam ${System.currentTimeMillis() - inicio} msecs."
}

chamaClosure(1, 2)
chamaClosure(1, 2)
chamaClosure(2, 3)
chamaClosure(2, 3)
chamaClosure(3, 4)
chamaClosure(3, 4)
chamaClosure(1, 2)
chamaClosure(2, 3)
chamaClosure(3, 4)

/*
  Expando

  A classe Expando é um bean dinâmico que permite adicionar propriedade e 
  closures como métodos a uma instância desta classe

  http://mrhaki.blogspot.mx/2009/10/groovy-goodness-expando-as-dynamic-bean.html
*/
  def usuario = new Expando(nome:"Roberto")
  assert 'Roberto' == nome.name

  nome.lastName = 'Pérez'
  assert 'Pérez' == nome.lastName

  nome.showInfo = { out ->
      out << "Name: $name"
      out << ", Last name: $lastName"
  }

  def sw = new StringWriter()
  println nome.showInfo(sw)


/*
  Metaprogramação (MOP)
*/

//Usando a ExpandoMetaClasse para adicionar comportamento
String.metaClass.testAdd = {
    println "adicionamos isto"
}

String x = "teste"
x?.testAdd()

//Interceptando chamadas a métodos
class Test implements GroovyInterceptable {
    def soma(Integer x, Integer y) { x + y }

    def invocaMetodo(String name, args) {
        System.out.println "Invoca método $name com argumentos: $args"
    }
}

def teste = new Test()
teste?.soma(2,3)
teste?.multiplica(2,3)

//Groovy suporta propertyMissing para lidar com tentativas de resolução de
//propriedades.
class Foo {
   def propertyMissing(String nome) { nome }
}
def f = new Foo()

assertEquals "boo", f.boo

/*
  TypeChecked e CompileStatic
  Groovy, por natureza, é e sempre será uma linguagem dinâmica, mas ela também
  suporta typecheked e compilestatic

  Mais informações: http://www.infoq.com/articles/new-groovy-20
*/
//TypeChecked
import groovy.transform.TypeChecked

void testeMethod() {}

@TypeChecked
void test() {
    testeMethod()

    def nome = "Roberto"

    println noomee

}

//Outro exemplo:
import groovy.transform.TypeChecked

@TypeChecked
Integer test() {
    Integer num = "1"

    Integer[] numeros = [1,2,3,4]

    Date dia = numeros[1]

    return "Teste"

}

//Exemplo de CompileStatic :
import groovy.transform.CompileStatic

@CompileStatic
int soma(int x, int y) {
    x + y
}

assert soma(2,5) == 7


```

## Referências

[Groovy documentation](http://www.groovy-lang.org/documentation.html)

[Groovy web console](http://groovyconsole.appspot.com/)

Junte-se a um [grupo de usuários Groovy](http://www.groovy-lang.org/usergroups.html)

## Livro

* [Groovy Goodness] (https://leanpub.com/groovy-goodness-notebook)

* [Groovy in Action] (http://manning.com/koenig2/)

* [Programming Groovy 2: Dynamic Productivity for the Java Developer] (http://shop.oreilly.com/product/9781937785307.do)

[1] http://roshandawrani.wordpress.com/2010/10/18/groovy-new-feature-closures-can-now-memorize-their-results/
[2] http://www.solutionsiq.com/resources/agileiq-blog/bid/72880/Programming-with-Groovy-Trampoline-and-Memoize
[3] http://mrhaki.blogspot.mx/2011/05/groovy-goodness-cache-closure-results.html



