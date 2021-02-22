---
language: Groovy
contributors:
    - ["Roberto Pérez Alcolea", "http://github.com/rpalcolea"]
translators:
    - ["Jhoon Saravia", "https://github.com/jhoon"]
lang: es-es
filename: groovy-es.html
---

Groovy - Un lenguaje dinámico para la plataforma Java [Leer más aquí.](http://www.groovy-lang.org/)

```groovy

/*
  Hora de configurar:

  1) Instala GVM - http://gvmtool.net/
  2) Instala Groovy: gvm install groovy
  3) Inicia la consola de groovy escribiendo: groovyConsole

*/

//  Los comentarios de una sola línea inician con dos barras inclinadas
/*
Los comentarios multilínea se ven así.
*/

// Hola Mundo
println "Hola mundo!"

/*
  Variables:

  Puedes asignar valores a variables para usarlas después
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
  Mapas y Colecciones
*/

// Creando una lista vacía
def technologies = []

/*** Agregando elementos a la lista ***/

// Como si fuera Java
technologies.add("Grails")

// Doble símbolo de menor agrega un elemento y, además, retorna la lista
technologies << "Groovy"

// Agregando múltiples elementos
technologies.addAll(["Gradle","Griffon"])

/*** Quitando elementos de la lista ***/

// Como si fuera Java
technologies.remove("Griffon")

// La resta también funciona
technologies = technologies - 'Grails'

/*** Iterando Listas ***/

// Para iterar sobre los elementos de una Lista
technologies.each { println "Technology: $it"}
technologies.eachWithIndex { it, i -> println "$i: $it"}

/*** Revisando los contenidos de una Lista ***/

// Evaluar si la lista contiene elemento(s) (boolean)
contained = technologies.contains( 'Groovy' )

// O
contained = 'Groovy' in technologies

// Evaluar por múltiples contenidos
technologies.containsAll(['Groovy','Grails'])

/*** Ordenando Listas ***/

// Para ordenar una Lista (modifica la lista original)
technologies.sort()

// Para ordenarla sin modificar la original, se puede hacer:
sortedTechnologies = technologies.sort( false )

/*** Manipulando Listas ***/

// Reemplazar todos los elementos en la lista
Collections.replaceAll(technologies, 'Gradle', 'gradle')

// Mezclar una lista
Collections.shuffle(technologies, new Random())

// Limpiar una lista
technologies.clear()

// Creando un mapa vacío
def devMap = [:]

// Agregando valores
devMap = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
devMap.put('lastName','Perez')

// Iterar sobre los elementos del mapa
devMap.each { println "$it.key: $it.value" }
devMap.eachWithIndex { it, i -> println "$i: $it"}

// Evaluar si el mapa contiene una llave
assert devMap.containsKey('name')

// Evaluar si el mapa contiene un valor
assert devMap.containsValue('Roberto')

// Para obtener las llaves del mapa
println devMap.keySet()

// Para obtener los valores del mapa
println devMap.values()

/*
  Groovy Beans

  GroovyBeans son JavaBeans pero usando una sintaxis mucho más simple

  Cuando Groovy es compilado a código de bytes, las siguientes reglas son usadas:

    * Si el nombre es declarado con un modificador de acceso (public, private o
      protected), entonces se genera un campo.

    * Un nombre declarado sin modificador de acceso genera un campo privado con
      un getter y un setter públicos (ej: una propiedad)

    * Si una propiedad es declarada como final, entonces el campo privado es creado
      como final y no se genera un setter.

    * Puedes declarar una propiedad y también sus propios getter y setter.

    * Puedes declarar una propiedad y un campo del mismo nombre, en ese caso, la
      propiedad usará ese campo.

    * Si quieres una propiedad private o proteceted, tienes que proveer tus propios
      getter y setter, los cuales deben ser declarados private o protected.

    * Si accedes a una propiedad desde dentro de la clase, la propiedad es definida
      en tiempo de compilación con this implícito o explícito (por ejemplo, this.foo
      o simplemente foo), Groovy accederá al campo directamente en vez de usar el 
      getter y setter.

    * Si accedes a una propiedad que no existe usando foo explícito o implícito, entonces
      Groovy accederá a la propiedad a través de la clase meta, que puede fallar en
      tiempo de ejecución.

*/

class Foo {
    // propiedad de solo lectura
    final String name = "Roberto"

    // propiedad de solo lectura, con getter público y setter como protected
    String language
    protected void setLanguage(String language) { this.language = language }

    // propiedad de tipo dinámico
    def lastName
}

/*
  Derivación Lógica e Iteraciones
*/

// Groovy soporta la clásica sintaxis de if - else
def x = 3

if(x==1) {
    println "One"
} else if(x==2) {
    println "Two"
} else {
    println "X greater than Two"
}

// Groovy también soporta el uso del operador ternario:
def y = 10
def x = (y > 1) ? "worked" : "failed"
assert x == "worked"

// ¡Groovy también soporta 'El Operador Elvis'!
// En lugar de usar el operador ternario:

displayName = user.name ? user.name : 'Anonymous'

// Podemos escribirlo así:
displayName = user.name ?: 'Anonymous'

// Iteración con For
// Iterando en un rango numérico
def x = 0
for (i in 0 .. 30) {
    x += i
}

// Iterando sobre una lista
x = 0
for( i in [5,3,2,1] ) {
    x += i
}

// Iterando sobre un arreglo
array = (0..20).toArray()
x = 0
for (i in array) {
    x += i
}

// Iterando sobre un mapa
def map = ['name':'Roberto', 'framework':'Grails', 'language':'Groovy']
x = ""
for ( e in map ) {
    x += e.value
    x += " "
}
assert x.equals("Roberto Grails Groovy ")

/*
  Operadores

  Para la lista de los operadores que Groovy soporta, visita:
  http://www.groovy-lang.org/operators.html#Operator-Overloading

  Operadores Groovy útiles
*/
// Operador de propagación:  invocar una acción en todos los elementos de un objeto agregado.
def technologies = ['Groovy','Grails','Gradle']
technologies*.toUpperCase() // equivale a: technologies.collect { it?.toUpperCase() }

// Operador de navegación segura: usado para evitar un NullPointerException.
def user = User.get(1)
def username = user?.username


/*
  Closures
  Un Closure en Groovy es como un "bloque de código" o un puntero a un método. Es una 
  porci´øn de código que es definida y ejecutada en un punto futuro en el tiempo.

  Más información en: http://www.groovy-lang.org/closures.html
*/
// Ejemplo:
def clos = { println "Hello World!" }

println "Executing the Closure:"
clos()

// Pasando parámetros a un closure
def sum = { a, b -> println a+b }
sum(2,4)

// Los Closures pueden referir a variables no listadas en sus listas de parámetros
def x = 5
def multiplyBy = { num -> num * x }
println multiplyBy(10)

// Si tienes un Closure que toma un solo argumento, puedes omitir la
// definición del parámetro en el Closure
def clos = { print it }
clos( "hi" )

/*
  Groovy puede memorizar los resultados de un Closure [1][2][3]
*/
def cl = {a, b ->
    sleep(3000) // simula algún proceso que consume tiempo
    a + b
}

mem = cl.memoize()

def callClosure(a, b) {
    def start = System.currentTimeMillis()
    mem(a, b)
    println "Inputs(a = $a, b = $b) - took ${System.currentTimeMillis() - start} msecs."
}

callClosure(1, 2)
callClosure(1, 2)
callClosure(2, 3)
callClosure(2, 3)
callClosure(3, 4)
callClosure(3, 4)
callClosure(1, 2)
callClosure(2, 3)
callClosure(3, 4)

/*
  Expando

  La clase Expando es un bean dinámico para que podamos agregar propiedades y closures
  como métodos a una instancia de esta clase

  http://mrhaki.blogspot.mx/2009/10/groovy-goodness-expando-as-dynamic-bean.html
*/
  def user = new Expando(name:"Roberto")
  assert 'Roberto' == user.name

  user.lastName = 'Pérez'
  assert 'Pérez' == user.lastName

  user.showInfo = { out ->
      out << "Name: $name"
      out << ", Last name: $lastName"
  }

  def sw = new StringWriter()
  println user.showInfo(sw)


/*
  Metaprogramación (MOP)
*/

// Usando ExpandoMetaClass para agregar comportamiento
String.metaClass.testAdd = {
    println "we added this"
}

String x = "test"
x?.testAdd()

// Interceptando llamadas a métodos
class Test implements GroovyInterceptable {
    def sum(Integer x, Integer y) { x + y }

    def invokeMethod(String name, args) {
        System.out.println "Invoke method $name with args: $args"
    }
}

def test = new Test()
test?.sum(2,3)
test?.multiply(2,3)

// Groovy soporta propertyMissing para lidiar con intentos de resolución de propiedades.
class Foo {
   def propertyMissing(String name) { name }
}
def f = new Foo()

assertEquals "boo", f.boo

/*
  TypeChecked y CompileStatic
  Groovy, por naturaleza, es y siempre será un lenguaje dinámico pero soporta
  typechecked y compilestatic

  Más información: http://www.infoq.com/articles/new-groovy-20
*/
// TypeChecked
import groovy.transform.TypeChecked

void testMethod() {}

@TypeChecked
void test() {
    testMeethod()

    def name = "Roberto"

    println naameee

}

// Otro ejemplo:
import groovy.transform.TypeChecked

@TypeChecked
Integer test() {
    Integer num = "1"

    Integer[] numbers = [1,2,3,4]

    Date date = numbers[1]

    return "Test"

}

// ejemplo de CompileStatic:
import groovy.transform.CompileStatic

@CompileStatic
int sum(int x, int y) {
    x + y
}

assert sum(2,5) == 7


```

## Más recursos

[Documentación de Groovy](http://www.groovy-lang.org/documentation.html)

[Consola Web de Groovy](http://groovyconsole.appspot.com/)

Únete a un [Groovy user group](http://www.groovy-lang.org/usergroups.html)

## Libros

* [Groovy Goodness] (https://leanpub.com/groovy-goodness-notebook)

* [Groovy in Action] (http://manning.com/koenig2/)

* [Programming Groovy 2: Dynamic Productivity for the Java Developer] (http://shop.oreilly.com/product/9781937785307.do)

[1] http://roshandawrani.wordpress.com/2010/10/18/groovy-new-feature-closures-can-now-memorize-their-results/
[2] http://www.solutionsiq.com/resources/agileiq-blog/bid/72880/Programming-with-Groovy-Trampoline-and-Memoize
[3] http://mrhaki.blogspot.mx/2011/05/groovy-goodness-cache-closure-results.html
